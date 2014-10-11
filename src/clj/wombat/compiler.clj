(ns wombat.compiler
  (:require [clojure.core.match :refer [match]])
  (:import [org.objectweb.asm ClassWriter ClassVisitor Opcodes Type Handle]
           [org.objectweb.asm.commons GeneratorAdapter Method]
           [clojure.lang DynamicClassLoader Compiler RT]
           [java.lang.invoke MethodType MethodHandle MethodHandles MethodHandles$Lookup
            CallSite VolatileCallSite]
           [wombat ILambda AsmUtil Global])
  (:refer-clojure :exclude [compile]))

(def ^:dynamic *class-loader* (DynamicClassLoader.))

(dotimes [n 22]
  (eval `(gen-interface :name ~(symbol (str "wombat.ILambda" n))
                        :extends [wombat.ILambda]
                        :methods [[invoke [~@(repeat n Object)] Object]])))

(def ^:dynamic *print-debug* nil)
(defn debug [& vals]
  (when *print-debug*
    (apply println vals)))

(def global-env (atom {}))
(def global-bindings (atom {}))

(def seqable?
  "True if the given value implements clojure.lang.Seqable. Note that
  that does NOT include all types which can be seq'd. Only internal
  Clojure collection types."
  (partial instance? clojure.lang.Seqable))

(defonce -id- (atom -1))
(defn next-id []
  (swap! -id- inc'))

(def specials
  '#{lambda let if define quote begin})

(defn sanitize-name
  [sym]
  (symbol (str sym "__#" (next-id))))

(defn extend-env
  [env syms]
  (reduce #(assoc %1 %2 (sanitize-name %2))
          env syms))

(defn substitute
  "Renames a plain symbol to its sanitized name in the given
  environment. IllegalArgumentException is thrown for symbols which
  are not in the given environment. Specials which are not in the
  environment map pass through unmodified.

  E.g. (substitute '{foo foo__#0} 'foo) ;=> foo__#0
       (substitute '{foo foo__#0} 'lambda) ;=> lambda"
  [env sym]
  (or (get env sym)
      (get specials sym)
      (throw (IllegalArgumentException. (str "symbol " sym " is not defined")))))

(defn sanitize
  "Renames all bound symbols within the given form to guarantee
  uniqueness."
  [env form]
  (cond
   (seqable? form)
   (match (seq form)
     nil form
     
     (['quote val] :seq) (list 'quote val)

     (['lambda params & body] :seq)
     (let [lambda-env (extend-env env params)]
       (list* 'lambda (map (partial substitute lambda-env) params) (map (partial sanitize lambda-env) body)))

     (['let bindings & body] :seq)
     (let [sani-binds (map (partial sanitize env) (map second bindings))
                                        ; Scheme-style let does not extend env with new
                                        ; names until after bindings are evaluated
           [let-env bind-syms] (loop [e env, sani-bs [], bs (map first bindings)]
                                 (if (seq bs)
                                   (let [sani (sanitize-name (first bs))]
                                     (recur (assoc env (first bs) sani)
                                            (conj sani-bs sani)
                                            (rest bs)))
                                   [e sani-bs]))]
       (list* 'let (map list bind-syms sani-binds) (map (partial sanitize let-env) body)))

     (['define name & val] :seq)
     (let [sani-name (get @global-env name (sanitize-name name))]
       (swap! global-env assoc name sani-name)
       (list* 'define sani-name (map (partial sanitize (assoc env name sani-name)) val)))

     :else
     (map (partial sanitize env) form))

   (symbol? form)
   (substitute env form)

   :else
   form))

(defn free-vars
  "Returns a sorted-set of free symbols in the given form. Sorting is
  arbitrary, but consitent."
  [form]
  (cond
   (seqable? form)
   (match (seq form)
     nil (sorted-set)

     (['quote val] :seq) (sorted-set)

     (['lambda params & body] :seq)
     (apply disj (free-vars body) params)

     (['let bindings & body] :seq)
     (apply disj
            (reduce into (sorted-set) (map #(free-vars (second %)) bindings))
            (map first bindings))

     :else
     (reduce into (sorted-set) (map free-vars form)))

   (and (symbol? form) (not (contains? specials form)))
   (sorted-set form)

   :else
   (sorted-set)))

(declare emit
         emit-seq
         emit-value
         emit-dup
         emit-symbol
         emit-string
         eval*
         scheme-eval)

(defn asmtype ^Type [^Class cls] (Type/getType cls))
(def object-type (asmtype Object))
(def boolean-object-type (asmtype Boolean))
(def ilambda-type (asmtype ILambda))
(def void-ctor (Method/getMethod "void <init>()"))

(defn close-name
  [fsym]
  (str "close_" (munge (name fsym))))

(defn local-name
  [lsym]
  (str "local_" (munge (name lsym))))

(defn handle-name
  [arity]
  (str "handle_" arity))

(defn gen-closure-fields
  [^ClassVisitor cv closed-overs]
  (doseq [c closed-overs]
    (. cv visitField Opcodes/ACC_FINAL (close-name c) (.getDescriptor object-type) nil nil)))

(defn gen-ctor
  [cw fqname fv]
  (let [ctor (Method. "<init>" Type/VOID_TYPE (into-array Type (repeat (count fv) object-type)))
        gen (GeneratorAdapter. Opcodes/ACC_PUBLIC ctor nil nil cw)]
    (. gen visitCode)
    (. gen loadThis)
    (. gen invokeConstructor object-type void-ctor)
    (dotimes [n (count fv)]
      (. gen loadThis)
      (. gen loadArg n)
      (. gen putField (Type/getObjectType fqname) (close-name (nth fv n)) object-type))
    (. gen returnValue)
    (. gen endMethod)))

(defn emit-body
  [env context gen exprs]
  (doseq [stmt (butlast exprs)]
    (emit env :context/statement gen stmt))
  (emit env context gen (last exprs)))

(defn gen-body
  [cw {:keys [params] :as env} body]
  (debug "gen-body" body)
  (let [m (Method. "invoke" object-type (into-array Type (repeat (count params) object-type)))
        gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)]
    (. gen visitCode)
    (emit-body env :context/return gen body)
    (. gen returnValue)
    (. gen endMethod)))

(defn emit-array
  [gen asm-type contents]
  (AsmUtil/pushInt gen (count contents))
  (. gen newArray asm-type)
  (dotimes [n (count contents)]
    (. gen dup)
    (AsmUtil/pushInt gen n)
    (if (fn? (nth contents n))
      ((nth contents n))
      (. gen push (nth contents n)))
    (. gen arrayStore asm-type)))

(defn method-type
  [gen arity]
  (. gen push object-type)
  (emit-array gen (asmtype Class) (repeat arity object-type))
  (. gen invokeStatic (asmtype MethodType) (Method/getMethod "java.lang.invoke.MethodType methodType(Class,Class[])")))

(defn gen-handle
  [cw fqname arity]
  (let [^ClassVisitor cv cw]
    (. cv visitField (+ Opcodes/ACC_STATIC Opcodes/ACC_FINAL) (handle-name arity)
       (.getDescriptor (asmtype MethodHandle)) nil nil))

  (let [clinitgen (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                                     (Method/getMethod "void <clinit>()")
                                     nil nil cw)
        mt-local (. clinitgen newLocal (asmtype MethodType))]
    (. clinitgen visitCode)
    (method-type clinitgen arity)
    (. clinitgen storeLocal mt-local)

    (. clinitgen invokeStatic (asmtype MethodHandles) (Method/getMethod "java.lang.invoke.MethodHandles$Lookup lookup()"))
    (. clinitgen push (Type/getObjectType fqname))
    (. clinitgen push "invoke")
    (. clinitgen loadLocal mt-local)
    (. clinitgen invokeVirtual (asmtype MethodHandles$Lookup)
       (Method/getMethod "java.lang.invoke.MethodHandle findVirtual(Class,String,java.lang.invoke.MethodType)"))

    (. clinitgen loadLocal mt-local)
    (AsmUtil/pushInt clinitgen 0)
    (emit-array clinitgen (asmtype Class) [ilambda-type])
    (. clinitgen invokeVirtual (asmtype MethodType)
       (Method/getMethod "java.lang.invoke.MethodType insertParameterTypes(int,Class[])"))

    (. clinitgen invokeVirtual (asmtype MethodHandle)
     (Method/getMethod "java.lang.invoke.MethodHandle asType(java.lang.invoke.MethodType)"))
    (. clinitgen putStatic (Type/getObjectType fqname) (handle-name arity) (asmtype MethodHandle))

    (. clinitgen returnValue)
    (. clinitgen endMethod))

  (let [gen (GeneratorAdapter. Opcodes/ACC_PUBLIC
                               (Method/getMethod "java.lang.invoke.MethodHandle getHandle(int)")
                               nil nil cw)
        false-label (. gen newLabel)
        end-label (. gen newLabel)]
    (. gen visitCode)
    (. gen loadArg 0)
    (AsmUtil/pushInt gen arity)
    (. gen ifCmp Type/INT_TYPE GeneratorAdapter/NE false-label)

    (. gen getStatic (Type/getObjectType fqname) (handle-name arity) (asmtype MethodHandle))
    (. gen goTo end-label)
    
    (. gen mark false-label)
    (. gen newInstance (asmtype IllegalArgumentException))
    (. gen dup)
    (. gen newInstance (asmtype StringBuilder))
    (. gen dup)
    (. gen invokeConstructor (asmtype StringBuilder) void-ctor)
    (. gen push "Wrong number of args (")
    (. gen invokeVirtual (asmtype StringBuilder) (Method/getMethod "StringBuilder append(String)"))
    (. gen loadArg 0)
    (. gen invokeVirtual (asmtype StringBuilder) (Method/getMethod "StringBuilder append(int)"))
    (. gen push (str " for " arity ")"))
    (. gen invokeVirtual (asmtype StringBuilder) (Method/getMethod "StringBuilder append(String)"))
    (. gen invokeVirtual (asmtype StringBuilder) (Method/getMethod "String toString()"))
    (. gen invokeConstructor (asmtype IllegalArgumentException) (Method/getMethod "void <init>(String)"))
    (. gen throwException)

    (. gen mark end-label)
    (. gen returnValue)
    (. gen endMethod)))

(def ^:dynamic *compiled-lambdas*)
(defn compile
  "Compiles a given sanitized lambda form. If the input is not already
  sanitized, the results are undefined!

  Returns a vector of [<class name>, <closed-overs>, <bytecode>]"
  [[_ params & body :as lambda]]
  (when-not (get @*compiled-lambdas* lambda)
    (debug "Compiling:" lambda)
    (let [fv (vec (apply disj (free-vars lambda) (vals @global-env)))
          cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)
          lname (str "lambda_" (next-id))
          fqname (str "wombat/" lname)
          _ (debug "fqname:" fqname)
          dotname (.replace fqname "/" ".")
          ifaces (into-array String ["wombat/ILambda"])
          env {:params params, :closed-overs fv, :locals {} :thistype (Type/getObjectType fqname)}]
      (. cw visit Opcodes/V1_7 Opcodes/ACC_PUBLIC fqname nil "java/lang/Object" ifaces)
      (gen-closure-fields cw fv)
      (gen-ctor cw fqname fv)
      (gen-body cw env body)
      (gen-handle cw fqname (count params))
      (swap! *compiled-lambdas* conj lambda)
      [dotname fv (.toByteArray cw)])))

(defn emit
  [env context gen form]
  (cond
   (seqable? form)
   (emit-seq env context gen form)

   (symbol? form)
   (emit-symbol env context gen form)

   :else
   (emit-value context gen form)))

(def global-bootstrap
  (Handle. Opcodes/H_INVOKESTATIC
                     "wombat/Global"
                     "bootstrap"
                     (.toMethodDescriptorString
                      (MethodType/methodType CallSite (into-array Class [MethodHandles$Lookup String MethodType])))))

(defn emit-global
  [gen sym]
  (. gen invokeDynamic (name sym)
     (.toMethodDescriptorString (MethodType/methodType Object (make-array Class 0)))
     global-bootstrap
     (make-array Object 0)))

(defn emit-symbol
  [{:keys [params closed-overs locals thistype] :as env} context gen sym]
  (cond
   ((set params) sym)
   (. gen loadArg (.indexOf params sym))

   ((set closed-overs) sym)
   (do
     (. gen loadThis)
     (. gen getField thistype (close-name sym) object-type))

   (contains? locals sym)
   (. gen loadLocal (get locals sym))

   (contains? @global-bindings sym)
   (emit-global gen sym)

   :else
   (throw (IllegalStateException. (str "Symbol " sym " is not defined"))))
  (when (= context :context/statement)
    (. gen pop)))

(defn emit-value
  [context gen const]
  (debug "emit-value" context const)
  (cond
   (nil? const)
   (. gen visitInsn Opcodes/ACONST_NULL)

   (= true const)
   (. gen getStatic boolean-object-type "TRUE" boolean-object-type)

   (= false const)
   (. gen getStatic boolean-object-type "FALSE" boolean-object-type)

   :else
   (emit-dup gen const))
  (when (= context :context/statement)
    (. gen pop)))

(defmulti emit-dup
  (fn [_ n] (class n)))

(defmethod emit-dup :default
  [_ obj]
  (throw (IllegalArgumentException. (str "cannot emit-dup of " obj))))

(defmethod emit-dup Integer
  [gen ^Integer int]
  (debug "emit-dup int" int)
  (AsmUtil/pushInt gen int)
  (. gen invokeStatic (asmtype Integer) (Method/getMethod "Integer valueOf(int)")))

(defmethod emit-dup Long
  [gen ^Long long]
  (debug "emit-dup long" long)
  (AsmUtil/pushLong gen long)
  (. gen invokeStatic (asmtype Long) (Method/getMethod "Long valueOf(long)")))

(defmethod emit-dup Float
  [gen ^Float float]
                                        ; Cast up to double
  (debug "emit-dup float" float)
  (. gen push (.doubleValue float))
  (. gen invokeStatic (asmtype Double) (Method/getMethod "Double valueOf(double)")))

(defmethod emit-dup Double
  [gen ^Double double]
  (debug "emit-dup double" double)
  (. gen push (.doubleValue double))
  (. gen invokeStatic (asmtype Double) (Method/getMethod "Double valueOf(double)")))

(defmethod emit-dup Character
  [gen ^Character char]
  (debug "emit-dup char" char)
  (. gen push (.charValue char))
  (. gen invokeStatic (asmtype Character) (Method/getMethod "Character valueOf(char)")))

(defmethod emit-dup String
  [gen ^String string]
  (debug "emit-dup string" string)
  (. gen push string))

(defmethod emit-dup clojure.lang.Symbol
  [gen ^clojure.lang.Symbol sym]
  (debug "emit-dup symbol" sym)
  (. gen push (namespace sym))
  (. gen push (name sym))
  (. gen invokeStatic (asmtype clojure.lang.Symbol) (Method/getMethod "clojure.lang.Symbol intern(String,String)")))

(defmethod emit-dup clojure.lang.Seqable
  [gen ^clojure.lang.Seqable lis]
  (debug "emit-dup list" lis)
  (emit-array gen object-type (map #(fn [] (emit-value :context/expression gen %)) lis))
  (. gen invokeStatic (asmtype java.util.Arrays) (Method/getMethod "java.util.List asList(Object[])"))
  (. gen invokeStatic (asmtype clojure.lang.PersistentList) (Method/getMethod "clojure.lang.IPersistentList create(java.util.List)")))


(defonce -invoke- (Object.))
(defmulti emit-seq
  (fn [_ _ _ form] (first form))
  :default -invoke-)

(defmethod emit-seq 'lambda
  [env context gen [_ params & body :as lambda]]
  (when-not (= context :context/statement)
    (let [[dotname closed-overs bytecode] (compile lambda)]
      (. *class-loader* defineClass dotname bytecode lambda)
      (let [slashname (.replace dotname "." "/")]
        (. gen newInstance (Type/getObjectType slashname))
        (. gen dup)
        (dotimes [n (count closed-overs)]
          (emit-symbol env :context/expression gen (nth closed-overs n)))
        (. gen invokeConstructor (Type/getObjectType slashname)
           (Method. "<init>" Type/VOID_TYPE (into-array Type (repeat (count closed-overs) object-type))))))))

(defmethod emit-seq 'let
  [env context gen [_ bindings & body :as the-let]]
  (let [start-label (. gen newLabel)
        end-label (. gen newLabel)
        names (mapv #(vector (first %) (. gen newLocal object-type)) bindings)
        vals (mapv second bindings)]
    (dotimes [n (count names)]
      (let [[sym local-id] (nth names n)
            val-expr (nth vals n)]
        (emit env :context/expression gen val-expr)
        (. gen storeLocal local-id)))
    (let [let-env (update-in env [:locals] merge (into {} names))]
      (emit-body let-env context gen body))))

(defmethod emit-seq 'quote
  [env context gen [_ quoted :as form]]
  (when (> (count form) 2)
    (throw (IllegalArgumentException. "quote takes exactly one argument")))
  (emit-value context gen quoted))

(defmethod emit-seq 'begin
  [env context gen [_ & exprs :as form]]
  (emit-body env context gen exprs))

(defmethod emit-seq 'define
  [env context gen [_ name val :as form]]
  (when (> 3 (count form))
    (throw (IllegalArgumentException. "define only takes one value")))
  (let [handle (MethodHandles/constant Object (eval* val))]
    (if (contains? @global-bindings name)
      (.setTarget ^VolatileCallSite (@global-bindings name) handle)
      (swap! global-bindings assoc name (VolatileCallSite. handle))))
  (emit-global gen name))

(defmethod emit-seq 'if
  [env context gen [_ condition then else :as the-if]]
  (debug "emit-seq if" the-if context)
  (when-not (<= 3 (count the-if) 4)
    (throw (IllegalArgumentException. "if takes 2 or 3 forms")))
  (let [null-label (. gen newLabel)
        false-label (. gen newLabel)
        end-label (. gen newLabel)]
    (emit env :context/expression gen condition)
    (. gen dup)
    (. gen ifNull null-label)
    (. gen getStatic boolean-object-type "FALSE" boolean-object-type)
    (. gen ifCmp boolean-object-type GeneratorAdapter/EQ false-label)
    (emit env context gen then)
    (. gen goTo end-label)
    (. gen mark null-label)
    (. gen pop) ; pop dup of condition created for false check
    (. gen mark false-label)
    (emit env context gen else)
    (. gen mark end-label)))

(defn asm-println
  "Generates bytecode to print the toString of whatever is on top of
  the stack. Leaves stack unchaged."
  [gen]
  (. gen dup)
  (. gen invokeVirtual object-type (Method/getMethod "String toString()"))
  (. gen push "DEBUG: ")
  (. gen swap)
  (. gen invokeVirtual (asmtype String) (Method/getMethod "String concat(String)"))
  (. gen push "clojure.core")
  (. gen push "*out*")
  (. gen invokeStatic (asmtype RT) (Method/getMethod "clojure.lang.Var var(String,String)"))
  (. gen invokeVirtual (asmtype clojure.lang.Var) (Method/getMethod "Object deref()"))
  (. gen checkCast (asmtype java.io.PrintWriter))
  (. gen swap)
  (. gen invokeVirtual (asmtype java.io.PrintWriter) (Method/getMethod "void println(String)")))

(defmethod emit-seq -invoke-
  [env context gen [fun & args :as call]]
  (debug "emitting invoke:" call)
  (debug "context:" context)

  (emit env :context/expression gen fun)
  (. gen dup)
  (. gen checkCast ilambda-type)
  (AsmUtil/pushInt gen (count args))
  (. gen invokeInterface ilambda-type (Method/getMethod "java.lang.invoke.MethodHandle getHandle(int)"))
  (. gen swap)

  (doseq [a args]
    (emit env :context/expression gen a))
  (. gen invokeVirtual (asmtype MethodHandle)
     (Method. "invoke" object-type (into-array Type (cons ilambda-type (repeat (count args) object-type)))))
  (when (= context :context/statement)
    (. gen pop)))

(defn eval*
  "Low-level eval call. Requires a pre-sanitized input."
  [form]
  (let [[name _ bytecode] (compile (list 'lambda () form))]
    (.defineClass *class-loader* name bytecode form)
    (.. (Class/forName name true *class-loader*) newInstance invoke)))

(defn scheme-eval
  "Top-level eval call. Initializes env and sanitizes input."
  [form]
  (binding [*compiled-lambdas* (atom #{})]
    (eval* (sanitize @global-env form))))
