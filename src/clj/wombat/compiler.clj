(ns wombat.compiler
  (:require [clojure.core.match :refer [match]]
            wombat.empty
            [wombat.reader :refer [read]]
            [wombat.printer :refer :all]
            [wombat.datatypes :refer :all])
  (:import [org.objectweb.asm ClassWriter ClassVisitor Opcodes Type Handle]
           [org.objectweb.asm.commons GeneratorAdapter Method TableSwitchGenerator]
           [clojure.lang DynamicClassLoader Compiler RT LineNumberingPushbackReader
            Keyword Symbol Numbers BigInt Ratio]
           [java.lang.invoke MethodType MethodHandle MethodHandles MethodHandles$Lookup
            CallSite VolatileCallSite]
           [java.io FileReader]
           [java.math BigInteger]
           [wombat ILambda AsmUtil Global])
  (:refer-clojure :exclude [compile load-file eval read cons list? vector? list* list]))
(alias 'core 'clojure.core)

;(set! *warn-on-reflection* true)

(def ^:dynamic *class-loader* (DynamicClassLoader. (RT/baseLoader)))
(def clean-resolve (partial ns-resolve 'wombat.empty))

(defn maybe-class
  [cname]
  (when-let [cls (clean-resolve cname)]
    (when (class? cls) cls)))

(def object-array-class (class (object-array 0)))

(def ^:dynamic *print-debug* true)
(defn debug [& vals]
  (when *print-debug*
    (apply println vals)))

(def global-bindings (atom {}))
(def ^:dynamic *current-define* nil)
(def ^:dynamic *lambda-name* nil)
(def ^:dynamic *top-level* nil)
(def ^:dynamic *return-continuations* nil)

(defn list-like?
  "True if the given value implements clojure.lang.Seqable. Note that
  that does NOT include all types which can be seq'd. Only internal
  Clojure collection types and wombat.datatypes.List."
  [o]
  (and (instance? clojure.lang.Seqable o)
       (not (vector? o))))

(defonce -id- (atom -1))
(defn next-id []
  (swap! -id- inc'))

(def rest-token (symbol "#!rest"))
(def noop-token (symbol "#!no-op"))
(def rest-token? #{rest-token})
(defn special-token?
  [sym]
  (.startsWith (str sym) "#!"))

(def specials
  #{'lambda
    'case-lambda
    'let
    'letrec
    'if
    'define
    'define-macro
    'define-class*
    'quote
    'begin
    'unquote
    'unquote-splicing})

(defn sanitize-name
  [sym]
  (symbol (str sym "__#" (next-id))))

(defn extend-env
  [env syms]
  (reduce #(assoc %1 %2 (sanitize-name %2))
          env (remove special-token? syms)))

(defn substitute
  "Renames a plain symbol to its sanitized name in the given
  environment. IllegalArgumentException is thrown for symbols which
  are not in the given environment. Specials, class names and global
  defines which are not in the environment map pass through
  unmodified.

  E.g. (substitute '{foo foo__#0} 'foo) ;=> foo__#0
       (substitute '{foo foo__#0} 'lambda) ;=> lambda
       (substitute '{foo foo__#0} 'java.lang.Object) ;=> java.lang.Object"  
  [env sym]
  (if (symbol? sym)
    (cond
     (contains? env sym)
     (env sym)

     (special-token? sym)
     sym

     (contains? specials sym)
     sym

     (contains? @global-bindings sym)
     sym

     (= *current-define* sym)
     sym

     (maybe-class sym)
     sym

     :else
     (throw (IllegalArgumentException. (str "symbol " sym " is not defined"))))
    sym))

(defn expand1
  [form]
  (if-let [^ILambda macro (and (list-like? form)
                               (not (contains? specials (first form)))
                               (get-in @global-bindings [(first form) :macro]))]
    (loop [expanded (.applyTo macro (cdr (seq->list form)))]
      (cond
       (instance? wombat.Continuation expanded)
       (recur (.invoke ^wombat.Continuation expanded))

       (= noop-token expanded)
       form

       :else
       expanded))
    form))

(defn expand
  [form]
  (let [expanded (expand1 form)]
    (if (identical? form expanded)
      form
      (recur expanded))))

(declare sanitize)
(defn expand-params
  [params]
  (cond
   (symbol? params)
   (list rest-token params)

   (instance? wombat.datatypes.Pair params)
   (reduce -cons (list rest-token (.end params))
           (reverse (.front params)))

   :else
   params))

(defn sanitize-method
  [env params body]
  (let [method-env (extend-env env params)]
    (list* (map (partial substitute method-env) params)
           (map (partial sanitize method-env) body))))

(defn validate-rest-token!
  [params]
  (when-let [rest (seq (drop-while (complement rest-token?) params))]
    (when (not= (count rest) 2)
      (throw (IllegalArgumentException. "invalid placement of #!rest token")))))

(defn validate-unique-names!
  [params]
  (doseq [p params]
    (when (> (count (filter #{p} params)) 1)
      (throw (IllegalArgumentException. (str "duplicate parameter name: " p))))))

(defn validate-params!
  [params]
  (validate-rest-token! params)
  (validate-unique-names! params))

(defn validate-arities
  [methods]
  (let [params (map #(expand-params (first %)) methods)
        variadic (seq (filter #(some rest-token? %) params))]
    (doseq [v variadic] (validate-rest-token! v))
    (when (> (count variadic) 1)
      (throw (IllegalArgumentException. "case-lambda only accepts one variadic overload")))
    (doseq [p params] (validate-unique-names! p))
    (when (and variadic
               (>= (apply max (map count (remove #(some rest-token? %) params)))
                   (dec (count (first variadic)))))
      (throw (IllegalArgumentException. "case-lambda can't have a fixed arity with more params than the variadic")))
    (when (not= (count (set (map #(count (remove rest-token? %)) params)))
                (count params))
      (throw (IllegalArgumentException. "case-lambda can't have two overloads with the same arity")))
    (map #(cons %1 (rest %2))
         params methods)))

(defn sanitize-lambda
  [env [_ params & body :as form]]
  (let [params (expand-params params)]
    (validate-params! params)
    (cons 'lambda (sanitize-method env params body))))

(defn sanitize-case-lambda
  [env [_ & methods :as form]]
  (cons 'case-lambda
        (for [[params & body] (validate-arities methods)]
          (sanitize-method env params body))))

(defn named-let
  [[_ name bindings & body]]
  (list 'letrec
        (list (list name (list* 'lambda (map first bindings) body)))
        (list* name (map second bindings))))

(defn sanitize-let
  [env [_ bindings & body :as form]]
  (if (symbol? bindings)
    (sanitize env (named-let form))
    (let [sani-binds (map (partial sanitize env) (map second bindings))
                                        ; Scheme-style let does not extend env with new
                                        ; names until after bindings are evaluated
          [let-env bind-syms] (loop [e env, sani-bs [], bs (map first bindings)]
                                (if (seq bs)
                                  (let [sani (sanitize-name (first bs))]
                                    (recur (assoc e (first bs) sani)
                                           (conj sani-bs sani)
                                           (rest bs)))
                                  [e sani-bs]))]
      (list* 'let (map list bind-syms sani-binds) (map (partial sanitize let-env) body)))))

(defn sanitize-letrec
  [env [_ bindings & body :as form]]
  (let [letrec-env (extend-env env (map first bindings))
        bind-syms (map #(substitute letrec-env (first %)) bindings)
        sani-binds (map (partial sanitize letrec-env) (map second bindings))]
    (list* 'letrec (map list bind-syms sani-binds) (map (partial sanitize letrec-env) body))))

(declare sanitize-jvm)

(defn sanitize-define
  [env [define name & val :as form]]
  (debug "sanitize-define: " form)
  (if-not (symbol? name)
    (let [[name & params] (expand-params name)]
      (when (or (nil? name)
                (rest-token? name))
        (throw (IllegalArgumentException. (str "Invalid lambda define: " (write-str form)))))
      (recur env (list define name (list* 'lambda params val))))
    (list* define name (map (partial sanitize (assoc env name name)) val))))

(defn sanitize-seq
  [env form]
  (if (seq form)
    (condp #(and (contains? %1 (first %2)) %2) form
      #{'quote} form

      #{'lambda} :>> (partial sanitize-lambda env)

      #{'case-lambda} :>> (partial sanitize-case-lambda env)

      #{'let} :>> (partial sanitize-let env)

      #{'letrec} :>> (partial sanitize-letrec env)
      
      #{'define 'define-macro} :>> (partial sanitize-define env)

      #{'define-class*} form

      #{:jvm} :>> (fn [[_ & insns]]
                    (list* :jvm (map (partial sanitize-jvm env) insns)))

      (map (partial sanitize env) form))
    nil))

(defn sanitize
  "Renames all bound symbols within the given form to guarantee
  uniqueness. Throws if a symbol is not accessible from the context in
  which it is referenced. Globally defined symbols are not renamed."
  [env form]
  (cond
   (list-like? form)
   (let [expanded (expand form)]
     (if (identical? expanded form)
       (sanitize-seq env form)
       (recur env expanded)))

   (symbol? form)
   (substitute env form)

   :else
   form))

(defn free-vars
  "Returns a sorted-set of free symbols in the given form. Sorting is
  arbitrary, but consistent."
  [form]
  (cond
   (list-like? form)
   (match (seq form)
     nil (sorted-set)

     (['quote val] :seq) (sorted-set)

     (['lambda params & body] :seq)
     (apply disj (free-vars body) (cons *lambda-name* (remove rest-token? params)))

     (['case-lambda & methods] :seq)
     (reduce into (sorted-set)
             (map #(apply disj (free-vars (second %))
                          (cons *lambda-name* (remove rest-token? (first %))))
                  methods))

     ([(:or 'let 'letrec) bindings & body] :seq)
     (let [[name bindings & body] (if (symbol? bindings)
                                    (cons bindings body)
                                    (list* nil bindings body))
           bind-frees (map #(free-vars (second %)) bindings)
           body-frees (map free-vars body)]
       (apply disj (reduce into (sorted-set) (concat bind-frees body-frees))
              (cons name (map first bindings))))

     (['define name & val] :seq)
     (disj (free-vars val) name)

     (['define-macro name & val] :seq)
     (disj (free-vars val) name)

     ;; define-class* is not a closure, leave free vars uncollected
     ;; so they will throw during compilation
     (['define-class* & rest] :seq)
     (sorted-set)

     ([:jvm & forms] :seq)
     (into (sorted-set)
           (mapcat #(when (keyword? (first %))
                      (free-vars (rest %)))
                   forms))

     :else
     (reduce into (sorted-set) (map free-vars form)))

   (and (symbol? form)
        (not (special-token? form))
        (not (contains? specials form))
        (not (contains? @global-bindings form))
        (not= *current-define* form)
        (not (maybe-class form)))
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
         eval)

(defn asmtype ^Type [cls] (if (instance? Type cls)
                            cls
                            (Type/getType ^Class cls)))
(def ^Type object-type (asmtype Object))
(def ^Type boolean-object-type (asmtype Boolean))
(def ^Type ilambda-type (asmtype ILambda))
(def ^Method void-ctor (Method/getMethod "void <init>()"))

(defn dotmunge
  [str]
  (.replace ^String (munge str) "." "_DOT_"))

(defn close-name
  [fsym]
  (str "close_" (dotmunge (name fsym))))

(defn local-name
  [lsym]
  (str "local_" (dotmunge (name lsym))))

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

(defn emit-list-from-array
  [^GeneratorAdapter gen emit-array-fn]
  (. gen getStatic (asmtype Global) "JAVALIST_TO_LIST" (asmtype clojure.lang.Var))
  (emit-array-fn)
  (. gen invokeStatic (asmtype java.util.Arrays) (Method/getMethod "java.util.List asList(Object[])"))
  (. gen invokeVirtual (asmtype clojure.lang.Var) (Method/getMethod "Object invoke(Object)")))

(defn gen-body
  [cw {:keys [params restarg] :as env} body]
  (debug "gen-body" body)
  (debug "params:" params)
  (debug "restarg:" restarg)
  (let [sig (repeat (count params) object-type)
        sig (if restarg
              (conj (vec sig) (asmtype object-array-class))
              sig)
        _ (debug "sig:" (seq sig))
        m (Method. "invoke" object-type (into-array Type sig))
        _ (debug "method:" m)
        gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)
        rest-id (when restarg (. gen newLocal object-type))
        env (if restarg
              (assoc-in env [:locals restarg] rest-id)
              env)
        top-label (. gen newLabel)
        env (assoc env :top-label top-label)]
    (. gen visitCode)
    (when rest-id
      (emit-list-from-array gen #(. gen loadArg (count params)))
      (. gen storeLocal rest-id)
      (. gen visitInsn Opcodes/ACONST_NULL)
      (. gen storeArg (count params)))

    (. gen mark top-label)
    (emit-body env :context/return gen body)
    (. gen returnValue)
    (. gen endMethod)))

(defn emit-arity-exception
  [^GeneratorAdapter gen ^Type thistype given-arity-fn]
  (. gen newInstance (asmtype IllegalArgumentException))
  (. gen dup)
  (. gen newInstance (asmtype StringBuilder))
  (. gen dup)
  (. gen invokeConstructor (asmtype StringBuilder) void-ctor)
  (. gen push "Wrong number of args (")
  (. gen invokeVirtual (asmtype StringBuilder) (Method/getMethod "StringBuilder append(String)"))
  (given-arity-fn)
  (. gen invokeVirtual (asmtype StringBuilder) (Method/getMethod "StringBuilder append(int)"))
  (. gen push (str ") passed to: " (.getClassName thistype)))
  (. gen invokeVirtual (asmtype StringBuilder) (Method/getMethod "StringBuilder append(String)"))
  (. gen invokeVirtual (asmtype StringBuilder) (Method/getMethod "String toString()"))
  (. gen invokeConstructor (asmtype IllegalArgumentException) (Method/getMethod "void <init>(String)"))
  (. gen throwException))

(declare emit-var)
(defn asm-println
  "Generates bytecode to print the toString of whatever is on top of
  the stack. Leaves stack unchaged."
  [^GeneratorAdapter gen]
  (. gen dup)
  (emit-var gen 'clojure.core/*out*)
  (. gen invokeVirtual (asmtype clojure.lang.Var) (Method/getMethod "Object deref()"))
  (. gen checkCast (asmtype java.io.Writer))
  (. gen dup)
  (. gen push "DEBUG: ")
  (. gen invokeVirtual (asmtype java.io.Writer) (Method/getMethod "void write(String)"))
  (. gen push "wombat.printer")
  (. gen push "write-obj")
  (. gen invokeStatic (asmtype RT) (Method/getMethod "clojure.lang.Var var(String,String)"))
  (. gen dupX2)
  (. gen pop)
  (. gen invokeVirtual (asmtype clojure.lang.Var) (Method/getMethod "Object invoke(Object,Object)"))
  (. gen pop)
  (emit-var gen 'clojure.core/println)
  (. gen invokeVirtual (asmtype clojure.lang.Var) (Method/getMethod "Object invoke()"))
  (. gen pop))

(defn fixed-arities
  [arglists]
  (sort (map count (remove #((set %) rest-token) arglists))))

(defn variadic-arity
  [arglists]
  (if-let [varg (some #(and ((set %) rest-token) %) arglists)]
    (dec (count varg))))

(defn min-variadic-arity
  ([arglists] (min-variadic-arity (fixed-arities arglists)
                                  (variadic-arity arglists)))
  ([fixed variadic]
     (when variadic
       (if (= (last fixed) (dec variadic))
         variadic
         (dec variadic)))))

(defn gen-applier
  [^ClassWriter cw ^Type thistype arglists]
  (let [gen (GeneratorAdapter. Opcodes/ACC_PUBLIC (Method/getMethod "Object applyTo(Object)") nil nil cw)
        list-local (. gen newLocal (asmtype wombat.datatypes.List))
        fixed-arities (fixed-arities arglists)
        variadic (variadic-arity arglists)
        min-varg-arity (min-variadic-arity fixed-arities variadic)
        emit-list-cnt (fn []
                        (let [is-null (. gen newLabel)
                              end (. gen newLabel)]
                          (. gen loadLocal list-local)
                          (. gen dup)
                          (. gen ifNull is-null)
                          (. gen getField (asmtype wombat.datatypes.List) "cnt" Type/INT_TYPE)
                          (. gen goTo end)
                          (. gen mark is-null)
                          (. gen pop)
                          (AsmUtil/pushInt gen 0)
                          (. gen mark end)))
        unroll-list (fn [n]
                      (dotimes [_ n]
                        (. gen dup)
                        (. gen getField (asmtype wombat.datatypes.List) "head" object-type)
                        (. gen swap)
                        (. gen getField (asmtype wombat.datatypes.List) "tail" object-type)
                        (. gen checkCast (asmtype wombat.datatypes.List))))]
    (. gen visitCode)

    (. gen loadArg 0)
    (. gen checkCast (asmtype wombat.datatypes.List))
    (. gen storeLocal list-local)
    
    (let [switch-label (. gen newLabel)
          end-switch-label (. gen newLabel)
          switch-generator (reify TableSwitchGenerator
                             (generateCase [this key end]
                               (let [method (Method. "invoke" object-type (into-array Type (repeat key object-type)))]
                                 (. gen loadThis)
                                 (. gen loadLocal list-local)
                                 (unroll-list key)
                                 (. gen storeLocal list-local) ; should be null
                                 (. gen invokeVirtual thistype method)
                                 (. gen goTo end)))
                             (generateDefault [this]
                               (emit-arity-exception gen thistype emit-list-cnt)))]
      (emit-list-cnt)
      (when variadic
        (. gen dup)
        (AsmUtil/pushInt gen min-varg-arity)
        (. gen ifCmp Type/INT_TYPE GeneratorAdapter/LT switch-label)
        (. gen pop) ; pop list count
        (. gen loadThis)
        (. gen loadLocal list-local)
        (unroll-list (dec variadic))
        (. gen invokeStatic (asmtype RT) (Method/getMethod "Object[] seqToArray(clojure.lang.ISeq)"))
        (. gen visitInsn Opcodes/ACONST_NULL)
        (. gen storeLocal list-local)
        (. gen invokeVirtual thistype (Method. "invoke" object-type (into-array Type (concat (repeat (dec variadic) object-type)
                                                                                             (list (asmtype object-array-class))))))
        (. gen goTo end-switch-label))
      
      (. gen mark switch-label)
      ;; list count is on stack
      (. gen tableSwitch (int-array fixed-arities) switch-generator)
      (. gen mark end-switch-label))
    
    (. gen returnValue)
    (. gen endMethod)))

(defn emit-array
  [^GeneratorAdapter gen type contents]
  (let [contents (seq contents)]
    (AsmUtil/pushInt gen (count contents))
    (. gen newArray (asmtype type))
    (dotimes [n (count contents)]
      (. gen dup)
      (AsmUtil/pushInt gen n)
      (emit-dup gen (nth contents n))
      (. gen arrayStore (asmtype type)))))

(defn method-type
  [^GeneratorAdapter gen arity restarg]
  (. gen push object-type)
  (let [positional (repeat (if restarg (dec arity) arity) Object)
        sig (if restarg
              (conj (vec positional) object-array-class)
              positional)]
    (emit-array gen Class sig))
  (. gen invokeStatic (asmtype MethodType) (Method/getMethod "java.lang.invoke.MethodType methodType(Class,Class[])")))

(defn gen-handle
  [^ClassWriter cw ^Type thistype arglists]
  (debug "gen-handle" thistype)
  (let [^ClassVisitor cv cw
        fixed (fixed-arities arglists)
        variadic (variadic-arity arglists)
        arities (if variadic
                  (concat fixed (list variadic))
                  fixed)
        min-varg-arity (min-variadic-arity fixed variadic)]
    (doseq [arity arities]
      (. cv visitField (+ Opcodes/ACC_STATIC Opcodes/ACC_FINAL) (handle-name arity)
         (.getDescriptor (asmtype MethodHandle)) nil nil))
    
    (let [clinitgen (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                                       (Method/getMethod "void <clinit>()")
                                       nil nil cw)
          mt-local (. clinitgen newLocal (asmtype MethodType))
          store-handle (fn [arity variadic?]
                         (method-type clinitgen arity variadic?)
                         (. clinitgen storeLocal mt-local)

                         (. clinitgen invokeStatic (asmtype MethodHandles) (Method/getMethod "java.lang.invoke.MethodHandles$Lookup lookup()"))

                         (. clinitgen push thistype)
                         (. clinitgen push "invoke")
                         (. clinitgen loadLocal mt-local)
                         (. clinitgen invokeVirtual (asmtype MethodHandles$Lookup)
                            (Method/getMethod "java.lang.invoke.MethodHandle findVirtual(Class,String,java.lang.invoke.MethodType)"))

                         (. clinitgen loadLocal mt-local)
                         (AsmUtil/pushInt clinitgen 0)
                         (emit-array clinitgen Class [ILambda])
                         (. clinitgen invokeVirtual (asmtype MethodType)
                            (Method/getMethod "java.lang.invoke.MethodType insertParameterTypes(int,Class[])"))

                         (. clinitgen invokeVirtual (asmtype MethodHandle)
                            (Method/getMethod "java.lang.invoke.MethodHandle asType(java.lang.invoke.MethodType)"))

                         (when variadic?
                           (. clinitgen push (asmtype object-array-class))
                           (. clinitgen invokeVirtual (asmtype MethodHandle)
                              (Method/getMethod "java.lang.invoke.MethodHandle asVarargsCollector(Class)")))

                         (. clinitgen putStatic thistype (handle-name arity) (asmtype MethodHandle)))]
      (. clinitgen visitCode)

      (doseq [f fixed]
        (store-handle f false))
      (when variadic
        (store-handle variadic true))

      (. clinitgen returnValue)
      (. clinitgen endMethod))

    (let [gen (GeneratorAdapter. Opcodes/ACC_PUBLIC
                                 (Method/getMethod "java.lang.invoke.MethodHandle getHandle(int)")
                                 nil nil cw)]
      (. gen visitCode)

      (let [min-varg-arity (when variadic
                             (if (= (last fixed) (dec variadic))
                               variadic
                               (dec variadic)))
            switch-label (. gen newLabel)
            end-switch-label (. gen newLabel)
            switch-generator (reify TableSwitchGenerator
                               (generateCase [this key end]
                                 (. gen getStatic thistype (handle-name key) (asmtype MethodHandle))
                                 (. gen goTo end))
                               (generateDefault [this]
                                 (emit-arity-exception gen thistype #(. gen loadArg 0))))]
        (. gen loadArg 0)
        (when variadic
          (. gen dup)
          (AsmUtil/pushInt gen min-varg-arity)
          (. gen ifCmp Type/INT_TYPE GeneratorAdapter/LT switch-label)
          (. gen pop) ; pop arity arg
          (. gen getStatic thistype (handle-name variadic) (asmtype MethodHandle))
          (. gen goTo end-switch-label))
        
        (. gen mark switch-label)
        ;; arity arg is on stack
        (. gen tableSwitch (int-array fixed) switch-generator)
        (. gen mark end-switch-label))
    
      (. gen returnValue)
      (. gen endMethod))))

(def ^:dynamic *compiled-lambdas*)

(defmulti compile
  "Compiles a given lambda or define-class*. Lambda input must be
  sanitized, or the results are undefined!

  Returns a vector of [<class name> <bytecode> <closed-overs>?]

  Note that because define-class* is not a closure, there will be no
  <closed-overs>"

  first)

(defn gen-methods
  [^ClassWriter cw {:keys [thistype] :as env} methods]
  (let [before-rest (complement #{rest-token})
        arglists (map first methods)]
    (doseq [[params & body] methods]
      (let [env (assoc env
                  :params (take-while before-rest params)
                  :restarg (second (drop-while before-rest params))
                  :arglists arglists)]
        (gen-body cw env body)))
    (gen-applier cw thistype arglists)
    (gen-handle cw thistype arglists)))

(defn compile-lambda
  [fv methods]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)
        lname (str (dotmunge (str (or *top-level* *lambda-name* 'lambda))) "_" (next-id))
        fqname (str "wombat/" lname)
        _ (debug "fqname:" fqname)
        dotname (.replace fqname "/" ".")
        ifaces (into-array String ["wombat/ILambda"])
        env {:closed-overs fv
             :locals {}
             :recur-sym *lambda-name*
             :thistype (Type/getObjectType fqname)}]
    (. cw visit Opcodes/V1_7 Opcodes/ACC_PUBLIC fqname nil "java/lang/Object" ifaces)
    (gen-closure-fields cw fv)
    (gen-ctor cw fqname fv)
    (binding [*lambda-name* (if *top-level* *lambda-name* nil)]
      (gen-methods cw env methods))
    (. cw visitEnd)
    [dotname (.toByteArray cw) fv]))

(defmethod compile 'lambda
  [[_ params & body :as lambda]]
  (when-not (get @*compiled-lambdas* lambda)
    (validate-params! params)
    (debug "Compiling:" lambda)
    (compile-lambda (vec (free-vars lambda)) (list (cons params body)))))

(defmethod compile 'case-lambda
  [[_ & methods :as case-lambda]]
  (debug "Compiling:" case-lambda)
  (compile-lambda (vec (free-vars case-lambda)) methods))

(defn emit
  [env context gen form]
  (cond
   (and (list-like? form) (seq form))
   (emit-seq env context gen form)

   (list-like? form) ; empty
   (emit-value context gen form)

   (symbol? form)
   (emit-symbol env context gen form)

   :else
   (emit-value context gen form)))

(def global-bootstrap
  (Handle. Opcodes/H_INVOKESTATIC
           "wombat/Global"
           "bootstrap"
           (.toMethodDescriptorString
            (MethodType/methodType CallSite (into-array Class [MethodHandles$Lookup String MethodType String])))))

(defn emit-global
  [^GeneratorAdapter gen sym]
  (. gen invokeDynamic "getGlobal"
     (.toMethodDescriptorString (MethodType/methodType Object (make-array Class 0)))
     global-bootstrap
     (object-array [(str sym)])))

(defn emit-symbol
  [{:keys [params closed-overs locals thistype recur-sym] :as env} context gen sym]
  (debug "emit-symbol: " sym)
  (cond
   (special-token? sym)
   (emit-value context gen sym)
   
   ((set params) sym)
   (. gen loadArg (.indexOf params sym))

   (contains? locals sym)
   (. gen loadLocal (get locals sym))

   (= recur-sym sym)
   (. gen loadThis)

   ((set closed-overs) sym)
   (do
     (. gen loadThis)
     (. gen getField thistype (close-name sym) object-type))

   (or (contains? @global-bindings sym)
       (= *current-define* sym))
   (emit-global gen sym)
   
   (maybe-class sym)
   (. gen push (asmtype (maybe-class sym)))

   :else
   (throw (IllegalStateException. (str "Symbol " sym " is not defined"))))
  (when (= context :context/statement)
    (. gen pop)))

(defn emit-value
  [context gen const]
  (debug "emit-value" context const)
  (emit-dup gen const)
  (when (= context :context/statement)
    (. gen pop)))

(defmulti emit-dup
  (fn [_ n] (class n)))

(defmethod emit-dup :default
  [_ obj]
  (throw (IllegalArgumentException. (str "cannot emit-dup of " obj))))

(defmethod emit-dup nil
  [^GeneratorAdapter gen _]
  (. gen visitInsn Opcodes/ACONST_NULL))

(defmethod emit-dup Boolean
  [^GeneratorAdapter gen ^Boolean b]
  (if b
    (. gen getStatic boolean-object-type "TRUE" boolean-object-type)
    (. gen getStatic boolean-object-type "FALSE" boolean-object-type)))

(defmethod emit-dup Class
  [^GeneratorAdapter gen ^Class class]
  (debug "emit-dup class" class)
  (. gen push (asmtype class)))

(defmethod emit-dup Integer
  [^GeneratorAdapter gen ^Integer int]
  (debug "emit-dup int" int)
  (AsmUtil/pushInt gen int)
  (. gen invokeStatic (asmtype Integer) (Method/getMethod "Integer valueOf(int)")))

(defmethod emit-dup Long
  [^GeneratorAdapter gen ^Long long]
  (debug "emit-dup long" long)
  (AsmUtil/pushLong gen long)
  (. gen invokeStatic (asmtype Long) (Method/getMethod "Long valueOf(long)")))

(defmethod emit-dup Float
  [^GeneratorAdapter gen ^Float float]
                                        ; Cast up to double
  (debug "emit-dup float" float)
  (. gen push (.doubleValue float))
  (. gen invokeStatic (asmtype Double) (Method/getMethod "Double valueOf(double)")))

(defmethod emit-dup Double
  [^GeneratorAdapter gen ^Double double]
  (debug "emit-dup double" double)
  (. gen push (.doubleValue double))
  (. gen invokeStatic (asmtype Double) (Method/getMethod "Double valueOf(double)")))

(defn emit-big-integer
  [^GeneratorAdapter gen bi]
  (. gen newInstance (asmtype BigInteger))
  (. gen dup)
  (. gen push (.toString bi))
  (. gen invokeConstructor (asmtype BigInteger) (Method/getMethod "void <init>(String)")))

(defmethod emit-dup BigInt
  [^GeneratorAdapter gen ^BigInt bigint]
  (debug "emit-dup BigInt" bigint)
  (emit-big-integer gen bigint)
  (. gen invokeStatic (asmtype BigInt) (Method/getMethod "clojure.lang.BigInt fromBigInteger(java.math.BigInteger)")))

(defmethod emit-dup Ratio
  [^GeneratorAdapter gen ^Ratio ratio]
  (debug "emit-dup Ratio" ratio)
  (. gen newInstance (asmtype Ratio))
  (. gen dup)
  (emit-big-integer gen (.numerator ratio))
  (emit-big-integer gen (.denominator ratio))
  (. gen invokeConstructor (asmtype Ratio) (Method/getMethod "void <init>(java.math.BigInteger,java.math.BigInteger)")))

(defmethod emit-dup Character
  [^GeneratorAdapter gen ^Character char]
  (debug "emit-dup char" char)
  (. gen push (int (.charValue char)))
  (. gen invokeStatic (asmtype Character) (Method/getMethod "Character valueOf(char)")))

(defmethod emit-dup String
  [^GeneratorAdapter gen ^String string]
  (debug "emit-dup string" string)
  (. gen push string))

(defmethod emit-dup Symbol
  [^GeneratorAdapter gen ^Symbol sym]
  (debug "emit-dup symbol" sym)
  (emit-dup gen (namespace sym))
  (emit-dup gen (name sym))
  (. gen invokeStatic (asmtype Symbol) (Method/getMethod "clojure.lang.Symbol intern(String,String)")))

(defmethod emit-dup clojure.lang.Keyword
  [^GeneratorAdapter gen ^clojure.lang.Keyword kw]
  (debug "emit-dup keyword" kw)
  (emit-dup gen (namespace kw))
  (emit-dup gen (name kw))
  (. gen invokeStatic (asmtype clojure.lang.Keyword) (Method/getMethod "clojure.lang.Keyword intern(String,String)")))

(defmethod emit-dup clojure.lang.Seqable
  [^GeneratorAdapter gen ^clojure.lang.Seqable lis]
  (debug "emit-dup list" lis)
  (emit-list-from-array gen #(emit-array gen Object lis)))

(defmethod emit-dup wombat.datatypes.Vector
  [^GeneratorAdapter gen ^wombat.datatypes.Vector v]
  (debug "emit-dup vector")
  (. gen newInstance (asmtype wombat.datatypes.Vector))
  (. gen dup)
  (emit-array gen Object v)
  (. gen invokeConstructor (asmtype wombat.datatypes.Vector) (Method/getMethod "void <init>(Object)")))

(defmethod emit-dup wombat.datatypes.Pair
  [^GeneratorAdapter gen ^wombat.datatypes.Pair p]
  (debug "emit-dup pair")
  (. gen newInstance (asmtype wombat.datatypes.Pair))
  (. gen dup)
  (emit-dup gen (.front p))
  (emit-dup gen (.end p))
  (. gen invokeConstructor (asmtype wombat.datatypes.Pair) (Method/getMethod "void <init>(Object,Object)")))

(prefer-method emit-dup wombat.datatypes.Vector clojure.lang.Seqable)

(defmethod emit-dup object-array-class
  [^GeneratorAdapter gen ^objects ary]
  (debug "emit-dup Object[]")
  (emit-array gen Object ary))

(defonce -invoke- (Object.))
(defmulti emit-seq
  (fn [_ _ _ form] (first form))
  :default -invoke-)

(defn emit-lambda
  [env context ^GeneratorAdapter gen lambda]
  (when-not (= context :context/statement)
    (let [[dotname bytecode closed-overs] (binding [*top-level* nil]
                                            (compile lambda))]
      (. *class-loader* defineClass dotname bytecode lambda)
      (let [slashname (.replace dotname "." "/")]
        (. gen newInstance (Type/getObjectType slashname))
        (. gen dup)
        (dotimes [n (count closed-overs)]
          (emit-symbol env :context/expression gen (nth closed-overs n)))
        (. gen invokeConstructor (Type/getObjectType slashname)
           (Method. "<init>" Type/VOID_TYPE (into-array Type (repeat (count closed-overs) object-type))))))))

(defmethod emit-seq 'case-lambda
  [env context ^GeneratorAdapter gen case-lambda]
  (emit-lambda env context gen case-lambda))

(defmethod emit-seq 'lambda
  [env context ^GeneratorAdapter gen [_ params & body :as lambda]]
  (emit-lambda env context gen lambda))

;; (defmethod emit-seq 'define-class*
;;   [env context gen [_ fields :as defcls]])

(defn emit-let
  [env context ^GeneratorAdapter gen [the-let bindings & body :as form]]
  (let [names (mapv #(vector (first %) (. gen newLocal object-type)) bindings)
        vals (mapv second bindings)
        let-env (update-in env [:locals] merge (into {} names))]
    (dotimes [n (count names)]
      (let [[sym local-id] (nth names n)
            val-expr (nth vals n)]
        (binding [*lambda-name* (if (= the-let 'letrec) sym *lambda-name*)]
          (emit let-env :context/expression gen val-expr))
        (. gen storeLocal local-id)))
    (emit-body let-env context gen body)))

(defmethod emit-seq 'let
  [env context ^GeneratorAdapter gen [_ bindings & body :as form]]
  (emit-let env context gen form))

(defmethod emit-seq 'letrec
  [env context ^GeneratorAdapter gen letrec]
  (emit-let env context gen letrec))

(defmethod emit-seq 'quote
  [env context ^GeneratorAdapter gen [_ quoted :as form]]
  (when (> (count form) 2)
    (throw (IllegalArgumentException. "quote takes exactly one argument")))
  (emit-value context gen quoted))

(defmethod emit-seq 'begin
  [env context ^GeneratorAdapter gen [_ & exprs :as form]]
  (emit-body env context gen exprs))

(defn update-global-handle
  ^CallSite [sym ^MethodHandle handle]
  (if-let [^CallSite s (get-in @global-bindings [sym :call-site])]
    (doto s (.setTarget handle))
    (VolatileCallSite. handle)))

(defn set-global!
  [sym value]
  (cast Symbol sym)
  (let [handle (MethodHandles/constant Object value)
        site (update-global-handle sym handle)]
    (swap! global-bindings assoc sym {:call-site site}))
  value)

(defn macro-handle
  [sym]
  (. (MethodHandles/throwException Object RuntimeException)
     (bindTo (RuntimeException. (str "Can't take the value of a macro: " sym)))))

(defn set-macro!
  [sym value]
  (cast Symbol sym)
  (cast ILambda value)
  (let [handle (macro-handle sym)
        site (update-global-handle sym handle)]
    (swap! global-bindings assoc sym {:macro value
                                      :call-site site}))
  sym)

(defn add-inline!
  [sym value]
  (cast Symbol sym)
  (cast ILambda value)
  (when-not (contains? @global-bindings sym)
    (throw (IllegalStateException. (str sym " is not an existing global binding"))))
  (swap! global-bindings assoc-in [sym :macro] value)
  sym)

(defn add-function!
  [sym value]
  (cast Symbol sym)
  (cast ILambda value)
  (when-not (get-in @global-bindings [sym :macro])
    (throw (IllegalStateException. (str sym " is not an existing macro"))))
  (swap! global-bindings
         assoc-in [sym :call-site]
         (update-global-handle sym (MethodHandles/constant Object value)))
  value)

(defmethod emit-seq 'define
  [env context ^GeneratorAdapter gen [_ name val :as form]]
  (when (> (count form) 3)
    (throw (IllegalArgumentException. "define only takes one value")))
  (binding [*lambda-name* name]
    (set-global! name (eval* val)))
  (emit env context gen name))

(defmethod emit-seq 'define-macro
  [env context ^GeneratorAdapter gen [_ name val :as form]]
  (when (> (count form) 3)
    (throw (IllegalArgumentException. "define-macro only takes one value")))
  (binding [*lambda-name* name]
    (set-macro! name (eval* val)))
  (emit env context gen name))

(defmethod emit-seq 'if
  [env context ^GeneratorAdapter gen [_ condition then else :as the-if]]
  (debug "emit-seq if" the-if context)
  (when-not (<= 3 (count the-if) 4)
    (throw (IllegalArgumentException. "if takes 2 or 3 forms")))
  (let [false-label (. gen newLabel)
        end-label (. gen newLabel)]
                                        ; continuation must always be exploded for condition
    (binding [*return-continuations* false]
      (cond
                                        ; optimize common case (if (null? ...) ...)
       (and (list-like? condition) (= (first condition) 'null?))
       (do
         (when (not= (count condition) 2)
           (throw (IllegalArgumentException. "null? takes exactly one argument")))
         (emit env :context/expression gen (second condition))
         (. gen ifNonNull false-label))

                                        ; optimize common case (if (not (null? ...)) ...)
       (and (list-like? condition) (= (first condition) 'not)
            (list-like? (second condition)) (= (first (second condition)) 'null?))
       (do
         (when (not= (count condition) 2)
           (throw (IllegalArgumentException. "not takes exactly one argument")))
         (when (not= (count (second condition)) 2)
           (throw (IllegalArgumentException. "null? takes exactly one argument")))
         (emit env :context/expression gen (second (second condition)))
         (. gen ifNull false-label))

                                        ; optimize common case (if (eq? ...) ...)
       (and (list-like? condition) (= (first condition) 'eq?))
       (do
         (when (not= (count condition) 3)
           (throw (IllegalArgumentException. "eq? takes exactly 2 arguments")))
         (let [[_ a b] condition]
           (emit env :context/expression gen a)
           (emit env :context/expression gen b)
           (. gen ifCmp object-type GeneratorAdapter/NE false-label)))

                                        ; optimize common case (if (not (eq? ...)) ...)
       (and (list-like? condition) (= (first condition) 'not)
            (list-like? (second condition)) (= (fnext condition) 'eq?))
       (do
         (when (not= (count condition) 2)
           (throw (IllegalArgumentException. "not takes exactly one argument")))
         (when (not= (count (second condition)) 3)
           (throw (IllegalArgumentException. "eq? takes exactly two arguments")))
         (let [[_ a b] (second condition)]
           (emit env :context/expression gen a)
           (emit env :context/expression gen b)
           (. gen ifCmp object-type GeneratorAdapter/EQ false-label)))

                                        ; optimize common case (if (not ...) ...)
       (and (list-like? condition) (= (first condition) 'not))
       (do
         (when (not= (count condition) 2)
           (throw (IllegalArgumentException. "not takes exactly one argument")))
         (emit env :context/expression gen (second condition))
         (. gen getStatic boolean-object-type "FALSE" boolean-object-type)
         (. gen ifCmp boolean-object-type GeneratorAdapter/NE false-label))

       :else
       (do
         (emit env :context/expression gen condition)
         (. gen getStatic boolean-object-type "FALSE" boolean-object-type)
         (. gen ifCmp boolean-object-type GeneratorAdapter/EQ false-label))))
    (emit env context gen then)
    (. gen goTo end-label)
    (. gen mark false-label)
    (emit env context gen else)
    (. gen mark end-label)))

(defn emit-var
  [^GeneratorAdapter gen sym]
  (. gen push (namespace sym))
  (. gen push (name sym))
  (. gen invokeStatic (asmtype RT) (Method/getMethod "clojure.lang.Var var(String,String)")))

(declare emit-jvm)
(defmethod emit-seq :jvm
  [env context ^GeneratorAdapter gen [_ & insns]]
  (let [labeled-env (assoc env :labels (atom {}))]
    (doseq [i insns]
      (emit-jvm labeled-env context gen i)))
  (if (= context :context/statement)
    (. gen pop)))

(defn emit-explode-continuation
  [^GeneratorAdapter gen]
  (let [precheck (. gen newLabel)
        no-cont-label (. gen newLabel)]
    (. gen mark precheck)
    (. gen dup)
    (. gen instanceOf (asmtype wombat.Continuation))
    (. gen ifZCmp GeneratorAdapter/EQ no-cont-label)
    (. gen checkCast (asmtype wombat.Continuation))
    (. gen invokeVirtual (asmtype wombat.Continuation) (Method/getMethod "Object invoke()"))
    (. gen goTo precheck)
    (. gen mark no-cont-label)))

(declare no-continuation-lambda explode-invocation)

(defmulti explode-special first)
(defmethod explode-special :default
  [form] form)
(defmethod explode-special 'if
  [[_ & forms]]
  (list* 'if (map explode-invocation forms)))

(defn explode-let
  [the-let binds body]
  (list* the-let
         (seq->list (map (fn [bind val]
                           (list bind
                                 (explode-invocation val)))
                         binds))
         (map explode-invocation body)))

(defmethod explode-special 'let
  [the-let binds & body]
  (explode-let the-let binds body))

(defmethod explode-special 'letrec
  [letrec binds & body]
  (explode-let letrec binds body))

(defmethod explode-special 'define
  [_ sym val]
  (list 'define sym (explode-invocation val)))
(defmethod explode-special 'define-macro
  [_ sym val]
  (list 'define-macro sym (explode-invocation val)))
(defmethod explode-special 'begin
  [_ & body]
  (list* 'begin (map explode-invocation body)))

(defn explode-invocation
  [listy]
  (if (list-like? listy)
    (letfn [(explode [v]
              (list :jvm
                    (list :emit v)
                    (list :explode-continuation)))]
      (loop [lis listy
             new-list []]
        (if (seq lis)
          (if (list-like? (first lis))
            (cond
             (contains? specials (ffirst lis))
             (recur (rest lis) (conj new-list (explode-special (first lis))))

             ; keyword is either :jvm or clojure interop, no explosion
             (keyword? (ffirst lis))
             (recur (rest lis) (conj new-list (first lis)))

             :else
             (recur (rest lis) (conj new-list (explode (explode-invocation (first lis))))))
            (recur (rest lis) (conj new-list (first lis))))
          (seq->list (seq new-list)))))
    listy))

(defn explodey-lambda
  [[fun & args :as call]]
  (let [res (sanitize-name "result")]
    (list 'lambda ()
          (list 'let (list (list res (explode-invocation call)))
                res))))

(defn arity-match?
  [{:keys [restarg params arglists] :as env} args]
  (if restarg
    (>= (count args) (min-variadic-arity arglists))
    (= (count args) (count params))))

(defn emit-tail-call
  [{:keys [recur-sym restarg top-label params locals] :as env} ^GeneratorAdapter gen [fun & args :as call]]
  (if (and (= fun recur-sym)
           (arity-match? env args))
                                        ; Self call, becomes loop
    (do
      (debug "emitting self tail-call: " call)
                                        ; emit all new values
      (dotimes [n (count params)]
        (emit env :context/expression gen (nth args n)))
      (when restarg
        (emit-list-from-array gen
                              #(let [extra (drop (count params) args)]
                                 (AsmUtil/pushInt gen (count extra))
                                 (. gen newArray object-type)
                                 (dotimes [n (count extra)]
                                   (. gen dup)
                                   (AsmUtil/pushInt gen n)
                                   (emit env :context/expression gen (nth extra n))
                                   (. gen arrayStore object-type))))
        (. gen storeLocal (get locals restarg)))
                                        ; store them in reverse order from stack
      (dotimes [n (count params)]
        (. gen storeArg (- (count params) n 1)))
      (. gen goTo top-label))
                                        ; Calls another fn, return continuation thunk
    (do
      (debug "emitting continuation:" call)
      (binding [*lambda-name* (symbol (str recur-sym "_CONT"))
                *return-continuations* true]
        (. gen newInstance (asmtype wombat.Continuation))
        (. gen dup)
        (emit env :context/expression gen (explodey-lambda call))
        (. gen invokeConstructor (asmtype wombat.Continuation) (Method/getMethod "void <init>(Object)"))))))

(defn emit-invoke
  [env context ^GeneratorAdapter gen [fun & args :as call]]
  (if (keyword? fun)
    (let [fn-sym (symbol (or (namespace fun) "clojure.core") (name fun))]
      (debug "keyword invoke: " fun)
      (when-not (clean-resolve fn-sym)
        (throw (IllegalArgumentException. (str "Unable to resolve clojure symbol " fn-sym))))

      (emit-var gen fn-sym)
      (doseq [a args]
        (emit env :context/expression gen a))
      (. gen invokeVirtual (asmtype clojure.lang.Var) (Method. "invoke" object-type (into-array Type (repeat (count args) object-type)))))
    (do
      (emit env :context/expression gen fun)
      (. gen dup)
      (. gen checkCast ilambda-type)
      (AsmUtil/pushInt gen (count args))
      (. gen invokeInterface ilambda-type (Method/getMethod "java.lang.invoke.MethodHandle getHandle(int)"))
      (. gen swap)

      (doseq [a args]
        (emit env :context/expression gen a))
      (. gen invokeVirtual (asmtype MethodHandle)
         (Method. "invoke" object-type (into-array Type (core/cons ilambda-type (repeat (count args) object-type)))))
      
      (when-not *return-continuations*
        (debug "*** NOT RETURNING CONTINUATION" call)
        (emit-explode-continuation gen))
      (when *return-continuations*
        (debug "*** RETURNING CONTINUATION" call))))
  (when (= context :context/statement)
    (. gen pop)))

(defmethod emit-seq -invoke-
  [env context ^GeneratorAdapter gen [fun & args :as call]]
  (when (nil? fun)
    (throw (IllegalArgumentException. "Can't invoke '()")))
  (debug "emitting invoke:" call)
  (debug "context:" context)

  (if (and (= context :context/return)
           (not (keyword? fun))) ;; No TCO for calls into clojure
    (emit-tail-call env gen call)
    (emit-invoke env context gen call)))

(defn compile-and-load
  ^Class [form]
  (debug "COMPILE-AND-LOAD: " form)
  (binding [*top-level* 'eval]
    (let [[name bytecode] (compile form)]
      (.defineClass *class-loader* name bytecode form)
      (Class/forName name true *class-loader*))))

(defn no-continuation-lambda
  [form]
  (let [gs (sanitize-name 'result)]
    (list 'lambda ()
          (list 'let (list (list gs form))
                gs))))

(defn eval*
  "Low-level eval call. Requires a pre-sanitized input."
  [form]
  (debug "EVAL*: " form)
  (cond
   (and (list-like? form) (= (first form) 'define))
   (binding [*current-define* (second form)
             *lambda-name* (second form)]
     (set-global! (second form) (eval* (first (nnext form)))))

   (and (list-like? form) (= (first form) 'define-macro))
   (binding [*current-define* (second form)
             *lambda-name* (second form)]
     (set-macro! (second form) (eval* (first (nnext form)))))

   (and (list-like? form) (= (first form) 'define-class*))
   (set-global! (second form) (compile-and-load form))

   :else
   (.. (compile-and-load (no-continuation-lambda form)) newInstance invoke)))

(defn eval
  "Top-level eval call. Initializes env and sanitizes input."
  [form]
  (binding [*compiled-lambdas* (atom #{})]
    (eval* (sanitize {} form))))

(defn load-file
  [file]
  (binding [*print-debug* false
            *class-loader* (DynamicClassLoader. *class-loader*)]
    (let [file-reader (FileReader. file)
          pb-reader (LineNumberingPushbackReader. file-reader)
          eof (Object.)]
      (loop []
        (let [f (read pb-reader false eof)]
          (when-not (identical? f eof)
            (eval f)
            (recur)))))))

(load "compiler_defclass")
