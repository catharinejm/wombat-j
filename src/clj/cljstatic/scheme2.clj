(ns cljstatic.scheme2
  (:require [clojure.core.match :refer [match]])
  (:import [org.objectweb.asm ClassWriter ClassVisitor Opcodes Type Handle]
           [org.objectweb.asm.commons GeneratorAdapter Method]
           [clojure.lang DynamicClassLoader Compiler RT]
           [java.lang.invoke MethodType CallSite MethodHandles$Lookup]
           [cljstatic ALambda AsmUtil])
  (:refer-clojure :exclude [compile]))

(def ^:dynamic *class-loader* (DynamicClassLoader.))

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
     
     (['quote val] :seq) val

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
         emit-string)

(defn asmtype ^Type [^Class cls] (Type/getType cls))
(def object-type (asmtype Object))
(def boolean-object-type (asmtype Boolean))
(def alambda-type (asmtype ALambda))
(def void-ctor (Method/getMethod "void <init>()"))

(defn close-name
  [fsym]
  (str "close_" (munge (name fsym))))

(defn local-name
  [lsym]
  (str "local_" (munge (name lsym))))

(defn gen-fields
  [^ClassVisitor cv closed-overs]
  (doseq [c closed-overs]
    (. cv visitField 0 (close-name c) (.getDescriptor object-type) nil nil)))

(defn gen-ctor
  [cw fv]
  (let [ctor (Method. "<init>" Type/VOID_TYPE (into-array Type (repeat (count fv) object-type)))
        gen (GeneratorAdapter. Opcodes/ACC_PUBLIC ctor nil nil cw)]
    (. gen visitCode)
    (. gen loadThis)
    (. gen invokeConstructor alambda-type void-ctor)
    (dotimes [n (count fv)]
      (. gen loadThis)
      (. gen loadArg n)
      (. gen putField (close-name (nth fv n)) object-type))
    (. gen returnValue)
    (. gen endMethod)))

(defn emit-body
  [env context gen exprs]
  (doseq [stmt (butlast exprs)]
    (emit env :context/statement gen stmt))
  (emit env context gen (last exprs)))

(defn gen-body
  [cw {:keys [params] :as env} body]
  (println "gen-body" body)
  (let [m (Method. "invoke" object-type (into-array Type (repeat (count params) object-type)))
        gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)]
    (. gen visitCode)
    (emit-body env :context/return gen body)
    (. gen returnValue)
    (. gen endMethod)))

(defn compile
  "Compiles a given sanitized lambda form. If the input is not already
  sanitized, the results are undefined!

  Returns a vector of [<class name>, <closed-overs>, <bytecode>]"
  [[_ params & body :as lambda]]
  (println "Compiling:" lambda)
  (let [fv (vec (free-vars lambda))
        cw (ClassWriter. ClassWriter/COMPUTE_MAXS)
        lname (str "lambda_" (next-id))
        fqname (str "cljstatic/scheme2/" lname)
        _ (println "fqname:" fqname)
        dotname (.replace fqname "/" ".")
        ifaces (make-array String 0)
        env {:params params, :closed-overs fv, :locals {}}]
    (. cw visit Opcodes/V1_7 Opcodes/ACC_PUBLIC fqname nil "cljstatic/ALambda" ifaces)
    (gen-fields cw fv)
    (gen-ctor cw fv)
    (gen-body cw env body)
    [dotname fv (.toByteArray cw)]))

(defn emit
  [env context gen form]
  (cond
   (seqable? form)
   (emit-seq env context gen form)

   (symbol? form)
   (emit-symbol env context gen form)

   :else
   (emit-value context gen form)))

(defn emit-symbol
  [{:keys [params closed-overs locals] :as env} context gen sym]
  (cond
   ((set params) sym)
   (. gen loadArg (.indexOf params sym))

   ((set closed-overs) sym)
   (do
     (. gen loadThis)
     (. gen getField object-type (close-name sym) object-type))

   ((set (keys locals)) sym)
   (. gen loadLocal (get locals sym))

   :else
   (throw (IllegalStateException. (str "Symbol " sym " is not defined"))))
  (when (= context :context/statement)
    (. gen pop)))

(defn emit-value
  [context gen const]
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
  [obj]
  (throw (IllegalArgumentException. (str "cannot emit-dup of " obj))))

(defmethod emit-dup Integer
  [gen ^Integer int]
  (println "emit-dup int" int)
  (AsmUtil/pushInt gen int)
  (. gen invokeStatic (asmtype Integer) (Method/getMethod "Integer valueOf(int)")))

(defmethod emit-dup Long
  [gen ^Long long]
  (println "emit-dup long" long)
  (AsmUtil/pushLong gen long)
  (. gen invokeStatic (asmtype Long) (Method/getMethod "Long valueOf(long)")))

(defmethod emit-dup Float
  [gen ^Float float]
                                        ; Cast up to double
  (println "emit-dup float" float)
  (. gen push (.doubleValue float))
  (. gen invokeStatic (asmtype Double) (Method/getMethod "Double valueOf(double)")))

(defmethod emit-dup Double
  [gen ^Double double]
  (println "emit-dup double" double)
  (. gen push (.doubleValue double))
  (. gen invokeStatic (asmtype Double) (Method/getMethod "Double valueOf(double)")))

(defmethod emit-dup Character
  [gen ^Character char]
  (println "emit-dup char" char)
  (. gen push (.charValue char))
  (. gen invokeStatic (asmtype Character) (Method/getMethod "Character valueOf(char)")))

(defmethod emit-dup String
  [gen ^String string]
  (println "emit-dup string" string)
  (. gen push string))

(defmethod emit-dup clojure.lang.Symbol
  [gen ^clojure.lang.Symbol sym]
  (println "emit-dup symbol" sym)
  (. gen push (.ns sym))
  (. gen push (.name sym))
  (. gen invokeStatic (asmtype clojure.lang.Symbol) (Method/getMethod "clojure.lang.Symbol intern(String,String)")))

(defmethod emit-dup clojure.lang.Seqable
  [gen ^clojure.lang.Seqable lis]
  (println "emit-dup list" lis)
  (. gen push (count lis))
  (. gen newArray object-type)
  (dotimes [n (count lis)]
    (. gen dup)
    (AsmUtil/pushInt gen n)
    (emit-value :context/expression gen (nth lis n))
    (. gen arrayStore object-type))
  (. gen invokeStatic (asmtype java.util.Arrays) (Method/getMethod "java.util.List asList(Object[])"))
  (. gen invokeStatic (asmtype clojure.lang.PersistentList) (Method/getMethod "clojure.lang.IPersistentList create(java.util.List)")))


(defonce -invoke- (Object.))
(defmulti emit-seq
  (fn [_ _ _ form] (first form))
  :default -invoke-)

(defmethod emit-seq 'lambda
  [env context gen [_ params & body :as lambda]]
  (let [[dotname closed-overs bytecode] (compile lambda)]
    (. *class-loader* defineClass dotname bytecode lambda)
    (let [slashname (.replace dotname "." "/")]
      (. gen newInstance (Type/getObjectType slashname))
      (. gen dup)
      (dotimes [n (count closed-overs)]
        (emit-symbol env :context/expression gen (nth closed-overs n)))
      (. gen invokeConstructor (Type/getObjectType slashname)
         (Method. "<init>" Type/VOID_TYPE (into-array Type (repeat (count closed-overs) object-type))))))
  (when (= context :context/statement)
    (. gen pop)))

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
  [& args]
  (throw (Exception. "No define yet")))

(defmethod emit-seq 'if
  [env context gen [_ condition then else :as the-if]]
  (when-not (<= 3 (count the-if) 4)
    (throw (IllegalArgumentException. "if takes 2 or 3 forms")))
  (let [null-label (. gen newLabel)
        false-label (. gen newLabel)
        end-label (. gen newLabel)]
    (emit env :context/expression gen condition)
    (. gen dup)
    (. gen ifNull null-label)
    (. gen getStatic boolean-object-type "FALSE" boolean-object-type)
    (. gen ifCmp boolean-object-type false-label)
    (emit env context gen then)
    (. gen goTo end-label)
    (. gen mark null-label)
    (. gen pop) ; pop dup of condition created for false check
    (. gen mark false-label)
    (emit env context gen else)
    (. gen mark end-label)))

(def bootstrap-method-type (MethodType/methodType CallSite MethodHandles$Lookup (into-array Class [String MethodType])))
(def bootstrap-handle (Handle. Opcodes/H_INVOKESTATIC
                               "cljstatic/ALambda"
                               "bootstrap"
                               (.toMethodDescriptorString bootstrap-method-type)))
(defmethod emit-seq -invoke-
  [env context gen [fun & args :as call]]
  (println "emitting invoke:" call)
  (println "context:" context)
  (let [mt (MethodType/methodType Object ALambda (into-array Class (repeat (count args) Object)))]
    (emit env :context/expression gen fun)
    (. gen dup)
    (. gen invokeVirtual alambda-type (Method/getMethod "void registerForBootstrap()"))
    (doseq [a args]
      (emit env :context/expression gen a))
    (. gen invokeDynamic "invoke" (.toMethodDescriptorString mt) bootstrap-handle (make-array Object 0))
    (when (= context :context/statement)
      (. gen pop))))


(defn scheme-eval
  [form]
  (let [[name _ bytecode] (compile (sanitize {} `(~'lambda () '~form)))]
    (.defineClass *class-loader* name bytecode form)
    (.. (Class/forName name true *class-loader*) newInstance invoke)))
