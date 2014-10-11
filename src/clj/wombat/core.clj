(ns wombat.core
  (:import [org.objectweb.asm ClassWriter ClassVisitor Opcodes Type]
           [org.objectweb.asm.commons GeneratorAdapter Method]
           [clojure.lang DynamicClassLoader Compiler RT])
  (:refer-clojure :except [compile]))

(def ^:dynamic *bindings* {})

(def -unbound- (Object.))

(declare analyze* analyze-seq* compile*)


(defmacro analyze
  [context env form]
  (analyze* form))

(defmulti analyze*
  (fn [_ _ form] (type form)))

(defmethod analyze* :default
  [context env form]
  (throw (IllegalArgumentException. (str "cannot analyze form " form))))

(defmethod analyze* nil nil)

(defmethod analyze* clojure.lang.ISeq
  [context env form]
  (analyze-seq* context env form))

(defmethod analyze* Number
  [context env form]
  {:env env
   :type :number
   :children [:value]
   :value form
   :constant true})

(defmethod analyze* String
  [context env form]
  {:env env
   :type :string
   :children [:value]
   :value form
   :constant true})

(defmethod analyze* Character
  [context env form]
  {:env env
   :type :character
   :children [:value]
   :value form
   :constant true})

(defmethod analyze* Boolean
  [context env form]
  {:env env
   :type :boolean
   :children [:value]
   :value form
   :constant true})

(defmethod analyze* clojure.lang.Symbol
  [context {locals :locals :as env} form]
  (or (get-in locals [form :value-expr])
      (throw (IllegalArgumentException. "Cannot resolve symbol " form))))

(defmulti analyze-seq*
  {:default :invoke}
  (fn [_ _ form] (first form)))

(defmethod analyze-seq* 'let
  [_ bindings]
  (when-not (list? bindings)
    (throw (IllegalArgumentException. "let bindings must be in a list")))
  (when-not (every? #(and (list? %) (= (count %) 2)) bindings)
    (throw (IllegalArgumentException. "let bindings must be lists of exactly two elements")))
  nil)

(defmethod analyze-seq* 'lambda
  [context env [_ [& parms] & body]]
  (when-not (= context :statement)
    (let [_ (doseq [p parms]
              (when-not (symbol? p)
                (throw (IllegalArgumentException. (str "lambda parameters must be symbols")))))
          fn-env (update-in env [:locals] #(reduce
                                            (fn [lcls parm]
                                              (assoc lcls parm {:type :symbol
                                                                :children [:symbol :value-expr :funarg]
                                                                :funarg true
                                                                :symbol parm
                                                                :value-expr -unbound-}))
                                            % parms))
          body-statements (map (partial analyze* :statement fn-env) (butlast body))
          ret-expr (analyze* :return fn-env (last body))]
      {:env env
       :type :lambda
       :children [:params :statements :return]
       :params parms
       :statements body-statements
       :return ret-expr})))

(defmethod analyze-seq* :invoke
  [context env [the-fn & args]]
  (let [fn-expr (analyze* context env the-fn)]
    (when-not fn-expr
      (throw (IllegalArgumentException. "Cannot invoke nil.")))
    (let [arg-exprs (map (partial analyze* :expression env) args)
          invoke-env (assoc env :locals
                            (reduce (fn [locals [sym arg]]
                                      (assoc-in env [sym :value-expr] arg))
                                    (:locals env) (map vector (:params fn-expr) arg-exprs)))]
      {:env invoke-env
       :type :invoke
       })))

(comment
  (def cw (ClassWriter. ClassWriter/COMPUTE_MAXS))
  (. cw (visit Opcodes/V1_5 Opcodes/ACC_PUBLIC
               "wombat/core/TestClass" nil "java/lang/Object" (make-array String 0)))

  (let [m (Method. "turd" Type/VOID_TYPE (into-array Type [(Type/getType String)]))
        gen (GeneratorAdapter. (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                               m nil nil cw)]
    (.visitCode gen)
    (. gen push "clojure.core")
    (. gen push "*out*")
    (. gen invokeStatic (Type/getType clojure.lang.RT) (. Method getMethod "clojure.lang.Var var(String,String)"))
    (. gen invokeVirtual (Type/getType clojure.lang.Var) (. Method getMethod "java.lang.Object get()"))
    (. gen dup)
    (. gen checkCast (Type/getType java.io.PrintWriter))
    (. gen loadArg 0)
    (. gen invokeVirtual (Type/getType java.io.PrintWriter) (. Method getMethod "void println(String)"))
    (.returnValue gen)
    (.endMethod gen))

  (.visitEnd cw)

  (. @Compiler/LOADER (defineClass "wombat.core.TestClass" (.toByteArray cw) nil))

  (import 'wombat.core.TestClass))
