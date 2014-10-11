(ns wombat.scheme
  (:import [org.objectweb.asm ClassWriter ClassVisitor Opcodes Type]
           [org.objectweb.asm.commons GeneratorAdapter Method]
           [clojure.lang DynamicClassLoader Compiler RT])
  (:refer-clojure :except [compile]))

(def -unbound- (Object.))

(def ^:dynamic *bindings* {})
(def ^:dynamic *binding-exprs* {})
(def ^:dynamic *closes*)
(def ^:dynamic *containing-scope*)

(declare analyze analyze-seq compile emit)

(defmulti analyze
  (fn [form] (type form)))

(defmethod analyze :default
  [form]
  (throw (IllegalArgumentException. (str "Cannot analyze form " form))))

(defmethod analyze clojure.lang.ISeq
  [form]
  (analyze-seq form))

(defmethod analyze Number
  [form]
  {:env *binding-exprs*
   :type :number
   :children [:value]
   :value form
   :constant true})

(defmethod analyze String
  [form]
  {:env *binding-exprs*
   :type :string
   :children [:value]
   :value form
   :constant true})

(defmethod analyze Character
  [form]
  {:env *binding-exprs*
   :type :character
   :children [:value]
   :value form
   :constant true})

(defmethod analyze Boolean
  [form]
  {:env *binding-exprs*
   :type :boolean
   :children [:value]
   :value form
   :constant true})

(defmethod analyze clojure.lang.Symbol
  [form]
  (let [ksym (keyword form)
        val (or (get-in *binding-exprs* [ksym :value-expr])
                (throw (IllegalArgumentException. "Cannot resolve symbol " form)))]
    (when *closes*
      (swap! *closes* conj ksym))
    val))

(defmulti analyze-seq
  {:default :invoke}
  (fn [form] (first form)))

(defn lambda-name [l]
  (str "wombat/scheme/" (:name l)))

(defn lambda-type [l]
  (Type/getObjectType (lambda-name l)))

(defmethod analyze-seq 'lambda
  [[_ [& parms] & body]]
  (doseq [p parms]
    (when-not (symbol? p)
      (throw (IllegalArgumentException. (str "Lambda parameters must be symbols")))))
  (let [kparms (map keyword parms)
        fn-env (reduce (fn [lcls kp]
                         (assoc lcls kp {:type :symbol
                                         :children [:symbol :value-expr :funarg]
                                         :funarg true
                                         :symbol kp
                                         :value-expr -unbound-}))
                       *binding-exprs* kparms)
        closes (atom [])
        [body-statements ret-expr] (binding [*binding-exprs* fn-env
                                             *closes* closes]
                                     [(map analyze (butlast body))
                                      (analyze (last body))])
        lambda-expr {:env *binding-exprs*
                     :type :lambda
                     :children [:params :statements :return :closes :name]
                     :name (str "lambda_" (RT/nextID))
                     :params kparms
                     :statements body-statements
                     :return ret-expr
                     :closes (remove (set parms) closes)}]
    (compile lambda-expr)
    (. @Compiler/LOADER defineClass (.replace (lambda-name lambda-expr) "/" ".") (.toByteArray cw) nil)
    lambda-expr))

(defmethod analyze-seq :invoke
  [[the-fn & args]]
  (let [fn-expr (or (analyze the-fn)
                    (throw (IllegalArgumentException. "Cannot invoke nil")))
        arg-exprs (map analyze args)]
    {:env *binding-exprs*
     :type :invoke
     :children [:param-exprs]
     :param-exprs arg-exprs}))

(defmulti compile
  (fn [ast] (:type ast)))

(defmethod compile :default
  [ast]
  (throw (IllegalArgumentException. (str "Can't compile " (:type ast)))))

(def callsites (atom {}))

(def object-type (Type/getType Object))

(defn close-name [c]
  (str "close_" (name c)))

(defmethod compile :lambda
  [ast]
  (binding [*containing-scope* ast]
    (let [cw (ClassWriter. ClassWriter/COMPUTE_MAXS)]
      (. cw (visit Opcodes/V1_7 Opcodes/ACC_PUBLIC
                   (lambda-name ast) nil "java/lang/Object" (make-array String 0)))
      
      (doseq [c (:closes ast)]
        (. cw visitField 0 (close-name c) (.getDescriptor object-type) nil nil))

      (let [ctor (Method. "<init>" Type/VOID_TYPE
                          (into-array Type (repeat (count (:closes ast)) object-type)))
            gen (GeneratorAdapter. Opcodes/ACC_PUBLIC ctor nil nil cw)]
        (. gen visitCode)
        
        (. gen loadThis)
        (. gen invokeConstructor object-type (Method/getMethod "void <init>()"))

        (. gen loadThis)
        (dotimes [n (count (:closes ast))]
          (. gen visitVarInsn (. object-type getOpcode Opcodes/ILOAD) (inc n))
          (. gen putField (lambda-type ast) (close-name (nth (:closes ast) n)) object-type))

        (. gen returnValue)
        (. gen endMethod))
      
      (let [m (Method. "invoke" object-type
                       (into-array Type (repeat (count (:params ast)) object-type)))
            gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)]
        (. gen visitCode)

        (doseq [s (:statements ast)]
          (emit s :statement gen))
        (emit :return (:return ast) gen)

        (. gen returnValue)
        (. gen endMethod))))
  nil)

(defmulti emit
  (fn [ast context gen] (:type ast)))

(defmethod emit :default
  [ast & rest]
  (throw (IllegalArgumentException. "Cannot emit " (:type ast))))

(defn emit-local
  [ast lname gen]
  (cond
   (some #{lname} (:closes ast))
   (. gen getField (lambda-type ast) (close-name lname) object-type)

   (some #{lname} (:params ast))
   (. gen visitVarInsn (. object-type getOpcode Opcodes/ILOAD)
      (inc (.indexOf ^java.util.List (:params ast) lname)))

   :else
   (throw (IllegalStateException. (str "Value " lname " not found in enclosing scope")))))

(defmethod emit :lambda
  [ast context gen]
  (let [objtype (lambda-type ast)]
    (. gen newInstance objtype)
    (. gen dup)
    (doseq [c (:closes ast)]
      (emit-local *containing-scope* c gen))
    (. gen invokeConstructor objtype (Method. "<init>" Type/VOID_TYPE
                                              (into-array Type (repeat (count (:closes ast)) object-type))))
    (when (= context :statement)
      (. gen pop))))

(defmethod emit :number
  [ast context gen]
  (. gen push (:value ast))
  (when (= context :statement)
    (. gen pop)))

(defmethod emit :string
  [ast context gen]
  (. gen push (:value ast))
  (when (= context :statement)
    (. gen pop)))

(defmethod emit :boolean
  [ast context gen]
  (. gen push (:value ast))
  (when (= context :statement)
    (. gen pop)))

(defmethod emit :character
  [ast context gen]
  (. gen push (:value ast))
  (when (= context :statement)
    (. gen pop)))
