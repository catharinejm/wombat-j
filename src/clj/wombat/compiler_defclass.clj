(in-ns 'wombat.compiler)

(defn sanitize-jvm
  [env [insn & args :as form]]
  (if (keyword? insn)
    (list* insn (map (partial sanitize env) args))
    form))

(defn assert-arity!
  [form arity]
  (when-not (= (count form) (inc arity))
    (throw (IllegalArgumentException. (str (first form) " directive takes exactly " arity " argument" (if (= 1 arity) "." "s.")
                                           " Extras: " (drop (inc arity) form))))))

(defn assert-min-arity!
  [form min-arity]
  (when-not (>= (count form) (inc min-arity))
    (throw (IllegalArgumentException. (str (first form) " directeive takes at least " min-arity " argument" (if (= 1 min-arity) "." "s."))))))

(defonce -emit-jvm-default- (Object.))
(defmulti emit-jvm
  (fn [_ _ _ insn] (first insn))
  :default -emit-jvm-default-)

(defmethod emit-jvm -emit-jvm-default-
  [_ _ _ insn]
  (throw (IllegalArgumentException. (str "unrecognized jvm form: " insn))))

(defmethod emit-jvm :emit
  [env context gen [_ sym :as form]]
  (assert-arity! form 1)
  (emit-symbol env :context/expression gen sym))

(defmethod emit-jvm :invoke
  [env context gen [_ fun & args :as form]]
  (assert-min-arity! form 1)
  (emit-seq env :context/expression gen (cons fun args)))

(defmethod emit-jvm 'checkCast
  [env context gen [_ class :as form]]
  (assert-arity! form 1)
  (. gen checkCast (Type/getType (clean-resolve class))))

(defn maybe-prim-resolve
  [sym]
  (condp = sym
    'int Integer/TYPE
    'ints (class (int-array 0))
    'long Long/TYPE
    'longs (class (long-array 0))
    'float Float/TYPE
    'floats (class (float-array 0))
    'double Double/TYPE
    'doubles (class (double-array 0))
    'short Short/TYPE
    'shorts (class (short-array 0))
    'char Character/TYPE
    'chars (class (char-array 0))
    'byte Byte/TYPE
    'bytes (class (byte-array 0))
    'boolean Boolean/TYPE
    'booleans (class (boolean-array 0))
    'void Void/TYPE
    (clean-resolve sym)))

(defn resolve-asm
  [cls]
  (let [resolved (cond
                  (and (seqable? cls) (= (count cls) 1))
                  (class (make-array (maybe-prim-resolve (first cls)) 0))

                  (symbol? cls)
                  (maybe-prim-resolve cls)

                  :else
                  (throw (IllegalArgumentException. (str "Invalid type annotation: " cls))))]
    (asmtype resolved)))

(defn method
  [[ret name & params]]
  (Method. name (resolve-asm ret) (into-array Type (map resolve-asm params))))

(defmethod emit-jvm 'invokeInterface
  [env context gen [_ owner sig :as form]]
  (assert-arity! form 2)
  (. gen invokeInterface (resolve-asm owner) (method sig)))

(defmethod emit-jvm 'invokeStatic
  [env context gen [_ owner sig :as form]]
  (assert-arity! form 2)
  (. gen invokeStatic (resolve-asm owner) (method sig)))

(defmethod emit-jvm 'invokeVirtual
  [env context gen [_ owner sig :as form]]
  (assert-arity! form 2)
  (. gen invokeVirtual (resolve-asm owner) (method sig)))

(defmethod emit-jvm 'box
  [env context gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen box (resolve-asm type)))
