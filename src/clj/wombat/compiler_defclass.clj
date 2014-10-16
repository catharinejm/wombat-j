(in-ns 'wombat.compiler)

(defn sanitize-jvm
  [env [insn & args :as form]]
  (if (keyword? insn)
    (list* insn (map (partial sanitize env) args))
    form))

(defn assert-arity!
  [form arity]
  (when-not (= (count form) (inc arity))
    (throw (IllegalArgumentException. (str (first form) " directive takes exactly " arity " argument" (if (= 1 arity) "." "s."))))))

(defn assert-min-arity!
  [form min-arity]
  (when-not (>= (count form) (inc min-arity))
    (throw (IllegalArgumentException. (str (first form) " directive takes at least " min-arity " argument" (if (= 1 min-arity) "." "s."))))))

(defn assert-range-arity!
  [form min-arity max-arity]
  (when-not (<= (inc min-arity) (count form) (inc max-arity))
    (throw (IllegalArgumentException. (str (first form) " directive takes " min-arity " to " max-arity " arguments.")))))

(defonce -emit-jvm-default- (Object.))
(defmulti emit-jvm
  (fn [_ _ _ insn] (first insn))
  :default -emit-jvm-default-)

(defmethod emit-jvm -emit-jvm-default-
  [env context gen insns]
  (throw (IllegalArgumentException. (str "unrecognized jvm form: " insns))))

(defmethod emit-jvm :emit
  [env context gen [_ val :as form]]
  (assert-arity! form 1)
  (emit env :context/expression gen val))

(defmethod emit-jvm :invoke
  [env context gen [_ fun & args :as form]]
  (assert-min-arity! form 1)
  (emit-seq env :context/expression gen (cons fun args)))

(defmethod emit-jvm 'checkCast
  [env context gen [_ class :as form]]
  (assert-arity! form 1)
  (. gen checkCast (asmtype (clean-resolve class))))

(defmethod emit-jvm 'loadThis
  [env context gen form]
  (assert-arity! form 0)
  (. gen loadThis))

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

(defmethod emit-jvm :str
  [env context gen [_ val :as form]]
  (assert-arity! form 1)
  (emit-symbol env :context/expression gen val)
  (. gen invokeVirtual object-type (method '(String "toString"))))

(defmethod emit-jvm :thistype
  [{thistype :thistype} context gen form]
  (assert-arity! form 0)
  (. gen push thistype))

(defmethod emit-jvm 'getField
  [{thistype :thistype :as env} context gen [_ name type :as form]]
  (assert-arity! form 2)
  (. gen loadThis)
  (. gen getField thistype name (resolve-asm type)))

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
  (if (= type 'boolean)
    (let [false-label (. gen newLabel)
          end-label (. gen newLabel)]
      (. gen ifZCmp GeneratorAdapter/EQ false-label)
      (. gen getStatic boolean-object-type "TRUE" boolean-object-type)
      (. gen goTo end-label)
      (. gen mark false-label)
      (. gen getStatic boolean-object-type "FALSE" boolean-object-type)
      (. gen mark end-label))
    (. gen box (resolve-asm type))))

(defmethod emit-jvm 'unbox
  [env context gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen unbox (resolve-asm type)))

(defmethod emit-jvm 'push
  [env context gen [_ type val :as form]]
  (assert-arity! form 2)
  (condp #(%1 %2) type
    '#{int} (AsmUtil/pushInt gen val)
    '#{long} (AsmUtil/pushLong gen val)
    '#{float} (AsmUtil/pushFloat gen val)
    '#{double} (AsmUtil/pushDouble gen val)
    '#{boolean} (. gen push (.booleanValue val))
    '#{Class java.lang.Class} (. gen push (resolve-asm val))
    '#{String java.lang.String} (. gen push (cast String val))
    (throw (IllegalArgumentException. (str "Cannot push type " type)))))
