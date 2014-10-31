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
  [env context ^GeneratorAdapter gen insns]
  (throw (IllegalArgumentException. (str "unrecognized jvm form: " (write-str insns)))))

(defmethod emit-jvm :emit
  [env context ^GeneratorAdapter gen [_ val :as form]]
  (assert-arity! form 1)
  (emit env :context/expression gen val))

(defmethod emit-jvm :invoke
  [env context ^GeneratorAdapter gen [_ fun & args :as form]]
  (assert-min-arity! form 1)
  (emit-seq env :context/expression gen (cons fun args)))

(defmethod emit-jvm :macro
  [env context ^GeneratorAdapter gen [_ macro & args :as form]]
  (assert-min-arity! form 1)
  (emit-jvm env context gen (expand (cons macro args))))

(defn add-label
  [{labels :labels} ^GeneratorAdapter gen lname]
  (let [l (. gen newLabel)]
    (swap! labels assoc lname l)
    l))

(defn label
  [{labels :labels :as env} ^GeneratorAdapter gen lname]
  (or (get @labels lname)
      (add-label env gen lname)))

(defmethod emit-jvm 'ifNull
  [env context ^GeneratorAdapter gen [_ lname :as form]]
  (assert-arity! form 1)
  (. gen ifNull (label env gen lname)))

(def cmp-ops
  {'= GeneratorAdapter/EQ
   'not= GeneratorAdapter/NE
   '< GeneratorAdapter/LT
   '<= GeneratorAdapter/LE
   '> GeneratorAdapter/GT
   '>= GeneratorAdapter/GE})

(defn get-op
  [op]
  (or (cmp-ops op)
      (throw (IllegalArgumentException. (str "Invalid comparison op: " op)))))

(defn maybe-prim-resolve
  [sym]
  (if (and (list-like? sym) (= (count sym) 1))
    (class (make-array (maybe-prim-resolve (first sym)) 0))
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
      (clean-resolve sym))))

(defn resolve-asm
  [cls]
  (asmtype (maybe-prim-resolve cls)))

(defmethod emit-jvm :jvm
  [env context ^GeneratorAdapter gen [_ & forms]]
  (doseq [f forms]
    (emit-jvm env context gen f)))

(defmethod emit-jvm 'ifCmp
  [env context ^GeneratorAdapter gen [_ type op lname :as form]]
  (assert-arity! form 3)
  (. gen ifCmp (resolve-asm type) (get-op op) (label env gen lname)))

(defmethod emit-jvm 'ifZCmp
  [env context ^GeneratorAdapter gen [_ op lname :as form]]
  (assert-arity! form 2)
  (. gen ifZCmp (get-op op) (label env gen lname)))

(defmethod emit-jvm 'label
  [{:keys [labels] :as env} context ^GeneratorAdapter gen [_ lname :as form]]
  (assert-arity! form 1)
  (. gen mark (label env gen lname)))

(defmethod emit-jvm 'goto
  [{:keys [labels] :as env} context ^GeneratorAdapter gen [_ lname :as form]]
  (assert-arity! form 1)
  (. gen goTo (label env gen lname)))

(defmethod emit-jvm 'checkCast
  [env context ^GeneratorAdapter gen [_ class :as form]]
  (assert-arity! form 1)
  (. gen checkCast (resolve-asm class)))

(defmethod emit-jvm 'loadThis
  [env context ^GeneratorAdapter gen form]
  (assert-arity! form 0)
  (. gen loadThis))

(defn method
  [[ret name & params]]
  (Method. name (resolve-asm ret) (into-array Type (map resolve-asm params))))

(defmethod emit-jvm :str
  [env context ^GeneratorAdapter gen [_ val :as form]]
  (assert-arity! form 1)
  (emit-symbol env :context/expression gen val)
  (. gen invokeVirtual object-type (method '(String "toString"))))

(defmethod emit-jvm :thistype
  [{thistype :thistype} context ^GeneratorAdapter gen form]
  (assert-arity! form 0)
  (. gen push thistype))

(defmethod emit-jvm :explode-continuation
  [env context gen form]
  (assert-arity! form 0)
  (emit-explode-continuation gen))

(defmethod emit-jvm 'newInstance
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen newInstance (resolve-asm type)))

(defmethod emit-jvm 'invokeConstructor
  [env context ^GeneratorAdapter gen [_ owner sig :as form]]
  (assert-arity! form 2)
  (. gen invokeConstructor (resolve-asm owner) (method sig)))

(defmethod emit-jvm 'invokeInterface
  [env context ^GeneratorAdapter gen [_ owner sig :as form]]
  (assert-arity! form 2)
  (. gen invokeInterface (resolve-asm owner) (method sig)))

(defmethod emit-jvm 'invokeStatic
  [env context ^GeneratorAdapter gen [_ owner sig :as form]]
  (assert-arity! form 2)
  (. gen invokeStatic (resolve-asm owner) (method sig)))

(defmethod emit-jvm 'invokeVirtual
  [env context ^GeneratorAdapter gen [_ owner sig :as form]]
  (assert-arity! form 2)
  (. gen invokeVirtual (resolve-asm owner) (method sig)))

(defmethod emit-jvm 'getStatic
  [env context ^GeneratorAdapter gen [_ owner fname ftype :as form]]
  (assert-arity! form 3)
  (. gen getStatic (resolve-asm owner) fname (resolve-asm ftype)))

(defmethod emit-jvm 'putStatic
  [env context ^GeneratorAdapter gen [_ owner fname ftype :as form]]
  (assert-arity! form 3)
  (. gen putStatic (resolve-asm owner) fname (resolve-asm ftype)))

(defmethod emit-jvm 'getField
  [env context ^GeneratorAdapter gen [_ owner fname ftype :as form]]
  (assert-arity! form 3)
  (. gen getField (resolve-asm owner) fname (resolve-asm ftype)))

(defmethod emit-jvm 'putField
  [env context ^GeneratorAdapter gen [_ owner fname ftype :as form]]
  (assert-arity! form 3)
  (. gen putField (resolve-asm owner) fname (resolve-asm ftype)))

(defmethod emit-jvm 'instanceOf
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen instanceOf (resolve-asm type)))

(defmethod emit-jvm 'throwException
  [env context ^GeneratorAdapter gen [_ type? msg? :as form]]
  (when-not (or (= 1 (count form))
                (= 3 (count form)))
    (throw (IllegalArgumentException. "throwException directive takes 0 or 2 arguments")))
  (if type?
    (. gen throwException (resolve-asm type?) msg?)
    (. gen throwException)))

(defmethod emit-jvm 'box
  [env context ^GeneratorAdapter gen [_ type :as form]]
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
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen unbox (resolve-asm type)))

(defmethod emit-jvm 'push
  [env context ^GeneratorAdapter gen [_ type val :as form]]
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

(defmethod emit-jvm 'pop
  [env context ^GeneratorAdapter gen form]
  (assert-arity! form 0)
  (. gen pop))

(defmethod emit-jvm 'dup
  [env context ^GeneratorAdapter gen form]
  (assert-arity! form 0)
  (. gen dup))

(defmethod emit-jvm 'dupX1
  [env context ^GeneratorAdapter gen form]
  (assert-arity! form 0)
  (. gen dupX1))

(defmethod emit-jvm 'dupX2
  [env context ^GeneratorAdapter gen form]
  (assert-arity! form 0)
  (. gen dupX2))

(defmethod emit-jvm 'swap
  [env context ^GeneratorAdapter gen form]
  (assert-arity! form 0)
  (. gen swap))

(defmethod emit-jvm 'add
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/ADD (resolve-asm type)))

(defmethod emit-jvm 'sub
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/SUB (resolve-asm type)))

(defmethod emit-jvm 'mul
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/MUL (resolve-asm type)))

(defmethod emit-jvm 'div
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/DIV (resolve-asm type)))

(defmethod emit-jvm 'rem
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/REM (resolve-asm type)))

(defmethod emit-jvm 'neg
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/NEG (resolve-asm type)))

(defmethod emit-jvm 'shl
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/SHL (resolve-asm type)))

(defmethod emit-jvm 'shr
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/SHR (resolve-asm type)))

(defmethod emit-jvm 'ushr
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/USHR (resolve-asm type)))

(defmethod emit-jvm 'and
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/AND (resolve-asm type)))

(defmethod emit-jvm 'or
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/OR (resolve-asm type)))

(defmethod emit-jvm 'xor
  [env context ^GeneratorAdapter gen [_ type :as form]]
  (assert-arity! form 1)
  (. gen math GeneratorAdapter/XOR (resolve-asm type)))

(defn plain-sym?
  [sym]
  (not (or (namespace sym)
           (.contains (name sym) "."))))

(declare modifiers)
(defn compute-modifiers
  [mods]
  (reduce #(+ %1 (or (modifiers %2)
                     (throw (IllegalArgumentException. (str "Invalid modifier: " %2)))))
          mods))

(defn parse-fields
  [fields]
  (letfn [(validate! [sym]
            (when-not (and (symbol? sym) (plain-sym? sym))
                            (throw (IllegalArgumentException. (str "invalid field name: " sym))))
            sym)
          (parse [f]
            (if (list-like? f)
              (parse-seq f)
              (let [sym (validate! f)]
                {:sym sym :name (munge (name sym)) :type Object :modifiers ['public]})))
          (parse-seq [[f type & mods]]
            (doseq [m mods]
              (when-not (contains? modifiers m)
                (throw (IllegalArgumentException. (str "Invalid modifer: " m)))))
            (let [type (maybe-prim-resolve type)
                  sym (validate! f)]
              {:sym sym :name (munge (name sym)) :type type :modifiers mods}))]
    (mapv parse fields)))

(defmethod compile 'define-class*
  [[_ cname fields interfaces & methods :as defclass]]

  (throw (IllegalStateException. "define-class* isn't implemented. Oops."))
  
  ;; (debug "Compiling: " defclass)
  ;; (when (or (namespace cname)
  ;;           (.contains (name cname) "."))
  ;;   (throw (IllegalArgumentException. "class name cannot contain \"/\" or \".\"")))
  ;; (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)
  ;;       fqname (str "wombat/" (munge cname))
  ;;       dotname (.replace fqname "/" ".")
  ;;       parsed-fields (parse-fields fields)
  ;;       env {:thistype (Type/getObjectType fqname)}]
  ;;   (. cw visit Opcodes/V1_7 Opcodes/ACC_PUBLIC fqname nil "java/lang/Object" (make-array String 0)))
  )
