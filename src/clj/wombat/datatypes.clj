(ns wombat.datatypes
  (:refer-clojure :exclude [cons list? vector?]))

(defprotocol ICons
  (-cons [c o])
  (-car [c])
  (-cdr [c]))

(defn assert-pair!
  [o]
  (when-not (instance? wombat.datatypes.ICons o)
    (throw (IllegalArgumentException. "Exptected pair"))))

(defn car
  [o]
  (assert-pair! o)
  (-car o))

(defn cdr
  [o]
  (assert-pair! o)
  (-cdr o))

(defprotocol IList
  (-length [l]))

(defn assert-list!
  [o]
  (when-not (instance? wombat.datatypes.IList o)
    (throw (IllegalArgumentException. "Expected list"))))

(defn length
  [o]
  (assert-list! o)
  (-length o))

(deftype List [head tail cnt]
  ICons
  (-cons [l o]
    (List. o l (inc cnt)))
  (-car [l] head)
  (-cdr [l] tail)
  IList
  (-length [l] cnt)
  clojure.lang.Seqable
  (seq [l]
    (loop [l l
           s []]
      (if l
        (recur (cdr l) (conj s (car l)))
        (seq s)))))

(deftype Pair [front end]
  ICons
  (-cons [p o]
    (Pair. (-cons front o) end))
  (-car [p]
    (car front))
  (-cdr [p]
    (if (-cdr front)
      (Pair. (-cdr front) end)
      end)))

(defprotocol IVector
  (-vector-length [v])
  (-vector-ref [v i])
  (-vector-set! [v i o]))

(defn assert-vector!
  [o]
  (when-not (instance? wombat.datatypes.IVector o)
    (throw (IllegalArgumentException. "Expected vector"))))

(defn vector-length
  [v]
  (assert-vector! v)
  (-vector-length v))

(defn vector-ref
  [v i]
  (assert-vector! v)
  (-vector-ref v i))

(defn vector-set!
  [v i o]
  (assert-vector! v)
  (-vector-set! v i o))

(deftype Vector [ary]
  IVector
  (-vector-length [v]
    (alength ary))
  (-vector-ref [v i]
    (aget ary i))
  (-vector-set! [v i o]
    (aset ary i o))
  clojure.lang.Seqable
  (seq [v] (seq ary)))

(defn cons
  [h t]
  (cond
   (nil? t)
   (List. h nil 1)

   (instance? wombat.datatypes.ICons t)
   (-cons t h)

   :else
   (Pair. (List. h nil 1) t)))

(defn list?
  [o]
  (or (nil? o) (instance? wombat.datatypes.IList o)))

(defn pair?
  [o]
  (or (instance? wombat.datatypes.ICons o)))

(defn vector?
  [o]
  (instance? wombat.datatypes.IVector o))

(defn javalist->list
  ([^java.util.List lis] (javalist->list lis nil false))
  ([^java.util.List lis fail-if-dot?] (javalist->list lis nil fail-if-dot?))
  ([^java.util.List lis init fail-if-dot?]
     (let [iter (.listIterator lis (.size lis))]
       (loop [res init]
         (if (.hasPrevious iter)
           (let [o (.previous iter)]
             (when (and fail-if-dot? (= o '.))
               (throw (IllegalArgumentException. "Unexpected `.' in list!")))
             (recur (cons o res)))
           res)))))
