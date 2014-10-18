(ns wombat.printer
  (:import [java.io Writer]))

(defmulti write
  (fn [f w] (class f)))

(defmethod write String
  [^String string ^Writer w]
  (print-method string w))

(defmethod write clojure.lang.Symbol
  [^clojure.lang.Symbol sym ^Writer w]
  (print-method sym w))

(defmethod write clojure.lang.Keyword
  [^clojure.lang.Keyword kw ^Writer w]
  (.write w "#")
  (print-method kw w))

(defn write-list
  [lis ^Writer w]
  (when (seq lis)
    (write (first lis) w)
    (loop [l (rest lis)]
      (when (seq l)
        (.write w " ")
        (write (first l) w)
        (recur (rest l))))))

(defmethod write clojure.lang.ISeq
  [^clojure.lang.ISeq s ^Writer w]
  (.write w "(")
  (write-list s w)
  (.write w ")"))

(defmethod write (class (object-array 0)) ; Scheme vector
  [^objects svec ^Writer w]
  (.write w "#(")
  (write-list svec w)
  (.write w ")"))

(defmethod write Boolean
  [^Boolean b ^Writer w]
  (if b
    (.write w "#t")
    (.write w "#f")))

(defmethod write nil
  [_ ^Writer w]
  (.write w "'()"))

(defmethod write Character
  [^Character c ^Writer w]
  (let [char-names {(char 0) "null"
                    \backspace "backspace"
                    \tab "tab"
                    \newline "newline"
                    (char 11) "vtab"
                    \formfeed "page"
                    \return "return"
                    \space "space"
                    (char 127) "rubout"}]
    (.write w "#\\")
    (.write w (get char-names c (.toString c)))))

(defmethod write Number
  [^Number n ^Writer w]
  (.write w (.toString n)))
