(ns wombat.printer
  (:require [wombat.datatypes :refer :all])
  (:import [java.io Writer]
           [wombat.datatypes List Pair Vector])
  (:refer-clojure :exclude [cons list? vector?]))
(alias 'core 'clojure.core)

(defmulti write-obj
  (fn [f w] (class f)))

(defmethod write-obj String
  [^String string ^Writer w]
  (print-method string w))

(defmethod write-obj clojure.lang.Symbol
  [^clojure.lang.Symbol sym ^Writer w]
  (print-method sym w))

(defmethod write-obj clojure.lang.Keyword
  [^clojure.lang.Keyword kw ^Writer w]
  (.write w "#")
  (print-method kw w))

(defn write-list
  [lis ^Writer w]
  (when (seq lis)
    (write-obj (first lis) w)
    (loop [l (rest lis)]
      (when (seq l)
        (.write w " ")
        (write-obj (first l) w)
        (recur (rest l))))))

(defmethod write-obj List
  [^List lis ^Writer w]
  (.write w "(")
  (write-list lis w)
  (.write w ")"))

(defmethod write-obj Pair
  [^Pair p ^Writer w]
  (.write w "(")
  (write-list (.front p) w)
  (.write w " . ")
  (write-obj (.end p) w)
  (.write w ")"))

(defmethod write-obj Vector
  [^Vector vec ^Writer w]
  (.write w "#(")
  (write-list vec w)
  (.write w ")"))

(defmethod write-obj Boolean
  [^Boolean b ^Writer w]
  (if b
    (.write w "#t")
    (.write w "#f")))

(defmethod write-obj nil
  [_ ^Writer w]
  (.write w "'()"))

(defmethod write-obj Character
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

(defmethod write-obj Number
  [^Number n ^Writer w]
  (.write w (.toString n)))

(defmethod write-obj :default
  [o ^Writer w]
  (print-method o w))

(defn write
  ([o] (write-obj o *out*))
  ([o w] (write-obj o w)))

(defn write-str
  [o]
  (let [sw (java.io.StringWriter.)]
    (write-obj o sw)
    (.toString sw)))
