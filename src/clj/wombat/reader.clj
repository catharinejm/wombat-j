(ns wombat.reader
  (:require [clojure.java.io :as io]
            [wombat.datatypes :as wd :refer [javalist->list]])
  (:import [clojure.lang LineNumberingPushbackReader LispReader$ReaderException
            PersistentList Symbol Keyword BigInt Numbers]
           [java.io FileReader Reader]
           [java.util ArrayList]
           [java.math BigInteger BigDecimal]
           [wombat.datatypes List Pair Vector])
  (:refer-clojure :exclude [read read-string]))

(defn ^LispReader$ReaderException reader-exception
  [^LineNumberingPushbackReader rdr ^Throwable cause]
  (LispReader$ReaderException. (.getLineNumber rdr) (.getColumnNumber rdr) cause))

(declare read* read-form read-dispatch-form)
(defn read
  ([rdr] (read rdr false nil false))
  ([rdr eof-error?] (read rdr eof-error? nil false))
  ([rdr eof-error? eof] (read rdr eof-error? eof false))
  ([rdr eof-error? eof recursive?]
     (let [^LineNumberingPushbackReader rdr (if-not (instance? LineNumberingPushbackReader rdr)
                                              (LineNumberingPushbackReader. (io/reader rdr))
                                              rdr)]
       (read* rdr eof-error? eof recursive?))))

(defn kill-whitespace
  [^LineNumberingPushbackReader rdr]
  (loop [c (.read rdr)]
    (if (Character/isWhitespace c)
      (recur (.read rdr))
      c)))

(defn read*
  ([^LineNumberingPushbackReader rdr eof-error? eof recursive?]
     (when-not (instance? LineNumberingPushbackReader rdr)
       (throw (RuntimeException. "read* must be called with a LineNumberingPushbackReader")))
     (let [c (kill-whitespace rdr)]
       (if (= c -1)
         (if eof-error?
           (throw (RuntimeException. "EOF while reading"))
           eof)
         (let [form (try
                      (read-form rdr c)
                      (catch Throwable t
                        (if recursive?
                          (throw t)
                          (throw (reader-exception rdr t)))))]
           (if (identical? rdr form)
             (recur rdr eof-error? eof recursive?)
             form))))))

(def form-tokens
  {\" :string
   \' :quote
   \` :quote
   \, :unquote
   \( :list
   \) :unmatched
   \[ :list
   \] :unmatched
   \{ :list
   \} :unmatched
   \; :comment
   \# :dispatch})

(def dispatch-tokens
  {\\ :char
   \: :keyword
   \( :vector
   \; :discard
   \! :special-symbol
   \d :decimal
   \o :octal
   \x :hex
   \b :binary
   \# :internal-symbol})

(defn unread
  [^LineNumberingPushbackReader rdr c]
  (when-not (= c -1)
    (.unread rdr (int c))))

(defn end-of-token?
  [ch]
  (or (= ch -1)
      (Character/isWhitespace (int ch))
      (and (contains? form-tokens (char ch))
           (not (#{\# \' \` \,} (char ch))))))

(defn read-token
  [^LineNumberingPushbackReader rdr c]
  (let [sb (StringBuilder.)]
    (.append sb (char c))
    (loop []
      (let [c (.read rdr)]
        (if-not (end-of-token? c)
          (do
            (.append sb (char c))
            (recur))
          (unread rdr c))))
    (.toString sb)))

(def int-re #"([-+]?)(?:(0)|([1-9][0-9]*))")
(def float-re #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)")
(def ratio-re #"([-+]?[0-9]+)/([0-9]+)")

(defn match-number
  [numstr]
  (condp re-matches numstr
    int-re (let [bn (BigInteger. numstr 10)]
             (if (< (.bitLength bn) 64)
               (Numbers/num (.longValue bn))
               (BigInt/fromBigInteger bn)))
    
    float-re (Double/parseDouble numstr)

    ratio-re :>> (fn [[_ numerator denominator]]
                   (Numbers/divide (BigInteger. numerator) (BigInteger. denominator)))))

(defn read-number
  [^LineNumberingPushbackReader rdr c]
  (let [tok (read-token rdr c)]
    (or (match-number tok)
        (throw (NumberFormatException. (str "Invalid number: " tok))))))

(defmulti read-form
  (fn [rdr c] (form-tokens (char c))))

(defmethod read-form :default
  [^LineNumberingPushbackReader rdr c]
  (cond
   (Character/isDigit (int c))
   (read-number rdr c)
   
   (#{\+ \-} (char c))
   (let [next-c (.read rdr)]
     (unread rdr next-c)
     (if (Character/isDigit next-c)
       (read-number rdr c)
       (Symbol/intern (read-token rdr c))))

   :else
   (Symbol/intern (read-token rdr c))))

(defmethod read-form :string
  [^LineNumberingPushbackReader rdr c]
  (let [sb (StringBuilder.)
        escape-char (fn [c]
                      (or ({\t \tab
                            \r \return
                            \n \newline
                            \\ \\
                            \" \"
                            \b \backspace
                            \f \formfeed} (char c))
                          (throw (RuntimeException. (str "Unsupported escape character: \\" (char c))))))]
    (loop []
      (let [c (.read rdr)]
        (when (= -1 c)
          (throw (RuntimeException. "EOF while reading string")))
        (when-not (= (char c) \")
          (if (= (char c) \\)
                                        ; escaped
            (let [c (.read rdr)]
              (when (= -1 c)
                (throw (RuntimeException. "EOF while reading string")))
              (.append sb (escape-char c))
              (recur))
                                        ; not escaped
            (do
              (.append sb (char c))
              (recur))))))
    (.toString sb)))

(defmethod read-form :quote
  [^LineNumberingPushbackReader rdr c]
  (let [form (read* rdr true nil true)
        quote ({\' 'quote \` 'quasiquote} (char c))]
    (wd/list quote form)))

(defmethod read-form :unquote
  [^LineNumberingPushbackReader rdr c]
  (let [c (.read rdr)]
    (cond
     (= -1 c)
     (throw (RuntimeException. "EOF while reading"))

     (= (char c) \@)
     (wd/list 'unquote-splicing (read* rdr true nil true))

     :else
     (do
       (unread rdr c)
       (wd/list 'unquote (read* rdr true nil true))))))

(defn ^java.util.List read-to-delimiter
  [^LineNumberingPushbackReader rdr close-delim]
  (let [start-line (.getLineNumber rdr)
        lis (ArrayList.)]
    (loop []
      (let [c (.read rdr)]
        (when (= -1 c)
          (throw (RuntimeException. (str "EOF while reading list"
                                         (when (> -1 start-line)
                                           (str ", starting at line " start-line))))))
        (cond
         (Character/isWhitespace c)
         (recur)

         (and (contains? form-tokens (char c)) (not= (char c) close-delim))
         (do
           (let [frm (read-form rdr c)]
             (when-not (identical? rdr frm)
               (.add lis frm)))
           (recur))

         (not= (char c) close-delim)
         (do
           (.unread rdr c)
           (let [frm (read* rdr true nil true)]
             (.add lis frm))
           (recur)))))
    lis))

(defmethod read-form :unmatched
  [rdr delim]
  (throw (RuntimeException. (str "Unmatched delimiter: " (char delim)))))

(defn interpret-list
  [^java.util.List lis]
  (if (.isEmpty lis)
    nil
    (if (> (.size lis) 2)
      (let [end (.get lis (dec (.size lis)))
            dot? (.get lis (- (.size lis) 2))]
        (when (= end '.)
          (throw (IllegalArgumentException. "Unexpected `.' in list!")))
        (if (= dot? '.)
          (let [sublis (.subList lis 0 (- (.size lis) 2))]
            (if (wd/list? end)
              (javalist->list sublis end true)
              (Pair. (javalist->list sublis true) end)))
          (javalist->list lis true)))
      (javalist->list lis true))))

(defmethod read-form :list
  [^LineNumberingPushbackReader rdr c]
  (let [close-delim ({\( \), \[ \], \{ \}} (char c))]
    (when-not close-delim
      (throw (RuntimeException. (str "Unmatchable list delimiter: " (char c)))))
    (let [line (.getLineNumber rdr)
          col (dec (.getColumnNumber rdr))
          lis (interpret-list (read-to-delimiter rdr close-delim))]
      ;; (when (and lis (> line -1))
      ;;   (add-meta lis {:line line :column col}))
      lis)))

(defmethod read-form :comment
  [^LineNumberingPushbackReader rdr c]
  (while (let [c (.read rdr)]
           (and (not= c -1)
                (not= (char c) \newline)
                (not= (char c) \return))))
  rdr)

(defmethod read-form :dispatch
  [^LineNumberingPushbackReader rdr c]
  (let [c (.read rdr)]
    (when (= -1 c)
      (throw (RuntimeException. "EOF while reading character")))
    (read-dispatch-form rdr c)))

(defmulti read-dispatch-form
  (fn [rdr c] (dispatch-tokens (char c))))

(defmethod read-dispatch-form :char
  [^LineNumberingPushbackReader rdr c]
  (let [c (.read rdr)]
    (when (= -1 c)
      (throw (RuntimeException. "EOF while reading character")))
    (let [tok (read-token rdr c)
          named-chars {"nul" (char 0)
                       "null" (char 0)
                       "backspace" \backspace
                       "tab" \tab
                       "newline" \newline
                       "linefeed" \newline
                       "vtab" (char 11)
                       "page" \formfeed
                       "return" \return
                       "space" \space
                       "rubout" (char 127)}]
      (cond
       (= (count tok) 1)
       (Character/valueOf (.charAt tok 0))

       (contains? named-chars tok)
       (named-chars tok)

       :else
       (throw (RuntimeException. (str "Unsupported character: #\\" tok)))))))

(defmethod read-dispatch-form :keyword
  [^LineNumberingPushbackReader rdr c]
  (let [c (.read rdr)]
    (when (= -1 c)
      (throw (RuntimeException. "EOF while reading keyword")))
    (Keyword/intern (read-token rdr c))))

(defmethod read-dispatch-form :vector
  [^LineNumberingPushbackReader rdr c]
  (let [lis (read-to-delimiter rdr \) )]
    (Vector. (.toArray lis))))

(defmethod read-dispatch-form :discard
  [^LineNumberingPushbackReader rdr c]
  (read* rdr true nil true)
  rdr)

(defmethod read-dispatch-form :special-symbol
  [^LineNumberingPushbackReader rdr c]
  (let [c (.read rdr)]
    (when (= -1 c)
      (throw (RuntimeException. "EOF while reading #! form")))
    (let [specials #{"rest"
                     ;;"optional"
                     } ; not sure what else right now
          tok (read-token rdr c)]
      (if (contains? specials tok)
        (symbol (str "#!" (specials tok)))
        (throw (RuntimeException. (str "Invalid #! token: " tok)))))))

(defmethod read-dispatch-form :internal-symbol
  [^LineNumberingPushbackReader rdr c]
  (let [c (.read rdr)]
    (when (= -1 c)
      (throw (RuntimeException. "EOF while reading symbol")))
    (symbol (str "##" (read-token rdr c)))))

(defmethod read-dispatch-form :default
  [^LineNumberingPushbackReader rdr c]
  (let [tok (read-token rdr c)]
    (cond
     (= tok "t")
     true

     (= tok "f")
     false

     :else
     (throw (RuntimeException. (str "Invalid # token: " tok))))))

(defn read-integer
  [^LineNumberingPushbackReader rdr radix]
  (let [c (.read rdr)]
    (when (= -1 c)
      (throw (RuntimeException. "EOF while reading number")))
    (let [bn (BigInteger. (read-token rdr c) radix)]
      (if (< (.bitLength bn) 64)
        (Numbers/num (.longValue bn))
        (BigInt/fromBigInteger bn)))))

(defmethod read-dispatch-form :decimal
  [^LineNumberingPushbackReader rdr c]
  (read-integer rdr 10))

(defmethod read-dispatch-form :octal
  [^LineNumberingPushbackReader rdr c]
  (read-integer rdr 8))

(defmethod read-dispatch-form :hex
  [^LineNumberingPushbackReader rdr c]
  (read-integer rdr 16))

(defmethod read-dispatch-form :binary
  [^LineNumberingPushbackReader rdr c]
  (read-integer rdr 2))

(defn read-string
  [^String str]
  (read (java.io.StringReader. str)))
