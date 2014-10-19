(ns wombat.core
  (:require [wombat.compiler :as compiler]
            [wombat.reader :as reader]
            [wombat.printer :refer [write]])
  (:import [java.lang.invoke MethodHandles CallSite]))

(defn bootstrap
  ([] (bootstrap false))
  ([throw?]
     (try (compiler/load-file "src/scm/core.scm")
          (catch Exception e
            (if throw?
              (throw e)
              (binding [*out* *err*]
                (println "bootstrap failed...")
                (println (.getMessage e))
                (.printStackTrace e *out*)))))))

(defn print-exception
  [^Exception e]
  (binding [*out* *err*]
    (println (str (.getName (class e)) ": " (.getMessage e)))))

(defonce -bad-input- (Object.))
(defn repl
  []
  (binding [compiler/*print-debug* false]
    (doseq [expr '[*1 *2 *3 *e]]
      (compiler/eval (list 'define expr)))
    (let [last-err (atom nil)]
      (loop []
        (printf "wombat> ")
        (.flush *out*)
        (let [f (try (reader/read *in*)
                     (catch Throwable e
                       (reset! last-err e)
                       (print-exception e)
                       -bad-input-))]
          (cond
           (= f :debug)
           (do
             (println (str (if compiler/*print-debug* "Disabling" "Enabling") " debug..."))
             (set! compiler/*print-debug* (not compiler/*print-debug*))
             (recur))

           (= f :print-err)
           (do
             (if @last-err
               (.printStackTrace @last-err)
               (println "no exception to print"))
             (recur))

           (identical? f -bad-input-)
           (recur)
           
           (not= f :quit)
           (do
             (try
               (let [val (compiler/eval f)]
                 (write val *out*)
                 (.write *out* "\n")
                 (.flush *out*)
                 (binding [compiler/*print-debug* false]
                   (compiler/eval '(define *3 *2))
                   (compiler/eval '(define *2 *1))
                   (compiler/set-global! '*1 val)))
               (catch Throwable e
                 (reset! last-err e)
                 (binding [compiler/*print-debug* false]
                   (compiler/set-global! '*e e))
                 (print-exception e)))
             (recur))

           :else
           (println "Exiting...")))))))

(defn -main
  [& args]
  (bootstrap)
  (repl))
