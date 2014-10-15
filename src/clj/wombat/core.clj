(ns wombat.core
  (:require [wombat.compiler :refer [scheme-eval] :as compiler])
  (:import [java.lang.invoke MethodHandles CallSite]))

(defn bootstrap []
  (compiler/load-file "src/scm/core.scm"))

(defn print-exception
  [^Exception e]
  (binding [*out* *err*]
    (println (str (.getName (class e)) ": " (.getMessage e)))))

(defonce -bad-input- (Object.))
(defn repl
  []
  (binding [compiler/*print-debug* false]
    (doseq [expr '[*1 *2 *3 *e]]
      (scheme-eval (list 'define expr)))
    (let [last-err (atom nil)]
      (loop []
        (printf "wombat> ")
        (.flush *out*)
        (let [f (try (read)
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
               (let [val (scheme-eval f)]
                 (println val)
                 (.flush *out*)
                 (binding [compiler/*print-debug* false]
                   (scheme-eval '(define *3 *2))
                   (scheme-eval '(define *2 *1))
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
