(ns wombat.core
  (:require [wombat.compiler :refer [scheme-eval] :as compiler]))

(defn print-exception
  [^Exception e]
  (binding [*out* *err*]
    (println (.getMessage e))))

(defonce -bad-input- (Object.))

(defn repl
  []
  (binding [compiler/*print-debug* false]
    (let [last-err (atom nil)]
      (loop []
        (printf "wombat> ")
        (.flush *out*)
        (let [f (try (read)
                     (catch Exception e
                       (reset! last-err e)
                       (print-exception e)
                       -bad-input-))]
          (cond
           (= f :debug)
           (do
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
               (println (scheme-eval f))
               (.flush *out*)
               (catch Exception e
                 (reset! last-err e)
                 (print-exception e)))
             (recur))

           :else
           (println "Exiting...")))))))

(defn -main
  [& args]
  (repl))
