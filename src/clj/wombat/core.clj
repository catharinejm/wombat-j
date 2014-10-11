(ns wombat.core
  (:require [wombat.compiler :refer [scheme-eval]]))

(defn repl
  []
  (loop []
    (printf "wombat> ")
    (.flush *out*)
    (let [f (read)]
      (when-not (= f 'quit)
        (try
          (println (scheme-eval f))
          (catch Exception e
            (println (.getMessage e))))
        (recur)))))

(defn -main
  [& args]
  (repl))
