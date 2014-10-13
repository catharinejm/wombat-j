(define car
  (lambda (lis)
    (:clojure.core/first lis)))

(define cdr
  (lambda (lis)
    (:clojure.core/rest lis)))

(define cons
  (lambda (a d)
    (:clojure.core/cons a d)))

(define next :clojure.core/next)
(next '(1 2))
