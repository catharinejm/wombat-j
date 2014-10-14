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

(define namespace
  (lambda (named)
    (:jvm (:emit named)
          (checkCast clojure.lang.Named)
          (invokeInterface clojure.lang.Named (String "getNamespace")))))

(define name
  (lambda (named)
    (:jvm (:emit named)
          (checkCast clojure.lang.Named)
          (invokeInterface clojure.lang.Named (String "getName")))))

(define get-var
  (lambda (sym)
    (:jvm (:invoke namespace sym)
          (checkCast String)
          (:invoke name sym)
          (checkCast String)
          (invokeStatic clojure.lang.RT (clojure.lang.Var "var" String String)))))

(define class
  (lambda (obj)
    (:jvm (:emit obj)
          (invokeVirtual Object (Class "getClass")))))

(define instance?
  (lambda (cls o)
    (:jvm (:emit cls)
          (checkCast Class)
          (:emit o)
          (invokeVirtual Class (boolean "isInstance" Object))
          (box boolean))))
