(define load-file
  (lambda (file)
    (:wombat.compiler/load-file file)))

(define list (lambda elems elems))

(define car
  (lambda (lis)
    (:clojure.core/first lis)))

(define cdr
  (lambda (lis)
    (:clojure.core/rest lis)))

(define cons
  (lambda (a d)
    (:clojure.core/conj d a)))

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
