(define load-file
  (lambda (file)
    (:wombat.compiler/load-file file)))

(define list (lambda elems elems))

(define car
  (lambda (lis)
    (:clojure.core/first lis)))

(define cdr
  (lambda (lis)
    (:clojure.core/next lis)))

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

(define string->symbol
  (lambda (str)
    (:jvm (:emit str)
          (checkCast String)
          (invokeStatic clojure.lang.Symbol (clojure.lang.Symbol "intern" String)))))

(define str
  (lambda (val)
    (:jvm (:str val))))

(define conc
  (lambda (s1 s2)
    (:jvm (:str s1)
          (:str s2)
          (invokeVirtual String (String "concat" String)))))

(define eval
  (lambda (form)
    (:wombat.compiler/eval* form)))

(define eqv?
  (lambda (a b)
    (:jvm (:emit a)
          (:emit b)
          (invokeVirtual Object (boolean "equals" Object))
          (box boolean))))

(define gensym
  (lambda ()
    (string->symbol (conc "G__#" (str (:wombat.compiler/next-id))))))

(define map
  (lambda (f lis)
    (if lis
      (cons (f (car lis)) (map f (cdr lis)))
      '())))

(define foldl
  (lambda (f init vals)
    (if (:seq vals)
      (foldl f (f (car vals) init) (cdr vals))
      init)))

(define foldr
  (lambda (f init vals)
    (if (:seq vals)
      (f (car vals) (foldr f init (cdr vals)))
      init)))

(define define-record
  (lambda (rname fields)
    (:wombat.compiler/set-global!
     (string->symbol (conc "make-" rname))
     (eval (list 'lambda fields
                 (let ((gs (gensym)))
                   (list 'lambda (list gs)
                         (cons 'list fields) ;; capture in closure
                         (foldr (lambda (f coll)
                                  (cons 'if (cons (list 'eqv? (list 'quote f) gs)
                                                  (list f coll))))
                                nil fields))))))
    nil))
