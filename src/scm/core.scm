(define load-file
  (lambda (file)
    (#:wombat.compiler/load-file file)))

(define list (lambda elems elems))

(define car
  (lambda (lis)
    (#:wombat.datatypes/car lis)))

(define cdr
  (lambda (lis)
    (#:wombat.datatypes/cdr lis)))

(define cons
  (lambda (a d)
    (#:wombat.datatypes/cons a d)))

(define eqv?
  (lambda (a b)
    (#:jvm (#:emit a)
           (#:emit b)
           (invokeVirtual Object (boolean "equals" Object))
           (box boolean))))

(define namespace
  (lambda (named)
    (#:jvm (#:emit named)
          (checkCast clojure.lang.Named)
          (invokeInterface clojure.lang.Named (String "getNamespace")))))

(define name
  (lambda (named)
    (#:jvm (#:emit named)
          (checkCast clojure.lang.Named)
          (invokeInterface clojure.lang.Named (String "getName")))))

; This is NOT recursive!
; (if (null? ...) ...) is a special case in the compiler!
(define null?
  (lambda (o)
    (if (null? o)
      #t
      #f)))

(define not
  (lambda (o)
    (if o
      #f
      #t)))

(define get-var
  (lambda (sym)
    (#:jvm (#:invoke namespace sym)
           (checkCast String)
           (#:invoke name sym)
           (checkCast String)
           (invokeStatic clojure.lang.RT (clojure.lang.Var "var" String String)))))

(define class
  (lambda (obj)
    (if (null? obj)
      '()
      (#:jvm (#:emit obj)
             (invokeVirtual Object (Class "getClass"))))))

(define instance?
  (lambda (cls o)
    (#:jvm (#:emit cls)
           (checkCast Class)
           (#:emit o)
           (invokeVirtual Class (boolean "isInstance" Object))
           (box boolean))))

(define add1
  (lambda (n)
    (#:jvm (#:emit n)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (push long 1)
           (add long)
           (box long))))

(define sub1
  (lambda (n)
    (#:jvm (#:emit n)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (push long 1)
           (sub long)
           (box long))))

(define +
  (lambda (x y)
    (#:jvm (#:emit x)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (#:emit y)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (add long)
           (box long))))

(define -
  (lambda (x y)
    (#:jvm (#:emit x)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (#:emit y)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (sub long)
           (box long))))

(define *
  (lambda (x y)
    (#:jvm (#:emit x)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (#:emit y)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (mul long)
           (box long))))

(define <
  (lambda (x y)
    (#:jvm (#:emit x)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (#:emit y)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (ifCmp long < #:is-less)
           (#:emit #f)
           (goto #:end)
           (label #:is-less)
           (#:emit #t)
           (label #:end))))

(define >
  (lambda (x y)
    (#:jvm (#:emit x)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (#:emit y)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (ifCmp long > #:is-more)
           (#:emit #f)
           (goto #:end)
           (label #:is-more)
           (#:emit #t)
           (label #:end))))

(define =
  (lambda (x y)
    (#:jvm (#:emit x)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (#:emit y)
           (checkCast Number)
           (invokeVirtual Number (long "longValue"))
           (ifCmp long = #:are-equal)
           (#:emit #f)
           (goto #:end)
           (label #:are-equal)
           (#:emit #t)
           (label #:end))))

(define print
  (lambda (o)
    (#:print o)))

(define obj->str
  (lambda (o)
    (#:wombat.printer/write-str o)))

(define fib-print
  (lambda (n m x y)
    (print "******\n")
    (print "n: ")
    (print (obj->str n))
    (print "\n")
    (print "m: ")
    (print (obj->str m))
    (print "\n")
    (print "x: ")
    (print (obj->str x))
    (print "\n")
    (print "y: ")
    (print (obj->str y))
    (print "\n\n")
    (if (< n m)
      (fib-print (add1 n) m y (+ y x))
      y)))

(define fac
  (lambda (n)
    (if (< n 2)
      1
      (* (fac (sub1 n)) n))))

(define sum-to
  (lambda (n)
    (if (> n 0)
      (+ (sum-to (sub1 n)) n)
      0)))

(define fib
  (lambda (n)
    (if (< n 2)
      1
      (+ (fib (sub1 n)) (fib (- n 2))))))

(define fib*
  (lambda (n m x y)
    (if (< n m)
      (fib* (add1 n) m y (+ x y))
      y)))

(define string->symbol
  (lambda (str)
    (#:jvm (#:emit str)
           (checkCast String)
           (invokeStatic clojure.lang.Symbol (clojure.lang.Symbol "intern" String)))))

(define str
  (lambda (val)
    (#:jvm (#:str val))))

(define conc
  (lambda (s1 s2)
    (#:jvm (#:str s1)
           (#:str s2)
           (invokeVirtual String (String "concat" String)))))

(define eval
  (lambda (form)
    (#:wombat.compiler/eval* form)))

(define gensym
  (lambda ()
    (string->symbol (conc "G__#" (str (#:wombat.compiler/next-id))))))

(define map
  (lambda (f lis)
    (if lis
      (cons (f (car lis)) (map f (cdr lis)))
      '())))

(define foldl
  (lambda (f init vals)
    (if (null? vals)
      init
      (foldl f (f (car vals) init) (cdr vals)))))

(define foldr
  (lambda (f init vals)
    (if (null? vals)
      init
      (f (car vals) (foldr f init (cdr vals))))))

#;
(define define-record
  (lambda (rname fields)
    (#:wombat.compiler/set-global!
     (string->symbol (conc "make-" rname))
     (eval (list 'lambda fields
                 (let ((gs (gensym)))
                   (list 'lambda (list gs)
                         (cons 'list fields) ;; capture in closure
                         (foldr (lambda (f coll)
                                  (cons 'if (cons (list 'eqv? (list 'quote f) gs)
                                                  (list f coll))))
                                '() fields))))))
    '()))


(define doit2)
(define doit
  (lambda (n)
    (if (> n 0)
      (doit2 (sub1 n))
      (print "Done!\n"))))
(define doit2
  (lambda (n)
    (doit n)))


(define m4)
(define m3)
(define m2)
(define m1
  (lambda (n)
    (if (> n 0)
      (m2 (sub1 n))
      (print "done!\n"))))
(define m2
  (lambda (n)
    (m3 n)))
(define m3
  (lambda (n)
    (m4 n)))
(define m4
  (lambda (n)
    (m1 n)))

(define time
  (lambda (thunk)
    (let ((start (#:jvm (invokeStatic System (long "nanoTime")) (box long)))
          (ret (thunk))
          (end (#:jvm (invokeStatic System (long "nanoTime")) (box long))))
      (#:printf "time: %.3f ms\n" (#:- (#:/ (#:double end) 1000000) (#:/ (#:double start) 1000000)))
      ret)))
