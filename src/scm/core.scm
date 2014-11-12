(define (load-file file)
  (#:wombat.compiler/load-file file))

(define (eval form)
  (#:wombat.compiler/eval* form))

(define (list . elems) elems)

(define (car lis)
  (#:wombat.datatypes/car lis))

(define (cdr lis)
  (#:wombat.datatypes/cdr lis))

(define (cons a d)
  (#:wombat.datatypes/cons a d))

(define (length lis)
  (#:wombat.datatypes/length lis))

;; This is NOT recursive!
;; (if (null? ...) ...) is a special case in the compiler!
(define (null? o)
  (if (null? o)
    #t
    #f))

(define (not o)
  (if o
    #f
    #t))

(define (foldl f init vals)
  (if (null? vals)
    init
    (foldl f (f (car vals) init) (cdr vals))))

(define (foldr f init vals)
  (if (null? vals)
    init
    (f (car vals) (foldr f init (cdr vals)))))

(define (map f lis)
  (foldr (lambda (x xs)
           (cons (f x) xs)) '() lis))

(define (filter f lis)
  (foldr (lambda (x xs)
           (if (f x)
             (cons x xs)
             xs))
         '() lis))

(define (cat* l1 l2)
  (foldr cons l2 l1))

(define (concat . ls)
  (foldr cat* '() ls))

;; like null?, this relies on a special case in the compiler
(define (eq? a b)
  (if (eq? a b)
    #t
    #f))

(define (eqv? a b)
  (if (eq? a b)
    #t
    (#:jvm (#:emit a)
           (#:emit b)
           (invokeVirtual Object (boolean "equals" Object))
           (box boolean))))

(define (list? x)
  (#:wombat.datatypes/list? x))

(define (pair? x)
  (#:wombat.datatypes/pair? x))

(define (print o)
  (#:print o))

(define (expand f)
  (#:wombat.compiler/expand f))

(define (sanitize f)
  (#:wombat.compiler/sanitize (#:hash-map) f))

(define (obj->str o)
  (#:wombat.printer/write-str o))

(define (reverse lis)
  (foldl cons '() lis))

(define (list! o)
  (#:jvm (#:emit o)
         (instanceOf wombat.datatypes.List)
         (ifZCmp = #:not-list)
         (#:emit o)
         (goto #:end)
         (label #:not-list)
         (#:invoke #:wombat.datatypes/seq->list o)
         (label #:end)))

(define (list* . ls)
  (if (null? ls)
    '()
    (let ((rlis (reverse ls)))
      (if (not (list? (car rlis)))
        (#:jvm (throwException IllegalArgumentException "list* expects list as final arg")))
      (foldl cons (car rlis) (cdr rlis)))))

(define apply
  (case-lambda
    ([f] (f))
    ([f args] (#:jvm (#:emit f)
                     (checkCast wombat.ILambda)
                     (#:emit args)
                     (invokeInterface wombat.ILambda (Object "applyTo" Object))))
    ([f x args]
     (apply f (cons x args)))
    ([f x y args]
     (apply f (cons x (cons y args))))
    ([f x y z . rest]
     (apply f (cons x (cons y (cons z (apply list* rest))))))))

(define (quasiquote-pair* c l ls)
  (if (null? c)
    (list 'apply 'concat (cons 'list (reverse (cons (cons 'list (reverse l)) ls))))
    (if (pair? (car c))
      (if (eqv? (car (car c)) 'unquote)
        (quasiquote-pair* (cdr c)
                          (cons (car (cdr (car c))) l)
                          ls)
        (if (eqv? (car (car c)) 'unquote-splicing)
          (quasiquote-pair* (cdr c)
                            '()
                            (list* (car (cdr (car c))) (cons 'list (reverse l)) ls))
          (if (eqv? (car (car c)) 'quasiquote)
            (quasiquote-pair* (cdr c)
                              (cons (car c) l)
                              ls)
            (quasiquote-pair* (cdr c)
                              (cons (quasiquote-pair* (car c) '() '()) l)
                              ls))))
      (quasiquote-pair* (cdr c) (cons (list 'quote (car c)) l) ls))))

(define-macro (quasiquote x)
  (let ((res (if (pair? x)
               (if (eqv? (car x) 'unquote)
                 (car (cdr x))
                 (if (eqv? (car x) 'unquote-splicing)
                   (#:jvm (throwException RuntimeException "unquote-splicing outside of list!"))
                   (quasiquote-pair* x '() '())))
               (list 'quote x))))
    res))

(define-macro (fail msg)
  `(#:jvm (throwException RuntimeException ,msg)))

(define-macro (caar l) `(car (car ,l)))
(define-macro (cadr l) `(car (cdr ,l)))
(define-macro (cdar l) `(cdr (car ,l)))
(define-macro (cddr l) `(cdr (cdr ,l)))
(define-macro (caaar l) `(car (caar ,l)))
(define-macro (caadr l) `(car (cadr ,l)))
(define-macro (cadar l) `(car (cdar ,l)))
(define-macro (caddr l) `(car (cddr ,l)))
(define-macro (cdaar l) `(cdr (caar ,l)))
(define-macro (cdadr l) `(cdr (cadr ,l)))
(define-macro (cddar l) `(cdr (cdar ,l)))
(define-macro (cdddr l) `(cdr (cddr ,l)))

(define-macro (let* binds . body)
  (foldr (lambda (l r) (list 'let (list l) r))
         (cons 'begin body) binds))

(define-macro (and . vals)
  (if (null? vals)
    #t
    (if (null? (cdr vals))
      (car vals)
      `(if ,(car vals)
         (and ,@(cdr vals))
         #f))))

(define-macro (and* . vals)
  (if (null? vals)
    #t
    (if (null? (cdr vals))
      `(let ((tmp ,(car vals)))
         (and (not (null? tmp))
              tmp))
      `(let ((temp ,(car vals)))
         (and (not (null? tmp))
              tmp
              (and* ,@(cdr vals)))))))

(define-macro (or . vals)
  (if (null? vals)
    #f
    (if (null? (cdr vals))
      (car vals)
      `(let ((tmp ,(car vals)))
         (if tmp
           tmp
           (or ,@(cdr vals)))))))

(define-macro (or* . vals)
  (if (null? vals)
    #f
    (if (null? (cdr vals))
      `(let ((tmp ,(car vals)))
         (and (not (null? tmp))
              tmp))
      `(let ((tmp ,(car vals)))
         (or (and (not (null? tmp))
                  tmp)
             (or* ,@(cdr vals)))))))

(define-macro (if* cond then . else)
  `(if (and* ,cond)
     ,then
     ,@else))

(define-macro (unless cond then . else)
  (if (and (not (null? else))
           (not (null? (cdr else))))
    (fail "unless takes 2 or 3 forms"))
  `(if ,cond
     ,@else
     ,then))

(define-macro (unless* cond . rest)
  `(unless (and* ,cond) ,@rest))

(define-macro (if-not cond then . else)
  (if (and (not (null? else))
           (not (null? (cdr else))))
    (fail "if-not takes 2 or 3 forms"))
  `(if ,cond
     ,@else
     ,then))

(define-macro (if-not* cond . rest)
  `(if-not (and* ,cond) ,@rest))

(define-macro (when cond . exprs)
  `(if ,cond
     (begin ,@exprs)))

(define-macro (when* cond . exprs)
  `(if* ,cond
     (begin ,@exprs)))

(define-macro (when-not cond . exprs)
  `(if-not ,cond
     (begin ,@exprs)))

(define-macro (when-not* cond . exprs)
  `(if-not* cond ,@exprs))

(define-macro (cond . conds)
  (if (null? conds)
    '()
    (if (eq? (caar conds) 'else)
      `(begin ,@(cdar conds))
      `(if ,(caar conds)
         (begin ,@(cdar conds))
         (cond ,@(cdr conds))))))

(define-macro (cond* . conds)
  (if (null? conds)
    '()
    (if (eq? (caar conds) 'else)
      `(begin ,@(cdar conds))
      `(if* ,(caar conds)
         (begin ,@(cdar conds))
         (cond* ,@(cdr conds))))))

(define-macro (define-case name . methods)
  `(define ,name
     (case-lambda ,@methods)))

(define (not* o)
  (if-not* o
    #t
    #f))

(define (expand-all form)
  (if (list? form)
    (let ([exp (expand form)])
      (if (eq? form exp)
        (map expand-all form)
        (expand-all exp)))
    form))

(define (last lis)
  (if (null? lis)
    '()
    (if (null? (cdr lis))
      (car lis)
      (last (cdr lis)))))

;; (define-macro new
;;   (lambda (c)
;;     `(#:jvm (newInstance ,c)
;;             (dup)
;;             (invokeConstructor ,c (void "<init>")))))

;; obj format: (Type object)
;; meth format: (ReturnType "methodName" Arg1Type Arg2type ...)
;; (define-macro call
;;   (lambda (obj meth #!rest args)
;;     (let ((type (car obj))
;;           (obj (car (cdr obj))))
;;       `(#:jvm (#:emit ,obj)
;;               (checkCast ,type)
;;               (invokeVirtual ,type ,meth)))))

;; (define str
;;   (lambda vals
;;     (let ((sb (new StringBuilder)))
;;       (foldl (lambda (v sb)
;;                (if (string? v)
;;                  (call (StringBuilder sb) (StringBuilder "append" String))
;;                  (call (StringBuilder sb) (StringBuilder "append"))))))))

(define (namespace named)
  (#:jvm (#:emit named)
         (checkCast clojure.lang.Named)
         (invokeInterface clojure.lang.Named (String "getNamespace"))))

(define (name named)
  (#:jvm (#:emit named)
         (checkCast clojure.lang.Named)
         (invokeInterface clojure.lang.Named (String "getName"))))

(define (get-var sym)
  (#:jvm (#:invoke namespace sym)
         (checkCast String)
         (#:invoke name sym)
         (checkCast String)
         (invokeStatic clojure.lang.RT (clojure.lang.Var "var" String String))))

(define (class obj)
  (if (null? obj)
    '()
    (#:jvm (#:emit obj)
           (invokeVirtual Object (Class "getClass")))))

(define (add1 n)
  (#:jvm (#:emit n)
         (invokeStatic clojure.lang.Numbers (Number "incP" Object))))

(define (sub1 n)
  (#:jvm (#:emit n)
         (invokeStatic clojure.lang.Numbers (Number "decP" Object))))

(define (swap f)
  (case-lambda
    ([] (f))
    ([x] (f x))
    ([x y] (f y x))
    ([x y . zs] (apply f y x zs))))

(define-macro <arithmetic>
  (case-lambda
    ([op x] `(#:jvm (#:emit ,x)
                    (invokeStatic clojure.lang.Numbers (Number ,op Object Object))))
    ([op x y] `(#:jvm (#:emit ,x)
                      (#:emit ,y)
                      (invokeStatic clojure.lang.Numbers (Number ,op Object Object))))))

(define-macro (<instanceof> cls o)
  `(#:jvm (#:emit ,o)
          (instanceOf ,cls)
          (box boolean)))

(define-macro (add-inline name . lam)
  (if (pair? name)
    `(add-inline ,(car name) (lambda ,(cdr name) ,@lam))
    (if (or (null? lam)
            (not (null? (cdr lam))))
      (fail "add-inline only takes one lambda")
      `(#:wombat.compiler/add-inline! ',name ,@lam))))

(define-macro (reify-macro name . lam)
  (if (pair? name)
    `(reify-macro ,(car name)
                  (lambda ,(cdr name) ,@lam))
    (if (or (null? lam)
            (not (null? (cdr lam))))
      (fail "reify-macro only takes one lambda")
      `(#:wombat.compiler/add-function! ',name ,@lam))))

(define-macro (define-with-inline name macro fun)
  `(begin
     (define-macro ,name ,macro)
     (reify-macro ,name ,fun)))

(define (resolve-class sym)
  (let ([cls (#:wombat.compiler/clean-resolve sym)])
    (#:jvm (#:emit cls)
           (instanceOf Class)
           (ifZCmp not= #:is-class)
           (#:emit #f)
           (goto #:end)
           (label #:is-class)
           (#:emit cls)
           (label #:end))))

(define (instance? c o)
  (#:jvm (#:emit c)
         (checkCast Class)
         (#:emit o)
         (invokeVirtual Class (boolean "isInstance" Object))
         (box boolean)))

(add-inline (instance? c o)
  (if (and (#:jvm (#:macro <instanceof> clojure.lang.Symbol c))
           (resolve-class c))
    `(<instanceof> ,c ,o)
    #!no-op))

;; Redefine list? and pair? using faster instance?
(define (list? o)
  (or (null? o)
      (instance? wombat.datatypes.IList o)))
(define (pair? o)
  (instance? wombat.datatypes.ICons o))
(define (string? o)
  (instance? String o))
(define (number? o)
  (instance? Number o))

(define (complement f)
  (lambda (x)
    (let ([ret (not (f x))])
      ret)))

(define (arith-foldl f init coll)
  (if (null? coll)
    init
    (arith-foldl f (f (car coll) init) (cdr coll))))

(define-case +
  ([] 1)
  ([x] x)
  ([x y] (<arithmetic> "addP" x y))
  ([x y . zs]
   (foldl + (+ x y) zs)))

(add-inline +
  (case-lambda
    ([] 1)
    ([x] (if (<instanceof> Number x)
           x
           `(<arithmetic> "addP" ,x)))
    ([x y]
     (if (and (<instanceof> Number x)
              (<instanceof> Number y))
       (+ x y)
       `(<arithmetic> "addP" ,x ,y)))
    ([x y . zs]
     (let ([num-sum (apply + (filter number? (list* x y zs)))]
           [rest (filter (complement number?) (list* x y zs))])
       (arith-foldl (lambda (a b) `(+ ,a ,b))
                    num-sum rest)))))

(define-case -
  ([x] (- 0 x))
  ([x y]
   (#:jvm (#:emit x)
          (#:emit y)
          (invokeStatic clojure.lang.Numbers (Number "minusP" Object Object))))
  ([x y . zs]
   (foldl (swap -) (- x y) zs)))

(define (* x y)
  (#:jvm (#:emit x)
         (#:emit y)
         (invokeStatic clojure.lang.Numbers (Number "multiplyP" Object Object))))

(define (/ x y)
    (#:jvm (#:emit x)
         (#:emit y)
         (invokeStatic clojure.lang.Numbers (Number "divide" Object Object))))

(define (< x y)
  (#:jvm (#:emit x)
         (#:emit y)
         (invokeStatic clojure.lang.Numbers (boolean "lt" Object Object))
         (box boolean)))

(define (<= x y)
  (#:jvm (#:emit x)
         (#:emit y)
         (invokeStatic clojure.lang.Numbers (boolean "lte" Object Object))
         (box boolean)))

(define (> x y)
  (#:jvm (#:emit x)
         (#:emit y)
         (invokeStatic clojure.lang.Numbers (boolean "gt" Object Object))
         (box boolean)))

(define (>= x y)
  (#:jvm (#:emit x)
         (#:emit y)
         (invokeStatic clojure.lang.Numbers (boolean "gte" Object Object))
         (box boolean)))

(define (= x y)
  (#:jvm (#:emit x)
         (#:emit y)
         (invokeStatic clojure.lang.Numbers (boolean "equiv" Object Object))
         (box boolean)))

(define (fac n)
  (let loop ((n n)
             (p 1))
    (if (> n 0)
      (loop (sub1 n) (* p n))
      p)))

(define (sum-to n)
  (if (> n 0)
    (+ (sum-to (sub1 n)) n)
    0))

(define (fib n)
  (if (< n 2)
    1
    (+ (fib (sub1 n)) (fib (- n 2)))))

(define (fib* n m x y)
  (if (< n m)
    (fib* (add1 n) m y (+ x y))
    y))

(define (string->symbol str)
  (#:jvm (#:emit str)
         (checkCast String)
         (invokeStatic clojure.lang.Symbol (clojure.lang.Symbol "intern" String))))

(define (str val)
  (#:jvm (#:str val)))

(define (conc s1 s2)
  (#:jvm (#:str s1)
         (#:str s2)
         (invokeVirtual String (String "concat" String))))

(define (gensym . s)
  (if (> (length s) 1)
    (fail "gensym takes 0 or 1 arguments"))
  (if (null? s)
    (#:wombat.compiler/sanitize-name "G_")
    (#:wombat.compiler/sanitize-name (#:str (car s)))))


;; (define define-record
;;   (lambda (rname fields)
;;     (#:wombat.compiler/set-global!
;;      (string->symbol (conc "make-" rname))
;;      (eval (list 'lambda fields
;;                  (let ((gs (gensym)))
;;                    (list 'lambda (list gs)
;;                          (cons 'list fields) ;; capture in closure
;;                          (foldr (lambda (f coll)
;;                                   (cons 'if (cons (list 'eqv? (list 'quote f) gs)
;;                                                   (list f coll))))
;;                                 '() fields))))))
;;     '()))


;; (define doit2)
;; (define (doit n)
;;   (if (> n 0)
;;     (doit2 (sub1 n))
;;     (print "Done!\n")))
;; (define (doit2 n)
;;   (doit n))


;; (define m4)
;; (define m3)
;; (define m2)
;; (define (m1 n)
;;   (if (> n 0)
;;     (m2 (sub1 n))
;;     (print "done!\n")))
;; (define (m2 n)
;;   (m3 n))
;; (define (m3 n)
;;   (m4 n))
;; (define (m4 n)
;;   (m1 n))

(define-macro (time . body)
  `(let ((start (#:jvm (invokeStatic System (long "nanoTime")) (box long)))
         (ret (begin ,@body))
         (end (#:jvm (invokeStatic System (long "nanoTime")) (box long))))
     (#:printf "time: %.3f ms\n" (#:- (#:/ (#:double end) 1000000) (#:/ (#:double start) 1000000)))
     ret))

(define-macro (dotimes n . body)
  `(let loop ((n ,n))
     (if (> n 0)
       (begin
         ,@body
         (loop (sub1 n))))))

(define (get-state)
  (lambda (s) (cons s s)))
(define (set-state s)
  (lambda (_) (cons () s)))
(define (pure-state a)
  (lambda (s) (cons a s)))

(define (map-state f)
  (lambda (s)
    (let ([s1 (s)])
      (cons (f (car s1))
            (cdr s1)))))

(define (zip-index lis)
  (foldl (lambda (a acc)
           (let ([xs (map-state acc)])))))
