#lang racket
(require rackunit racket/runtime-path)
(define-runtime-path lang-test.rkt "lang-test.rkt")
(define (run-mod m)
  ((dynamic-require `(submod ,lang-test.rkt ,m) 'main)))

(module var1 "lang.rkt" #:left-to-right
  (var y 0)
  y)
(check-equal? (run-mod 'var1) 0)

(module assign1 "lang.rkt" #:left-to-right
  (var y 0)
  (seq (:= y 1)
       y))
(check-equal? (run-mod 'assign1) 1)

(module fun "lang.rkt" #:left-to-right
  (define (f z) (+ z 1))
  (f 3))
(check-equal? (run-mod 'fun) 4)

(module par1 "lang.rkt" #:left-to-right
  (var y 0)
  (seq (par (:= y (+ y 1))
            (:= y (+ y 1)))
       y))
(check-equal? (run-mod 'par1) 2)

(module struct1 "lang.rkt"
  (dot (rec (x 1) (y 2)) y))
(check-equal? (run-mod 'struct1) 2)

(module struct2 "lang.rkt"
  (var z (rec (x 1) (y 2)))
  (seq (:= (dot z x) 3)
       (dot z x)))
(check-equal? (run-mod 'struct2) 3)

(module struct3 "lang.rkt" #:left-to-right
  (var z (rec (x 1) (y 2)))
  (seq (par (:= (dot z x) 3)
            (:= (dot z x) 4))
       (dot z x)))
(check-equal? (run-mod 'struct3) 4)

(module struct4 "lang.rkt" #:left-to-right
  (var z (rec (x 1) (y 2)))
  (seq (par (:= (dot z x) 4)
            (:= (dot z x) 3))
       (dot z x)))
(check-equal? (run-mod 'struct4) 3)

(module struct5 "lang.rkt" #:left-to-right
  (var s (rec (f 1)))
  (var y 0)
  (define (f s)
    (seq (par (:= (dot s f) 2)
              (:= y (dot s f)))
         y))
  (f s))
(check-equal? (run-mod 'struct5) 2)

(module struct6 "lang.rkt" #:left-to-right
  (var s (rec (f 1)))
  (var y 0)
  (define (f s)
    (seq (par (:= y (dot s f))
              (:= (dot s f) 2))
         y))
  (f s))
(check-equal? (run-mod 'struct6) 1)

(module sema1 "lang.rkt" #:left-to-right
  (var s (sema 0))
  (var x 0)
  (seq (par (seq (:= x 1)
                 (post s))
            (seq (wait s)
                 (:= x 2)))
       x))
(check-equal? (run-mod 'sema1) 2)

(module sema2 "lang.rkt" #:left-to-right
  (var s (sema 0))
  (var x 0)
  (seq (par (seq (wait s)
                 (:= x 2))
            (seq (:= x 1)
                 (post s)))
       x))
(check-equal? (run-mod 'sema2) 2)

(module if1 "lang.rkt"
  (if true 1 2))
(check-equal? (run-mod 'if1) 1)
(module if2 "lang.rkt"
  (if (if (< 2 1) true false) 1 2))
(check-equal? (run-mod 'if2) 2)

(module while "lang.rkt"
  (var x 1)
  (var l 10)
  (seq (while (l . > . 0)
              (seq (:= x (* x l))
                   (:= l (+ l -1))))
       x))
(check-equal? (run-mod 'while) (* 10 9 8 7 6 5 4 3 2))

(module or "lang.rkt"
  (rec (a (or false true))
    (b (or true true))
    (c (or false false))
    (d (or true (* true false)))))
(check-equal? (run-mod 'or)
              (make-hash (list (cons 'a #t)
                               (cons 'b #t)
                               (cons 'c #f)
                               (cons 'd #t))))

(module and "lang.rkt"
  (rec (a (and true false))
    (b (and true true))
    (c (and false false))
    (d (and false (* true false)))))
(check-equal? (run-mod 'and)
              (make-hash (list (cons 'a #f)
                               (cons 'b #t)
                               (cons 'c #f)
                               (cons 'd #f))))

(module print1 "lang.rkt"
  (var x 0)
  (print x))
(define (run-mod/output name)
  (define sp (open-output-string))
  (parameterize ([current-output-port sp])
    (run-mod name))
  (get-output-string sp))
(check-equal? (run-mod/output 'print1)
              "x 0\n")

(module print2 "lang.rkt"
  (var x 0)
  (var y 1)
  (var z 2)
  (print x y z))
(check-equal? (run-mod/output 'print2)
              "x 0 y 1 z 2\n")
