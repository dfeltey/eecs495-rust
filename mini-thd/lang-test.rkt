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