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
