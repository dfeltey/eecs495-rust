#lang s-exp "../lang.rkt" #:dot

(let mut flag-0 false)
(let mut flag-1 false)

(define (lock-0)
  (seq (:= flag-0 true)
       (while flag-1 (seq))))

(define (unlock-0)
  (:= flag-0 false))

(define (lock-1)
  (seq (:= flag-1 true)
       (while flag-0 (seq))))

(define (unlock-1)
  (:= flag-1 false))

(let mut count 0)

(define (get-and-inc-0)
  (seq (lock-0)
       (let old count)
       (:= count (+ old 1))
       (unlock-0)))

(define (get-and-inc-1)
  (seq (lock-1)
       (let old count)
       (:= count (+ old 1))
       (unlock-1)))
       
(seq (par (get-and-inc-0)
          (get-and-inc-1))
     (print count)
     false)
            