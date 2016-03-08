#lang s-exp "../lang.rkt" #:histogram

(let mut waiting -1)

(define (lock-0)
  (seq (:= waiting 0)
       (while (= waiting 0) (seq))))

(define (unlock-0)
  (:= waiting -1))

(define (lock-1)
  (seq (:= waiting 1)
       (while (= waiting 1) (seq))))

(define (unlock-1)
  (:= waiting -1))

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
     count)
            