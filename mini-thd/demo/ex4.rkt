#lang s-exp "../lang.rkt" #:histogram

(let s (sema 1))
(let mut x 0)

(seq
 (par (seq (wait s)
           (:= x (+ x 1))
           (post s))
      (seq (wait s)
           (:= x (+ x 2))
           (post s)))
 (= x 3))