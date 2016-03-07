#lang s-exp "lang.rkt" 

(let mut y 0)

(seq (par (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1))
          (:= y (+ y 1)))
     false)
