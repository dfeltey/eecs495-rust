#lang s-exp "lang.rkt"

(var y 1)

(seq (par (:= y 2)
          (:= y 3))
     y)

