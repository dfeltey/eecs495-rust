#lang s-exp "lang.rkt"

(var l0 (rec (hd 0) (tl false)))
(var l1 (rec (hd 1) (tl l0)))
(var l2 (rec (hd 2) (tl l1)))
(var l3 (rec (hd 3) (tl l2)))

(define (drop-next l)
  (:= (dot l tl) (dot (dot l tl) tl)))

(seq (par (drop-next l3)
          (drop-next l2)
          (drop-next l1))
     l3)
