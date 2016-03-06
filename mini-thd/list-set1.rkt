#lang s-exp "lang.rkt" 

(define (make-lock)
  (sema 1))
(define (lock l)
  (wait l))
(define (unlock l)
  (post l))

(define (make-node val next)
  (rec (guard (make-lock)) (val val) (next next)))

(var null false)


(define (matches node key)
  (and (dot node next)
       (= (dot (dot node next) val) key)))

(define (find-predecessor-locking lst key)
  (var ptr lst)
  (seq
   (lock (dot ptr guard))
   (while
    (and (dot ptr next)
         (< (dot (dot ptr next) val) key))
    (seq (lock (dot (dot ptr next) guard))
         (unlock (dot ptr guard))
         (:= ptr (dot ptr next))))
   ptr))

(define (insert lst key)
  (var prev (find-predecessor-locking lst key))
  (var new-node
       (make-node key (dot prev next)))
  (if (matches prev key)
      false
      (seq (:= (dot prev next) new-node)
           (unlock (dot prev guard))
           true)))

(define (remove lst key)
  (var prev (find-predecessor-locking lst key))
  (if (not (matches prev key))
      false
      (seq (:= (dot prev next) (dot (dot prev next) next))
           (unlock (dot prev guard))
           true)))

(define (build-list n)
  (var lst (make-node 0 null))
  (var i 1)
  (seq (while (< i n) (seq (insert lst i) (:= i (+ i 1)))) lst))

(define (list->plain-list lst)
  (if lst
      (rec (val (dot lst val))
        (next (list->plain-list (dot lst next))))
      false))

(var the-list (build-list 3))
(seq (par (remove the-list 1)
          (remove the-list 2))
     (equal? (list->plain-list the-list)
             (list->plain-list (build-list 1))))

;(list->plain-list (build-list 2))




