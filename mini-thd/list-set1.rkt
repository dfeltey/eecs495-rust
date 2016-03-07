#lang s-exp "lang.rkt"

(define (make-lock)
  (sema 1))

(define (make-list lst)
  (rec (guard (make-lock)) (next lst)))

(define (make-node val next)
  (rec (guard (make-lock)) (val val) (next next)))

(let null false)


(define (matches node key)
  (and (dot node next)
       (= (dot (dot node next) val) key)))

(define (find-predecessor-locking lst key)
  (var ptr lst)
  (seq
   (wait (dot ptr guard))
   (while
    (and (dot ptr next)
         (< (dot (dot ptr next) val) key))
    (seq (wait (dot (dot ptr next) guard))
         (post (dot ptr guard))
         (:= ptr (dot ptr next))))
   ptr))

(define (find-predecessor-locking-rec lst key)
  (seq
   (wait (dot lst guard))
   (if (and (dot lst next)
            (< (dot (dot lst next) val) key))
       (seq (wait (dot (dot lst next) guard))
            (post (dot lst guard))
            (find-predecessor-locking-rec (dot lst next) key))
       lst)))

(define (insert lst key)
  (let prev (find-predecessor-locking lst key))
  (let new-node
       (make-node key (dot prev next)))
  (if (matches prev key)
      false
      (seq (:= (dot prev next) new-node)
           (post (dot prev guard))
           true)))

(define (remove lst key)
  (let prev (find-predecessor-locking lst key))
  (if (matches prev key)
      (seq (:= (dot prev next) (dot (dot prev next) next))
           (post (dot prev guard))
           true)
      false))

(define (do-build-list n count)
    (if (< count n)
        (make-node count (do-build-list n (+ 1 count)))
        (make-node n null)))

(define (build-list n)
  (make-list (do-build-list n 0)))

(define (list->plain-list lst)
  (if lst
      (rec (val (dot lst val))
        (next (list->plain-list (dot lst next))))
      false))

(define (clean-list lst)
  (list->plain-list (dot lst next)))

(let the-list (build-list 1))
(seq (par (remove the-list 1)
          (insert the-list 2))
     (seq
      (equal? (clean-list the-list)
             (rec (val 0) (next (rec (val 2) (next null)))))))






