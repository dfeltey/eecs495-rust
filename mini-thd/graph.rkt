#lang racket/base
(require racket/format
         pict)
(provide make-empty-graph
         new-basic-node
         add-edge!
         add-hb-edge!

         graph-neighbors
         graph-hb)

(define (get-neighbors graph node)
  (hash-ref (graph-neighbors graph) node '()))
(define (get-backwards-neighbors graph node)
  (hash-ref (graph-backwards-neighbors graph) node '()))

;; neighbors : string -o> (listof string)
;; backwards-neighbors : string -o> (listof string)
;; hb : string -o> (listof string)
;; nodes : string -o> yinfo
(struct graph (neighbors backwards-neighbors hb nodes) #:transparent)

(define (new-basic-node graph name)
  (define nodes (graph-nodes graph))
  (define n (~a "n" (hash-count nodes)))
  (hash-set! nodes n (text name))
  n)
(define (add-edge! a-graph n1 n2)
  (define neighbors (graph-neighbors a-graph))
  (define backwards-neighbors (graph-backwards-neighbors a-graph))
  (hash-set! neighbors n1 (cons n2 (get-neighbors a-graph n1)))
  (hash-set! backwards-neighbors n2 (cons n1 (get-backwards-neighbors a-graph n2))))
(define (add-hb-edge! a-graph n1 n2)
  (define hb (graph-hb a-graph))
  (hash-set! hb n1 (cons n2 (hash-ref hb n1 '()))))
(define (make-empty-graph)
  (define neighbors (make-hash))
  (define backwards-neighbors (make-hash))
  (define hb (make-hash))
  (define nodes (make-hash))
  (graph neighbors backwards-neighbors hb nodes))
