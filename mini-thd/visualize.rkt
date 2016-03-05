#lang racket/base
(require "graph.rkt" "ld.rkt"
         pict
         racket/list)

(define row-gap-size 10)
(define (node-height a-graph n)
  (pict-height (node-info-pict (hash-ref (graph-nodes a-graph) n))))

(define (build-y-coordinates a-graph)
  (define layers (build-layers a-graph))
  
  (define y-coordinates (make-hash))
  (define max-layer
    (argmax (λ (i) (length (hash-ref layers i)))
            (build-list (hash-count layers) values)))
  
  (for ([(layer nodes) (in-hash layers)])
    (for/fold ([start 0]) ([node (in-list nodes)])
      (hash-set! y-coordinates node start)
      (+ row-gap-size
         start
         (node-height a-graph node))))
  max-layer)

;; EFFECT: modifies a-graph to include some new edges.
(define (build-layers a-graph)
  (define layers (make-hash))
  (define lds (longest-distances (graph-neighbors a-graph)))
  (for ([(node dist) (in-hash lds)])
    (hash-set! layers dist (cons node (hash-ref layers dist '()))))
  (for ([(node node-info) (in-hash (graph-nodes a-graph))])
    (define node-layer (hash-ref layers node))
    (for ([neighbor (in-list (get-neighbors a-graph node))])
      (define neighbor-layer (hash-ref layers neighbor))
      (unless (node-layer . < . neighbor-layer)
        (error 'build-layers "node ~s is at layer ~s, but has an edge to ~s at layer ~s"
               node node-layer neighbor neighbor-layer))
      (for/fold ([node node])
                ([middle-layer (in-range (+ node-layer 1) neighbor-layer)])
        (define middle (new-basic-node a-graph #f ?))
        (remove-edge! node neighbor)
        (add-edge! node middle)
        (add-edge! middle neighbor))))
  layers)

(module+ test
  (require rackunit)

  (let ()
    (define a-graph (make-empty-graph))
    (define n0 (new-basic-node a-graph "zero" '()))
    (define n1 (new-basic-node a-graph "one" '(0)))
    (define n2 (new-basic-node a-graph "two" '(1)))
    (define n3 (new-basic-node a-graph "three" '()))
    (add-edge! a-graph n0 n1)
    (add-edge! a-graph n1 n3)
    (add-edge! a-graph n0 n2)
    (add-edge! a-graph n2 n3)
    (define n2-height (+ (node-height a-graph n1) row-gap-size))
    (define n0-height (/ n2-height 2))
    (check-equal? (build-y-coordinates a-graph)
                  (make-hash (list (cons n0 n0-height)
                                   (cons n2 n2-height)
                                   (cons n1 0)
                                   (cons n3 n0-height)))))

  (let ()
    (define a-graph (make-empty-graph))
    (define n0 (new-basic-node a-graph "zero" '()))
    (define n1 (new-basic-node a-graph "one" '(0)))
    (define n2 (new-basic-node a-graph "two" '(0)))
    (define n3 (new-basic-node a-graph "three" '(1)))
    (define n4 (new-basic-node a-graph "four" '()))
#|

     +------> n3 ------+
n0 --+                 +--> n4
     +---> n1 --> n2 --+

|#
    (add-edge! a-graph n0 n1)
    (add-edge! a-graph n1 n2)
    (add-edge! a-graph n2 n4)
    (add-edge! a-graph n0 n3)
    (add-edge! a-graph n3 n4)
    (define n3-height (+ (max (node-height a-graph n1)
                              (node-height a-graph n2))
                         row-gap-size))
    (define n0-height (/ n3-height 2))

    (check-equal? (build-layers a-graph)
                  (make-hash '((3 . ("n4"))
                               (2 . ("n2" "??"))
                               (1 . ("n1" "n3"))
                               (0 . ("n0")))))

    (check-equal? (build-y-coordinates a-graph)
                  (make-hash (list (cons n0 n0-height)
                                   (cons n1 0)
                                   (cons n2 0)
                                   (cons n3 n3-height)
                                   (cons n4 n0-height)))))

  (let ()
    (define a-graph (make-empty-graph))
    (define n0 (new-basic-node a-graph "zero" '(1)))
    (define n1 (new-basic-node a-graph "one" '(0 1)))
    (define n2 (new-basic-node a-graph "two" '(0 1)))
    (define n3 (new-basic-node a-graph "three" '(1 1)))
    (define n4 (new-basic-node a-graph "four" '(1)))
    (define n5 (new-basic-node a-graph "five" '()))
    (define n6 (new-basic-node a-graph "six" '(0)))
    (define n7 (new-basic-node a-graph "seven" '()))
    
#|

     +------> n6 ----------------+
n5 --+                           |
     +  +- n1 --> n2 --+         +---> n7
    n0--+              +--> n4 --+
        +----- n3 -----+

|#
    (add-edge! a-graph n5 n0)
    (add-edge! a-graph n0 n1)
    (add-edge! a-graph n1 n2)
    (add-edge! a-graph n2 n4)
    (add-edge! a-graph n0 n3)
    (add-edge! a-graph n3 n4)
    (add-edge! a-graph n5 n6)
    (add-edge! a-graph n6 n7)
    (add-edge! a-graph n4 n7)
    
    (check-equal? (build-y-coordinates a-graph)
                  '?)))
