#lang racket/base
(require "graph.rkt"
         "ld.rkt"
         "sync.rkt"
         pict
         racket/list)

(define row-gap-size 10)
(define (node-height a-graph n)
  (define pct (node-info-pict (hash-ref (graph-nodes a-graph) n)))
  (if (pict? pct)
      (pict-height pct)
      0))

(define (build-y-coordinates a-graph)
  (define layers (build-layers a-graph))
  (define y-coordinates (make-hash))
  (for ([(layer-index nodes) (in-hash layers)])
    (define (node->lon n)
      (node-info-identification
       (hash-ref (graph-nodes a-graph) n)))
    (define sorted (sort nodes lon< #:key node->lon))
    (define num-nodes (length sorted))
    (cond
      [(= num-nodes 1)
       (hash-set! y-coordinates (car sorted) 1/2)]
      [else
       (for ([node (in-list sorted)]
             [i (in-range 0 num-nodes)])
         (hash-set! y-coordinates node (/ i (- num-nodes 1))))]))
  y-coordinates)

(define (avg . ls) (/ (apply + ls) (length ls)))

;; EFFECT: modifies a-graph to include some new edges in such a way
;; that every edge that crosses a layer has a node in that layer
(define (build-layers a-graph)
  (define layers (make-hash))
  (define lds (longest-distances (graph-neighbors a-graph)))
  (for ([(node dist) (in-hash lds)])
    (hash-set! layers dist (cons node (hash-ref layers dist '()))))
  (for ([a-node (in-list (hash-keys (graph-nodes a-graph)))])
    (define node-layer (hash-ref lds a-node))
    (define a-node-identification
      (node-info-identification (hash-ref (graph-nodes a-graph) a-node)))
    (for ([neighbor (in-list (get-neighbors a-graph a-node))])
      (define neighbor-layer (hash-ref lds neighbor))
      (unless (node-layer . < . neighbor-layer)
        (error 'build-layers "node ~s is at layer ~s, but has an edge to ~s at layer ~s"
               a-node node-layer neighbor neighbor-layer))
      (for/fold ([node a-node])
                ([middle-layer (in-range (+ node-layer 1) neighbor-layer)])
        (define middle (new-basic-node a-graph #f a-node-identification))
        (hash-set! layers middle-layer (cons middle (hash-ref layers middle-layer '())))
        (remove-edge! a-graph node neighbor)
        (add-edge! a-graph node middle)
        (add-edge! a-graph middle neighbor)
        middle)))
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
    (check-equal? (build-layers a-graph)
                  (make-hash '((2 . ("n3")) (1 . ("n1" "n2")) (0 . ("n0"))))))

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
                  (make-hash '(("n1" . 0) ("n2" . 1) ("n0" . 1/2) ("n3" . 1/2)))))

  (let ()
    (define a-graph (make-empty-graph))
    (define n0 (new-basic-node a-graph "zero" '()))
    (define n1 (new-basic-node a-graph "one" '(1)))
    (define n2 (new-basic-node a-graph "two" '(1)))
    (define n3 (new-basic-node a-graph "three" '(0)))
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
                               (2 . ("n5" "n2"))
                               (1 . ("n1" "n3"))
                               (0 . ("n0")))))

    (check-equal? (node-info-identification (hash-ref (graph-nodes a-graph) "n5"))
                  '(0))

    (check-equal? (build-y-coordinates a-graph)
                  (make-hash '(("n1" . 1)
                               ("n3" . 0)
                               ("n0" . 1/2)
                               ("n4" . 1/2)
                               ("n2" . 1)
                               ("n5" . 0)))))

  
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

    (check-equal? (build-layers a-graph)
                  (make-hash '((0 . ("n5"))
                               (1 . ("n0" "n6"))
                               (2 . ("n8" "n3" "n1"))
                               (3 . ("n11" "n9" "n2"))
                               (4 . ("n10" "n4"))
                               (5 . ("n7")))))
    
    (check-equal? (build-y-coordinates a-graph)
                  (make-hash '(("n8" . 0)
                               ("n9" . 0)
                               ("n4" . 1)
                               ("n11" . 1)
                               ("n2" . 1/2)
                               ("n1" . 1/2)
                               ("n3" . 1)
                               ("n0" . 1)
                               ("n10" . 0)
                               ("n7" . 1/2)
                               ("n6" . 0)
                               ("n5" . 1/2))))))
