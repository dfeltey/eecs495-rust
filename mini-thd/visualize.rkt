#lang racket/base
(require "graph.rkt"
         "ld.rkt"
         "sync.rkt"
         pict
         racket/list)
(provide graph->pict
         build-layers)

(define row-gap-size 10)
(define column-gap-size 40)

(define (node-height a-graph n)
  (define pct (node-pict a-graph n))
  (if (pict? pct) (pict-height pct) 0))
(define (node-width a-graph n)
  (define pct (node-pict a-graph n))
  (if (pict? pct) (pict-width pct) 0))
(define (node-pict a-graph n)
  (node-info-pict (hash-ref (graph-nodes a-graph) n)))

(define (graph->pict a-graph)
  (define layers (build-layers a-graph))
  (define y-coordinates (build-y-coordinates layers a-graph))
  (define diagram-main-height (build-diagram-main-height a-graph layers))

  (define ld-with-hb-edges
    (longest-distances (combine-edges (graph-neighbors a-graph)
                                      (graph-hb a-graph))))
  (define hb-layers (make-hash))
  (for ([(node dist) (in-hash ld-with-hb-edges)])
    (hash-set! hb-layers dist (cons node (hash-ref hb-layers dist '()))))
  (define layer-centers
    (apply vector (build-layer-centers a-graph hb-layers)))
  
  (define nodes-pict
    (panorama
     (for*/fold ([main (blank)])
                ([(layer-index nodes) (in-hash hb-layers)]
                 [node (in-list nodes)])
       (define p (node-pict a-graph node))
       (define x (- (vector-ref layer-centers layer-index)
                    (/ (pict-width p) 2)))
       (define y (- (* (hash-ref y-coordinates node)
                       diagram-main-height)
                    (/ (pict-height p) 2)))
       (pin-over main x y p))))
  (add-edges a-graph nodes-pict))

(define (add-edges a-graph nodes-pict)
  (add-edges-from-hash
   (add-edges-from-hash
    nodes-pict
    a-graph
    (graph-neighbors a-graph) 3 "black")
   a-graph 
   (graph-hb a-graph) 1 "red"))

(define (add-edges-from-hash main a-graph neighbors-hash width color)
  (for*/fold ([main main])
             ([(node a-node-info) (in-hash (graph-nodes a-graph))]
              [neighbor (in-list (hash-ref neighbors-hash node '()))])
    (pin-line/under main
                    (node-pict a-graph node)
                    (node-pict a-graph neighbor)
                    width color)))

(define (pin-line/under main from to width color)
  (cc-superimpose
   (colorize
    (linewidth width (launder (pin-line (ghost main) from cc-find to cc-find)))
    color)
   main))

(define (build-layer-centers a-graph layers)
  (define-values (_1 _2 starts)
    (for/fold ([last-layer-right-edge 0]
               [first? #t]
               [starts '()])
              ([layer-index (in-list (sort (hash-keys layers) <))])
      (define layer (hash-ref layers layer-index))
      (define max-width (for/fold ([max-width 0])
                                  ([node (in-list layer)])
                          (max max-width (node-width a-graph node))))
      (values (+ max-width
                 last-layer-right-edge
                 (if first? 0 column-gap-size))
              #f
              (cons (+ last-layer-right-edge
                       (/ max-width 2)
                       (if first? 0 column-gap-size))
                    starts))))
  (reverse starts))

;; returns the height of the part of the diagram that's between
;; the y-coordinates, which are in [0,1]. This won't be quite
;; the height because those [0,1] coordinates assume that the
;; nodes have zero height
(define (build-diagram-main-height a-graph layers)
  (for/fold ([biggest 0])
            ([(layer-index nodes) (in-hash layers)])
    (define node-count (length nodes))
    (define this-size
      (for/sum ([node (in-list nodes)]
                [i (in-naturals)])
        (cond
          [(= i 0) (/ (node-height a-graph node) 2)]
          [(= i (- node-count 1))
           (+ row-gap-size (/ (node-height a-graph node) 2))]
          [else
           (+ (node-height a-graph node) row-gap-size)])))
    (max biggest this-size)))

(define (build-y-coordinates layers a-graph)
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

(define (combine-edges edges1 edges2)
  (define ht (make-hash))
  (define (add-em edges)
    (for ([(k v) (in-hash edges)])
      (hash-set! ht k (remove-duplicates (append (hash-ref ht k '()) v)))))
  (add-em edges1)
  (add-em edges2)
  ht)

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
    (check-equal? (build-y-coordinates (build-layers a-graph) a-graph)
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

    (check-equal? (build-y-coordinates (build-layers a-graph) a-graph)
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
    
    (check-equal? (build-y-coordinates (build-layers a-graph) a-graph)
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
