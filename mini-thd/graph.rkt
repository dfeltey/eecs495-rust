#lang racket/base
(require racket/format
         racket/contract
         racket/list
         pict)
(provide
 (contract-out
  [new-basic-node (-> graph?
                      (or/c #f string?)
                      (listof exact-nonnegative-integer?)
                      string?)]
  [make-empty-graph (-> graph?)]
  [add-edge! (-> graph? string? string? void?)]
  [add-hb-edge! (-> graph? string? string? void?)]
  [remove-edge! (-> graph? string? string? void?)]
  [node->node-info (->i ([g graph?]
                         [n string?])
                        #:pre (g n) (λ (n) (node-in-graph? g n))
                        [res node-info?])]
  [node-in-graph? (-> graph? string? boolean?)])
 (struct-out node-info)
 in-nodes
 (rename-out [derived-graph-neighbors graph-neighbors])
 (rename-out [derived-graph-hb graph-hb])
 node-info-pict
 get-neighbors
 gen-dot-code
 graph?)

(define (derived-graph-neighbors a-graph)
  (make-derived-graph-neighbors a-graph get-neighbors))
(define (derived-graph-hb a-graph)
  (make-derived-graph-neighbors a-graph get-hb-neighbors))
(define (make-derived-graph-neighbors a-graph node->some-neighbors)
  (define ht (make-hash))
  (for ([node (in-nodes a-graph)])
    (for ([neighbor (in-list (node->some-neighbors a-graph node))])
      (hash-set! ht node (cons neighbor (hash-ref ht node '()))))
    (hash-set! ht node (reverse (hash-ref ht node '()))))
  ht)

(define (in-nodes a-graph)
  (in-hash-keys (graph-nodes a-graph)))

(define (node-in-graph? a-graph node)
  (and (hash-ref (graph-nodes a-graph) node #f) #t))
(define (get-neighbors a-graph node [type 'normal])
  (for/list ([edge (in-list (hash-ref (graph-edges a-graph) node '()))]
             #:when (equal? (edge-type edge) type))
    (edge-dest edge)))
(define (get-hb-neighbors a-graph node) (get-neighbors a-graph node 'hb))
(define (get-backwards-neighbors a-graph node)
  (hash-ref (graph-backwards-neighbors a-graph) node '()))
(define (node->node-info a-graph node)
  (hash-ref (graph-nodes a-graph) node))
(define ((mk-fail who a-graph node))
  (raise-argument-error
   who
   (format "node in graph, one of ~s"
           (sort (hash-keys (graph-nodes a-graph)) string<?))
   1
   a-graph node))

;; neighbors : string -o> (listof edge)
;; backwards-neighbors : string -o> (listof string)
;; nodes : string -o> yinfo
(struct graph (edges backwards-neighbors nodes) #:transparent)

;; dest : string
;; type : symbol
(struct edge (dest type) #:transparent)

(struct node-info (pict name identification) #:transparent)
(define (new-basic-node graph name identification)
  (define nodes (graph-nodes graph))
  (define n (~a "n" (hash-count nodes)))
  (hash-set! nodes n (node-info (if name (make-a-pict name) (blank)) name identification))
  n)

(define (make-a-pict name)
  (define basic (text name))
  (define space 10)
  (cc-superimpose
   (colorize (filled-rounded-rectangle (+ space (pict-width basic))
                                       (+ space (pict-height basic)))
             "white")
   basic))

(define (add-edge! a-graph n1 n2 [type 'normal])
  (define edges (graph-edges a-graph))
  (define backwards-neighbors (graph-backwards-neighbors a-graph))
  (define an-edge (edge n2 type))
  (hash-set! edges n1 (cons an-edge (hash-ref edges n1 '())))
  (hash-set! backwards-neighbors n2 (cons n1 (get-backwards-neighbors a-graph n2))))
(define (remove-edge! a-graph n1 n2 [type 'normal])
  (define edges (graph-edges a-graph))
  (define backwards-neighbors (graph-backwards-neighbors a-graph))
  (hash-set! edges n1 (remove (edge n2 type) (hash-ref edges n1 '())))
  (hash-set! backwards-neighbors n2 (remove n1 (get-backwards-neighbors a-graph n2))))
(define (add-hb-edge! a-graph n1 n2) (add-edge! a-graph n1 n2 'hb))
(define (make-empty-graph)
  (define neighbors (make-hash))
  (define backwards-neighbors (make-hash))
  (define nodes (make-hash))
  (graph neighbors backwards-neighbors nodes))

(module+ test
  (require rackunit)
  (let ()
    (define g (make-empty-graph))
    (define n0 (new-basic-node g "zero" '()))
    (define n1 (new-basic-node g "one" '()))
    (add-edge! g n0 n1)
    (check-equal? (get-neighbors g n0) (list n1))
    (remove-edge! g n0 n1)
    (check-equal? (get-neighbors g n0) (list))))

(define (gen-dot-code a-graph port)
  (fprintf port "digraph {\n")
  (fprintf port "  rankdir = LR\n")
  (for ([(node a-node-info) (in-hash (graph-nodes a-graph))])
    (define name (node-info-name a-node-info))
    (cond
      [name (fprintf port "  ~a [label=\"~a\"]\n" node name)]
      [else (fprintf port "  ~a [shape=point]\n" node)]))
  (printf "\n")
  (for ([(parent edges) (in-hash (graph-edges a-graph))])
    (for ([edge (in-list edges)])
      (define child (edge-dest edge))
      (define child-info (hash-ref (graph-nodes a-graph) child))
      (case (edge-type edge)
        [(normal)
         (fprintf port "  \"~a\" -> \"~a\"~a\n"
                  parent
                  child
                  (if (node-info-name child-info)
                      ""
                      " [arrowhead=none]"))]
        [(hb)
         (fprintf port "  \"~a\" -> \"~a\" [style=dashed, color=red, constraint=false]\n"
                  parent
                  child)])))
  (fprintf port "}\n"))
