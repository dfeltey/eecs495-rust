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
 graph-neighbors
 graph-hb
 node-info-pict
 get-neighbors
 gen-dot-code
 graph?)

(define (in-nodes a-graph)
  (in-hash-keys (graph-nodes a-graph)))

(define (node-in-graph? a-graph node)
  (and (hash-ref (graph-nodes a-graph) node #f) #t))
(define (get-neighbors a-graph node)
  (hash-ref (graph-neighbors a-graph) node '()))
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

;; neighbors : string -o> (listof string)
;; backwards-neighbors : string -o> (listof string)
;; hb : string -o> (listof string)
;; nodes : string -o> yinfo
(struct graph (neighbors backwards-neighbors hb nodes) #:transparent)

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

(define (add-edge! a-graph n1 n2)
  (define neighbors (graph-neighbors a-graph))
  (define backwards-neighbors (graph-backwards-neighbors a-graph))
  (hash-set! neighbors n1 (cons n2 (get-neighbors a-graph n1)))
  (hash-set! backwards-neighbors n2 (cons n1 (get-backwards-neighbors a-graph n2))))
(define (remove-edge! a-graph n1 n2)
  (define neighbors (graph-neighbors a-graph))
  (define backwards-neighbors (graph-backwards-neighbors a-graph))
  (hash-set! neighbors n1 (remove n2 (get-neighbors a-graph n1)))
  (hash-set! backwards-neighbors n2 (remove n1 (get-backwards-neighbors a-graph n2))))
(define (add-hb-edge! a-graph n1 n2)
  (define hb (graph-hb a-graph))
  (hash-set! hb n1 (cons n2 (hash-ref hb n1 '()))))
(define (make-empty-graph)
  (define neighbors (make-hash))
  (define backwards-neighbors (make-hash))
  (define hb (make-hash))
  (define nodes (make-hash))
  (graph neighbors backwards-neighbors hb nodes))

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
  (for ([(parent children) (in-hash (graph-neighbors a-graph))])
    (for ([child (in-list children)])
      (define child-info (hash-ref (graph-nodes a-graph) child))
      (fprintf port "  \"~a\" -> \"~a\"~a\n"
               parent
               child
               (if (node-info-name child-info)
                   ""
                   " [arrowhead=none]"))))
  (for ([(parent children) (in-hash (graph-hb a-graph))])
    (for ([child (in-list children)])
      (fprintf port "  \"~a\" -> \"~a\" [style=dashed, color=red, constraint=false]\n" parent child)))
  (fprintf port "}\n"))
