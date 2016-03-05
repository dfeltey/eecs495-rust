#lang racket/base
(require "visualize-struct.rkt"
         racket/format
         racket/match
         pict)

(module+ test (require rackunit))

(define pth
  (string->path "/Users/robby/git/dfeltey/eecs495-rust/mini-thd/ex2.rkt"))

(define example
  (list
   (t-choice '() (srcloc pth 21 5 342 2))
   (t-choice '#(() join) (srcloc pth 18 5 266 70))
   (t-choice '(2) (srcloc pth 9 2 173 85))
   (t-choice '(2) (srcloc pth 12 6 208 49))
   (t-choice '(0) (srcloc pth 9 2 173 85))
   (t-choice '(0) (srcloc pth 12 6 208 49))
   (t-choice '(2) (srcloc pth 13 7 220 26))
   (t-choice '(0) (srcloc pth 13 7 220 26))
   (t-choice '(1) (srcloc pth 9 2 173 85))
   (t-choice '(1) (srcloc pth 12 6 208 49))
   (t-choice '(0) (srcloc pth 18 21 282 2))
   (t-choice '(2) (srcloc pth 20 21 332 2))
   (t-choice '(1) (srcloc pth 13 7 220 26))
   (t-choice '(1) (srcloc pth 19 21 307 2))
   (t-par '() (srcloc pth 18 5 266 70) 3)
   (t-choice '() (srcloc pth 6 24 142 2))
   (t-choice '() (srcloc pth 5 24 112 2))
   (t-choice '() (srcloc pth 4 24 82 2))))


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

(define (build-basic-graph transcript)
  (define a-graph (make-empty-graph))
  
  (define first-node (new-basic-node a-graph "start"))
  (let loop ([transcript transcript]
             
             [last-thing first-node]
             
             ;; thread-identifier -> node
             [threads (hash '() first-node)])
    (cond
      [(null? transcript) (void)]
      [else
       (match (car transcript)
         [(t-par identification srcloc size)
          (define par-start (new-basic-node a-graph (~a "par line " (srcloc-line srcloc))))
          (define node (hash-ref threads identification))
          (add-edge! a-graph node par-start)
          (add-hb-edge! a-graph last-thing par-start)
          (loop (cdr transcript)
                par-start
                (for/fold ([threads (hash-remove threads identification)])
                          ([i (in-range size)])
                  (define child-identification (cons i identification))
                  (hash-set threads
                            (cons i identification)
                            par-start)))]
         [(t-choice (vector identification 'join) srcloc)
          (define join-node (new-basic-node a-graph (~a "join line " (srcloc-line srcloc))))
          (add-hb-edge! a-graph last-thing join-node)
          (define to-remove
            (for/list ([(thread-identification node) (in-hash threads)]
                       #:when (and (pair? thread-identification)
                                   (equal? (cdr thread-identification) identification)))
              (add-edge! a-graph node join-node)
              thread-identification))
          (define without-child-threads
            (for/fold ([threads threads])
                      ([identification-to-remove (in-list to-remove)])
              (hash-remove threads identification-to-remove)))
          (loop (cdr transcript)
                join-node
                (hash-set threads identification join-node))]
         [(t-choice identification srcloc)
          (define prev-node (hash-ref threads identification))
          (define next-node (new-basic-node a-graph (~a "line " (srcloc-line srcloc))))
          (add-edge! a-graph prev-node next-node)
          (add-hb-edge! a-graph last-thing next-node)
          (loop (cdr transcript)
                next-node
                (hash-set threads identification next-node))])]))
  a-graph)

(module+ test
  (define (mksrc line) (srcloc (syntax-source #'here) line 0 #f #f))
  (check-equal?
   (graph-neighbors
    (build-basic-graph (list (t-par '() (mksrc 1) 2)
                             (t-choice '(0) (mksrc 2))
                             (t-choice '(1) (mksrc 3))
                             (t-choice '#(() join) (mksrc 4)))))
   (make-hash '(("n1" . ("n3" "n2")) ("n2" . ("n4")) ("n0" . ("n1")) ("n3" . ("n4")))))
  (check-equal?
   (graph-hb
    (build-basic-graph (list (t-par '() (mksrc 1) 2)
                             (t-choice '(0) (mksrc 2))
                             (t-choice '(1) (mksrc 3))
                             (t-choice '#(() join) (mksrc 4)))))
   (make-hash '(("n1" . ("n2")) ("n2" . ("n3")) ("n0" . ("n1")) ("n3" . ("n4"))))))
