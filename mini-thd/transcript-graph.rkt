#lang racket/base
(require "visualize-struct.rkt"
         "graph.rkt"
         racket/format
         racket/match
         racket/contract
         pict)

(provide
 (contract-out
  [build-transcript-graph
   (-> (listof any/c) graph?)]))

(module+ test (require rackunit)
  
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
     (t-choice '() (srcloc pth 4 24 82 2)))))

(define (build-transcript-graph transcript)
  (define a-graph (make-empty-graph))
  
  (define first-node (new-basic-node a-graph "start" '()))
  (let loop ([transcript transcript]
             
             [last-thing first-node]
             
             ;; thread-identifier -> node
             [threads (hash '() first-node)])
    (cond
      [(null? transcript) (void)]
      [else
       (match (car transcript)
         [(t-par identification srcloc size)
          (define par-start (new-basic-node a-graph
                                            (~a "par " (fmt-srcloc srcloc))
                                            identification))
          (define node (hash-ref threads identification))
          (add-edge! a-graph node par-start)
          (add-hb-edge! a-graph last-thing par-start)
          (loop (cdr transcript)
                par-start
                (for/fold ([threads (hash-remove threads identification)])
                          ([i (in-range size)])
                  (define child-identification (cons i identification))
                  (define child-start-node (new-basic-node a-graph #f child-identification))
                  (add-edge! a-graph par-start child-start-node)
                  (hash-set threads
                            (cons i identification)
                            child-start-node)))]
         [(t-choice (vector identification 'join) srcloc)
          (define join-node (new-basic-node a-graph
                                            (~a "join " (fmt-srcloc srcloc))
                                            identification))
          (add-hb-edge! a-graph last-thing join-node)
          (define to-remove
            (for/list ([(thread-identification node) (in-hash threads)]
                       #:when (and (pair? thread-identification)
                                   (equal? (cdr thread-identification) identification)))
              (define child-end-node (new-basic-node a-graph #f thread-identification))
              (add-edge! a-graph node child-end-node)
              (add-edge! a-graph child-end-node join-node)
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
          (define next-node (new-basic-node a-graph
                                            (fmt-srcloc srcloc)
                                            identification))
          (add-edge! a-graph prev-node next-node)
          (add-hb-edge! a-graph last-thing next-node)
          (loop (cdr transcript)
                next-node
                (hash-set threads identification next-node))])]))
  a-graph)

(define (fmt-srcloc a-srcloc)
  (match-define (srcloc src line column position span) a-srcloc)
  (define file
    (cond
      [(path? src)
       (define-values (base name dir?) (split-path src))
       name]
      [else "??.rkt"]))
  (~a file
      (cond
        [(and line column)
         (~a ":" line ":" column)]
        [position
         (~a "::" position)]
        [else ""])))

(module+ test
  (define (mksrc line) (srcloc (syntax-source #'here) line 0 #f #f))
  (check-equal?
   (graph-neighbors
    (build-transcript-graph (list (t-par '() (mksrc 1) 2)
                                  (t-choice '(0) (mksrc 2))
                                  (t-choice '(1) (mksrc 3))
                                  (t-choice '#(() join) (mksrc 4)))))
   (make-hash '(("n0" . ("n1")) ("n1" . ("n3" "n2")) ("n2" . ("n4")) ("n3" . ("n5"))
                                ("n4" . ("n7")) ("n5" . ("n8"))  ("n8" . ("n6")) ("n7" . ("n6"))))
  (check-equal?
   (graph-hb
    (build-transcript-graph (list (t-par '() (mksrc 1) 2)
                                  (t-choice '(0) (mksrc 2))
                                  (t-choice '(1) (mksrc 3))
                                  (t-choice '#(() join) (mksrc 4)))))
   (make-hash '(("n0" . ("n1")) ("n1" . ("n4")) ("n4" . ("n5")) ("n5" . ("n6")))))))
