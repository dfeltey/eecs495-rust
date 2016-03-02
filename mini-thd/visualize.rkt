#lang racket/base
(require "visualize-struct.rkt"
         racket/format
         racket/match)

(define pth
  (string->path "/Users/robby/git/dfeltey/eecs495-rust/mini-thd/ex2.rkt"))
#;
(define example
  (list
   (t-choice '#(() join) (srcloc pth 428 5 15283 6))
   (t-choice '#((1) join) (srcloc pth 431 12 15359 6))
   (t-choice '(1 1) (srcloc pth 433 23 15432 6))
   (t-choice '(0 1) (srcloc pth 432 23 15389 6))
   (t-choice '(0) (srcloc pth 429 16 15306 6))
   (t-par '(1) (srcloc pth 431 12 15359 6) 2)
   (t-par '() (srcloc pth 428 5 15283 6) 2)
   (t-choice '#(() join) (srcloc pth 421 5 15092 6))
   (t-choice '#((1) join) (srcloc pth 424 12 15168 6))
   (t-choice '(1 1) (srcloc pth 426 23 15241 6))
   (t-choice '(0 1) (srcloc pth 425 23 15198 6))
   (t-choice '(0) (srcloc pth 422 16 15115 6))
   (t-par '(1) (srcloc pth 424 12 15168 6) 2)
   (t-par '() (srcloc pth 421 5 15092 6) 2)))
(define example
(list
 (t-choice
  '()
  (srcloc
   pth
   21
   5
   342
   2))
 (t-choice
  '#(() join)
  (srcloc
   pth
   18
   5
   266
   70))
 (t-choice
  '(2)
  (srcloc
   pth
   9
   2
   173
   85))
 (t-choice
  '(2)
  (srcloc
   pth
   12
   6
   208
   49))
 (t-choice
  '(0)
  (srcloc
   pth
   9
   2
   173
   85))
 (t-choice
  '(0)
  (srcloc
   pth
   12
   6
   208
   49))
 (t-choice
  '(2)
  (srcloc
   pth
   13
   7
   220
   26))
 (t-choice
  '(0)
  (srcloc
   pth
   13
   7
   220
   26))
 (t-choice
  '(1)
  (srcloc
   pth
   9
   2
   173
   85))
 (t-choice
  '(1)
  (srcloc
   pth
   12
   6
   208
   49))
 (t-choice
  '(0)
  (srcloc
   pth
   18
   21
   282
   2))
 (t-choice
  '(2)
  (srcloc
   pth
   20
   21
   332
   2))
 (t-choice
  '(1)
  (srcloc
   pth
   13
   7
   220
   26))
 (t-choice
  '(1)
  (srcloc
   pth
   19
   21
   307
   2))
 (t-par
  '()
  (srcloc
   pth
   18
   5
   266
   70)
  3)
 (t-choice
  '()
  (srcloc
   pth
   6
   24
   142
   2))
 (t-choice
  '()
  (srcloc
   pth
   5
   24
   112
   2))
 (t-choice
  '()
  (srcloc
   pth
   4
   24
   82
   2))))



(define neighbors (make-hash))
(define hb (make-hash))
(define nodes (make-hash))

(define (new-node name)
  (define n (~a "n" (hash-count nodes)))
  (hash-set! nodes n name)
  n)
(define (add-edge! n1 n2)
  (hash-set! neighbors n1 (cons n2 (hash-ref neighbors n1 '()))))
(define (add-hb-edge! n1 n2)
  (hash-set! hb n1 (cons n2 (hash-ref hb n1 '()))))

(define first-node (new-node "start"))
(let loop ([transcript (reverse example)]

           [last-thing first-node]
           
           ;; thread-identifier -> node
           [threads (hash '() first-node)])
  (cond
    [(null? transcript) (void)]
    [else
     (match (car transcript)
       [(t-par identification srcloc size)
        (define par-start (new-node (~a "par line " (srcloc-line srcloc))))
        (define node (hash-ref threads identification))
        (add-edge! node par-start)
        (add-hb-edge! last-thing par-start)
        (loop (cdr transcript)
              par-start
              (for/fold ([threads (hash-remove threads identification)])
                        ([i (in-range size)])
                (define child-identification (cons i identification))
                (hash-set threads
                          (cons i identification)
                          par-start)))]
       [(t-choice (vector identification 'join) srcloc)
        (define join-node (new-node (~a "join line " (srcloc-line srcloc))))
        (add-hb-edge! last-thing join-node)
        (define to-remove
          (for/list ([(thread-identification node) (in-hash threads)]
                     #:when (equal? (cdr thread-identification) identification))
            (add-edge! node join-node)
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
        (define next-node (new-node (~a "line " (srcloc-line srcloc))))
        (add-edge! prev-node next-node)
        (add-hb-edge! last-thing next-node)
        (loop (cdr transcript)
              next-node
              (hash-set threads identification next-node))])]))

(printf "digraph {\n")
(for ([(node name) (in-hash nodes)])
  (printf "  ~a [label=\"~a\"]\n" node name))
(printf "\n")
(for ([(parent children) (in-hash neighbors)])
  (for ([child (in-list children)])
    (printf "  \"~a\" -> \"~a\"\n" parent child)))
(for ([(parent children) (in-hash hb)])
  (for ([child (in-list children)])
    (printf "  \"~a\" -> \"~a\" [style=dashed, color=red]\n" parent child)))
(printf "}\n")

