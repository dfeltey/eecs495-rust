#lang racket/base
(require racket/set
         racket/format)

;; This uses the topological sorting algorithm
;; from Wikipedia attributed to Kahn (1962). 
(define (topological-sort neighbors)
  (define comes-after (build-comes-after neighbors))
  (define comes-before (build-comes-before comes-after))
  
  (define (remove-edge from to)
    (hash-set! comes-after
               from
               (remove to (hash-ref comes-after from)))
    (hash-set! comes-before
               to
               (remove from (hash-ref comes-before to))))

  (define no-incoming-edges-in-original-graph
    (for/list ([(node preds) (in-hash comes-before)]
               #:when (null? preds))
      node))
  
  (define sorted '())
  (let loop ([no-incoming-edges
              no-incoming-edges-in-original-graph])
    (unless (null? no-incoming-edges)
      (define n (car no-incoming-edges))
      (set! sorted (cons n sorted))
      (for ([m (in-list (hash-ref comes-after n))])
        (remove-edge n m)
        (cond
          [(null? (hash-ref comes-before m))
           (loop (cons m (cdr no-incoming-edges)))]
          [else
           (loop (cdr no-incoming-edges))]))))
  
  (reverse sorted))

(define (build-comes-after neighbors)
  (define comes-after (hash-copy neighbors))
  (define all-pointed-to
    (apply append (for/list ([(k v) (in-hash neighbors)]) v)))
  (for ([node (in-list all-pointed-to)])
    (unless (hash-ref comes-after node #f)
      (hash-set! comes-after node '())))
  comes-after)

(define (build-comes-before comes-after)
  (define node-names (for/list ([(k v) (in-hash comes-after)]) k))
  (define comes-before (make-hash))
  (for ([node-name (in-list node-names)])
    (hash-set! comes-before node-name '()))
  (for ([node-name (in-list node-names)])
    (for ([neighbor (in-list (hash-ref comes-after node-name))])
      (hash-set!
       comes-before
       neighbor
       (cons node-name (hash-ref comes-before neighbor)))))
  comes-before)

(module+ test
  (require rackunit)
  (check-equal? (topological-sort (hash)) '())
  (check-equal? (topological-sort (hash 'x '())) '(x))
  (check-equal? (topological-sort (hash 'x '(y) 'y '()))
                '(x y))
  (check-equal? (topological-sort (hash 'x '(y)))
                '(x y))
  (check-equal? (topological-sort
                 (hash 'x '(y z) 'y '(z) 'z '()))
                '(x y z))
  (check-equal? (topological-sort
                 (hash 'x '(y z) 'y '(z)))
                '(x y z)))

(define (longest-distances neighbors)
  (define comes-before (build-comes-before
                        (build-comes-after neighbors)))
  (define ans (make-hash))
  (for ([node (in-list (topological-sort neighbors))])
    (define preds (hash-ref comes-before node))
    (define dist
      (cond
        [(null? preds) 0]
        [else
         (+ 1 (for/fold ([biggest (hash-ref ans (car preds))])
                        ([pred (in-list (cdr preds))])
                (max biggest (hash-ref ans pred))))]))
    (hash-set! ans node dist))
  ans)

(module+ test
  (require rackunit)
  (check-equal? (longest-distances (hash)) (make-hash))
  (check-equal? (longest-distances (hash 'x '()))
                (make-hash '((x . 0))))
  (check-equal? (longest-distances (hash 'x '(y)))
                (make-hash '((x . 0) (y . 1))))
  (check-equal? (longest-distances
                 (hash 'x '(y z) 'y '(z) 'z '()))
                (make-hash '((x . 0) (y . 1) (z . 2)))))