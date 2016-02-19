#lang racket/base
(require (for-syntax syntax/parse
                     racket/base)
         racket/format)
(provide true false var
         #%app := par
         (rename-out [-define define]
                     [datum #%datum]
                     [module-begin #%module-begin]
                     [begin seq]))

(define-syntax (var stx)
  (raise-syntax-error 'var
                      "must be used right inside `define` or at the top-level"
                      stx))

(begin-for-syntax
  (define-syntax-class var-decl
    #:description "variable declaration"
    (pattern (var id:id rhs:expr)))
  (define-syntax-class define-header
    #:description "define header"
    (pattern (f-id:id x-id:id ...+)))
  (define-syntax-class define-or-var
    #:description "define header"
    (pattern (f-id:id x-id:id ...+))))

(define-syntax (-define stx)
  (syntax-parse stx
    [(_ h:define-header var-decls:var-decl ... body)
     #'(define h (define var-decls.id var-decls.rhs) ... body)]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ define-or-var ... body)
     #`(#%module-begin
        (provide main)
        (define (main)
          #,@(for/list ([d-or-v (in-list (syntax->list #'(define-or-var ...)))])
               (syntax-parse d-or-v #:literals (var -define)
                 [(var id:id expr:expr)
                  #'(define id expr)]
                 [(-define . whatever)
                  d-or-v]))
          body)
        (module+ main (run-many-trials main)))]))

(define-syntax (true stx) #'#t)
(define-syntax (false stx) #'#t)
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     (exact-integer? (syntax-e #'d))
     #'(#%datum . d)]))

(define-syntax (:= stx)
  (syntax-parse stx
    [(_ id:id expr:expr)
     #'(set! id expr)]))

(define-syntax (par stx)
  (syntax-parse stx
    [(_ e:expr ...)
     #'(do-par (λ () e) ...)]))

(define (do-par . thunks)
  (let ([done-sema (make-semaphore)])
    (define new-semas
      (for/list ([thunk (in-list thunks)])
        (define start-sema (make-semaphore 0))
        (thread
         (λ ()
           (semaphore-wait start-sema)
           (thunk)
           (channel-put new-waiters '())
           (semaphore-post done-sema)))
        start-sema))
    (channel-put new-waiters new-semas)
    (for ([thunk (in-list thunks)])
      (semaphore-wait done-sema))))

(define (maybe-swap-thread)
  (define sema (make-semaphore 0))
  (channel-put new-waiters (list sema))
  (semaphore-wait sema))

(define new-waiters (make-channel))
(void
 (thread
  (λ ()
    (let loop ([waiters '()])
      (sync
       (wrap-evt
        new-waiters
        (λ (semas)
          (define new-waiters (append semas waiters))
          (cond
            [(null? new-waiters)
             (loop '())]
            [else
             (define next-one (pick-one new-waiters))
             (semaphore-post next-one)
             (loop (remove next-one new-waiters))]))))))))


(define (pick-one lst)
  (list-ref lst (random (length lst))))

(define (run-many-trials thunk)
  (let loop ([current-trials (hash)]
             [current-summary (hash)]
             [min 100])
    (define next (thunk))
    (define next-trials
      (hash-set current-trials
                next
                (+ (hash-ref current-trials next 0) 1)))
    (define next-summary (summarize next-trials))
    (cond
      [(and (zero? min) (equal? next-summary current-summary))
       (for ([k (in-list (sort (hash-keys next-summary)
                               string<?
                               #:key ~s))])
         (printf "~a% ~s\n"
                 (~r (hash-ref current-summary k))
                 k))]
      [else
       (loop next-trials next-summary (max 0 (- min 1)))])))

(define (summarize ht)
  (define denom (apply + (hash-values ht)))
  (for/hash ([(k v) (in-hash ht)])
    (values k (/ (round (* 1000 (/ v denom))) 1000))))
