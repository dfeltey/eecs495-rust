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

(define semaphore-index 0)
(struct sema (count index))
(define (semaphore n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'semaphore "exact-nonnegative-integer?" n))
  (set! semaphore-index (+ semaphore-index 1))
  (sema n semaphore-index)) 
  

(define-syntax (var stx)
  (syntax-parse stx
    [(_ id:id init:expr)
     (with-syntax ([(secret-id) (generate-temporaries (list #'id))])
       #'(begin
           (define secret-id init)
           (define-syntax id
             (make-set!-transformer
              (λ (stx)
                (syntax-parse stx #:literals (set!)
                  [(_ _ new-expr)
                   #'(let ([nv new-expr])
                       (maybe-swap-thread)
                       (set! secret-id nv))]
                  [x
                   (identifier? #'x)
                   #'(begin
                       (maybe-swap-thread)
                       secret-id)]))))))]))

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
                  d-or-v]
                 [(-define . whatever)
                  d-or-v]))
          body)
        ;(module+ main (printf "starting\n") (main))
        
        (module+ main (time (run-many-trials main))))]))

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
    [(_ e:expr ...+)
     #'(do-par (λ () e) ...)]))

(define (do-par thunk1 . thunks)
  (define new-semas+thds
    (for/list ([thunk (in-list thunks)])
      (define start-sema (make-semaphore 0))
      (cons (thread
             (λ ()
               (semaphore-wait start-sema)
               (thunk)
               (channel-put remove-me (void))))
            start-sema)))
  (channel-put new-waiters-chan (map cdr new-semas+thds))
  (define join-sema (make-semaphore))
  (channel-put join-on (vector (map car new-semas+thds) join-sema))
  (thunk1)
  (channel-put remove-me (void))
  (semaphore-wait join-sema))

(define (maybe-swap-thread)
  (define sema (make-semaphore 0))
  (channel-put maybe-swap sema)
  (semaphore-wait sema))

(define new-waiters-chan (make-channel))
(define maybe-swap (make-channel))
(define join-on (make-channel))
(define remove-me (make-channel))
(void
 (thread
  (λ ()
    (let loop ([waiters '()]
               [previous-pending-joins '()])
      (define pending-joins
        (let loop ([pending-joins previous-pending-joins])
          (cond
            [(null? pending-joins) null]
            [else
             (define pending-join (car pending-joins))
             (define new-thds (filter (λ (t) (not (thread-dead? t)))
                                      (vector-ref pending-join 0)))
             (cond
               [(null? new-thds)
                (semaphore-post (vector-ref pending-join 1))
                (loop (cdr pending-joins))]
               [else
                (cons (vector new-thds (vector-ref pending-join 1))
                      (loop (cdr pending-joins)))])])))
      (sync
       (wrap-evt
        remove-me
        (λ (_)
          (cond
            [(null? waiters) (loop waiters pending-joins)]
            [else
             (define next-one (pick-one waiters))
             (semaphore-post next-one)
             (loop (remove next-one waiters)
                   pending-joins)])))
       (wrap-evt
        join-on
        (λ (thds+join-sema)
          (loop waiters
                (cons thds+join-sema pending-joins))))
       (wrap-evt
        new-waiters-chan
        (λ (semas)
          (loop (append semas waiters)
                pending-joins)))
       (wrap-evt
        maybe-swap
        (λ (sema)
          (define new-waiters (cons sema waiters))
          (define next-one (pick-one new-waiters))
          (semaphore-post next-one)
          (loop (remove next-one new-waiters)
                pending-joins))))))))


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
      [(and (<= min 0) (equal? next-summary current-summary))
       (printf "~a trials\n" (+ 100 (- min)))
       (for ([k (in-list (sort (hash-keys next-summary)
                               string<?
                               #:key ~s))])
         (printf "~a% ~s\n"
                 (~r (* 100 (hash-ref current-summary k)))
                 k))]
      [else
       (loop next-trials next-summary (max 0 (- min 1)))])))

(define (summarize ht)
  (define denom (apply + (hash-values ht)))
  (for/hash ([(k v) (in-hash ht)])
    (values k (/ (round (* 1000 (/ v denom))) 1000))))
