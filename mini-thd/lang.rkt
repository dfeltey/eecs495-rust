#lang racket/base
(require (for-syntax syntax/parse
                     racket/base)
         racket/format
         racket/match
         racket/pretty
         "sync.rkt")

(provide true false var
         #%app := par +
         (rename-out [-define define]
                     [datum #%datum]
                     [module-begin #%module-begin]
                     [begin seq]))

(define-values (par/proc maybe-swap-thread/proc)
  (start-server (λ (lst) (list-ref lst (random (length lst))))))

(define-syntax (maybe-swap-thread stx)
  #`(maybe-swap-thread/proc '#,(syntax-source stx)
                            #,(syntax-line stx)
                            #,(syntax-column stx)))

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
                  [(set! _ new-expr)
                   #`(let ([nv new-expr])
                       (maybe-swap-thread stx)
                       (set! secret-id nv))]
                  [x
                   (identifier? #'x)
                   #`(begin
                       (maybe-swap-thread stx)
                       secret-id)]))))))]))

(define-syntax (par stx)
  (syntax-parse stx
    [(_ e:expr ...+)
     #`(par/proc '#,(syntax-source stx)
                 #,(syntax-line stx)
                 #,(syntax-column stx)
                 (λ () e) ...)]))

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
     (syntax/loc stx (set! id expr))]))

(define (run-many-trials thunk)
  (let loop ([current-trials (hash)]
             [current-summary (hash)]
             [trials 0])
    (define next (thunk))
    (define next-trials
      (hash-set current-trials
                next
                (+ (hash-ref current-trials next 0) 1)))
    (define next-summary (summarize next-trials))
    (cond
      [(and (trials . >= . 100) (equal? next-summary current-summary))
       (printf "~a trials\n" trials)
       (for ([k (in-list (sort (hash-keys next-summary)
                               string<?
                               #:key ~s))])
         (printf "~a% ~s\n"
                 (~r (* 100 (hash-ref current-summary k)))
                 k))]
      [else
       (loop next-trials next-summary (+ trials 1))])))

(define (summarize ht)
  (define denom (apply + (hash-values ht)))
  (for/hash ([(k v) (in-hash ht)])
    (values k (/ (round (* 1000 (/ v denom))) 1000))))
