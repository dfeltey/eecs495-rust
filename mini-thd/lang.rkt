#lang racket/base
(require (for-syntax syntax/parse
                     racket/base)
         racket/format
         racket/class
         racket/stxparam
         racket/file
         racket/system
         racket/bool
         "graph.rkt"
         "transcript-graph.rkt"
         "sync.rkt"
         "visualize.rkt")

(provide true false while mut
         dot rec sema wait post if print
         #%app := par + < > <= >= = * equal?
         or and implies not xor nor nand
         (rename-out [-define define]
                     [-let let]
                     [datum #%datum]
                     [module-begin #%module-begin]
                     [begin seq]))

(define-syntax-parameter the-par/proc #f)
(define-syntax-parameter the-maybe-swap-thread/proc #f)
(define-syntax-parameter the-make-sema #f)

(define-syntax (sema stx)
  (define make-sema (syntax-parameter-value #'the-make-sema))
  (unless make-sema
    (raise-syntax-error #f "server not set up"))
  (syntax-parse stx
    [(_ arg)
     #`(#,make-sema arg #,(syntax/loc stx (here)))]))
(define (post sema)
  (unless (and (object? sema) (is-a? sema sema<%>))
    (raise-argument-error 'post "sema" sema))
  (send sema post))
(define-syntax (wait stx)
  (syntax-parse stx
    [(_ sema)
     #`(wait/proc sema #,(syntax/loc stx (here)))]))
    
(define (wait/proc sema srcloc)
  (unless (and (object? sema) (is-a? sema sema<%>))
    (raise-argument-error 'wait "sema" sema))
  (send sema wait srcloc))

(define-syntax (maybe-swap-thread stx)
  (define maybe-swap-thread/proc (syntax-parameter-value #'the-maybe-swap-thread/proc))
  (unless maybe-swap-thread/proc
    (raise-syntax-error #f "server not set up"))
  #`(#,maybe-swap-thread/proc #,(syntax/loc stx (here))))

(define-syntax (mut stx) (raise-syntax-error 'mut "expected to be just inside `let'" stx))

(define-syntax (-let stx)
  (syntax-parse stx #:literals (mut)
    [(_ mut id:id init:expr)
     (with-syntax ([(secret-id) (generate-temporaries (list #'id))])
       #'(begin
           (define secret-id init)
           (define-syntax id
             (make-set!-transformer
              (λ (stx)
                (syntax-parse stx #:literals (set!)
                  [(set! _ new-expr)
                   #`(let ([nv new-expr])
                       #,(syntax/loc stx (maybe-swap-thread))
                       (set! secret-id nv))]
                  [x
                   (identifier? #'x)
                   #`(begin
                       #,(syntax/loc stx (maybe-swap-thread))
                       secret-id)]))))))]
    [(_ id:id init:expr)
     (with-syntax ([(secret-id) (generate-temporaries (list #'id))])
       #'(begin
           (define secret-id init)
           (define-syntax id
             (make-set!-transformer
              (λ (stx)
                (syntax-parse stx #:literals (set!)
                  [(set! _ new-expr)
                   (raise-syntax-error 'let "immutable variable" stx)]
                  [x
                   (identifier? #'x)
                   #'secret-id]))))))]))

(define-syntax (print stx)
  (syntax-parse stx
    [(_ id:id ...)
     #'(print/proc (cons 'id id) ...)]))
(define (print/proc . stuff)
  (for ([x (in-list stuff)]
        [i (in-naturals)])
    (unless (zero? i) (display " "))
    (printf "~a ~s" (car x) (cdr x)))
  (newline))

(define-syntax (par stx)
  (syntax-parse stx
    [(_ e:expr ...+)
     (define par/proc (syntax-parameter-value #'the-par/proc))
     (unless par/proc (raise-syntax-error #f "server not set up"))
     #`(#,par/proc #,(syntax/loc stx (here))
                 (λ () e) ...)]))

(begin-for-syntax
  (define-syntax-class var-decl
    #:description "variable declaration"
    (pattern (let mut id:id rhs:expr))
    (pattern (let id:id rhs:expr)))
  (define-syntax-class define-or-var
    #:description "define header"
    (pattern (f-id:id x-id:id ...+)))
  (define-splicing-syntax-class maybe-left-to-right
    (pattern (~seq #:left-to-right)
             #:with left-to-right? #t)
    (pattern (~seq)
             #:with left-to-right? #f))
  (define-splicing-syntax-class maybe-histogram
    (pattern (~seq #:histogram)
             #:with histogram? #t)
    (pattern (~seq)
             #:with histogram? #f)))

(define-syntax (-define stx)
  (syntax-parse stx
    [(_ (f:id x:id ...) var-decls:var-decl ... body)
     (with-syntax ([(secret-f) (generate-temporaries #'(f))]
                   [(secret-x ...) (generate-temporaries #'(x ...))])
       #'(begin
           (define secret-f
             (let ([f (λ (secret-x ...)
                        (define-no-set!-var-transformer x secret-x) ...
                        (let ()
                          var-decls ...
                          body))])
               f))
           (define-no-set!-var-transformer f secret-f)))]))

(define-syntax (define-no-set!-var-transformer stx)
  (syntax-parse stx
    [(_ x:id secret-x:id)
     #'(define-syntax x
         (make-set!-transformer
          (λ (stx)
            (syntax-parse stx #:literals (set!)
              [id:id #'secret-x]
              [(_ args (... ...))
               (with-syntax ([app (datum->syntax stx '#%app)])
                 #'(app secret-x args (... ...)))]
              [(set! . whatever) (raise-syntax-error 'x "assignment not allowed")]))))]))

(define (pick-thd-randomly thds)
  (list-ref (sort-thds thds)
            (random (length thds))))
(define (pick-first-thd thds)
  (car (sort-thds thds)))

(define-for-syntax (check/rewrite-var-seq stx)
  (for/list ([d-or-v (in-list (syntax->list stx))])
    (syntax-parse d-or-v #:literals (mut -define -let)
      [(-let mut id:id expr:expr)
       d-or-v]
      [(-let id:id expr:expr)
       d-or-v]
      [(-define . whatever)
       d-or-v])))

(define-syntax (while stx)
  (syntax-parse stx
    [(_ test:expr define-or-var ... body)
     #`(let loop ()
         (when test
           #,@(check/rewrite-var-seq #'(define-or-var ...))
           body
           (loop)))]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ lr:maybe-left-to-right
        (~optional (~or (~seq (~and #:histogram histogram))
                        (~seq (~and #:dot dot)))) ...
        define-or-var ... body)
     #`(#%module-begin
        (provide main)
        
        (define (main)

          (define-values (par/proc maybe-swap-thread/proc make-sema get-transcript)
            (start-server #,(if (syntax-e (attribute lr.left-to-right?))
                                #'pick-first-thd
                                #'pick-thd-randomly)))
          
          (values
           (syntax-parameterize ([the-par/proc #'par/proc]
                                 [the-maybe-swap-thread/proc #'maybe-swap-thread/proc]
                                 [the-make-sema #'make-sema])
                                #,@(check/rewrite-var-seq #'(define-or-var ...))
                                body)
           get-transcript))
        
        (module+ main
          (run-many-trials
           main
           #,(if (attribute histogram)
                 #'#t
                 #'#f)
           #,(if (attribute dot)
                 #'#t
                 #'#f))))]))

(define-syntax (true stx) #'#t)
(define-syntax (false stx) #'#f)
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     (exact-integer? (syntax-e #'d))
     #'(#%datum . d)]))

(define-syntax (:= stx)
  (syntax-parse stx #:literals (dot)
    [(_ id:id expr:expr)
     (syntax/loc stx (set! id expr))]
    [(_ (dot s-expr:expr id:id) v-expr:expr)
     #`(let ([s s-expr]
             [v v-expr])
         #,(syntax/loc stx (maybe-swap-thread))
         (hash-set! s 'id v))]))

(define-syntax (rec stx)
  (syntax-parse stx
    [(_ (id:id expr:expr) ...)
     #'(make-hash (list (cons 'id expr) ...))]))
(define-syntax (dot stx)
  (syntax-parse stx
    [(_ expr:expr id:id)
     #`(let ([e expr])
         #,(syntax/loc stx (maybe-swap-thread))
         (hash-ref e 'id))]))

;; get-transcript : (or/c #f (-> any/c)) -- if a thunk, then we're looking
;;                  for a counter example; otherwise show a histogram
(define (run-many-trials thunk hist? dot?)
  (cond
    [hist? (run-many-trials/hist thunk)]
    [else
     (let loop ()
       (define-values (result get-transcript) (thunk))
       (cond
         [result (loop)]
         [else
          (define a-graph (build-transcript-graph (get-transcript)))
          (cond
            [dot? (show-dot-graph a-graph)]
            [else (graph->pict a-graph)])]))]))

(define (show-dot-graph a-graph)
  (define pf (make-temporary-file "mini-thd-transcript~a.pdf"))
  (define-values (in out) (make-pipe))
  (thread
   (λ ()
     (gen-dot-code a-graph out)
     (close-output-port out)))
  (call-with-output-file pf
    (λ (port)
      (parameterize ([current-output-port port]
                     [current-input-port in])
        (system "/usr/local/bin/dot -Tpdf")))
    #:exists 'truncate)
  (parameterize ([current-input-port (open-input-string "")])
    (system (format "/usr/bin/open ~a" pf)))
  (void))
          

(define (run-many-trials/hist thunk)
  (let loop ([current-trials (hash)]
             [current-summary (hash)]
             [trials 0])
    (define-values (next get-transcript) (thunk))
    (define next-trials
      (hash-set current-trials
                next
                (+ (hash-ref current-trials next 0) 1)))
    (define next-summary (summarize next-trials))
    (cond
      [(or (and (trials . >= . 100) (equal? next-summary current-summary))
           (trials . >= . 1000))
       (printf "~a trials\n" trials)
       (for ([k (in-list (sort (hash-keys next-summary)
                               string<?
                               #:key ~s))])
         (printf "~a% ~s\n"
                 (~r #:min-width 4
                     #:precision '(= 1)
                     (* 100 (hash-ref current-summary k)))
                 k))]
      [else
       (unless (zero? trials)
         (when (zero? (modulo trials 200))
           (printf "~a trials ...\n" trials)))
       (loop next-trials next-summary (+ trials 1))])))

(define (summarize ht)
  (define denom (apply + (hash-values ht)))
  (for/hash ([(k v) (in-hash ht)])
    (values k (/ (round (* 1000 (/ v denom))) 1000))))
