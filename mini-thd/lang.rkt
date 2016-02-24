#lang racket/base
(require (for-syntax syntax/parse
                     racket/base)
         racket/format
         racket/match
         racket/pretty)

(provide true false var
         #%app := par +
         (rename-out [-define define]
                     [datum #%datum]
                     [module-begin #%module-begin]
                     [begin seq]))

(define-for-syntax (maybe-swap-thread stx)
  #`(maybe-swap-thread/proc '#,(syntax-source stx)
                            #,(syntax-line stx)
                            #,(syntax-column stx)))

(define (maybe-swap-thread/proc source line column)
  (define sema (make-semaphore 0))
  (channel-put maybe-swap-chan (vector (current-thread) sema source line column))
  (semaphore-wait sema))

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
                       #,(maybe-swap-thread stx)
                       (set! secret-id nv))]
                  [x
                   (identifier? #'x)
                   #`(begin
                       #,(maybe-swap-thread stx)
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
     (syntax/loc stx (set! id expr))]))

(define-syntax (par stx)
  (syntax-parse stx
    [(_ e:expr ...+)
     #`(do-par '#,(syntax-source stx)
               #,(syntax-line stx)
               #,(syntax-column stx)
               (λ () e) ...)]))

(define (do-par source line column thunk1 . thunks)
  (define new-thds+semas
    (for/list ([thunk (in-list thunks)])
      (define sema (make-semaphore))
      (cons (thread (λ () (semaphore-wait sema) (thunk)))
            sema)))
  (define new-thds (map car new-thds+semas))
  (channel-put started-pars-chan new-thds)
  (for ([thd+sema (in-list new-thds+semas)])
    (semaphore-post (cdr thd+sema)))
  (thunk1)
  (define join-sema (make-semaphore))
  (channel-put join-on-chan
               (vector (vector (current-thread) join-sema source line column)
                       new-thds))
  (semaphore-wait join-sema))

(define join-on-chan (make-channel))
(define maybe-swap-chan (make-channel))
(define started-pars-chan (make-channel))

(void
 (let ([main-thd (current-thread)])
   (thread
    (λ ()
      (let loop ([state
                  (hash ;; (or/c #f thread?)
                        'active-thread main-thd

                        ;; (listof (vector/c thread? sema <srcloc info>))
                        'waiters '()

                        ;; listof thd
                        'started-pars '()

                        ;; (listof (vector sema (listof thd)))
                        'joins '())])
        (match-define (hash-table ('active-thread active-thread)
                                  ('waiters waiters)
                                  ('started-pars started-pars)
                                  ('joins joins)) state)
        ;(pretty-write state)
        (cond
          [(and (not active-thread)
                (null? started-pars)
                (not (null? waiters)))
           ;; nothing is running and we have no pending par that is starting work, so
           ;; start someone and loop; don't wait for things.
           (define thd+sema+srcloc (pick-one waiters))
           (match-define (vector thd sema source line column) thd+sema+srcloc)
           ;(printf "~a:~a:~a resuming ~a\n" source line column (eq-hash-code sema))
           (semaphore-post sema)
           (loop (hash-set* state
                            'active-thread thd
                            'waiters (remove thd+sema+srcloc waiters)))]
          [else
           (sync

            (if active-thread
                (wrap-evt
                 active-thread
                 (λ (_) (loop (hash-set state 'active-thread #f))))
                never-evt)
            
            (apply choice-evt
                   (for/list ([join (in-list joins)]
                              [i (in-naturals)])
                     (define waiter (vector-ref join 0))
                     (define thds (vector-ref join 1))
                     (apply choice-evt
                            (for/list ([thd (in-list thds)])
                              (wrap-evt
                               (thread-dead-evt thd)
                               (λ (_)
                                 (cond
                                   [(null? (cdr thds))
                                    (loop
                                     (hash-set* state
                                                'waiters (cons waiter waiters)
                                                'joins (remove-ith joins i)))]
                                   [else
                                    (loop (hash-set* state
                                                     'joins
                                                     (replace-ith
                                                      joins
                                                      i
                                                      (vector waiter (remove thd thds)))))])))))))
            
            (apply choice-evt
                   (for/list ([par (in-list started-pars)])
                     (wrap-evt
                      (thread-dead-evt par)
                      (λ (_) (loop (hash-set state 'started-pars (remove par started-pars)))))))
            
            (wrap-evt
             maybe-swap-chan
             (λ (thd+sema+srcloc)
               (match-define (vector thd sema source line column) thd+sema+srcloc)
               ;(printf "~a:~a:~a blocking ~a\n" source line column (eq-hash-code sema))
               (loop (hash-set* state
                                'active-thread (if (eq? thd active-thread) #f active-thread)
                                'waiters (cons thd+sema+srcloc waiters)
                                'started-pars (remove thd started-pars)))))
            
            (wrap-evt
             started-pars-chan
             (λ (new-thds)
               (loop (hash-set state 'started-pars (append new-thds started-pars)))))

            (wrap-evt
             join-on-chan
             (λ (info+thds)
               (define joining-thd (vector-ref (vector-ref info+thds 0) 0))
               (loop (hash-set* state
                                'active-thread (if (eq? joining-thd active-thread) #f active-thread)
                                'joins (cons info+thds joins))))))]))))))
            
(define (remove-ith lst i)
  (cond
    [(zero? i) (cdr lst)]
    [else (cons (car lst) (remove-ith (cdr lst) (- i 1)))]))

(define (replace-ith lst i new-ele)
  (cond
    [(zero? i) (cons new-ele (cdr lst))]
    [else (cons (car lst) (replace-ith (cdr lst) (- i 1) new-ele))]))  
  
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
