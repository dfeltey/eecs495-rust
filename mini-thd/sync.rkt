#lang racket/base
(require racket/match
         racket/contract
         racket/class
         (for-syntax syntax/parse
                     racket/base))
(provide
 (contract-out
  [start-server (-> any/c (values (-> srcloc? (-> any) (-> any) ... void?)
                                  (-> srcloc? void?)
                                  (class/c
                                   (init [count exact-nonnegative-integer?]
                                         [src srcloc?])
                                   [post (-> any/c void?)]
                                   [wait (-> any/c void?)])))]
  [sort-thds (-> (listof waitor?) (listof waitor?))])
 here
 sema<%>)

(define sema<%> (interface () post wait))

(define-syntax (here stx)
  #`(srcloc '#,(syntax-source stx)
            #,(syntax-line stx)
            #,(syntax-column stx)
            #,(syntax-position stx)
            #,(syntax-span stx)))

;; thread : thread?
;; id : (listof nat)
;; semaphore : semaphore? -- hit is to wake the thread
;; srcloc info: where in the original program something caused a wait
(struct waitor (thread id semaphore srcloc) #:transparent)

(define (start-server pick-one)
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
                     
                     ;; (listof waitor?)
                     'waitors '()
                     
                     ;; listof thd
                     'started-pars '()
                     
                     ;; (listof (vector semaphore? (listof thd)))
                     'joins '())])
                   
          (match-define (hash-table ('active-thread active-thread)
                                    ('waitors waitors)
                                    ('started-pars started-pars)
                                    ('joins joins)) state)
          ;(pretty-write state)
          (cond
            [(and (not active-thread)
                  (null? started-pars)
                  (not (null? waitors)))
             ;; nothing is running and we have no pending par that is starting work, so
             ;; start someone and loop; don't wait for things.
             (define a-waitor (pick-one waitors))
             (match-define (waitor thd identification semaphore srcloc) a-waitor)
             ;(printf "~a:~a:~a resuming ~a\n" source line column (eq-hash-code sema))
             (semaphore-post semaphore)
             (loop (hash-set* state
                              'active-thread thd
                              'waitors (remove a-waitor waitors)))]
            [else
             (sync
              
              (if active-thread
                  (handle-evt
                   active-thread
                   (λ (_) (loop (hash-set state 'active-thread #f))))
                  never-evt)
              
              (apply choice-evt
                     (for/list ([join (in-list joins)]
                                [i (in-naturals)])
                       (define a-waitor (vector-ref join 0))
                       (define thds (vector-ref join 1))
                       (apply choice-evt
                              (for/list ([thd (in-list thds)])
                                (handle-evt
                                 (thread-dead-evt thd)
                                 (λ (_)
                                   (cond
                                     [(null? (cdr thds))
                                      (loop
                                       (hash-set* state
                                                  'waitors (cons a-waitor waitors)
                                                  'joins (remove-ith joins i)))]
                                     [else
                                      (loop (hash-set* state
                                                       'joins
                                                       (replace-ith
                                                        joins
                                                        i
                                                        (vector waitor (remove thd thds)))))])))))))
              
              (apply choice-evt
                     (for/list ([par (in-list started-pars)])
                       (handle-evt
                        (thread-dead-evt par)
                        (λ (_) (loop (hash-set state 'started-pars (remove par started-pars)))))))
              
              (handle-evt
               maybe-swap-chan
               (λ (a-waitor)
                 (match-define (waitor thd identification semaphore srcloc) a-waitor)
                 ;(printf "~a:~a:~a blocking ~a\n" source line column (eq-hash-code semaphore))
                 (loop (hash-set* state
                                  'active-thread (if (eq? thd active-thread) #f active-thread)
                                  'waitors (cons a-waitor waitors)
                                  'started-pars (remove thd started-pars)))))
              
              (handle-evt
               started-pars-chan
               (λ (new-thds)
                 (loop (hash-set state 'started-pars (append new-thds started-pars)))))
              
              (handle-evt
               join-on-chan
               (λ (info+thds)
                 (define joining-thd (waitor-thread (vector-ref info+thds 0)))
                 (loop (hash-set* state
                                  'active-thread (if (eq? joining-thd active-thread) #f active-thread)
                                  'joins (cons info+thds joins))))))]))))))

  (define sema%
    (class* object% (sema<%>)
      (define semaphore (make-semaphore 1))
      (init-field count src)
      (define/public (wait)
        (call-with-semaphore
         semaphore
         (λ ()
           (cond
             [(zero? count) (error 'ack "zero")]
             [else (set! count (- count 1))]))))
      (define/public (post)
        (call-with-semaphore
         semaphore
         (λ ()
           (cond
             [(zero? count)
              (define resp (make-channel))
              (void)]
             [else (set! count (+ count 1))]))))
      (super-new)))

  (define identification-param (make-parameter '()))

  (define (par/proc srcloc thunk1 . thunks)
    (define new-thds+semaphores
      (for/list ([thunk (in-list thunks)]
                 [i (in-naturals 1)])
        (define semaphore (make-semaphore))
        (cons (parameterize ([identification-param (cons 1 (identification-param))])
                (thread (λ () (semaphore-wait semaphore) (thunk))))
              semaphore)))
    (define new-thds (map car new-thds+semaphores))
    (channel-put started-pars-chan new-thds)
    (for ([thd+semaphore (in-list new-thds+semaphores)])
      (semaphore-post (cdr thd+semaphore)))
    (parameterize ([identification-param (cons 0 (identification-param))])
      (thunk1))
    (define join-semaphore (make-semaphore))
    (channel-put join-on-chan
                 (vector (waitor (current-thread)
                                 ;; waiting on the 'par' join counts as 'outside' the para
                                 (identification-param)
                                 join-semaphore srcloc)
                         new-thds))
    (semaphore-wait join-semaphore))

  (define (maybe-swap-thread/proc srcloc)
    (define semaphore (make-semaphore 0))
    (channel-put maybe-swap-chan (waitor (current-thread)
                                         (identification-param)
                                         semaphore srcloc))
    (semaphore-wait semaphore))
  
  (values par/proc
          maybe-swap-thread/proc
          sema%))

(define (remove-ith lst i)
  (cond
    [(zero? i) (cdr lst)]
    [else (cons (car lst) (remove-ith (cdr lst) (- i 1)))]))

(define (replace-ith lst i new-ele)
  (cond
    [(zero? i) (cons new-ele (cdr lst))]
    [else (cons (car lst) (replace-ith (cdr lst) (- i 1) new-ele))]))


(define (sort-thds thds)
  (sort thds lon< #:key waitor-id))
(define (lon< lon1-orig lon2-orig)
  (let loop ([lon1 (reverse lon1-orig)]
             [lon2 (reverse lon2-orig)])
    (cond
      [(and (null? lon2) (null? lon1))
       (error 'lon-compare "found two equal lons! ~s" lon1-orig)]
      [(null? lon1) #t]
      [(null? lon2) #f]
      [else
       (define n1 (car lon1))
       (define n2 (car lon2))
       (cond
         [(= n1 n2) (loop (cdr lon1) (cdr lon2))]
         [else (< n1 n2)])])))

(module+ test
  (require rackunit)

  (define (pick-smallest thds)
    (car (sort-thds thds)))
    
  (check-true  (lon< '(0) '(1)))
  (check-false (lon< '(1) '(0)))
  (check-false (lon< '(0 1) '(0)))
  (check-true  (lon< '(0) '(0 1)))
  (check-true  (lon< '() '(0)))

  (define-values (par/proc maybe-swap-thread/proc sema%) (start-server pick-smallest))
  (define (mst) (maybe-swap-thread/proc (here)))
  (for ([x (in-range 1000)])
    (check-equal?
     (let ([x 0])
       (par/proc
        (here)
        (λ () (mst) (set! x 1))
        (λ () (mst) (set! x 2)))
       x)
     2))

  (check-equal?
   (let ([x 0])
     (par/proc
      (here)
      (λ () (mst) (set! x 2))
      (λ () (mst) (set! x 1)))
     x)
   1)
  
  (check-equal?
   (let ([x '()])
     (par/proc
      (here)
      (λ () (mst) (let ([y (cons 1 x)]) (mst) (set! x y)))
      (λ () (mst) (let ([y (cons 2 x)]) (mst) (set! x y))))
     x)
   '(2 1))
  )

