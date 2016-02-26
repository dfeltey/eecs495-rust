#lang racket/base
(require racket/match
         (for-syntax syntax/parse
                     racket/base))
(provide start-server
         sort-thds)

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
                     
                     ;; (listof (vector/c thread? (listof nat) sema <srcloc info>))
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
             (define thd+identification+sema+srcloc (pick-one waiters))
             (match-define (vector thd identification sema source line column)
               thd+identification+sema+srcloc)
             ;(printf "~a:~a:~a resuming ~a\n" source line column (eq-hash-code sema))
             (semaphore-post sema)
             (loop (hash-set* state
                              'active-thread thd
                              'waiters (remove thd+identification+sema+srcloc waiters)))]
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
                       (define waiter (vector-ref join 0))
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
                       (handle-evt
                        (thread-dead-evt par)
                        (λ (_) (loop (hash-set state 'started-pars (remove par started-pars)))))))
              
              (handle-evt
               maybe-swap-chan
               (λ (thd+identification+sema+srcloc)
                 (match-define (vector thd identification sema source line column)
                   thd+identification+sema+srcloc)
                 ;(printf "~a:~a:~a blocking ~a\n" source line column (eq-hash-code sema))
                 (loop (hash-set* state
                                  'active-thread (if (eq? thd active-thread) #f active-thread)
                                  'waiters (cons thd+identification+sema+srcloc waiters)
                                  'started-pars (remove thd started-pars)))))
              
              (handle-evt
               started-pars-chan
               (λ (new-thds)
                 (loop (hash-set state 'started-pars (append new-thds started-pars)))))
              
              (handle-evt
               join-on-chan
               (λ (info+thds)
                 (define joining-thd (vector-ref (vector-ref info+thds 0) 0))
                 (loop (hash-set* state
                                  'active-thread (if (eq? joining-thd active-thread) #f active-thread)
                                  'joins (cons info+thds joins))))))]))))))

  (define identification-param (make-parameter '()))

  (define (par/proc source line column thunk1 . thunks)
    (define new-thds+semas
      (for/list ([thunk (in-list thunks)]
                 [i (in-naturals 1)])
        (define sema (make-semaphore))
        (cons (parameterize ([identification-param (cons 1 (identification-param))])
                (thread (λ () (semaphore-wait sema) (thunk))))
              sema)))
    (define new-thds (map car new-thds+semas))
    (channel-put started-pars-chan new-thds)
    (for ([thd+sema (in-list new-thds+semas)])
      (semaphore-post (cdr thd+sema)))
    (parameterize ([identification-param (cons 0 (identification-param))])
      (thunk1))
    (define join-sema (make-semaphore))
    (channel-put join-on-chan
                 (vector (vector (current-thread)
                                 ;; waiting on the 'par' join counts as 'outside' the para
                                 (identification-param)
                                 join-sema source line column)
                         new-thds))
    (semaphore-wait join-sema))

  (define (maybe-swap-thread/proc source line column)
    (define sema (make-semaphore 0))
    (channel-put maybe-swap-chan (vector (current-thread)
                                         (identification-param)
                                         sema source line column))
    (semaphore-wait sema))
  
  (values par/proc
          maybe-swap-thread/proc))

(define (remove-ith lst i)
  (cond
    [(zero? i) (cdr lst)]
    [else (cons (car lst) (remove-ith (cdr lst) (- i 1)))]))

(define (replace-ith lst i new-ele)
  (cond
    [(zero? i) (cons new-ele (cdr lst))]
    [else (cons (car lst) (replace-ith (cdr lst) (- i 1) new-ele))]))


(define (sort-thds thds)
  (sort thds lon<
        #:key (λ (x) (vector-ref x 1))))
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
         
  (define-values (par/proc maybe-swap-thread/proc) (start-server pick-smallest))
  (define (mst) (maybe-swap-thread/proc #f #f #f))
  (for ([x (in-range 1000)])
    (check-equal?
     (let ([x 0])
       (par/proc
        #f #f #f
        (λ () (mst) (set! x 1))
        (λ () (mst) (set! x 2)))
       x)
     2))

  (check-equal?
   (let ([x 0])
     (par/proc
      #f #f #f
      (λ () (mst) (set! x 2))
      (λ () (mst) (set! x 1)))
     x)
   1)
  
  (check-equal?
   (let ([x '()])
     (par/proc
      #f #f #f
      (λ () (mst) (let ([y (cons 1 x)]) (mst) (set! x y)))
      (λ () (mst) (let ([y (cons 2 x)]) (mst) (set! x y))))
     x)
   '(2 1))
  )

