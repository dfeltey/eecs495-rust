#lang racket/base
(require racket/match
         racket/contract
         racket/class
         racket/pretty
         "visualize-struct.rkt"
         (for-syntax syntax/parse
                     racket/base))
(provide
 (contract-out
  [start-server (-> (-> (listof waitor?) waitor?)
                    (values (-> srcloc? (-> any) (-> any) ... void?)
                            (-> srcloc? void?)
                            (class/c
                             (init [count exact-nonnegative-integer?]
                                   [src srcloc?])
                             [post (-> any/c void?)]
                             [wait (-> any/c srcloc? void?)])
                            (-> (listof any/c))))]
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
;; id : (listof (list/c nat nat))
;; semaphore : semaphore? -- hit is to wake the thread
;; srcloc info: where in the original program something caused a wait
(struct waitor (thread id semaphore srcloc) #:transparent)

(define (start-server pick-one [pick-a-sema-waitor pick-one])
  (define join-on-chan (make-channel))
  (define maybe-swap-chan (make-channel))
  (define started-pars-chan (make-channel))
  (define posted-a-zero (make-channel))
  (define waiting-on-sema (make-channel))
  (define transcript-chan (make-channel))
  
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
                    'joins '()
                    
                    ;; (listof (vector sema% waitor?))
                    'sema-waitors '()

                    ;; (listof ??)
                    'transcript '()
                    )])
         
         (match-define (hash-table ('active-thread active-thread)
                                   ('waitors waitors)
                                   ('started-pars started-pars)
                                   ('joins joins)
                                   ('sema-waitors sema+waitors)
                                   ('transcript transcript)) state)
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
                             'transcript (cons (t-choice identification srcloc)
                                               transcript)
                             'active-thread thd
                             'waitors (remove a-waitor waitors)))]
           [else
            (sync

             (handle-evt transcript-chan (λ (resp) (channel-put resp transcript) (loop state)))
             
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
                                                       (vector a-waitor (remove thd thds)))))])))))))
             
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
              (λ (identification+srcloc+new-thds)
                (match-define (vector identification srcloc new-thds) identification+srcloc+new-thds)
                (loop (hash-set* state
                                 'transcript (cons (t-par identification srcloc
                                                          (+ 1 (length new-thds)))
                                                   transcript)
                                 'started-pars (append new-thds started-pars)))))
             
             (handle-evt
              join-on-chan
              (λ (info+thds)
                (define joining-thd (waitor-thread (vector-ref info+thds 0)))
                (loop (hash-set* state
                                 'active-thread (if (eq? joining-thd active-thread) #f active-thread)
                                 'joins (cons info+thds joins)))))
             
             (handle-evt
              posted-a-zero
              (λ (sema+resp)
                (define these-sema+waitors
                  (for/list ([sema+waitor (in-list sema+waitors)]
                             #:when (equal? (vector-ref sema+waitor 0)
                                            (vector-ref sema+resp 0)))
                    sema+waitor))
                (cond
                  [(null? these-sema+waitors)
                   (channel-put (vector-ref sema+resp 1) #t)
                   (loop state)]
                  [else
                   (channel-put (vector-ref sema+resp 1) #f)
                   (define picked-waitor
                     (pick-a-sema-waitor
                      (map (λ (x) (vector-ref x 1)) these-sema+waitors)))
                   (define picked-sema-waitor
                     (for/or ([ele (in-list these-sema+waitors)])
                       (and (equal? picked-waitor (vector-ref ele 1))
                            ele)))
                   (unless picked-sema-waitor (error 'ack))
                   (loop (hash-set* state
                                    'sema-waitors (remove picked-sema-waitor sema+waitors)
                                    'waitors (cons picked-waitor waitors)))])))
             
             (handle-evt
              waiting-on-sema
              (λ (sema+waitor)
                (match-define (waitor thd identification semaphore srcloc)
                  (vector-ref sema+waitor 1))
                (loop (hash-set* state
                                 'active-thread (if (eq? thd active-thread) #f active-thread)
                                 'started-pars (remove thd started-pars)
                                 'sema-waitors (cons sema+waitor sema+waitors))))))])))))

  (define sema%
    (class* object% (sema<%>)
      (define semaphore (make-semaphore 1))
      (init-field count src)
      (define/public (wait srcloc)
        (define waitor-semaphore #f)
        (call-with-semaphore
         semaphore
         (λ ()
           (cond
             [(zero? count)
              (set! waitor-semaphore (make-semaphore))
              (channel-put waiting-on-sema
                           (vector
                            this
                            (waitor (current-thread)
                                    (identification-param)
                                    waitor-semaphore srcloc)))]
             [else (set! count (- count 1))])))
        (when waitor-semaphore (semaphore-wait waitor-semaphore)))
      (define/public (post)
        (call-with-semaphore
         semaphore
         (λ ()
           (cond
             [(zero? count)
              (define resp (make-channel))
              (channel-put posted-a-zero (vector this resp))
              (define inc? (channel-get resp))
              (when inc? (set! count (+ count 1)))]
             [else (set! count (+ count 1))]))))
      (super-new)))

  (define identification-param (make-parameter '()))

  (define (par/proc srcloc thunk1 . thunks)
    (define new-thds+semaphores
      (for/list ([thunk (in-list thunks)]
                 [i (in-naturals 1)])
        (define semaphore (make-semaphore))
        (cons (parameterize ([identification-param (cons i (identification-param))])
                (thread (λ () (semaphore-wait semaphore) (thunk))))
              semaphore)))
    (define new-thds (map car new-thds+semaphores))
    (channel-put started-pars-chan (vector (identification-param) srcloc new-thds))
    (for ([thd+semaphore (in-list new-thds+semaphores)])
      (semaphore-post (cdr thd+semaphore)))
    (parameterize ([identification-param (cons 0 (identification-param))])
      (thunk1))
    (define join-semaphore (make-semaphore))
    (channel-put join-on-chan
                 (vector (waitor (current-thread)
                                 ;; waiting on the 'par' join counts as 'outside' the para
                                 (vector (identification-param) 'join)
                                 join-semaphore srcloc)
                         new-thds))
    (semaphore-wait join-semaphore))

  (define (maybe-swap-thread/proc srcloc)
    (define semaphore (make-semaphore 0))
    (channel-put maybe-swap-chan (waitor (current-thread)
                                         (identification-param)
                                         semaphore srcloc))
    (semaphore-wait semaphore))

  (define (get-transcript)
    (define chan (make-channel))
    (channel-put transcript-chan chan)
    (reverse (channel-get chan)))
  
  (values par/proc
          maybe-swap-thread/proc
          sema%
          get-transcript))

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
       (error 'lon-compare "found two equal lons! ~s ~s" lon1-orig lon2-orig)]
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

  (define-values (par/proc maybe-swap-thread/proc sema% get-transcript)
    (start-server pick-smallest))
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
      (λ () 1)
      (λ () 2))
     x)
   0)

  (check-equal?
   (let ([x 0])
     (par/proc
      (here)
      (λ () (mst) (set! x 2))
      (λ () (mst) (set! x 1)))
     x)
   1)

  (check-equal?
   (let ([x 0])
     (par/proc
      (here)
      (λ () (mst) (set! x 5))
      (λ () (mst) (set! x 4))
      (λ () (mst) (set! x 3))
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

  (check-equal?
   (let ([x '()]
         [s (new sema% [count 1] [src (here)])])
     (par/proc
      (here)
      (λ () (send s wait (here)) (set! x (cons 1 x)) (send s post))
      (λ () (send s wait (here)) (set! x (cons 2 x)) (send s post)))
     x)
   '(2 1))

  (check-equal?
   (let ([x '()]
         [s (new sema% [count 0] [src (here)])])
     (par/proc
      (here)
      (λ () (send s wait (here)) (set! x (cons 1 x)))
      (λ () (set! x (cons 2 x)) (send s post)))
     x)
   '(1 2))

  (check-equal?
   (let ([x '()]
         [s (new sema% [count 0] [src (here)])])
     (par/proc
      (here)
      (λ () (set! x (cons 2 x)) (send s post))
      (λ () (send s wait (here)) (set! x (cons 1 x))))
     x)
   '(1 2))

  (check-equal?
   (let ([x '()]
         [s (new sema% [count 0] [src (here)])])
     (par/proc
      (here)
      (λ () (send s wait (here)) (set! x (cons 2 x)) (send s post))
      (λ () (send s wait (here)) (set! x (cons 3 x)) (send s post))
      (λ () (send s wait (here)) (set! x (cons 4 x)) (send s post))
      (λ () (set! x (cons 1 x)) (send s post)))
     x)
   '(4 3 2 1))

  )
