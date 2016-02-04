#lang racket

;; -> void
(define (main)
  (define readings (get-readings (current-input-port)))
  (cond
    [(null? readings) (displayln "No measurements given.")]
    [else
     (define mn (mean readings))
     (displayln (~a "Mean rainfall: " mn  " cm" ))
     (displayln (~a "Below count : " (count mn readings #t)))
     (displayln (~a "Above count : " (count mn readings #f)))]))

;; mean : (listof nonnegative?) -> nonnegative?
(define (mean readings)
  [/ [apply + readings] [length readings]])

;; count : nonnegative (listof nonnegative?) boolean? -> natural
(define (count mean readings below?)
  (for/sum ([reading (in-list readings)]
            #:when (include? mean reading below?))
    1))

;; include? : real? real? boolean? -> boolean?
(define (include? mean reading below?)
  (and (<= (abs (- mean reading)) 5)
       (if below? (> mean reading) (< mean reading))))

(define (get-readings in)
  (for*/list ([line (in-lines (terminate-on-999 in))]
              [n (in-value (string->number line))]
              #:when (and n (>= n 0)))
    n))

;; terminate-on-999 : input-port -> input-port
;; result port is copied from input, unless input has a line of 999
;; in which case the result port closes
(define (terminate-on-999 p)
  (define-values (in out) (make-pipe))
  (thread
   (λ ()
     (let loop ()
       (define l (read-line p))
       (cond
         [(eof-object? l)
          (close-output-port out)]
         [(equal? l "999")
          (close-output-port out)]
         [else
          (displayln l out)
          (loop)]))))
  in)

(module+ test
  (require rackunit)
  (check-equal? (get-readings (open-input-string "")) '())
  (check-equal? (get-readings (open-input-string "3")) '(3))
  (check-equal? (get-readings (open-input-string "3\n4")) '(3 4))
  (check-equal? (get-readings (open-input-string "-3")) '())
  (check-equal? (get-readings (open-input-string "three")) '())
  (check-equal? (get-readings (open-input-string "3\n")) '(3))
  (check-equal? (get-readings (open-input-string "3\n999\n4")) '(3))

  (check-equal? (count 5 (build-list 10 values) #t) 5)
  (check-equal? (count 5 (build-list 10 values) #f) 4)

  (check-equal?
   (let ([sp (open-output-string)])
     (parameterize ([current-input-port (open-input-string "1\n2\n3\n4\n10\n-100")]
                    [current-output-port sp])
       (main))
     (get-output-string sp))
   "Mean rainfall: 4 cm\nBelow count : 3\nAbove count : 0\n")
  (check-equal?
   (let ([sp (open-output-string)])
     (parameterize ([current-input-port (open-input-string "")]
                    [current-output-port sp])
       (main))
     (get-output-string sp))
   "No measurements given.\n"))
