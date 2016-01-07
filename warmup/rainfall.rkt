#lang racket

(define (main)
  (define readings (get-readings (current-input-port)))
  (define mn (mean readings))
  (displayln (~a "Mean rainfall: " mn  " cm" ))
  (displayln (~a "Below count : " (below-count mn readings)))
  (displayln (~a "Above count : " (above-count mn readings))))

;; mean : (listof nonnegative?) -> nonnegative?
(define (mean readings)
  [/ [apply + readings] [length readings]])

;; below-count : nonnegative (listof nonnegative?) -> natural
(define (below-count mean readings)
  (length (filter (λ (n) (and (> mean n) (>= n (- mean 5)))) readings)))

;; above-count : nonnegative (listof nonnegative?) -> natural
(define (above-count mean readings)
  (length (filter (λ (n) (and (>= (+ mean 5) n) (> n mean))) readings)))

(define (get-readings in)
  (for*/list ([line (in-lines (terminate-on-999 in))]
              [n (in-value (string->number line))]
              #:when (and n (>= n 0)))
    n))
    
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
  )
  