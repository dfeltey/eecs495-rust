#|

The goal is to implement a small language (to be used via #lang in DrRacket)
that helps students get their heads around the combinatorial nature of
interleaving threads.

We plan to implement the language below with special support for scheduling,
namely an exhausive search of all schedules or randomly search for schedules
that falsify assertions that the programmer writes. Probably this can compile
into regular racket with explicitly added syncronization that allows us to
have fine-grained control over the schedule that the program see.

The language we have in mind is below and we don't know exactly what the
language for assertions is yet, but we want it to include at least enough
to specify the correctness property for a semaphore-based implementation of
mutexes, and possibly other things.

We also plan to investigate other basic concurrency building blocks like
test-and-set or channels and to drive the design of the language by what
problems we can ask students to solve using it. For that, some advice
on concurrent primitives and their implementation would be useful.....

|#

#lang racket


(require redex)
(define-language L

  ;; programs
  (p ::= (d ... a ...))

  ;; definitions
  (d ::=
     (define (x x ...) s)
     (var x s))

  ;; statments
  (s ::=
     (spawn s)
     (park s)
     (unpark s)
     (semaphore)
     (P s)
     (V s)
     (seq s ...)
     (:= x s)
     (x s ....) ;; fn call
     (if0 s s s)
     (+ s s)
     (event x))

  ;; assertions ... not sure about these.
  (a ::=
     x         ;; event happens
     (or a a)  ;; choice
     (seq a a) ;; both
     (* a)))   ;; repeat
