#|

The goal is to implement a small language (to be used via #lang in DrRacket)
that helps students get their heads around the combinatorial nature of
interleaving threads.

We plan to implement the language below with special support for scheduling,
namely an exhausive search of all schedules or randomly search for schedules
that falsify assertions that the programmer writes. Probably this can compile
into regular racket with explicitly added syncronization that allows us to
have fine-grained control over the schedule that the program see.

We also plan to investigate other basic concurrency building blocks like
test-and-set or channels and to drive the design of the language by what
problems we can ask students to solve using it.

Minimum:
  - an implementation of the language below with support for randomized schedules
  - an implementation of the barber problem using semaphores below
  - various broken implementations of barbers and an evaluation of how good
    random testing is at finding deadlocks and assertion failures

Bonus:
  - enumerating schedules
  - adding test-and-set (or other low-level prim)
    and implementing semaphores (or other higher-level prims) over it
  - a way to specify invariants of traces
    (which the random tester will then use to falsify)

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
     (event x)
     (assert s))) ;; s produces a 0

