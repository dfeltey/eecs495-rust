#|

The goal is to implement a small language (to be used via #lang in DrRacket)
that helps students get their heads around the combinatorial nature of
interleaving threads.

We plan to implement the language below with special support for scheduling,
namely an exhausive search of all schedules or randomly searching for schedules
that falsify assertions that the programmer writes. Probably this can compile
into regular racket with explicitly added syncronization that allows us to
have fine-grained control over the schedule that the program see.

We also plan to investigate other basic concurrency building blocks like
test-and-set or channels and to drive the design of the language by what
problems we can ask students to solve using it.

We can use this language to investigate the implementation of concurrent data
structures. As seen in class, it is easy for subtle bugs to go undetected when
writing code that manages concurrency explicitly. The support for scheduling in
the language will allow us to state properties of concurrent data structures and
then try to generate schedules that exhibit bugs in the implementation.

Minimum:
  - an implementation of the language below with support for randomized schedules
  - an implementation of the barber problem using semaphores below
  - various broken implementations of barbers and an evaluation of how good
    random testing is at finding deadlocks and assertion failures
  - implementations of concurrent data structures such as the list_set seen in class
    utilizing different locking methods, we should be able to exhibit program
    traces which reveal bugs in the program

Bonus:
  - enumerating schedules
  - adding test-and-set (or other low-level prim)
    and implementing semaphores (or other higher-level prims) over it
  - a way to specify invariants of traces
    (which the random tester will then use to falsify)
  - a Rust-like {} parser for this language
    (along with syntax highlighting and inentation in DrRacket)
  - a GUI to visualize traces

|#

#lang racket


(require redex)
(define-language L

  ;; programs
  (p ::= (d ...))

  ;; definitions
  (d ::=
     (define (x x ...) s)
     (var x s))

  ;; statements
  (s ::=
     x            ;; variable reference
     (var x s)    ;; local variable defintiions
     integer
     (spawn s)
     (park s)
     (unpark s)
     (semaphore s)
     (P s)
     (V s)
     (seq s ...)
     (:= x s)     ;; variable assignment
     (:= (s x) s) ;; field assignment
     (x s ....)   ;; fn call
     (if0 s s s)
     (while s s)
     (+ s s)
     (- s s)
     (event x)
     (assert s)   ;; s produces a 0
     (record (x s) ...)
     (dot s x)
     (print x ...))

  (x ::= variable-not-otherwise-mentioned))

