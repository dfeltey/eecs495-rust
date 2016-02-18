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
  - an implementation of the various list-as-set ADTs as described in class
    (buggy and not) in the language below
  - a simple way to specify properties of programs that's good enough to
    capture what's wrong with the buggy code in class
  - an evaluation of whether or not random testing reveals the bugs from class

Bonus:
  - enumerating schedules
  - a Rust-like {} parser for this language
    (along with syntax highlighting and inentation in DrRacket)
    --=> this will probably entail some changes to the syntax
         as below too, but the set of programs will remain about the same
  - a GUI to visualize traces

|#

#lang racket


(require redex)
(define-language L

  ;; programs
  (p ::= (d ...))

  ;; definitions
  (d ::=
     (define (x x ...) (var x s) ... s)
     (var x s))

  ;; statements
  (s ::=
     x            ;; variable reference
     integer
     (par s s)
     (semaphore s)
     (P s)
     (V s)
     (seq s ...)
     (:= x s)     ;; variable assignment
     (:= (s x) s) ;; field assignment
     (x s ....)   ;; fn call
     (if s s s)
     (while s s)
     (+ s s)
     (- s s)
     (< s s)
     (= s s)
     true false
     (record (x s) ...)
     (dot s x)
     (print x ...))

  (x ::= variable-not-otherwise-mentioned))

