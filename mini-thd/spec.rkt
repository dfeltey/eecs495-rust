#|

rfindler, dfeltey@github

The goal is to implement a small language (to be used via #lang in DrRacket)
that helps students get their heads around the combinatorial nature of
interleaving threads.

We plan to implement the language below with special support for scheduling,
namely an exhausive search of all schedules or randomly searching for schedules
that falsify assertions that the programmer writes.

The language is a first-order, untyped, parallel functional language
with integers, booleans, semaphores, and mutable records (of any of those
or of records) as values.

Minimum:
  - an implementation of the language below with support for randomized schedules
  - implementations of the list-as-set ADT as described in class
    (buggy and not) in the language below
  - a simple way to specify properties of programs that's good enough to
    capture what's wrong with the buggy last-as-set ADT implementations from class
  - an evaluation of whether or not random testing reveals the bugs from class

Bonus:
  - enumerating schedules
  - a Rust-like {} parser for this language
    (along with syntax highlighting and indentation in DrRacket)
  - a GUI to visualize traces, with support to make choices and
    to "go back" (ie re-run up to the given point) and change
    the scheduling decisions at each point

|#

#lang racket

(require redex)
(define-language L

  ;; programs
  (p ::= (d ... s))
  ;; definitions plus an expression that should
  ;; always evaluate to true (but might not if there
  ;; is a bug)

  ;; definitions
  (d ::=
     (define (x x ...) (var x s) ... s) ;; define a function w/local vars
     (var x s)) ;; global variable

  ;; statement/expressions
  (s ::=
     (par s s ...) ;; run args in parallel, complete when all do
     (semaphore s) ;; create a semaphore with initial value (that must be a nat)
     (post s)      ;; increment the count in a semaphore
     (wait s)      ;; decrement it
     x             ;; variable reference
     integer       ;; integer constant
     (seq s s ...) ;; sequential composition
     (:= x s)      ;; variable assignment
     (:= (s x) s)  ;; field assignment
     (x s ....)    ;; fn call
     (if s s s)    ;; conditional
     (while s (var s s) ... s) ;; loop
     (+ s s)
     (- s s)
     (< s s)
     (= s s)
     (or s s)        ;; shortcircuiting
     (and s s)       ;; shortcircuiting
     true false      ;; boolean constants
     (rec (x s) ...) ;; construct a record with fields named by 'x's
     (dot s x)       ;; extract the 'x' field from the record 's'
     (print x ...))  ;; print the name and value of the given variables

  (x ::= variable-not-otherwise-mentioned))

