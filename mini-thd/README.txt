Intermediate Handin

So far, we've implemented the language. To try it out, look at the
files ex.rkt and ex2.rkt. Run them at the command-line like this:

  $ racket ex.rkt
  493 trials
  23.9% 1
  22.3% 2
  53.8% 3
  cpu time: 87 real time: 88 gc time: 30

  $ racket ex2.rkt
  267 trials
  3% #hash((hd . 3) (tl . #f))
  21.3% #hash((hd . 3) (tl . #hash((hd . 0) (tl . #f))))
  75.7% #hash((hd . 3) (tl . #hash((hd . 1) (tl . #f))))
  cpu time: 91 real time: 92 gc time: 35
  
The output shows the results of running the program some number of
times (minimum 200) and collecting the percentage of times the various
results show up. So, in the first case, we get about 1/4 of the time
the answer 1 or 2 and about 1/2 of the time the answer 3. (It runs the
program until the percentages stop changing in the third digit.)

The bulk of the implementation is in the file sync.rkt. It has a
standard CML-style loop that accepts 'state' in the thread created in
the body. This thread handles various messages that indicate when
operations that require synchronization have happened, e.g., when a
reference to a variable, a mutation to a field of an object, or a wait
or post to a semaphore whose internal count is 0. For most of those
events, the thread that was about to take the event is put into the
'waitors' list and then one of the threads from that list is chosen at
random to resume the computation.

The file lang.rkt sets up the syntactic forms of the language. Mostly
it re-exports small changes to Racket constructs. The 'var' form is
probably the most interesting. It turns all references and assignments
to 'var'-bound identifiers into calls into sync.rkt's loop via the
function maybe-swap-thread.

There are tests in the bottom of sync.rkt that check to make sure the
threading loops works as expected and the file lang-test.rkt checks
the various primitive functions in the language.
   
