#lang racket/base
(provide (struct-out transcript-element)
         (struct-out t-choice)
         (struct-out t-par))
(struct transcript-element (identification srcloc) #:transparent)
(struct t-choice transcript-element () #:transparent)
(struct t-par transcript-element (size) #:transparent)
