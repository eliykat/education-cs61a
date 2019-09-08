#lang simply-scheme

; EXERCISE 6 - SPECIAL-TEST

; Test if 'and' is a special form
(not (and #f #t (infinity)))

; Test if 'or' is a special form
(or #t (infinity))

; Infinite loop - designed to crash the system if it is evaluated - which is our feedback mechanism to tell us what *hasn't* been evaluated
(define (infinity)
  (infinity))