#lang simply-scheme

; LAB EXERCISE 7 - DUPLS-REMOVED

; Sentence -> sentence
; Takes a sentence as input and returns the result of removing duplicate words
; Specification requires that it keep the right-most occurrence of any word
; given: '(a b a b a c) expect: '(b a c)
(define (dupls-removed s)
  (if (> (count s) 1)
      (if (member? (first s) (bf s))
          (dupls-removed (bf s))
          (se (first s) (dupls-removed (bf s))))
      s))

(dupls-removed '(a b a b a c))