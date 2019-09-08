#lang simply-scheme

; EXERCISE 4 - ORDERED

; Sentence of numbers => Boolean
; returns #true if numbers in sentence are ordered, #false otherwise
(define (ordered? s)
  (if (= (count s) 2)
      (< (item 1 s) (item 2 s))
      (and (ordered? (sentence (item 1 s) (item 2 s))) (ordered? (bf s)))
      ))

(ordered? '(1 2 3 4 5))
(ordered? '(1 3 4 5 2))