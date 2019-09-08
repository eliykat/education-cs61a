#lang simply-scheme

; EXERCISE 2 - SQUARES

; Number => Number
; Squares a number
(define (square x)
  (* x x))

; Sentence => Sentence
; Computes a sentence of numbers into a sentence of those numbers^2
; given: (2 3 4 5) expect: (4 9 16 25)
(define (squares s)
  (if (= (count s) 1)
      (square (first s))
      (sentence (squares (first s)) (squares (butfirst s)))
      )
  )

(squares '(2 3 4 5))