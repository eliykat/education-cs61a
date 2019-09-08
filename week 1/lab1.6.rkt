#lang simply-scheme

; LAB EXERCISE 6 - SUM OF SQUARES
; Note: there is certainly a better way of doing this, and this implementation doesn't handle the situation in which 2 numbers in the sentence are equal.

(define (square x) (* x x))

; Sentence of numbers -> number
; Takes 2 numbers in a sentence and returns the sum of their squares
; Given: '(2 3), expect: 13
(define (sum-of-squares s)
  (+ (square (item 1 s)) (square (item 2 s)))
  )

; Sentence of numbers -> number
; Returns the biggest number in a sentence
(define (biggest s)
  (if (> (count s) 2)
       (biggest (se (first s) (biggest (bf s))))
       (if (> (item 1 s) (item 2 s))
           (item 1 s)
           (item 2 s))
       ))

; Sentence of numbers -> number
; Returns the smallest number in a sentence
(define (smallest s)
  (if (> (count s) 2)
       (smallest (se (first s) (smallest (bf s))))
       (if (< (item 1 s) (item 2 s))
           (item 1 s)
           (item 2 s))
       ))

; Sentence of numbers -> number
; Returns the middle number in a sentence of 3 numbers, i.e. the number that is neither the biggest nor smallest
(define (middle s)
  (cond ((and (not (= (item 1 s) (biggest s))) (not (= (item 1 s) (smallest s)))) (item 1 s))
        ((and (not (= (item 2 s) (biggest s))) (not (= (item 2 s) (smallest s)))) (item 2 s))
        ((and (not (= (item 3 s) (biggest s))) (not (= (item 3 s) (smallest s)))) (item 3 s))
        ))


; Sentence of numbers -> number
; Takes 3 numbers in a sentence and returns the sum of the squares of the largest 2
(define (sos-full s)
  (sum-of-squares (se (biggest s) (middle s)))
  )