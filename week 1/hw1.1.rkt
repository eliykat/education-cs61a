#lang sicp

; Guess is a Number representing an incremental guess as per the Newtonian Method

; Guess => Guess
; Tests whether Guess is (good-enough?), and either returns the Guess (base case) or iterates recursively
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x )))


; Guess => Guess
; Improves a guess as per the Newtonian Method and returns the improved Guess
(define (improve guess x)
  (average guess (/ x guess)))

; Number => Number
; averages two numbers
(define (average x y)
  (/ (+ x y) 2))

; Guess => Boolean
; Tests whether a Guess is within a given margin of error of X squared
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; Number => Guess
; Finds the square root of a number as per Newtonian Method
(define (sqrt x)
  (sqrt-iter 1.0 x))

; Number => Number
; Squares a number
(define (square x)
  (* x x))

; Boolean => value
; Alternative if clause as per exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(sqrt 9)