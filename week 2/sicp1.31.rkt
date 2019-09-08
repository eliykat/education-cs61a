#lang simply-scheme

; SICP 1.31(a)

; Product - returns the product of the values of a function at points over a given range
(define (product f inc a b)
  (if (> a b)
  1
  (* (f a) (product f inc (inc a) b))))

;; Test case using a simple square function - should equal 14400
(product
 (lambda (x) (* x x))
 (lambda (x) (+ 1 x))
 1
 5)

;; Implement a factorial function using product
;; The factorial function simply multiplies each preceding number together - so the lambda provided to product as f does not actually need to do anything other than return its value
(define (factorial a)
  (product
   (lambda (x) x)
   (lambda (x) (+ 1 x))
   1
   a))

;; Equals 720
(factorial 6)

;; Use product to compute approximations to pi using John Wallis' formula

;; Constants
(define pi-div-4 (/ 3.14159265359 4) )
(define threshold 0.001)

;; Test for whether it is close enough
(define (good-enough? guess)
  (< (abs (- guess pi-div-4)) threshold))

; iterative version
; note that the increments to a are arbitrary but intended to be large to save processing time on smaller thresholds (> 0.0001)
(define (wallis-iter guess a)
  (if (good-enough? guess)
      guess
      (wallis-iter (* guess (product
                             wallis-pair
                             (lambda (x) (+ 2 x))
                             a
                             (+ a 1000)))
                   (+ a 1000))))

; creates 2 of the fractions required by Wallis' formula and multiplies them together
(define (wallis-pair n)
  (/ (* (- n 1) (+ n 1)) (* n n)))

; entry function with required initial variables for wallis-iter
(define (wallis)
  (wallis-iter 1 3))

; Should equal approx 0.7853981633974483096156608
(wallis)