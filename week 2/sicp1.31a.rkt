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