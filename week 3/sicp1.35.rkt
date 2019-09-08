#lang simply-scheme
(require trace)

; SICP EXERCISE 1.35
; Use the fixed-point function to show that the golden ratio is the fixed point of lambda x -> 1 + 1/x

; This uses the fixed-point function from p 92
(define tolerance 0.0000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (gr x)
  (+ 1 (/ 1 x)))

(fixed-point gr 1)