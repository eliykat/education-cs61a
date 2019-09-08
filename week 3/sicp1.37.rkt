#lang simply-scheme
(require trace)

; SICP EXERCISE 1.37
; Infinite continued fraction
; should produce 1/golden ratio

; NB: here, the recursive function needs a separate 'counter' variable (c) because it needs to start at 1, and have the base case equal to k.
; by contrast, the loop starts at the base case, so it starts at k and counts down to 0 with each iteration.

; Recursive implementation
(define (cont-frac n d k)
  (define (loop n d k c)
    (if (= k c)
        (/ (n c) (d c))
        (/ (n c) (+ (d c) (loop n d k (+ c 1))))))
  (loop n d k 1))

'cont-frac:
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

; Iterative implementation
'iter-cont-frac:
(define (iter-cont-frac n d k)
  (define (loop n d k acc)
    (if (= k 0)
        acc
        (loop n d (- k 1) (/ (n k) (+ (d k) acc)))))
  (loop n d k 0))

(iter-cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                100)