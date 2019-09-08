#lang simply-scheme
(require trace)

; EXERCISE 2
; Write a procedure (next-perf n) which increments from n until it finds a perfect number
; A perfect number is a number equal to the sum of its factors.

(define (next-perf n)

  ; sum-of-factors subprocedure
  ; sums all factors of n
  (define (sum-of-factors n d)
    (if (= n d)
        0
        (+ (factor n d) (sum-of-factors n (+ d 1)))))

  ; if d is a factor of n, return d
  ; else return 0
  (define (factor n d)
    (if (= (remainder n d) 0)
        d
        0))
      
  (if (= (sum-of-factors n 1) n)
      n
      (next-perf (+ n 1))))