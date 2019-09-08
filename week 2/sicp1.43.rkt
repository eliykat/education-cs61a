#lang simply-scheme

(define (square x) (* x x))

; SICP 1.43
; Repeated - returns a function that repeats f n times
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)