#lang simply-scheme

; SICP 1.41
; double
; takes procedure f(x) and returns a procedure f(f(x))
(define (double f)
  (lambda (x) (f(f x))))

(define (inc x) (+ 1 x))
(define (d-inc x) ((double inc) x))
(d-inc 10)