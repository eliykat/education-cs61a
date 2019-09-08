#lang simply-scheme

; SICP 1.42
; Composition of functions
(define (compose f g)
  (lambda (x) (f(g x))))