#lang simply-scheme
(require trace)

; SICP EXERCISE 1.38
; approximate the mathematical constant e - 2 using cont-frac

; Recursive implementation from SICP 1.37
(define (cont-frac n d k)
  (define (loop n d k c)
    (if (= k c)
        (/ (n c) (d c))
        (/ (n c) (+ (d c) (loop n d k (+ c 1))))))
  (loop n d k 1))

; This function is from the scheme wiki: http://community.schemewiki.org/?sicp-ex-1.38
; I couldn't figure out how to get a function to create this pattern of values
(define (e-seq i)
  (if (= (remainder i 3) 2) 
      (/ (+ i 1) 1.5) 
      1))

'(approximate constant (e - 2) using cont-frac - should equal approx 0.71828)

(cont-frac (lambda (i) 1.0)
           e-seq
           100)