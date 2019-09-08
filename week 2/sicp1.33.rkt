#lang simply-scheme

; SICP exercise 1.33
; filtered-accumulate
; Accumulate function, but with additional predicate argument which only accumulates terms that pass that filter
(define (filtered-accumulate combiner null-value term a next b filter?)
  (if (> a b)
  null-value
  (if (filter? a)
      (combiner (term a)
                (filtered-accumulate combiner null-value term (next a) next b filter?))
      (filtered-accumulate combiner null-value term (next a) next b filter?)
      )))

; Using different unit tests to the exercise because CS61A skips section 1.2 in which we write a prime? predicate
(define (filtered-sum term a next b filter?)
  (filtered-accumulate + 0 term a next b filter?))

(define (square x) (* x x))

(define (even? x)
  (if (= (remainder x 2) 0)
      #t
      #f))

(define (sum-squares-of-evens a b)
 (filtered-sum square a (lambda (x) (+ 1 x)) b even?))