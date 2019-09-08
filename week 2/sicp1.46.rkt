#lang simply-scheme

(define (square x) (* x x))

; SICP Exercise 1.46
; iterative-improve
; Takes 2 procedures as arguments: a good-enough? procedure and an improve-guess procedure
; Iteratively improves guesses until good-enough? is true
; This recursive lambda strategy is from: https://stackoverflow.com/questions/7719004/in-scheme-how-do-you-use-lambda-to-create-a-recursive-function
; Really great explanation here which helped me understand it: https://gist.github.com/z5h/238891
(define (iterative-improve good-enough? improve-guess)
  ((lambda (x) (x x))
   (lambda (self)
    (lambda (guess num)
       (if (good-enough? guess num)
           guess
           ((self self) (improve-guess guess num) num)))
     )
   )
  )

; test - sqrt function
(define (average a b)
  (/ (+ a b) 2))

(let ( (ge? (lambda (guess x)
              (< (abs (- (square guess) x)) 0.001)))
       (ig (lambda (guess x)
             (average guess (/ x guess)))))
   ( (iterative-improve ge? ig) 1.0 9) )