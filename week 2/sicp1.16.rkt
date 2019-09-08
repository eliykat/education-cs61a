#lang simply-scheme
(require trace)

; SICP EXERCISE 1.16
; Write an iterative exponentiation procedure with an efficiency of O(log n)

; Recursive example from SICP p 58-59 to see how it works
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

; Solution courtesy of Oscar Lopez on Stackoverflow: https://stackoverflow.com/questions/33855937/turning-a-recursive-procedure-to-a-iterative-procedure-sicp-exercise-1-16.
; I couldn't figure this one out on my own, but seeing and understanding this solution helped.
; The lesson is: you make the transformation from recursive to iterative by having an "accumulator" variable which carries forward the result of each iteration, in the same way that returning from each function
; carries the result back up the chain of recursive function calls.  For this reason, the acc variable should initiate to the return value of the base case.
(define (expt b n)
  (define (iter-expt acc b n)
    (if (= n 0)
        acc
        (if (even? n)
            (iter-expt acc (square b) (/ n 2))
            (iter-expt (* acc b) b (- n 1)))))
  (iter-expt 1 b n))