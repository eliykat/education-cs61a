#lang simply-scheme

; EXERCISE 3
; What does changing the base case of the count change procedure do? What arguments will produce different results?

; This change produces different results if the arguments are (0 0).
; - in the original, it will count it as 1 way to make change, because it checks the amount first and thinks that we have divided the amount perfectly by the kinds-of-change.
; - in the swapped version, it will count as 0 ways to make change, because we are immediately out of change to make change from.  This seems like the better way to do it.
; However, this seems like a moot point because the recursive call will never reach this case. It only decrements 1 argument at a time, not both, so only 1 argument will reach 0 before reaching a base case.
; e.g. (0 1) or (1 0), never (0 0).

; Count change procedure from SICP p. 52
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))


(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Count change procedure, but with the base cases swapped as per the homework question
(define (cc-swapped amount kinds-of-coins)
  (cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
        ((= amount 0) 1)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(cc 0 0)
(cc-swapped 0 0)