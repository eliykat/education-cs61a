#lang simply-scheme

; EXTRA FOR EXPERTS

; Adapted from the count change procedure from SICP p. 52
(define (partition n)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          kinds-of-coins)
                       kinds-of-coins)))))
  (cc n n))


(partition 5)

; Counting partitions is like making change, where the coins are ... each number equal to or less than n.