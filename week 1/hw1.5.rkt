#lang simply-scheme

; EXERCISE 5 - ENDS-E

; Sentence => Sentence
; returns a sentence composed of only the words that end in e
(define (ends-e s)
  (if (= (count s) 1)
      (if (equal? (last(last s)) 'e)
          s
          '())
      (se (ends-e (se(first s))) (ends-e (bf s)))
      ))

(ends-e '(please put the salami above the blue elephant))