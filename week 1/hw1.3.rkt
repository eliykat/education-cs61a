#lang simply-scheme

; EXERCISE 3 - SWITCH

; Sentence => Sentence
; computes a sentence by replacing as follows:
;   I / me => you
;   you => me, unless at start of sentence, in which case you => i
(define (switch s)
  (if (= (count s) 1)
      (word-swap (first s))
      (sentence (switch (sentence (first s))) (switch (bf s)))
      ))

; Word => Word
; takes a word and swaps it if required by the exercise
(define (word-swap w)
  (cond ( (or (equal? w 'I) (equal? w 'me)) 'you)
        ( (equal? w 'you) 'me )
        ( (equal? w 'You) 'i )
        ( else w)))

(switch '(You told me that I should wake you up))