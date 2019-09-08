#lang simply-scheme

; EXERCISE 2
; every
; applies function f to every word in sentence s
(define (every f s)
  (if (empty? s)
      '()
      (se (f (first s)) (every f (bf s)))))

(every (lambda (w)
         (if (equal? (last w) 'e)
             '()
             w))
       '(Hi there neighbour welcome to your new house))