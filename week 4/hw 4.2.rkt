; HW 4.2
; Substitute takes 3 arguments: a list, an old word, and a new word.
; Returns: a copy of the list with every old word replaced with new word - including sublists.

(define (substitute tree old new)
  (let ((change-word (lambda (test)
                       (if (equal? old test)
                           new
                           test))))
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (substitute sub-tree old new)
               (change-word sub-tree))) tree)))

; unit test as per hw
(substitute '((lead guitar) (bass guitar) (rhythm guitar) drums) 'guitar 'axe)