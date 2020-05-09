; SICP 2.17
; last-pair returns the list that contains only the last element of a given non-empty list.
(define (last-pair target)
  (if (empty? (cdr target))
      target
      (last-pair (cdr target))))

; unit test
(define lista (list 1 2 3 4 5 6))
(last-pair lista)