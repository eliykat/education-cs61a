; SICP 2.54

(define (equal? lista listb)
  (cond ((and (null? lista) (null? listb)) ; both lists are null
         #t)
        ((or (null? lista) (null? listb)) ; one list is null but not the other
         #f)
        ((and (list? (car lista)) (list? (car listb))) ; nested lists
         (equal? (car lista) (car listb)))
        (else ; treat cars as symbols to be compared
         (and (eq? (car lista) (car listb))
              (equal? (cdr lista) (cdr listb))))))

; Identical lists - should return true
(equal? '(this (is a) list) '(this (is a ) list))

; Nonidentical lists - should return false
(equal? '(this is a list) '(this (is a) list))
(equal? '(this (is a) list) '(this is a list))

; Nonidentical lists of different lengths - should return false
(equal? '(this is a list) '(this is))
(equal? '(this is) '(this is a list))