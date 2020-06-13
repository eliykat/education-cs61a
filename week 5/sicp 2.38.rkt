; SICP 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; from SICP p. 158, aka fold-right
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list null (list 1 2 3))
(fold-left list null (list 1 2 3))

; What property should op satisfy to guarantee that fold-right and fold-left
; return the same result for any sequence?
; Answer: op should be commutative, i.e. should produce the same result regardless of the order
; of arguments. For example:

(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))
(fold-right + 0 (list 4 5 6))
(fold-left + 0 (list 4 5 6))