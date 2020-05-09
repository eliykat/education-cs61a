; SICP 2.23
; for-each takes a procedure and a list of elements
; and applies the procedure to each element in turn.
; NB: it does not form a list of the results.

; lambda is used here to keep within the syntax of the if statement.

(define (for-each f listx)
  (if (empty? (cdr listx))
      (f (car listx))
      ((lambda ()
       (f (car listx))
       (for-each f (cdr listx))))))

; test from page 146
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))