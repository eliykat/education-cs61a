; SICP 2.22

; Louis' program produces the answer list in reverse order because
; it computes the first list item first, and then appends it to the start of the
; 'answer' list - not the end. As each item of the list is appended to the start of
; the 'answer' list, it reverses the order. It's a first-in, last-out order.

; Louis' fix also doesn't work because it seems to break the structure of the list -
; the car of the first pair is nil, and the cdr is the value (rather than a list pointer).
; Subsequent constructor calls will just create nested pairs rather than a list.

; Test of the 'fixed' function to help me figure it out:
(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

(square-list '(2 3 4))