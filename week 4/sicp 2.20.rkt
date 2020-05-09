;SICP 2.20
;same-parity takes an arbitrary number of integers as arguments
; and returns a list of all the arguments that have the same even-odd parity as the first argument.
(define (same-parity . args)
  (define (loop test-num args)
    (let ( (parity? (lambda (a b)
                      (equal? (even? a) (even? b)))))
      (cond ( (empty? args)
              args)
            ( (parity? test-num (car args))
              (cons (car args) (loop test-num (cdr args))))
            ( else
              (loop test-num (cdr args))))))
  (loop (car args) args))

; unit test
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8)

; A couple of notes:
; The internal loop definition is needed, because the 'dot' notation in the function signature
; will create a list of args. When calling itself recursively, this means we get a list of list of args.
; We really only need the dot notation at the entry point to the function, not for its recursive calls.
; This separate internal definition also allows us to separately define test-num, which is the natural way of doing it.

; Also be careful when using let definitions, because they will always be evaluated, even when the conditional case in which they are invoked
; is never executed. This can cause problems - e.g. in the case of (define rest-of-list (cdr args)), but in the base case args is an empty list.