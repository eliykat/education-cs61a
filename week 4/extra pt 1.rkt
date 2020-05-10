; Week 4 extra for experts part 1
; The trick here is to remember that cxr-function is only composing a set of functions, NOT calling them.
; Think carefully about what you want to evaluate in the recursive calls - and what you do not (e.g. car/cdr).
; This was my first problem after returning to cs61a after 8 months or so and it was a good reminder of
; some major themes so far: lambda functions,recursion, evaluation, higher order functions.

; cxr-function
; input: word starting with c, ending with r, and having a string of a and/or d in between. eg. cdddadadr
; output: the corresponding function to traverse a list. i.e. a function composed of car and cdr
(define (cxr-function cxr)
  (let ((f (cond
        ( (equal? (first cxr) 'c)
          (cxr-function (butfirst cxr)))
        ( (equal? (first cxr) 'a)
          ((lambda (f) (lambda (x) (car (f x)))) (cxr-function (butfirst cxr)) ) )
        ( (equal? (first cxr) 'd)
          ((lambda (f) (lambda (x) (cdr (f x)))) (cxr-function (butfirst cxr)) ) )
        ( (equal? (first cxr) 'r)
          (lambda (x) x) ))))
    (lambda (x) (f x))))

(define test-list (list '(because i love you) '(truth hurts) 'juice '(good as hell)))

((cxr-function 'cadadddr) test-list) ; as