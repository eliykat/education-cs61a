; SICP 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

; What are the expected results of evaluating the following expressions?

; Append joins two lists together.
; Expected result: (1 2 3 4 5 6)
(append x y)

; Cons adds a single new item to the start of a list.
; In this case, it has the effect of nesting x within y.
; Expected result: ((1 2 3) 4 5 6)
(cons x y)

; List creates a list with its arguments as items.
; In this case, it would nest both lists.
; Expected result: ((1 2 3) (4 5 6))
(list x y)