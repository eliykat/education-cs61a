; SICP 2.31
; abstraction of tree-map in exercise 2.3

(define (square x) (* x x))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

; unit test from text
(define test-tree (list 1
                        (list 2 (list 3 4) 5)
                        (list 6 7)))

(square-tree test-tree)