; SICP 2.37
; this took a while because I had to figure out the matrix algebra; however, this was made easier
; by the structure of each function already supplied by the book, and the fact that they build on
; each other - which is in fact the point of the exercise. These are fairly convoluted algebraic
; operations that can be implemented within a few lines of code because they all use a common
; interface that can be chained together, and can build on top of each other.

; Key to this was finding an explanation which defined the operations in terms of each other:
; https://mathinsight.org/matrix_vector_multiplication

; ---- CODE FROM SICP

; copy accumulate function from p 158
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; accumulate-n from 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; example function
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; ---- MY CODE

(define (matrix-*-vector m v)
 (map (lambda (w) (dot-product v w)) m))

(matrix-*-vector
 (list (list 1 2 3) (list 4 5 6) (list 7 8 9))
 (list 2 1 3))

(define (transpose mat)
  (accumulate-n cons null mat))

(transpose
 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (v) (matrix-*-vector cols v))
     m)))

(matrix-*-matrix
 (list (list 0 4 -2)
       (list -4 -3 0))
 (list (list 0 1)
       (list 1 -1)
       (list 2 3)))