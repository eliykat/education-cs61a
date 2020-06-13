; SICP 2.32
; I had to look at the sicp-wiki to understand the logic of this.
; In retrospect, re-inserting the first element of s into the subset using map
; is necessary, because otherwise we miss elements entirely, but for this reason the sample code provided really threw me.

; However, the code is mine (excluding the code provided by the exercise).
; Feeling really good about the technique of wrapping lambda statements in this way!
; Very cool.

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      ((lambda (e)
                         (lambda (item)
                           (cons e item))) (car s))
                      rest)))))

(subsets (list 1 2 3))