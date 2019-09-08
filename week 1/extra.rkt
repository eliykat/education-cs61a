#lang simply-scheme

; Extra for experts
; I already learned how to do this for SICP 1.46, but doing it again without referring to those resources to confirm that I understand it.
; For reference, the materials I used to learn this were:
; https://stackoverflow.com/questions/7719004/in-scheme-how-do-you-use-lambda-to-create-a-recursive-function
; https://gist.github.com/z5h/238891

(((lambda (x) (x x))
(lambda (fact)
  (lambda (n)
    (if (= n 0)
        1
        (* n ((fact fact) (- n 1)))))
  )) 5)