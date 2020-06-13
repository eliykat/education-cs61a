; SICP 2.29
; Lessons:
; 1. constructors and selectors really help you focus on the logic of problems rather than
; the details of their implementation - even if they seem too simple to bother with
; (e.g. they are just synonyms for car or cadr).

; 2. It is sometimes easier to write an algorithm in higher-level logic, and then write the
; lower-level functions afterwards. e.g. writing balanced? using branch-torque, then writing branch-torque afterwards

; 3. abstraction lets you change the detail of implementations without having to refactor code unnecessarily

; Initial constructors supplied by text
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; Refactored as per d.
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))
  

; UNIT TEST
(define test-mobile
  (make-mobile
   (make-branch 10
                (make-mobile
                 (make-branch 5 10)
                 (make-branch 20
                              (make-mobile
                               (make-branch 10 5)
                               (make-branch 10 10)))))
   (make-branch 50 50)))
                             

; a. Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

; refactored as per d.
; no need to change left-branch
(define (right-branch mobile)
  (cdr mobile))

; branch-length and branch-structure, which return the components of a branch
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; refactored as per d.
; no need to change branch-length
(define (branch-structure branch)
  (cdr branch))
  

; b. Using your selectors, define a procedure total-weight that returns the total weight of a mobile
(define (total-weight mobile)
  (let ((left-structure (branch-structure (left-branch mobile)))
        (right-structure (branch-structure (right-branch mobile))))
    (+
     (if (pair? left-structure)
         (total-weight left-structure)
         left-structure)
     (if (pair? right-structure)
         (total-weight right-structure)
         right-structure))))
       
; unit test
(total-weight test-mobile) ;75

; c. define a predicate that tests whether a binary mobile is balanced
; balanced =
;   length * weight of left rod == length * weight of right rod
;   and all submobiles must be balanced.
; Note: this means that we cannot have unbalanced submobiles that balance out higher up.
; All "leaves" must be balanced.

(define (balanced? mobile)
  (let ((branch-torque (lambda (branch)
                         (if (pair? (branch-structure branch))
                             (* (total-weight (branch-structure branch)) (branch-length branch))
                             (* (branch-structure branch) (branch-length branch))))))
    (and
     (equal?
      (branch-torque (left-branch mobile))
      (branch-torque (right-branch mobile)))
     (if (pair? (branch-structure (left-branch mobile)))
         (balanced? (branch-structure (left-branch mobile)))
         #t)
     (if (pair? (branch-structure (right-branch mobile)))
         (balanced? (branch-structure (right-branch mobile)))
         #t))))


; build a balanced mobile for testing purposes
(define test-mobile-2
  (make-mobile
   (make-branch 10 ; torque 450
                (make-mobile
                 (make-branch 10 30) ; torque 300
                 (make-branch 20 ; torque 300
                              (make-mobile
                               (make-branch 10 5) ; torque 15
                               (make-branch 5 10))))) ; torque 15
   (make-branch 10 45 ))) ; torque 450

(balanced? test-mobile) ;#f
(balanced? test-mobile-2); #t


; d. change constructors of mobile and branch to use pairs (i.e. cons) instead of a list
; see refactored code above
; The only thing we needed to change was the selectors, as no other function interacted
; with the details of the implementation.