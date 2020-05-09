; HW 4.3
; Substitute 2
; Takes: list, list of old words, list of new words. (old & new word lists should be same length)
; Returns: copy of first argument list, but with each word that occurs in the second argument, replaced by the corresponding word of the third argument.

(define (substitute2 tree old new)

  ; Combines the old and new word lists into a single list,
  ; where each item is a pair of the old word and corresponding new word.
  ; Assumes that both lists are of equal length.
  (define (combine-lists old new)
    (if (empty? old)
        '()
        (let ((old-word (car old))
              (new-word (car new))
              (rest-of-old (cdr old))
              (rest-of-new (cdr new)))
          (cons (cons old-word new-word) (combine-lists rest-of-old rest-of-new)))))

  ; Given a word, iterates along the change-list to find the replacement new word.
  ; If no replacement is found, returns the current word unchanged.
  ; Note that the let block is nested after the empty? check so that we do not try to access car/cdr of an empty list. The trade-off is that we are obscuring what is really a 'cond' structure with nested 'ifs'.
  (define (change-word current-word change-list)
    (if (empty? change-list)
        current-word
        (let ((old-word (car (car change-list)))
              (new-word (cdr (car change-list)))
              (rest-of-list (cdr change-list)))
            (if (equal? old-word current-word)
                new-word
                (change-word current-word rest-of-list)))))

  ; Main program loop
  (define (loop tree change-list)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (loop sub-tree change-list)
               (change-word sub-tree change-list))) tree))

  ; Entry point into main program loop
  (loop tree (combine-lists old new)))
        

; unit test as per hw
(substitute2
 '((4 calling birds) (3 french hens) (2 turtle doves))
 '(1 2 3 4)
 '(one two three four))