#lang simply-scheme

; PROJECT ONE: TWENTY-ONE

; Code provided:

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
          ((< (best-total dealer-hand-so-far) 17)
           (play-dealer customer-hand
                        (se dealer-hand-so-far (first rest-of-deck))
                        (bf rest-of-deck)))
          ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
          ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
          (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
          ((strategy customer-hand-so-far dealer-up-card)
           (play-customer (se customer-hand-so-far (first rest-of-deck))
                          dealer-up-card
                          (bf rest-of-deck)))
          (else
           (play-dealer customer-hand-so-far
                        (se dealer-up-card (first rest-of-deck))
                        (bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
                   (first (bf (bf deck)))
                   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
          (se (first in) (shuffle (se (bf in) out) (- size 1)))
          (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
        deck
        (move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

; 1. Write best-total

; Best-total
; Given a hand of cards, returns the optimal total value, being the value as close as possible to 21 without busting
; This includes dealing with aces, which can have multiple values.
; given '(ad 8s), expect 19
; given '(ad 8s 5h), expect 14
; given '(ad as 9h), expect 21
(define (best-total hand)

  ; Sum the value of all cards but aces
  (define (sum-non-aces hand)
    (if (equal? hand '())
        0
        (+ (card-value (first hand)) (sum-non-aces (bf hand)))))

  ; Count number of aces in a hand
  (define (count-aces hand)
    (cond ( (equal? hand '()) 0)
          ( (= (card-value (first hand)) 0) (+ 1 (count-aces (bf hand))))
          ( else (count-aces (bf hand)))))
  
  ; Body of function
  (define (loop sum aces)
    (let ((choose-best (lambda (a b)
                         (cond ( (and (>= a b) (<= a 21)) a)
                               ( (and (<= a b) (<= b 21)) b)
                               ( else a)))))
      (if (= aces 0)
          sum
          (choose-best (loop (+ sum 1) (- aces 1)) (loop (+ sum 11) (- aces 1))))))

  (loop (sum-non-aces hand) (count-aces hand)))

; Returns the value of a card
; Jack, King, Queen = 10
; Ace = 0. If required, this should be tested for so it can be dealt with separately
; Any other card = its number
(define (card-value card)
  (cond ( (member? (first card) '(J K Q j k q)) 10)
        ( (member? (first card) '(A a)) 0)
        ( else (bl card))))

; 2. stop-at-17.
; Write a strategy that mirrors the dealers.  Hits only if total is less than 17.
; Returns #t if hitting, #f otherwise
(define (stop-at-17 player dealer)
  (if (< (best-total player) 17)
      #t
      #f))

; 3. play-n
; Plays n games with given strategy and returns the number of games won, less number of games lost
(define (play-n strategy n)
  (if (= n 0)
      0
      (+ (twenty-one strategy) (play-n strategy (- n 1)))))

; 4. dealer-sensitive strategy
; player hits if and only if:
; dealer has ace, 7-10 or picture card, and player has less than 17
; OR
; dealer has 2-6, and customer less than 12.
; assume that it is acceptable to use our best-total function even though that refers to 21 as the ideal max and not the thresholds below
(define (dealer-sensitive player dealer)
  (cond ( ( and (member? (card-value dealer) '(0 7 8 9 10)) (< (best-total player) 17))
          #t)
        ( ( and (member? (card-value dealer) '(2 3 4 5 6)) (< (best-total player) 12))
          #t)
        (else #f)))

; 5. stop-at
; Domain: an integer n
; Range: a procedure representing a strategy that 'hits' until the player's total is n or more.
(define (stop-at n)
  (lambda (player dealer)
    (if (< (best-total player) n)
      #t
      #f)))

; 6. Valentine's
; a strategy that stops at 17 unless you have a heart, in which case it stops at 19.

; Has-suit?
; Returns #t if hand has suit, #f otherwise
; note that this is iterative rather than recursive, because this allows us to shortcut further function calls as soon as we find the suit
(define (has-suit hand suit)
  (cond ( (equal? hand  '())
          #f)
        ( ( equal? (last (first hand)) suit)
          #t)
        (else (has-suit (bf hand) suit))))

(define (valentines player dealer)
  (if (has-suit player 'H)
      ((stop-at 19) player dealer)
      ((stop-at 17) player dealer)))

; 7. Suit-strategy
; Takes 3 arguments: a suit, a strategy to use if your hand doesn't include the suit, and a strategy to use if your hand does include that suit
; Returns a strategy
(define (suit-strategy suit no-strategy yes-strategy)
  (lambda (player dealer)
    (if (has-suit player suit)
        (yes-strategy player dealer)
        (no-strategy player dealer))))

; Refactor valentines using suit-strategy
(define valentines-v2
  (suit-strategy 'H (stop-at 17) (stop-at 19)))

; 8. Majority
; takes 3 strategies as arguments
; returns a strategy that will hit only if 2/3rds of the argument strategies recommends hitting
(define (majority strategy-1 strategy-2 strategy-3)
  (lambda (player dealer)
    (cond ( (and (strategy-1 player dealer) (strategy-2 player dealer)) #t)
          ( (and (strategy-1 player dealer) (strategy-3 player dealer)) #t)
          ( (and (strategy-2 player dealer) (strategy-3 player dealer)) #t)
          ( else #f))))

(define majority-test
  (majority stop-at-17 dealer-sensitive valentines))

(play-n majority-test 10)

; Reckless
; Takes a strategy, returns a strategy
; New strategy should take one more card than the original would
(define (reckless strategy)
  (lambda (player dealer)
    (strategy (bl player) dealer)))

(define reckless-stop-at-17 (reckless stop-at-17))