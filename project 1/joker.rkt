#lang simply-scheme

; PROJECT ONE: TWENTY-ONE
; JOKER VERSION
; Adds 2 jokers to the pack, which can be worth any number from 1 to 11
; Modify what you need to to keep the game working - don't worry about optimising strategy, just make sure nothing blows up

; Code provided (but modified to include jokers)

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
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C) 'JOKER 'JOKER) ) ; JOKER: added 2 jokers to the deck when it is created

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
          (se (first in) (shuffle (se (bf in) out) (- size 1)))
          (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
        deck
        (move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 54) ) ; JOKER: increased deck size to accommodate additional jokers

; 1. Write best-total

; Best-total
; Given a hand of cards, returns the optimal total value, being the value as close as possible to 21 without busting
; This includes dealing with aces and jokers, which can have multiple values.
; given '(ad 8s), expect 19
; given '(ad 8s 5h), expect 14
; given '(ad as 9h), expect 21
; given '(3h joker), expect 14
(define (best-total hand)

  ; Simple-total
  ; Sum the value of all cards, disregarding aces and counting jokers as 1 (treated as their default value)
  (define (simple-total hand)
    (if (equal? hand '())
        0
        (+ (card-value (first hand)) (simple-total (bf hand)))))

  ; Best-total-aces
  ; Given the simple-total of a hand (int) and number of aces in the hand (int), will determine the best value of the aces
  ; to get as close as possible to 21 without exceeding 21.
  ; Does not determine the optimal value for jokers.
  (define (best-total-aces sum aces)
    (let ( (choose-best (lambda (a b)
                        (cond ( (and (>= a b) (<= a 21)) a)
                              ( (and (<= a b) (<= b 21)) b)
                              ( else a)))))
      (if (= aces 0)
          sum
          (choose-best (best-total-aces (+ sum 1) (- aces 1)) (best-total-aces (+ sum 11) (- aces 1))))))

  ; Card-count
  ; Counts the number (not value) of cards of a specified value in the hand of cards (disregarding suit)
  (define (card-count hand card)
    (cond ( (equal? hand '())
            0)
          ( (= (card-value (first hand)) card)
            (+ 1 (card-count (bf hand) card)))
          ( else
            (card-count (bf hand) card))))

  ; Body of function
  (let ( (aces-total (best-total-aces (simple-total hand) (card-count hand 0)))
         (jokers-count (card-count hand 1)) )
    (cond ( (or (= jokers-count 0) (>= aces-total 21))
            aces-total)
          ( (= jokers-count 1)
            (if (> (+ aces-total 10) 21) ; NB: joker already counts as 1, so adding 10 not 11
                21
                (+ aces-total 10)))
          ( (= jokers-count 2) ; assume that with 2 jokers we can make any combination to obtain 21
            21))))
  
; Returns the value of a card
; Jack, King, Queen = 10
; Ace = 0. If required, this should be tested for so it can be dealt with separately
; Any other card = its number
; JOKER: equal to 1 (treated as its default value, which is increased later if appropriate)
(define (card-value card)
  (cond ( (equal? card 'JOKER) 1)
        ( (member? (first card) '(J K Q j k q)) 10)
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

; Reckless
; Takes a strategy, returns a strategy
; New strategy should take one more card than the original would
(define (reckless strategy)
  (lambda (player dealer)
    (strategy (bl player) dealer)))

(define reckless-stop-at-17 (reckless stop-at-17))