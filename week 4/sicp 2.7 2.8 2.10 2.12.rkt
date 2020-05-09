; EXERCISE 2.7

(define (make-interval lower upper)
  (cons lower upper))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

; unit test
(define testa (make-interval 5 10))
(lower-bound testa)
(upper-bound testa)

; EXERCISE 2.8
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (lower-bound b))
                 (- (upper-bound a) (upper-bound b))))

; unit test
(define testb (make-interval 2 -20))

(sub-interval testa testb)

; EXERCISE 2.10

; copy mul-interval from sicp
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; spans-zero
(define (spans-zero? x y)
  (if (or (and (>= x 0) (<= y 0))
          (and (<= x 0) (>= y 0)))
      #t
      #f))

; modified div-interval
(define (div-interval x y)
  (if (or (spans-zero? (upper-bound x) (lower-bound x)) (spans-zero? (upper-bound y) (lower-bound y)))
      (error "Error: cannot divide by an interval that spans zero.")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

; EXERCISE 2.12
; make-center-percent is a constructor that takes a center and a percentage tolerance and produces the desired interval.
(define (make-center-percent center percent)
  (let ((percent-value (* center percent)))
    (make-interval (- center percent-value) (+ center percent-value))))

; percent selector returns the percentage tolerance from a given interval
(define (percent interval)
  (let ( (midpoint (/ (+ (upper-bound interval) (lower-bound interval)) 2)))
    (- (/ (upper-bound interval) midpoint) 1 )))
  

; unit test
(define testc (make-center-percent 50 0.24))
testc
(percent testc)