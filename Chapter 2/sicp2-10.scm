; Print
(define (print line)
    (display line)
    (newline))

; Make interval
(define (make-interval upper lower)
    (cons upper lower))

; Way to get upper bound and lower bound of
; given interval
(define (lower-bound interval)
    (car interval))

(define (upper-bound interval)
    (cdr interval))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
    (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
        (error "Division by zero error") ; if divisor is zero raise error
        (mul-interval x 
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

; difference of interval is just upper bound - lower bound

; procedure that get the difference of given interval
(define (sub-interval interval)
    (- (upper-bound interval)
       (lower-bound interval)))

(define (width-interval interval)
    (/ (sub-interval interval)
       2))

(define test-1 (make-interval 8.0 20.0))
(define test-2 (make-interval 21.0 50.0))
(define add-test-1-and-test-2 (add-interval test-1 test-2))
(define mul-test-1-and-test-2 (mul-interval test-1 test-2))
(define test-3 (make-interval 0 4))
(define error-interval (div-interval test-1 test-3)) ; This line will raise zero division error

(print (width-interval test-1))
(print (width-interval test-2))
(print (width-interval add-test-1-and-test-2))
; (width-interval test-1) + (width-interval test-2) = (width-interval add-test-1-and-test-2)
(print (width-interval mul-test-1-and-test-2))
; (width-interval test-1) + (width-interval test-2) ≠ (width-interval mul-test-1-and-test-2)

; We able to proof this using algebra
