; Print
(define (print line)
    (display line)
    (newline))

; Check if given number is negative or not
(define (negative? x)
    (< x 0))

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

; Divided into 9 cases
(define (mul-interval2 x y)
    (let ((lx (lower-bound x))
          (ly (lower-bound y))
          (ux (upper-bound x))
          (uy (upper-bound y)))
    (cond ((or (and (not (negative? lx)) ; - - - -
                    (not (negative? ux)) ; + + + +  Cases
                    (not (negative? ly))
                    (not (negative? uy)))
               (and (negative? lx)
                    (negative? ux)
                    (negative? ly)
                    (negative? uy)))
            (make-interval (* lx ly)
                           (* ux uy)))
          ()

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
