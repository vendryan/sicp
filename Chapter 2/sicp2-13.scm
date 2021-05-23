; Print
(define (print line)
    (display line)
    (newline))
; Check if given number is negative or not
(define (negative? x)
    (< x 0))
; Make interval
(define (make-interval a b)
    (if (< b a) 
        (cons b a)
        (cons a b)))
; Way to get upper bound and lower bound of
; given interval
(define (lower-bound interval)
    (car interval))
(define (upper-bound interval)
    (cdr interval))
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (print-interval interval)
    (display (lower-bound interval))
    (display " < x < ")
    (display (upper-bound interval))
    (newline))

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

; *** Answer here ***
; Produce interval of given center and percentage
(define (make-center-percent center percentage)
    (let ((deviation (* (/ percentage 100.0) 
                        (abs center))))
        (make-interval (- center deviation)
                       (+ center deviation))))

; Produce percentage tolerance of given interval
(define (percent interval)
    (let ((ctr (center interval))
          (lx (lower-bound interval)))
      (let ((deviation (- ctr lx)))
          (abs (* (/ deviation ctr) 100)))))

; Test
(define test (make-center-percent 25 0.001))
(define test2 (make-center-percent 35 0.001))
(define test3 (mul-interval test test2))
(print-interval test)
(print (percent test)) ; 0.0010000000000047748
(print-interval test2)
(print (percent test2)) ; 0.000999999999992594
(print-interval test3)
(print (percent test3)) ; 0.0019999999997916845

; let a = %x, b = %y 
; We can see that for the small percentage tolerance
; The percentage tolerance if we mul the interval is approximately
; %c ≈ %x + %y
