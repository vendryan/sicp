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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; Test
; Hmmm... why different?
(define r1 (make-center-percent 35 0.1))
(define r2 (make-center-percent 45 0.1))
(define test (par1 r1 r2))
(define test2 (par2 r1 r2))
(define test3 (div-interval r1 r1))
(define test4 (div-interval r2 r2))

; The bigger the percentage, the bigger the error
; Even divide interval by itself is not accurate
; It's should be 1 < x < 1 but it give big error
; The bigger the percentage
(print-interval test)
(print-interval test2)
(print-interval test3)
(print-interval test4)
