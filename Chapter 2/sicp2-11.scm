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

; Divided into 9 cases
(define (mul-interval2 x y)
    (let ((lx (lower-bound x))
          (ly (lower-bound y))
          (ux (upper-bound x))
          (uy (upper-bound y)))       ;   lx ux ly uy
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
          ((or (and (not (negative? lx)) ; - - + +
                    (not (negative? ux)) ; + + - -  Cases
                    (negative? ly)
                    (negative? uy))
               (and (negative? lx)
                    (negative? ux)
                    (not (negative? ly))
                    (not (negative? uy))))
            (make-interval (* lx uy)
                           (* ux ly)))
          ((and (negative? lx) ; - + + +
                (not (negative? ux))
                (not (negative? ly))
                (not (negative? uy)))
            (make-interval (* lx uy)
                           (* ux uy)))
          ((and (not (negative? lx)) ; + + - +
                (not (negative? ux))
                (negative? ly)
                (not (negative? uy)))
            (make-interval (* ux ly)
                           (* ux uy)))
          ((and (negative? lx) ; - + - +
                (not (negative? ux))
                (negative? ly)
                (not (negative? uy)))
            (make-interval (min (* ux ly) (* lx uy))
                           (max (* ux uy) (* lx ly))))
          ((and (negative? lx) ; - - - +
                (negative? ux)
                (negative? ly)
                (not (negative? uy)))
            (make-interval (* lx uy)
                           (* lx ly)))
          ((and (negative? lx) ; - + - -
                (not (negative? ux))
                (negative? ly)
                (negative? uy))
            (make-interval (* ux ly)
                           (* lx ly))))))
         

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

(define pp (make-interval 8 20))
(define mp (make-interval -8 9))
(define mm (make-interval -9 -1))

; Test all cases
(print-interval (mul-interval2 pp pp))
(print-interval (mul-interval2 mp pp))
(print-interval (mul-interval2 mm pp))
(print-interval (mul-interval2 pp mp))
(print-interval (mul-interval2 mp mp))
(print-interval (mul-interval2 mm mp))
(print-interval (mul-interval2 pp mm))
(print-interval (mul-interval2 mp mm))
(print-interval (mul-interval2 mm mm))
(newline)
(print-interval (mul-interval pp pp))
(print-interval (mul-interval mp pp))
(print-interval (mul-interval mm pp))
(print-interval (mul-interval pp mp))
(print-interval (mul-interval mp mp))
(print-interval (mul-interval mm mp))
(print-interval (mul-interval pp mm))
(print-interval (mul-interval mp mm))
(print-interval (mul-interval mm mm))
