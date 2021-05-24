; Library for representing point and line segment

; Point lib
(define (make-point x y)
    (cons x y))
(define (x-point point)
    (car point))
(define (y-point point)
    (cdr point))
; Segment lib
(define (make-segment point-a point-b)
    (cons point-a point-b))
(define (start-segment segment)
    (car segment))
(define (end-segment segment)
    (cdr segment))

(define (average a b)
    (/ (+ a b) 2.0))

(define (print-point point)
    (display "(")
    (display (x-point point))
    (display ",")
    (display (y-point point))
    (display ")")
    (newline))

(define (midpoint-segment seg)
    (let ((x1 (x-point (start-segment seg)))
          (x2 (x-point (end-segment seg)))
          (y1 (y-point (start-segment seg)))
          (y2 (y-point (end-segment seg))))
      (make-point (average x1 x2)
                  (average y1 y2))))

(define a (make-point 5 6))
(define b (make-point 10 10))
(define segment-a (make-segment a b))

(print-point (midpoint-segment segment-a))
