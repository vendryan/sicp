(define (iterative-improve good-enough? improve guess)
    (let ((next (improve guess)))
        (if (good-enough? guess next)
            guess
            (iterative-improve good-enough? improve next))))

(define (sqrt x)
    (define (average a b)
        (/ (+ a b) 2))
    (iterative-improve (lambda (y z) (< (abs (- (* y y) x))
                                      0.0001))
                       (lambda (y) (average y (/ x y)))
                       1.0))

(define (print x)
    (display x)
    (newline))

(print (sqrt 2))

(define (fixed-point f guess)
    (define (close-enough? a b)
        ((lambda (tolerance) (< (abs (- a b)) tolerance))
                 0.0001))
    (iterative-improve close-enough?
                       f
                       guess))

; Compute square root of 2
(print (fixed-point (lambda (y) (/ (+ y (/ 2 y)) 2))
                    1.0))
