(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (fixed-point f guess)
    (define (close-enough? a b)
        ((lambda (tolerance) (< (abs (- a b)) tolerance))
                 0.0001)) 
    (define (fixed-point-search guess)
        (let ((next (f guess)))
             (if (close-enough? guess next)
                 next
                 (fixed-point-search next))))
    (fixed-point-search guess))

; Finding golden ratio
(display (fixed-point (lambda (x) (+ 1 (/ 1 x)))
                      1.0))
