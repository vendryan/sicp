(define dx 0.0001)
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


(define deriv
    (lambda (g)
        (lambda (x) 
            (/ (- (g (+ x dx))
                  (g x))
                dx))))

(define (newton-transform f)
    (define deriv-of-f (deriv f))
    (lambda (x)
        (- x
           (/ (f x)
              (deriv-of-f x)))))

(define (newton-method f guess)
    (fixed-point (newton-transform f)
                 guess))

(define (cubic a b c)
    (lambda (x)
        (+ (* x x x)
           (* a x x)
           (* b x)
           c)))

(display (newton-method (cubic 1 1 1)
                        1.0))
