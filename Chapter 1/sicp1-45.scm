(define dx 0.0001)

(define (print x)
    (display x)
    (newline))

(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (expt b n)
    (if (= n 0)
        1
        (* b (expt b (- n 1)))))

(define (fixed-point f guess)
    (define (close-enough? a b)
        ((lambda (tolerance) (< (abs (- a b)) tolerance))
                 0.000000001)) 
    (define (fixed-point-search guess)
        (let ((next (f guess)))
             (if (close-enough? guess next)
                 next
                 (fixed-point-search next))))
    (fixed-point-search guess))

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))

(define (average-damp f)
    (lambda (x) (/ (+ x
                      (f x))
                  2)))

(define (root x n)
  ; The root will not converge if (floor (log n (base 2))) > number-of-repeated-average-damp
  ; So to calculate the number of repeat we can do as following
    (define repeated-damp (floor (/ (log n)
                                    (log 2))))
    (fixed-point ((repeated average-damp repeated-damp) (lambda (y) (/ x 
                                                                       (expt y (- n 1)))))
                 1.0))

(print (root 2 32)) 
(print (root 2 33))
(print (root 2 2))
(print (root 2 4))
