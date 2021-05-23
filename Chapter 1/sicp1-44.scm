(define dx 0.00001)
(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))

(define (square x)
    (* x x))

(define (smooth f)
    (define (average a b c)
        (/ (+ a b c) 3))
    (lambda (x) (average (f (- x dx))
                         (f x)
                         (f (+ x dx)))))

(define (n-smoothed-func f n)
    (repeated (smooth f)
              n))

(display ((n-smoothed-func (lambda (x) (* 0.5 x)) 10) 5))
