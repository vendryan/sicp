(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (square x)
    (* x x))

(define (even? x)
    (= (remainder x 2) 0))

; Recursive process
(define (cont-frac combiner n d k)
    (define (recur a)
        (if (= a k)
            (/ (n a) (d a))
            (/ (n a) 
               (combiner (d a)
                         (recur (+ a 1))))))
    (recur 1))

(define (tan-cf x k)
    (define (expt-pattern n)
        (cond ((= n 1) x)
              (else (square x))))
    
    (cont-frac -
               expt-pattern
               (lambda (n) (- (* 2 n) 1))
               k))

; Compute tan using continued fraction
(display (tan-cf 0.7853981634 1000))
