(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define reciprocal-phi 0.6180257510729613)

; Recursive process
(define (cont-frac n d k)
    (define (recur a)
        (if (= a k)
            (/ (n a) (d a))
            (/ (n a) 
               (+ (d a)
                  (recur (+ a 1))))))
    (recur 1))

(display (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    11))

; phi : golden ratio
; So when k = 11, 1/phi is accurate to 4 decimal place

; Iterative process
(define (cont-frac-iter n d k)
    (define (iter a result)
        (if (= a 0)
            result
            (iter (- a 1) (/ (n a)
                             (+ (d a)
                                result)))))
    (iter k 0))

(newline)
(display (cont-frac-iter (lambda (i) 1.0)
                         (lambda (i) 1.0)
                         11))
