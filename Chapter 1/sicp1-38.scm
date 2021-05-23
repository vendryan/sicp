(define (abs x)
    (if (< x 0)
        (- x)
        x))

; 1, 2, 1, 1, 4, 1,1 ,8, 1, ...
; Don't mind the procedure name
(define (strange-pattern k)
    (if (= (remainder k 3) 2)
        (* (/ (+ k 1) 3) 2)
        1))

; Recursive process
(define (cont-frac n d k)
    (define (recur a)
        (if (= a k)
            (/ (n a) (d a))
            (/ (n a) 
               (+ (d a)
                  (recur (+ a 1))))))
    (recur 1))

; Approximate e - 2
(display (cont-frac (lambda (i) 1.0)
                    strange-pattern
                    100))
