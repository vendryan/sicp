; Iterative process of *

(define (double x)
    (+ x x))

(define (halve x)
    (/ x 2))

(define (even? x)
    (= (remainder x 2) 0))

(define (* a b)
    (*-iter a b 0))

(define (*-iter a b ans)
    (cond ((= b 0) ans)
          ((even? b) (*-iter (double a) (halve b) ans))
          (else (*-iter a (- b 1) (+ ans a)))))
      
(display (* 4 19))
(newline)
(display (* 4 0))
(newline)
(display (* 0 4))
