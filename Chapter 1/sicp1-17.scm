; Recursive process of *

(define (double x)
    (+ x x))

(define (halve x)
    (/ x 2))

(define (even? x)
    (= (remainder x 2) 0))

(define (* a b)
    (cond ((= b 0) 0)
          ((even? b) (double (* a (halve b))))
          (else (+ a (* a (- b 1))))))
      
(display (* 4 9))
