(define (fast-expt b n)
    (fast-expt-iter b 1 n))

(define (square x)
    (* x x))

(define (even? x)
    (= (remainder x 2) 0))

(define (fast-expt-iter b a n)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) a (/ n 2)))
          (else (fast-expt-iter b (* a b) (- n 1)))))

(define (expt b n)
    (if (= n 0)
        1
        (* b (expt b (- n 1)))))

(display (fast-expt 5 10000))
(newline)
(display (expt 5 10000))
