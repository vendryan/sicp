(define (square x)
    (* x x))

; Recursive process of product
(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))

; Factorial 
(define (factorial n)
    (define (inc x)
        (+ x 1))
    (define (fact-identity x) ; 0! = 1
        (cond ((= x 0) 1)
              ((> x 0) x)))
    (product fact-identity 0 inc n))

(display (factorial 3))
(newline)
(display (factorial 10))
(newline)

(define (pi-product n)
    (define (pi-term x)
        (/ (* x 2 
              (* (+ x 1) 
                 2))
           (square (+ (* x 2) 1))))
    (define (inc x)
        (+ x 1))
    (product pi-term 1 inc n))

(display (* 4.0 (pi-product 1000)))

; Iterative process of product
(define (product-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

(newline)
(display (product-iter square 1 (lambda (x) (+ x 1)) 3))
