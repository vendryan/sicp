; Recursive
(define (filtered-accumulate combiner null-value filter term a next b)
    (if (> a b)
        null-value
        (combiner (if (filter a) (term a) null-value)
                  (filtered-accumulate combiner null-value filter term (next a) next b))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (square x)
    (* x x))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
    (find-divisor n 2))

(define (prime? n)
    (= (smallest-divisor n) n))

; Sum the prime in the interval from a to b inclusive
(display (filtered-accumulate +
                              0
                              prime?
                              (lambda (x) x)
                              2
                              (lambda (x) (+ x 1))
                              10))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(newline)
(display (gcd 206 45))
(newline)

(define (product-relatively-prime n)
    (define (relatively-prime? a)
        (= (gcd n a) 1))
    (filtered-accumulate *
                         1
                         relatively-prime?
                         (lambda (x) x)
                         1
                         (lambda (x) (+ x 1))
                         n))

; Product of number i less than n such that gcd(i, n) = 1
(display (product-relatively-prime 10))
