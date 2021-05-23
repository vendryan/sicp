; Theorem : If n is composite number, then there is at
; least one number x such that x <= sqrt(n) and
; x divides n. If there's no such number, n is a prime

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

(display (smallest-divisor 199)) (newline) ; 199
(display (smallest-divisor 1999)) (newline) ; 1999
(display (smallest-divisor 19999)) (newline) ; 7
