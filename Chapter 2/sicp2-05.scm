; Represent pair of number a and b in term of 
; 2^a*3^b

(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

; 2^a*3^b can be expressed as 2x2x...x2x3x3x...x3
; where 2 appears a times and 3 appears b time and hence
; to get a we just modulo the number we got with two till the
; mod is not zero. The number of time modulo equal zero
; Is the a. This same method can applied to be but
; mod with number 3 instead
(define (carr pair)
    (if (= (remainder pair 2) 0)
        (+ 1 (carr (/ pair 2)))
        0))

(define (cdrr pair)
    (if (= (remainder pair 3) 0)
        (+ 1 (cdrr (/ pair 3)))
        0))

(define (print line)
    (display line)
    (newline))

; try
(define x (cons 3 4))

(print (carr x)) ; should be 3
(print (cdrr x)) ; should be 4
