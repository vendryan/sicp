(define (square x)
    (* x x))

(define (even? x)
    (= (remainder x 2) 0))

(define (expmod b n m)
    (cond ((= n 0) 1)
          ((even? n) 
            (remainder (square (expmod b (/ n 2) m)) m))
          (else (remainder (* b (expmod b (- n 1) m)) m))))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (fast-prime? n 3)
        (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

(define (search-for-prime upper lower)
    (define (iter upper)
        (timed-prime-test upper)
        (cond ((< upper lower) (iter (+ upper 1)))))
    (iter upper))

; 561, 1105, 1729, 2465, 2821, and 6601 (Carmichael number fool fermat test)
; Not a prime but the test conclude that these number is a prime
(timed-prime-test 561)
(timed-prime-test 1105)
(timed-prime-test 1729)
(timed-prime-test 2465)
(timed-prime-test 2821)
(timed-prime-test 6601)