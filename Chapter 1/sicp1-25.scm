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

; (define (expmod b n m)
;     (cond ((= n 0) 1)
;           ((even? n) 
;             (remainder (square (expmod b (/ n 2) m)) m))
;           (else (remainder (* b (expmod b (- n 1) m)) m))))

(define (expmod b n m)
    (remainder (fast-expt b n) m))

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
    (if (fast-prime? n 4)
        (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

(define (search-for-prime upper lower)
    (define (iter upper)
        (timed-prime-test upper)
        (cond ((< upper lower) (iter (+ upper 1)))))
    (iter upper))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003) ; Stuck here in fact the number become so big that 
(timed-prime-test 100019) ; The computer struggling to compute it
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)