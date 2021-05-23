; This one is hard so i searched internet

(define (square x)
    (* x x))

(define (even? x)
    (= (remainder x 2) 0))

(define (expmod base exp m)
    (define (trivial-sqrt-of-1 x m)
        (if (and (not (or (= x 1)
                          (= x (- m 1))))
                 (= (remainder (square x) m) 1))
            0
            (remainder (square x) m)))
    (cond ((= exp 0) 1)
          ((even? exp)
            (trivial-sqrt-of-1 (expmod base (/ exp 2) m) m))
          (else 
            (remainder (* base (expmod base (- exp 1) m)) m))))


(define (fermat-test n)
    (define (try-it a)
        (= (expmod a (- n 1) n) 1))
    (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (miller-rabin-prime? n (- times 1)))
          (else #f)))

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (miller-rabin-prime? n 3)
        (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

(define (search-for-prime upper lower)
    (define (iter upper)
        (timed-prime-test upper)
        (cond ((< upper lower) (iter (+ upper 1)))))
    (iter upper))

; Carmichael number didn't fool Miller Rabin Test
(timed-prime-test 561)
(timed-prime-test 2)
(timed-prime-test 3)
(timed-prime-test 4)
(timed-prime-test 5)
(timed-prime-test 6)
(timed-prime-test 1105)
(timed-prime-test 1729)
(timed-prime-test 2465)
(timed-prime-test 2821)
(timed-prime-test 6601)

; (load "sicp1-28.scm")