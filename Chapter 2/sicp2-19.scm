(define test (list 1 2 3 4 5 9))
(define test2 (list 4 5 9 3 7))
(define us-coins (list 25 5 10 1 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
    (define (no-more? coin-list)
        (null? coin-list))
    (define (except-first-denomination coin-list)
        (cdr coin-list))
    (define (first-denomination coin-list)
        (car coin-list))
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
            (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(display (cc 100 us-coins))
(newline)
(display (cc 100 uk-coins))
; The order of coins amount in the list will not affect
; the result because the procedure is checking all
; possible combination of the coin and thus by changing
; the order doesn't affect anything
