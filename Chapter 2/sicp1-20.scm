(define (even? x)
    (= (remainder x 2) 0))
(define (odd? x)
    (= (remainder x 2) 1))

(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define (same-parity x . y)
    ; Iterative
    (define (iter a n ans)
        (cond ((null? a) ans)
              ((and (even? x) (even? (car a)))
                    (iter (cdr a) (+ n 1) (append ans (list (car a)))))
              ((and (odd? x) (odd? (car a)))
                    (iter (cdr a) (+ n 1) (append ans (list (car a)))))
              (else (iter (cdr a) (+ n 1) ans))))
    ; Recursive
    (define (recur a)
        (cond ((null? a) '())
              ((and (even? x) (even? (car a)))
                    (cons (car a) (recur (cdr a))))
              ((and (odd? x) (odd? (car a)))
                    (cons (car a) (recur (cdr a))))
              (else (recur (cdr a)))))
    ; (iter y 0 (list x))) ; Iterative solution
    (cons x (recur y))) ; Recursive solution

(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))
