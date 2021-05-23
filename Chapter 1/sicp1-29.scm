(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
            (sum term (next a) next b))))

(define (cube x)
    (* x x x))

(define (even? x)
    (= (remainder x 2) 0))

(define (inc x)
    (+ x 1))

(define (identity x)
    x)

(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    
    (define (simpson-rule cur)
        (f (+ a (* cur h))))
        
    (define (simpson-term cur)
        (cond ((or (= cur 0) ; First and last term
                   (= cur n)) (simpson-rule cur))
              ((even? cur) (* 2 (simpson-rule cur)))
              (else (* 4 (simpson-rule cur)))))

    (* (/ h 3) (sum simpson-term 0 inc n)))

; Compute integral using simpson rule
(display (simpson-integral cube 0.0 1.0 100))
(newline)
(display (simpson-integral cube 0.0 1.0 1000))
