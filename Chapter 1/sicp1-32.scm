; Recursive
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))

(display (accumulate + 
                     0 
                     (lambda (x) x)
                     1
                     (lambda (x) (+ x 1))
                     5))
(newline)
(display (accumulate * 
                     1 
                     (lambda (x) x)
                     1
                     (lambda (x) (+ x 1))
                     5))

; Iterarive
(define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))))
    (iter a null-value))

(newline)
(display (accumulate-iter + 
                          0 
                          (lambda (x) x)
                          1
                          (lambda (x) (+ x 1))
                          5))
(newline)
(display (accumulate-iter * 
                          1 
                          (lambda (x) x)
                          1
                          (lambda (x) (+ x 1))
                     5))
