(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (fixed-point f guess)
    (define (close-enough? a b)
        ((lambda (tolerance) (< (abs (- a b)) tolerance))
                 0.0001)) 
    (define (fixed-point-search guess)
        (let ((next (f guess)))
             (newline)
             (display guess)
             (newline)
             (display next)
             (newline)
             (if (close-enough? guess next)
                 ((lambda (x) 
                      (newline)
                      (display "***")
                      (newline)
                      (display x)) next)
                 (fixed-point-search next))))
    (fixed-point-search guess))

; average damping ans : 4.5555465521473675
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 
             2.0)
(newline) (newline)

; without average damping : 4.555563237292884
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)
         
; My conclusion : Not much difference, both roughly converge to
; the same result. The number of step for average damping is smaller
; then step without average damping
