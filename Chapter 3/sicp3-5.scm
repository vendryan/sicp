; Assume by wishful thinking that we have random procedure
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (inside-unit-circle? x y)
  (<= (+ (square (- x 1.0)) (square (- y 1.0))) (square 1.0)))

(define (estimate-integral predicate x1 x2 y1 y2 trial)
  (define (iter trial-performed result)
    (cond ((> trial-performed trial) (/ result trial))
          ((predicate (random-in-range x1 x2)
                      (random-in-range y1 y2))
            (iter (+ trial-performed 1) (+ result 1)))
          (else (iter (+ trial-performed 1) result))))
  (iter 0 0))

(display (inside-unit-circle? 1 1))
