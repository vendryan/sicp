; Assume by wishful thinking that we have random procedure
; (Actually exist as primitive in MIT Scheme but hard to compile)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (inside-unit-circle? x y)
  (<= (+ (square (- x 1.0)) (square (- y 1.0))) (square 1.0)))

(define (estimate-integral predicate x1 x2 y1 y2 trial)
  (define (iter trial-remaining passed)
    (cond ((= trial-remaining 0) (/ passed trial))
          ((predicate (random-in-range x1 x2)
                      (random-in-range y1 y2))
            (iter (- trial-remaining 1) (+ passed 1)))
          (else (iter (- trial-remaining 1) passed))))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (* area (iter trial 0))))

; Estimate pi using monte-carlo simulation
(newline)
(display (estimate-integral inside-unit-circle? 0.0 2.0 0.0 2.0 10))
(newline)
(display (estimate-integral inside-unit-circle? 0.0 2.0 0.0 2.0 100))
(newline)
(display (estimate-integral inside-unit-circle? 0.0 2.0 0.0 2.0 1000))
(newline)
(display (estimate-integral inside-unit-circle? 0.0 2.0 0.0 2.0 10000))

; (load "sicp3-5.scm")
; Output 1
; 2.4
; 3.52
; 3.196
; 3.1232

; Output 2
; 2.8
; 3.28
; 3.1
; 3.1308
; The more we make the trial, the closer it is to pi
