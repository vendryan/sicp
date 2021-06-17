(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define (integrate-series s)
; elegant approach: (stream-map / s integers))
;  (define (integrate s n)
;    (cons-stream (stream-car s)
;                 (stream-map (lambda (x) (* x (/ 1 n)))
;                             (integrate s (+ n 1)))))
  (define (integrate s n)
    (cons-stream (/ (stream-car s) n)
                 (integrate (stream-cdr s) (+ n 1))))
  (integrate s 1))

(define (partial-sum s)
  (define ps (cons-stream (stream-car s)
                          (stream-map + (stream-cdr s)
                                      ps)))
  ps)

(define sine-series
  (cons-stream 0.0 (integrate-series cosine-series)))
(define cosine-series
  (cons-stream 1.0 (integrate-series (stream-map - sine-series))))
