(define (invert-unit-series series)
  (define x (cons-stream 1
                         (mul-series (stream-scale (stream-cdr series) -1)
                                     x)))
  x)
