; Stream procedure in a-stream.scm github
; Too big

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones
                                             integers)))

(define factorial (cons-stream 1 
                               (mul-streams factorial
                                            (stream-cdr integers))))

(define (partial-sums s)
  (define partial (cons-stream (stream-car s)
                               (add-streams partial
                                            (stream-cdr s))))
  partial)

(print (stream-ref (partial-sums integers) 3))
