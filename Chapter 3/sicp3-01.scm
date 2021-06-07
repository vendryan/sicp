(define (print line)
  (display line)
  (newline))

(define (make-accumulator init)
  (lambda (add-to-init)
    (set! init (+ init add-to-init))
    init))

(define x (make-accumulator 5))
(print (x 5))
(print (x 6))
