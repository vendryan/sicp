(define (print line)
  (display line)
  (newline))

(define x (list 'a 'b))
(define y (list 'c 'd))

(set-cdr! x y)

(print x)
