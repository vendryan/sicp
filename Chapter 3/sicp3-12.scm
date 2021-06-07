(define (print line)
  (display line)
  (newline))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(print z)
(print (cdr x)) ; Box and pointer in my book
; the response is (b) (not mutated yet)

(define w (append! x y))
(print w)

(print (cdr x)) ; Box and pointer in my book
; the response is (b c d) (mutated)
