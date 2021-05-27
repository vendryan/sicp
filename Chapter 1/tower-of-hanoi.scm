(define nil '())
(define (print line line2)
    (display "Move width ")
    (display line)
    (display " to ")
    (display line2)
    (newline))

; I don't really comprehend it but cool
(define (move n from to spare)
    (cond ((> n 0) (move (- n 1) from spare to)
                   (print n to)
                   (move (- n 1) spare to from))))

(move 4 1 3 2)
