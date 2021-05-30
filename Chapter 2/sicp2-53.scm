(define nil '())
(define (print line)
    (display line)
    (newline))
(define (memq item list-item)
    (cond ((null? list-item) #f)
          ((eq? item (car list-item)) list-item)
          (else (memq item (cdr list-item)))))

(print (list 'a 'b 'c)) ; (a b c)
(print (list (list 'george))) ; ((george))
(print (cdr '((x1 x2) (y1 y2)))) ; ((y1 y2))
(print (cadr '((x1 x2) (y1 y2)))) ; (y1 y2)
(print (pair? (car '(a short list)))) ; False
(print (memq 'red '((red shoes) (blue socks)))) ; False
(print (memq 'red '(red shoes blue socks)))
; (red shoes blue socks)
