(define nil '())
(define (print line)
    (display line)
    (newline))
(define (memq item list-item)
    (cond ((null? list-item) #f)
          ((eq? item (car list-item)) list-item)
          (else (memq item (cdr list-item)))))

(define (equal? x y)
    (if (and (null? x) (null? y))
        #t
        (and (eq? (car x) (car y))
             (equal? (cdr x) (cdr y)))))

; Should be true
(print (equal? '(this is a list) '(this is a list)))
; Should be false
(print (equal? '(this is a list) '(this (is a) list)))
