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

(print (car ''abracadabra))
; the same as below maybe and hence Eva Lu Ator
(print (car '(quote abcracadabra)))
; see quote as the answer

(print (list 'car (list 'quote '(a b c))))
(print (list 'quote '(a b c)))
(print '(list 'a))
