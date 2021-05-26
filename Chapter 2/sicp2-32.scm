(define nil '())
(define test (list 1 2 3))
(define test2 (list 6 7 8 9 10))
(define (print line)
    (display line)
    (newline))

; It work because at each level of recursion it append the
; lower level with the current car value to the list and
; thus produce all the possible combination
(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
             (append rest (map (lambda (x) 
                                   (append (list (car s)) x)) 
                               rest)))))

(print (subsets test))
(print (subsets test2))
