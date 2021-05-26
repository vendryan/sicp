(define nil '())
(define test (list 1 2 3 4 5 9))
(define test2 (list 4 5 9 3 7))
(define test3 (list (list 2 3 (list 4 5)) 2 4 6))
(define test4 (list 12 (list 3 4)))

(define (print line)
    (display line)
    (newline))

; cool
(define (fringe items)
    (cond ((null? items) nil)
          ((not (pair? items)) (list items))
          (else (append (fringe (car items))
                        (fringe (cdr items))))))

(print (fringe test4))
(print (fringe test3))
