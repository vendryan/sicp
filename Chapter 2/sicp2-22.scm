(define nil '())
(define test (list 1 2 3 4 5))

; Print
(define (print x)
    (display x)
    (newline))

; Transform the list with given procedure
(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))
; Reversed because each time it cons the result, 
; The frist item is the first one so (cons item1 nil)
; Then the second one become (cons item2 (cons item1 nil))
; And thus it's reversed

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
; We can find the error by substitution modeland
; going through each iteration
; 1: (cons nil (square (car item1)))
; 2: (cons (cons nil (square (car item1))) (square (car item2)))
; Here we see that the cons become complicated and not supposed
; to work this way
