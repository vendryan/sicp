(define nil '())
(define test (list 1 2 3 4 5 9))
(define test2 (list 4 5 9 3 7))
(define test3 (list (list 2 3 (list 4 5)) 2 4 6))

(define (print line)
    (display line)
    (newline))

; man this is so hard, recursive solution i searched
; if null return nil
; otherwise if not pair return that thing
; otherwise append the reversed cdr items with reversed 
; list car items
(define (reverse-recur items)
    (cond ((null? items) nil)
          ((not (pair? items)) items)
          (else (append (reverse-recur (cdr items))
                        (list (reverse-recur (car items)))))))

; Iterative reverse given list
(define (deep-reverse items)
    (define (iter a result)
        (cond ((null? a) result)
              ((not (pair? a)) a)
              (else (iter (cdr a) 
                          (cons (deep-reverse (car a)) 
                                result)))))
    (iter items nil))

(print (deep-reverse test))
(print (deep-reverse test2))

(print test3)
(print (deep-reverse test3))
;(print (append test (list 9 10)))
(print (reverse-recur test3))
(print (reverse-recur test2))
