(define nil '())
(define (print line)
    (display line)
    (newline))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 
              0 
              (map (lambda (x) (cond ((null? x) 0)
                                     ((pair? x) 
                                        (count-leaves x))
                                     (else 1)))
                   t)))

; should be 9
(print (count-leaves (list 1 2 (list 3 4 (list 5 6) 5) 3 5)))
