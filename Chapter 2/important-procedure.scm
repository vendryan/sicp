(define nil '())
(define (print line)
    (display line)
    (newline))

(define (map proc sequence)
    (if (null? items)
        nil
        (cons (proc (car sequence))
              (map proc (cdr sequence)))))

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence)) 
            (cons (car sequence)
                (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (tree-enumerate tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (tree-enumerate (car tree))
                        (tree-enumerate (cdr tree))))))

; Generate list of given interval
(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low 
              (enumerate-interval (+ low 1) high))))
    
(print (filter (lambda (x) (= x 0))
               (list 0 2 3 4 0)))

(print (tree-enumerate (list 3 4 (list 4 5) (list 6 7))))

(print (enumerate-interval 0 6))
