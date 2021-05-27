(define nil '())
(define (print line)
    (display line)
    (newline))

(define (map proc sequence)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

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
    
(print (filter (lambda (x) (= x 0))
               (list 0 2 3 4 0)))
