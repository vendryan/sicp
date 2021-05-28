(define nil '())
(define (print line)
    (display line)
    (newline))

(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))))
;(define (accumulate-n op init seqs)
;    (if (null? (car seqs))
;        nil
;        (cons (accumulate op init (map car seqs))
;              (accumulate-n op init (map cdr seqs)))))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

(print (fold-right / 1 (list 1 2 3 4)))
(print (fold-left (lambda (x y) (/ y x)) 1 (list 1 2 3 4)))
(print (fold-right list nil (list 1 2 3)))
(print (fold-left (lambda (x y) (list y x)) nil (list 1 2 3)))

(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) nil sequence))

(print (reverse (list 1 2 3 4)))

(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) nil sequence))

(print (reverse (list 1 2 3 4)))
