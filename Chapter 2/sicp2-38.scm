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

(print (fold-right / 1 (list 1 2 3)))
(print (fold-left / 1 (list 1 2 3)))
(print (fold-right list nil (list 1 2 3)))
(print (fold-left list nil (list 1 2 3)))

; Condition that guarantee fold-right and fold-left 
; produc the same result is we need to reverse the 
; either of the sequence
