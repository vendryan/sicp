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

(print (fold-right / 1 (list 2 3 4 6)))
(print (fold-left (lambda (x y) (/ y x)) 1 (list 2 3 4 6)))
(print (fold-right list nil (list 1 2 3)))
(print (fold-left (lambda (x y) (list y x)) nil (list 1 2 3)))

; Condition that guarantee fold-right and fold-left 
; produce the same result is we need to reverse the 
; either of the sequence and reverse the operation of the op maybe???

; I think too hard, the answer is just one word
; COMMMUTATIVE operation will produce same result
; that cross my mind but i keep thinking the way for
; not cummutative operation to produce the
; same result. Why i didn't think of that
; i already think "hey + and * same result if commutative it's the same"
; but i didn't think commutative as the answer...
