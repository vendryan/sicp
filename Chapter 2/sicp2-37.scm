(define nil '())
(define (print line)
    (display line)
    (newline))

(define (map proc sequence)
    (if (null? sequence)
        nil
        (cons (proc (car sequence))
              (map proc (cdr sequence)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

; Transform the sequence of sequence into one sequence where
; each nth item is combined with the op
; ex : (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
; become (12 15 18)
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

; should be (12 15 18)
(print (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9))))
