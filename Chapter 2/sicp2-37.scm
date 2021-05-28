(define nil '())
(define (print line)
    (display line)
    (newline))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product x v))
         m))
(define (transpose mat)
    (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (x)
                 (map (lambda (y) (dot-product x y))
                      cols))
             m)))
; MATRIX
; _     _
;| 1 2 3 |
;| 4 5 6 |
;-      -
(define matrix (list (list 1 2 3) (list 4 5 6)))
(define matrix2 (transpose matrix))
; vector ( 1 2 3)
(define vector (list 1 2 3))

(print (dot-product vector vector))
(print (matrix-*-vector matrix
                        vector))
(print (transpose matrix))
(print (matrix-*-matrix matrix matrix2))
