(define (print line)
  (display line)
  (newline))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (exist? x y)
  (if (null? y)
      #f
      (or (eq? x (car y))
          (exist? x (cdr y)))))

(define (check-cycle pairs)
  (let ((traversed (list '())))
    (define (check-cycle-2 pairs)
      (cond ((null? pairs)
              (set! traversed (list '()))
              #f)
            ((or (not (pair? pairs))
                 (null? (car pairs))) 
              #f)
            ((exist? pairs traversed) #t)
            (else
              (append! traversed pairs)
              (if (eq? (car pairs) (cdr pairs))
                       (check-cycle-2 (car pairs))
                       (or (check-cycle-2 (car pairs))
                           (check-cycle-2 (cdr pairs)))))))
    (check-cycle-2 pairs)))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(print (check-cycle z))
