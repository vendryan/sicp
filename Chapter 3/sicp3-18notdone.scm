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
  (let ((traversed '()))
    (define (append!-if-not-null x)
      (if (null? traversed)
          (set! traversed (list x))
          (append! traversed (list x))))
    
    (define (check-cycle-2 pairs)
      (cond ((null? pairs) ; if found the null cdr then reset the traversed and is false
              (set! traversed '())
              #f)
            ((or (not (pair? pairs)) ; if not a pair and
                 (null? (car pairs))) ; car is null then its not a cycle
              #f)
            ((or (exist? pairs traversed) ; if current pairs exist in traversed and
                 (exist? (cdr pairs) traversed)) ; cdr exist in traversed then its a cycle
              #t)
            (else
              (append!-if-not-null pairs) ; if null set to current pairs else append
              (if (eq? (car pairs) (cdr pairs)) ; if car and cdr are equal then check either one
                       (check-cycle-2 (car pairs))
                       (or (check-cycle-2 (car pairs)) ; else check both
                           (check-cycle-2 (cdr pairs)))))))
    (check-cycle-2 pairs)))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(print (check-cycle z))
(print (check-cycle (list 1 2 3)))
