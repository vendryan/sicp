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
            ((not (pair? pairs)) ; if not a pair and
              #f)
            ((or (exist? pairs traversed) ; if current pairs exist in traversed and
                 (exist? (cdr pairs) traversed)) ; cdr exist in traversed then its a cycle
              #t)
            ((null? (car pairs)) ; if car is null and cdr not in traversed then reset the traversed and is false
             (set! traversed '())
              #f)
            (else
              (append!-if-not-null pairs)
              (if (eq? (car pairs) (cdr pairs)) ; if car and cdr are equal then check either one
                       (check-cycle-2 (car pairs))
                       (or (check-cycle-2 (car pairs))
                           (check-cycle-2 (cdr pairs)))))))
    (check-cycle-2 pairs)))

; Functional way
; (actually search the idea on internet cause my solution so complex)
; (no need to use assignment i need to remember that)
; (using assignment become so complicated)
(define (check-cycle pairs)
  (define (check-cycle-2 pairs traversed)
    (cond ((not (pair? pairs)) #f)
          ((or (exist? pairs traversed) 
               (exist? (cdr pairs) traversed))
            #t)
          ((null? (car pairs)) #f)
          (else
            (if (eq? (car pairs) (cdr pairs))
                     (check-cycle-2 (car pairs) (cons pairs traversed))
                     (or (check-cycle-2 (car pairs) (cons pairs traversed))
                         (check-cycle-2 (cdr pairs) (cons pairs traversed)))))))
  (check-cycle-2 pairs '()))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(print (check-cycle z))
(print (check-cycle (list 1 2 3)))

(define x (list 'a '()))
(define y (list 'b x))
(define z (list x y))

; Extreme case
(print (check-cycle z))
