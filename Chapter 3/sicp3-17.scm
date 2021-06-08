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

(define (count-pairs y)
  (let ((traversed (list '())))
    (define (count-pairs-2 x)
      (cond ((not (pair? x)) 0)
            ((exist? x traversed) 0) ; Check if x already traversed or not
            (else (append! traversed (list x)) ; Need to be (list x), took me so long to figure it out or else traverse and x will merge
                  (+ (count-pairs-2 (car x))
                     (count-pairs-2 (cdr x))
                     1))))
    (count-pairs-2 y)))

(define a '(a))
(define x (cons 'a '()))
(define y (cons x x)) 
(define str2 (cons y y))
(print (count-pairs str2))
