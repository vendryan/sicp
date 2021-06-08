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
            (else (append! traversed (list x)) ; Need to be (list x), took me so long to figure it out or else traverse and x will merge. 
                  (+ (count-pairs-2 (car x)) ; Append the current x to traversed
                     (count-pairs-2 (cdr x))
                     1))))
    (count-pairs-2 y)))

(define a '(a))
(define x (cons 'a '()))
(define y (cons x x)) 
(define str2 (cons y y))
(print (count-pairs str2)) ; Formerly 7

(define x '(foo)) 
(define y (cons x x)) 
(define str2 (list y)) 
(print (count-pairs str2)) ; Formerly 4
 
; Never return
(define t (list 'a 'b 'c))
(set-cdr! (cddr t) t)
(print (count-pairs t)) ; Formerly never return

(define last-test (list 'a 'b 'c))
(print (count-pairs last-test))

; Extreme case
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(print (count-pairs z1)) ; 3
(print (count-pairs z2)) ; 5 this is correct
