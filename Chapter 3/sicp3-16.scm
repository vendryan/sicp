(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(display (count-pairs (list 'a 'b 'c)))

; 4
(define x '(foo)) 
(define y (cons x x)) 
(define str2 (list y)) 
 
(newline)
(display (count-pairs str2))
 
; Never return
(define t (list 'a 'b 'c))
(set-cdr! (cddr t) t)
(newline)
;(display (count-pairs t))

; 7 
(define a '(a))
(define x (cons 'a '()))
(define y (cons x x)) 
(define str2 (cons y y))
(display (count-pairs str2))
