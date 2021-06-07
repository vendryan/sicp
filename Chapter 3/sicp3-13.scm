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

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(print z) ; I expect (a b c a b c)
; Actual result: #0=(a b c . #0#)
; What the heck

; I get i now, thanks to box-pointer picture 
; the last cdr point to the first list again and hence it cycled infinitely

(print (last-pair z))
; Because the list is cycled it will never have the lst pair and hence gone into infinite loop 
