(define (attach-tag tag-type content)
  (cons tag-type content))
(define (tag-type tagged-item)
  (car tagged-item))
(define (contents tagged-item)
  (cdr tagged-item))
(define (deriv exp var)
  (define (helper exp)
    (cond ((null? exp) '())
          ((number? (car exp)) (helper (cdr exp)))
          ((variable? (car exp)) (append (if (same-variable? exp var) 1 0)
                                         (helper (cdr exp))))
          (else (append (deriv (car exp) var)
                        (helper (cdr exp))))))
    (append '+ (helper exp)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a
; Because number? and variable? work for both
; + and * operator

(define (install-plus-deriv)
  (define (deriv-+ exp var)
    (define (helper exp)
      (cond ((null? exp) '())
            ((number? (car exp)) (helper (cdr exp)))
            ((variable? (car exp)) (append (if (same-variable? exp var) '(1) '(0))
                                           (helper (cdr exp))))
            (else (append (deriv (car exp) var)
                          (helper (cdr exp))))))
    (append '(+) (helper exp)))
  
  (put 'deriv '+ deriv-+))

(define (install-mul-deriv)
  ; Derive each part of the exp
  ; (uvw)' = u'vw + u(vw)'
  (define (deriv-* exp var)
    (define (helper exp)
      (cond ((null? exp) '())
            (else (append (list (append '(* (deriv (car exp))
                                         (cdr exp))))
                          
    (append '(+) (helper exp)))
  
  (put 'deriv '* deriv-*))
  
(display (append '(+) '(5)))
  
