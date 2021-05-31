(define nil '())
(define (print line)
    (display line)
    (newline))

(define (=number? x y)
  (and (number? x) (number? y) (= x y)))
(define (length item)
  (if (null? item)
      0
      (+ 1 (length (cdr item)))))

(define (variable? x) (symbol? x))
(define (same-variable? x1 x2)
  (and (variable? x1) (variable? x2) (eq? x1 x2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
          (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
          (* m1 m2))
        (else (list m1 '* m2))))
(define (make-exponent b p)
  (cond ((= p 1) b)
        ((= p 0) 1)
        (else (list b '** p))))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (exponent? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (addend x)
  (car x))
(define (augend x)
  (caddr x))
(define (multiplier x)
  (car x))
(define (multiplicand x)
  (caddr x))
(define (base x)
  (car x))
(define (power x)
  (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponent? exp)
         (make-product 
            (make-product (power exp) 
                          (make-exponent (base exp) 
                                         (- (power exp) 1)))
            (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(print (deriv '(x + x) 'x))
(print (deriv '(x * x) 'x))
;(print (deriv '(+ (* 3 (* x x)) (* 2 x)) 'x))
;(print (deriv '(** x 2) 'x))