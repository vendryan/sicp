(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1)
                                  (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok)
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1)
                                 (get-signal a2)))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder wire-list-a wire-list-b wire-list-s carry)
  (if (null? wire-list-a)
      'ok
      (let ((a (car wire-list-a))
            (b (car wire-list-b))
            (s (car wire-list-s))
            (c-out (make-wire)))
        (full-adder a b carry s c-out)
        (ripple-carry-adder (cdr wire-list-a)
                            (cdr wire-list-b)
                            (cdr wire-list-s)
                            c-out))))
;; Let orx be the or gate delay, andx be and gate delay and inx be inverter delay
;; The half adder delay is 2andx + 1orx + 1inx
;; In full adder there is 2 half adder and one or gate and thus
;; 1 full adder delay is 2(half-adder-delay) + orx
;; 4andx + 2orx + 2inx + orx = 4andx + 3orx + 2inx
;; And ripple carry adder if there is two n bit number is
;; n  * (full-adder-dealy)
;;
