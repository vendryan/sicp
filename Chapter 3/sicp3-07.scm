(define (print line)
  (display line)
  (newline))

(define (make-account balance real-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input-password req)
    (cond ((not (eq? input-password real-password))
            (error "Incorrect password"))
          ((eq? req 'withdraw) withdraw)
          ((eq? req 'deposit) deposit)
          ((eq? req 'balance) balance)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       req))))
  dispatch)

; Searching because this one is hard
(define (make-joint acc pass new-pass)
  ; I never thought of this trick damn it
  (define (withdraw amount) 
    ((acc pass 'withdraw) amount))
  (define (deposit amount) 
    ((acc pass 'deposit) amount))
  
  (define (dispatch input-password req)
    (cond ((not (eq? input-password new-pass))
            (error "Incorrect password"))
          ((eq? req 'withdraw) withdraw)
          ((eq? req 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       req))))
  (if (number? (deposit 0))
      dispatch
      (error "Incorrect password")))
  
(define evan-acc (make-account 500 'pro))
(define jimmy-acc (make-joint evan-acc 'pro 'sesame))

(print ((jimmy-acc 'sesame 'withdraw) 500))
(print (evan-acc 'pro 'balance))
(print ((jimmy-acc 'sesame 'deposit) 1000))
(print (evan-acc 'pro 'balance))
