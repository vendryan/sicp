(define (print line)
  (display line)
  (newline))

(define (make-monitored f)
  (let ((call-num 0))
    (define (mf x)
      (cond ((eq? x 'how-many-call?) call-num)
            ((eq? x 'reset-count) (set! call-num 0))
            (else (begin (set! call-num (+ call-num 1))
                  (f x)))))
    mf))

(define s (make-monitored sqrt))

(print (s 100))
