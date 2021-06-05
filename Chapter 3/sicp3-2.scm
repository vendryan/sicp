(define (print line)
  (display line)
  (newline))

(define (make-monitored f)
  (let ((call-num 0))
    (lambda (x)
      (if (eq? x 'how-many-call?)
          call-num
          (begin (set! call-num (+ call-num 1))
                 (f x))))))

(define s (make-monitored sqrt))

(print (s 100))

(print (s 100))
(print (s 100))
(print (s 100))
(print (s 'how-many-call?))
