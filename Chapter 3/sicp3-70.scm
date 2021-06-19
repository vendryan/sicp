(define (merge weight-proc s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((x1 (stream-car s1))
                (x2 (stream-car s2)))
            (cond ((> (weight-proc x1) (weight-proc x2))
                    (cons-stream x2
                                 (merge weight-proc
                                        s1
                                        (stream-cdr s2))))
                  ((> (weight-proc x1) (weight-proc x2))
                    (cons-stream x1
                                 (merge weight-proc
                                        (stream-cdr s1)
                                        s2)))
                  (else (cons-stream x1
                                 (merge weight-proc
                                        (stream-cdr s1)
                                        s2))))))))
                                     

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge
    (lambda (x) (+ (car x) (cadr x)))
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(display-top-n (pairs integers integers) 100)
