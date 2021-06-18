(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
       (stream-map (lambda (x) (list x (stream-car s)))
                   (stream-cdr t))
       (stream-map (lambda (x) (list (stream-car s) x))
                   (stream-cdr t))
       (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2 s3)
  (cond ((and (stream-null? s1) (stream-null? s2)) s3)
        ((and (stream-null? s2) (stream-null? s3)) s1)
        ((and (stream-null? s1) (stream-null? s3)) s2)
        (else (cons-stream (stream-car s1)
                           (interleave s2 s3 (stream-cdr s1))))))

(display-top-n (pairs integers integers) 1000)
