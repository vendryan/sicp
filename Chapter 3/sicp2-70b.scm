(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

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
                                     
(define (pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge
    weight
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (divisible-by2-3-5? item)
  (or (divisible? item 2) (divisible? item 3) (divisible? item 5)))

(define strange-pairs (stream-filter 
                        (lambda (x) (and (not (divisible-by2-3-5? (car x)))
                                         (not (divisible-by2-3-5? (cadr x)))))
                        (pairs integers integers (lambda (x)
                                                   (+ (* 2 (car x))
                                                      (* 3 (cadr x))
                                                      (* 5 (car x) (cadr x)))))))
