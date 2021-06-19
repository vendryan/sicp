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
    (pairs (stream-cdr s) (stream-cdr t) weight))))

(define (consecutive-value s weight times)
  (define (iter s val how-many)
    (cond ((stream-null? s) the-empty-stream)
          ((null? val) (iter (stream-cdr s) 
                             (weight (stream-car s))
                             (+ how-many 1)))
          ((= how-many times)
            (cons-stream val
                         (iter (stream-cdr s)
                               val
                               0)))
          ((not (= val (weight (stream-car s))))
            (iter (stream-cdr s)
                  (weight (stream-car s))
                  1))
          (else (iter (stream-cdr s)
                      val
                      (+ how-many 1)))))
  (iter s '() 0))

(define (cube x)
  (* x x x))

(define (weight-proc x)
  (+ (cube (car x))
     (cube (cadr x))))
  
(define weighted-cube-pair (pairs integers integers weight-proc))
(define ramanujan-number (consecutive-value weighted-cube-pair
                                            weight-proc
                                            2))

(display-top10 ramanujan-number)
; 1729 first ramanujan

; next five
; 4104
; 13832
; 20683
; 32832
; 39312
