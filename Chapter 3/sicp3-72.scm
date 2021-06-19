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

; Tweaked this so display what pair that fulfill
; not the value result
(define (consecutive-value s weight times)
  (define (iter s val how-many accumulated)
    (cond ((stream-null? s) the-empty-stream)
          ((null? val) (iter (stream-cdr s) 
                             (weight (stream-car s))
                             (+ how-many 1)
                             (cons (stream-car s) accumulated)))
          ((= how-many times)
            (cons-stream accumulated
                         (iter (stream-cdr s)
                               val
                               0
                               '())))
          ((not (= val (weight (stream-car s))))
            (iter (stream-cdr s)
                  (weight (stream-car s))
                  1
                  (cons (stream-car s) '())))
          (else (iter (stream-cdr s)
                      val
                      (+ how-many 1)
                      (cons (stream-car s) accumulated)))))
  (iter s '() 0 '()))

(define (cube x)
  (* x x x))

(define (weight-proc x)
  (+ (cube (car x))
     (cube (cadr x))))

(define (sum-square x)
  (+ (square (car x))
     (square (cadr x))))
  
(define weighted-cube-pair (pairs integers integers weight-proc))
(define ramanujan-number (consecutive-value weighted-cube-pair
                                            weight-proc
                                            2))
(define three-consecutive-sum-square
  (consecutive-value (pairs integers integers sum-square)
                     sum-square
                     3))
  
(display-top10 three-consecutive-sum-square)
; ((10 15) (6 17) (1 18))
; ((13 16) (8 19) (5 20))
; ((17 19) (11 23) (5 25))
; ((14 23) (10 25) (7 26))
; ((19 22) (13 26) (2 29))
; ((15 25) (11 27) (3 29))
; ((21 22) (14 27) (5 30))
; ((20 25) (8 31) (1 32))
; ((12 31) (9 32) (4 33))
; ((25 25) (17 31) (5 35))
