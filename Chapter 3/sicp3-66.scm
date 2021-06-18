(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
       (stream-map (lambda (x) (list (stream-car s) x))
                   (stream-cdr t))
       (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

; (1 1) <-- 1
; (1 2) 1
; (2 2) <-- 3
; (1 3)
; (2 3) 3
; (1 4)
; (3 3) <-- 7
; (1 5)
; (2 4)
; (1 6)
; (3 4)
; (1 7) 7
; (2 5)
; (1 8)
; (4 4) <-- 15
; (1 9)
; (2 6)
; (1 10)
; (3 5)
; (1 11)
; (2 7)
; (1 12)
; (4 5)
; (1 13) 15
; (2 8)
; (1 14)
; (3 6)
; (1 15)
; (2 9)
; (1 16)
; (5 5) <-- 31

; Let's call same-pair as (x y) where x = y
; We can see that same-pair
; Appear every 1 + 2 + 4 + ... 2^n where
; n start from 0 and inbetween them appear the
; number that is the same-pair in 1 + 2 + 4 + ... + 2^n times
; Starting from that part
; lets call Sn = 1 + 2 + 4 + ... + 2^n

; using this we can know that (1 100) appears in the
; part where Sn + 1 > 100 (+ 1 from the same-pair)

; (99 100) will appear somewhere around
; S98 and S99 which is very big and far
; as for (100 100) will appear exactly at S99

(define (expt-series b n)
  (cons-stream (expt b n)
               (expt-series b (+ n 1))))

(define Sn (partial-sum (expt-series 2 0)))
(define mystery (pairs integers integers))

; minus 1 because stream-ref counting start from zero
; we can't compute S99 because it get really big
(print (stream-ref mystery (- (stream-ref Sn 15) 1)))
(print (- (stream-ref Sn 15) 1))
