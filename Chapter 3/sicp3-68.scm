(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

; It goes to infinite loops because Scheme is using
; Applicative order evaluation
; when pairs is called the interleave will be called
; and then in turn pairs will be called and then
; in turn interleave will be called and so on. There's 
; no delay and thus into infinite loop

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
