(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (stream-limit s tolerance)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s0 s1)) tolerance)
        s1
        (stream-limit (stream-cdr s) tolerance))))

(define (ln-series n)
  (cons-stream (/ 1 n)
               (stream-map - (ln-series (+ n 1)))))

(define ln-2 (partial-sum (ln-series 1.0)))

(display-top10 (accelerated-sequence euler-transform ln-2))

; in my opinion converge slower than pi
