(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define (force delayed-proc)
  (delayed-proc))
(define the-empty-stream '())
(define (stream-null? s)
  (null? s))

(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))
(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter pred
                                      (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc . argstream)
  (if (stream-null? (car argstream))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstream))
        (apply stream-map
               (cons proc (map stream-cdr
                               argstream))))))

(define (display-top10 s)
  (display-top-n s 10))

(define (display-top-n s times)
    (if (= times 0)
        'done
        (begin (print (stream-car s))
                      (display-top-n (stream-cdr s) (- times 1)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each print s))

(define (print . x)
  (map (lambda (x) (display x) (newline))
       x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (integrate-series s)
; elegant approach: (stream-map / s integers))
;  (define (integrate s n)
;    (cons-stream (stream-car s)
;                 (stream-map (lambda (x) (* x (/ 1 n)))
;                             (integrate s (+ n 1)))))
  (define (integrate s n)
    (cons-stream (/ (stream-car s) n)
                 (integrate (stream-cdr s) (+ n 1))))
  (integrate s 1))

(define (partial-sum s)
  (cons-stream (stream-car s)
               (stream-map + (stream-cdr s)
                             (partial-sum s))))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))
