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
(define (empty-stream? s)
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
  (cond ((empty-stream? stream) the-empty-stream)
        ((pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter pred
                                      (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

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

(print (stream-enumerate-interval 1 6))
