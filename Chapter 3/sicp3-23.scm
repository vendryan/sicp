(define (print line)
  (display line)
  (newline))

; SELECTOR
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) 
  (set-car! deque item))
(define (set-rear-ptr! deque item) 
  (set-cdr! deque item))
(define (empty-deque? deque)
  (null? (front-ptr deque)))
(define (front-deque deque)
  (caar (front-ptr deque)))
(define (rear-deque deque)
  (caar (rear-ptr deque)))
(define (deque-item-pair deque)
  (car (front-ptr deque)))

; CONSTRUCTOR
(define (make-deque) (cons '() '()))

; MUTATOR
(define (front-deque-insert! deque item)
  (let ((pointer-pair (cons '() '())))
    (let ((new-pair (cons item '())))
      (cond ((empty-deque? deque)
              (set-car! pointer-pair new-pair)
              (set-front-ptr! deque pointer-pair)
              (set-rear-ptr! deque pointer-pair))
            (else
              (set-cdr! (deque-item-pair deque) pointer-pair)
              (set-cdr! pointer-pair (front-ptr deque))
              (set-car! pointer-pair new-pair)
              (set-front-ptr! deque pointer-pair))))))



(define d1 (make-deque))
(front-deque-insert! d1 'a)
(print d1)
(front-deque-insert! d1 'b)
(print d1)
(print (front-deque d1))
(print (rear-deque d1))
