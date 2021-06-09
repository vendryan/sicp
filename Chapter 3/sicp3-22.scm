(define (print line)
  (display line)
  (newline))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr  '())
    (define (empty-queue?)
      (null? front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'front-queue)
              (if (empty-queue?) 
                  (error "FRONT queue is empty" front-ptr)
                  (car front-ptr)))
            ((eq? m 'insert!)
              (cond 
    dispatch))
