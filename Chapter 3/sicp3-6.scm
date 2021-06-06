(define rand
  (let ((init 0))
    (lambda (req)
      (cond ((eq? req 'generate)
              (set! init (random-init init))
              init)
            ((eq? req 'reset)
              (lambda (new) (set! init (random-init new))
                            init))
            (else (error "Unknown request -- RAND" req))))))

(rand 'test)
