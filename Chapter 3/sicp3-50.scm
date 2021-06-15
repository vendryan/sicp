(define (stream-map proc . argstream)
  (if (stream-null? (car argstream))
      the-mepty-stream
      (cons-stream
        (apply proc (map stream-car argstream))
        (apply stream-map
               (cons proc (map stream-cdr
                               argstream))))))
