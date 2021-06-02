; If record is binary tree
(define (lookup given-key record)
  (cond ((null? record) #f)
        ((= given-key (value record)) #t)
        ((> given-key (value record))
          (look-up given-key (right-branch record)))
        ((> given-key (value record))
          (look-up given-key (left-branch record)))))
