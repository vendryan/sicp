(define nil '())
(define test (list 1 2 3 4 5))

; Print
(define (print x)
    (display x)
    (newline))

; Transform the list with given procedure
(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

; Apply procedure to each item in the list
(define (for-each proc items)
    (define (apply-and-continue proc items)
        (proc (car items))
        (for-each proc (cdr items)))
    
    ; Very criptic
    ;(if (null? items)
    ;    #t
    ;    ((lambda (proc items) (proc (car items))
    ;                          (for-each proc (cdr items))) proc items)))
    ; More readable
    (if (null? items)
        #t
        (apply-and-continue proc items)))
    
(for-each print test)
