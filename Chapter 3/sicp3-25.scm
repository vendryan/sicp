(define (make-table)
  (let ((local-table (list '*table*))
        (number-of-key 0))
    (define (empty-table?)
      (null? (cdr local-table)))
    (define (wrong-key-number? key-list)
      (not (= number-of-key (length key-list))))
    
    (define (assoc record key)
      (cond ((null? record) #f)
            ((equal? key (caar record)) (car record))
            (else (assoc (cdr record) key))))
    
    ; This is damn hard took me so long to test and
    ; figure the pattern
    (define (table-pattern value key-list)
      (if (null? (cdr key-list))
          (cons (car key-list) value)
          (list (car key-list)
                (table-pattern value (cdr key-list)))))
    
    (define (lookup table . key-list) 
      (define (lookup-2 table key-list)
        (let ((record (assoc (cdr table) (car key-list))))
          (cond ((record)
                  (if (null? (cdr key-list)) ; if cdr is null
                      (cdr record) ; then cdr of record is the value and thus get the cdr of record
                      (lookup-2 record (cdr key-list))))
                (else #f))))
      ;; Error checking first
      (cond ((empty-table?)
              (error "TABLE is empty -- " local-table))
            ((wrong-key-number? key-list)
              (error "WRONG number of KEY -- " key-list))
            (else (lookup-2 table key-list))))
    (define (insert! table value . key-list)
      (define (insert-2! table value key-list)
        (let ((record (assoc (cdr table) (car key-list))))
          (if (record)
              (if (null? (cdr key-list)) ; Means its found and its
                  (set-cdr! record value) ; the last key
                  (insert-2! record value (cdr key-list))
              (set-cdr! table (cons (table-pattern value key-list) ;; cons the kay table pettaern with cdr of table
                                    (cdr table))))))
      
      ;; Error checking
      (cond ((empty-table?)
              (set! number-of-key (length key-list))
              (insert! table value key-list))
            ((wrong-key-number? key-list)
              (error "WRONG number of KEY -- " key-list))
            (else (insert! table value key-list))))
      
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert)
            ((eq? m 'number-of-key) number-of-key)
            (else (error "UNKNOWN request -- " m))))
    dispatch))
