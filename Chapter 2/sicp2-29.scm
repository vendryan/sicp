(define nil '())
(define (print line)
    (display line)
    (newline))
(define (make-mobile left right)
    (list left right))
(define (make-branch length structure)
    (list length structure))
(define (left-branch mobile)
    (car mobile))
(define (right-branch mobile)
    (cadr mobile))
(define (branch-length branch)
    (car branch))
(define (branch-structure branch)
    (cadr branch))
(define (total-weight mobile)
    (cond ; ((null? mobile) 0) (this is not necessary as we never encounter null value)
          ((not (pair? mobile)) mobile) ; means its a number
          (else (+ (total-weight (branch-structure (left-branch mobile)))
                   (total-weight (branch-structure (right-branch mobile)))))))

(define branch1 (make-branch 5 10))
(define branch2 (make-branch 3 4))
(define mobile1 (make-mobile branch1 branch2))
(define branch3 (make-branch 5 7))
(define branch4 (make-branch 3 mobile1))
(define mobile2 (make-mobile branch3 branch4))

(print mobile2)
(print (total-weight mobile2)) ; expected 21 and it's really produce 21
