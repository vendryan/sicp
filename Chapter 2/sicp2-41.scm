(define nil '())
(define (print line)
    (display line)
    (newline))

(define (prime? n)
    (define (divides? a b)
        (= (remainder b a) 0))
    (define (find-divisor test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor (+ test-divisor 1)))))
    (define (smallest-divisor x)
        (find-divisor 2))
    (= (smallest-divisor n) n))

(define (map proc sequence)
    (if (null? sequence)
        nil
        (cons (proc (car sequence))
              (map proc (cdr sequence)))))

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence)) 
            (cons (car sequence)
                (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (tree-enumerate tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (tree-enumerate (car tree))
                        (tree-enumerate (cdr tree))))))

; Generate list of given interval
(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low 
              (enumerate-interval (+ low 1) high))))

(define (flatmap proc sequence)
    (accumulate append nil (map proc sequence)))

; Generate unique pairs
(define (unique-pairs n)
    (flatmap (lambda (x) 
                 (map (lambda (y) (list y x)) 
                      (enumerate-interval 1 (- x 1))))
             (enumerate-interval 1 n)))
         
; for each number x <= n, generate the unique
; pairs of x-1 and append it with x
(define (unique-triple n)
    (flatmap (lambda (x)
                 (map (lambda (y) (append y (list x)))
                      (unique-pairs (- x 1))))
             (enumerate-interval 1 n)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
(define (make-sum-pair pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; prime-sum-pair in term of unique-pairs
(define (prime-sum-pair n)
    (map make-sum-pair
         (filter prime-sum?
                 (unique-pairs n))))

(print (unique-triple 4))
(print (unique-pairs 1))
