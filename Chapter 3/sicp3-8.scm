(define f
  (let ((x 1))
    (lambda (y)
      (cond ((eq? y 'reset) (set! x 1))
            ((or (= y 0) (= x 0))
              (set! x 0) x)
            (else x)))))

; Evaluate (+ (f 0) (f 1)) from left to right
; and right to left

; Left to right
(display (+ (f 0) (f 1))) ; 0
(newline)

; Reset to former form
(f 'reset)

; Right to left
(display (+ (f 1) (f 0)))
