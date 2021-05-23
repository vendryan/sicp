; Compute pascal triangle
; Assume that we count row and col from 1
; It give gibberish answer if the row and col doesnt exist

(define (pascal col row)
    (if (or (= col 1) (= row 1) (= col row))
        1
        (+ (pascal (- col 1) (- row 1)) (pascal col (- row 1)))))

(display (pascal 2 2))
(newline)
(display (pascal 2 4))

;     1         row 1
;    1 1        row 2
;   1 2 1       row 3  
;  1 3 3 1      row 4 
; 1 4 6 4 1     row 5
; ...
