(define mt (list 4 (list 5 7) 2))
(define (leaf? x)
  (not (pair? x)))
(define (count-leaves t)
  (cond
   ((null? t) 0)
   ((leaf? t) 1)
   (else
    (+ (count-leaves (car t)) (count-leaves (cdr t))))))
