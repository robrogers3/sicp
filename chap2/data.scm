(define (adjoin x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (first z) (z 0))

(define (rest z) (z 1))

(define (rons x y)
  (lambda (m) (m x y)))

(define (rar z)
  (z (lambda (p q) p)))

(define (rdr z)
  (z (lambda (p q) q)))

;;2^n

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
