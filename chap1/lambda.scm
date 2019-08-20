(define (even? n)
  (= (remainder n 2) 0))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))
(define (sum-it term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))
(define (sum-int a b)
  (sum (lambda (x) x) a (lambda (x) (+ 1 x)) b))

(define (f x y)
  (
   (lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   
   (+ 1 (* x y))
   (- 1 y)
   ))

(define (zed z)
  (display z)
  (newline)
  (
   (lambda (a)
     (display a)
     (+ z a))
     (+ z 1)
     ))
