(define (even? n)
  (= (remainder n 2) 0))
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
	 (sum term (next a) next b))))
(define (cube x)
  (* x x x))
(define (inc n) (+ n 1))
(define (sum-squ a b)
  (sum square a inc b))

(define (identity x) x)

(define (sum-int a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f  (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (kth-multiple k)
    (cond ((or (= k 0) (= k n)) 1.0)
          ((even? k) 2.0)
          (else 4.0)))
  (define (kth-term k)
    (* (kth-multiple k)
       (f (+ a (* k h)))))
  ;;       (y k))) 
  (* (sum kth-term 0 inc n)
     (/ h 3.0)))

;;(simpsons-rule cube 0 1 100)


(define (sum-it term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-num x y)
  (sum-it identity x inc y))


(define (sum-sqr-it a b)
  (sum-it square a inc b))


(define (multiplex term a next b)
  (if (> a b) 1
      (* (term a)
	 (multiplex term (next a) next b))))

(define (product term a next b)
  (if (> a b)
      1.0
      (* (term a)
	 (product term (next a) next b))))

(define (multiplex-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (prod-num a b)
  (multiplex identity a inc b))
(define (prod-num-it a b)
  (multiplex-iter identity a inc b))
(define (num-k k)
  (cond ((= k 1) 2)
	((even? k) (+ k 2))
	(else (+ k 1))))
(define (denom-k k)
  (cond ((even? k) (+ k 1))
	(else (+ k 2))))

(define (wiley-pi-num n)
  (multiplex-iter num-k 1 inc n))
(define (wiley-pi-denom n)
  (multiplex-iter denom-k 1 inc n))
(define (wiley-pi n)
  (/ (* 1.0 (wiley-pi-num n)) (wiley-pi-denom n)))

(define (pi-prod n)
  (define (pi-term n)
    (if (even? n)
	(/ (+ n 2) (+ n 1))
	(/ (+ n 1) (+ n 2))))
  (product pi-term 1 inc n))

(define (pi-prod n)
  (define (pi-term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (product pi-term 1 inc n))
  
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner seed-value term a next b)
  (define (iter a result)
    (if (> a b) result
	(iter (next a) (combiner result (term a)))))
  (iter a seed-value)
  )
(define (accum-sum a b)
  (accumulate + 0 identity a inc b))
(define (accum-sum-it a b)
  (accumulate-iter + 0 identity a inc b))

(define (accum-prod a b)
  (accumulate * 1 identity a inc b))
(define (accum-prod-iter a b)
  (accumulate * 1 identity a inc b))


(define (filter-accumulate combiner null-value term a next b predicate)
  (cond ((> a b) null-value)
	((predicate a) (combiner (term a)
				 (filter-accumulate combiner null-value term (next a) next b predicate)))
	(else (filter-accumulate combiner null-value term (next a) next b predicate))))
  

(define (even-sum a b)
  (filter-accumulate + 0 identity a inc b even?))
(define (even-prod a b)
  (filter-accumulate * 1 identity a inc b even?))

;;GCD(i,n) = 1
(define (prod-relative-primes n)
  (define (relative-prime? x) (= (gcd x n) 1))
  (filter-accumulate * 1 identity 1 inc n relative-prime?))

