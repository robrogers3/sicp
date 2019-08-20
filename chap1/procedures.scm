(define adjoin cons)
(define first car)
(define rest cdr)
(define nil '())
(define (generate-interval a b)
  (if (> a b) nil
      (cons a (generate-interval (+ a 1) b))))

(define double (lambda (x) (+ x x)))
(define (identity x) x)
(define sq (lambda (y) (* y y)))
(define epsilon 0.001)
(define deriv
  (lambda (f)
    (lambda (x) (/ (- (f (+ x epsilon)) (f x))
		   epsilon))))

;;((deriv sq) 5)
(define (even? n)
  (= (remainder n 2) 0))
(define (map proc lst)
  (if (null? lst)
      nil
      (adjoin (proc (first lst))
	      (map proc (rest lst)))))

(define (filter pred? lst)
  (cond ((null? lst) nil)
	((pred? (first lst))
	 (adjoin (first lst) (filter pred? (rest lst))))
	(else (filter pred? (rest lst)))))

(define (sq-list lst)
  (map sq lst))

(define (dbl-list lst)
  (map (lambda (x) (* 2 x)) lst))

(define (fold-right op init lst)
  (if (null? lst) init
      (op (first lst)
	  (fold-right op init (rest lst)))))

(define (sum-interval f start inc terms)
  (fold-right + 0
	      (map (lambda (x) (f (+ start (* x inc))))
		   (generate-interval 0 terms))))

(define (negative? x)
  (< x 0))
(define (positive? x)
  (> x 0))
(define (average x y)
  (/ (+ x y) 2))
;;fixed point
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((> 0 test-value)
		 (search f neg-point midpoint))
		((< 0 test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "values are not of opponisite values")))))


(define fzz (lambda (x) (- (* x x x) (* 2 x) 3)))
(half-interval-method fzz 1.0 2.0)
;;ratio f(x) -> 1 + 1/x
(define fibf (lambda (x) (+ 1 (/ 1 x))))

(define (integral f a b n)
  (let ((delta (/ (- b a) n)))
    (* (sum-interval f a delta n) delta)))
(define atan (lambda (a)
	       (integral (lambda (x) (/ 1 (+ 1 (square x)))) 0 a 10000)))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enuf? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "next: ")
      (display next)
      (newline)
      (if (close-enuf? guess next)
	  next
	  (try next))))
  (display "first ")
  (display first-guess)
  (newline)
	   
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (avg y (/ x y)))
	       1.0))

(define gold
  (lambda ()
  (fixed-point (lambda (y) (+ 1 (/ 1 y)))
	       1.0)))

 (define x-to-x
   (lambda ()
     (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.0001)
     ))

;; f = N / D
;; fk = (/ Nk (Dk + (fk+1))
(define (cont-frac n d k)
  (define (iter i result)
    (display (d i))
    (newline)
    (display "i ")(display i)
    (newline)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))
	))
  (iter k 0))
(define (cont-frac-r n d k)
  (define (recur i)
    (if (> i k)
	0
	(/ (n i) (+ (d i) (recur (+ 1 i))))
	))
  (recur 1))

;;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8)
;;(cont-frac-r (lambda (i) 1.0) (lambda (i) 1.0) 8)
;;1, 2, 1, ;;i @ 2 == 2 (
;;1, 4, 1, ;;i @ 5 == 4
;;1, 6, 1, ;;i @ 8 == 6
;;1, 8, 1, ;;i @ 11 == 8
;;1, 10,1  ;;i @ 14 == 10
;;1, 2,
;;1, 1,
;;4, 1,
;;1, 6,
;;1, 1,
;;1, 8,
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-fp x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
;;ube root of x is a fixed point of the function y  x/y2

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define dx 0.00001)

(define (derive g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))  ;; (2 - 1) = 1; (- 2 1) = 1; (- 2 1 1) == 0 ( / 1 2) (* 3 3 2) 18
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-nm x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (applyTwice f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) 
    (f (g x))))

(define (repeated f n)
  (define (iter i g)
    (if (= i n)
	g
	(iter (+ i 1) (compose f g))))
  (iter 1 f))

(define (repeated f n)
  (cond ((= n 1) f)
	(else (compose f (repeated f (- n 1))))))

(define (repeated-proc proc n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x)
	(proc ((repeated proc (- n 1)) x))
	)))


;;smooth = (avg f( - x dx), (f x), (f(+ x dx))
(define (smooth f)
  (lambda (x) 
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))
  )

(define zed
  (lambda (x) (+ x 1)))

(define (n-fold-smooth f n)
  (repeated smooth n) f)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
(define (fourth x)
  (* x x x x))

(define (fifth x)
  (* x x x x x))

(define (pow x y)
    (define (iter i results)
      (if (= i y)
	  results
	  (iter (+ i 1) (* results x))))
    (iter 0 1))

(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))
;;  ((repeated (lambda (z) (* z z)) y) x))

(define (cube-root x)
  (fixed-point
   (average-damp (lambda (y) (/ x (square y))))
   1.0
   ))

(define (fourth-root x)
  (fixed-point
   (average-damp (average-damp (lambda (y) (/ x (pow y 3)))))
   1.0))

(define (fifth-root x)
  (fixed-point
   (average-damp (average-damp (lambda (y) (/ x (pow y 4)))))
   1.0))

(define (sixth-root x)
  (fixed-point
   (average-damp (average-damp (lambda (y) (/ x (pow y 5)))))
   1.0))

(define (seventh-root x)
  (fixed-point
   (average-damp (average-damp (lambda (y) (/ x (pow y 6)))))
   1.0))


;; We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y  x/y does not converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y  x/y2. Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for y  x/y3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y  x/y3) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon repeated average damping of y  x/yn-1. Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you need are available as primitives.

(define (nth-root-function x n)
  (lambda (y) (/ x (expt y (- n 1)))))


(define (damped-nth-root base pow damps)
  (if (= 0 damps)
      (average-damp (nth-root-function base pow))
      ((repeated average-damp damps) (nth-root-function base pow))))

(define (test-nth-root-damping x n a)
  (fixed-point (damped-nth-root x n a) 1.0))
(define (nth-root x n)
  (let ((damps (floor (/ (log n) (log 2)))))
	(fixed-point (damped-nth-root x n damps) 1.0)))
;; 1 -3 -> 1
;; 4- 7 -> 2
;; 8 - n -> 3

;; (define (damped-nth-root x n a)
;;   (if (= a 0)
;;       (nth-root-function x n)
;;       ((repeated average-damp a) (nth-root-function x n))))

;; (define (nth-root x n)
;;   (let ((damps (floor (/ (log n) (log 2)))))
;;         (fixed-point (damped-nth-root x n damps) 1.0)))
		  
