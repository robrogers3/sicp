(define (memmq item items)
  (cond ((null? items) false)
	((eq? item (car items)) items)
	(else (memmq item (cdr items)))))
	 
(define (equal l1 l2)
  (cond ((and (null? l1) (null? l2)) true)
	((not (eq? (car l1) (car l2))) false)
	(else
	 (equal (cdr l1) (cdr l2)))))


;;deriv
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x)
  (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
  (list '* m1 m2))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base p)
  (cadr p))
(define (exponent p)
  (caddr p))
(define (make-exponent base exponent)
  (list '** base exponent))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp);; sum deriv of addend and deriv augend
         (make-sum (deriv (addend exp) var) 
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product
	   (exponent exp)
	   (make-exponentiation (base exp)
				(make-sum (exponent exp) -1)))
	  (deriv (base exp) var)))
;;d(un)  = nx**n-1 * du
;;dx               dx			 
        (else
         (error "unknown expression type -- DERIV" exp))))

	 
;;(deriv '(* x y) 'x)
;; (deriv '(+ x 3) 'x)
;; (deriv '(* x y) 'x)
(define (augend s)
  (let ((a (cddr s)))
    (if (pair? a)
        (if (null? (cdr a))
            (car a)
            (apply make-sum a))
        (error "malformed addition -- AUGEND" s))))

(define (multiplicand p)
  (let ((m (cddr p)))
    (if (pair? m)
        (if (null? (cdr m))
            (car m)
            (apply make-product m))
        (error "malformed multiplication -- MULTIPLICAND" m))))
;; (define (augend s)    
;;    (accumulate make-sum 0 (cddr s))) 
  
;;  (define (multiplicand p)  
;;    (accumulate make-product 1 (cddr  p))) 

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else
	 (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2))
	 '())
	((null? set2) set1)
	((null? set1) set2)
	((not (element-of-set? (car set1) set2))
	 (cons (car set1) (union-set (cdr set1) set2)))
	(else
	 (union-set (cdr set1) set2))))
	     
(define set1 (list 1 1 2 2 4 4))
(define set2 (list 1 1 3 3 4 4))
(define set3 (list 0 1 3 3 4 4))

;; (define (adjoin-set x set)
;;   (cons x set))
;; (define (inter-set set1 set2)
;;   '()
;;   )
;; (define (union-set set1 set2)
;;   '()
;;   )

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else
	 (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x (intersection-set (cdr set1) (cdr set2))))
	      ((< x1 x2) (intersection-set (cdr set1) set2))
	      ((< x2 x1) (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((element-of-set? x set) set)
	((null? set) (cons x set))
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))





;;'(1) '(2 3)
;;1 < 2 so skip it, 
(define (intersection set1 set2)
    (if (or (null? set1) (null? set2))
	'()
	(let ((x1 (car set1)) (x2 (car set2)))
	 (cond
	  ((= x1 x2) (cons x1 (intersection (cdr set1) (cdr set2))))
	  ((< x1 x2) (intersection (cdr set1) set2))
	  ((< x2 x1) (intersection set1 (cdr set2)))))))
	
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2))
	 '())
	((null? set2) set1)
	((null? set1) set2)
	((not (element-of-set? (car set1) set2))
	 (cons (car set1) (union-set (cdr set1) set2)))
	(else
	 (union-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2))
	 '())
	((null? set2) set1)
	((null? set1) set2)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond
	    ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
	     ;;cons x to to cdr set1 && all of set2
	    ((< x1 x2)
	     (cons x1 (union-set (cdr set1) set2)))
	    ((< x2 x1)
	     (cons x2 (union-set set1 (cdr set2)))))))))
	     
		   
(define (app list1 list2)
  (if (null? list1) list2
      (cons (car list1) (append (cdr list1) list2))))

(define (make-change price paid)
  
  (define bills (list 10 5 1))
  
  (map (lambda (bill)
	 (define (iter due results)
	   (display 'due)(display due)(newline)
	   (cond ((> bill due) results)
		 (else
		  (iter (- due bill) (change-for-bill bill due results)))))
	 (iter ((- price paid) '()))
	 bills)))

  
  ;; iter here
  ;;(display 'curretn)(display current-change)(newline)


(define (make-change price paid)
  (define bills (list 10 5 1))
  (define (mk activebills due)
    (if (null? activebills)
	nil
	(let ((bill (car activebills))
	      (change (change-for-bill (car activebills) due)))
	  (cond
	   ((null? change)
	    (mk (cdr activebills) due))
	   (else
	    (cons change (mk (cdr activebills)
			     (- due (* (car change) (cadr change)))))
	  
	  )))))
  (mk bills (- price paid)))

;;((10:1)(5:1)) ;;15 (+ (* 10 1) (* 5 1))

;; (list 10 5 1)
;; (paid due)
(define (change-for-bill bill due)
  (define (iter due change)
    (cond ((> bill due)
	   change)
	  ((null? change)
	   (iter (- due bill) (list bill 1)))
	  (else
	   (iter (- due bill) (list bill (+ 1 (cadr change)))))))
    (iter due '()))

(define (sum-of-change change-so-far)
  (display 'sum-of-change)
  (display change-so-far)(newline)
  (if (null? change-so-far) 0
      (accumulate + 0 (map
		       (lambda (change-for-bill)
			 (display 'labmbda-change-for-bill)
			 (display change-for-bill)(newline)
			 (* (car change-for-bill) (cadr change-for-bill)))
		       change-so-far))))
(define (make-change price paid) ;; (make-change 28 30) ((1: 2))
  (define bills (list 10 5 1))
  (let ((due (- paid price)))
    (acc
     (lambda (change-so-far current-bill)
       (display 'make-change-change-so-far)
       (display change-so-far)(newline)
       (display 'remanign-bills)
       (display current-bill)
       (newline)
       (append change-so-far
	       (change-for-bill current-bill
	       (- due (sum-of-change change-so-far)))))
     '(()) bills)))

  
(define (mul x y)
  (newline)
  (display 'x)(display x)(newline)
  (display 'y)(display y)(newline)
  (* x y)
  )

;;(accumulate mul 1 (list 2 3 4))

(define (boo x)
  x)

(define (bazz)
  (map boo (enum-interval 1 10))
  )
	 
	 
(define (acc op init seq)
  (if (null? seq)
      init
      (acc op (op init (car seq)) (cdr seq))))

(define (flat-map proc seq)
  (acc append '() (map proc seq)))

