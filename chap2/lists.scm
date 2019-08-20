(define (list-ref lst n)
  (if (null? list)
      (error "rob says no go"))
  (if (= n 0)
      (car lst)
      (list-ref
       (cdr lst)
       (- n 1))))
(define nil '())
(define (last lst)
  (list-ref lst (- (length lst) 1)))
(define (first lst)
  (car lst))
  
  
(define x (list 'a 'b 'c))
(define y (list 'e 'f 'g))
;; (define u (list 'a b c))

(define (copy alist)
  (if (null? alist)
      '()
      (cons (car alist) (copy (cdr alist)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
	    (append (cdr list1) list2))))


(define (reverse alist)
  (if (null? alist)
      '()
      (append (reverse (cdr alist)) (list (car alist)))))

(define centroid
  (lambda (gp)
    (let (
	  (x-sum (add-x gp))
	  (y-sum (add-y gp))
	  (how-many (length gp)))
	  (make-point (/ x-sum how-many)
		      (/ y-sum how-many)))))
	  
	  
		 

(define max-mag
  (lambda (num . nums)
    (apply max (map magnitude (cons num nums)))))
 
