(define nil '())
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

(define (reverse l)
  (define (iter remaining result)
    (if (null? remaining)
	result
	(iter (cdr remaining) (cons (car remaining) result))))
  (iter l nil))

(define mul
  (lambda (a b)
    ((repeated (lambda (x)
		 (+ x a)) b) 0)))


(define (repeated proc n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) 
	(proc ((repeated proc (- n 1)) x)))))

  

(define (same-same test . args)
  (define (filterout parity? lst)
    (cond ((null? lst) lst)
	  ((parity? (car lst)) (cons (car lst) (filterout parity? (cdr lst))))
	  (else (filterout parity? (cdr lst)))))
  
  (cons test
  (if (even? test)
      (filterout even? args)
      (filterout odd? args))))




(define (squareslist items)
  (if (null? items)
      items
      (cons (square (car items)) (squareslist (cdr items)))))

(define (foureach proc items)
  (cond ((null? items) #t)
	(else (proc (car items))
	      (foureach proc (cdr items)))))

(define (count-leaves tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	(else (+ (count-leaves (car tree))
	   (count-leaves (cdr tree))))))



(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

(define (reverse items)
  (define (iter remaining result)
    (if (null? remaining)
	result
	(iter (cdr remaining) (cons (car remaining) result))))
  (iter items ()))


(define (deep-reverse items)
  (define (iter remaining result)
    (cond ((null? remaining) result)
	  ((pair? (car remaining))
	   (iter (cdr remaining) (cons (deep-reverse (car remaining)) result)))
	  (else (iter (cdr remaining) (cons (car remaining) result)))))
  (iter items ()))

(define (fringe tree)
  (cond ((null? tree) nil)
	 ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
	 (else (cons (car tree) (fringe (cdr tree))))
	 ))


(define (xfringe l)
  (cond ((null? l) nil)
        ((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
        (else (cons (car l) (fringe (cdr l))))))
;; deep woods roll ocean bound
;; feet fall heel toe muffled soft
;; a pine cone rolls left

