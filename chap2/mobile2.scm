(define (make-mobile left right)
  (list left right))
(define (is-mobile? structure)
  (pair? structure))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (display 'left-branch)
  (display (car mobile))
  (newline)
  (car mobile))
(define (right-branch mobile)
  (display 'rightbranch)
  (display (cadr mobile))
  (newline)
  
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (display 'branch-struct)
  (display (cadr branch))
  (newline)	
  (cadr branch))

(define (mobile-length mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (+
     (if (is-mobile? (branch-structure left)) (mobile-length left)
	 (branch-length left))
     (if (is-mobile? (branch-structure right)) (mobile-length right)
	 (branch-length right))
     )))
(define (mobile-weight mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (+
     (if (is-mobile? (branch-structure left)) (mobile-weight left)
	 (branch-structure left))
     (if (is-mobile? (branch-structure right)) (mobile-weight right)
	 (branch-structure right))
     )))


(define (branch-torque branch)
  (display 'branch-torque)(display branch)(newline)
  (if (is-mobile? (branch-structure branch)) (* (branch-length branch) (mobile-weight (branch-structure branch)))
      (* (branch-structure branch) (branch-length branch))))


(define (balanced mobile)
  (display "balanced? mobile" )
  (display mobile) 
  (newline)
  (display 'righttorch)(display (branch-torque (right-branch mobile)))
  (newline)
  (display 'lefttorch)(display (branch-torque (left-branch mobile)))
  (newline)
  (branch-torque (left-branch mobile))
  (branch-torque (right-branch mobile))
  (= (branch-torque (left-branch mobile))
   (branch-torque (right-branch mobile)))
  )

  ;;(= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile))))



(define m1 (make-mobile (make-branch 1 4) (make-branch 4 1)))
(define m2 (make-mobile (make-branch 1 6)
			(make-branch 2 (make-mobile
					(make-branch 2 1) (make-branch 1 2)))))
(define m3 (make-mobile (make-branch 1 1) (make-branch 2 2)))
(define b1 (make-branch 10 10))


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
		      (lambda (x)
			(cons (car s) x))
		      rest)))))

(define (filtr pred? items)
  (cond ((null? items) '())
	((pred? (car items))
	 (cons (car items) (filtr pred? (cdr items))))
	(else
	 (filtr pred? (cdr items)))))


(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
      (accumulate op initial (cdr sequence)))))

;; (define (accumulate op initial sequence)
;;   (if (null? sequence)
;;       initial
;;       (op (car sequence)
;;           (accumulate op initial (cdr sequence)))))
