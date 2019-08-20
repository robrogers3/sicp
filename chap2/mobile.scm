(define nil '())

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
	((leaf? tree) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define mobile1
  (make-mobile (make-branch 10 1)
	       (make-branch 2 5)))

(define mobile2
  (make-mobile (make-branch 10 1)
	       (make-branch 2 (make-mobile (make-branch 4 1) (make-branch 1 4)))))
			    

(define mobile3
  (make-mobile (make-branch 9 1)
	       (make-branch 2 4)))

(define mobile4
  (make-mobile (make-branch 4 (make-mobile (make-branch 4 1) (make-branch 1 4)))
	       (make-branch 2 (make-mobile (make-branch 2 1) (make-branch 1 2)))))
			    

(define (accumulate op init seq)
  (display seq)(newline)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (leaf? node)
  (not (pair? node)))
;;(define (mobile-weight mobile)

(define (mobile-length mobile)
  (define (blength branch)
    (if  (pair? (branch-structure branch))
	 (cons (branch-length branch) (mobile-length (branch-structure branch)))
	 (branch-length branch)))
  (list (blength (left-branch mobile))
	(blength (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? (branch-structure branch))
	(mobile-weight (branch-structure branch))
	(branch-structure branch))))

(define (mobile-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
		    
(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))
	
(define (balanced mobile)
  (let (
	(left (left-branch mobile))
	(right (right-branch mobile)))
	
    (= (torque left) (torque right))))

