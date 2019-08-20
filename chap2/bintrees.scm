(define t1 (list 1 (list 2 (list 3 4)) (list 5)))



(define (num-leaves tree)
  (cond
   ((null? tree) 0)
   ((not (pair? tree)) 1)
   (else
    (+ (num-leaves (car tree)) (num-leaves (cdr tree))))))

(define (num-leaves tree)
  (accumulate + 0 (map (lambda (x) 1) (enum-tree tree))))

(define (tree-map tree proc)
  (map proc  (enum-tree tree)))

(define (tree-manip leaf-op init merge tree)
  (if (null? tree) init
      (if (not (pair? tree))
	  (leaf-op tree)
	  (merge (tree-manip leaf-op init merge (car tree))
		 (tree-manip leaf-op init merge (cdr tree))))))


(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set x set)
  (cond
   ((null? set) false)
   ((= x (entry set)) true)
   ((< x (entry set)) (element-of-set x (left-branch set)))
   ((> x (entry set)) (element-of-set x (right-branch set)))))
(define (adjoin-set x set)
  (cond
   ((null? set) (make-tree x '() '()))
   ((= x (entry set)) set)
   ((< x (entry set))
    (make-tree (entry set)
	       (adjoin-set x (left-branch set))
	       (right-branch set)))
   ((> x (entry set))
    (make-tree (entry set)
	       (adjoin-set x (right-branch set))
	       (left-branch set)))))

(define (tree->list-1 tree)
  (if (null? tree) '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree) result-list
	(copy-to-list
	 (left-branch tree)
	 (cons (entry tree)
	       (copy-to-list (right-branch tree)
			     result-list)))))
  (copy-to-list tree '()))

(define bint1 (list 7 (list 3 (list 1 '() '()) '()) (list 9 '() (list 11 '() '() ))))
(define bint2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define bint3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (10 () ()))))
(define bint4 (list->tree '(2 3 8 1 5)))
(define (app list1 list2)
  (if (null? list1) list2
      (cons (car list1) (app (cdr list1) list2))))

(define (list->tree elements)
  (car (partial-tree elements  (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts) right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))
	    

  
(define (key record)
  (car record))

(define (lookup given-key records)
  (if (null? records) #f
      (let ((entry-key (entry records)))
	(cond ((= given-key entry-key) (entry records))
	      ((< given-key entry-key)
	       (lookup given-key (left-branch records)))
	      ((> given-key entry-key)
	       (lookup given-key (right-branch records)))))))
