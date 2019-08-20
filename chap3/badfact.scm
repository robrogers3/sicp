(define (node? item)
  (not (pair? item)))
(define (leaf? item)
  (node? item))
(define (fct n)
  (let ((i 1) (m 1))
    (define (loop)
      (cond ((> i n) m)
	    (else
	     (set! i (+ i 1))
	     (set! m (* i m))

	     (loop))))
    (loop)))

(define (rev items)
  (if (null? items) items
      (append (reverse (cdr items)) (list (car items)))))

(define (rev items)
  (define (iter remaining result)
    (if (null? remaining) result
	(iter (cdr remaining) (cons (car remaining) result))))
  (iter items '()))

(define (deeprev items)
  (define (iter remaining result)
    (cond ((null? remaining) result)
	  ((pair? (car remaining))
	   (iter (cdr remaining) (cons (deeprev (car remaining)) result)))
	  (else
	   (iter (cdr remaining) (cons (car remaining) result)))))
  (iter items '()))

(define (sqlist list)
  (map (lambda (item)
	 (square item))
       list))

(define (repeated proc times)
  (if (= times 0) (lambda (x) x)
      (lambda (x)
	(proc ((repeated proc (- times 1)) x)))))


(define foo
  (lambda (a)
    ((repeated square 2))))
(define (app list1 list2)
  (if (null? list1) list2
      (cons (car list1) (app (cdr list1) list2))))


(define (rev items)
  (if (null? items) items
      (app (rev (cdr items)) (list (car items)))))

(define (rev items)
  (define (iter remaining result)
    (display result)
    (newline)
    (if (null? remaining) result
	(iter (cdr remaining) (cons (car remaining) result))))
  (iter items '()))

(define (deprev items)
  (define (iter remaining result)
    (cond ((null? remaining) result)
	  ((pair? (car remaining))
	   (iter (cdr remaining) (cons (deeprev (car remaining)) result)))
	  (else
	   (iter (cdr remaining) (cons (car remaining) result)))))
  (iter items '()))

(define (cntleaves tree)
  (cond ((null? tree) 0)
	((leaf? tree) 1)
	(else (+ (cntleaves (car tree))
		 (cntleaves (cdr tree))))))

(define x (list (list 1 2) (list 3 4) 5))
(define (fringz tree)
  (cond ((null? tree) nil)
	((leaf? (car tree)) (cons (car tree) (fringz (cdr tree))))
	(else
	 (append (fringz (car tree)) (fringz (cdr tree))))))

(define (fring tree)
  (define (build-fringe t result)
    (cond ((null? t) result)
	  ((not (pair? t)) (cons t result))
	  (else
	   (build-fringe (car t)
			 (build-fringe (cdr t) result)))))
    
  (build-fringe tree '()))


(define (fring tree)
  (define (build t result)
    (cond ((null? t) result)
	  ((pair? t) (build (car t) (build (cdr t) result)))
	  (else
	   (display t)(newline)
	   (display result)(newline)(newline)
	   (cons t result))))
  (build tree '()))
	  
(define (last-pair items)
  (if (null? items) 'moo
  (let ((tail (cdr items)))
  (if (null? tail) items
      (last-pair tail)))))


(define (scale-tree tree factor)
  (cond ((null? tree) '())
	((pair? tree)
	 (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))
	(else
	 (* tree factor))))


(define (sc tree f)
  (map
   (lambda (sub-tree)
     (if (pair? sub-tree) (sc sub-tree f)
	 (* sub-tree f)))
   tree))
(define (sqtree tree)
  (map
   (lambda (sub-tree)
     (if (pair? sub-tree) (sqtree sub-tree)
	 (square sub-tree)))
   tree))

(define (sqtree t)
  (cond ((null? t) '())
	((pair? t)
	 (cons (sqtree (car t)) (sqtree (cdr t))))
	(else (square t))))
	 
(define (tree-map proc tree)
    (map
     (lambda (sub-tree)
       (if (pair? sub-tree) (tree-map proc sub-tree)
	   (proc sub-tree)))
   tree))

;;(tree-map square x)

