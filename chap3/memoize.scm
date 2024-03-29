(define (make-table)
  (list '*table*))
(define (lookup key table)
  (let ((record (asc key (cdr table))))
    (if record
	(cdr record)
	false)))
(define (insert! key value table)
  (let ((record (asc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else
	 (+ (fib (- n 1)) (fib (- n 2))))))


(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((prev (lookup x table)))
	(or prev
	    (let ((result (f x)))
	      (insert! x result table)
	      result))))))
	    
(define memo-fib
  (memoize
   (lambda (n)
       (cond ((= n 0) 0)
	((= n 1) 1)
	(else
	 (+ (memo-fib (- n 1)) (memo-fib (- n 2))))))))

(define (bar position)
  (lambda (m . args)
    (cond ((eq? m 'pos) position)
	  ((eq? m 'set-pos) (set! position (car args)))
	  ))
  )
(define ship (bar 1))
(display ship)
(display (ship 'pos))
