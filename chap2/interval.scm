(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


(define (reverse alist)
  (if (null? alist)
      alist
      (append (reverse (cdr alist)) (list (car alist)))))

(define (last-pair l)
  (if (= (length l) 0)
      l
      (let ((tail (cdr l)))
	(if (null? tail)
	    l
	    (last-pair tail)))))

(define nil ())

(define (reverse l)
  (define (iter remaining result)
    (if (null? remaining)
	result
	(iter (cdr remaining) (cons (car remaining) result))))
  (iter l nil))
  

