(define (even? n)
  (= (remainder n 2) 0))
(define (odd? n)
  (not (even? n)))
(define (xfilter predicate? lst)
  (if (null? lst)
      lst
      (if (predicate? (car lst))
	  (cons (car lst) (xfilter predicate? (cdr lst)))
	  (xfilter predicate? (cdr lst)))))
(define (same-same-parity n . nums)
  (cons n
  (if (odd? n)
      (xfilter odd? nums)
      (xfilter even? nums))))

    
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))


(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (square x)) items))

(define (fud-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))


(define (rud-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons
	       answer
	       (square (car things))))))
  (iter items nil))

(define (for-each proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
       (for-each proc (cdr items))))
      
  #t)
;; (define x
;;   (lambda ()
;;   (for-each (lambda (x) (newline) (display x)) (list 57 321 88))
;;   ))
(define (same-parity i . l)
  (define (filter-list filter-to-even ll)
    (cond ((null? ll) nil)
          ((equal? filter-to-even (even? (car ll)))
           (cons (car ll) (filter-list filter-to-even (cdr ll))))
          (else (filter-list filter-to-even (cdr ll)))))
  (cons i (filter-list (even? i) l)))

(define (same-parity i . l)
  (define (filter-list filter-to-even ll)
    (cond ((null? ll) nil)
	  ((equal? filter-to-even (even? (car ll)))
	   (cons (car ll) (filter-list filter-to-even (cdr ll))))
	  (else (filter-list filter-to-even (cdr ll)))))
  (cons i (filter-list (even? i) l)))


;;(define three (list (1 (list 2 (list 3)))))
;;(define four (cons 1 (cons 2 (cons 3 (cons 4 nil)))))
;;(define four (cons 1 (cons 2 (cons 3 (cons 4)))))

(define (sp i . l)
  (define (filter-list on-even ll)
    (cond ((null? ll) '())
	  ((equal? on-even (even? (car ll)))
	   (cons (car ll) (filter-list on-even (cdr ll))))
	  (else (filter-list on-even (cdr ll)))))
  (cons i (filter-list (even? i) l)))
	  

    
