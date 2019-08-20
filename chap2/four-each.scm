(define (four-each proc items)
  (cond ((null? items) #t)
	(else (proc (car items))
	      (four-each proc (cdr items)))))


