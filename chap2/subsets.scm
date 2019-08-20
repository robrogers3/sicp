(define nil '())
(define (subsets set)
  (if (null? set) (list '())
      (let ((rest (subsets (cdr set))))
	(display 'pre-rest)(display rest)(newline)(newline)
	(append rest
		(map
		 (lambda (x)
		   (display set)(newline)
		   (display 'x)
		   (display x)(newline)
		   (display 'map-rest)
		   (display rest)(newline)(newline)

		   (cons (car set) x)
		   )
		 rest)
		)

	)
      ))


;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map (lambda (x) (cons (car s) x)) rest)))))
(define (mmap proc items)
  (if (null? items) nil
      (cons (proc (car items))
	    (mmap proc (cdr items)))))
