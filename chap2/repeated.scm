;; (define (mul a b)
;;   (if (= b 0)
;;       0
;;       (+ a (mul a (- b 1)))))
;; (define (expon a b)
;;   (if (= b 0)
;;       1
;;       (mul a (expon a (- b 1)))))

(define mul 
  (lambda (a b)
    ((repeated (lambda (x) (+ x a))  b) 0)))

(define expon
  (lambda (a b)
    ((repeated (lambda (x) (* x a)) b) 1)))

(define (repeated proc n)
	(display n)
	(newline)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x)
	(display 'x-is)
	(newline)
	(display x)
	(newline)
	;;(+ 0
	(proc
	 ;; (repeated * n--
	 ((repeated proc (- n 1)) x)))))
