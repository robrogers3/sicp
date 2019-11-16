(define (filtr pred? items)
  (cond ((null? items) '())
	((pred? (car items))
	 (cons (car items) (filtr pred? (cdr items))))
	(else
	 (filtr pred? (cdr items)))))


(define (enum-interval low hi)
  (if (> low hi)
      nil
      (cons low (enum-interval (+ low 1) hi))))

(define enumerate-interval enum-interval)
(define x (list (list 1 2) (list 3 4) 5))
(define (fringz tree)
  (cond ((null? tree) nil)
	((leaf? (car tree))
	 (display 'leaf)
	 (display tree)
	 (newline)
	 (cons (car tree) (fringz (cdr tree))))
	(else
	 (append (fringz (car tree)) (fringz (cdr tree))))))

(define (enum-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else
	 (append (enum-tree (car tree)) (enum-tree (cdr tree))))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((pair? tree) (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))
	(else (list tree))))
(define (fringzz tree)
  (display 'fringz)
  (display tree)
  (newline)
  (cond ((null? tree) nil)
	((not (pair? tree)) (display tree)(newline) (list tree))
	(else
	 (display 'else)
	 (display tree)(newline)
 	 (append (enum-tree (car tree)) (enum-tree (cdr tree))))))


(define (accumulate op init seq)
  (if (null? seq) init
      (op (car seq)
	  (accumulate op init (cdr seq)))))
(define (sum-odd-squares n)
  (accumulate + 0
	      (mapp square
		   (filter odd?
			   (enum-interval 0 n)))))

(define nil '())

(define (mapp p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
	      nil sequence))
(define (appd seq1 seq2)
  (accumulate cons seq2 seq1))
(define (len sequence)
  (accumulate (lambda (x y) (display x)(newline)(display y)(newline) (+ 1 y))  0 sequence))


;; fib 0 = 0
;; fib 1 = 1
;; fib n = (+ (fib (n -1))  (fib (n - 2)))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))


(define (list-fib-squares n)
  (accumulate cons '()
	      (mapp square
		   (mapp fib
			(enum-interval 0 n)))))


(define (accum-squares-len n)
  (len
  (filter odd?
  (accumulate (lambda (x y)
		(cons (square x) y))
	      '()
	      (enum-interval 0 n)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		;; (display "this ceoff: ")(display this-coeff)
		;; (newline)
		;; (display higher-terms)
		;; (newline)
		;; (display (+ this-coeff (* x higher-terms)))
		;; (newline)(newline)
		;;1
		;;(list this-coeff higher-terms)

		(+ this-coeff (* x higher-terms)))
		;;this-coeff
              0
              coefficient-sequence))
;;in other words, we start with aN, multi by x, add aN-1, multi x, and so on until we reach a0
;; ( … (anx + an-1)x + … + a1)x + a0
(define z (list (list 1 2) (list 3 4)))
(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (x) x)
	(enumerate-tree t))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (car )
            (accumulate-n op init )))))

(define l1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define l2 (list (list 1 2 3) (list 10 2 3)))

;;(accumulate-n + 0 l2) -> (2 4 6)

(define (even-fibs n)
  (define (next k)
    (if (> k n) '()
	(let ((f (fib k)))
	  (display k)
	  (display " ")
	  (display f)
	  (newline)
	  (newline)
	  (if (even? f)
	      (cons f (next (+ k 1)))
	      (next (+ k 1))))))
  (next 0))

(define (accumulate op initial sequence)
  (cond ((null? sequence) initial)
	(else
	 ;; (display 'car)(display (car sequence))(newline)
	 ;; (display 'squence)(display (cdr sequence))
	 ;; (newline)
	 (op (car sequence)
	     (accumulate op initial (cdr sequence))))))

(define (even-fibbs n)
  ;;accumlate cons
  ;;filter even?
  ;;map fib
  ;;enum-int
  (accumulate cons '()
	      (filter even?
		      (map fib
			   (enum-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons '()
	      (map square
		   (map fib
			(enum-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1
	      (map square
		   (filter odd? (sequence)))))
			   
(define (add x y)
  (display x)
  (display " x:y ")
  (display y)
  (newline)
  (+ x y))

(define (mappp p sequence)
  (accumulate (lambda (x y) (cons (p x y) y)) nil sequence))

(define (mapp p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))
(define (appd seq1 seq2)
  (accumulate cons seq2 seq1))
(define (lenn sequence)
  (accumulate (lambda (x y) (display x)(display y)(newline) (+ 1 y)) 0 sequence))
;; (* (+ aN-1 (* aN x)) x)
(define (horner x coff-seq)
  (accumulate
   (lambda (coff terms)
     (display 'terms)(display terms)
     (newline)
     (display 'coff)(display coff)
     (newline)
     (+ coff (* terms x))
     terms
     )
   0
   coff-seq))

(define (cnt-leaves t)
  (accumulate add 0 (map (lambda (x) x) (enumerate-tree t))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init
			(map
			 car
			 seqs))
			
            (accumulate-n op init (map cdr seqs)))))


(define s (list (list 1 2) (list 3 4)))
;;(accumulate-n + 0 s)

(define (dot-product v r) ;;       returns the sum Σiviwi
  (accumulate + 0 (map * v r))) ;;(1 2) (1 2) ;;(1 4) 5


;;returns the vector t, where ti = Σjmijvj
(define (matrix*vector m v)
  (map (lambda (r) (dot-product v r)) m))
;;(transpose m)           returns the matrix n, where nij = mji

(define (transpose m)
  (accumulate-n cons nil m))

;;returns the matrix p, where pij = Σkmiknkj
(define (matrix-*-matrix m1 m2)
  (let ((cols (transpose m2)))
    (map (lambda (vector) (matrix*vector cols vector)) m1)))
   
(define (foo m)
  (map (lambda (r) (display r)(newline)
	       r) m))
(define v (list 1 2 3)) ;;vector
(define m1 (list (list 1 2 3) (list 4 5 6)))
(define m2 (list (list 1 2) (list 3 4) (list 5 6)))

;;1,4 => (1 4),2,5=>(2 5),3,6=>((3 6)
;;(1 3 5)(2 4 6)

(define fold-right accumulate)

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest) result
	(iter (op result (car rest)) (cdr rest))))
  (iter init seq))

(define (rev1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (rev2 sequence)
  (fold-left (lambda (x y) (cons y x) ) nil sequence))

(define (bar n)
  (flatmap
   (lambda (i)
     (map
      (lambda (j)
	(list i j))
	(enum-interval 1 (- i 1))))
   (enum-interval 1 n)))

(define (unique-pairs n)
  (flatmap (lambda (i)
                   (map (lambda (j) (list i j))
                        (enum-interval 1 (- i 1))))
                 (enum-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (divides? a b)
  (= (remainder b a) 0))
(define (find-divisor n test-divisor)

  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n)
	 test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-prime-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  ;;map make-prime-sum
  ;;filter prime-sum?
  ;;flat-map n lambda i lambda j
  (map make-prime-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map 
		   (lambda (j)
		     (list i j))
		   (enum-interval 1 (- i 1))))
		  (enum-interval 1 n)))))

(define (remove item seq)
  (filter (lambda (x)
	    (not (= x item))) seq))
  
(define (permutations s)
  (if (null? s) (list '())
      (flatmap
       (lambda (x)
	 (display "x ")(display x)(newline)
	 (map
	  (lambda (p) (display "p ")(display p)(newline)(cons x p))
	  (permutations (remove x s))))
       s)))

;;uniq pairs
;;given n, gen seq of pairs (i,j) with 1<=j<=i<=n
(define (prime-sum-pairs n)
  ;;map make-prime-sum
  ;;filter prime-sum?
  ;;flat-map n lambda i lambda j
  (map make-prime-sum
       (filter prime-sum?
	       (unique-pairs n))))


(define (ordered-triplets-equal-to n sum)
  (define (make-sum-set pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  (define (sum-equals pair)
    (= sum (+ (car pair) (cadr pair))))
  (map make-sum-set
       (filter sum-equals
	       (unique-pairs n))))

;;
(define (queens board-size)
  ;;seq of all ways to place queens in first k cols of board
  (define (queens-cols col) ;; I bet k gets reduced
    (if (= col 0) (list empty-board)
	(filter
	 (lambda (positions) (safe? col positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map
	     (lambda (new-row)
	       (adjoin-position new-row col rest-of-queens))
	     (enum-interval 1 board-size)))
	  (queens-cols (- col 1))))))
  (queens-cols board-size))

(define empty-board nil)
(define (adjoin-position row col board)
  (cons (cons row col) board))
(define (get-row position)
  (car position))
(define (get-column position)
  (cdr position))
(define (safe? col position)
  #t)
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
