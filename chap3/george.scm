(define make-vector cons)
(define xcor car)
(define ycor cdr)
(define make-segment list)
(define start-segment car)
(define end-segment cadr)
;;rect
(define make-rect list)
(define origin car)
(define horiz cadr)
(define vert caddr)
;;vector ops
(define (+vect v1 v2)
  (make-vector (+ (xcor v1) (xcor v2))
	       (+ (ycor v1) (ycor v2))))

(define (scale-vect vect fact)
  (make-vector (* (xcor vect) fact)
	       (* (ycor vect) fact)))

(define (-vect v1 v2)
  (+vect v1 (scale-vector v2 -1)))

(define (rotate-vect v angle)
  (let ((c (cos angle))
	(s (sin angle)))
    (make-vector
     (- (* c (xcor v))
	(* s (ycor v)))
     (+ (* c (ycor v))
	(* s (xcor v))))))
	
(define (make-pict seglist)
  (lambda (rect)
    (for-each
     (lambda (segment)
       (let ((b (start-segment segment))
	     (e (end-segment segment)))
	 (draw-line rect
		    (xcor b)
		    (ycor b)
		    (xcor e)
		    (ycor e))))
     seglist)))

(define (rotate90 pict)
  (lambda (rect)
    (pict (make-rectangle
	   (+vect (origin rect)
		  (horiz rect))
	   (vert rect)
	   (scale-vector
	    (horiz rect) -1)))))
	   
    
    
	  
  

(define p1 (make-vector 2 3))
(define p2 (make-vector 5 4))
(define s1 (make-segment p1 p2))

