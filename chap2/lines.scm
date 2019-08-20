(define (average x y)
  (/ (+ x y) 2))
(define (make-point x y)
  (cons x y))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment point1 point2)
  (cons point1 point2))
  
(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint segment)
  ;; left pt x + right pt x / 2
  ;; left pt y + right pt y / 2
  (make-point (average (x-point (start-segment segment)) (x-point (end-segment segment)))
	      (average (y-point (start-segment segment)) (y-point (end-segment segment)))))


(define (make-rectangle point1 point2)
  (cons point1 point2))

(define (make-rectangle point width height)
  (cons point (cons width height)))
(define (rectangle-first-point rect)
  (car rect))

(define (rectangle-second-point rect)
  (cdr rect))

;;left bottom x,y right top x,y

;;(define (get-point rect x-selector y-selector)

(define (get-bottom-left rect)
  (let ((p1 (car rect))
	(p2 (cdr rect)))
    (make-point
     (min (x-point p1) (x-point p2))
     (min (y-point p1) (y-point p2)))))

(define (get-bottom-right rect)
  (let ((p1 (car rect))
	(p2 (cdr rect)))
    (make-point
     (max (x-point p1) (x-point p2))
     (min (y-point p1) (y-point p2)))))

(define (get-point rect x-selector y-selector)
  (let ((p1 (car rect))
	(p2 (cdr rect)))
    (make-point
     (x-selector (x-point p1) (x-point p2))
     (y-selector (y-point p1) (y-point p2)))))

(define (get-top-right rect)
  (get-point rect max max))

(define (get-top-left rect)
  (get-point rect max min))

(define (rectangle-width rect)
  ;;x2 - x1
  ;;(- (max x2 x1) (min x2 x1))
  (let ((x1 (x-point (rectangle-first-point rect)))
	(x2 (x-point (rectangle-second-point rect)))
	)
    (abs (- x2 x1))))

(define (rectangle-height rect)
  (let ((y1 (y-point (rectangle-first-point rect)))
	(y2 (y-point (rectangle-second-point rect))))
    (abs (- y2 y1))))

(define (rectangle-area rect)
  (* (rectangle-width rect) (rectangle-height rect)))
(define (rectangle-perimeter rect)
  (+ (* 2 (rectangle-height rect)) (* 2 (rectangle-width rect))))
;;(define (area rect))

;;
;;      . 
;;
;;.     (4,1)
;;- - - - -
