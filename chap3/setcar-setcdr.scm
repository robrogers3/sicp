(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
(car (last-pair x))
(define w (append! x y))
w
(cdr x)
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (myst x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (newline)
          (set-cdr! x y)
          (display temp)(newline)
          (display x)
          (newline)
          (loop temp x))))
  (loop x '()))

;;(cdr x)
;;(myst x)
;;y <- (b c d); x <- a
;;y <- (c d) x <- b a
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow))
z1
z2

(set-to-wow! z1)
(set-to-wow! z2)
z2
(eq? (car z1) (cdr z1))
(cdr z1)
(define (count-pairs-wrong l)
  (if (not (pair? l))
      0
      (+ (count-pairs-wrong (car l)) (count-pairs-wrong (cdr l))
         1)))

(define (count-pairs! l)
  (define visited '())
  (define (visit x)
    (if (and (pair? x) (not (memq x visited)))
        (begin
          (display visited)(newline)
          (set! visited (cons x visited))
          (display visited)(newline)(newline)
          (+ (visit (car x)) (visit (cdr x)) 1))
        0))
  (visit l))

(define (count-pairs x)
  (car
   (let go ((x x)
            (visited '()))
     (if (and (pair? x) (not (memq x visited)))
         (let* ((p-car (go (car x) (cons x visited)))
                (p-cdr (go (cdr x) (cdr p-car))))
           (cons (+ (car p-car) (car p-cdr) 1) (cdr p-cdr)))
         (cons 0 visited)))))
