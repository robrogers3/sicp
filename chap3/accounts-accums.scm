(define (err x y)
  (display x)(display y)(newline)
  )
(define (make-account password balance)
  (define (call-cops . x)
    'calling-cops)
  (define attempts (make-accumulator 0))
  (define (reset-attempts) (attempts (- (attempts 0))))
  (define (withdraw amount)
    (if (>= balance amount)
	(begin
	  (set! balance (- balance amount))
	  balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (display 'attempts)(display (reset-attempts))(newline)
    (if (eq? pass password)
	(begin
	  (reset-attempts)
	  (cond
	   ((eq? m 'withdraw) withdraw)
	   ((eq? m 'deposit) deposit)
	   (else
	    (error "Unknown request make-account" m))))
	(cond ((> (attempts 0) 2) call-cops)
	      (else
	       (attempts 1)
	       (lambda (x)
		 (display x)
		 (err "in correct password for account " m))))))
  dispatch)
(define (make-joint acc account-password joint-password)
  (define (dispatcher given-password message)
    (cond ((not (eq? given-password joint-password)) (err "incorrect pass for joint account" joint-password))
	  ((eq? message 'withdraw)
	   (acc account-password 'withdraw))
	  (else
	   (display 'else)
	   (display account-password)
	   (display joint-password)
	   (display message)
	   (newline))))
  dispatcher)
(define pauls-acc (make-account 'secret 100))
(define peters-acc (make-joint pauls-acc 'secret 'joint))
(define (make-accumulator init)
  (define (accum val)
    (set! init (+ init val))
    init
    )
  accum)

(define A (make-accumulator 5))

(define (make-monitored proc)
  (define times 0)
  (define (runner arg)
    (set! times (+ times 1))
    (proc arg))
  (define (dispatch arg)
    (cond
     ((eq? arg 'times) times)
     (else (runner arg))
     ))
  dispatch)

(define s (make-monitored square))

(define (make-accumulator acc)
  (lambda (x)
    (set! acc (+ x acc))
    acc))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (random 100) (random 100)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-rang low high)
  (let ((range (- high low)))
    (display range)
    (+ low (random range))))

(define random-init 0)
(define (rand-update x) (+ x 1))
(define x 10)
(define rand
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-value) (set! x new-value)))))
    dispatch))


(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (* (- x2 x1)
        (- y2 y1))
     (monte-carlo trials P)))

(define (in-circle)
  (>= 1 (+ (square (random-in-range -1.0 1.0))
           (square (random-in-range -1.0 1.0)))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-pi)
  (estimate-integral in-circle -1.0 1.0 -1.0 1.0 1000))

;; monte carlo procedure
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (P x y)
  (< (+ (square (- x 5)) (square (- y 7))) (square 3)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (monte-carlo trials experiment))

 (define pi-approx
   (/ (* (estimate-integral P 2.0 8.0 4.0 10.0 10000) 36)
      9.0))
;;franction of rect in circle = area of circle,
;;% of rect in cicrle * area of rect = areeae o
;;area = pi r2
;;pi = area/r2
;; 866-703-4169

;;exec 3.8
(define func
  (let ((state 1))
    (lambda (n)
      (set! state (* state n))
      state)))
(define (make-withdraw initial)
  (let ((balance initial))
    (lambda (amount)
      (if (>= balance amount)
	  (begin
	    (set! balance (- balance amount))
	    balance)
	  "insufficient funds"))))
