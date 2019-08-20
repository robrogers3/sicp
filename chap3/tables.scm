(define (asc key records)
  ;;(display 'asc)(display key)(newline)
  ;; (if (null? records) 'nullasc
  ;;     (begin
  ;; 	(display (caar records))(newline)))
  (cond ((null? records) false)
	((equal? key (caar records))(car records))
	(else (asc key (cdr records)))))
(define (lookup key table)
  (let ((record (asc key (cdr table))))
    (if record
	(cdr record)
	false)))
(define (insert! key value table)
  (let ((record (asc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define tbl1 (make-table))

(define (lookup-2d key1 key2 table)
  (let ((subtable (asc key1 (cdr table))))
    (if subtable
	(let ((record (asc key2 (cdr subtable))))
	  (if record
	      (cdr record)
	      false))
	false)))

(define (insert-2d! key1 key2 value table)
  (let ((subtable (asc key1 (cdr table))))
    (if subtable
	(let ((record (asc key2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key1 (cons key2 value))
			(cdr table))))))
(define tbl2 (make-table))


(define (make-table-local)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (asc key-1 (cdr local-table))))
        (if subtable
            (let ((record (asc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (asc key-1 (cdr local-table))))
        (if subtable
            (let ((record (asc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      local-table)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define op-table (make-table-local))
(define putt (op-table 'insert-proc!))
(define gett (op-table 'lookup-proc))

(putt 'alpha 'a 97)
(putt 'alpha 'b 98)
(putt 'math '+ 43)
(putt 'math '* 45)
(define mm (putt 'math '- 42))
(gett 'math '-)
(gett 'alpha 'a)
(gett 'alpha 'b)
;;(putt 'sym 'at '@)
;;(define xx (list (list 'numbers '()) (list 'letters (list 'a-n (cons 'a 97)))))
(define mathT (cons 'math (list (cons '* 45) (cons '+  43))))
(define tbll
  '(*table* (math (- . 42) (* . 45) (+ . 43)) ((alpha (b . 98) (a . 97)))))
(define (lookmeup keys)
  (define (recur key remaining tbl)
    (let ((sub (asc key tbl)))
      (if sub
	  (begin
	    (display 'sub)(display sub)(newline)
	    (cond
	     ((null? remaining) (cdr sub))
	     (else (recur (car remaining) (cdr remaining) (cdr sub)))))
	  'remaining)))
  (recur (car keys) (cdr keys) (cdr tbll)))

;; node (k,v) pair


(define (make-bin-table)
  (define local-table '())
  (define key=? equal?)
  (define (key<? key1 key2)
    (cond ((and (string? key1)
                (string? key2)) (string<? key1 key2))
          ((and (number? key1)
                (number? key2)) (< key1 key2))
          ((and (char? key1)
                (char? key2)) (char<? key1 key2))
          (else (error "Unsupported key types -- KEY<?" key1 key2))))
  ;;(define key<? <)
  (define get-value cdr)
  (define get-key car)
  (define make-record cons)
  (define entry car)
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))
  (define (element-of-set given-key set)
    (display set)(newline)
    (if (null? set)
	false
	(let ((entry-key (get-key (entry set))))
	  (cond
	   ((key=? given-key entry-key) (entry set))
	   ((key<? given-key entry-key) (element-of-set given-key (left-branch set)))
	   (else (element-of-set given-key (right-branch set)))))))
  (define (adjoin-set given-key value set)
    (if (null? set)
	(make-tree (make-record given-key value) '() '())
	(let ((entry-key (get-key (entry set))))
	  (cond 
	   ((key=? given-key entry-key) set)
	   ((key<? given-key entry-key)
	    (make-tree
	     (entry set)
	     (adjoin-set given-key value (left-branch set))
	     (right-branch set)))
	   (else
	    (make-tree
	     (entry set)
	     (left-branch set)
	     (adjoin-set given-key value (right-branch set))))))))

  (define (insert! key val)
    (let ((record (lookup key local-table)))
      ;;(display 'innsert-record-found)(display record)(newline)
      (cond (record
	     (display 'insert-record)(display record)(newline)
	     (set-cdr! record val))
	  (else
	   (set! local-table (adjoin-set key val local-table)))))
    local-table)
    
      
  (define (lookup given-key table)
    (element-of-set given-key table))

  (define (print)
    (display local-table)
    #t)
    
  
  (define (dispatch m)
    (cond ((eq? m 'insert-proc) insert!)
	  ((eq? m 'lookup-proc) (lambda (key)
				  (let ((record (lookup key local-table)))
				    (if record
					(get-value record)
					'not-found))))
				    
	  ((eq? m 'print) print)
	  (else "Unknown op for table" m)))
  dispatch)

(define num-tbl (make-bin-table))
(define puts (num-tbl 'insert-proc))
(define gets (num-tbl 'lookup-proc))
(define print (num-tbl 'print))
(puts 1 'one)
(puts 2 'two)
(puts 3 'three)
(puts 4 'four)
(puts 5 'five)
(gets 1)
(gets 3)
(print)
(define str-tbl (make-bin-table))
(define puts (str-tbl 'insert-proc))
(define gets (str-tbl 'lookup-proc))
(define print (str-tbl 'print))
(puts "one" 1)
(puts "two" 2)






