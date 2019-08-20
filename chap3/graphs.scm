;; make-graph-entry Node, list<Node>, Contents -> entry
(define (make-graph-entry node children contents)
  (list 'graph-entry node children contents))
(define (graph-entry? entry)
  (and (pair? entry) (eq? 'graph-entry (car entry))))

(define (graph-entry->node entry)
  (if (not (graph-entry? entry))
      (error "obj not an entry" entry)
      (first (cdr entry))))
;;Entry -> list<node>
(define (graph-entry->children entry)
  (if (not (graph-entry? entry))
      (error "obj not an graph entry" entry)
      (second (cdr entry))))

(define (graph-entry->contents entry)
  (if (not (graph-entry? entry))
      (error "obj not an graph entry" entry)
      (third (cdr entry))))

(define (make-graph entries)
  (cons 'graph entries))

(define (graph? graph)
  (and (pair? graph) (eq? 'graph (car graph))))

(define (graph-entries graph)
  (if (not (graph? graph))
      (error "obj is nnot a graph")
      (cdr graph)))


(define (graph-root graph)
  (let ((entries (graph-entries graph)))
    (if (null? entries)
	#f
	(graph-entry->node (car entries)))))

(define ge1 (make-graph-entry 'doc1 (list 'doc2 'doc3) "doc1 links to doc2, doc3"))
(define ge2 (make-graph-entry 'doc2 (list 'doc1) "doc2 links to doc1"))
(define ge3 (make-graph-entry 'doc3 (list 'doc2 'doc1) "doc3 links to doc1 n doc2"))
(define g1 (make-graph (list ge1 ge2 ge3)))
(graph-entry->node ge1)	     
(graph-entry->children ge1)
(define (depth-first graph goal? children)
  (let ((to-be-visited '()))
    (define (where-next? here)
      (set! to-be-visited (append (children graph here) to-be-visited))
      (cond
       ((goal? here) #t)
       ((null? to-be-visited) #f)
       (else
	(let ((next (car to-be-visited)))
	  (set! to-be-visited (cdr to-be-visited))
	  next))))
    where-next?))

(define (breadth-first graph goal? children)
  (let ((to-be-visited '()))
    (define (where-next here)
      (set! to-be-visited (append to-be-visited (children graph here)))
      (cond
       ((goal? here) #t)
       ((null? to-be-visited) #f)
       (else
	(let ((next (car to-be-visited)))
	  (set! to-be-visited (cdr to-be-visited))
	  next))))
    where-next?))

(define (depth-first graph goal? children)
  (let ((mark-procedures (make-mark-procedures)))
    (let ((deja-vu? (car mark-procedures))
	  (node-visited! (cadr mark-procedures))
	  (to-be-visited '()))
      (define (try-node candidates)
	(cond
	 ((null? candidates) #f)
	 ((deja-vu? (car candidates))
	  (try-nodes (cdr candidates)))
	 (else
	  (set! to-be-visited (cdr candidates))
	  (car candidates))))
      (define (where-next? here)
	(node-visited! here)
	(set! to-be-visited (append (childgraph here) to-be-visited))
	(if (goal? here) #t
	    (try-node to-be-visited)))
      where-next?)))
	    
      
