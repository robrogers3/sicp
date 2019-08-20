;;Collapsing Intervals
;;; useful for managing timeslots!

(define (makeInterval start end)
  (cons start end))

(define (intervalStart interval)
  (car interval))

(define (intervalEnd interval)
  (cdr interval))

(define (overlaps interval1 interval2)
  (if (> (intervalStart interval1) (intervalStart interval2))
      (or (startsDuring interval2 interval1) (endsBefore interval2 interval1))
      (or (startsDuring interval1 interval2) (endsBefore interval1 interval2))))

;;interval2 starts before interval1 ends
(define (startsDuring interval1 interval2)
  (<= (intervalStart interval2) (intervalEnd interval1)))


;;interval2 ends before iterval1 ends
(define (endsBefore interval1 interval2)
  (<= (intervalEnd interval2) (intervalEnd interval1)))

;;makes an interval from the greateast range of two intervals
(define (intervalFrom interval1 interval2)
  (makeInterval (min (intervalStart interval1) (intervalStart interval2))
		(max (intervalEnd interval1) (intervalEnd interval2))))



;;kick off merge
(define (mergeIntervals intervals)
  ;;merges curr and next interval if they ovelap
  (define (merge intervals)
    ;;can't overlap null or only one interval
    (if (or (null? intervals) (= 1 (length intervals)))
	intervals
	(if (overlaps (car intervals) (car (cdr intervals)))
	    (cons (intervalFrom (car intervals) (car (cdr intervals))) (merge (cddr intervals)))
	    (cons (car intervals) (merge (cdr intervals))))))
  

  ;;if attempting to merge yields same length then there are no overlaps
  (define (shouldMerge intervals)
    (not (= (length intervals) (length (merge intervals)))))

  ;;recursive merging, as updating the list of merged intervals may mean they need to be remerged.
  (define (mergeWhileNeeded intervals)
    (if (shouldMerge intervals)
	(mergeWhileNeeded (merge intervals))
	intervals))

  (display intervals)
  (newline)
  ;; should only need sorting once. but stuck it here anyway
  (mergeWhileNeeded (sort intervals
			  (lambda (x y) ( < (intervalStart x) (intervalStart y))))))

(define intervals1 (list (makeInterval 0 2) (makeInterval 3 4) (makeInterval 3 4) (makeInterval 10 14)))
(define intervals2 (list (makeInterval 0 1) (makeInterval 2 3) (makeInterval 4 5) (makeInterval 6 7)))

;;; usage
;; (mergeIntervals (append intervals1 intervals2))
;; (mergeIntervals intervals1)
