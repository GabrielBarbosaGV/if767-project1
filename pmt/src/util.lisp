(defun phi (a b) (if (eq a b) 0 1))

(defun compare-chars-by-position (first second position)
  (eq (aref first position) (aref second position)))

(defun get-mismatch-position (first second &key (backwards nil))
  "Get index of mismatch position between two equally long strings,
returns -1 if not found"
  (let* ((len (length first))
	 (initial-value (if backwards (1- len) 0))
	 (step-func (if backwards #'1- #'1+))
	 (exit-cond #'(lambda (i)
			(not (compare-chars-by-position
			      first second i))))
	 (test-func (if backwards
			#'(lambda (i)
			    (or (< i 0)
			     (funcall exit-cond i)))
		        #'(lambda (i)
			    (or (>= i len)
			     (funcall exit-cond i))))))
    (do ((i initial-value (funcall step-func i)))
	((funcall test-func i)
	 (if (= i len) -1 i))))) ;(= i len) -> no mismatch, return -1

(defun string-equals (first second)
  "Determines if two strings are equal"
  (= (get-mismatch-position first second) -1))

(defun get-text (filepath)
  "Reads file with with filepath = (elt *posix-argv* 1) and returns it as
a string"
  (with-open-file (in filepath)
    (let ((contents (make-string (file-length in))))
      (read-sequence contents in)
      contents)))

(defun vector-range (&key (start 0) (end 0) (step 1))
  "\
Returns vector with elements from start (inclusive) incremented
by step to end (exclusive)"
  (when (zerop step) (error 'zero-step))
  (let ((v (make-array (ceiling (/ (- end start) step)))))
    (do ((i 0 (1+ i)) (val start (+ val step))) ((>= val end) v)
	 (setf (aref v i) val))))

(defun get-occurrences
    (text pattern
     &key
       (backwards nil)
       (increment-function
	#'(lambda (&rest r) (declare (ignore r)) 1)))
  "\
Loops over text attempting to match pattern against it,
returning occurrences's indices, increment-function is used to
increment search index. If not provided, defaults to
incrementing by one, in equivalence to the
brute-force approach"
  (let* ((occ nil) ;occ: occurrences
	(lt (length text))
	(lp (length pattern))
	(lendiff (- lt lp)))
    (do ((i 0))
	 ((> i lendiff) (nreverse occ))
      (let* ((slice (subseq text i (+ i lp)))
	     (mp
	      (get-mismatch-position
	       slice pattern
	       :backwards backwards))) ;mp: mismatch position
	(when (= -1 mp) (push i occ))
	(incf i (funcall increment-function mp i))))))

(defun submit (occurrences)
  "Prints occurrences's values"
  (format t "~{~a ~}" occurrences))
