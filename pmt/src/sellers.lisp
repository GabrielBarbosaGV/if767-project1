(require 'util (or (probe-file #p"util.fasl") (probe-file #p"util.lisp")))

(defun sellers (text pattern error-size)
  (let* ((n (length text))
	 (m (length pattern))
	 (occ nil)
	 (d (cons
	     (vector-range :end (1+ m))
	     (make-array (1+ m) :initial-element 0)))
	 (cur (cdr d))
	 (prev (car d)))
    (when (<= (aref prev m) error-size)
      (push 0 occ))
    (do ((j 1 (1+ j))) ((> j n) (nreverse occ))
      (do ((i 1 (1+ i))) ((> i m))
	(let ((diag
	       (+ (aref prev (1- i))
		  (phi (aref pattern (1- i))
		       (aref text (1- j)))))
	      (left
	       (1+ (aref prev i)))
	      (up
	       (1+ (aref cur (1- i)))))
	  (setf (aref cur i) (min diag left up))))
      (when (<= (aref cur m) error-size)
	(push (1- j) occ))
      (rotatef cur prev))))
