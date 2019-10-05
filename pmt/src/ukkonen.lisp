(require 'util (or (probe-file #p"util.fasl") (probe-file #p"util.lisp")))
(require 'hash-set (or (probe-file #p"hashset.fasl") (probe-file #p"hashset.lisp")))
(require 'queue (or (probe-file #p"queue.fasl") (probe-file #p"queue.lisp")))

(defun next-column (pattern
		    column
		    error-size
		    character)
  (let* ((m (length pattern))
	 (new-column (make-array (1+ m) :initial-element 0)))
    (do ((i 1 (1+ i))) ((> i m) new-column)
      (setf
       (aref new-column i)
       (min
	(1+ (aref column i))
	(1+ (aref new-column (1- i)))
	(+
	 (aref column (1- i))
	 (phi character (aref pattern (1- i))))
	(1+ error-size))))))


(defun make-finite-state-machine (pattern
				  error-size
				  alphabet)
  (let* ((m (length pattern))
	 (states (make-hash-table :test 'equalp))
	 (delta (make-hash-table :test 'equal))
	 (queue (make-instance 'queue))
	 (final (make-instance 'hash-set))
	 (q-zero (make-array (1+ m)))
	 (state-count 1))
    (do ((i 0 (1+ i))) ((> i m))
      (setf (aref q-zero i) (min i (1+ error-size))))
    (setf (gethash q-zero states) 0)
    (queue-append queue q-zero)
    (do () ((queue-empty queue) (cons delta final))
      (let* ((state (queue-pop queue))
	     (i-state (gethash state states)))
	(do ((i 0 (1+ i))) ((>= i (length alphabet)))
	  (let ((next-state (next-column
			     pattern
			     state
			     error-size
			     (aref alphabet i)))
		(i-next state-count))
	    (if (not (nth-value 1 (gethash next-state states)))
		(progn
		  (setf (gethash next-state states) state-count)
		  (incf state-count)
		  (queue-append queue next-state)
		  (when (<= (aref next-state m) error-size)
		    (hash-set-put final i-next)))
		(setf i-next (gethash next-state states)))
	    (setf (gethash (cons i-state (aref alphabet i)) delta)
		  i-next)))))))

(defun scan (delta-final text)
  (let ((cur 0)
	(n (length text))
	(occ nil))
    (destructuring-bind (delta . final) delta-final
      (when (hash-set-in final cur)
	(push 0 occ))
      (do ((j 0 (1+ j))) ((>= j n) (nreverse occ))
	(setf cur (gethash (cons cur (aref text j)) delta))
	(when (hash-set-in final cur)
	  (push j occ))))))

(defun ukkonen-scanner (pattern error-size)
  (let ((alphabet (make-array 200 :element-type 'character)))
    (do ((i 0 (1+ i))) ((>= i 200))
      (setf (aref alphabet i) (code-char i)))
    (let ((finite-state-machine
	   (make-finite-state-machine pattern error-size alphabet)))
	  (lambda (text) (scan finite-state-machine text)))))
