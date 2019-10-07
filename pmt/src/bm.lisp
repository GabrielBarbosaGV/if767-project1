(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))
(require 'badchar (or (probe-file "badchar.fasl") (probe-file "badchar.lisp")))
(require 'goodsuffix (or (probe-file "goodsuffix.fasl") (probe-file "goodsuffix.lisp")))

(defun badchar-increment (text pattern)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 3))
	   (type string text pattern))
  (let ((badacc (get-badchar-hashfunc pattern)))
    (lambda (mp i)
      (declare (type integer mp i))
      (let ((char-pos
	     (funcall badacc (aref text (max 0 (+ mp i))))))
	(max 1 (- mp char-pos))))))

(defun goodsuffix-increment (pattern)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
	   (type string pattern))
  (let ((gs (get-goodsuffix-table pattern)))
    (lambda (mp &rest r)
      (declare (type integer mp))
      (declare (ignore r))
      (aref gs
	    (if (= -1 mp)
		(1- (length pattern))
		mp)))))
  
(defun boyer-moore-increment (text pattern)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
	   (type string text pattern))
  (let ((bi (badchar-increment text pattern))
	(gi (goodsuffix-increment pattern)))
    (lambda (&rest r)
      (max
       (apply bi r)
       (apply gi r)))))

(defun boyer-moore (text pattern)
  (declare (type string text pattern))
  (get-occurrences
   text pattern
   :backwards t
   :increment-function (boyer-moore-increment text pattern)))
