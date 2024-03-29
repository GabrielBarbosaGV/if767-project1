(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))
(require 'borders (or (probe-file "borders.fasl") (probe-file "borders.lisp")))

(defun kmp-increment (pattern)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 3))
	   (type string pattern))
  (let ((borders (get-borders pattern)))
    (declare (type (vector integer) borders))
    (lambda (mp &rest r) ;mp: Mismatch position
      (declare (ignore r))
      (1+ (aref borders (max mp 0))))))

(defun knuth-morris-pratt (text pattern)
  (declare (type string text pattern))
  (get-occurrences
   text pattern
   :increment-function (kmp-increment pattern)))
