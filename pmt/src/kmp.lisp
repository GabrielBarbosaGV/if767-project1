(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))
(require 'borders (or (probe-file "borders.fasl") (probe-file "borders.lisp")))

(defun kmp-increment (pattern)
  (let ((borders (get-borders pattern)))
    (lambda (mp &rest r) ;mp: Mismatch position
      (declare (ignore r))
      (1+ (aref borders (max mp 0))))))

(defun knuth-morris-pratt (text pattern)
  (get-occurrences
   text pattern
   :increment-function (kmp-increment pattern)))
