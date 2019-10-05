(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))

(defun brute-force (text pattern)
  "Finds matches between text and pattern by brute-force"
  (get-occurrences
   text pattern
   :increment-function
   #'(lambda (&rest r)
       (declare (ignore r)) 1)))
