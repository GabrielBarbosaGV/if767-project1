(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))

(defun get-borders (pattern)
  "Returns border array for given pattern"
  (let* ((len (length pattern)) (borders (make-array len :initial-element 0)) (j 0))
    (cond
      ((= 0 len) #())
      ((= 1 len) #(0))
      (t
       (loop
	  for i from 1 below len
	  if (eql (aref pattern i) (aref pattern j)) do
	    (progn
	      (setf (aref borders i) (1+ j))
	      (incf j))
	  else do
	    (setf j (aref borders (max 0 (1- j)))))
       borders))))
