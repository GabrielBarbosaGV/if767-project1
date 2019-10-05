(defun create-badchar-table (pattern)
  (let ((ascii-chars (make-array 2000 :initial-element -1)))
    (loop
       for i from 0
       for c across pattern do
	 (setf (aref ascii-chars (char-code c)) i))
    ascii-chars))

(defun get-badchar-hashfunc (pattern)
  (let ((badchar-table (create-badchar-table pattern)))
    (lambda (c) (aref badchar-table (char-code c)))))
