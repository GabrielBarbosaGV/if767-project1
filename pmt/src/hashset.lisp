(defclass hash-set ()
  ((inner-hash-table
    :initform (make-hash-table :test 'equal)
    :reader inner-hash-table)))


(defgeneric hash-set-in (hash-set value))
(defgeneric hash-set-put (hash-set value))
(defgeneric hash-set-remove (hash-set value))
(defgeneric hash-set-map (hash-set func))


(defmethod hash-set-in ((hash-set hash-set) value)
  (nth-value 1 (gethash value (inner-hash-table hash-set))))

(defmethod hash-set-put ((hash-set hash-set) value)
  (setf (gethash value (inner-hash-table hash-set)) t))

(defmethod hash-set-remove ((hash-set hash-set) value)
  (remhash value (inner-hash-table hash-set)))

(defmethod hash-set-map ((hash-set hash-set) func)
  (let ((new-hash-set (make-instance 'hash-set)))
    (maphash
     #'(lambda (a b)
	 (declare (ignore b))
	 (hash-set-put new-hash-set (funcall func a)))
     (inner-hash-table hash-set))
    new-hash-set))
