(defclass queue ()
    ((removal-list
      :initarg :removal-list
      :initform nil
      :accessor removal-list)
     (addition-list
      :initarg :addition-list
      :initform nil
      :accessor addition-list)))

(defgeneric queue-pop (queue))
(defgeneric queue-append (queue value))
(defgeneric queue-empty (queue))

(defmethod queue-pop ((queue queue))
  (cond
    ((and
      (eq () (removal-list queue))
      (eq () (addition-list queue)))
     ()) ;Return empty list if neither list has storage
    ((eq () (removal-list queue))
     (setf (removal-list queue)
	   (nreverse (addition-list queue))) ;Transform addition list into removal-list
     (setf (addition-list queue) ())
     (pop (removal-list queue))) 
    (t (pop (removal-list queue))))) ;Simply remove element from removal-list

(defmethod queue-append ((queue queue) value)
  (push value (addition-list queue)))

(defmethod queue-empty ((queue queue))
  (and
   (eq () (removal-list queue))
   (eq () (addition-list queue))))
