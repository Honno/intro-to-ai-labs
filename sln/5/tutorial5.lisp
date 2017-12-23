; be sure to load house-db.lisp first to access the variable

(defun tut () (load "tutorial5.lisp"))

(setf students `((dave 81) (dee 26) (dozy 58) (beaky 52) (mick 45) (tich 64)))

(defun sort-students (students min max)
  (let ((query nil))
    (dolist (stu-mark students query)
      (let ((mark (second stu-mark)))
	(when (and (< min mark) (> max mark))
	  (setf query (cons stu-mark query)))))))

(defun get-property-value (property association-list)
  (second (assoc property association-list)))

(defun get-rooms (house)
  (get-property-value `rooms house))

(defun get-room (room house)
  (get-property-value room (get-rooms house)))

(defun get-room-count (houses)
  (let ((house-counts nil))
    (dolist (house houses house-counts)
     (setf house-counts
      (cons (list (get-property-value `name house)
       (let ((count 0))
	 (dolist (room (get-rooms house) count)
	   (if (equal `bedrooms (first room))
	       (dolist (bedroom (second room) count)
		 (incf count 1))
	     (incf count 1))))) house-counts)))))

(defun sort-houses (houses min max)
  (let ((query nil))
    (dolist (house houses query)
      (let ((cost (get-property-value `cost house)))
	(unless (equal cost nil)
	  (when (and (< min cost) (> max cost))
	    (setf query (cons (list (get-property-value `name house) cost) query))))))))
