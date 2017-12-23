(defun tut () (load "house.lisp"))

(setf *house
      `((type terrace)
	(rooms ((kitchen ((dimensions (20 12))
			  (appliances (cooker fridge dishwasher))))
		(bedroom ((dimensions (15 21))
			  (features ((double-bed washbasin)))))))
	(garden (pond lawn shed))))

(defun get-property-value (property association-list)
  (second (assoc property association-list)))

(defun get-rooms (house)
  (get-property-value `rooms house))

(defun get-room (room house)
  (get-property-value room (get-rooms house)))
