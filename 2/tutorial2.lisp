(defun tut () (load "tutorial2.lisp"))

(defun double (numbers)
  (reverse
   (let ((double-list nil))
    (dolist (element numbers double-list)
      (if (= element 1)
	(setf double-list (cons `one double-list))
	(setf double-list (cons (* 2 element) double-list)))))))

(defun extract-all (char list)
  (reverse
   (let ((extracted nil))
     (dolist (element list extracted)
       (if (equal element char)
	()
	(setf extracted (cons element extracted)))))))

(defun keep-vowels (list)
  (reverse
   (let ((vowels `(a e i o u)) (extracted nil))
     (dolist (element list extracted)
       (if (find element vowels)
	 (setf extracted (cons element extracted))
	 ())))))
  
(defun colour (colour colours)
   (let ((count 0))
     (dolist (element colours count)
       (if (equal element colour)
	 (incf count 1)
	 ()))))

(defun mauve (colours)
  (reverse
   (let ((colours-nu nil))
    (dolist (element colours colours-nu)
      (if (equal element `purple)
	(setf colours-nu (cons 'mauve colours-nu))
	(setf colours-nu (cons element colours-nu)))))))
