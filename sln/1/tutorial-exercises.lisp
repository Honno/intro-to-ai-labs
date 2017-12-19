(defun tut () (load "tutorial-exercises.lisp"))

(setf *opinion `(some of the earliest languages are still the greatest))

(setf *second-opinion `(lisp is a flexible language and good for symbol processing))

(setf *taylor-speak `(do i not like that))

(setf *proper-speak (append ;; (i do not like that)
		     (list ;; (i do)
		      (first (rest *taylor-speak)) ;; i
			     (first *taylor-speak)) ;; do
		     (rest (rest *taylor-speak)))) ;; (not like that)

(setf *proper-speak-2 (append ;; (i do not like that)
		      (list ;; (i do)
		       (second *taylor-speak) ;; i
		       (first *taylor-speak)) ;; do
		      (member `not *taylor-speak))) ;; (not like that)
