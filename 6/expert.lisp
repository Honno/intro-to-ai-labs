(defun tut () (load "expert.lisp"))

(setf *test-rule `(mammal ((hair y) (give-milk y))))

(setf *rules '
      ((mammal ((hair y)(give-milk y)))
       (bird ((feathers y)(lay-eggs y)))
       (carnivore ((mammal y) (eats-meat y)(pointed-teeth y) (forward-eyes y)))
       (carnivore ((mammal y)(eats-meat y)(claws y)))
       (ungulate ((mammal y)(hoofs y)))
       (ungulate ((mammal y) (chew-cud y)))
       (cheetah ((mammal y) (carnivore y) (tawney y) (dark-spots y)))
       (tiger   ((mammal y) (carnivore y) (tawney y) (black-stripes y)))
       (giraffe ((ungulate y) (long-neck y) (long-legs y) (dark-spots y)))
       (zebra ((ungulate y)(black-stripes y)))
       (ostrich ((bird y) (fly n) (long-neck y) (long-legs y)
                 (black-and-white-colour y)))
       (penguin ((bird y) (fly n) (swim y) (black-and-white-colour y)))
       (albatross ((bird y) (fly-well y)))))

(setf *goals '(cheetah tiger giraffe zebra ostrich penguin albatross))

(defun get-conclusion (rule)
  (first rule))

(defun get-conditions (rule)
  (second rule))

(defun get-yes-conditions (rule)
  (let ((conditions nil))
    (dolist (condition (get-conditions rule) conditions)
      (when (equal (second condition) `y)
	(setf conditions (cons condition conditions))))))

(defun get-no-conditions (rule)
  (let ((conditions nil))
    (dolist (condition (get-conditions rule) conditions)
      (when (equal (second condition) `n)
	(setf conditions (cons condition conditions))))))

(defun conclusion-known (rule facts)
  (assoc (get-conclusion rule) facts))

(defun condition-known (condition facts)
  (assoc (first condition) facts))

(defun condition-true (condition facts)
  (equal condition (condition-known condition facts)))

(defun triggered-rule (rule facts)
  (unless (conclusion-known rule facts)
    (dolist (condition (get-conditions rule) rule)
	(unless (condition-true condition facts)
	  (setf rule nil)))))

(defun get-triggered-rules (rules facts)
  (let ((triggered-rules nil))
    (dolist (rule rules triggered-rules)
      (let ((nu-rule (triggered-rule rule facts)))
	(when nu-rule
	  (setf triggered-rules (cons rule triggered-rules)))))))

(defun add-facts (fact facts)
  (cons fact facts))
