(defun tut () (load "expert.lisp"))

;;; knowledge base

;; known rules
(setf *rules `
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

;; known goals
(setf *goals `(cheetah tiger giraffe zebra ostrich penguin albatross))

;; working memory
;; initially stores users known facts
;; facts that are inferred kept on being added until either:
;; - a goal is found
;; - no untriggered rules exist
(setf *facts ())
;i.e. (setf *facts `((black-stripes y) (hair y) (give-milk y) (hoofs y)))

;;; main mehods

(defun run (rules goals)
  (forward-chain rules goals))

;; forward chaining reasoning
(defun forward-chain (rules goals)
  (let ((goal nil)
	(prev-triggered-rules nil)
	(triggered-rules rules) 
	(untriggered nil))
    (unless (loop while (and (not untriggered) (not goal))
		  do
		  (setf goal
			(fire-rules
		    (setf triggered-rules
			  (get-triggered-rules rules *facts))
		    goals))
		  (setf untriggered (equal prev-triggered-rules triggered-rules))
		  (setf prev-triggered-rules triggered-rules))
      goal)))

;;; main helper methods

;; add conclusions of triggered rules to working memory
(defun fire-rules (triggered-rules goals)
  (goal-known
   (dolist (rule triggered-rules *facts)
     (setf *facts (add-fact (list (get-conclusion rule) `y))))))
	      
;; returns all rules that are triggered
(defun get-triggered-rules (rules facts)
  (let ((triggered-rules nil))
    (dolist (rule rules triggered-rules)
      (when (triggered-rule rule facts)
	(setf triggered-rules (cons rule triggered-rules))))))

;; check if goal is known in working memory
(defun goal-known (facts)
  (let ((found-goal nil))
    ;; check working memory with all known goals
    (dolist (goal *goals found-goal)
      (when (assoc goal facts)
	(setf found-goal goal)))))

;;; main helper methods' helper methods

(defun get-conclusion (rule)
  (first rule))

(defun get-conditions (rule)
  (second rule))

(defun get-yes-conditions (rule)
  (let ((conditions nil))
    (dolist (condition (get-conditions rule) conditions)
      (when (eql (second condition) `y)
	(setf conditions (cons condition conditions))))))

(defun get-no-conditions (rule)
  (let ((conditions nil))
    (dolist (condition (get-conditions rule) conditions)
      (when (eql (second condition) `n)
	(setf conditions (cons condition conditions))))))

(defun conclusion-known (rule facts)
  (assoc (get-conclusion rule) facts))

(defun condition-known (condition facts)
  (assoc (first condition) facts))

(defun condition-true (condition facts)
  (equal condition (condition-known condition facts)))

(defun add-fact (fact)
  (unless (assoc (first fact) *facts)
      (setf *facts (cons fact *facts))))

(defun get-rule (conclusion rules)
  (assoc conclusion rules))

;; a triggered rule:
;; - is not known by working memory
;; - shares conditions with working memory
;; i.e. given rule contains related information (found by inference)
(defun triggered-rule (rule facts)
  (unless (conclusion-known rule facts)
    (dolist (condition (get-conditions rule) rule)
      (unless (condition-true condition facts)
	(setf rule nil)))))
