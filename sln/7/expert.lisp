(defun tut () (load "expert.lisp"))

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

(defun add-facts (fact facts)
  (unless (assoc (first fact) facts)
    (setf facts (cons fact facts))))

(defun get-rule (conclusion rules)
  (assoc conclusion rules))

;; find goal of working memory
(defun goal-known (facts)
  (let ((found-goal nil))
    ;; check working memory with all known goals
    (dolist (goal *goals found-goal)
      (unless found-goal
	(let ((rule (get-rule goal *rules)))
	  (let ((conditions (get-conditions rule))
		(yes-conditions (get-yes-conditions rule))
		(no-conditions (get-no-conditions rule)))
	    ;; check if all the goal's conditions match with working memory
	    (when
		;; check if each condition of goal matches working memory
		(let ((matched `T))
		  (dolist (condition conditions matched)
		    ;; when the condition matches working memory, add it to list of matches
		    (unless
			;; if condition is:
			;; - is a no-condition
			;; - is not matched in working memory
			;; then accept match unless condition exists in working memory
			;; otherwise accept match when condition is matched in working memory
			(let ((if-true (condition-true condition facts)))
			  (if (and (condition-known condition no-conditions) (not if-true))
			      (not (condition-true (list (first condition) `y) facts))
			    if-true))
		      (setf matched nil))))
	      (setf found-goal goal))))))))
				       
	    
;; a triggered rule:
;; - is not known by working memory
;; - shares conditions with working memory
;; i.e. given rule contains related information (found by inference)
(defun triggered-rule (rule facts)
  (unless (conclusion-known rule facts)
    (dolist (condition (get-conditions rule) rule)
      (unless (condition-true condition facts)
	(setf rule nil)))))
	      
;; returns all rules that are triggered
(defun get-triggered-rules (rules facts)
  (let ((triggered-rules nil))
    (dolist (rule rules triggered-rules)
      (when (triggered-rule rule facts)
	(setf triggered-rules (cons rule triggered-rules))))))

;; add conclusions of triggered rules to working memory
(defun fire-rules (triggered-rules goals facts)
  (goal-known
   (dolist (rule triggered-rules facts)
     (setf facts (add-facts (list (get-conclusion rule) `y) facts)))))

;; UNFINISHED
(defun forward-chain (triggered-rules rules goals)
  (let ((untriggered-rules triggered-rules)
	(triggered-rules rules)
	(goal nil))
    (while (and (not untriggered-rules) (not goal))
      (setf goal
	    (fire-rules
	     (get-triggered-rules rules *facts)
	     goals
	     *facts)))))
