(in-package :basic-interaction-parameters)

(define-constant +decay-by-use+                :use                         :test #'eql)

(define-constant +decay-by-turns+              :turns                       :test #'eql)

(define-constant +effect-when-used+            :when-used                   :test #'eql)

(define-constant +effect-when-worn+            :when-worn                   :test #'eql)

(define-constant +effect-when-consumed+        :when-consumed               :test #'eql)

(define-constant +poison-effect-faint+         :faint                       :test #'eql)

(define-constant +poison-effect-terror+        :terror                      :test #'eql)

(define-constant +poison-effect-berserk+       :berserk                     :test #'eql)

(define-constant +can-talk+                    :can-talk                    :test #'eq)

(define-constant +can-ask-for-help+            :can-ask-for-help            :test #'eq)

(define-constant +can-be-opened+               :can-be-opened               :test #'eq)

(define-constant +can-open+                    :can-open                    :test #'eq)

(define-constant +can-attack+                  :can-attack                  :test #'eq)

(define-constant +can-be-attacked+             :can-be-attacked             :test #'eq)

(define-constant +can-be-destroyed+            :can-be-destroyed            :test #'eq)

(define-constant +can-be-burned+               :can-be-burned               :test #'eq)

(define-constant +can-heal+                    :can-heal                    :test #'eq)

(define-constant +can-be-heal+                 :can-be-heal                 :test #'eq)

(define-constant +can-poison+                  :can-poison                  :test #'eq)

(define-constant +can-be-poisoned+             :can-be-poisoned             :test #'eq)

(define-constant +can-be-drunk+                :can-be-drunk                :test #'eq)

(define-constant +can-be-eaten+                :can-be-eaten                :test #'eq)

(define-constant +can-be-weared-arm+           :can-be-weared-arm           :test #'eq)

(define-constant +can-be-weared-head+          :can-be-weared-head          :test #'eq)

(define-constant +can-be-weared-neck+          :can-be-weared-neck          :test #'eq)

(define-constant +can-be-weared-feet+          :can-be-weared-feet          :test #'eq)

(define-constant +can-cut+                     :can-cut                     :test #'eq)

(define-constant +can-smash+                   :can-smash                   :test #'eq)

(define-constant +can-pierce+                  :can-pierce                  :test #'eq)

(define-constant +can-launch-bolt+              :can-lauch-bolt              :test #'eq)

(define-constant +can-launch-arrow+             :can-lauch-arrow             :test #'eq)

(define-constant +mounted-on-pole+             :mounted-on-pole             :test #'eq)

(define-constant +decay+                       :decay                       :test #'eq)

(define-constant +effects+                     :effects                     :test #'eq)

(define-constant +strength+                    :strength                    :test #'eq)

(define-constant +stamina+                     :stamina                     :test #'eq)

(define-constant +dexterity+                   :dexterity                   :test #'eq)

(define-constant +agility+                     :agility                     :test #'eq)

(define-constant +smartness+                   :smartness                   :test #'eq)

(define-constant +empaty+                      :empaty                      :test #'eq)

(define-constant +weight+                      :weight                      :test #'eq)

(define-constant +damage-point+                :damage-point                :test #'eq)

(define-constant +movement-point+              :movement-point              :test #'eq)

(define-constant +magic-point+                 :magic-point                 :test #'eq)

(define-constant +dodge-chance+                :dodge-chance                :test #'eq)

(define-constant +melee-attack-chance+         :melee-attack-chance         :test #'eq)

(define-constant +range-attack-chance+         :range-attack-chance         :test #'eq)

(define-constant +melee-attack-damage+         :melee-attack-damage         :test #'eq)

(define-constant +range-attack-damage+         :range-attack-damage         :test #'eq)

(define-constant +edge-weapons-chance-bonus+   :edge-weapons-chance-bonus   :test #'eq)

(define-constant +edge-weapons-damage-bonus+   :edge-weapons-damage-bonus   :test #'eq)

(define-constant +impact-weapons-chance-bonus+ :impact-weapons-chance-bonus :test #'eq)

(define-constant +impact-weapons-damage-bonus+ :impact-weapons-damage-bonus :test #'eq)

(define-constant +pole-weapons-chance-bonus+   :pole-weapons-chance-bonus   :test #'eq)

(define-constant +pole-weapons-damage-bonus+   :pole-weapons-damage-bonus   :test #'eq)

(define-constant +range-weapons-chance-bonus+  :range-weapons-chance-bonus  :test #'eq)

(define-constant +range-weapons-damage-bonus+  :range-weapons-damage-bonus  :test #'eq)

(define-constant +unlock-chance+               :unlock-chance               :test #'eq)

(define-constant +deactivate-trap-chance+      :deactivate-trap-chance      :test #'eq)

(define-constant +reply-attack-chance+         :reply-attack-chance         :test #'eq)

(define-constant +ambush-attack-chance+        :ambush-attack-chance        :test #'eq)

(define-constant +spell-chance+                :spell-chance                :test #'eq)

(define-constant +attack-spell-chance+         :attack-spell-chance         :test #'eq)

(define-constant +heal-damage-points+          :heal-damage-points          :test #'eq)

(define-constant +healing-effects+             :healing-effects             :test #'eq)

(define-constant +heal-poison+                 :heal-poison                 :test #'eq)

(define-constant +heal-berserk+                :heal-berserk                :test #'eq)

(define-constant +heal-faint+                  :heal-faint                  :test #'eq)

(define-constant +heal-terror+                 :heal-terror                 :test #'eq)

(define-constant +cause-poison+                :cause-poison                :test #'eq)

(define-constant +cause-berserk+               :cause-berserk               :test #'eq)

(define-constant +cause-faint+                 :cause-faint                 :test #'eq)

(define-constant +cause-terror+                :cause-terror                :test #'eq)

(define-constant +immune-poison+               :immune-poison               :test #'eq)

(define-constant +immune-berserk+              :immune-berserk              :test #'eq)

(define-constant +immune-faint+                :immune-faint                :test #'eq)

(define-constant +immune-terror+               :immune-terror               :test #'eq)

(define-constant +magic-effects+               :magic-effect                :test #'eq)

(defun effect-unlimited-p (val)
  (not (numberp val)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun valid-keyword-p (probe &rest availables)
    (find probe availables :test #'eq))

  (defun standard-decay-fn (get-decay-object-fn)
    #'(lambda (object)
	(let ((decay-object (funcall get-decay-object-fn object)))
	  (decf (points decay-object) 1)
	  (> (points decay-object) 0))))

  (defclass decay-parameters ()
    ((when-decay
      :initform :use
      :initarg   :when-decay
      :accessor when-decay)
     (points
      :initform 0
      :initarg   :points
      :accessor points)
     (decay-fn
      :initform :standard-decay-fn
      :initarg  :decay-fn
      :accessor decay-fn)
     (leaving-message
      :initform "Farewell..."
      :initarg   :leaving-message
      :accessor leaving-message)))

  (defmethod print-object ((object decay-parameters) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "when? ~a points ~a fn ~a msg ~s"
	      (when-decay      object)
	      (points          object)
	      (decay-fn        object)
	      (leaving-message object))))

  (defmethod make-load-form ((object decay-parameters) &optional environment)
    (make-load-form-saving-slots object
				 :slot-names '(when-decay points leaving-message decay-fn)
				 :environment environment)))

(defmacro define-decay (params)
  (let* ((parameters (misc:build-plist params))
	 (points     (cdr (assoc :points   parameters)))
	 (when-decay (cdr (assoc :duration parameters)))
	 (message    (cdr (assoc :message  parameters))))
    (when (null points)
      (warn (_ "Interation: No points for decay, using 1000.")))
    (when (null when-decay)
      (warn (_ "Interation: No decay condition specified, using :turns.")))
    (when (null message)
      (warn (_ "Interation: No message.")))
    (when (not (valid-keyword-p when-decay +decay-by-use+ +decay-by-turns+))
      (error (format nil (_ "Invalid decay condition ~a, expected ~a")
		     when-decay (list +decay-by-use+ +decay-by-turns+))))
    (make-instance 'decay-parameters
		   :when-decay      (or when-decay +decay-by-turns+)
		   :points          (or points     1000)
		   :leaving-message (or message " "))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass effect-parameters ()
    ((trigger
      :initform :use
      :initarg  :trigger
      :accessor trigger)
     (duration
      :initform 1000000
      :initarg  :duration
      :accessor duration)
     (modifier
      :initform 0
      :initarg  :modifier
      :accessor modifier)))

    (defmethod print-object ((object effect-parameters) stream)
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "when? ~a duration ~a mod ~a"
		(trigger object)
		(duration    object)
		(modifier    object))))

    (defmethod make-load-form ((object effect-parameters) &optional environment)
      (make-load-form-saving-slots object
				   :slot-names '(trigger duration modifier)
				   :environment environment))

    (defmethod description-for-humans ((object effect-parameters))
      (format nil "~@d (~a)" (modifier object) (if (effect-unlimited-p (duration object))
						   (_ "unlimited")
						   (format nil
							   (_ "~d turns")
							   (duration object))))))

(defmacro define-effect (params)
  (let* ((parameters  (misc:build-plist params))
	 (trigger     (cdr (assoc :trigger parameters)))
	 (modifier    (cdr (assoc :modifier    parameters)))
	 (duration    (cdr (assoc :duration    parameters))))
    (when (null trigger)
      (warn (_ "Interation: No activation trigger specified for effect, using \":use.\"")))
    (when (null modifier)
      (warn (_ "Interation: No modifier specified for effect, using 0.")))
    (when (null duration)
      (warn (_ "Interation: No duration specified for effect, using :unlimited.")))
    (when (not (valid-keyword-p trigger +effect-when-worn+ +effect-when-used+
				+effect-when-consumed+))
      (error (format nil (_ "Invalid trigger ~a, expected ~a")
		     trigger (list +effect-when-worn+ +effect-when-used+
				   +effect-when-consumed+))))
    (make-instance 'effect-parameters
		   :trigger  (or trigger  +effect-when-used+)
		   :duration (or duration :unlimited)
		   :modifier (or modifier 0))))

(defmacro define-effects (&rest parameters)
  (let ((params (misc:build-plist parameters)))
    `(list ,@(loop for i in params collect
		  `(cons ,(car i) ,(if (consp (cdr i))
				       (cadr i)
				       (cdr i)))))))

(defun chance->chance-for-human (c)
  (* 100 c))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass parameters-with-chance ()
    ((chance
      :initform 0.0
      :initarg  :chance
      :accessor chance)))

  (defmethod make-load-form ((object parameters-with-chance) &optional environment)
      (make-load-form-saving-slots object
				   :slot-names '(chance)
				   :environment environment)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass healing-effect-parameters (parameters-with-chance)
    ((trigger
      :initform :use
      :initarg  :trigger
      :accessor trigger)
     (duration
      :initform 1000000
      :initarg  :duration
      :accessor duration)))

    (defmethod print-object ((object healing-effect-parameters) stream)
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "when? ~a duration ~a chance ~a"
		(trigger  object)
		(duration object)
		(chance   object))))

    (defmethod make-load-form ((object healing-effect-parameters) &optional environment)
      (make-load-form-saving-slots object
				   :slot-names '(trigger duration)
				   :environment environment))

    (defmethod description-for-humans ((object healing-effect-parameters))
      (format nil "~a chance: ~a%"
	      (if (effect-unlimited-p (duration object))
		  (_ "unlimited")
		  (format nil (_ "~d turns") (duration object)))
	      (chance->chance-for-human (chance object)))))

(defmacro define-healing-effect (params)
  (let* ((parameters (misc:build-plist params))
	 (trigger    (cdr (assoc :trigger  parameters)))
	 (duration   (cdr (assoc :duration parameters)))
	 (chance     (cdr (assoc :chance   parameters))))
    (when (null trigger)
      (warn (_ "Interation: No activation trigger specified for healing effect, using \":use.\"")))
    (when (null chance)
      (warn (_ "Interation: No chance specified for healing effect, using 0")))
    (when (null duration)
      (warn (_ "Interation: No duration specified for effect, using 1000000.")))
    (when (not (valid-keyword-p trigger +effect-when-worn+ +effect-when-used+
				+effect-when-consumed+))
      (error (format nil (_ "Invalid trigger ~a, expected ~a")
		     trigger (list +effect-when-worn+ +effect-when-used+
				   +effect-when-consumed+))))
    (make-instance 'healing-effect-parameters
		   :trigger  (or trigger  +effect-when-used+)
		   :duration (or duration 1000000)
		   :chance   (or chance  0.0))))

(defmacro define-healing-effects (&rest parameters)
  (let ((params (misc:build-plist parameters)))
    `(list ,@(loop for i in params collect
		  `(cons ,(car i) ,(if (consp (cdr i))
				       (cadr i)
				       (cdr i)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass magic-effect-parameters ()
    ((trigger
      :initform :use
      :initarg  :trigger
      :accessor trigger)
     (spell-id
      :initform :heal-1
      :initarg  :spell-id
      :accessor spell-id)))

    (defmethod print-object ((object magic-effect-parameters) stream)
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "when? ~a spell ~a"
		(trigger object)
		(spell-id object))))

    (defmethod make-load-form ((object magic-effect-parameters) &optional environment)
      (make-load-form-saving-slots object
				   :slot-names '(trigger spell-id)
				   :environment environment))

    (defmethod description-for-humans ((object magic-effect-parameters))
      (format nil "~a" (spell-id object))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass poison-effect-parameters (parameters-with-chance)
    ((points-per-turn
      :initform :points-per-turn
      :initarg  :points-per-turn
      :accessor  points-per-turn)
     (trigger
      :initform :use
      :initarg  :trigger
      :accessor trigger)))

    (defmethod print-object ((object poison-effect-parameters) stream)
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "when? ~a points? ~a chance? ~a"
		(trigger object)
		(points-per-turn object)
		(chance object))))

    (defmethod make-load-form ((object poison-effect-parameters) &optional environment)
      (make-load-form-saving-slots object
				   :slot-names '(points-per-turn trigger)
				   :environment environment))

    (defmethod description-for-humans ((object poison-effect-parameters))
      (format nil (_ "poison enemy ~a chance ~a")
	      (if (and (points-per-turn object)
		       (> (points-per-turn object) 0))
		  (format nil (_ "(~a damage per turn)") (points-per-turn object))
		  "")
	      (chance->chance-for-human (chance object)))))

(defmacro define-poison-effect (params)
  (let* ((parameters (misc:build-plist params))
	 (points     (cdr (assoc :points  parameters)))
	 (trigger    (cdr (assoc :trigger parameters)))
	 (chance     (cdr (assoc :chance  parameters))))
    (when (null points)
      (warn (_ "Interation: No points specified for poison effect, using \"-1\".")))
    (when (null trigger)
      (warn (_ "Interation: No activation trigger specified for poison effect, using \":use.\"")))
    (when (null chance)
      (warn (_ "Interation: No chance specified for healing effect, using 0")))
    (when (not (valid-keyword-p trigger +effect-when-worn+ +effect-when-used+
				+effect-when-consumed+))
      (error (format nil (_ "Invalid trigger ~a, expected ~a")
		     trigger (list +effect-when-worn+ +effect-when-used+
				   +effect-when-consumed+))))
    (when (not (numberp points))
      (error (format nil (_ "Invalid points ~a, expected a number") points)))
    (make-instance 'poison-effect-parameters
		   :points-per-turn  points
		   :trigger (or trigger  +effect-when-used+)
		   :chance  (or chance 0.0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass heal-damage-points-effect-parameters (parameters-with-chance)
    ((points
      :initform -10
      :initarg  :points
      :accessor  points)
     (trigger
      :initform :use
      :initarg  :trigger
      :accessor trigger)))

    (defmethod print-object ((object heal-damage-points-effect-parameters) stream)
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "when? ~a points? ~a chance ~a"
		(trigger object)
		(points  object)
		(chance->chance-for-human (chance object)))))

    (defmethod make-load-form ((object heal-damage-points-effect-parameters) &optional environment)
      (make-load-form-saving-slots object
				   :slot-names '(points trigger)
				   :environment environment))

    (defmethod description-for-humans ((object heal-damage-points-effect-parameters))
      (format nil (_ "heal ~a DMG")
	      (or (points object)
		  0))))

(defmacro define-heal-dmg-effect (params)
  (let* ((parameters (misc:build-plist params))
	 (points     (cdr (assoc :points parameters)))
	 (trigger    (cdr (assoc :trigger  parameters)))
	 (chance     (cdr (assoc :chance  parameters))))
    (when (null points)
      (warn (_ "Interation: No points specified for healing effect, using \"1\".")))
    (when (null trigger)
      (warn (_ "Interation: No activation trigger specified for healing effect, using \":use.\"")))
    (when (null chance)
      (warn (_ "Interation: No chance specified for healing effect, using 0")))
    (when (not (numberp points))
      (error (format nil (_ "Invalid points ~a, expected a number") points)))
    (when (not (valid-keyword-p trigger +effect-when-worn+ +effect-when-used+
				+effect-when-consumed+))
      (error (format nil (_ "Invalid trigger ~a, expected ~a")
		     trigger (list +effect-when-worn+ +effect-when-used+
				   +effect-when-consumed+))))
    (make-instance 'heal-damage-points-effect-parameters
		   :points-per-turn  points
		   :trigger  (or trigger +effect-when-used+)
		   :chance   (or chance  0.0))))

(defmacro define-magic-effect (params)
  (let* ((parameters (misc:build-plist params))
	 (trigger    (cdr (assoc :trigger parameters)))
	 (spell-id   (cdr (assoc :spell-id parameters))))
    (when (null trigger)
      (warn (_ "Interation: No activation trigger specified for magic effect, using \":use.\"")))
    (when (null spell-id)
      (warn (_ "Interation: No spell for magic effect, using \":heal-1.\"")))
    (when (not (valid-keyword-p trigger +effect-when-worn+ +effect-when-used+
				+effect-when-consumed+))
      (error (format nil (_ "Invalid trigger ~a, expected ~a")
		     trigger (list +effect-when-worn+ +effect-when-used+
				   +effect-when-consumed+))))
    (make-instance 'magic-effect-parameters
		   :trigger  (or trigger  +effect-when-used+)
		   :spell-id  (or spell-id :heal-1))))

(defparameter *interaction-parameters* nil)

(defmacro define-character (&rest params)
  `(setf *interaction-parameters* (list ,@params)))

(defmacro define-interaction (&rest parameters)
  (let ((params (misc:build-plist parameters)))
    `(setf *interaction-parameters*
	   (list ,@(loop for i in params collect
			`(cons ,(car i) ,(if (consp (cdr i))
					     (cadr i)
					     (cdr i))))))))

(defmacro with-interaction-parameters ((params file) &body body)
  `(let* ((*interaction-parameters* nil))
     (load ,file)
     (let ((,params *interaction-parameters*))
       ,@body)))
