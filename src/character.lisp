;; dawn of the Era: a tactical game.
;; Copyright (C) 2015  cage

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :player-character)

(alexandria:define-constant +unknown-ability-bonus+  -5                                 :test #'=)

(alexandria:define-constant +starting-exp-points+    10                                 :test #'=)

(alexandria:define-constant +first-name+                   :first-name                  :test #'eq)

(alexandria:define-constant +last-name+                    :last-name                   :test #'eq)

(alexandria:define-constant +description+                  :description                 :test #'eq)

(alexandria:define-constant +portrait+                     :portrait                    :test #'eq)

(alexandria:define-constant +strength+                     :strength                    :test #'eq)

(alexandria:define-constant +stamina+                      :stamina                     :test #'eq)

(alexandria:define-constant +dexterity+                    :dexterity                   :test #'eq)

(alexandria:define-constant +agility+                      :agility                     :test #'eq)

(alexandria:define-constant +smartness+                    :smartness                   :test #'eq)

(alexandria:define-constant +empaty+                       :empaty                      :test #'eq)

(alexandria:define-constant +weight+                       :weight                      :test #'eq)

(alexandria:define-constant +damage-points+                :damage-points               :test #'eq)

(alexandria:define-constant +movement-points+              :movement-points             :test #'eq)

(alexandria:define-constant +magic-points+                 :magic-points                :test #'eq)

(alexandria:define-constant +dodge-chance+                 :dodge-chance                :test #'eq)

(alexandria:define-constant +melee-attack-chance+          :melee-attack-chance         :test #'eq)

(alexandria:define-constant +range-attack-chance+          :range-attack-chance         :test #'eq)

(alexandria:define-constant +melee-attack-damage+          :melee-attack-damage         :test #'eq)

(alexandria:define-constant +range-attack-damage+          :range-attack-damage         :test #'eq)

(alexandria:define-constant +edge-weapons-chance-bonus+    :edge-weapons-chance-bonus   :test #'eq)

(alexandria:define-constant +edge-weapons-damage-bonus+    :edge-weapons-damage-bonus   :test #'eq)

(alexandria:define-constant +impact-weapons-chance-bonus+  :impact-weapons-chance-bonus :test #'eq)

(alexandria:define-constant +impact-weapons-damage-bonus+  :impact-weapons-damage-bonus :test #'eq)

(alexandria:define-constant +pole-weapons-chance-bonus+    :pole-weapons-chance-bonus   :test #'eq)

(alexandria:define-constant +pole-weapons-damage-bonus+    :pole-weapons-damage-bonus   :test #'eq)

(alexandria:define-constant +unlock-chance+                :unlock-chance               :test #'eq)

(alexandria:define-constant +deactivate-trap-chance+       :deactivate-trap-chance      :test #'eq)

(alexandria:define-constant +reply-attack-chance+          :reply-attack-chance         :test #'eq)

(alexandria:define-constant +ambush-attack-chance+         :ambush-attack-chance        :test #'eq)

(alexandria:define-constant +spell-chance+                 :spell-chance                :test #'eq)

(alexandria:define-constant +attack-spell-chance+          :attack-spell-chance         :test #'eq)

(alexandria:define-constant +status+                       :status                      :test #'eq)

(alexandria:define-constant +race+                         :race                        :test #'eq)

(alexandria:define-constant +level+                        :level                       :test #'eq)

(alexandria:define-constant +exp-points+                   :exp-points                  :test #'eq)

(defparameter *standard-capital-characteristic* 200)

(defclass player-character (identificable)
  ((first-name
    :initform ""
    :initarg :first-name
    :accessor first-name
    :type string)
   (last-name
    :initform ""
    :initarg :last-name
    :accessor last-name
    :type string)
   (portrait
    :initform nil
    :initarg :portrait
    :accessor portrait
    :type texture:texture)
   (strength
    :initarg :strength
    :initform 0
    :accessor strength
    :type integer)
   (stamina
    :initarg :stamina
    :initform 0
    :accessor stamina
    :type integer)
   (dexterity
    :initarg :dexterity
    :initform 0
    :accessor dexterity
    :type integer)
   (agility
    :initarg :agility
    :initform 0
    :accessor agility
    :type integer)
   (smartness
    :initarg :smartness
    :initform 0
    :accessor smartness
    :type integer)
   (empaty
    :initarg :empaty
    :initform 0
    :accessor empaty
    :type integer)
   (weight
    :initarg :weight
    :initform 35
    :accessor weight
    :type integer)
   (damage-points
    :initarg :damage-points
    :initform 0
    :accessor damage-points
    :type integer)
   (movement-points
    :initarg :movement-points
    :initform 0
    :accessor movement-points
    :type integer)
   (magic-points
    :initarg :magic-points
    :initform 0
    :accessor magic-points
    :type integer)
   (dodge-chance
    :initarg :dodge-chance
    :initform 0
    :accessor dodge-chance
    :type integer)
   (melee-attack-chance
    :initarg :melee-attack-chance
    :initform 0
    :accessor melee-attack-chance
    :type integer)
   (range-attack-chance
    :initarg :range-attack-chance
    :initform 0
    :accessor range-attack-chance
    :type integer)
   (melee-attack-damage
    :initarg :melee-attack-damage
    :initform 0
    :accessor melee-attack-damage
    :type integer)
   (range-attack-damage
    :initarg :range-attack-damage
    :initform 0
    :accessor range-attack-damage
    :type integer)
   (edge-weapons-chance-bonus
    :initarg :edge-weapons-chance-bonus
    :initform +unknown-ability-bonus+
    :accessor edge-weapons-chance-bonus
    :type integer)
   (edge-weapons-damage-bonus
    :initarg :edge-weapons-damage-bonus
    :initform +unknown-ability-bonus+
    :accessor edge-weapons-damage-bonus
    :type integer)
   (impact-weapons-chance-bonus
    :initarg :impact-weapons-chance-bonus
    :initform +unknown-ability-bonus+
    :accessor impact-weapons-chance-bonus
    :type integer)
   (impact-weapons-damage-bonus
    :initarg :impact-weapons-damage-bonus
    :initform +unknown-ability-bonus+
    :accessor impact-weapons-damage-bonus
    :type integer)
   (pole-weapons-chance-bonus
    :initarg :pole-weapons-chance-bonus
    :initform +unknown-ability-bonus+
    :accessor pole-weapons-chance-bonus
    :type integer)
   (pole-weapons-damage-bonus
    :initarg :pole-weapons-damage-bonus
    :initform +unknown-ability-bonus+
    :accessor pole-weapons-damage-bonus
    :type integer)
   (unlock-chance
    :initarg :unlock-chance
    :initform +unknown-ability-bonus+
    :accessor unlock-chance
    :type integer)
   (deactivate-trap-chance
    :initarg :deactivate-trap-chance
    :initform +unknown-ability-bonus+
    :accessor deactivate-trap-chance
    :type integer)
   (reply-attack-chance
    :initarg :reply-attack-chance
    :initform +unknown-ability-bonus+
    :accessor reply-attack-chance
    :type integer)
   (ambush-attack-chance
    :initarg :ambush-attack-chance
    :initform +unknown-ability-bonus+
    :accessor ambush-attack-chance
    :type integer)
   (spell-chance
    :initarg :spell-chance
    :initform 0
    :accessor spell-chance
    :type integer)
   (attack-spell-chance
    :initarg :attack-spell-chance
    :initform 0
    :accessor attack-spell-chance
    :type integer)
   (status
    :initarg :status
    :initform :normal
    :accessor status)
   (race
    :initarg :race
    :initform :human
    :accessor race)
   (level
    :initarg :level
    :initform 1
    :accessor level)
   (exp-points
    :initarg :exp-points
    :initform 0
    :accessor exp-points)
   (basic-interaction-params
    :initform nil
    :initarg  :basic-interaction-params
    :accessor basic-interaction-params)))

(defmethod print-object ((object player-character) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
	    "name ~a ~a~%strength: ~a ~%stamina: ~a ~%dexterity: ~a ~%agility: ~a ~%smartness: ~a ~%empaty: ~a ~%weight: ~a ~%damage-points: ~a ~%movement-points: ~a ~%magic-points: ~a ~%dodge-chance: ~a ~%melee-attack-chance: ~a ~%range-attack-chance: ~a ~%melee-attack-damage: ~a ~%range-attack-damage: ~a ~%edge-weapons-chance-bonus: ~a ~%edge-weapons-damage-bonus: ~a ~%impact-weapons-chance-bonus: ~a ~%impact-weapons-damage-bonus: ~a ~%pole-weapons-chance-bonus: ~a ~%pole-weapons-damage-bonus: ~a ~%unlock-chance: ~a ~%deactivate-trap-chance: ~a ~%reply-attack-chance: ~a ~%ambush-attack-chance: ~a ~%spell-chance: ~a ~%attack-spell-chance: ~a ~%status: ~a  ~%race: ~a ~%level: ~a ~%exp-points: ~a~%interaction ~a"
	    (first-name                  object)
	    (last-name                   object)
	    (strength                    object)
	    (stamina                     object)
	    (dexterity                   object)
	    (agility                     object)
	    (smartness                   object)
	    (empaty                      object)
	    (weight                      object)
	    (damage-points               object)
	    (movement-points             object)
	    (magic-points                object)
	    (dodge-chance                object)
	    (melee-attack-chance         object)
	    (range-attack-chance         object)
	    (melee-attack-damage         object)
	    (range-attack-damage         object)
	    (edge-weapons-chance-bonus   object)
	    (edge-weapons-damage-bonus   object)
	    (impact-weapons-chance-bonus object)
	    (impact-weapons-damage-bonus object)
	    (pole-weapons-chance-bonus   object)
	    (pole-weapons-damage-bonus   object)
	    (unlock-chance               object)
	    (deactivate-trap-chance      object)
	    (reply-attack-chance         object)
	    (ambush-attack-chance        object)
	    (spell-chance                object)
	    (attack-spell-chance         object)
	    (status                      object)
	    (race                        object)
	    (level                       object)
	    (exp-points                  object)
	    (basic-interaction-params    object))))

(defmethod marshal:class-persistant-slots ((object player-character))
  (append  '(first-name
	     last-name
	     portrait
	     strength
	     stamina
	     dexterity
	     agility
	     smartness
	     empaty
	     weight
	     damage-points
	     movement-points
	     magic-points
	     dodge-chance
	     melee-attack-chance
	     range-attack-chance
	     melee-attack-damage
	     range-attack-damage
	     edge-weapons-chance-bonus
	     edge-weapons-damage-bonus
	     impact-weapons-chance-bonus
	     impact-weapons-damage-bonus
	     pole-weapons-chance-bonus
	     pole-weapons-damage-bonus
	     unlock-chance
	     deactivate-trap-chance
	     reply-attack-chance
	     ambush-attack-chance
	     spell-chance
	     attack-spell-chance
	     status
	     race
	     level
	     exp-points
	     basic-interaction-params)
	   (call-next-method)))

(defmethod serialize ((object player-character))
  (when (portrait object)
    (setf (texture:filename (portrait object))
	  (format nil "~a" (num:fnv-hash-32 (pixmap:bits (portrait object))))))
  (format nil "~s" (marshal:marshal object)))

(defmethod deserialize ((object player-character) file)
  (declare (ignore object))
  (let ((res (marshal:unmarshal (read-from-string (filesystem-utils:slurp-file file)))))
    (when (portrait res)
      (texture:gen-name-and-inject-in-database (portrait res)))
    res))

(defgeneric random-fill-slots (object capital characteristics))

(defgeneric lookup-basic-interaction (object key))

(defgeneric import-interaction-from-definition (object file))

(defgeneric object-keycode (object))

(defmethod random-fill-slots ((object player-character) capital characteristics)
  (loop for charact in characteristics do
       (when (> capital 0)
	 (let ((random-value (+ (caadr charact)
				(if (/= 0 (cadadr charact))
				    (num:lcg-next-in-range
				     (- (cadadr charact)) (cadadr charact))
				    0))))
	   (if (< random-value capital)
	       (progn
		 (incf (slot-value object (car charact)) random-value)
		 (decf capital random-value))
	       (progn
		 (incf (slot-value object (car charact)) capital)
		 (setf capital 0))))))
  capital)


(defmacro gen-make-player (class)
  (alexandria:with-gensyms (char rest-capital)
    `(defun ,(alexandria:format-symbol t "~@:(make-~a~)" class)
	 (capital race
	  &optional (charact nil) (slots '((strength (50 10))
					   (stamina  (40 10))
					   (dexterity (5 3))
					   (agility   (5 3))
					   (empaty    (2 0))
					   (smartness (2 0))
					   (weight    (25 10)))))
       (let* ((,char (or charact (make-instance 'player-character)))
	      (,rest-capital (random-fill-slots ,char capital slots)))
	 (setf (damage-points ,char)   (truncate (* (stamina ,char) 0.5))
	       (movement-points ,char) (truncate (* (agility ,char) 0.5))
	       (magic-points ,char) (truncate (/ (alexandria:lerp 0.1
								 (alexandria:lerp 0.5
										  (smartness ,char)
										  (empaty ,char))
								 (dexterity ,char))
						3))
	       (dodge-chance ,char) (truncate (max 0 (- (agility ,char) (/ (weight ,char) 2))))
	       (melee-attack-chance ,char) (alexandria:lerp 0.3 (strength ,char) (agility ,char))
	       (range-attack-chance ,char) (alexandria:lerp 0.1
							    (dexterity ,char)
							    (agility   ,char))
	       (melee-attack-damage ,char) (truncate (* (strength ,char) 0.25))
	       (range-attack-damage ,char) (truncate (* (dexterity ,char) 0.25))
	       (unlock-chance ,char) (truncate (/ (alexandria:lerp 0.9
								   (agility ,char)
								   (dexterity ,char))

						  10))
	       (deactivate-trap-chance ,char) (truncate
					       (/ (alexandria:lerp 0.8
								   (agility ,char)
								   (dexterity ,char))
						  10))
	       (reply-attack-chance ,char) (truncate (/ (agility ,char) 5))
	       (ambush-attack-chance ,char) (truncate
					     (/ (alexandria:lerp 0.9
								 (agility ,char)
								 (strength ,char))
						10))
	       (spell-chance ,char) (truncate (/ (alexandria:lerp 0.9
								  (smartness ,char)
								  (empaty ,char))
						 3))
	       (attack-spell-chance ,char) (truncate (/ (alexandria:lerp 0.9
									 (agility ,char)
									 (smartness ,char))
							3))
	       (race ,char) race)
	 (if (> ,rest-capital 0)
	     (,(alexandria:format-symbol t "~@:(make-~a~)" class) ,rest-capital race ,char slots)
	     (values ,char ,rest-capital))))))

(gen-make-player player)

(defun make-warrior (race)
  (make-player *standard-capital-characteristic* race nil '((strength (50 10))
							    (stamina  (40 10))
							    (dexterity (5 3))
							    (agility   (10 3))
							    (empaty    (2 0))
							    (smartness (2 0))
							    (weight    (52 23)))))

(defun make-wizard (race)
  (make-player *standard-capital-characteristic* race nil '((smartness (40 10))
							    (agility   (30 10))
							    (stamina  (20 10))
							    (strength (10 10))
							    (dexterity (5 3))
							    (empaty    (2 0))
							    (weight    (30 5)))))

(defun make-healer (race)
  (make-player *standard-capital-characteristic* race nil '((empaty (40 10))
							    (smartness (20 20))
							    (agility   (15 10))
							    (stamina  (10 10))
							    (strength (10 10))
							    (dexterity (5 3))
							    (weight    (42 12)))))

(defun make-archer (race)
  (make-player *standard-capital-characteristic* race nil '((dexterity (50 0))
							    (stamina  (40 10))
							    (strength (10 10))
							    (empaty    (1 1))
							    (agility   (10 3))
							    (smartness (5 3))
							    (weight    (30 55)))))

(defun make-ranger (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((agility (60 0))
									  (dexterity  (40 10))
									  (stamina (15 10))
									  (strength (15 10))
									  (empaty    (10 10))
									  (smartness (5 3))
									  (weight    (56 20))))))
    (incf (deactivate-trap-chance player) 3)
    player))

;;;; interaction

(defmethod lookup-basic-interaction ((object player-character) key)
  (cdr (assoc key (basic-interaction-params object))))

(defmacro gen-interaction-predicate ((name) &body body)
  (let ((name-fn (format-symbol t "~:@(~a-p~)" name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object player-character))
	 (progn ,@body)))))

(defmacro gen-trivial-interaction-path (name &rest vars)
  (let ((name-fn (format-symbol t "~:@(interaction-~a~)" name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object player-character))
	 (let ((path (build-assocs-chain ,(reverse vars) (basic-interaction-params object))))
	    path)))))

(gen-trivial-interaction-path get-strength                    +effects+ +strength+)

(gen-trivial-interaction-path get-stamina                     +effects+ +stamina+)

(gen-trivial-interaction-path get-dexterity                   +effects+ +dexterity+)

(gen-trivial-interaction-path get-agility                     +effects+ +agility+)

(gen-trivial-interaction-path get-smartness                   +effects+ +smartness+)

(gen-trivial-interaction-path get-empaty                      +effects+ +empaty+)

(gen-trivial-interaction-path get-weight                      +effects+ +weight+)

(gen-trivial-interaction-path get-damage-points               +effects+ +damage-points+)

(gen-trivial-interaction-path get-movement-points             +effects+ +movement-point+)

(gen-trivial-interaction-path get-magic-points                +effects+ +magic-point+)

(gen-trivial-interaction-path get-dodge-chance                +effects+ +dodge-chance+)

(gen-trivial-interaction-path get-melee-attack-chance         +effects+ +melee-attack-chance+)

(gen-trivial-interaction-path get-range-attack-chance         +effects+ +range-attack-chance+)

(gen-trivial-interaction-path get-melee-attack-damage         +effects+ +melee-attack-damage+)

(gen-trivial-interaction-path get-range-attack-damage         +effects+ +range-attack-damage+)

(gen-trivial-interaction-path get-edge-weapons-chance-bonus   +effects+ +edge-weapons-chance-bonus+)

(gen-trivial-interaction-path get-edge-weapons-damage-bonus   +effects+ +edge-weapons-damage-bonus+)

(gen-trivial-interaction-path get-impact-weapons-chance-bonus +effects+
                                                              +impact-weapons-chance-bonus+)

(gen-trivial-interaction-path get-impact-weapons-damage-bonus +effects+
                                                              +impact-weapons-damage-bonus+)

(gen-trivial-interaction-path get-pole-weapons-chance-bonus   +effects+ +pole-weapons-chance-bonus+)

(gen-trivial-interaction-path get-pole-weapons-damage-bonus   +effects+ +pole-weapons-damage-bonus+)

(gen-trivial-interaction-path get-unlock-chance               +effects+ +unlock-chance+)

(gen-trivial-interaction-path get-deactivate-trap-chance      +effects+ +deactivate-trap-chance+)

(gen-trivial-interaction-path get-reply-attack-chance         +effects+ +reply-attack-chance+)

(gen-trivial-interaction-path get-ambush-attack-chance        +effects+ +ambush-attack-chance+)

(gen-trivial-interaction-path get-spell-chance                +effects+ +spell-chance+)

(gen-trivial-interaction-path get-attack-spell-chance         +effects+ +attack-spell-chance+)

(gen-trivial-interaction-path get-heal-poison                 +healing-effects+ +heal-poison+)

(gen-trivial-interaction-path get-heal-berserk                +healing-effects+ +heal-berserk+)

(gen-trivial-interaction-path get-heal-faint                  +healing-effects+ +heal-faint+)

(gen-trivial-interaction-path get-heal-terror                 +healing-effects+ +heal-terror+)

(gen-trivial-interaction-path get-cause-poison                +healing-effects+ +cause-poison+)

(gen-trivial-interaction-path get-cause-berserk               +healing-effects+ +cause-berserk+)

(gen-trivial-interaction-path get-cause-faint                 +healing-effects+ +cause-faint+)

(gen-trivial-interaction-path get-cause-terror                +healing-effects+ +cause-terror+)

(gen-trivial-interaction-path get-immune-poison               +healing-effects+ +immune-poison+)

(gen-trivial-interaction-path get-immune-berserk              +healing-effects+ +immune-berserk+)

(gen-trivial-interaction-path get-immune-faint                +healing-effects+ +immune-faint+)

(gen-trivial-interaction-path get-immune-terror               +healing-effects+ +immune-terror+)

(gen-trivial-interaction-path get-magic-effect                +magic-effects+)

(gen-trivial-plist-predicates player-character
				   (lambda (v k) (lookup-basic-interaction v k))
				   +can-talk+
				   +can-be-opened+
				   +can-open+
				   +can-attack+
				   +can-attack+
				   +can-be-destroyed+
				   +can-be-burned+
				   +can-heal+
				   +can-be-heal+
				   +can-poison+
				   +can-be-poisoned+
				   +can-be-drunk+
				   +can-be-eaten+
				   +can-be-weared-arm+
				   +can-be-weared-head+
				   +can-be-weared-neck+
				   +can-be-weared-feet+
				   +can-cut+
				   +can-smash+
				   +can-pierce+
				   +can-launch-bolt+
				   +can-launch-arrow+
				   +mounted-on-pole+
				   +decay+
				   +effects+
				   +healing-effects+
				   +magic-effects+)

(gen-trivial-plist-gets player-character
			     (lambda (v k) (lookup-basic-interaction v k))
			     fetch
			     +can-open+
			     +can-be-opened+
			     +can-poison+)

(gen-interaction-predicate (be-consumed)
  (or (lookup-basic-interaction object +can-be-drunk+)
      (lookup-basic-interaction object +can-be-eaten+)))

(gen-interaction-predicate (weapon)
  (or (lookup-basic-interaction object +can-cut+)
      (lookup-basic-interaction object +can-smash+)
      (lookup-basic-interaction object +can-pierce+)))

(gen-interaction-predicate (con-be-worn)
  (or (lookup-basic-interaction object +can-be-weared-feet+)
      (lookup-basic-interaction object +can-be-weared-arm+)
      (lookup-basic-interaction object +can-be-weared-neck+)
      (lookup-basic-interaction object +can-be-weared-head+)))

(defmethod object-keycode ((object player-character))
  (or (and (lookup-basic-interaction object +can-be-opened+)
	   (stringp (lookup-basic-interaction object +can-be-opened+))
	   (lookup-basic-interaction object +can-be-opened+))
      (and (lookup-basic-interaction object +can-open+)
	   (stringp (lookup-basic-interaction object +can-open+))
	   (lookup-basic-interaction object +can-open+))))

(defmethod import-interaction-from-definition ((object player-character) file)
  (with-interaction-parameters (parameters file)
    (setf (basic-interaction-params object) parameters)
    object))

(defmethod description-for-humans ((object player-character))
  (strcat
   (format nil (_ "~:[~;Edge weapon ~]~:[~;Impact weapon ~]~:[~;Range weapon ~]~:[~;Range weapon ~]")
	   (can-cut-p   object)
	   (can-smash-p object)
	   (can-launch-bolt-p object)
	   (can-launch-arrow-p object))
   (format nil (_ "~@[, ~a strength~]")  (description-for-humans (interaction-get-strength object)))
   (format nil (_ "~@[, ~a stamina~]")   (description-for-humans (interaction-get-stamina object)))
   (format nil (_ "~@[, ~a dexterity~]") (description-for-humans (interaction-get-dexterity object)))
   (format nil (_ "~@[, ~a agility~]")   (description-for-humans (interaction-get-agility object)))
   (format nil (_ "~@[, ~a smartness~]") (description-for-humans (interaction-get-smartness object)))
   (format nil (_ "~@[, ~a empaty~]")    (description-for-humans (interaction-get-empaty object)))
   (format nil (_ "~@[, ~a weight~]")    (description-for-humans (interaction-get-weight object)))
   (format nil (_ "~@[, ~a damage points~]") (description-for-humans (interaction-get-damage-points object)))
   (format nil (_ "~@[, ~a movement points~]") (description-for-humans
					       (interaction-get-movement-points object)))
   (format nil (_ "~@[, ~a magic points~]") (description-for-humans (interaction-get-magic-points object)))
   (format nil (_ "~@[, ~a dodge chance~]") (description-for-humans (interaction-get-dodge-chance object)))

   (format nil (_ "~@[, ~a melee attack chance~]") (description-for-humans
						   (interaction-get-melee-attack-chance object)))
   (format nil (_ "~@[, ~a range attack chance~]") (description-for-humans
						   (interaction-get-range-attack-chance object)))
   (format nil (_ "~@[, ~a melee attack damage~]") (description-for-humans
						   (interaction-get-melee-attack-damage object)))
   (format nil (_ "~@[, ~a range attack damage~]") (description-for-humans
						   (interaction-get-range-attack-damage object)))
   (format nil (_ "~@[, ~a edge weapons chance bonus~]") (description-for-humans
							 (interaction-get-edge-weapons-chance-bonus object)))
   (format nil (_ "~@[, ~a edge weapons damage bonus~]") (description-for-humans
							  (interaction-get-edge-weapons-damage-bonus object)))
   (format nil (_ "~@[, ~a impact weapons chance bonus~]") (description-for-humans
							    (interaction-get-impact-weapons-chance-bonus object)))
   (format nil (_ "~@[, ~a impact weapons damage bonus~]") (description-for-humans
							    (interaction-get-impact-weapons-damage-bonus object)))
   (format nil (_ "~@[, ~a pole weapons chance bonus~]") (description-for-humans
							  (interaction-get-pole-weapons-chance-bonus object)))
   (format nil (_ "~@[, ~a pole weapons damage bonus~]") (description-for-humans
							  (interaction-get-pole-weapons-damage-bonus object)))
   (format nil (_ "~@[, ~a unlock chance~]") (description-for-humans (interaction-get-unlock-chance object)))
   (format nil (_ "~@[, ~a deactivate traps chance~]") (description-for-humans
							(interaction-get-deactivate-trap-chance object)))
   (format nil (_ "~@[, ~a reply attack chance~]") (description-for-humans
						    (interaction-get-reply-attack-chance object)))
   (format nil (_ "~@[, ~a ambush attack chance~]") (description-for-humans
						     (interaction-get-ambush-attack-chance object)))
   (format nil (_ "~@[, ~a spell chance~]") (description-for-humans
					     (interaction-get-spell-chance object)))
   (format nil (_ "~@[, ~a attack spell chance~]") (description-for-humans
						    (interaction-get-attack-spell-chance object)))
   (format nil (_ "~@[, ~a heal poison condition~]") (description-for-humans
						      (interaction-get-heal-poison object)))
   (format nil (_ "~@[, ~a heal berserk condition~]") (description-for-humans
						       (interaction-get-heal-berserk object)))
   (format nil (_ "~@[, ~a heal faint condition~]")  (description-for-humans
						      (interaction-get-heal-faint object)))
   (format nil (_ "~@[, ~a heal terror condition~]")  (description-for-humans
						       (interaction-get-heal-terror object)))
   (format nil (_ "~@[, makes immune from poison (~a)~]") (description-for-humans
							(interaction-get-immune-poison object)))
   (format nil (_ "~@[, makes immune from terror (~a)~]") (description-for-humans
							(interaction-get-immune-terror object)))
   (format nil (_ "~@[, makes immune from berserk (~a)~]") (description-for-humans
							 (interaction-get-immune-berserk object)))
   (format nil (_ "~@[, makes immune from faint (~a)~]") (description-for-humans
						       (interaction-get-immune-berserk object)))
   (format nil (_ "~@[, ~a heals faint condition~]")  (description-for-humans
						      (interaction-get-heal-faint object)))
   (format nil (_ "~@[, ~a heals terror condition~]")  (description-for-humans
						       (interaction-get-heal-terror object)))
   (format nil (_ "~@[, launchs ~a~]")  (description-for-humans (interaction-get-magic-effect
								 object)))))

;; character definition

(defparameter *character-parameters* nil)

(defmacro define-character (&rest parameters)
  (let ((params (build-plist parameters)))
    `(setf *character-parameters*
	   (list ,@(loop for i in params collect
			`(cons ,(car i) ,(if (consp (cdr i))
					     (cadr i)
					     (cdr i))))))))

(defmacro with-character-parameters ((params file) &body body)
  `(let* ((*character-parameters* nil))
     (load ,file)
     (let ((,params *character-parameters*))
       ,@body)))

(gen-trivial-plist-gets t
			(lambda (l k) (cdr (assoc k l)))
			fetch
			+first-name+
			+last-name+
			+portrait+
			+strength+
			+stamina+
			+dexterity+
			+agility+
			+smartness+
			+empaty+
			+weight+
			+damage-points+
			+movement-points+
			+magic-points+
			+dodge-chance+
			+melee-attack-chance+
			+range-attack-chance+
			+melee-attack-damage+
			+range-attack-damage+
			+edge-weapons-chance-bonus+
			+edge-weapons-damage-bonus+
			+impact-weapons-chance-bonus+
			+impact-weapons-damage-bonus+
			+pole-weapons-chance-bonus+
			+pole-weapons-damage-bonus+
			+unlock-chance+
			+deactivate-trap-chance+
			+reply-attack-chance+
			+ambush-attack-chance+
			+spell-chance+
			+attack-spell-chance+
			+status+
			+race+
			+level+
			+exp-points+)

(defun randomize-a-bit (val &key (displacement 0.1))
  (if (numberp val)
      (floor (num:gaussian-probability (* displacement val) val))
      val))

(defun params->character (params)
  (let ((results (make-instance
		    'player-character
		    :first-name                  (fetch-first-name                  params)
		    :last-name                   (fetch-last-name                   params)
		    :portrait                    (fetch-portrait                    params)
		    :strength                    (fetch-strength                    params)
		    :stamina                     (fetch-stamina                     params)
		    :dexterity                   (fetch-dexterity                   params)
		    :agility                     (fetch-agility                     params)
		    :smartness                   (fetch-smartness                   params)
		    :empaty                      (fetch-empaty                      params)
		    :weight                      (fetch-weight                      params)
		    :damage-points               (fetch-damage-points               params)
		    :movement-points             (fetch-movement-points             params)
		    :magic-points                (fetch-magic-points                params)
		    :dodge-chance                (fetch-dodge-chance                params)
		    :melee-attack-chance         (fetch-melee-attack-chance         params)
		    :range-attack-chance         (fetch-range-attack-chance         params)
		    :melee-attack-damage         (fetch-melee-attack-damage         params)
		    :range-attack-damage         (fetch-range-attack-damage         params)
		    :edge-weapons-chance-bonus   (fetch-edge-weapons-chance-bonus   params)
		    :edge-weapons-damage-bonus   (fetch-edge-weapons-damage-bonus   params)
		    :impact-weapons-chance-bonus (fetch-impact-weapons-chance-bonus params)
		    :impact-weapons-damage-bonus (fetch-impact-weapons-damage-bonus params)
		    :pole-weapons-chance-bonus   (fetch-pole-weapons-chance-bonus   params)
		    :pole-weapons-damage-bonus   (fetch-pole-weapons-damage-bonus   params)
		    :unlock-chance               (fetch-unlock-chance               params)
		    :deactivate-trap-chance      (fetch-deactivate-trap-chance      params)
		    :reply-attack-chance         (fetch-reply-attack-chance         params)
		    :ambush-attack-chance        (fetch-ambush-attack-chance        params)
		    :spell-chance                (fetch-spell-chance                params)
		    :attack-spell-chance         (fetch-attack-spell-chance         params)
		    :status                      (fetch-status                      params)
		    :race                        (fetch-race                        params)
		    :level                       (fetch-level                       params)
		    :exp-points                  (fetch-exp-points                  params))))
	  results))

(defun load-randomize-character (file)
  (with-character-parameters (params file)
    (let ((results (make-instance 'player-character
				  :first-name                  (randomize-a-bit
								(fetch-first-name params))
				  :last-name                   (randomize-a-bit
								(fetch-last-name params))
				  :portrait                    (texture:get-texture
								(portrait-texture-resource
								 (fetch-portrait params)))
				  :strength                    (randomize-a-bit
								(fetch-strength params))
				  :stamina                     (randomize-a-bit
								(fetch-stamina params))
				  :dexterity                   (randomize-a-bit
								(fetch-dexterity params))
				  :agility                     (randomize-a-bit
								(fetch-agility params))
				  :smartness                   (randomize-a-bit
								(fetch-smartness params))
				  :empaty                      (randomize-a-bit
								(fetch-empaty params))
				  :weight                      (randomize-a-bit
								(fetch-weight params))
				  :damage-points               (randomize-a-bit
								(fetch-damage-points params))
				  :movement-points             (randomize-a-bit
								(fetch-movement-points params))
				  :magic-points                (randomize-a-bit
								(fetch-magic-points params))
				  :dodge-chance                (randomize-a-bit
								(fetch-dodge-chance params))
				  :melee-attack-chance         (randomize-a-bit
								(fetch-melee-attack-chance params))
				  :range-attack-chance         (randomize-a-bit
								(fetch-range-attack-chance params))
				  :melee-attack-damage         (randomize-a-bit
								(fetch-melee-attack-damage params))
				  :range-attack-damage         (randomize-a-bit
								(fetch-range-attack-damage params))
				  :edge-weapons-chance-bonus   (randomize-a-bit
								(fetch-edge-weapons-chance-bonus
								 params))
				  :edge-weapons-damage-bonus   (randomize-a-bit
								(fetch-edge-weapons-damage-bonus
								 params))
				  :impact-weapons-chance-bonus (randomize-a-bit
								(fetch-impact-weapons-chance-bonus
								 params))
				  :impact-weapons-damage-bonus (randomize-a-bit
								(fetch-impact-weapons-damage-bonus
								 params))
				  :pole-weapons-chance-bonus   (randomize-a-bit
								(fetch-pole-weapons-chance-bonus
								 params))
				  :pole-weapons-damage-bonus   (randomize-a-bit
								(fetch-pole-weapons-damage-bonus
								 params))
				  :unlock-chance               (randomize-a-bit
								(fetch-unlock-chance params))
				  :deactivate-trap-chance      (randomize-a-bit
								(fetch-deactivate-trap-chance
								 params))
				  :reply-attack-chance         (randomize-a-bit
								(fetch-reply-attack-chance params))
				  :ambush-attack-chance        (randomize-a-bit
								(fetch-ambush-attack-chance params))
				  :spell-chance                (randomize-a-bit
								(fetch-spell-chance params))
				  :attack-spell-chance         (randomize-a-bit
								(fetch-attack-spell-chance params))
				  :status                      (randomize-a-bit
								(fetch-status params))
				  :race                        (randomize-a-bit
								(fetch-race params))
				  :level                       (randomize-a-bit
								(fetch-level params))
				  :exp-points                  (randomize-a-bit
								(fetch-exp-points params)))))
      results)))

(defun conflict-all-effects ()
  (list (list :effects :strength)
	(list :effects :stamina)
	(list :effects :dexterity)
	(list :effects :agility)
	(list :effects :smartness)
	(list :effects :empaty)
	(list :effects :weight)
	(list :effects :damage-points)
	(list :effects :movement-points)
	(list :effects :magic-points)
	(list :effects :dodge-chance)
	(list :effects :melee-attack-chance)
	(list :effects :range-attack-chance)
	(list :effects :melee-attack-damage)
	(list :effects :range-attack-damage)
	(list :effects :edge-weapons-chance-bonus)
	(list :effects :edge-weapons-damage-bonus)
	(list :effects :impact-weapons-chance-bonus)
	(list :effects :impact-weapons-damage-bonus)
	(list :effects :pole-weapons-chance-bonus)
	(list :effects :pole-weapons-damage-bonus)
	(list :effects :unlock-chance)
	(list :effects :deactivate-trap-chance)
	(list :effects :reply-attack-chance)
	(list :effects :ambush-attack-chance)
	(list :effects :spell-chance)
	(list :effects :attack-spell-chance)
	(list :healing-effects :heal-poison)
	(list :healing-effects :heal-berserk)
	(list :healing-effects :heal-faint)
	(list :healing-effects :heal-terror)
	(list :healing-effects :cause-poison)
	(list :healing-effects :cause-berserk)
	(list :healing-effects :cause-faint)
	(list :healing-effects :cause-terror)
	(list :healing-effects :immune-poison)
	(list :healing-effects :immune-berser)
	(list :healing-effects :immune-faint)
	(list :healing-effects :immune-terror)
	:magic-effect))

(defun %trivial-conflict-list (to-be-removed)
  (remove to-be-removed '(:can-talk
			  :can-ask-for-help
			  :can-be-opened
			  :can-open
			  :can-attack
			  :can-be-attacked
			  :can-be-destroyed
			  :can-be-burned
			  :can-heal
			  :can-be-heal
			  :can-poison
			  :can-be-poisoned
			  :can-be-drunk
			  :can-be-eaten
			  :can-be-weared-arm
			  :can-be-weared-head
			  :can-be-weared-neck
			  :can-be-weared-feet
			  :can-cut
			  :can-smash
			  :can-pierce
			  :can-launch-bolt
			  :can-launch-arrow
			  :mounted-on-pole)))

(defparameter *relations* `((:path (:can-talk)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects)))
			    (:path (:can-open)
				   :conflicts ,(append (list
							:can-talk
							:can-be-opened
							:can-attack
							:can-be-attacked
							:can-be-destroyed
							:can-be-burned
							:can-heal
							:can-be-heal
							:can-poison
							:can-be-poisoned
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole
							(conflict-all-effects))))
			    (:path (:can-be-opened)
				   :conflicts ,(append (list
							:can-talk
							:can-open
							:can-attack
							:can-be-attacked
							:can-be-destroyed
							:can-be-burned
							:can-heal
							:can-be-heal
							:can-poison
							:can-be-poisoned
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole
							(conflict-all-effects))))
			    (:path (:can-attack)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects)))
			    (:path (:can-be-attacked)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects)))
			    (:path (:can-be-destroyed)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects)))
			    (:path (:can-be-burned)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects)))
			    (:path (:can-heal)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects)))
			    (:path (:can-be-heal)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects)))
			    (:path (:can-poison)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects)))
			    (:path (:can-be-poisoned)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-weared-arm
							:can-be-weared-head
							:can-be-weared-neck
							:can-be-weared-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects)))
			    (:path (:can-be-drunk)
				   :conflicts (:can-talk
					       :can-ask-for-help
					       :can-be-opened
					       :can-open
					       :can-attack
					       :can-be-attacked
					       :can-be-destroyed
					       :can-be-burned
					       :can-heal
					       :can-be-heal
					       :can-poison
					       :can-be-poisoned
					       :can-be-eaten
					       :can-be-weared-arm
					       :can-be-weared-head
					       :can-be-weared-neck
					       :can-be-weared-feet
					       :can-cut
					       :can-smash
					       :can-pierce
					       :can-launch-bolt
					       :can-launch-arrow
					       :mounted-on-pole))
			    (:path (:can-be-eaten)
				   :conflicts (:can-talk
					       :can-ask-for-help
					       :can-be-opened
					       :can-open
					       :can-attack
					       :can-be-attacked
					       :can-be-destroyed
					       :can-be-burned
					       :can-heal
					       :can-be-heal
					       :can-poison
					       :can-be-poisoned
					       :can-be-drunk
					       :can-be-weared-arm
					       :can-be-weared-head
					       :can-be-weared-neck
					       :can-be-weared-feet
					       :can-cut
					       :can-smash
					       :can-pierce
					       :can-launch-bolt
					       :can-launch-arrow
					       :mounted-on-pole))
			    (:path     (:can-cut)
				       :conflicts (:can-smash
						   :can-pierce
						   :can-launch-bolt
						   :can-launch-arrow
						   :mounted-on-pole
						   :can-talk
						   :can-be-opened
						   :can-open
						   :can-attack
						   :can-be-attacked
						   :can-be-destroyed
						   :can-be-burned
						   :can-heal
						   :can-be-heal
						   :can-poison
						   :can-be-poisoned
						   :can-be-drunk
						   :can-be-eaten))
			    (:path     (:can-smash)
				       :conflicts (:can-cut
						   :can-pierce
						   :can-launch-bolt
						   :can-launch-arrow
						   :mounted-on-pole
						   :can-talk
						   :can-be-opened
						   :can-open
						   :can-attack
						   :can-be-attacked
						   :can-be-destroyed
						   :can-be-burned
						   :can-heal
						   :can-be-heal
						   :can-poison
						   :can-be-poisoned
						   :can-be-drunk
						   :can-be-eaten))
			    (:path     (:can-pierce)
				       :conflicts (:can-cut
						   :can-smash
						   :can-launch-bolt
						   :can-launch-arrow
						   :mounted-on-pole
						   :can-talk
						   :can-be-opened
						   :can-open
						   :can-attack
						   :can-be-attacked
						   :can-be-destroyed
						   :can-be-burned
						   :can-heal
						   :can-be-heal
						   :can-poison
						   :can-be-poisoned
						   :can-be-drunk
						   :can-be-eaten))
			    (:path     (:can-pierce)
				       :conflicts (:can-cut
						   :can-smash
						   :can-launch-bolt
						   :can-launch-arrow
						   :mounted-on-pole
						   :can-talk
						   :can-be-opened
						   :can-open
						   :can-attack
						   :can-be-attacked
						   :can-be-destroyed
						   :can-be-burned
						   :can-heal
						   :can-be-heal
						   :can-poison
						   :can-be-poisoned
						   :can-be-drunk
						   :can-be-eaten))
			    (:path     (:can-launch-bolt)
				       :conflicts (:can-cut
						   :can-pierce
						   :can-smash
						   :can-launch-arrow
						   :mounted-on-pole
						   :can-talk
						   :can-be-opened
						   :can-open
						   :can-attack
						   :can-be-attacked
						   :can-be-destroyed
						   :can-be-burned
						   :can-heal
						   :can-be-heal
						   :can-poison
						   :can-be-poisoned
						   :can-be-drunk
						   :can-be-eaten))
			    (:path     (:can-launch-arrow)
				       :conflicts (:can-cut
						   :can-pierce
						   :can-smash
						   :can-launch-bolt
						   :mounted-on-pole
						   :can-talk
						   :can-be-opened
						   :can-open
						   :can-attack
						   :can-be-attacked
						   :can-be-destroyed
						   :can-be-burned
						   :can-heal
						   :can-be-heal
						   :can-poison
						   :can-be-poisoned
						   :can-be-drunk
						   :can-be-eaten))
			    (:path     (:mounted-on-pole)
				       :conflicts (:can-cut
						   :can-pierce
						   :can-smash
						   :can-launch-arrow
						   :can-launch-bolt
						   :can-talk
						   :can-be-opened
						   :can-open
						   :can-attack
						   :can-be-attacked
						   :can-be-destroyed
						   :can-be-burned
						   :can-heal
						   :can-be-heal
						   :can-poison
						   :can-be-poisoned
						   :can-be-drunk
						   :can-be-eaten))
			    (:path     (:can-be-weared-arm)
				       :conflicts ,(%trivial-conflict-list :can-be-weared-arm))
			    (:path     (:can-be-weared-head)
				       :conflicts ,(%trivial-conflict-list :can-be-weared-head))
			    (:path     (:can-be-weared-neck)
				       :conflicts ,(%trivial-conflict-list :can-be-weared-neck))
			    (:path     (:can-be-weared-feet)
				       :conflicts ,(%trivial-conflict-list :can-be-weared-feet))))

(defun listify (a)
  (if (listp a) a (list a)))

(defun get-path (path relations)
  (find (listify path)
	relations
	:test #'equalp :key #'(lambda (a) (getf a :path))))

(defun get-conflict-elements (item)
  (getf item :conflicts))

(defun conflictp (interactions item)
  (let* ((potential-conflicts (get-conflict-elements item))
	 (item-set-p (recursive-assoc (listify (getf item :path))  interactions))
	 (conflict (remove-if-null (mapcar #'(lambda (path)
						    (if (recursive-assoc (listify path)
									      interactions)
							path
							nil))
						potential-conflicts))))
    (when (and item-set-p conflict)
      (error (format nil "~a conflict with ~a" (getf item :path) conflict)))
    (and item-set-p conflict)))

(defun validate-interaction-file (file)
  (with-interaction-parameters (params file)
    (find-if #'(lambda (a) (not (null a)))
	     (loop for i in *relations* collect (conflictp params i)))))

;;; object generation

(define-constant +weapon-file-record-sep+ "-"  :test #'string=)

(define-constant +weapon-level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +weapon-level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.3 3.4 3.6)
  :test #'equalp)

(define-constant +decay-weapon-sigma+    #(56.0 48.0 40.0 36.0 36.0 24.0 22.0 20.0 30.0 35.0)
  :test #'equalp)

(define-constant +decay-weapon-mean+     #(30.0 28.0 26.0 24.0 12.0 10.0 9.0 8.0 4.0 0.0)
  :test #'equalp)

(define-constant +modifier-weapon-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +modifier-weapon-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +duration-healing-fx-sigma+  #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +duration-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+  #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+   #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)


(define-constant +magic-fx-sigma+            #(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
  :test #'equalp)

(define-constant +magic-fx-mean+             #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0)
  :test #'equalp)

(define-constant +minimum-magic-fx-level+                1.0        :test #'=)

(define-constant +minimum-duration-healing-fx+           2.0        :test #'=)

(define-constant +minimum-chance-healing-fx+             0.05       :test #'=)

(define-constant +minimum-decay-weapon+                 20.0        :test #'=)

(define-constant +minimum-weapon-level+                  1          :test #'=)

(define-constant +maximum-weapon-level+                 10          :test #'=)

(define-constant +minimum-chance-weapon-effects+         2.0        :test #'=)

(define-constant +maximum-chance-weapon-effects+         4.0        :test #'=)

(define-constant +minimum-chance-weapon-healing-effects+ 2.0        :test #'=)

(define-constant +maximum-chance-weapon-healing-effects+ 4.0        :test #'=)

(define-constant +minimum-chance-weapon-magic-effects+   0.0        :test #'=)

(define-constant +maximum-chance-weapon-magic-effects+   2.0        :test #'=)

(define-constant +sword-type-name+                       "sword"    :test #'string=)

(define-constant +spear-type-name+                       "spear"    :test #'string=)

(define-constant +staff-type-name+                       "staff"    :test #'string=)

(define-constant +mace-type-name+                        "mace"     :test #'string=)

(define-constant +bow-type-name+                         "bow"      :test #'string=)

(define-constant +crossbow-type-name+                    "crossbow" :test #'string=)

(define-constant +mace-or-stuff-chance+                  3          :test #'=)

(defun weapon-decay-params (weapon-level)
  (values (elt +decay-weapon-sigma+ weapon-level)
	  (elt +decay-weapon-mean+  weapon-level)))

(defun calculate-weapon-decay-points (weapon-level)
  (multiple-value-bind (sigma mean)
      (weapon-decay-params (1- weapon-level))
    (truncate (max +minimum-decay-weapon+ (gaussian-probability sigma mean)))))

(defun calculate-weapon-decay (object-level character decay-points)
  (make-instance 'decay-parameters
		 :leaving-message (format nil
					  "~a broken"
					  (plist-path-value character (list +description+)))
		 :points decay-points
		 :when-decay (if (and (> object-level (/ +maximum-weapon-level+ 2))
				      (= (lcg-next-upto 10) 0))
				 +decay-by-turns+ +decay-by-use+)))

(defun weapon-level-params (map-level)
  (values (elt +weapon-level-sigma+ map-level)
	  (elt +weapon-level-mean+  map-level)))

(defun weapon-modifier-params (weapon-level)
  (values (elt +modifier-weapon-sigma+ weapon-level)
	  (elt +modifier-weapon-mean+  weapon-level)))

(defun calculate-weapon-modifier (weapon-level)
  (multiple-value-bind (sigma mean)
      (weapon-modifier-params (1- weapon-level))
    (gaussian-probability sigma mean)))

(defun calculate-weapon-level (map-level)
  (multiple-value-bind (sigma mean)
      (weapon-level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-weapon-level+ +maximum-weapon-level+)))

(defun number-of-effects (weapon-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- weapon-level)))
			       +minimum-chance-weapon-effects+
			       +maximum-chance-weapon-effects+))))
    (lcg-next-upto max)))

(defun number-of-healing-effects (weapon-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- weapon-level)))
				  +minimum-chance-weapon-healing-effects+
				  +maximum-chance-weapon-healing-effects+)
		       number-of-normal-effects))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun number-of-magic-effects (weapon-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   5.0
							   (d (1- weapon-level)))
			       +minimum-chance-weapon-magic-effects+
			       +maximum-chance-weapon-magic-effects+))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun generate-weapon (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((weapon-level       (calculate-weapon-level map-level))
	     (weapon-decay       (calculate-weapon-decay-points weapon-level))
	     (effects-no         (number-of-effects weapon-level))
	     (healing-effect-no  (number-of-healing-effects weapon-level effects-no)))
	(cond
	  ((plist-path-value template (list +can-cut+))
	   (generate-weapon-common template char-template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (fill-sword-plists char-template template weapon-level))
	  ((plist-path-value template (list +can-smash+))
	   (generate-weapon-common template char-template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (if (= (lcg-next-upto (truncate (+ +mace-or-stuff-chance+ (* 0.3 weapon-level)))) 0)
	       (fill-staff-plists char-template template weapon-level)
	       (fill-mace-plists char-template template weapon-level)))
	  ((plist-path-value template (list +can-pierce+))
	   (generate-weapon-common template char-template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (fill-spear-plists char-template template weapon-level))
  	  ((plist-path-value template (list +can-launch-bolt+))
	   (generate-weapon-common template char-template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (fill-crossbow-plists char-template template weapon-level))
	  ((plist-path-value template (list +can-launch-arrow+))
	   (generate-weapon-common template char-template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (fill-crossbow-plists char-template template weapon-level))
	  (t
	   (error (_ "Unknown weapon type"))))
	(let ((weapon-character (params->character char-template)))
	  (setf (basic-interaction-params weapon-character) template)
	  (dbg "2~%~a ~%~% ~a" template char-template)
	  weapon-character)))))

(defun %get-effects-shuffled (l num)
  (subseq (shuffle l) 0 num))

(defun %get-normal-fx-shuffled (db num)
  (let ((all (plist-path-value db (list +effects+))))
    (%get-effects-shuffled (mapcar #'car (remove-if #'(lambda (a) (null (cdr a))) all)) num)))

(defun %get-healing-fx-shuffled (db num)
  (let ((all (plist-path-value db (list +healing-effects+))))
    (%get-effects-shuffled (mapcar #'car (remove-if #'(lambda (a) (null (cdr a))) all)) num)))

(defun %get-magic-fx-shuffled (db num)
  (and (> num 0)
       (plist-path-value db (list +magic-effects+))))

(defun set-weapon-effect (effect-path weapon-level interaction)
  (let ((effect-object (make-instance 'effect-parameters
				      :modifier (calculate-weapon-modifier weapon-level)
				      :trigger  +effect-when-worn+
				       ;; effect lasting forever  for
				       ;; weapons,  they   will  broke
				       ;; anyway.
				      :duration  :unlimited)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun healing-fx-params-duration (weapon-level)
  (values (elt +duration-healing-fx-sigma+ weapon-level)
	  (elt +duration-healing-fx-mean+  weapon-level)))

(defun calculate-healing-fx-params-duration (weapon-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-duration (1- weapon-level))
    (truncate (max +minimum-duration-healing-fx+ (gaussian-probability sigma mean)))))

(defun healing-fx-params-chance (weapon-level)
  (values (elt +chance-healing-fx-sigma+ weapon-level)
	  (elt +chance-healing-fx-mean+  weapon-level)))

(defun calculate-healing-fx-params-chance (weapon-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-chance (1- weapon-level))
    (max +minimum-chance-healing-fx+ (gaussian-probability sigma mean))))

(defun set-healing-effect (effect-path weapon-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-when-worn+
				       ;; effect lasting forever  for
				       ;; weapons,  they   will  broke
				       ;; anyway.
				      :duration  :unlimited
				      :chance (calculate-healing-fx-params-chance weapon-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun set-poison-effect (effect-path weapon-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :points-per-turn (calculate-weapon-modifier weapon-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun magic-fx-params (weapon-level)
  (values (elt +magic-fx-sigma+ weapon-level)
	  (elt +magic-fx-mean+  weapon-level)))

(defun calculate-magic-fx-level (weapon-level)
  (multiple-value-bind (sigma mean)
      (magic-fx-params (1- weapon-level))
    (truncate (max +minimum-magic-fx-level+ (gaussian-probability sigma mean)))))

(defun random-spell-by-level (spell-level)
  (and spell-level :fireball-1)) ;; TODO

(defun set-magic-effect (weapon-level interaction)
  (let* ((spell-level (calculate-magic-fx-level weapon-level))
	 (spell-id    (random-spell-by-level spell-level))
	 (effect-object (make-instance 'magic-effect-parameters
				       :spell-id spell-id
				       :trigger  :use)))
    (n-setf-path-value interaction (list +magic-effects+) effect-object)))

(defun sum-effects-mod (interactions path)
  (reduce #'+ (mapcar #'(lambda (a) (if (typep (cdr a) 'effect-parameters) (modifier (cdr a)) 0.0))
		      (plist-path-value interactions path))
	  :initial-value 0.0))

(defun generate-weapon-common (interaction character weapon-level weapon-decay-points
			       effects-no healing-effects-no)
  (let* ((effects         (%get-normal-fx-shuffled  interaction effects-no))
	 (healing-effects (%get-healing-fx-shuffled interaction healing-effects-no)))
    (n-setf-path-value interaction
		       (list +decay+)
		       (calculate-weapon-decay weapon-level character weapon-decay-points))
    (loop for i in effects do
	 (set-weapon-effect (list +effects+ i) weapon-level interaction))
    (loop for i in healing-effects do
	 (cond
	   ((eq i +cause-poison+)
	    (set-poison-effect (list +healing-effects+ i) weapon-level interaction))
	   (t
	    (set-healing-effect (list +healing-effects+ i) weapon-level interaction))))))

(defun generate-sword (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 5)
		   (< sum-effects -10)))
      (set-magic-effect weapon-level interaction))))

(defun generate-spear (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 5)
		   (< sum-effects -10)))
      (set-magic-effect weapon-level interaction))))

(defun generate-mace (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 5)
		   (< sum-effects -10)))
      (set-magic-effect weapon-level interaction))))

(defun generate-staff (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 2)
		   (< sum-effects  0)))
      (set-magic-effect weapon-level interaction))))

(defun generate-bow (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 5)
		   (< sum-effects -10)))
      (set-magic-effect weapon-level interaction))))

(defun generate-crossbow (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 5)
		   (< sum-effects -10)))
      (set-magic-effect weapon-level interaction))))

(defun remove-generate-symbols (db)
  (if (null db)
      nil
      (append
       (loop for i in db collect
	    (cons (car i)
		  (cond
		    ((eq :generate (cdr i))
		     nil)
		    ((listp (cdr i))
		     (remove-generate-symbols (cdr i)))
		    (t
		     (cdr i))))))))

(defun weapon-filename-effects-string (interaction)
  (cond
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat "heal" +weapon-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +heal-berserk+))
     (strcat "heal" +weapon-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +heal-faint+))
     (strcat "heal" +weapon-file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +heal-terror+))
     (strcat "heal" +weapon-file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat "cause" +weapon-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +cause-berserk+))
     (strcat "cause" +weapon-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +cause-faint+))
     (strcat "cause" +weapon-file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +cause-terror+))
     (strcat "cause" +weapon-file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +immune-poison+))
     (strcat "immune" +weapon-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +immune-berserk+))
     (strcat "immune" +weapon-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +immune-faint+))
     (strcat "immune" +weapon-file-record-sep+ "faint"))
    (t
     "normal")))

(defun regexp-file-portrait (interaction weapon-name-type weapon-level)
  (strcat weapon-name-type
	  +weapon-file-record-sep+
	  (weapon-filename-effects-string interaction)
	  +weapon-file-record-sep+
	  (format nil "~2,'0d" weapon-level)))

(defun build-file-names-db (weapon-name-type weapon-level)
  (strcat weapon-name-type
	  +weapon-file-record-sep+
	  (format nil "~2,'0d" weapon-level)
	  ".lisp"))

(defun fill-character-plist (weapon-name-type character interaction weapon-level)
  (let* ((regex          (regexp-file-portrait interaction weapon-name-type weapon-level))
	 (names-filename (build-file-names-db weapon-name-type weapon-level))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (random-names:load-db* +weapons-names-resource+ names-filename)
    (n-setf-path-value character (list +last-name+)  (random-names:generate))
    (n-setf-path-value character (list +first-name+) weapon-name-type)))

(defun fill-sword-plists (character interaction weapon-level)
  (fill-character-plist +sword-type-name+ character interaction weapon-level))

(defun fill-spear-plists (character interaction weapon-level)
  (fill-character-plist +spear-type-name+ character interaction weapon-level))

(defun fill-mace-plists (character interaction weapon-level)
  (fill-character-plist +mace-type-name+ character interaction weapon-level))

(defun fill-staff-plists (character interaction weapon-level)
  (fill-character-plist +staff-type-name+ character interaction weapon-level))

(defun fill-bow-plists (character interaction weapon-level)
  (fill-character-plist +bow-type-name+ character interaction weapon-level))

(defun fill-crossbow-plists (character interaction weapon-level)
  (fill-character-plist +crossbow-type-name+ character interaction weapon-level))
