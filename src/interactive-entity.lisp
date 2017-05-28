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

(in-package :interactive-entity)

(alexandria:define-constant +unknown-ability-bonus+             -5.0                    :test #'=)

(alexandria:define-constant +starting-exp-points+               10.0                    :test #'=)

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

(alexandria:define-constant +status-poisoned+              :poisoned                    :test #'eq)

(alexandria:define-constant +status-terror+                :terror                      :test #'eq)

(alexandria:define-constant +status-berserk+               :berserk                     :test #'eq)

(alexandria:define-constant +status-faint+                 :faint                       :test #'eq)

(alexandria:define-constant +race+                         :race                        :test #'eq)

(alexandria:define-constant +level+                        :level                       :test #'eq)

(alexandria:define-constant +exp-points+                   :exp-points                  :test #'eq)

(defclass interactive-entity ()
  ((basic-interaction-params
    :initform nil
    :initarg  :basic-interaction-params
    :accessor basic-interaction-params)))

(defmethod marshal:class-persistant-slots ((object interactive-entity))
  (append  '(basic-interaction-params)
	   (call-next-method)))

(defgeneric lookup-basic-interaction (object key))

(defgeneric import-interaction-from-definition (object file))

(defgeneric object-keycode (object))

(defgeneric clean-effects (object))

(defgeneric description-type (object))

(defmethod lookup-basic-interaction ((object interactive-entity) key)
  (cdr (assoc key (basic-interaction-params object))))

(defmacro gen-interaction-predicate ((name) &body body)
  (let ((name-fn (format-symbol t
				(if (> (count "-" (symbol-name name) :test #'string=) 0)
				    "~:@(~a-p~)"
				    "~:@(~ap~)")
				name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object t)) nil)
       (defmethod  ,name-fn ((object interactive-entity))
	 (progn ,@body)))))

(defmacro gen-trivial-interaction-path (name &rest vars)
  (let ((name-fn (format-symbol t "~:@(interaction-~a~)" name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object interactive-entity))
	 (let ((path (build-assocs-chain ,(reverse vars) (basic-interaction-params object))))
	    path)))))

;; events

(defmethod game-event:on-game-event ((object interactive-entity) (event game-event:end-turn))
  ;(misc:dbg " end turn character ~a(~a) ~a" (type-of object) (id object) (type-of event))
  (incf (age object))
  nil)

(gen-trivial-interaction-path get-strength                    +effects+ +strength+)

(gen-trivial-interaction-path get-stamina                     +effects+ +stamina+)

(gen-trivial-interaction-path get-dexterity                   +effects+ +dexterity+)

(gen-trivial-interaction-path get-agility                     +effects+ +agility+)

(gen-trivial-interaction-path get-smartness                   +effects+ +smartness+)

(gen-trivial-interaction-path get-empaty                      +effects+ +empaty+)

(gen-trivial-interaction-path get-weight                      +effects+ +weight+)

(gen-trivial-interaction-path get-decay                       +decay+)

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

(gen-trivial-interaction-path get-heal-damage-points
			      +healing-effects+
			      +heal-damage-points+)

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

(gen-trivial-plist-predicates interactive-entity
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
			      +can-be-picked+
			      +can-be-dropped+
			      +can-be-worn-arm+
			      +can-be-worn-head+
			      +can-be-worn-neck+
			      +can-be-worn-feet+
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

(gen-trivial-plist-gets interactive-entity
			(lambda (v k) (lookup-basic-interaction v k))
			fetch
			+can-open+
			+can-be-opened+
			+can-poison+)

(gen-interaction-predicate (be-consumed)
  (or (lookup-basic-interaction object +can-be-drunk+)
      (lookup-basic-interaction object +can-be-eaten+)))

(gen-interaction-predicate (potion)
  (and (lookup-basic-interaction object +can-be-picked+)
       (lookup-basic-interaction object +can-be-drunk+)))

(gen-interaction-predicate (fountain)
  (and (not (lookup-basic-interaction object +can-be-picked+))
       (lookup-basic-interaction object +can-be-drunk+)))

(gen-interaction-predicate (weapon)
  (or (lookup-basic-interaction object +can-cut+)
      (lookup-basic-interaction object +can-smash+)
      (lookup-basic-interaction object +can-pierce+)
      (bowp object)
      (crossbowp object)))

(gen-interaction-predicate (bow)
  (lookup-basic-interaction object +can-launch-arrow+))

(gen-interaction-predicate (crossbow)
  (lookup-basic-interaction object +can-launch-bolt+))

(gen-interaction-predicate (armor)
  (and (lookup-basic-interaction object +can-intercept-attacks+)
       (lookup-basic-interaction object +can-be-worn-body+)
       (not (lookup-basic-interaction object +can-be-held-in-hand+))))

(gen-interaction-predicate (container)
  (and (not (lookup-basic-interaction object +can-be-picked+))
       (lookup-basic-interaction object +can-be-opened+)))

(gen-interaction-predicate (locked)
  (and (not (lookup-basic-interaction object +can-be-picked+))
       (lookup-basic-interaction object +can-be-opened+)
       (stringp (lookup-basic-interaction object +can-be-opened+))))

(gen-interaction-predicate (key)
  (and (lookup-basic-interaction object +can-be-picked+)
       (lookup-basic-interaction object +can-open+)))

(gen-interaction-predicate (shield)
  (and (lookup-basic-interaction object +can-intercept-attacks+)
       (not (lookup-basic-interaction object +can-be-worn-body+))
       (lookup-basic-interaction object +can-be-held-in-hand+)))

(gen-interaction-predicate (shoes)
  (lookup-basic-interaction object +can-be-worn-feet+))

(gen-interaction-predicate (elm)
  (lookup-basic-interaction object +can-be-worn-head+))

(gen-interaction-predicate (ring)
  (lookup-basic-interaction object +can-be-worn-hand+))

(gen-interaction-predicate (can-be-worn)
  (or (lookup-basic-interaction object +can-be-worn-feet+)
      (lookup-basic-interaction object +can-be-worn-arm+)
      (lookup-basic-interaction object +can-be-worn-neck+)
      (lookup-basic-interaction object +can-be-worn-head+)
      (lookup-basic-interaction object +can-be-worn-body+)
      (lookup-basic-interaction object +can-be-worn-hand+)))

(gen-interaction-predicate (trap)
  (and (lookup-basic-interaction object +can-be-dropped+)
       (not (lookup-basic-interaction object +can-attack+))
       (lookup-basic-interaction object +magic-effects+)))

(defmethod object-keycode ((object interactive-entity))
  (or (and (lookup-basic-interaction object +can-be-opened+)
	   (stringp (lookup-basic-interaction object +can-be-opened+))
	   (lookup-basic-interaction object +can-be-opened+))
      (and (lookup-basic-interaction object +can-open+)
	   (stringp (lookup-basic-interaction object +can-open+))
	   (lookup-basic-interaction object +can-open+))))

(defmethod import-interaction-from-definition ((object interactive-entity) file)
  (with-interaction-parameters-file (parameters file)
    (setf (basic-interaction-params object) parameters)
    object))

(defun prepare-format-description (label)
  (format nil "~~@[-~a~a~a~~]"
	  +gui-static-text-nbsp+
	  label
	  +gui-static-text-delim+))

(defmethod description-type ((object interactive-entity))
  (strcat
   (format nil (_ "~:[~;Edge weapon~]~:[~;Impact weapon~]~:[~;Range weapon~]~:[~;Range weapon~]~:[~;Fountain~]~:[~;Potion~]~:[~;Elm~]~:[~;Armor~]~:[~;Ring~]~:[~;Shoes~]~:[~;Trap~] ")
	   (can-cut-p   object)
	   (can-smash-p object)
	   (can-launch-bolt-p object)
	   (can-launch-arrow-p object)
	   (fountainp  object)
	   (potionp    object)
	   (elmp       object)
	   (armorp     object)
	   (ringp      object)
	   (shoesp     object)
	   (trapp      object))
   (call-next-method)))

(defmethod description-for-humans ((object interactive-entity))
  (strcat
   (format nil (prepare-format-description (_ "~a strength"))
	   (description-for-humans
	    (interaction-get-strength object)))
   (format nil (prepare-format-description (_ "~a stamina"))
	   (description-for-humans (interaction-get-stamina object)))
   (format nil (prepare-format-description (_ "~a dexterity"))
	   (description-for-humans (interaction-get-dexterity object)))
   (format nil (prepare-format-description (_ "~a agility"))
	   (description-for-humans (interaction-get-agility object)))
   (format nil (prepare-format-description (_ "~a smartness"))
	   (description-for-humans (interaction-get-smartness object)))
   (format nil (prepare-format-description (_ "~a empaty"))
	   (description-for-humans (interaction-get-empaty object)))
   (format nil (prepare-format-description (_ "~a weight"))
	   (description-for-humans (interaction-get-weight object)))
   (format nil (prepare-format-description (_ "~a damage points"))
	   (description-for-humans (interaction-get-damage-points object)))
   (format nil (prepare-format-description (_ "~a movement points"))
	   (description-for-humans (interaction-get-movement-points object)))
   (format nil (prepare-format-description (_ "~a magic points"))
	   (description-for-humans (interaction-get-magic-points object)))
   (format nil (prepare-format-description (_ "~a dodge chance"))
	   (description-for-humans (interaction-get-dodge-chance object)))
   (format nil (prepare-format-description (_ "~a melee attack chance"))
	   (description-for-humans (interaction-get-melee-attack-chance object)))
   (format nil (prepare-format-description (_ "~a range attack chance"))
	   (description-for-humans (interaction-get-range-attack-chance object)))
   (format nil (prepare-format-description (_ "~a melee attack damage"))
	   (description-for-humans (interaction-get-melee-attack-damage object)))
   (format nil (prepare-format-description (_ "~a range attack damage"))
	   (description-for-humans (interaction-get-range-attack-damage object)))
   (format nil (prepare-format-description (_ "~a edge weapons chance bonus"))
	   (description-for-humans (interaction-get-edge-weapons-chance-bonus object)))
   (format nil (prepare-format-description (_ "~a edge weapons damage bonus"))
	   (description-for-humans (interaction-get-edge-weapons-damage-bonus object)))
   (format nil (prepare-format-description (_ "~a impact weapons chance bonus"))
	   (description-for-humans (interaction-get-impact-weapons-chance-bonus object)))
   (format nil (prepare-format-description (_ "~a impact weapons damage bonus"))
	   (description-for-humans (interaction-get-impact-weapons-damage-bonus object)))
   (format nil (prepare-format-description (_ "~a pole weapons chance bonus"))
	   (description-for-humans (interaction-get-pole-weapons-chance-bonus object)))
   (format nil (prepare-format-description (_ "~a pole weapons damage bonus"))
	   (description-for-humans (interaction-get-pole-weapons-damage-bonus object)))
   (format nil (prepare-format-description (_ "~a unlock chance"))
	   (description-for-humans (interaction-get-unlock-chance object)))
   (format nil (prepare-format-description (_ "~a deactivate traps chance"))
	   (description-for-humans (interaction-get-deactivate-trap-chance object)))
   (format nil (prepare-format-description (_ "~a reply attack chance"))
	   (description-for-humans (interaction-get-reply-attack-chance object)))
   (format nil (prepare-format-description (_ "~a ambush attack chance"))
	   (description-for-humans (interaction-get-ambush-attack-chance object)))
   (format nil (prepare-format-description (_ "~a spell chance"))
	   (description-for-humans (interaction-get-spell-chance object)))
   (format nil (prepare-format-description (_ "~a attack spell chance"))
	   (description-for-humans (interaction-get-attack-spell-chance object)))
   (format nil (prepare-format-description (_ "~a recover DMG"))
	   (description-for-humans (interaction-get-heal-damage-points object)))
   (format nil (prepare-format-description (_ "heals poison condition ~a"))
	   (description-for-humans (interaction-get-heal-poison object)))
   (format nil (prepare-format-description (_ "heals berserk condition ~a"))
	   (description-for-humans (interaction-get-heal-berserk object)))
   (format nil (prepare-format-description (_ "heals faint condition ~a"))
	   (description-for-humans (interaction-get-heal-faint object)))
   (format nil (prepare-format-description (_ "heals terror condition ~a"))
	   (description-for-humans (interaction-get-heal-terror object)))
   (format nil (prepare-format-description (_ "makes immune from poison (~a)"))
	   (description-for-humans (interaction-get-immune-poison object)))
   (format nil (prepare-format-description (_ "makes immune from terror (~a)"))
	   (description-for-humans (interaction-get-immune-terror object)))
   (format nil (prepare-format-description (_ "makes immune from berserk (~a)"))
	   (description-for-humans (interaction-get-immune-berserk object)))
   (format nil (prepare-format-description (_ "makes immune from faint (~a)"))
	   (description-for-humans (interaction-get-immune-faint object)))
   (format nil (prepare-format-description (_ "~a cause faint condition"))
	   (description-for-humans (interaction-get-cause-faint object)))
   (format nil (prepare-format-description (_ "~a cause poison condition"))
	   (description-for-humans (interaction-get-cause-poison object)))
   (format nil (prepare-format-description (_ "~a cause terror condition"))
	   (description-for-humans (interaction-get-cause-terror object)))
   (format nil (prepare-format-description (_ "~a cause berserk condition"))
	   (description-for-humans (interaction-get-cause-berserk object)))
   (format nil (prepare-format-description (_ "launchs ~a"))
	   (description-for-humans (interaction-get-magic-effect object)))))

(defun clean-single-contradiction (interaction-params path1 path2)
  (when (and (plist-path-value interaction-params path1)
	     (plist-path-value interaction-params path2))
    (if (dice:pass-d2 1)
	(n-setf-path-value interaction-params path1 nil)
	(n-setf-path-value interaction-params path2 nil))))

(alexandria:define-constant +all-effects+
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
	  :magic-effect)
  :test #'equalp)

(defmethod clean-effects ((object interactive-entity))
  (with-accessors ((basic-interaction-params basic-interaction-params)) object
    (clean-single-contradiction basic-interaction-params
				(list +healing-effects+ +immune-berserk+)
				(list +healing-effects+ +cause-berserk+))
    (clean-single-contradiction basic-interaction-params
				(list +healing-effects+ +immune-faint+)
				(list +healing-effects+ +cause-faint+))
    (clean-single-contradiction basic-interaction-params
				(list +healing-effects+ +immune-terror+)
				(list +healing-effects+ +cause-terror+))
    (clean-single-contradiction basic-interaction-params
				(list +healing-effects+ +immune-poison+)
				(list +healing-effects+ +cause-poison+))
    (clean-single-contradiction basic-interaction-params
				(list +healing-effects+ +cause-berserk+)
				(list +healing-effects+ +heal-berserk+))
    (clean-single-contradiction basic-interaction-params
				(list +healing-effects+ +cause-faint+)
				(list +healing-effects+ +heal-faint+))
    (clean-single-contradiction basic-interaction-params
				(list +healing-effects+ +cause-terror+)
				(list +healing-effects+ +heal-terror+))
    (clean-single-contradiction basic-interaction-params
				(list +healing-effects+ +cause-poison+)
				(list +healing-effects+ +heal-poison+))
    object))

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
			  :can-be-worn-arm
			  :can-be-worn-head
			  :can-be-worn-neck
			  :can-be-worn-feet
			  :can-be-worn-body
			  :can-be-worn-hand
			  :can-be-held-in-hand
			  :can-cut
			  :can-smash
			  :can-pierce
			  :can-launch-bolt
			  :can-launch-arrow
			  :mounted-on-pole)))

(defun conflict-all-effects-except (&rest to-be-removed)
  (reduce #'intersection
	  (mapcar #'(lambda (r) (remove r +all-effects+ :test #'equalp))
		  to-be-removed)
	  :initial-value +all-effects+))

(defparameter *relations* `((:path (:can-be-worn-hand)
				   :conflicts  (:can-be-held-in-hand)
				   :depends-on (:can-be-picked))
			    (:path (:can-be-held-in-hand)
				   :conflicts  (:can-be-worn-hand)
				   :depends-on (:can-be-picked))
			    (:path (:can-talk)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects-except)))
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
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole
							(conflict-all-effects-except))))
			    (:path (:can-be-opened)
				   :conflicts ,(append (list
							:can-talk
							:can-open
							:can-attack
							:can-heal
							:can-be-heal
							:can-poison
							:can-be-poisoned
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole
							(conflict-all-effects-except))))
			    (:path (:can-attack)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects-except)))
			    (:path (:can-be-attacked)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects-except)))
			    (:path (:can-be-destroyed)
				   :conflicts ,(append (list
							:can-open
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects-except
							'(:healing-effects :heal-poison)
							'(:healing-effects :heal-berserk)
							'(:healing-effects :heal-faint)
							'(:healing-effects :heal-terror)
							'(:healing-effects :cause-poison)
							'(:healing-effects :cause-berserk)
							'(:healing-effects :cause-faint)
							'(:healing-effects :cause-terror))))
			    (:path (:can-be-burned)
				   :conflicts ,(append (list
							:can-open
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects-except
							'(:healing-effects :heal-poison)
							'(:healing-effects :heal-berserk)
							'(:healing-effects :heal-faint)
							'(:healing-effects :heal-terror)
							'(:healing-effects :cause-poison)
							'(:healing-effects :cause-berserk)
							'(:healing-effects :cause-faint)
							'(:healing-effects :cause-terror))))
			    (:path (:can-heal)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects-except)))
			    (:path (:can-be-heal)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects-except)))
			    (:path (:can-poison)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects-except)))
			    (:path (:can-be-poisoned)
				   :conflicts ,(append (list
							:can-open
							:can-be-opened
							:can-be-drunk
							:can-be-eaten
							:can-be-worn-arm
							:can-be-worn-head
							:can-be-worn-neck
							:can-be-worn-feet
							:can-cut
							:can-smash
							:can-pierce
							:can-launch-bolt
							:can-launch-arrow
							:mounted-on-pole)
						       (conflict-all-effects-except)))
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
					       :can-be-worn-arm
					       :can-be-worn-head
					       :can-be-worn-neck
					       :can-be-worn-feet
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
					       :can-be-worn-arm
					       :can-be-worn-head
					       :can-be-worn-neck
					       :can-be-worn-feet
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
			    (:path     (:can-be-worn-arm)
				       :conflicts ,(%trivial-conflict-list :can-be-worn-arm))
			    (:path     (:can-be-worn-head)
				       :conflicts ,(%trivial-conflict-list :can-be-worn-head))
			    (:path     (:can-be-worn-neck)
				       :conflicts ,(%trivial-conflict-list :can-be-worn-neck))
			    (:path     (:can-be-worn-feet)
				       :conflicts ,(%trivial-conflict-list :can-be-worn-feet))))

(defun listify (a)
  (if (listp a) a (list a)))

(defun get-path (path relations)
  (find (listify path)
	relations
	:test #'equalp :key #'(lambda (a) (getf a :path))))

(defun get-conflict-elements (item)
  (getf item :conflicts))

(defun get-depend-elements (item)
  (getf item :depends-on))

(defun item-set-p (path interaction)
  (let ((val (recursive-assoc (listify path) interaction)))
    (and val
	 (not (find val +nil-equiv-bag+)))))

(defun conflictp (interactions item)
  (let* ((potential-conflicts (get-conflict-elements item))
	 (item-set-p (recursive-assoc (listify (getf item :path))  interactions))
	 (conflict (remove-if-null (mapcar #'(lambda (path)
					       (if (item-set-p path interactions)
						   path
						   nil))
					   potential-conflicts))))
    (when (and item-set-p conflict)
      (error (format nil "~a conflict with ~a" (getf item :path) conflict)))
    (and item-set-p conflict)))

(defun depends-on-p (interactions item)
  (let* ((potential-dependencies (get-depend-elements item))
	 (item-set-p (recursive-assoc (listify (getf item :path))  interactions))
	 (dependencies  (remove-if-null (mapcar #'(lambda (path)
							(if (not (item-set-p path interactions))
							    path
							    nil))
						    potential-dependencies))))

    (when (and item-set-p dependencies)
      (error (format nil "~a depend on ~a" (getf item :path) dependencies)))
    (and item-set-p dependencies)))

(defun sum-effects-mod (interactions path)
  (reduce #'+ (mapcar #'(lambda (a) (if (typep (cdr a) 'effect-parameters) (modifier (cdr a)) 0.0))
		      (plist-path-value interactions path))
	  :initial-value 0.0))

(defun validate-interaction-file (file)
  (with-interaction-parameters-file (params file)
    (values (find-if #'(lambda (a) (not (null a)))
		     (loop for i in *relations* collect (conflictp params i)))
	    (find-if #'(lambda (a) (not (null a)))
		     (loop for i in *relations* collect (depends-on-p params i))))))

(defun get-effects-shuffled (l num)
  (subseq (shuffle l) 0
	  (if (< num (length l))
	      num
	      (length l))))

(defun get-normal-fx-shuffled (db num)
  (let ((all (plist-path-value db (list +effects+))))
    (get-effects-shuffled (mapcar #'car (remove-if #'(lambda (a) (null (cdr a))) all)) num)))

(defun get-healing-fx-shuffled (db num)
  (let ((all (plist-path-value db (list +healing-effects+))))
    (get-effects-shuffled (mapcar #'car (remove-if #'(lambda (a) (null (cdr a))) all)) num)))

(defun get-magic-fx-shuffled (db num)
  (and (> num 0)
       (plist-path-value db (list +magic-effects+))))

(defun cdr-eq-generate-p (i)
  (eq :generate (cdr i)))

(defun eq-generate-p (i)
  (eq :generate i))

(defun remove-generate-symbols (db)
  (if (null db)
      nil
      (append
       (loop for i in db collect
	    (cons (car i)
		  (cond
		    ((cdr-eq-generate-p i)
		     nil)
		    ((listp (cdr i))
		     (remove-generate-symbols (cdr i)))
		    (t
		     (cdr i))))))))

(defclass entity-w-portrait ()
  ((portrait
    :initform nil
    :initarg :portrait
    :accessor portrait
    :type texture:texture)))

(defmethod marshal:class-persistant-slots ((object entity-w-portrait))
  (append  '(portrait)
	   (call-next-method)))
