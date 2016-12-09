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

(in-package :character)

(alexandria:define-constant +unknown-ability-bonus+             -5.0                    :test #'=)

(alexandria:define-constant +starting-exp-points+               10.0                    :test #'=)

(alexandria:define-constant +max-inventory-slots-page+           3.0                    :test #'=)

(alexandria:define-constant +weight-for-half-capacity-inventory+ 20.0                   :test #'=)

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

(defparameter *standard-capital-characteristic* 200)

(defclass interactive-entity ()
  ((basic-interaction-params
    :initform nil
    :initarg  :basic-interaction-params
    :accessor basic-interaction-params)))

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
  (misc:dbg " end turn character ~a(~a) ~a" (type-of object) (id object) (type-of event))
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
  (format nil (_ "~:[~;Edge weapon~]~:[~;Impact weapon~]~:[~;Range weapon~]~:[~;Range weapon~]~:[~;Fountain~]~:[~;Potion~]~:[~;Elm~]~:[~;Armor~]~:[~;Ring~]~:[~;Shoes~]~:[~;Trap~] ~a ~a")
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
	   (trapp      object)
	   (first-name object)
	   (last-name  object)
	   +gui-static-text-delim+))

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

(defclass np-character (identificable interactive-entity m-tree)
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
   (description
    :initform ""
    :initarg :description
    :accessor description
    :type string)
   (portrait
    :initform nil
    :initarg :portrait
    :accessor portrait
    :type texture:texture)
   (weight
    :initarg :weight
    :initform 35
    :accessor weight
    :type integer)
   (damage-points
    :initarg :damage-points
    :initform 0.0
    :accessor damage-points
    :type integer)
   (current-damage-points
    :initarg :current-damage-points
    :initform 0.0
    :accessor current-damage-points
    :type integer)
   (level
    :initarg :level
    :initform 1
    :accessor level)
   (age
    :initarg :age
    :initform 0
    :accessor age)))

(defmethod marshal:class-persistant-slots ((object np-character))
  (append  '(first-name
	     last-name
	     description
	     portrait
	     weight
	     damage-points
	     current-damage-points
	     level
	     age
	     basic-interaction-params)
	   (call-next-method)))
;;;; interaction

(defmethod description-for-humans :around ((object np-character))
  (strcat
   (format nil (_ "~:[~;Edge weapon~]~:[~;Impact weapon~]~:[~;Range weapon~]~:[~;Range weapon~]~:[~;Fountain~]~:[~;Potion~]~:[~;Elm~]~:[~;Armor~]~:[~;Ring~]~:[~;Shoes~]~:[~;Trap~] ~a ~a~a")
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
	   (trapp      object)
	   (first-name object)
	   (last-name  object)
	   +gui-static-text-delim+)
   (call-next-method)))

(defgeneric restart-age (object))

(defmethod restart-age ((object np-character))
  (setf (age object) 0.0))

(defun init-postponed-messages-functions (pq)
  (setf (pq:key-function     pq) #'game-event:trigger-turn)
  (setf (pq:compare-function pq) #'<)
  (setf (pq:equal-function   pq) #'=)
  pq)

(defclass player-character (np-character)
  ((model-origin-dir
    :initform ""
    :initarg  :model-origin-dir
    :accessor model-origin-dir)
   (current-path
    :initform nil
    :initarg  :current-path
    :accessor current-path)
   (gender
    :initarg :gender
    :initform :male
    :accessor gender)
   (player-class
    :initarg :player-class
    :initform nil
    :accessor player-class)
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
   (movement-points
    :initarg :movement-points
    :initform 0
    :accessor movement-points
    :type integer)
   (current-movement-points
    :initarg :current-movement-points
    :initform 0
    :accessor current-movement-points
    :type integer)
   (magic-points
    :initarg :magic-points
    :initform 0
    :accessor magic-points
    :type integer)
   (current-magic-points
    :initarg :current-magic-points
    :initform 0
    :accessor current-magic-points
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
   (modifiers-effects
    :initarg  :modifiers-effects
    :initform '()
    :accessor modifiers-effects)
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
    :initarg  :status
    :initform nil
    :accessor status)
   (immune-faint-status
    :initarg :immune-faint-status
    :initform nil
    :accessor immune-faint-status)
   (immune-berserk-status
    :initarg :immune-berserk-status
    :initform nil
    :accessor immune-berserk-status)
   (immune-poison-status
    :initarg :immune-poison-status
    :initform nil
    :accessor immune-poison-status)
   (immune-terror-status
    :initarg :immune-terror-status
    :initform nil
    :accessor immune-terror-status)
   (recurrent-effects
    :initarg  :recurrent-effects
    :initform nil
    :accessor recurrent-effects)
   (postponed-messages
    :initform (init-postponed-messages-functions (make-instance 'pq:priority-queue))
    :initarg  :postponed-messages
    :accessor postponed-messages)
   (race
    :initarg :race
    :initform :human
    :accessor race)
   (exp-points
    :initarg :exp-points
    :initform 0
    :accessor exp-points)
   (elm
    :initarg :elm
    :initform nil
    :accessor elm)
   (shoes
    :initarg :shoes
    :initform nil
    :accessor shoes)
   (armor
    :initarg :armor
    :initform nil
    :accessor armor)
   (left-hand
    :initarg :left-hand
    :initform nil
    :accessor left-hand)
   (right-hand
    :initarg :right-hand
    :initform nil
    :accessor right-hand)
   (ring
    :initarg :ring
    :initform nil
    :accessor ring)
   (spell-loaded
    :initarg  :spell-loaded
    :initform nil
    :accessor spell-loaded)
   (inventory
    :initarg :inventory
    :initform '()
    :accessor inventory)))

(defmethod initialize-instance :after ((object player-character) &key &allow-other-keys)
  ;; copy some new points to current
  (setf (current-damage-points   object) (damage-points object))
  (setf (current-movement-points object) (movement-points object))
  (setf (current-magic-points    object) (magic-points object)))

(defmethod print-object ((object player-character) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
	    "description ~a name ~a ~a~%class: ~a~%gender: ~a~%strength: ~a ~%stamina: ~a ~%dexterity: ~a ~%agility: ~a ~%smartness: ~a ~%empaty: ~a ~%weight: ~a ~%damage-points: ~a ~%movement-points: ~a ~%magic-points: ~a ~%dodge-chance: ~a ~%melee-attack-chance: ~a ~%range-attack-chance: ~a ~%melee-attack-damage: ~a ~%range-attack-damage: ~a ~%edge-weapons-chance-bonus: ~a ~%edge-weapons-damage-bonus: ~a ~%impact-weapons-chance-bonus: ~a ~%impact-weapons-damage-bonus: ~a ~%pole-weapons-chance-bonus: ~a ~%pole-weapons-damage-bonus: ~a ~%unlock-chance: ~a ~%deactivate-trap-chance: ~a ~%reply-attack-chance: ~a ~%ambush-attack-chance: ~a ~%spell-chance: ~a ~%attack-spell-chance: ~a ~%status: ~a  ~%race: ~a ~%level: ~a ~%exp-points: ~a~%interaction ~a inventory ~a"
	    (description                 object)
	    (first-name                  object)
	    (last-name                   object)
	    (player-class                object)
	    (gender                      object)
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
	    (basic-interaction-params    object)
	    (mapcar #'description-for-humans (inventory object)))))

(defmethod marshal:class-persistant-slots ((object player-character))
  (append  '(model-origin-dir
	     current-path
	     gender
	     player-class
	     strength
	     stamina
	     dexterity
	     agility
	     smartness
	     empaty
	     weight
	     movement-points
	     current-movement-points
	     magic-points
	     current-magic-points
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
	     modifiers-effects
	     unlock-chance
	     deactivate-trap-chance
	     reply-attack-chance
	     ambush-attack-chance
	     spell-chance
	     attack-spell-chance
	     status
	     immune-faint-status
	     immune-berserk-status
	     immune-poison-status
	     immune-terror-status
	     recurrent-effects
	     race
	     exp-points)
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
    (init-postponed-messages-functions (postponed-messages res))
    res))

;; events

(defmethod game-event:on-game-event :after ((object player-character) (event game-event:end-turn))
  (misc:dbg " end turn character ~a(~a) ~a" (type-of object) (id object) (type-of event))
  (remove-decayed-items object (game-event:end-turn-count event))
  nil)

(defgeneric random-fill-slots (object capital characteristics))

(defgeneric inventory-slot-pages-number (object))

(defgeneric player-class->class-description (object))

(defgeneric player-gender->gender-description (object))

(defgeneric reset-movement-points (object))

(defgeneric reset-magic-points (object))

(defgeneric add-to-inventory (object item))

(defgeneric remove-decayed-items (object turn-count))

(defgeneric remove-from-inventory (object item))

(defgeneric remove-from-modifiers (object item-origin-id))

(defgeneric sum-modifiers (object modifier-name))

(defgeneric item->player-character-slot (object item))

(defgeneric item->available-player-character-slot (object item))

(defgeneric worn-weapon (object))

(defgeneric weapon-type (object))

(defgeneric weapon-type-short-range (object))

(defgeneric weapon-type-long-range (object))

(defmacro gen-player-class-test (name)
  (let ((name-fn (format-symbol t "~:@(pclass-~a-p~)" name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object player-character))
	 (eq (player-class object) ,(alexandria:make-keyword name))))))

(gen-player-class-test ranger)

(gen-player-class-test warrior)

(gen-player-class-test archer)

(gen-player-class-test wizard)

(gen-player-class-test healer)

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

(defmethod inventory-slot-pages-number ((object player-character))
  (ceiling (enzyme-kinetics +max-inventory-slots-page+
			    +weight-for-half-capacity-inventory+
			    (d (weight object)))))

(defmethod player-class->class-description ((object player-character))
  (with-accessors ((player-class player-class)) object
    (ecase player-class
      (:warrior
       (_ "warrior"))
      (:archer
       (_ "archer"))
      (:wizard
       (_ "wizard"))
      (:healer
       (_ "healer"))
      (:ranger
       (_ "ranger")))))

(defmethod player-gender->gender-description ((object player-character))
  (with-accessors ((gender gender)) object
    (ecase gender
      (:male
       (_ "male"))
      (:female
       (_ "female")))))

(defmethod reset-movement-points ((object player-character))
  (setf (current-movement-points object) (actual-movement-points object)))

(defmethod reset-magic-points ((object player-character))
  (setf (current-magic-points object) (magic-points object)))

(defmethod add-to-inventory ((object player-character) item)
  (game-event:register-for-end-turn item)
  (push item (character:inventory object)))

(defmethod remove-decayed-items ((object player-character) turn-count)
  (with-accessors ((inventory inventory)
		   (elm        elm)
		   (shoes      shoes)
		   (armor      armor)
		   (left-hand  left-hand)
		   (right-hand right-hand)
		   (ring       ring)) object
    (flet ((decayedp (item)
	     (let* ((decay-params (interaction-get-decay item))
		    (decay-point  (and decay-params (interaction:points decay-params))))
	       (and decay-point
		    (>= (age item) decay-point)))))
      (let* ((new-inventory (remove-if #'(lambda (a) (decayedp a)) inventory))
	     (removed-items (remove-if #'(lambda (a) (not (decayedp a))) inventory)))
	(when (and elm
		   (decayedp elm))
	  (push elm removed-items)
	  (setf elm nil))
	(when (and shoes
		   (decayedp shoes))
	  (push shoes removed-items)
	  (setf shoes nil))
	(when (and armor
		   (decayedp armor))
	  (push armor removed-items)
	  (setf armor nil))
	(when (and left-hand
		   (decayedp left-hand))
	  (push left-hand removed-items)
	  (setf left-hand nil))
	(when (and right-hand
		   (decayedp right-hand))
	  (push right-hand removed-items)
	  (setf right-hand nil))
	(when (and ring
		   (decayedp ring))
	  (push ring removed-items)
	  (setf ring nil))
	(setf inventory new-inventory)
	(values removed-items inventory)))))

(defmethod remove-from-inventory ((object player-character) item)
  (setf (inventory object) (remove (id item) (inventory object) :key #'id :test #'=)))

(defmethod remove-from-modifiers ((object player-character) item-origin-id)
  (with-accessors ((modifiers-effects modifiers-effects)) object
    (setf modifiers-effects
	  (remove-if #'(lambda (a)
			 (= (random-object-messages:msg-origin a)
			    item-origin-id))
		     modifiers-effects))))

(defmethod sum-modifiers ((object player-character) modifier-name)
  (with-accessors ((modifiers-effects modifiers-effects)) object
    (reduce #'(lambda (a b) (d+ a (random-object-messages:msg-modifier b)))
	    (remove-if #'(lambda (a)
			   (or (not (typep a 'random-object-messages:modifier-effect-msg))
			       (not (eq    modifier-name
					   (random-object-messages:msg-characteristic a)))))
		       modifiers-effects)
	    :initial-value 0.0)))

(defmethod item->player-character-slot ((object player-character) item)
  (with-accessors ((left-hand  left-hand)
		   (right-hand right-hand)) object
    (when item
      (let ((item-id (id item)))
	 (cond
	   ((and (character:ringp item)
		 (ring object)
		 (= item-id (id (ring object))))
	    'character:ring)
	   ((and (character:armorp item)
		 (armor object)
		 (= item-id (id (armor object))))
	    'character:armor)
	   ((and (character:elmp item)
		 (elm object)
		 (= item-id (id (elm object))))
	    'character:elm)
	   ((and (character:shoesp item)
		 (shoes object)
		 (= item-id (id (shoes object))))
	    'character:shoes)
	   ((or (character:weaponp item)
		(character:shieldp item))
	    (cond
	      ((and (left-hand object)
		    (= item-id (id (left-hand object))))
	       'character:left-hand)
	      ((and (right-hand object)
		    (= item-id (id (right-hand object))))
	       'character:right-hand))))))))

(defmethod item->available-player-character-slot ((object player-character) item)
  (with-accessors ((left-hand  left-hand)
		   (right-hand right-hand)) object
    (and item
	 (cond
	   ((character:ringp item)
	    'character:ring)
	   ((character:armorp item)
	    'character:armor)
	   ((character:elmp item)
	    'character:elm)
	   ((character:shoesp item)
	    'character:shoes)
	   ((or (character:weaponp item)
		(character:shieldp item)
		(character:bowp item)
		(character:crossbowp item))
	    (if (null left-hand)
		'character:left-hand
		(if (null right-hand)
		    'character:right-hand
		    nil)))))))

(defmethod worn-weapon ((object player-character))
  (cond
    ((weaponp (left-hand object))
     (left-hand object))
    ((weaponp (right-hand object))
     (right-hand object))
    (t
     nil)))

(defmethod weapon-type ((object player-character))
  (or (weapon-type-long-range  object)
      (weapon-type-short-range object)))

(defmethod weapon-type (object)
  nil)

(defmethod weapon-type-short-range ((object player-character))
 (let* ((weapon       (worn-weapon object))
	(weapon-type  (when weapon
			(cond
			  ((can-cut-p weapon)
			   :edge)
			  ((can-smash-p weapon)
			   :impact)
			  ((mounted-on-pole-p weapon)
			   :pole)
			  (t
			   nil)))))
   weapon-type))

(defmethod weapon-type-long-range ((object player-character))
 (let* ((weapon       (worn-weapon object))
	(weapon-type  (when weapon
			(cond
			  ((bowp weapon)
			   :bow)
			  ((crossbowp weapon)
			   :crossbow)
			  (t
			   nil)))))
   weapon-type))

(defmacro gen-actual-characteristic (name)
  (let ((fn       (format-fn-symbol t "actual-~a" name))
	(accessor (format-fn-symbol t "~a"        name))
	(constant (format-fn-symbol t "+~a+"      name)))
    `(progn
       (defgeneric ,fn (object))
       (defmethod  ,fn ((object player-character))
	 (d+ (,accessor object)
	     (sum-modifiers object ,constant))))))

(defmacro gen-actual-characteristics (&rest names)
  `(progn
     ,@(loop for name in names collect
	    `(gen-actual-characteristic ,name))))

(gen-actual-characteristics damage-points
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
			    attack-spell-chance)

(defmacro gen-make-player (player-class)
  (alexandria:with-gensyms (char rest-capital)
    `(defun ,(alexandria:format-symbol t "~@:(make-~a~)" player-class)
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
	 (setf (damage-points ,char)   (d (truncate (* (stamina ,char) 0.5)))
	       (movement-points ,char) (d (truncate (* (agility ,char) 0.5)))
	       (magic-points ,char) (d (truncate (/ (alexandria:lerp 0.1
								     (alexandria:lerp 0.5
										      (smartness ,char)
										      (empaty ,char))
								     (dexterity ,char))
						    3)))
	       (dodge-chance ,char) (d (truncate (max 0 (- (agility ,char) (/ (weight ,char) 2)))))
	       (melee-attack-chance ,char) (alexandria:lerp 0.3 (strength ,char) (agility ,char))
	       (range-attack-chance ,char) (alexandria:lerp 0.1
							    (dexterity ,char)
							    (agility   ,char))
	       (melee-attack-damage ,char) (d (truncate (* (strength ,char) 0.25)))
	       (range-attack-damage ,char) (d (truncate (* (dexterity ,char) 0.25)))
	       (unlock-chance ,char) (d (truncate (/ (alexandria:lerp 0.9
								   (agility ,char)
								   (dexterity ,char))

						  10)))
	       (deactivate-trap-chance ,char) (d (truncate
						  (/ (alexandria:lerp 0.8
								      (agility ,char)
								      (dexterity ,char))
						     10)))
	       (reply-attack-chance ,char)  (d (truncate (/ (agility ,char) 5)))
	       (ambush-attack-chance ,char) (d (truncate
						(/ (alexandria:lerp 0.9
								    (agility ,char)
								    (strength ,char))
						   10)))
	       (spell-chance ,char) (d (truncate (/ (alexandria:lerp 0.9
								     (smartness ,char)
								     (empaty ,char))
						    3)))
	       (attack-spell-chance ,char) (d (truncate (/ (alexandria:lerp 0.9
									    (agility ,char)
									    (smartness ,char))
							   3)))
	       (race ,char) race)
	 (if (> ,rest-capital 0)
	     (,(alexandria:format-symbol t "~@:(make-~a~)" player-class)
	       ,rest-capital race ,char slots)
	     (progn
	       (setf (current-magic-points    ,char) (magic-points ,char)
		     (current-movement-points ,char) (movement-points ,char)
		     (current-damage-points    ,char) (damage-points ,char))
	       (values ,char ,rest-capital)))))))

(gen-make-player player)

(defun make-warrior (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((strength (50 10))
									  (stamina  (40 10))
									  (dexterity (5 3))
									  (agility   (10 3))
									  (empaty    (2 0))
									  (smartness (2 0))
									  (weight    (52 23))))))
    (setf (player-class player) :warrior)
    player))

(defun make-wizard (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((smartness (40 10))
									  (agility   (30 10))
									  (stamina  (20 10))
									  (strength (10 10))
									  (dexterity (5 3))
									  (empaty    (2 0))
									  (weight    (30 5))))))
    (setf (player-class player) :wizard)
    player))

(defun make-healer (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((empaty (40 10))
									  (smartness (20 20))
									  (agility   (15 10))
									  (stamina  (10 10))
									  (strength (10 10))
									  (dexterity (5 3))
									  (weight    (42 12))))))
    (setf (player-class player) :healer)
    player))

(defun make-archer (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((dexterity (50 0))
									  (stamina  (40 10))
									  (strength (10 10))
									  (empaty    (1 1))
									  (agility   (10 3))
									  (smartness (5 3))
									  (weight    (30 55))))))
    (setf (player-class player) :archer)
    player))

(defun make-ranger (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((agility (60 0))
									  (dexterity  (40 10))
									  (stamina (15 10))
									  (strength (15 10))
									  (empaty    (10 10))
									  (smartness (5 3))
									  (weight    (56 20))))))
    (incf (deactivate-trap-chance player) 3)
    (setf (player-class player) :ranger)
    player))

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
			+description+
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

(defun calculate-randomized-damage-points (level
					   min-level max-level
					   min-damage max-damage
					   displacement)
  (let ((val (num:dlerp (num:smoothstep-interpolate (d min-level) (d max-level) (d level))
			min-damage
			max-damage)))
    (num:gaussian-probability (d* displacement val) val)))

(defun params->np-character (params)
  (let ((results (make-instance 'np-character
				:description   (fetch-description                   params)
				:first-name    (fetch-first-name                    params)
				:last-name     (fetch-last-name                     params)
				:portrait      (and (fetch-portrait params)
						    (texture:get-texture (fetch-portrait params)))
				:weight        (fetch-weight                        params)
				:damage-points (fetch-damage-points                 params)
				:level         (fetch-level                         params)
				:age           0)))
    (when (portrait results)
      (setf (texture:s-wrap-mode (portrait results)) :clamp-to-border)
      (setf (texture:t-wrap-mode (portrait results)) :clamp-to-border)
      (setf (texture:border-color (portrait results)) §c00000000)
      (texture:prepare-for-rendering (portrait results)))
    results))

(defun params->player-character (params)
  (let ((results (make-instance
		  'player-character
		  :description                 (fetch-description                   params)
		  :first-name                  (fetch-first-name                    params)
		  :last-name                   (fetch-last-name                     params)
		  :portrait                    (and (fetch-portrait params)
						    (texture:get-texture (fetch-portrait params)))
		  :strength                    (fetch-strength                      params)
		  :stamina                     (fetch-stamina                       params)
		  :dexterity                   (fetch-dexterity                     params)
		  :agility                     (fetch-agility                       params)
		  :smartness                   (fetch-smartness                     params)
		  :empaty                      (fetch-empaty                        params)
		  :weight                      (fetch-weight                        params)
		  :damage-points               (fetch-damage-points                 params)
		  :movement-points             (fetch-movement-points               params)
		  :magic-points                (fetch-magic-points                  params)
		  :dodge-chance                (fetch-dodge-chance                  params)
		  :melee-attack-chance         (fetch-melee-attack-chance           params)
		  :range-attack-chance         (fetch-range-attack-chance           params)
		  :melee-attack-damage         (fetch-melee-attack-damage           params)
		  :range-attack-damage         (fetch-range-attack-damage           params)
		  :edge-weapons-chance-bonus   (fetch-edge-weapons-chance-bonus     params)
		  :edge-weapons-damage-bonus   (fetch-edge-weapons-damage-bonus     params)
		  :impact-weapons-chance-bonus (fetch-impact-weapons-chance-bonus   params)
		  :impact-weapons-damage-bonus (fetch-impact-weapons-damage-bonus   params)
		  :pole-weapons-chance-bonus   (fetch-pole-weapons-chance-bonus     params)
		  :pole-weapons-damage-bonus   (fetch-pole-weapons-damage-bonus     params)
		  :unlock-chance               (fetch-unlock-chance                 params)
		  :deactivate-trap-chance      (fetch-deactivate-trap-chance        params)
		  :reply-attack-chance         (fetch-reply-attack-chance           params)
		  :ambush-attack-chance        (fetch-ambush-attack-chance          params)
		  :spell-chance                (fetch-spell-chance                  params)
		  :attack-spell-chance         (fetch-attack-spell-chance           params)
		  :status                      (fetch-status                        params)
		  :race                        (fetch-race                          params)
		  :level                       (fetch-level                         params)
		  :age                         0
		  :exp-points                  (fetch-exp-points                    params))))
    (when (portrait results)
      (setf (texture:s-wrap-mode (portrait results)) :clamp-to-border)
      (setf (texture:t-wrap-mode (portrait results)) :clamp-to-border)
      (setf (texture:border-color (portrait results)) §c00000000)
      (texture:prepare-for-rendering (portrait results)))
    results))

(defun load-randomize-character (file)
  (with-character-parameters (params file)
    (let ((results (make-instance 'player-character
				  :first-name                  (randomize-a-bit
								(fetch-first-name params))
				  :last-name                   (randomize-a-bit
								(fetch-last-name params))
				  :portrait                    (texture:get-texture
								 (fetch-portrait params))
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

(defun conflict-all-effects-except (&rest to-be-removed)
  (reduce #'intersection
	  (mapcar #'(lambda (r) (remove r +all-effects+ :test #'equalp))
		  to-be-removed)
	  :initial-value +all-effects+))

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

(defun validate-interaction-file (file)
  (with-interaction-parameters-file (params file)
    (values (find-if #'(lambda (a) (not (null a)))
		     (loop for i in *relations* collect (conflictp params i)))
	    (find-if #'(lambda (a) (not (null a)))
		     (loop for i in *relations* collect (depends-on-p params i))))))

(defun %get-effects-shuffled (l num)
  (subseq (shuffle l) 0
	  (if (< num (length l))
	      num
	      (length l))))

(defun %get-normal-fx-shuffled (db num)
  (let ((all (plist-path-value db (list +effects+))))
    (%get-effects-shuffled (mapcar #'car (remove-if #'(lambda (a) (null (cdr a))) all)) num)))

(defun %get-healing-fx-shuffled (db num)
  (let ((all (plist-path-value db (list +healing-effects+))))
    (%get-effects-shuffled (mapcar #'car (remove-if #'(lambda (a) (null (cdr a))) all)) num)))

(defun %get-magic-fx-shuffled (db num)
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

(defun sum-effects-mod (interactions path)
  (reduce #'+ (mapcar #'(lambda (a) (if (typep (cdr a) 'effect-parameters) (modifier (cdr a)) 0.0))
		      (plist-path-value interactions path))
	  :initial-value 0.0))
