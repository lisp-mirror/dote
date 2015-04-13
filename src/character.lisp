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

(alexandria:define-constant +unknown-ability-bonus+  -5 :test #'=)

(alexandria:define-constant +starting-exp-points+    10 :test #'=)

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
   (damage-point
    :initarg :damage-point
    :initform 0
    :accessor damage-point
    :type integer)
   (movement-point
    :initarg :movement-point
    :initform 0
    :accessor movement-point
    :type integer)
   (magic-point
    :initarg :magic-point
    :initform 0
    :accessor magic-point
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
    :accessor exp-points)))

(defmethod print-object ((object player-character) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
	    "~%strength: ~a ~%stamina: ~a ~%dexterity: ~a ~%agility: ~a ~%smartness: ~a ~%empaty: ~a ~%weight: ~a ~%damage-point: ~a ~%movement-point: ~a ~%magic-point: ~a ~%dodge-chance: ~a ~%melee-attack-chance: ~a ~%range-attack-chance: ~a ~%melee-attack-damage: ~a ~%range-attack-damage: ~a ~%edge-weapons-chance-bonus: ~a ~%edge-weapons-damage-bonus: ~a ~%impact-weapons-chance-bonus: ~a ~%impact-weapons-damage-bonus: ~a ~%pole-weapons-chance-bonus: ~a ~%pole-weapons-damage-bonus: ~a ~%unlock-chance: ~a ~%deactivate-trap-chance: ~a ~%reply-attack-chance: ~a ~%ambush-attack-chance: ~a ~%spell-chance: ~a ~%attack-spell-chance: ~a ~%status: ~a  ~%race: ~a ~%level: ~a ~%exp-points: ~a"
	    (strength object)
	    (stamina object)
	    (dexterity object)
	    (agility object)
	    (smartness object)
	    (empaty object)
	    (weight object)
	    (damage-point object)
	    (movement-point object)
	    (magic-point object)
	    (dodge-chance object)
	    (melee-attack-chance object)
	    (range-attack-chance object)
	    (melee-attack-damage object)
	    (range-attack-damage object)
	    (edge-weapons-chance-bonus object)
	    (edge-weapons-damage-bonus object)
	    (impact-weapons-chance-bonus object)
	    (impact-weapons-damage-bonus object)
	    (pole-weapons-chance-bonus object)
	    (pole-weapons-damage-bonus object)
	    (unlock-chance object)
	    (deactivate-trap-chance object)
	    (reply-attack-chance object)
	    (ambush-attack-chance object)
	    (spell-chance object)
	    (attack-spell-chance object)
	    (status object) 
	    (race object)
	    (level object)
	    (exp-points object))))

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
	     damage-point
	     movement-point
	     magic-point
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
    res))

(defgeneric random-fill-slots (object capital characteristics))

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
	 (setf (damage-point ,char)   (truncate (* (stamina ,char) 0.5))
	       (movement-point ,char) (truncate (* (agility ,char) 0.5))
	       (magic-point ,char) (truncate (/ (alexandria:lerp 0.1
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
