;; Dawn of the era: a tactical game.
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

(in-package :widget)

(defun preprt-window-w ()
  (d* (d *window-w*) 0.75))

(defun preprt-window-h ()
  (d* (d *window-h*) 0.8))

(defun preprt-gender/class-w ()
  (d/ (d *window-w*) 7.0))

(defun preprt-label-ability-w ()
  (d* 5.0 (input-text-w *reference-sizes*)))

(defun preprt-label-ability-h ()
  (d* 1.5 (input-text-h *reference-sizes*)))

(defun preprt-label-ability-x ()
  0.0)

(defun preprt-label-ability-y (row)
  (d+ (d* 1.8 +portrait-size+)
      (d* (d row)
	  (input-text-h *reference-sizes*))))

(defun preprt-characteristics-x ()
  (d+ (d* 2.0 (spacing *reference-sizes*))
      +portrait-size+
      (input-text-w *reference-sizes*)
      (preprt-gender/class-w)))

(defun preprt-characteristics-y (row)
  (d* (d row)
      (d+ (spacing *reference-sizes*)
	  (input-text-h *reference-sizes*))))

(defun preprt-portrait-y ()
  (d+
   (d* 2.0 (h1-font-size *reference-sizes*))
   (spacing *reference-sizes*)))

(defun preprt-portrait-x ()
  0.0)

(defclass player-report (window)
  ((player
    :initform  nil
    :initarg  :player
    :accessor player)
   (lb-class
    :initform (make-instance 'simple-label-prefixed
			     :prefix     (_ "Class: ")
			     :label      ""
			     :font-size (h1-font-size *reference-sizes*)
			     :width     (preprt-gender/class-w)
			     :x         0.0
			     :y         0.0)
    :initarg  :lb-class
    :accessor lb-class)
   (lb-gender
    :initform (make-instance 'simple-label-prefixed
			     :prefix     (_ "Gender: ")
			     :label      ""
			     :font-size (h1-font-size *reference-sizes*)
			     :width     (preprt-gender/class-w)
			     :x         0.0
			     :y         (h1-font-size *reference-sizes*))
    :initarg  :lb-gender
    :accessor lb-gender)
   (img-portrait
    :initform (make-instance 'signalling-light
			     :width  +portrait-size+
			     :height +portrait-size+
			     :x (preprt-portrait-x)
			     :y (preprt-portrait-y)
			     :texture-name +portrait-unknown-texture-name+
			     :button-status t)
    :initarg :img-portrait
    :accessor img-portrait)
   (lb-damage-pt
    :initform (make-instance 'simple-label-prefixed
			     :width  (preprt-label-ability-w)
			     :height (preprt-label-ability-h)
			     :x (preprt-label-ability-x)
			     :y (preprt-label-ability-y 0.0)
			     :prefix (_ "Damage points: ")
			     :label "")
    :initarg :lb-damage-pt
    :accessor lb-damage-pt)
   (lb-movement-pt
    :initform (make-instance 'simple-label-prefixed
			     :width  (preprt-label-ability-w)
			     :height (preprt-label-ability-h)
			     :x (preprt-label-ability-x)
			     :y (preprt-label-ability-y 1.0)
			     :prefix (_ "Movement points: ")
			     :label "")
    :initarg :lb-movement-pt
    :accessor lb-movement-pt)
   (lb-magic-pt
    :initform (make-instance 'simple-label-prefixed
			     :width  (preprt-label-ability-w)
			     :height (preprt-label-ability-h)
			     :x (preprt-label-ability-x)
			     :y (preprt-label-ability-y 2.0)
			     :prefix (_ "Magic points: ")
			     :label "")
    :initarg :lb-magic-pt
    :accessor lb-magic-pt)
   (lb-dodge-ch
    :initform (make-instance 'simple-label-prefixed
			     :width  (preprt-label-ability-w)
			     :height (preprt-label-ability-h)
			     :x (preprt-label-ability-x)
			     :y (preprt-label-ability-y 3.0)
			     :prefix (_ "Dodge chance: ")
			     :label "")
    :initarg :lb-dodge-ch
    :accessor lb-dodge-ch)
   (lb-melee-atk-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 4.0)
			      :prefix (_ "Short range attack chance: ")
			      :label "")
     :initarg  :lb-melee-atk-ch
     :accessor lb-melee-atk-ch)
   (lb-range-atk-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 5.0)
			      :prefix (_ "Long range attack chance: ")
			      :label "")
     :initarg  :lb-range-atk-ch
     :accessor lb-range-atk-ch)
   (lb-melee-atk-dmg
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 6.0)
			      :prefix (_ "Short Range attack damage: ")
			      :label "")
     :initarg  :lb-melee-atk-dmg
     :accessor lb-melee-atk-dmg)
   (lb-range-atk-dmg
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 7.0)
			      :prefix (_ "Long Range attack damage: ")
			      :label "")
     :initarg  :lb-range-atk-dmg
     :accessor lb-range-atk-dmg)
   (lb-edge-wpn-ch-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 8.0)
			      :prefix (_ "Edge weapon chance bonus: ")
			      :label "")
     :initarg  :lb-edge-wpn-ch-bonus
     :accessor lb-edge-wpn-ch-bonus)
   (lb-edge-wpn-dmg-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 9.0)
			      :prefix (_ "Edge weapon damage bonus: ")
			      :label "")
     :initarg  :lb-edge-wpn-dmg-bonus
     :accessor lb-edge-wpn-dmg-bonus)
   (lb-impact-wpn-ch-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 10.0)
			      :prefix (_ "Impact weapon chance bonus: ")
			      :label "")
     :initarg  :lb-impact-wpn-ch-bonus
     :accessor lb-impact-wpn-ch-bonus)
   (lb-impact-wpn-dmg-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 11.0)
			      :prefix (_ "Impact weapon damage bonus: ")
			      :label "")
     :initarg  :lb-impact-wpn-dmg-bonus
     :accessor lb-impact-wpn-dmg-bonus)
   (lb-pole-wpn-ch-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 12.0)
			      :prefix (_ "Pole weapon chance bonus: ")
			      :label "")
     :initarg  :lb-pole-wpn-ch-bonus
     :accessor lb-pole-wpn-ch-bonus)
   (lb-pole-wpn-dmg-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 13.0)
			      :prefix (_ "Pole weapon damage bonus: ")
			      :label "")
     :initarg  :lb-pole-wpn-dmg-bonus
     :accessor lb-pole-wpn-dmg-bonus)
   (lb-unlock-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 14.0)
			      :prefix (_ "Unlock chance: ")
			      :label "")
     :initarg  :lb-unlock-ch
     :accessor lb-unlock-ch)
   (lb-deactivate-trap-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 15.0)
			      :prefix (_ "Deactivate trap chance: ")
			      :label "")
     :initarg  :lb-deactivate-trap-ch
     :accessor lb-deactivate-trap-ch)
   (lb-reply-attack-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 16.0)
			      :prefix (_ "Reply to attack chance: ")
			      :label "")
     :initarg  :lb-reply-attack-ch
     :accessor lb-reply-attack-ch)
   (lb-ambush-attack-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 17.0)
			      :prefix (_ "Ambush attack chance: ")
			      :label "")
     :initarg  :lb-ambush-attack-ch
     :accessor lb-ambush-attack-ch)
   (lb-spell-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 18.0)
			      :prefix (_ "Spell chance: ")
			      :label "")
     :initarg  :lb-spell-ch
     :accessor lb-spell-ch)
   (lb-attack-spell-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 19.0)
			      :prefix (_ "Attack spell chance: ")
			      :label "")
     :initarg  :lb-attack-spell-ch
     :accessor lb-attack-spell-ch)
   (lb-level
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 20.0)
			      :prefix (_ "Level: ")
			      :label "")
     :initarg  :lb-level
     :accessor lb-level)
   (lb-exp-points
     :initform (make-instance 'simple-label-prefixed
			      :width  (preprt-label-ability-w)
			      :height (preprt-label-ability-h)
			      :x (preprt-label-ability-x)
			      :y (preprt-label-ability-y 21.0)
			      :prefix (_ "Experience points: ")
			      :label "")
     :initarg  :lb-exp-points
     :accessor lb-exp-points)
   (lb-name
    :initform (make-instance 'simple-label
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (d+ (d* 2.0 (spacing *reference-sizes*))
				    +portrait-size+
				    (preprt-gender/class-w))
			     :y 0.0
			     :label-font-color §cdffffeff
			     :label " ")
    :initarg :lb-name
    :accessor lb-name)
   (lb-strength
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (preprt-characteristics-x)
			     :y (preprt-characteristics-y 0.0)
			     :prefix (_ "STR: ")
			     :label "")
    :initarg  :lb-strength
    :accessor lb-strength)
   (lb-stamina
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (preprt-characteristics-x)
			     :y (preprt-characteristics-y 1.0)
			     :prefix (_ "ST:  ")
			     :label "")
    :initarg  :lb-stamina
    :accessor lb-stamina)
   (lb-dexterity
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (preprt-characteristics-x)
			     :y (preprt-characteristics-y 2.0)
			     :prefix (_ "DX:  ")
			     :label "")
    :initarg  :lb-dexterity
    :accessor lb-dexterity)
   (lb-agility
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (preprt-characteristics-x)
			     :y (preprt-characteristics-y 3.0)
			     :prefix (_ "AG:  ")
			     :label "")
    :initarg  :lb-agility
    :accessor lb-agility)
   (lb-smartness
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (preprt-characteristics-x)
			     :y (preprt-characteristics-y 4.0)
			     :prefix (_ "SM:  ")
			     :label "")
    :initarg  :lb-smartness
    :accessor lb-smartness)
   (lb-empaty
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (preprt-characteristics-x)
			     :y (preprt-characteristics-y 5.0)
			     :prefix (_ "EM:  ")
			     :label "")
    :initarg  :lb-empaty
    :accessor lb-empaty)
   (lb-weight
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (preprt-characteristics-x)
			     :y (preprt-characteristics-y 6.0)
			     :prefix (_ "WG:  ")
			     :label "")
    :initarg  :lb-weight
    :accessor lb-weight)))

(defmethod initialize-instance :after ((object player-report) &key &allow-other-keys)
  (with-accessors ((player player)
		   (lb-class lb-class)
		   (lb-gender lb-gender)
		   (b-generate b-generate)
		   (img-portrait img-portrait)
		   (lb-damage-pt lb-damage-pt)
		   (lb-movement-pt lb-movement-pt)
		   (lb-magic-pt lb-magic-pt)
		   (lb-dodge-ch lb-dodge-ch)
		   (lb-melee-atk-ch lb-melee-atk-ch)
		   (lb-range-atk-ch lb-range-atk-ch)
		   (lb-melee-atk-dmg lb-melee-atk-dmg)
		   (lb-range-atk-dmg lb-range-atk-dmg)
		   (lb-edge-wpn-ch-bonus lb-edge-wpn-ch-bonus)
		   (lb-edge-wpn-dmg-bonus lb-edge-wpn-dmg-bonus)
		   (lb-impact-wpn-ch-bonus lb-impact-wpn-ch-bonus)
		   (lb-impact-wpn-dmg-bonus lb-impact-wpn-dmg-bonus)
		   (lb-pole-wpn-ch-bonus lb-pole-wpn-ch-bonus)
		   (lb-pole-wpn-dmg-bonus lb-pole-wpn-dmg-bonus)
		   (lb-unlock-ch lb-unlock-ch)
		   (lb-deactivate-trap-ch lb-deactivate-trap-ch)
		   (lb-reply-attack-ch lb-reply-attack-ch)
		   (lb-ambush-attack-ch lb-ambush-attack-ch)
		   (lb-spell-ch lb-spell-ch )
		   (lb-attack-spell-ch lb-attack-spell-ch)
		   (lb-level lb-level)
		   (lb-exp-points lb-exp-points)
		   (lb-name lb-name)
		   (lb-strength lb-strength)
		   (lb-stamina lb-stamina)
		   (lb-dexterity lb-dexterity)
		   (lb-agility lb-agility)
		   (lb-smartness lb-smartness)
		   (lb-empaty lb-empaty)
		   (lb-weight lb-weight)) object
    (add-child object lb-class)
    (add-child object lb-gender)
    ;; second column
    (add-child object img-portrait)
    (add-child object lb-damage-pt)
    (add-child object lb-movement-pt)
    (add-child object lb-magic-pt)
    (add-child object lb-dodge-ch)
    (add-child object lb-melee-atk-ch)
    (add-child object lb-range-atk-ch)
    (add-child object lb-melee-atk-dmg)
    (add-child object lb-range-atk-dmg)
    (add-child object lb-edge-wpn-ch-bonus)
    (add-child object lb-edge-wpn-dmg-bonus)
    (add-child object lb-impact-wpn-ch-bonus)
    (add-child object lb-impact-wpn-dmg-bonus)
    (add-child object lb-pole-wpn-ch-bonus)
    (add-child object lb-pole-wpn-dmg-bonus)
    (add-child object lb-unlock-ch)
    (add-child object lb-deactivate-trap-ch)
    (add-child object lb-reply-attack-ch)
    (add-child object lb-ambush-attack-ch)
    (add-child object lb-spell-ch)
    (add-child object lb-attack-spell-ch)
    (add-child object lb-level)
    (add-child object lb-exp-points)
    ;; third column
    (add-child object lb-name)
    ;; fourth column
    (add-child object lb-strength)
    (add-child object lb-stamina)
    (add-child object lb-dexterity)
    (add-child object lb-agility)
    (add-child object lb-smartness)
    (add-child object lb-empaty)
    (add-child object lb-weight)
    (fill-data-character object)))

(defgeneric fill-data-character (object))

(defmacro gen-format-label-with-max (max-fn-symbol character)
  (let ((fn-current (alexandria:format-symbol :character "~:@(current-~a~)" max-fn-symbol))
	(fn-max     (alexandria:format-symbol :character "~:@(actual-~a~)"         max-fn-symbol)))
    `(format nil
	     (strcat "~,1f / ~d")
	     (,fn-current ,character)
	     (,fn-max ,character))))

(defmethod fill-data-character ((object player-report))
  (with-accessors ((input-name input-name)
		   (input-last-name input-last-name)
		   (lb-class lb-class)
		   (lb-gender lb-gender)
		   (lb-strength lb-strength)
		   (lb-stamina lb-stamina)
		   (lb-dexterity lb-dexterity)
		   (lb-agility lb-agility)
		   (lb-smartness lb-smartness)
		   (lb-empaty lb-empaty)
		   (lb-weight lb-weight)
		   (lb-name lb-name)
		   (img-portrait img-portrait)
		   (lb-damage-pt lb-damage-pt)
		   (lb-movement-pt lb-movement-pt)
		   (lb-magic-pt lb-magic-pt)
		   (lb-dodge-ch lb-dodge-ch)
		   (lb-melee-atk-ch lb-melee-atk-ch)
		   (lb-range-atk-ch lb-range-atk-ch)
		   (lb-melee-atk-dmg lb-melee-atk-dmg)
		   (lb-range-atk-dmg lb-range-atk-dmg)
		   (lb-edge-wpn-ch-bonus lb-edge-wpn-ch-bonus)
		   (lb-edge-wpn-dmg-bonus lb-edge-wpn-dmg-bonus)
		   (lb-impact-wpn-ch-bonus lb-impact-wpn-ch-bonus)
		   (lb-impact-wpn-dmg-bonus lb-impact-wpn-dmg-bonus)
		   (lb-pole-wpn-ch-bonus lb-pole-wpn-ch-bonus)
		   (lb-pole-wpn-dmg-bonus lb-pole-wpn-dmg-bonus)
		   (lb-unlock-ch lb-unlock-ch)
		   (lb-deactivate-trap-ch lb-deactivate-trap-ch)
		   (lb-reply-attack-ch lb-reply-attack-ch)
		   (lb-ambush-attack-ch lb-ambush-attack-ch)
		   (lb-spell-ch lb-spell-ch )
		   (lb-attack-spell-ch lb-attack-spell-ch)
		   (lb-level lb-level)
		   (lb-exp-points lb-exp-points)
		   (player player)) object
    (setf (label lb-name)
	  (format nil "~a ~a"
		  (character:first-name player)
		  (character:last-name  player)))
    (setf (label lb-gender) (character:player-gender->gender-description player))
    (setf (label lb-class)  (character:player-class->class-description player))
    (setf (texture-object img-portrait) (character:portrait player))
    (setf (character:exp-points player) (character:exp-points player))
    (setf (label lb-strength)  (format nil +standard-float-print-format+
				       (character:strength player)))
    (setf (label lb-stamina)   (format nil +standard-float-print-format+
				       (character:stamina player)))
    (setf (label lb-dexterity) (format nil +standard-float-print-format+
				       (character:dexterity player)))
    (setf (label lb-agility)   (format nil +standard-float-print-format+
				       (character:agility player)))
    (setf (label lb-smartness) (format nil +standard-float-print-format+
				       (character:smartness player)))
    (setf (label lb-empaty)    (format nil +standard-float-print-format+
				       (character:empaty player)))
    (setf (label lb-weight)    (format nil +standard-float-print-format+
				       (character:weight player)))
    (let ((max-length-prefix (%find-max-lenght-ability-prefix object)))
      (setf (prefix lb-damage-pt) (right-padding (prefix lb-damage-pt) max-length-prefix)
	    (label lb-damage-pt)  (gen-format-label-with-max damage-points player))
      (setf (prefix lb-movement-pt) (right-padding (prefix lb-movement-pt) max-length-prefix)
	    (label  lb-movement-pt) (gen-format-label-with-max movement-points player))
      (setf (prefix lb-magic-pt) (right-padding (prefix lb-magic-pt) max-length-prefix)
	    (label  lb-magic-pt) (gen-format-label-with-max magic-points player))
      (setf (prefix lb-dodge-ch) (right-padding (prefix lb-dodge-ch) max-length-prefix)
	    (label  lb-dodge-ch) (format nil
					 +standard-float-print-format+
					 (character:actual-dodge-chance player)))
      (setf (prefix lb-melee-atk-ch) (right-padding (prefix lb-melee-atk-ch) max-length-prefix)
	    (label  lb-melee-atk-ch) (format nil
					     +standard-float-print-format+
					     (character:actual-melee-attack-chance player)))
      (setf (prefix lb-range-atk-ch) (right-padding (prefix lb-range-atk-ch) max-length-prefix)
	    (label  lb-range-atk-ch) (format nil
					     +standard-float-print-format+
					     (character:actual-range-attack-chance player)))
      (setf (prefix lb-melee-atk-dmg) (right-padding (prefix lb-melee-atk-dmg) max-length-prefix)
	    (label  lb-melee-atk-dmg) (format nil
					      +standard-float-print-format+
					      (character:actual-melee-attack-damage player)))
      (setf (prefix lb-range-atk-dmg) (right-padding (prefix lb-range-atk-dmg) max-length-prefix)
	    (label  lb-range-atk-dmg) (format nil
					      +standard-float-print-format+
					      (character:actual-range-attack-damage player)))
      (setf (prefix lb-edge-wpn-ch-bonus) (right-padding (prefix lb-edge-wpn-ch-bonus)
							 max-length-prefix)
	    (label lb-edge-wpn-ch-bonus)
	    (format nil
		    +standard-float-print-format+
		    (character:actual-edge-weapons-chance-bonus player)))
      (setf (prefix lb-edge-wpn-dmg-bonus) (right-padding (prefix lb-edge-wpn-dmg-bonus)
							  max-length-prefix)
	    (label lb-edge-wpn-dmg-bonus)
	    (format nil
		    +standard-float-print-format+
		    (character:actual-edge-weapons-damage-bonus player)))
      (setf (prefix lb-impact-wpn-ch-bonus) (right-padding (prefix lb-impact-wpn-ch-bonus)
							   max-length-prefix)
	    (label lb-impact-wpn-ch-bonus)
	    (format nil
		    +standard-float-print-format+
		    (character:actual-impact-weapons-chance-bonus player)))
      (setf (prefix lb-impact-wpn-dmg-bonus) (right-padding (prefix lb-impact-wpn-dmg-bonus)
							    max-length-prefix)
	    (label lb-impact-wpn-dmg-bonus)
	    (format nil
		    +standard-float-print-format+
		    (character:actual-impact-weapons-damage-bonus player)))
      (setf (prefix lb-pole-wpn-ch-bonus) (right-padding (prefix lb-pole-wpn-ch-bonus)
							 max-length-prefix)
	    (label lb-pole-wpn-ch-bonus)
	    (format nil
		    +standard-float-print-format+
		    (character:actual-pole-weapons-chance-bonus player)))
      (setf (prefix lb-pole-wpn-dmg-bonus) (right-padding (prefix lb-pole-wpn-dmg-bonus)
							  max-length-prefix)
	    (label lb-pole-wpn-dmg-bonus)
	    (format nil
		    +standard-float-print-format+
		    (character:actual-pole-weapons-damage-bonus player)))
      (setf (prefix lb-unlock-ch) (right-padding (prefix lb-unlock-ch) max-length-prefix)
	    (label lb-unlock-ch) (format nil
					 +standard-float-print-format+
					 (character:actual-unlock-chance player)))
      (setf (prefix lb-deactivate-trap-ch) (right-padding (prefix lb-deactivate-trap-ch)
							  max-length-prefix)
	    (label lb-deactivate-trap-ch) (format nil
						  +standard-float-print-format+
						  (character:actual-deactivate-trap-chance player)))
      (setf (prefix lb-reply-attack-ch) (right-padding (prefix lb-reply-attack-ch)
						       max-length-prefix)
	    (label lb-reply-attack-ch) (format nil
					       +standard-float-print-format+
					       (character:actual-reply-attack-chance player)))
      (setf (prefix lb-ambush-attack-ch) (right-padding (prefix lb-ambush-attack-ch)
							max-length-prefix)
	    (label lb-ambush-attack-ch) (format nil
						+standard-float-print-format+
						(character:actual-ambush-attack-chance player)))
      (setf (prefix lb-spell-ch) (right-padding (prefix lb-spell-ch) max-length-prefix)
	    (label lb-spell-ch) (format nil
					+standard-float-print-format+
					(character:actual-spell-chance player)))
      (setf (prefix lb-attack-spell-ch) (right-padding (prefix lb-attack-spell-ch)
						       max-length-prefix)
	    (label lb-attack-spell-ch) (format nil
					       +standard-float-print-format+
					       (character:actual-attack-spell-chance player)))
      (setf (prefix lb-level) (right-padding (prefix lb-level) max-length-prefix)
	    (label lb-level) (format nil "~d" (character:level player)))
      (setf (prefix lb-exp-points) (right-padding (prefix lb-exp-points) max-length-prefix)
	    (label lb-exp-points)  (format nil "~d" (character:exp-points player))))))

(defun make-player-report-win (player)
  (make-instance 'player-report
		 :x 0.0
		 :y 100.0
		 :player player
		 :width  (preprt-window-w)
		 :height (preprt-window-h)
		 :label  (_ "Player status")))
