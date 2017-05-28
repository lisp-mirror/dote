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

(in-package :random-weapon)

(define-constant +file-record-sep+ "-"  :test #'string=)

(define-constant +level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.8 4.8 6.0)
  :test #'equalp)

(define-constant +decay-sigma+    #(56.0 48.0 40.0 36.0 36.0 24.0 22.0 20.0 30.0 35.0)
  :test #'equalp)

(define-constant +decay-mean+     #(30.0 28.0 26.0 24.0 12.0 10.0 9.0 8.0 4.0 0.0)
  :test #'equalp)

(define-constant +modifier-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(define-constant +modifier-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +duration-healing-fx-sigma+  #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(define-constant +duration-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+  #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+   #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +magic-fx-sigma+   #(2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0)
  :test #'equalp)

(define-constant +magic-fx-mean+    #(22.0 21.0 20.0 19.0 18.0 17.0 16.0 15.0 14.0 13.0)
  :test #'equalp)

(define-constant +minimum-magic-fx-level+                1.0        :test #'=)

(define-constant +minimum-duration-healing-fx+           2.0        :test #'=)

(define-constant +minimum-chance-healing-fx+             0.05       :test #'=)

(define-constant +minimum-num-healing-fx+                2.0        :test #'=)

(define-constant +maximum-num-healing-fx+                4.0        :test #'=)

(define-constant +minimum-decay+                        20.0        :test #'=)

(define-constant +minimum-level+                         1          :test #'=)

(define-constant +maximum-level+                         9          :test #'=)

(define-constant +minimum-chance-effects+                2.0        :test #'=)

(define-constant +maximum-chance-effects+                4.0        :test #'=)

(define-constant +sword-type-name+                       "sword"    :test #'string=)

(define-constant +spear-type-name+                       "spear"    :test #'string=)

(define-constant +staff-type-name+                       "staff"    :test #'string=)

(define-constant +mace-type-name+                        "mace"     :test #'string=)

(define-constant +bow-type-name+                         "bow"      :test #'string=)

(define-constant +crossbow-type-name+                    "crossbow" :test #'string=)

(define-constant +mace-or-staff-chance+                  3          :test #'=)

(define-constant +healing-target-self-chance+            3          :test #'=)

(define-constant +minimum-damage-point+                   1.0          :test #'=)

(define-constant +maximum-damage-point+                 100.0          :test #'=)

(defun randomize-damage-points (character level)
  (setf (damage-points character)
	(calculate-randomized-damage-points level
					     +minimum-level+
					     +maximum-level+
					     +minimum-damage-point+
					     +maximum-damage-point+
					     (d/ (d level) (d* 5.0 (d +maximum-level+))))))

(defun decay-params (weapon-level)
  (values (elt +decay-sigma+ weapon-level)
	  (elt +decay-mean+  weapon-level)))

(defun calculate-decay-points (weapon-level)
  (multiple-value-bind (sigma mean)
      (decay-params (1- weapon-level))
    (d (truncate (max +minimum-decay+ (gaussian-probability sigma mean))))))

(defun calculate-decay (object-level decay-points)
  (make-instance 'decay-parameters
		 :leaving-message (format nil
					  (_ " (object level ~a).") object-level)
		 :points decay-points
		 :when-decay +decay-by-turns+))

(defun level-params (map-level)
  (values (elt +level-sigma+ map-level)
	  (elt +level-mean+  map-level)))

(defun modifier-params (weapon-level)
  (values (elt +modifier-sigma+ weapon-level)
	  (elt +modifier-mean+  weapon-level)))

(defun calculate-modifier (weapon-level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- weapon-level))
    (d- (gaussian-probability sigma mean)
	(gaussian-probability (d/ sigma 4.0) (- weapon-level)))))

(defun calculate-level (map-level)
  (multiple-value-bind (sigma mean)
      (level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-level+ +maximum-level+)))

(defun number-of-effects (weapon-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- weapon-level)))
			       +minimum-chance-effects+
			       +maximum-chance-effects+))))
    (lcg-next-upto max)))

(defun number-of-healing-effects (weapon-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- weapon-level)))
				  +minimum-num-healing-fx+
				  +maximum-num-healing-fx+)
		       number-of-normal-effects))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun generate-weapon (map-level type)
  (let ((type-dir (ecase type
		    (:bow
		     +default-character-bow-dir+)
		    (:crossbow
		     +default-character-crossbow+)
		    (:mace
		     +default-character-mace+)
		    (:spear
		     +default-character-spear+)
		    (:staff
		     +default-character-staff+)
		    (:sword
		     +default-character-sword+))))
    (clean-effects
     (%generate-weapon (res:get-resource-file +default-interaction-filename+
					      (append +default-character-weapon-dir+
						      type-dir)
					      :if-does-not-exists :error)
		       (res:get-resource-file +default-character-filename+
					      (append +default-character-weapon-dir+
						      type-dir)
					      :if-does-not-exists :error)
		       map-level))))

(defun %generate-weapon (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters-file (template interaction-file)
      (let* ((weapon-level       (calculate-level map-level))
	     (weapon-decay       (calculate-decay-points weapon-level))
	     (effects-no         (number-of-effects weapon-level))
	     (healing-effect-no  (number-of-healing-effects weapon-level effects-no)))
	(n-setf-path-value char-template (list +level+) (d weapon-level))
	(cond
	  ((plist-path-value template (list +can-cut+))
	   (n-setf-path-value char-template (list +description+) +sword-type-name+)
	   (generate-weapon-common template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (fill-sword-plists char-template template weapon-level))
	  ((plist-path-value template (list +can-smash+))
	   (generate-weapon-common template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (if (= (lcg-next-upto (truncate (+ +mace-or-staff-chance+
					      (* 0.3 weapon-level)))) 0)
	       (progn
		 (n-setf-path-value char-template (list +description+)
				    +staff-type-name+)
		 (fill-staff-plists char-template template weapon-level))
	       (progn
		 (n-setf-path-value char-template (list +description+)
				    +mace-or-staff-chance+)
		 (fill-mace-plists char-template template weapon-level))))
	  ((plist-path-value template (list +mounted-on-pole+))
	   (n-setf-path-value char-template (list +description+) +spear-type-name+)
	   (generate-weapon-common template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (fill-spear-plists char-template template weapon-level))
  	  ((plist-path-value template (list +can-launch-bolt+))
	   (n-setf-path-value char-template (list +description+) +crossbow-type-name+)
	   (generate-weapon-common template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (fill-crossbow-plists char-template template weapon-level))
	  ((plist-path-value template (list +can-launch-arrow+))
	   (n-setf-path-value char-template (list +description+) +bow-type-name+)
	   (generate-weapon-common template weapon-level weapon-decay
			           effects-no healing-effect-no)
	   (setf template (remove-generate-symbols template))
	   (fill-bow-plists char-template template weapon-level))
	  (t
	   (error (_ "Unknown weapon type"))))
	(let ((weapon-character (params->np-character char-template)))
	  (setf (basic-interaction-params weapon-character) template)
	  (randomize-damage-points weapon-character weapon-level)
	  weapon-character)))))

(defun set-effect (effect-path weapon-level interaction)
  (let ((effect-object (make-instance 'effect-parameters
				      :modifier (calculate-modifier weapon-level)
				      :trigger  +effect-when-worn+
				       ;; effect lasting forever  for
				       ;; weapons,  they   will  broke
				       ;; anyway.
				      :duration +duration-unlimited+)))
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
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun healing-target ()
  (if (= (lcg-next-upto +healing-target-self-chance+) 0)
      +target-other+
      +target-self+))

(defun set-healing-effect (effect-path weapon-level interaction)
  (let* ((target (healing-target))
	 (effect-object (make-instance 'healing-effect-parameters
				       :trigger  +effect-when-worn+
				       :duration (ceiling (max 1
							       (- +maximum-level+
								  weapon-level)))
				       :chance (calculate-healing-fx-params-chance weapon-level)
				       :target  target)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun set-poison-effect (effect-path weapon-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :chance (calculate-healing-fx-params-chance weapon-level)
				      :target          +target-other+
				      :points-per-turn (calculate-modifier
							weapon-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun magic-fx-params (weapon-level)
  (values (elt +magic-fx-sigma+ weapon-level)
	  (elt +magic-fx-mean+  weapon-level)))

(defun calculate-magic-fx-level (weapon-level)
  (multiple-value-bind (sigma mean)
      (magic-fx-params (1- weapon-level))
    (truncate (max +minimum-magic-fx-level+ (gaussian-probability sigma mean)))))

(defun set-magic-effect (weapon-level interaction)
  (let* ((spell-level   (1+ weapon-level))
	 (spells        (spell:filter-spell-db #'(lambda (a)
						   (or (not (spell:attack-spell-p a))
							(> (spell:level a)
							    spell-level)
							(< (spell:level a)
							   (max 0
								(/ spell-level 4)))))))
	 (spell-id      (spell:identifier (random-elt spells)))
	 (effect-object (make-instance 'magic-effect-parameters
				       :spell-id spell-id
				       :trigger  +effect-when-worn+)))
    (n-setf-path-value interaction (list +magic-effects+) effect-object)))

(defun generate-weapon-common (interaction weapon-level weapon-decay-points
			       effects-no healing-effects-no)
  (let* ((effects         (get-normal-fx-shuffled  interaction effects-no))
	 (healing-effects (get-healing-fx-shuffled interaction healing-effects-no)))
    (n-setf-path-value interaction
		       (list +decay+)
		       (calculate-decay weapon-level weapon-decay-points))
    (loop for i in effects do
	 (set-effect (list +effects+ i) weapon-level interaction))
    (loop for i in healing-effects do
	 (cond
	   ((eq i +heal-damage-points+)
	    nil)
	   ((eq i +cause-poison+)
	    (set-poison-effect (list +healing-effects+ i) weapon-level interaction))
	   (t
	    (set-healing-effect (list +healing-effects+ i) weapon-level interaction))))))

(defun generate-sword (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level  5)
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

(defun filename-effects-string (interaction)
  (cond
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat "heal" +file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +heal-berserk+))
     (strcat "heal" +file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +heal-faint+))
     (strcat "heal" +file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +heal-terror+))
     (strcat "heal" +file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat "cause" +file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +cause-berserk+))
     (strcat "cause" +file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +cause-faint+))
     (strcat "cause" +file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +cause-terror+))
     (strcat "cause" +file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +immune-poison+))
     (strcat "immune" +file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +immune-berserk+))
     (strcat "immune" +file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +immune-faint+))
     (strcat "immune" +file-record-sep+ "faint"))
    (t
     "normal")))

(defun regexp-file-portrait (interaction weapon-name-type weapon-level)
  (strcat weapon-name-type
	  +file-record-sep+
	  (filename-effects-string interaction)
	  +file-record-sep+
	  (format nil "~2,'0d" weapon-level)))

(defun build-file-names-db (weapon-name-type weapon-level)
  (strcat weapon-name-type
	  +file-record-sep+
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
