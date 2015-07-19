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

(define-constant +weapon-file-record-sep+ "-"  :test #'string=)

(define-constant +weapon-level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +weapon-level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.3 3.4 3.6)
  :test #'equalp)

(define-constant +decay-weapon-sigma+    #(56.0 48.0 40.0 36.0 36.0 24.0 22.0 20.0 30.0 35.0)
  :test #'equalp)

(define-constant +decay-weapon-mean+     #(30.0 28.0 26.0 24.0 12.0 10.0 9.0 8.0 4.0 0.0)
  :test #'equalp)

(define-constant +weapon-modifier-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +weapon-modifier-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +duration-healing-fx-sigma+  #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +duration-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+  #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+   #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +magic-fx-sigma+           #(2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0)
  :test #'equalp)

(define-constant +magic-fx-mean+            #(22.0 21.0 20.0 19.0 18.0 17.0 16.0 15.0 14.0 13.0)
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

(define-constant +mace-or-staff-chance+                  3          :test #'=)

(define-constant +weapon-healing-target-self-chance+     3          :test #'=)

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
					  (_ "~a broken")
					  (plist-path-value character (list +description+)))
		 :points decay-points
		 :when-decay (if (and (> object-level (/ +maximum-weapon-level+ 2))
				      (= (lcg-next-upto 10) 0))
				 +decay-by-turns+ +decay-by-use+)))

(defun weapon-level-params (map-level)
  (values (elt +weapon-level-sigma+ map-level)
	  (elt +weapon-level-mean+  map-level)))

(defun weapon-modifier-params (weapon-level)
  (values (elt +weapon-modifier-sigma+ weapon-level)
	  (elt +weapon-modifier-mean+  weapon-level)))

(defun calculate-weapon-modifier (weapon-level)
  (multiple-value-bind (sigma mean)
      (weapon-modifier-params (1- weapon-level))
    (gaussian-probability sigma mean)))

(defun calculate-weapon-level (map-level)
  (multiple-value-bind (sigma mean)
      (weapon-level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-weapon-level+ +maximum-weapon-level+)))

(defun weapon-number-of-effects (weapon-level)
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
	     (effects-no         (weapon-number-of-effects weapon-level))
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
	   (if (= (lcg-next-upto (truncate (+ +mace-or-staff-chance+ (* 0.3 weapon-level)))) 0)
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
	  weapon-character)))))

(defun weapon-set-effect (effect-path weapon-level interaction)
  (let ((effect-object (make-instance 'effect-parameters
				      :modifier (calculate-weapon-modifier weapon-level)
				      :trigger  +effect-until-held+
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
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun weapon-healing-target ()
  (if (= (lcg-next-upto +weapon-healing-target-self-chance+) 0)
      +target-other+
      +target-self+))

(defun weapon-set-healing-effect (effect-path weapon-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-until-held+
				       ;; effect lasting forever  for
				       ;; weapons,  they   will  broke
				       ;; anyway.
				      :duration  :unlimited
				      :chance (calculate-healing-fx-params-chance weapon-level)
				      :target (weapon-healing-target))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun weapon-set-poison-effect (effect-path weapon-level interaction)
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

(defun weapon-set-magic-effect (weapon-level interaction)
  (let* ((spell-level (calculate-magic-fx-level weapon-level))
	 (spell-id    (random-spell-by-level spell-level))
	 (effect-object (make-instance 'magic-effect-parameters
				       :spell-id spell-id
				       :trigger  +effect-until-held+)))
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
	 (weapon-set-effect (list +effects+ i) weapon-level interaction))
    (loop for i in healing-effects do
	 (cond
	   ((eq i +heal-damage-points+)
	    nil)
	   ((eq i +cause-poison+)
	    (weapon-set-poison-effect (list +healing-effects+ i) weapon-level interaction))
	   (t
	    (weapon-set-healing-effect (list +healing-effects+ i) weapon-level interaction))))))

(defun generate-sword (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level  5)
		   (< sum-effects -10)))
      (weapon-set-magic-effect weapon-level interaction))))

(defun generate-spear (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 5)
		   (< sum-effects -10)))
      (weapon-set-magic-effect weapon-level interaction))))

(defun generate-mace (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 5)
		   (< sum-effects -10)))
      (weapon-set-magic-effect weapon-level interaction))))

(defun generate-staff (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 2)
		   (< sum-effects  0)))
      (weapon-set-magic-effect weapon-level interaction))))

(defun generate-bow (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 5)
		   (< sum-effects -10)))
      (weapon-set-magic-effect weapon-level interaction))))

(defun generate-crossbow (interaction character weapon-level)
  (declare (ignore character))
  (let ((sum-effects (sum-effects-mod interaction (list +effects+))))
    (when (or (plist-path-value interaction (list +magic-effects+))
	      (and (> weapon-level 5)
		   (< sum-effects -10)))
      (weapon-set-magic-effect weapon-level interaction))))

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
