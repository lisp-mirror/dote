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

(define-constant +shield-file-record-sep+ "-"  :test #'string=)

(define-constant +shield-level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +shield-level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.3 3.4 3.6)
  :test #'equalp)

(define-constant +decay-shield-sigma+    #(56.0 48.0 40.0 36.0 36.0 24.0 22.0 20.0 30.0 35.0)
  :test #'equalp)

(define-constant +decay-shield-mean+     #(30.0 28.0 26.0 24.0 12.0 10.0 9.0 8.0 4.0 0.0)
  :test #'equalp)

(define-constant +shield-modifier-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +shield-modifier-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
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

(define-constant +minimum-decay-shield+                 20.0        :test #'=)

(define-constant +minimum-shield-level+                  1          :test #'=)

(define-constant +maximum-shield-level+                 10          :test #'=)

(define-constant +minimum-chance-shield-effects+         2.0        :test #'=)

(define-constant +maximum-chance-shield-effects+         4.0        :test #'=)

(define-constant +minimum-chance-shield-healing-effects+ 2.0        :test #'=)

(define-constant +maximum-chance-shield-healing-effects+ 4.0        :test #'=)

(define-constant +minimum-chance-shield-magic-effects+   0.0        :test #'=)

(define-constant +maximum-chance-shield-magic-effects+   2.0        :test #'=)

(define-constant +shield-type-name+                      "shield"   :test #'string=)

(define-constant +shield-healing-target-self-chance+     3          :test #'=)

(defun shield-decay-params (shield-level)
  (values (elt +decay-shield-sigma+ shield-level)
	  (elt +decay-shield-mean+  shield-level)))

(defun calculate-shield-decay-points (shield-level)
  (multiple-value-bind (sigma mean)
      (shield-decay-params (1- shield-level))
    (truncate (max +minimum-decay-shield+ (gaussian-probability sigma mean)))))

(defun calculate-shield-decay (object-level character decay-points)
  (make-instance 'decay-parameters
		 :leaving-message (format nil
					  (_ "~a broken")
					  (plist-path-value character (list +description+)))
		 :points decay-points
		 :when-decay (if (and (> object-level (/ +maximum-shield-level+ 2))
				      (= (lcg-next-upto 10) 0))
				 +decay-by-turns+ +decay-by-use+)))

(defun shield-level-params (map-level)
  (values (elt +shield-level-sigma+ map-level)
	  (elt +shield-level-mean+  map-level)))

(defun shield-modifier-params (shield-level)
  (values (elt +shield-modifier-sigma+ shield-level)
	  (elt +shield-modifier-mean+  shield-level)))

(defun calculate-shield-modifier (shield-level)
  (multiple-value-bind (sigma mean)
      (shield-modifier-params (1- shield-level))
    (gaussian-probability sigma mean)))

(defun calculate-shield-level (map-level)
  (multiple-value-bind (sigma mean)
      (shield-level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-shield-level+ +maximum-shield-level+)))

(defun shield-number-of-effects (shield-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- shield-level)))
			       +minimum-chance-shield-effects+
			       +maximum-chance-shield-effects+))))
    (lcg-next-upto max)))

(defun number-of-healing-effects (shield-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- shield-level)))
				  +minimum-chance-shield-healing-effects+
				  +maximum-chance-shield-healing-effects+)
		       number-of-normal-effects))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun number-of-magic-effects (shield-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   5.0
							   (d (1- shield-level)))
			       +minimum-chance-shield-magic-effects+
			       +maximum-chance-shield-magic-effects+))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun generate-shield (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((shield-level       (calculate-shield-level map-level))
	     (shield-decay       (calculate-shield-decay-points shield-level))
	     (effects-no         (shield-number-of-effects shield-level))
	     (healing-effects-no (number-of-healing-effects shield-level effects-no))
	     (effects         (%get-normal-fx-shuffled  template effects-no))
	     (healing-effects (%get-healing-fx-shuffled template healing-effects-no))
	     (sum-effects (sum-effects-mod template (list +effects+))))
	(n-setf-path-value char-template (list +description+) +shield-type-name+)
	(n-setf-path-value template
			   (list +decay+)
			   (calculate-shield-decay shield-level char-template shield-decay))
	(loop for i in effects do
	     (shield-set-effect (list +effects+ i) shield-level template))
	(loop for i in healing-effects do
	     (cond
	       ((eq i +heal-damage-points+)
		nil)
	       ((eq i +cause-poison+)
		(shield-set-poison-effect (list +healing-effects+ i) shield-level template))
	       (t
		(shield-set-healing-effect (list +healing-effects+ i) shield-level template))))
	(when (or (plist-path-value template (list +magic-effects+))
		  (and (> shield-level  5)
		       (< sum-effects -10)))
	  (shield-set-magic-effect shield-level template))
	(setf template (remove-generate-symbols template))
	(shield-fill-character-plist char-template template shield-level)
	(let ((shield-character (params->character char-template)))
	  (setf (basic-interaction-params shield-character) template)
	  shield-character)))))

(defun shield-set-effect (effect-path shield-level interaction)
  (let ((effect-object (make-instance 'effect-parameters
				      :modifier (calculate-shield-modifier shield-level)
				      :trigger  +effect-until-held+
				       ;; effect lasting forever  for
				       ;; shields,  they   will  broke
				       ;; anyway.
				      :duration  :unlimited)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun healing-fx-params-duration (shield-level)
  (values (elt +duration-healing-fx-sigma+ shield-level)
	  (elt +duration-healing-fx-mean+  shield-level)))

(defun calculate-healing-fx-params-duration (shield-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-duration (1- shield-level))
    (truncate (max +minimum-duration-healing-fx+ (gaussian-probability sigma mean)))))

(defun healing-fx-params-chance (shield-level)
  (values (elt +chance-healing-fx-sigma+ shield-level)
	  (elt +chance-healing-fx-mean+  shield-level)))

(defun calculate-healing-fx-params-chance (shield-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-chance (1- shield-level))
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun shield-healing-target ()
  (if (= (lcg-next-upto +shield-healing-target-self-chance+) 0)
      +target-other+
      +target-self+))

(defun shield-set-healing-effect (effect-path shield-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-until-held+
				       ;; effect lasting forever  for
				       ;; shields,  they   will  broke
				       ;; anyway.
				      :duration  :unlimited
				      :chance (calculate-healing-fx-params-chance shield-level)
				      :target (shield-healing-target))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun shield-set-poison-effect (effect-path shield-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :points-per-turn (calculate-shield-modifier shield-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun magic-fx-params (shield-level)
  (values (elt +magic-fx-sigma+ shield-level)
	  (elt +magic-fx-mean+  shield-level)))

(defun calculate-magic-fx-level (shield-level)
  (multiple-value-bind (sigma mean)
      (magic-fx-params (1- shield-level))
    (truncate (max +minimum-magic-fx-level+ (gaussian-probability sigma mean)))))

(defun random-spell-by-level (spell-level)
  (and spell-level :fireball-1)) ;; TODO

(defun shield-set-magic-effect (shield-level interaction)
  (let* ((spell-level (calculate-magic-fx-level shield-level))
	 (spell-id    (random-spell-by-level spell-level))
	 (effect-object (make-instance 'magic-effect-parameters
				       :spell-id spell-id
				       :trigger  +effect-until-held+)))
    (n-setf-path-value interaction (list +magic-effects+) effect-object)))

(defun sum-effects-mod (interactions path)
  (reduce #'+ (mapcar #'(lambda (a) (if (typep (cdr a) 'effect-parameters) (modifier (cdr a)) 0.0))
		      (plist-path-value interactions path))
	  :initial-value 0.0))

(defun shield-filename-effects-string (interaction)
  (cond
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat "heal" +shield-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +heal-berserk+))
     (strcat "heal" +shield-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +heal-faint+))
     (strcat "heal" +shield-file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +heal-terror+))
     (strcat "heal" +shield-file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat "cause" +shield-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +cause-berserk+))
     (strcat "cause" +shield-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +cause-faint+))
     (strcat "cause" +shield-file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +cause-terror+))
     (strcat "cause" +shield-file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +immune-poison+))
     (strcat "immune" +shield-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +immune-berserk+))
     (strcat "immune" +shield-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +immune-faint+))
     (strcat "immune" +shield-file-record-sep+ "faint"))
    (t
     "normal")))

(defun regexp-file-portrait (interaction shield-name-type shield-level)
  (strcat shield-name-type
	  +shield-file-record-sep+
	  (shield-filename-effects-string interaction)
	  +shield-file-record-sep+
	  (format nil "~2,'0d" shield-level)))

(defun shield-build-file-names-db (shield-name-type shield-level)
  (strcat shield-name-type
	  +shield-file-record-sep+
	  (format nil "~2,'0d" shield-level)
	  ".lisp"))

(defun shield-fill-character-plist (character interaction shield-level)
  (let* ((regex          (regexp-file-portrait interaction +shield-type-name+ shield-level))
	 (names-filename (shield-build-file-names-db +shield-type-name+ shield-level))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (random-names:load-db* +shields-names-resource+ names-filename)
    (n-setf-path-value character (list +last-name+)  (random-names:generate))
    (n-setf-path-value character (list +first-name+) +shield-type-name+)))
