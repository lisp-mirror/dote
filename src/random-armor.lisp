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

(define-constant +armor-file-record-sep+ "-"  :test #'string=)

(define-constant +armor-level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +armor-level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.3 3.4 3.6)
  :test #'equalp)

(define-constant +decay-armor-sigma+    #(56.0 48.0 40.0 36.0 36.0 24.0 22.0 20.0 30.0 35.0)
  :test #'equalp)

(define-constant +decay-armor-mean+     #(30.0 28.0 26.0 24.0 12.0 10.0 9.0 8.0 4.0 0.0)
  :test #'equalp)

(define-constant +armor-modifier-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +armor-modifier-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
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

(define-constant +minimum-decay-armor+                 20.0        :test #'=)

(define-constant +minimum-armor-level+                  1          :test #'=)

(define-constant +maximum-armor-level+                 10          :test #'=)

(define-constant +minimum-chance-armor-effects+         2.0        :test #'=)

(define-constant +maximum-chance-armor-effects+         4.0        :test #'=)

(define-constant +minimum-chance-armor-healing-effects+ 2.0        :test #'=)

(define-constant +maximum-chance-armor-healing-effects+ 4.0        :test #'=)

(define-constant +minimum-chance-armor-magic-effects+   0.0        :test #'=)

(define-constant +maximum-chance-armor-magic-effects+   2.0        :test #'=)

(define-constant +armor-type-name+                      "armor"   :test #'string=)

(define-constant +armor-healing-target-self-chance+     3          :test #'=)

(defun armor-decay-params (armor-level)
  (values (elt +decay-armor-sigma+ armor-level)
	  (elt +decay-armor-mean+  armor-level)))

(defun calculate-armor-decay-points (armor-level)
  (multiple-value-bind (sigma mean)
      (armor-decay-params (1- armor-level))
    (truncate (max +minimum-decay-armor+ (gaussian-probability sigma mean)))))

(defun calculate-armor-decay (object-level character decay-points)
  (make-instance 'decay-parameters
		 :leaving-message (format nil
					  (_ "~a broken")
					  (plist-path-value character (list +description+)))
		 :points decay-points
		 :when-decay (if (and (> object-level (/ +maximum-armor-level+ 2))
				      (= (lcg-next-upto 10) 0))
				 +decay-by-turns+ +decay-by-use+)))

(defun armor-level-params (map-level)
  (values (elt +armor-level-sigma+ map-level)
	  (elt +armor-level-mean+  map-level)))

(defun armor-modifier-params (armor-level)
  (values (elt +armor-modifier-sigma+ armor-level)
	  (elt +armor-modifier-mean+  armor-level)))

(defun calculate-armor-modifier (armor-level)
  (multiple-value-bind (sigma mean)
      (armor-modifier-params (1- armor-level))
    (gaussian-probability sigma mean)))

(defun calculate-armor-level (map-level)
  (multiple-value-bind (sigma mean)
      (armor-level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-armor-level+ +maximum-armor-level+)))

(defun armor-number-of-effects (armor-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- armor-level)))
			       +minimum-chance-armor-effects+
			       +maximum-chance-armor-effects+))))
    (lcg-next-upto max)))

(defun number-of-healing-effects (armor-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- armor-level)))
				  +minimum-chance-armor-healing-effects+
				  +maximum-chance-armor-healing-effects+)
		       number-of-normal-effects))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun number-of-magic-effects (armor-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   5.0
							   (d (1- armor-level)))
			       +minimum-chance-armor-magic-effects+
			       +maximum-chance-armor-magic-effects+))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun generate-armor (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((armor-level       (calculate-armor-level map-level))
	     (armor-decay       (calculate-armor-decay-points armor-level))
	     (effects-no         (armor-number-of-effects armor-level))
	     (healing-effects-no (number-of-healing-effects armor-level effects-no))
	     (effects         (%get-normal-fx-shuffled  template effects-no))
	     (healing-effects (%get-healing-fx-shuffled template healing-effects-no))
	     (sum-effects (sum-effects-mod template (list +effects+))))
	(n-setf-path-value char-template (list +description+) +armor-type-name+)
	(n-setf-path-value template
			   (list +decay+)
			   (calculate-armor-decay armor-level char-template armor-decay))
	(loop for i in effects do
	     (armor-set-effect (list +effects+ i) armor-level template))
	(loop for i in healing-effects do
	     (cond
	       ((eq i +heal-damage-points+)
		nil)
	       ((eq i +cause-poison+)
		(armor-set-poison-effect (list +healing-effects+ i) armor-level template))
	       (t
		(armor-set-healing-effect (list +healing-effects+ i) armor-level template))))
	(when (or (plist-path-value template (list +magic-effects+))
		  (and (> armor-level  5)
		       (< sum-effects -10)))
	  (armor-set-magic-effect armor-level template))
	(setf template (remove-generate-symbols template))
	(armor-fill-character-plist char-template template armor-level)
	(let ((armor-character (params->character char-template)))
	  (setf (basic-interaction-params armor-character) template)
	  armor-character)))))

(defun armor-set-effect (effect-path armor-level interaction)
  (let ((effect-object (make-instance 'effect-parameters
				      :modifier (calculate-armor-modifier armor-level)
				      :trigger  +effect-when-worn+
				       ;; effect lasting forever  for
				       ;; armors,  they   will  broke
				       ;; anyway.
				      :duration  :unlimited)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun healing-fx-params-duration (armor-level)
  (values (elt +duration-healing-fx-sigma+ armor-level)
	  (elt +duration-healing-fx-mean+  armor-level)))

(defun calculate-healing-fx-params-duration (armor-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-duration (1- armor-level))
    (truncate (max +minimum-duration-healing-fx+ (gaussian-probability sigma mean)))))

(defun healing-fx-params-chance (armor-level)
  (values (elt +chance-healing-fx-sigma+ armor-level)
	  (elt +chance-healing-fx-mean+  armor-level)))

(defun calculate-healing-fx-params-chance (armor-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-chance (1- armor-level))
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun armor-healing-target ()
  (if (= (lcg-next-upto +armor-healing-target-self-chance+) 0)
      +target-other+
      +target-self+))

(defun armor-set-healing-effect (effect-path armor-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-until-held+
				       ;; effect lasting forever  for
				       ;; armors,  they   will  broke
				       ;; anyway.
				      :duration  :unlimited
				      :chance (calculate-healing-fx-params-chance armor-level)
				      :target (armor-healing-target))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun armor-set-poison-effect (effect-path armor-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :points-per-turn (calculate-armor-modifier armor-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun magic-fx-params (armor-level)
  (values (elt +magic-fx-sigma+ armor-level)
	  (elt +magic-fx-mean+  armor-level)))

(defun calculate-magic-fx-level (armor-level)
  (multiple-value-bind (sigma mean)
      (magic-fx-params (1- armor-level))
    (truncate (max +minimum-magic-fx-level+ (gaussian-probability sigma mean)))))

(defun random-spell-by-level (spell-level)
  (and spell-level :fireball-1)) ;; TODO

(defun armor-set-magic-effect (armor-level interaction)
  (let* ((spell-level (calculate-magic-fx-level armor-level))
	 (spell-id    (random-spell-by-level spell-level))
	 (effect-object (make-instance 'magic-effect-parameters
				       :spell-id spell-id
				       :trigger  +effect-when-worn+)))
    (n-setf-path-value interaction (list +magic-effects+) effect-object)))

(defun sum-effects-mod (interactions path)
  (reduce #'+ (mapcar #'(lambda (a) (if (typep (cdr a) 'effect-parameters) (modifier (cdr a)) 0.0))
		      (plist-path-value interactions path))
	  :initial-value 0.0))

(defun armor-filename-effects-string (interaction)
  (cond
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat "heal" +armor-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +heal-berserk+))
     (strcat "heal" +armor-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +heal-faint+))
     (strcat "heal" +armor-file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +heal-terror+))
     (strcat "heal" +armor-file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat "cause" +armor-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +cause-berserk+))
     (strcat "cause" +armor-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +cause-faint+))
     (strcat "cause" +armor-file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +cause-terror+))
     (strcat "cause" +armor-file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +immune-poison+))
     (strcat "immune" +armor-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +immune-berserk+))
     (strcat "immune" +armor-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +immune-faint+))
     (strcat "immune" +armor-file-record-sep+ "faint"))
    (t
     "normal")))

(defun regexp-file-portrait (interaction armor-name-type armor-level)
  (strcat armor-name-type
	  +armor-file-record-sep+
	  (armor-filename-effects-string interaction)
	  +armor-file-record-sep+
	  (format nil "~2,'0d" armor-level)))

(defun armor-build-file-names-db (armor-name-type armor-level)
  (strcat armor-name-type
	  +armor-file-record-sep+
	  (format nil "~2,'0d" armor-level)
	  ".lisp"))

(defun armor-fill-character-plist (character interaction armor-level)
  (let* ((regex          (regexp-file-portrait interaction +armor-type-name+ armor-level))
	 (names-filename (armor-build-file-names-db +armor-type-name+ armor-level))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (random-names:load-db* +armors-names-resource+ names-filename)
    (n-setf-path-value character (list +last-name+)   (random-names:generate))
    (n-setf-path-value character (list +first-name+)  +armor-type-name+)))
