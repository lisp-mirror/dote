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

(in-package :random-armor)

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

(define-constant +magic-fx-sigma+       #(2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0)
  :test #'equalp)

(define-constant +magic-fx-mean+        #(22.0 21.0 20.0 19.0 18.0 17.0 16.0 15.0 14.0 13.0)
  :test #'equalp)

(define-constant +minimum-magic-fx-level+                1.0        :test #'=)

(define-constant +minimum-duration-healing-fx+           2.0        :test #'=)

(define-constant +minimum-chance-healing-fx+             0.05       :test #'=)

(define-constant +minimum-decay+                        20.0        :test #'=)

(define-constant +minimum-level+                         1          :test #'=)

(define-constant +maximum-level+                         9          :test #'=)

(define-constant +minimum-chance-effects+                2.0        :test #'=)

(define-constant +maximum-chance-effects+                4.0        :test #'=)

(define-constant +minimum-num-healing-effects+           2.0        :test #'=)

(define-constant +maximum-num-healing-effects+           4.0        :test #'=)

(define-constant +minimum-chance-magic-effects+          0.0        :test #'=)

(define-constant +maximum-chance-magic-effects+          2.0        :test #'=)

(define-constant +type-name+                             "armor"    :test #'string=)

(define-constant +healing-target-self-chance+            3          :test #'=)

(define-constant +minimum-damage-point+                   1.0        :test #'=)

(define-constant +maximum-damage-point+                 100.0        :test #'=)

(defun randomize-damage-points (character level)
  (setf (damage-points character)
	(calculate-randomized-damage-points level
					    +minimum-level+
					    +maximum-level+
					    +minimum-damage-point+
					    +maximum-damage-point+
					    (d/ (d level) (d* 5.0 (d +maximum-level+))))))

(defun decay-params (armor-level)
  (values (elt +decay-sigma+ armor-level)
	  (elt +decay-mean+  armor-level)))

(defun calculate-decay-points (armor-level)
  (multiple-value-bind (sigma mean)
      (decay-params (1- armor-level))
    (truncate (max +minimum-decay+ (gaussian-probability sigma mean)))))

(defun calculate-decay (object-level decay-points)
  (make-instance 'decay-parameters
		 :leaving-message (format nil
					  (_ " (object level ~a).") object-level)
		 :points decay-points
		 :when-decay +decay-by-turns+))

(defun level-params (map-level)
  (values (elt +level-sigma+ map-level)
	  (elt +level-mean+  map-level)))

(defun modifier-params (armor-level)
  (values (elt +modifier-sigma+ armor-level)
	  (elt +modifier-mean+  armor-level)))

(defun calculate-modifier (armor-level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- armor-level))
    (d- (gaussian-probability sigma mean)
	(gaussian-probability (d/ sigma 4.0) (- armor-level)))))

(defun calculate-level (map-level)
  (multiple-value-bind (sigma mean)
      (level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-level+ +maximum-level+)))

(defun number-of-effects (armor-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- armor-level)))
			       +minimum-chance-effects+
			       +maximum-chance-effects+))))
    (lcg-next-upto max)))

(defun number-of-healing-effects (armor-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- armor-level)))
				  +minimum-num-healing-effects+
				  +maximum-num-healing-effects+)
		       number-of-normal-effects))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun generate-armor (map-level)
  (clean-effects
   (%generate-armor (res:get-resource-file +default-interaction-filename+
					   +default-character-armor-dir+
					   :if-does-not-exists :error)
		    (res:get-resource-file +default-character-filename+
					   +default-character-armor-dir+
					   :if-does-not-exists :error)
		    map-level)))

(defun %generate-armor (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((armor-level       (calculate-level map-level))
	     (armor-decay       (calculate-decay-points armor-level))
	     (effects-no         (number-of-effects armor-level))
	     (healing-effects-no (number-of-healing-effects armor-level effects-no))
	     (effects         (%get-normal-fx-shuffled  template effects-no))
	     (healing-effects (%get-healing-fx-shuffled template healing-effects-no))
	     (sum-effects (sum-effects-mod template (list +effects+))))
	(n-setf-path-value char-template (list +description+) +type-name+)
	(n-setf-path-value template
			   (list +decay+)
			   (calculate-decay armor-level armor-decay))
	(loop for i in effects do
	     (set-effect (list +effects+ i) armor-level template))
	(loop for i in healing-effects do
	     (cond
	       ((eq i +heal-damage-points+)
		nil)
	       ((eq i +cause-poison+)
		(set-poison-effect (list +healing-effects+ i) armor-level template))
	       (t
		(set-healing-effect (list +healing-effects+ i) armor-level template))))
	(when (or (plist-path-value template (list +magic-effects+))
		  (and (> armor-level  5)
		       (< sum-effects -10)))
	  (set-magic-effect armor-level template))
	(setf template (remove-generate-symbols template))
	(fill-character-plist char-template template armor-level)
	(let ((armor-character (params->np-character char-template)))
	  (setf (basic-interaction-params armor-character) template)
	  (randomize-damage-points armor-character armor-level)
	  armor-character)))))

(defun set-effect (effect-path armor-level interaction)
  (let ((effect-object (make-instance 'effect-parameters
				      :modifier (calculate-modifier armor-level)
				      :trigger  +effect-when-worn+
				       ;; effect lasting forever  for
				       ;; armors,  they   will  broke
				       ;; anyway.
				      :duration +duration-unlimited+)))
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

(defun set-healing-effect (effect-path armor-level interaction)
  (let* ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-when-worn+
				      :duration (ceiling (max 1
							      (- +maximum-level+
								 armor-level)))
				      :chance   (calculate-healing-fx-params-chance armor-level)
				      :target   +target-self+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun set-poison-effect (effect-path armor-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :points-per-turn (calculate-modifier armor-level))))
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

(defun set-magic-effect (armor-level interaction)
  (let* ((spell-level (calculate-magic-fx-level armor-level))
	 (spell-id    (random-spell-by-level spell-level))
	 (effect-object (make-instance 'magic-effect-parameters
				       :spell-id spell-id
				       :trigger  +effect-when-worn+)))
    (n-setf-path-value interaction (list +magic-effects+) effect-object)))

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

(defun regexp-file-portrait (interaction armor-name-type armor-level)
  (strcat armor-name-type
	  +file-record-sep+
	  (filename-effects-string interaction)
	  +file-record-sep+
	  (format nil "~2,'0d" armor-level)))

(defun build-file-names-db (armor-name-type armor-level)
  (strcat armor-name-type
	  +file-record-sep+
	  (format nil "~2,'0d" armor-level)
	  ".lisp"))

(defun fill-character-plist (character interaction armor-level)
  (let* ((regex          (regexp-file-portrait interaction +type-name+ armor-level))
	 (names-filename (build-file-names-db +type-name+ armor-level))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (random-names:load-db* +armors-names-resource+ names-filename)
    (n-setf-path-value character (list +first-name+) (random-names:generate))
    (n-setf-path-value character (list +last-name+)  "")))
