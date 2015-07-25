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

(in-package :random-potion)

(define-constant +file-record-sep+ "-" :test #'string=)

(define-constant +type-name+              "potion"     :test #'string=)

(define-constant +minimum-level+            1          :test #'=)

(define-constant +maximum-level+           10          :test #'=)

(define-constant +minimum-modifier+         1.0        :test #'=)

(define-constant +minimum-chance-effects+   .1         :test #'=)

(define-constant +maximum-chance-effects+   1.0        :test #'=)

(define-constant +re-healing+               "p-heal"   :test #'string=)

(define-constant +berserk+                  "berserk"  :test #'string=)

(define-constant +faint+                    "faint"    :test #'string=)

(define-constant +terror+                   "terror"   :test #'string=)

(define-constant +poison+                   "poison"   :test #'string=)

(define-constant +level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.8 4.8 6.0)
  :test #'equalp)

(define-constant +modifier-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +modifier-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+  #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+   #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+ #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +minimum-duration-healing-fx+           2.0        :test #'=)

(define-constant +minimum-num-healing-fx+             2.0           :test #'=)

(define-constant +maximum-num-healing-fx+             4.0           :test #'=)

(define-constant +minimum-chance-healing-fx+          0.05          :test #'=)

(define-constant +minimum-potion-level+                  1          :test #'=)

(define-constant +maximum-potion-level+                 10          :test #'=)

(defun level-params (map-level)
  (values (elt +level-sigma+ map-level)
	  (elt +level-mean+  map-level)))

(defun modifier-params (potion-level)
  (values (elt +modifier-sigma+ potion-level)
	  (elt +modifier-mean+  potion-level)))

(defun calculate-modifier (potion-level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- potion-level))
    (abs (gaussian-probability sigma mean))))

(defun calculate-potion-level (map-level)
  (multiple-value-bind (sigma mean)
      (level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-level+ +maximum-level+)))

(defun healing-fx-params-chance (armor-level)
  (values (elt +chance-healing-fx-sigma+ armor-level)
	  (elt +chance-healing-fx-mean+  armor-level)))

(defun calculate-healing-fx-params-chance (armor-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-chance (1- armor-level))
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun set-healing-dmg-effect (path potion-level interaction)
  (let ((effect-object (make-instance 'heal-damage-points-effect-parameters
				      :trigger +effect-when-used+
				      :points  (calculate-modifier potion-level)
				      :chance  (calculate-healing-fx-params-chance
						potion-level)
				      :target  +target-self+)))
    (n-setf-path-value interaction path effect-object)))

(defun set-healing-effect (effect-path potion-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-when-used+
				      :duration  (calculate-modifier potion-level)
				      :chance (calculate-healing-fx-params-chance
					       potion-level)
				      :target  +target-self+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun set-poison-effect (effect-path potion-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :points-per-turn (calculate-modifier potion-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun number-of-healing-effects (potion-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- potion-level)))
				  +minimum-num-healing-fx+
				  +maximum-num-healing-fx+)
		       number-of-normal-effects))))
    (if (<= max 0)
     	0
     	(lcg-next-upto (max 0.0 max)))))

(defun generate-potion (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((potion-level       (calculate-potion-level map-level))
	     (healing-effects-no (number-of-healing-effects potion-level 0))
	     (healing-effects    (%get-healing-fx-shuffled template healing-effects-no)))
	(loop for i in healing-effects do
	     (cond
	       ((eq i +heal-damage-points+)
		(set-healing-dmg-effect (list +healing-effects+ i) potion-level template))
	       ((eq i +cause-poison+)
		(set-poison-effect (list +healing-effects+ i) potion-level template))
	       (t
		(set-healing-effect (list +healing-effects+ i) potion-level template))))
	(setf template (remove-generate-symbols template))
	(fill-character-plist +type-name+ char-template template potion-level)
	(let ((potion-character (params->character char-template)))
	  (setf (basic-interaction-params potion-character) template)
	  potion-character)))))

(defun filename-effects-string (interaction)
  (cond
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat +re-healing+ +file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +heal-berserk+))
     (strcat +re-healing+ +file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +heal-faint+))
     (strcat +re-healing+ +file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +heal-terror+))
     (strcat +re-healing+ +file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +heal-damage-points+))
     +re-healing+)
    ((plist-path-value interaction (list +healing-effects+ +immune-poison+))
     (strcat +re-healing+ +file-record-sep+ "immune"
	     +file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +immune-berserk+))
     (strcat +re-healing+ +file-record-sep+  "immune"
	     +file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +immune-faint+))
     (strcat +re-healing+ +file-record-sep+ "immune"
	     +file-record-sep+ "faint"))
    (t
     (strcat +re-healing+ +file-record-sep+ "immune"
	     +file-record-sep+ "faint"))))

(defun regexp-file-portrait (interaction potion-level)
  (strcat (filename-effects-string interaction)
	  +file-record-sep+
	  (format nil "~2,'0d" potion-level)))

(defun build-file-names-db (potion-name-type potion-level)
  (strcat potion-name-type
	  +file-record-sep+
	  (format nil "~2,'0d" potion-level)
	  ".lisp"))

(defun fill-character-plist (potion-name-type character interaction potion-level)
  (let* ((regex          (regexp-file-portrait interaction potion-level))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (n-setf-path-value character (list +last-name+) "")
    (n-setf-path-value character (list +first-name+) potion-name-type)))
