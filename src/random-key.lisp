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

(in-package :random-key)

(define-constant +file-record-sep+                       "-"   :test #'string=)

(define-constant +type-name+                             "key" :test #'string=)

(define-constant +minimum-level+                         1     :test #'=)

(define-constant +maximum-level+                         9     :test #'=)

(define-constant +minimum-modifier+                      1.0   :test #'=)

(define-constant +minimum-duration-healing-fx+           2.0   :test #'=)

(define-constant +minimum-chance-healing-fx+             0.05  :test #'=)

(define-constant +re-healing+                 "p-heal"         :test #'string=)

(define-constant +berserk+                    "berserk"        :test #'string=)

(define-constant +faint+                      "faint"          :test #'string=)

(define-constant +terror+                     "terror"         :test #'string=)

(define-constant +poison+                     "poison"         :test #'string=)

(define-constant +level-sigma+         #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +level-mean+          #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.8 4.8 6.0)
  :test #'equalp)

(define-constant +modifier-sigma+      #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(define-constant +modifier-mean+       #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +duration-healing-fx-sigma+  #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(define-constant +duration-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+ #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +minimum-num-healing-effects+            2.0          :test #'=)

(define-constant +maximum-num-healing-effects+            4.0          :test #'=)

(define-constant +minimum-damage-point+                   1.0          :test #'=)

(define-constant +maximum-damage-point+                  20.0          :test #'=)

(defun randomize-damage-points (character level)
  (setf (damage-points character)
	(calculate-randomized-damage-points level
					     +minimum-level+
					     +maximum-level+
					     +minimum-damage-point+
					     +maximum-damage-point+
					     (d/ (d level) (d* 5.0 (d +maximum-level+))))))


(defun level-params (map-level)
  (values (elt +level-sigma+ map-level)
	  (elt +level-mean+  map-level)))

(defun calculate-level (map-level)
  (multiple-value-bind (sigma mean)
      (level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-level+ +maximum-level+)))

(defun modifier-params (key-level)
  (values (elt +modifier-sigma+ key-level)
	  (elt +modifier-mean+  key-level)))

(defun calculate-modifier (key-level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- key-level))
    (d- (gaussian-probability sigma mean)
	(gaussian-probability (d/ sigma 4.0) (- key-level)))))

(defun healing-fx-params-duration (key-level)
  (values (elt +duration-healing-fx-sigma+ key-level)
	  (elt +duration-healing-fx-mean+  key-level)))

(defun calculate-healing-fx-params-duration (key-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-duration (1- key-level))
    (truncate (max +minimum-duration-healing-fx+ (gaussian-probability sigma mean)))))

(defun healing-fx-params-chance (armor-level)
  (values (elt +chance-healing-fx-sigma+ armor-level)
	  (elt +chance-healing-fx-mean+  armor-level)))

(defun calculate-healing-fx-params-chance (armor-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-chance (1- armor-level))
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun set-healing-hp-effect (path key-level interaction)
  (let ((effect-object (make-instance 'heal-damage-points-effect-parameters
				      :trigger +effect-when-used+
				      :points  (calculate-modifier key-level)
				      :chance  (calculate-healing-fx-params-chance key-level)
				      :target  +target-self+)))
    (n-setf-path-value interaction path effect-object)))

(defun set-healing-effect (effect-path key-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-when-used+
				      :duration (healing-effect-duration
						 effect-path
						 (ceiling (healing-fx-params-duration key-level)))
				      :chance   (calculate-healing-fx-params-chance key-level)
				      :target  +target-self+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun number-of-healing-effects (key-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- key-level)))
				  +minimum-num-healing-effects+
				  +maximum-num-healing-effects+)
		       number-of-normal-effects))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun generate-key (map-level keycode)
  (generate-key* (res:get-resource-file +default-interaction-filename+
					+default-character-key-dir+
					:if-does-not-exists :error)
		 (res:get-resource-file +default-character-filename+
					+default-character-key-dir+
					:if-does-not-exists :error)
		 map-level
		 keycode))

(defun generate-key* (interaction-file character-file map-level keycode)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((key-level          (calculate-level map-level))
	     (healing-effects-no (number-of-healing-effects key-level 0))
	     (healing-effects    (%get-healing-fx-shuffled template healing-effects-no)))
	(loop for i in healing-effects do
	     (cond
	       ((eq i +heal-damage-points+)
		(set-healing-hp-effect (list +healing-effects+ i) key-level template))
	       (t
		(set-healing-effect (list +healing-effects+ i) key-level template))))
	(n-setf-path-value template (list +can-open+) keycode)
	(setf template (remove-generate-symbols template))
	(fill-character-plist char-template)
	(let ((key-character (params->np-character char-template)))
	  (setf (basic-interaction-params key-character) template)
	  (randomize-damage-points key-character key-level)
	  key-character)))))

(defun regexp-file-portrait ()
  (strcat +type-name+ +file-record-sep+))

(defun fill-character-plist (character)
  (let* ((regex          (regexp-file-portrait))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (n-setf-path-value character (list +last-name+) "")
    (n-setf-path-value character (list +first-name+) "")))
