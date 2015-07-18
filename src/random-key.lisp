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

(define-constant +key-file-record-sep+        "-"       :test #'string=)

(define-constant +key-type-name+              "key"     :test #'string=)

(define-constant +minimum-key-level+          1         :test #'=)

(define-constant +maximum-key-level+          10        :test #'=)

(define-constant +minimum-key-modifier+       1.0       :test #'=)

(define-constant +minimum-chance-key-effects+ 0.1       :test #'=)

(define-constant +maximum-chance-key-effects+ 1.0       :test #'=)

(define-constant +re-healing+                 "p-heal"  :test #'string=)

(define-constant +berserk+                    "berserk" :test #'string=)

(define-constant +faint+                      "faint"   :test #'string=)

(define-constant +terror+                     "terror"  :test #'string=)

(define-constant +poison+                     "poison"  :test #'string=)

(define-constant +key-level-sigma+         #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +key-level-mean+          #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.3 3.4 3.6)
  :test #'equalp)

(define-constant +key-modifier-sigma+      #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +key-modifier-mean+       #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+ #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +minimum-key-level+                  1          :test #'=)

(define-constant +maximum-key-level+                 10          :test #'=)

(defun key-level-params (map-level)
  (values (elt +key-level-sigma+ map-level)
	  (elt +key-level-mean+  map-level)))

(defun calculate-key-level (map-level)
  (multiple-value-bind (sigma mean)
      (key-level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-key-level+ +maximum-key-level+)))

(defun key-modifier-params (key-level)
  (values (elt +key-modifier-sigma+ key-level)
	  (elt +key-modifier-mean+  key-level)))

(defun calculate-key-modifier (key-level)
  (multiple-value-bind (sigma mean)
      (key-modifier-params (1- key-level))
    (gaussian-probability sigma mean)))

(defun key-set-healing-hp-effect (path key-level interaction)
  (let ((effect-object (make-instance 'heal-damage-points-effect-parameters
				      :trigger +effect-when-used+
				      :points  (calculate-key-modifier key-level)
				      :chance  (calculate-healing-fx-params-chance key-level)
				      :target  +target-self+)))
    (n-setf-path-value interaction path effect-object)))

(defun key-set-healing-effect (effect-path key-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-when-used+
				      :duration (healing-fx-params-duration key-level)
				      :chance   (calculate-healing-fx-params-chance key-level)
				      :target  +target-self+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun generate-key (interaction-file character-file map-level keycode)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((key-level       (calculate-key-level map-level))
	     (healing-effects-no (number-of-healing-effects key-level 0))
	     (healing-effects    (%get-healing-fx-shuffled template healing-effects-no)))
	(loop for i in healing-effects do
	     (cond
	       ((eq i +heal-damage-points+)
		(key-set-healing-hp-effect (list +healing-effects+ i) key-level template))
	       (t
		(key-set-healing-effect (list +healing-effects+ i) key-level template))))
	(n-setf-path-value template (list +can-open+) keycode)
	(setf template (remove-generate-symbols template))
	(key-fill-character-plist +key-type-name+ char-template)
	(let ((key-character (params->character char-template)))
	  (setf (basic-interaction-params key-character) template)
	  key-character)))))

(defun key-regexp-file-portrait ()
  (strcat +key-type-name+ +key-file-record-sep+))

(defun key-fill-character-plist (key-name-type character)
  (let* ((regex          (key-regexp-file-portrait))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (n-setf-path-value character (list +last-name+) "")
    (n-setf-path-value character (list +first-name+) key-name-type)))
