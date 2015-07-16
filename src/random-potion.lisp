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

(define-constant +potion-file-record-sep+ "-" :test #'string=)

(define-constant +potion-type-name+              "potion"     :test #'string=)

(define-constant +minimum-potion-level+            1          :test #'=)

(define-constant +maximum-potion-level+           10          :test #'=)

(define-constant +minimum-potion-modifier+         1.0        :test #'=)

(define-constant +minimum-chance-potion-effects+   .1         :test #'=)

(define-constant +maximum-chance-potion-effects+   1.0        :test #'=)

(define-constant +re-healing+                      "p-heal"   :test #'string=)

(define-constant +berserk+                         "berserk"  :test #'string=)

(define-constant +faint+                           "faint"    :test #'string=)

(define-constant +terror+                          "terror"   :test #'string=)

(define-constant +poison+                          "poison"   :test #'string=)

(define-constant +potion-level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +potion-level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.3 3.4 3.6)
  :test #'equalp)

(define-constant +modifier-potion-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +modifier-potion-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+  #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+   #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +minimum-potion-level+                  1          :test #'=)

(define-constant +maximum-potion-level+                 10          :test #'=)

(defun potion-level-params (map-level)
  (values (elt +potion-level-sigma+ map-level)
	  (elt +potion-level-mean+  map-level)))

(defun potion-modifier-params (potion-level)
  (values (elt +modifier-potion-sigma+ potion-level)
	  (elt +modifier-potion-mean+  potion-level)))

(defun calculate-potion-modifier (potion-level)
  (multiple-value-bind (sigma mean)
      (potion-modifier-params (1- potion-level))
    (abs (gaussian-probability sigma mean))))

(defun calculate-potion-level (map-level)
  (multiple-value-bind (sigma mean)
      (potion-level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-potion-level+ +maximum-potion-level+)))

(defun potion-set-healing-hp-effect (path potion-level interaction)
  (let ((effect-object (make-instance 'heal-damage-points-effect-parameters
				      :trigger +effect-when-used+
				      :points  (calculate-potion-modifier potion-level)
				      :chance  (calculate-healing-fx-params-chance potion-level))))
    (n-setf-path-value interaction path effect-object)))

(defun potion-set-healing-effect (effect-path potion-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-when-used+
				      :duration  (calculate-potion-modifier potion-level)
				      :chance (calculate-healing-fx-params-chance potion-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun potion-set-poison-effect (effect-path potion-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :points-per-turn (calculate-potion-modifier potion-level))))
    (n-setf-path-value interaction effect-path effect-object)))


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
		(potion-set-healing-hp-effect (list +healing-effects+ i) potion-level template))
	       ((eq i +cause-poison+)
		(potion-set-poison-effect (list +healing-effects+ i) potion-level template))
	       (t
		(potion-set-healing-effect (list +healing-effects+ i) potion-level template))))
	(setf template (remove-generate-symbols template))
	(potion-fill-character-plist +potion-type-name+ char-template template potion-level)
	(let ((potion-character (params->character char-template)))
	  (setf (basic-interaction-params potion-character) template)
	  potion-character)))))

(defun potion-filename-effects-string (interaction)
  (cond
    ((plist-path-value interaction (list +healing-effects+ +heal-poison+))
     (strcat +re-healing+ +potion-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +heal-berserk+))
     (strcat +re-healing+ +potion-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +heal-faint+))
     (strcat +re-healing+ +potion-file-record-sep+ "faint"))
    ((plist-path-value interaction (list +healing-effects+ +heal-terror+))
     (strcat +re-healing+ +potion-file-record-sep+ "terror"))
    ((plist-path-value interaction (list +healing-effects+ +heal-damage-points+))
     +re-healing+)
    ((plist-path-value interaction (list +healing-effects+ +immune-poison+))
     (strcat +re-healing+ +potion-file-record-sep+ "immune"
	     +potion-file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +immune-berserk+))
     (strcat +re-healing+ +potion-file-record-sep+  "immune"
	     +potion-file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +immune-faint+))
     (strcat +re-healing+ +potion-file-record-sep+ "immune"
	     +potion-file-record-sep+ "faint"))
    (t
     (strcat +re-healing+ +potion-file-record-sep+ "immune"
	     +potion-file-record-sep+ "faint"))))

(defun potion-regexp-file-portrait (interaction potion-level)
  (strcat (potion-filename-effects-string interaction)
	  +potion-file-record-sep+
	  (format nil "~2,'0d" potion-level)))

(defun potion-build-file-names-db (potion-name-type potion-level)
  (strcat potion-name-type
	  +potion-file-record-sep+
	  (format nil "~2,'0d" potion-level)
	  ".lisp"))

(defun potion-fill-character-plist (potion-name-type character interaction potion-level)
  (let* ((regex          (potion-regexp-file-portrait interaction potion-level))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (n-setf-path-value character (list +last-name+) "")
    (n-setf-path-value character (list +first-name+) potion-name-type)))
