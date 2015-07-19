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

(define-constant +container-file-record-sep+               "-"         :test #'string=)

(define-constant +container-type-name+                     "container" :test #'string=)

(define-constant +minimum-container-level+                  1          :test #'=)

(define-constant +maximum-container-level+                  10         :test #'=)

(define-constant +minimum-container-modifier+               1.0        :test #'=)

(define-constant +minimum-chance-container-healing-effects+ 2.0        :test #'=)

(define-constant +maximum-chance-container-healing-effects+ 4.0        :test #'=)

(define-constant +container-level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +container-level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.3 3.4 3.6)
  :test #'equalp)

(define-constant +container-modifier-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +container-modifier-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+  #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+   #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +minimum-container-level+                  1          :test #'=)

(define-constant +maximum-container-level+                 10          :test #'=)

(defun container-level-params (map-level)
  (values (elt +container-level-sigma+ map-level)
	  (elt +container-level-mean+  map-level)))

(defun container-modifier-params (container-level)
  (values (elt +container-modifier-sigma+ container-level)
	  (elt +container-modifier-mean+  container-level)))

(defun container-locked-params (container-level)
  (values (elt (reverse +container-level-sigma+) container-level)
	  (elt (reverse +container-level-mean+)  container-level)))

(defun calculate-container-locked-chance (container-level)
  (multiple-value-bind (sigma mean)
      (container-locked-params (1- container-level))
    (round (max 2.0 (gaussian-probability sigma mean)))))

(defun calculate-container-modifier (container-level)
  (multiple-value-bind (sigma mean)
      (container-modifier-params (1- container-level))
    (abs (gaussian-probability sigma mean))))

(defun calculate-container-level (map-level)
  (multiple-value-bind (sigma mean)
      (container-level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-container-level+ +maximum-container-level+)))

(defun container-number-of-healing-effects (container-level)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   20.0
							   (d (1- container-level)))
				  +minimum-chance-container-healing-effects+
				  +maximum-chance-container-healing-effects+)
		       1))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun container-set-healing-effect (effect-path container-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-when-used+
				      :duration  (calculate-container-modifier container-level)
				      :chance (calculate-healing-fx-params-chance container-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun container-set-poison-effect (effect-path container-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :points-per-turn (calculate-container-modifier container-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun container-fill-character-plist (character)
  (n-setf-path-value character (list +portrait+)  nil)
  (n-setf-path-value character (list +last-name+) "")
  (n-setf-path-value character (list +first-name+) +container-type-name+))

(defun generate-keycode ()
  (subseq (shuffle "1234567890") 0 8))

(defun generate-container (interaction-file character-file map-level
			   key-interaction-file key-character-file
			   &key
			     (keychain '()))
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((container-level        (calculate-container-level map-level))
	     (healing-effects-no     (number-of-healing-effects container-level 0))
	     (healing-effects       (%get-healing-fx-shuffled template healing-effects-no)))
	(n-setf-path-value template (list +decay+) nil)
	(loop for i in healing-effects do
	     (cond
	       ((eq i +heal-damage-points+) nil)
	       ((eq i +cause-poison+)
		(container-set-poison-effect (list +healing-effects+ i) container-level template))
	       (t
		(container-set-healing-effect (list +healing-effects+ i)
					      container-level template))))
	(setf template (remove-generate-symbols template))
	(container-fill-character-plist char-template)
	(when (= (lcg-next-upto (calculate-container-locked-chance container-level)) 0)
	  (let ((keycode (generate-keycode)))
	    (push (generate-key key-interaction-file key-character-file map-level keycode)
		  keychain)
	    (n-setf-path-value template (list +can-open+) keycode)))
	(let ((container-character (params->character char-template)))
	  (setf (basic-interaction-params container-character) template)
	  container-character))))
  keychain)
