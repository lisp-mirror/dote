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

(in-package :random-fountain)

(define-constant +file-record-sep+ "-" :test #'string=)

(define-constant +type-name+              "fountain"     :test #'string=)

(define-constant +minimum-level+            1          :test #'=)

(define-constant +maximum-level+            9          :test #'=)

(define-constant +minimum-modifier+         1.0        :test #'=)

(define-constant +minimum-chance-effects+   .1         :test #'=)

(define-constant +maximum-chance-effects+   1.0        :test #'=)

(define-constant +minimum-chance-healing-effects+ 2.0        :test #'=)

(define-constant +maximum-chance-healing-effects+ 4.0        :test #'=)

(define-constant +re-healing+             "p-heal"   :test #'string=)

(define-constant +berserk+                "berserk"  :test #'string=)

(define-constant +faint+                  "faint"    :test #'string=)

(define-constant +terror+                 "terror"   :test #'string=)

(define-constant +poison+                 "poison"   :test #'string=)

(define-constant +decay-sigma+    #(2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0)
  :test #'equalp)

(define-constant +decay-mean+     #(22.0 21.0 20.0 19.0 18.0 17.0 16.0 15.0 14.0 13.0)
  :test #'equalp)

(define-constant +minimum-decay+   1                 :test #'equalp)

(define-constant +level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.8 4.8 6.0)
  :test #'equalp)

(define-constant +modifier-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(define-constant +modifier-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+  #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+   #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +minimum-fountain-level+      1   :test #'=)

(define-constant +maximum-fountain-level+     10   :test #'=)

(define-constant +minimum-chance-healing-fx+  0.05 :test #'=)

(defun level-params (map-level)
  (values (elt +level-sigma+ map-level)
	  (elt +level-mean+  map-level)))

(defun modifier-params (fountain-level)
  (values (elt +modifier-sigma+ fountain-level)
	  (elt +modifier-mean+  fountain-level)))

(defun calculate-modifier (fountain-level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- fountain-level))
    (abs (gaussian-probability sigma mean))))

(defun calculate-level (map-level)
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

(defun set-healing-dmg-effect (path fountain-level interaction)
  (let ((effect-object (make-instance 'heal-damage-points-effect-parameters
				      :trigger +effect-when-used+
				      :points  (calculate-modifier fountain-level)
				      :chance  (calculate-healing-fx-params-chance
						fountain-level)
				      :target  +target-self+)))
    (n-setf-path-value interaction path effect-object)))

(defun set-healing-effect (effect-path fountain-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-when-used+
				      :duration (healing-effect-duration
						 effect-path
						 (ceiling (calculate-modifier fountain-level)))
				      :chance (calculate-healing-fx-params-chance
					       fountain-level)
				       :target  +target-self+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun set-poison-effect (effect-path fountain-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :points-per-turn (calculate-modifier
							fountain-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun fill-character-plist (character)
  (n-setf-path-value character (list +portrait+)  nil)
  (n-setf-path-value character (list +last-name+) "")
  (n-setf-path-value character (list +first-name+) +type-name+))

(defun decay-params (fountain-level)
  (values (elt +decay-sigma+ fountain-level)
	  (elt +decay-mean+  fountain-level)))

(defun calculate-decay-points (fountain-level)
  (multiple-value-bind (sigma mean)
      (decay-params (1- fountain-level))
    (truncate (max +minimum-decay+ (gaussian-probability sigma mean)))))

(defun calculate-decay (object-level character decay-points)
  (make-instance 'decay-parameters
		 :leaving-message (format nil
					  (_ "~a exhausted")
					  (plist-path-value character (list +description+)))
		 :points decay-points
		 :when-decay (if (and (> object-level (/ +maximum-level+ 2))
				      (= (lcg-next-upto 10) 0))
				 +decay-by-turns+ +decay-by-use+)))

(defun number-of-healing-effects (weapon-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- weapon-level)))
				  +minimum-chance-healing-effects+
				  +maximum-chance-healing-effects+)
		       number-of-normal-effects))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun generate-fountain (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((fountain-level        (calculate-level map-level))
	     (healing-effects-no    (number-of-healing-effects fountain-level 0))
	     (fountain-decay-points (calculate-decay-points fountain-level))
	     (healing-effects       (%get-healing-fx-shuffled template healing-effects-no)))
	(n-setf-path-value char-template (list +description+) +type-name+)
	(n-setf-path-value template
			   (list +decay+)
			   (calculate-decay fountain-level char-template
						     fountain-decay-points))
	(loop for i in healing-effects do
	     (cond
	       ((eq i +heal-damage-points+)
		(set-healing-dmg-effect (list +healing-effects+ i)
						fountain-level template))
	       ((eq i +cause-poison+)
		(set-poison-effect (list +healing-effects+ i) fountain-level template))
	       (t
		(set-healing-effect (list +healing-effects+ i) fountain-level template))))
	(setf template (remove-generate-symbols template))
	(fill-character-plist char-template)
	(let ((fountain-character (params->np-character char-template)))
	  (setf (basic-interaction-params fountain-character) template)
	  fountain-character)))))
