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

(in-package :random-container)

(define-constant +file-record-sep+               "-"         :test #'string=)

(define-constant +type-name+                     "container" :test #'string=)

(define-constant +minimum-level+                  1          :test #'=)

(define-constant +maximum-level+                  9         :test #'=)

(define-constant +minimum-modifier+               1.0        :test #'=)

(define-constant +minimum-num-healing-effects+ 2.0        :test #'=)

(define-constant +maximum-num-healing-effects+ 4.0        :test #'=)

(define-constant +minimum-chance-healing-fx+   0.05       :test #'=)

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

(define-constant +minimum-container-level+                  1          :test #'=)

(define-constant +maximum-container-level+                 10          :test #'=)

(define-constant +minimum-damage-point+                   1.0          :test #'=)

(define-constant +maximum-damage-point+                 100.0          :test #'=)

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

(defun modifier-params (container-level)
  (values (elt +modifier-sigma+ container-level)
	  (elt +modifier-mean+  container-level)))

(defun locked-params (container-level)
  (values (elt (reverse +level-sigma+) container-level)
	  (elt (reverse +level-mean+)  container-level)))

(defun calculate-locked-chance (container-level)
  (multiple-value-bind (sigma mean)
      (locked-params (1- container-level))
    (round (max 2.0 (gaussian-probability sigma mean)))))

(defun calculate-container-modifier (container-level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- container-level))
    (abs (gaussian-probability sigma mean))))

(defun calculate-container-level (map-level)
  (multiple-value-bind (sigma mean)
      (level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-level+ +maximum-level+)))

(defun healing-fx-params-chance (container-level)
  (values (elt +chance-healing-fx-sigma+ container-level)
	  (elt +chance-healing-fx-mean+  container-level)))

(defun calculate-healing-fx-params-chance (container-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-chance (1- container-level))
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun number-of-healing-effects (container-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- container-level)))
				  +minimum-num-healing-effects+
				  +maximum-num-healing-effects+)
		       number-of-normal-effects))))
    (if (<= max 0)
	0
	(lcg-next-upto (max 0.0 max)))))

(defun set-healing-effect (effect-path container-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
				      :trigger  +effect-when-used+
				      :duration  (ceiling
						  (calculate-container-modifier container-level))
				      :chance (calculate-healing-fx-params-chance container-level)
				      :target +target-self+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun set-poison-effect (effect-path container-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :points-per-turn (calculate-container-modifier container-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun fill-character-plist (character)
  (n-setf-path-value character (list +portrait+)  nil)
  (n-setf-path-value character (list +last-name+) "")
  (n-setf-path-value character (list +first-name+) +type-name+))

(defun generate-keycode ()
  (subseq (shuffle "1234567890") 0 8))

(defun generate-container (map-level &key (keychain (misc:make-fresh-array 0 nil t nil)))
  (clean-effects
   (%generate-container (res:get-resource-file +default-interaction-filename+
					       +default-character-container-dir+
					       :if-does-not-exists :error)
			(res:get-resource-file +default-character-filename+
					       +default-character-container-dir+
					       :if-does-not-exists :error)
			map-level
			(res:get-resource-file +default-interaction-filename+
					       +default-character-key-dir+
					       :if-does-not-exists :error)
			(res:get-resource-file +default-character-filename+
					       +default-character-key-dir+
					       :if-does-not-exists :error)
			:keychain keychain)))

(defun %generate-container (interaction-file character-file map-level
			    key-interaction-file key-character-file
			    &key
			      (keychain (misc:make-fresh-array 0 nil t nil)))
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((container-level       (calculate-container-level map-level))
	     (healing-effects-no    (number-of-healing-effects container-level 0))
	     (healing-effects       (%get-healing-fx-shuffled template healing-effects-no)))
	(n-setf-path-value char-template (list +level+) (d container-level))
	(n-setf-path-value template (list +decay+) nil)
	(loop for i in healing-effects do
	     (cond
	       ((eq i +heal-damage-points+) nil)
	       ((eq i +cause-poison+)
		(set-poison-effect (list +healing-effects+ i) container-level template))
	       (t
		(set-healing-effect (list +healing-effects+ i)
					      container-level template))))
	(setf template (remove-generate-symbols template))
	(fill-character-plist char-template)
	(if (= (lcg-next-upto (calculate-locked-chance container-level)) 0)
	    (let ((keycode (generate-keycode)))
	      (vector-push-extend (random-key:generate-key* key-interaction-file
							    key-character-file
							    map-level
							    keycode)
				  keychain)
	      (n-setf-path-value template (list +can-be-opened+) keycode)) ; can
									   ; be
									   ; opened
									   ; with
									   ; appropriate
									   ; key
	      (n-setf-path-value template (list +can-be-opened+) t)) ; no key
	(let ((container-character (params->np-character char-template)))
	  (setf (basic-interaction-params container-character) template)
	  (randomize-damage-points container-character container-level)
	  (values container-character keychain))))))
