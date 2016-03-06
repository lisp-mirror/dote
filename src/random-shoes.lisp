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

(in-package :random-shoes)

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

(define-constant +minimum-decay+                 20.0     :test #'=)

(define-constant +minimum-level+                  1       :test #'=)

(define-constant +maximum-level+                  9       :test #'=)

(define-constant +minimum-chance-effects+         2.0     :test #'=)

(define-constant +maximum-chance-effects+         4.0     :test #'=)

(define-constant +type-name+                      "shoes" :test #'string=)

(define-constant +minimum-damage-point+           1.0     :test #'=)

(define-constant +maximum-damage-point+          50.0     :test #'=)

(defun randomize-damage-points (character level)
  (setf (damage-points character)
	(calculate-randomized-damage-points level
					     +minimum-level+
					     +maximum-level+
					     +minimum-damage-point+
					     +maximum-damage-point+
					     (d/ (d level) (d* 5.0 (d +maximum-level+))))))

(defun decay-params (shoes-level)
  (values (elt +decay-sigma+ shoes-level)
	  (elt +decay-mean+  shoes-level)))

(defun calculate-decay-points (shoes-level)
  (multiple-value-bind (sigma mean)
      (decay-params (1- shoes-level))
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

(defun modifier-params (shoes-level)
  (values (elt +modifier-sigma+ shoes-level)
	  (elt +modifier-mean+  shoes-level)))

(defun calculate-modifier (weapon-level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- weapon-level))
    (d- (gaussian-probability sigma mean)
	(gaussian-probability (d/ sigma 4.0) (- weapon-level)))))

(defun calculate-level (map-level)
  (multiple-value-bind (sigma mean)
      (level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-level+ +maximum-level+)))

(defun number-of-effects (shoes-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- shoes-level)))
			       +minimum-chance-effects+
			       +maximum-chance-effects+))))
    (lcg-next-upto max)))

(defun generate-shoes (map-level)
  (clean-effects
   (%generate-shoes (res:get-resource-file +default-interaction-filename+
					   +default-character-shoes-dir+
					   :if-does-not-exists :error)
		    (res:get-resource-file +default-character-filename+
					   +default-character-shoes-dir+
					   :if-does-not-exists :error)
		    map-level)))

(defun %generate-shoes (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((shoes-level (calculate-level map-level))
	     (shoes-decay (calculate-decay-points shoes-level))
	     (effects-no  (number-of-effects shoes-level))
	     (effects     (%get-normal-fx-shuffled  template effects-no)))
	(n-setf-path-value char-template (list +level+) (d shoes-level))
	(n-setf-path-value char-template (list +description+) +type-name+)
	(n-setf-path-value template
			   (list +decay+)
			   (calculate-decay shoes-level shoes-decay))
	(loop for i in effects do
	     (set-effect (list +effects+ i) shoes-level template))
	(setf template (remove-generate-symbols template))
	(fill-character-plist char-template shoes-level)
	(let ((shoes-character (params->np-character char-template)))
	  (setf (basic-interaction-params shoes-character) template)
	  (randomize-damage-points shoes-character shoes-level)
	  shoes-character)))))

(defun set-effect (effect-path shoes-level interaction)
  (let ((effect-object (make-instance 'effect-parameters
				      :modifier (calculate-modifier shoes-level)
				      :trigger  +effect-when-worn+
				       ;; effect lasting forever  for
				       ;; shoes,  they   will  broke
				       ;; anyway.
				      :duration +duration-unlimited+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun regexp-file-portrait (shoes-level)
  (strcat +type-name+
	  +file-record-sep+
	  (format nil "~2,'0d" shoes-level)))

(defun fill-character-plist (character shoes-level)
  (let* ((regex          (regexp-file-portrait shoes-level))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (n-setf-path-value character (list +last-name+)  "")
    (n-setf-path-value character (list +first-name+) +type-name+)))
