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

(define-constant +shoes-file-record-sep+ "-"  :test #'string=)

(define-constant +shoes-level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +shoes-level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.3 3.4 3.6)
  :test #'equalp)

(define-constant +decay-shoes-sigma+    #(56.0 48.0 40.0 36.0 36.0 24.0 22.0 20.0 30.0 35.0)
  :test #'equalp)

(define-constant +decay-shoes-mean+     #(30.0 28.0 26.0 24.0 12.0 10.0 9.0 8.0 4.0 0.0)
  :test #'equalp)

(define-constant +shoes-modifier-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7 7.5 8)
  :test #'equalp)

(define-constant +shoes-modifier-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +minimum-decay-shoes+                 20.0        :test #'=)

(define-constant +minimum-shoes-level+                  1          :test #'=)

(define-constant +maximum-shoes-level+                 10          :test #'=)

(define-constant +minimum-chance-shoes-effects+         2.0        :test #'=)

(define-constant +maximum-chance-shoes-effects+         4.0        :test #'=)

(define-constant +shoes-type-name+                      "shoes"   :test #'string=)

(defun shoes-decay-params (shoes-level)
  (values (elt +decay-shoes-sigma+ shoes-level)
	  (elt +decay-shoes-mean+  shoes-level)))

(defun calculate-shoes-decay-points (shoes-level)
  (multiple-value-bind (sigma mean)
      (shoes-decay-params (1- shoes-level))
    (truncate (max +minimum-decay-shoes+ (gaussian-probability sigma mean)))))

(defun calculate-shoes-decay (object-level character decay-points)
  (make-instance 'decay-parameters
		 :leaving-message (format nil
					  (_ "~a broken")
					  (plist-path-value character (list +description+)))
		 :points decay-points
		 :when-decay (if (and (> object-level (/ +maximum-shoes-level+ 2))
				      (= (lcg-next-upto 10) 0))
				 +decay-by-turns+ +decay-by-use+)))

(defun shoes-level-params (map-level)
  (values (elt +shoes-level-sigma+ map-level)
	  (elt +shoes-level-mean+  map-level)))

(defun shoes-modifier-params (shoes-level)
  (values (elt +shoes-modifier-sigma+ shoes-level)
	  (elt +shoes-modifier-mean+  shoes-level)))

(defun calculate-shoes-modifier (shoes-level)
  (multiple-value-bind (sigma mean)
      (shoes-modifier-params (1- shoes-level))
    (gaussian-probability sigma mean)))

(defun calculate-shoes-level (map-level)
  (multiple-value-bind (sigma mean)
      (shoes-level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
	   +minimum-shoes-level+ +maximum-shoes-level+)))

(defun shoes-number-of-effects (shoes-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
							   10.0
							   (d (1- shoes-level)))
			       +minimum-chance-shoes-effects+
			       +maximum-chance-shoes-effects+))))
    (lcg-next-upto max)))

(defun generate-shoes (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let* ((shoes-level (calculate-shoes-level map-level))
	     (shoes-decay (calculate-shoes-decay-points shoes-level))
	     (effects-no  (shoes-number-of-effects shoes-level))
	     (effects     (%get-normal-fx-shuffled  template effects-no)))
	(n-setf-path-value char-template (list +description+) +shoes-type-name+)
	(n-setf-path-value template
			   (list +decay+)
			   (calculate-shoes-decay shoes-level char-template shoes-decay))
	(loop for i in effects do
	     (shoes-set-effect (list +effects+ i) shoes-level template))
	(setf template (remove-generate-symbols template))
	(shoes-fill-character-plist char-template shoes-level)
	(let ((shoes-character (params->character char-template)))
	  (setf (basic-interaction-params shoes-character) template)
	  shoes-character)))))

(defun shoes-set-effect (effect-path shoes-level interaction)
  (let ((effect-object (make-instance 'effect-parameters
				      :modifier (calculate-shoes-modifier shoes-level)
				      :trigger  +effect-when-worn+
				       ;; effect lasting forever  for
				       ;; shoes,  they   will  broke
				       ;; anyway.
				      :duration  :unlimited)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun regexp-file-portrait (shoes-level)
  (strcat +shoes-type-name+
	  +shoes-file-record-sep+
	  (format nil "~2,'0d" shoes-level)))

(defun shoes-fill-character-plist (character shoes-level)
  (let* ((regex          (regexp-file-portrait shoes-level))
	 (portrait-file  (random-elt (remove-if #'(lambda (a) (not (cl-ppcre:scan regex a)))
						(res:get-resource-files
						 +default-gui-inventory-items+)
						:key #'uiop:native-namestring))))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (n-setf-path-value character (list +last-name+)  "")
    (n-setf-path-value character (list +first-name+) +shoes-type-name+)))
