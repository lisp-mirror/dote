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

(in-package :random-inert-object)

(define-constant +minimum-level+                          1          :test #'=)

(define-constant +maximum-level+                          9          :test #'=)

(define-constant +minimum-damage-point+                   1.0        :test #'=)

(define-constant +maximum-damage-point+                 100.0        :test #'=)

(defun randomize-damage-points (character level )
  (setf (damage-points character)
	(calculate-randomized-damage-points level
					    +minimum-level+
					    +maximum-level+
					    +minimum-damage-point+
					    +maximum-damage-point+
					    (d/ (d level) (d* 5.0 (d +maximum-level+))))))

(defun calculate-level (map-level)
  (alexandria:clamp map-level 1 3))

(defun generate-inert-object (map-level)
  (clean-effects
   (%generate-inert-object (res:get-resource-file +default-interaction-filename+
						  +default-character-inert-obj-dir+
						  :if-does-not-exists :error)
			   (res:get-resource-file +default-character-filename+
						  +default-character-inert-obj-dir+
						  :if-does-not-exists :error)
			   map-level)))

(defun %generate-inert-object (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters (template interaction-file)
      (let ((object-level (calculate-level map-level)))
	(setf template (remove-generate-symbols template))
	(let ((object-character (params->np-character char-template)))
	  (setf (basic-interaction-params object-character) template)
	  (randomize-damage-points object-character object-level)
	  object-character)))))
