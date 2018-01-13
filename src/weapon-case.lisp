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

(in-package :character)

(defmacro weapon-case ((entity) &body body)
  (let ((ammitted '(:pole :bow :crossbow :melee :none)))
    (misc:check-body-keywords body ammitted)
    (with-gensyms (ghost) ; weapon-type)
      `(let* ((,ghost (entity:ghost ,entity)))
         #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
         ;; (,weapon-type (character:weapon-type ,ghost)))
         ;; (when ,weapon-type
         (cond
           ((character:weapon-type-pole-p ,ghost)
            ,(getf body (elt ammitted 0)))
           ((character:weapon-type-bow-p ,ghost)
            ,(getf body (elt ammitted 1)))
           ((character:weapon-type-crossbow-p ,ghost)
            ,(getf body (elt ammitted 2)))
           ((or (character:weapon-type-edge-p   ,ghost)
                (character:weapon-type-impact-p ,ghost))
            ,(getf body (elt ammitted 3)))
           (t ;; no weapon
            ,(getf body (elt ammitted 4))))))))
