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

(in-package :transformable)

(defclass transformable ()
  ((projection-matrix
    :initform (vector (sb-cga:identity-matrix))
    :reader projection-matrix
    :initarg :projection-matrix)
   (model-matrix
    :initform (vector (sb-cga:identity-matrix))
    :reader model-matrix
    :initarg :model-matrix)
   (view-matrix
    :initform (vector (sb-cga:identity-matrix))
    :reader view-matrix
    :initarg :view-matrix)))

(defmacro gen-accessors-matrix (name)
  (let ((fn-name (list (alexandria:format-symbol t "~:@(setf~)")
                       (alexandria:format-symbol t "~:@(~a~)" name))))
    `(progn
       (defgeneric ,fn-name (new-value object))
       (defmethod ,fn-name (new-value (object transformable))
         (with-slots (,name) object
           (setf (elt ,name 0) new-value))))))

(gen-accessors-matrix projection-matrix)

(gen-accessors-matrix model-matrix)

(gen-accessors-matrix view-matrix)

(defgeneric build-projection-matrix (object near far fov ratio))

(defmethod clone-into :after ((from transformable) (to transformable))
  (setf (projection-matrix to) (3d-utils:clone-matrix (elt (projection-matrix from) 0))
        (model-matrix      to) (3d-utils:clone-matrix (elt (model-matrix      from) 0))
        (view-matrix       to) (3d-utils:clone-matrix (elt (view-matrix       from) 0)))
  to)
