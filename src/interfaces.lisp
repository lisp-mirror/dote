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

(in-package :interfaces)

(defclass destructible () ())

(defgeneric destroy (object))

(defmethod destroy ((object destructible)))

(defgeneric current-time (object))

(defgeneric main-light-pos (renderer))

(defgeneric main-light-pos-eye-space (renderer))

(defgeneric main-light-color (renderer))

(defgeneric elapsed-time (renderer))

(defgeneric get-camera-pos (renderer))

(defgeneric clone (object))

(defmethod clone (object))

(defgeneric clone-into (from to))

(defmethod  clone-into (from to))

(defgeneric initializedp (object))

(defgeneric serialize (object))

(defgeneric deserialize (object file))

(defmethod serialize (object)
    (format nil "~s" (marshal:marshal object)))

(defmethod deserialize (object file)
  (declare (ignore object))
  (marshal:unmarshal (read-from-string (filesystem-utils:slurp-file file))))

(defclass renderizable () 
  ((compiled-shaders
    :initform nil
    :accessor compiled-shaders
    :initarg :compiled-shaders)))

(defmethod clone-into :after ((from renderizable) (to renderizable))
  (setf (compiled-shaders to) (compiled-shaders from))
  to)

(defgeneric prepare-for-rendering (object))

(defgeneric update-for-rendering (object))

(defgeneric render (object renderer))

(defgeneric render-debug (object renderer))

(defgeneric render-for-reflection (object renderer))

(defmethod  render-for-reflection (object renderer)
  (render object renderer))

(defgeneric calculate (object dt))

(defmethod prepare-for-rendering ((object renderizable)))

(defmethod render ((object renderizable) renderer))

(defmethod calculate ((object renderizable) dt))

(defgeneric main-state (object))

(defgeneric to-sexp (object))

(defmethod-inline-function to-sexp ((object number))
  object)

(defmethod-inline-function to-sexp ((object (eql nil)))
  nil)

(defgeneric from-sexp (object sexp))
