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

(in-package :mesh-material)

(defclass mesh-material ()
  ((ka
    :initform (num:d 1.0)
    :initarg  :ka
    :accessor ka)
   (kd
    :initform (num:d 1.0)
    :initarg  :kd
    :accessor kd)
   (ks
    :initform (num:d 1.0)
    :initarg  :ks
    :accessor ks)
   (roughness
    :initform (num:d 1.0)
    :initarg  :roughness
    :accessor roughness)
   (shininess 
    :initform (num:d 128.0)
    :initarg  :shininess
    :accessor shininess)))

(defmethod print-object ((object mesh-material) stream)
  (format stream "[ ka ~a kd ~a ks ~a roughness ~a shininess ~a ]"
	  (ka object) (kd object) (ks object) (roughness object) (shininess object)) )

(defmethod marshal:class-persistant-slots ((object mesh-material))
  '(ka kd ks roughness shininess))

(defmethod clone-into :after ((from mesh-material) (to mesh-material))
  (setf (ka        to) (ka from)
	(kd        to) (kd from)
	(ks        to) (ks from)
	(roughness to) (roughness from)
	(shininess to) (shininess from)))

(defmethod clone ((object mesh-material))
  (make-mesh-material (ka object)
		      (kd object)
		      (ks object)
		      (roughness object)
		      (shininess object)))

(defun make-mesh-material (ka kd ks roughness shininess)
  (make-instance 'mesh-material :ka ka :kd kd :ks ks :roughness roughness
		 :shininess shininess))
