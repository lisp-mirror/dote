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

(in-package :identificable)

(defparameter *clone-id* nil)

;; not thread-safe i guess
(defparameter *entity-id-counter* (the fixnum +start-id-counter+))

(defun init-counter ()
  (setf *entity-id-counter* (the fixnum +start-id-counter+)))

(defun valid-id-p (id)
  (>= id +start-id-counter+))

(defun next-id ()
  (prog1
      *entity-id-counter*
    (incf *entity-id-counter* 1)))

(defun not-valid-id ()
  (1- +start-id-counter+))

(defclass identificable ()
  ((id
    :initform (not-valid-id)
    :initarg :id
    :accessor id)))

(defmethod initialize-instance :after ((object identificable) &key &allow-other-keys)
  (refresh-id object))

(defmethod clone-into :after ((from identificable) (to identificable))
  (when *clone-id*
    (setf (id to) (id from)))
  to)

(defgeneric refresh-id (object))

(defmethod refresh-id ((object identificable))
  (setf (id object) (next-id))
  object)

(defun test-id= (a b)
  (= (id a) (id b)))

(defmethod cl-kanren:equivp ((lhs identificable) (rhs identificable))
  (= (id lhs) (id rhs)))
