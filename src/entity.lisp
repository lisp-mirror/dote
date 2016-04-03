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

(in-package :entity)

(defclass entity (identificable)
  ((modified
    :accessor modified
    :initform nil)
   (pos
    :accessor pos
    :initarg :pos
    :initform (vec 0.0 0.0 0.0))
   (dir
    :accessor dir
    :initarg :dir
    :initform +entity-forward-direction+)
   (scaling
    :accessor scaling
    :initarg :scaling
    :initform (vec 1.0 1.0 1.0))
   (up
    :accessor up
    :initarg :up
    :initform +entity-up-direction+)
   (ghost
    :accessor ghost
    :initarg :ghost
    :initform nil
    :type (or character:np-character character:player-character))
   (tooltip-count
    :accessor tooltip-count
    :initarg  :tooltip-count
    :initform 0.0
    :type     num:desired-type)
   (state
    :accessor state
    :initarg :state
    :initform nil
    :allocation :class)))

(defmethod clone-into :after ((from entity) (to entity))
  (setf (modified to) (modified from)
	(pos to)      (copy-vec (pos from))
	(dir to)      (copy-vec (dir from))
	(scaling to)  (copy-vec (scaling from))
	(up to)       (copy-vec (up from))
	(state to)    (state    from))
  to)

(defmethod marshal:class-persistant-slots ((object entity))
  (append '(modified
	    pos
	    dir
	    scaling
	    up)
	  (call-next-method)))

(defgeneric aabb-2d (object))

(defgeneric find-entity-by-id (object id))

(defgeneric remove-entity-by-id (object id))

(defgeneric remove-entity-if (object predicate))

(defgeneric entity-dead-p (object))

(defgeneric incf-tooltip-ct (object))

(defgeneric decf-tooltip-ct (object))

(defgeneric reset-tooltip-ct (object))

(defmethod incf-tooltip-ct ((object entity))
  (with-accessors ((tooltip-count tooltip-count)) object
    (setf tooltip-count (d+ tooltip-count 1.0))))

(defmethod decf-tooltip-ct ((object entity))
  (with-accessors ((tooltip-count tooltip-count)) object
    (setf tooltip-count (max 0.0
			     (d- tooltip-count 1.0)))))

(defmethod reset-tooltip-ct ((object entity))
  (with-accessors ((tooltip-count tooltip-count)) object
    (setf tooltip-count 0.0)))
