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

(in-package :action-scheduler)

(defclass game-action (identificable)
  ((launch-action-fn
    :initform      (constantly t)
    :initarg       :launch-action-fn
    :accessor      launch-action-fn
    :type          function
    :documentation "Value is nil if something wrong happened")))

(defclass launch-arrow-action (game-action) ())

(defun %make-queue ()
  (make-instance 'simple-queue))

(defclass action-scheduler ()
  ((current-action
    :initform nil
    :initarg  :current-action
    :accessor current-action
    :type     game-action)
   (scheduled-actions
    :initform (%make-queue)
    :initarg  :scheduled-actions
    :accessor scheduled-actions)))

(defmethod initialize-instance :after ((object action-scheduler) &key &allow-other-keys)
  (game-event:register-for-end-turn               object)
  (game-event:register-for-game-action-terminated object))

(defmethod game-event:on-game-event ((object action-scheduler) (event game-event:end-turn))
  (with-accessors ((current-action current-action)
		   (scheduled-actions scheduled-actions)) object
    (setf current-action nil)
    (setf scheduled-actions (%make-queue))))

(defmethod on-game-event ((object action-scheduler) (event game-action-terminated))
  (declare (ignore event))
  (substitute-action object))

(defgeneric enqueue-action (object new-action))

(defgeneric substitute-action (object))

(defmethod enqueue-action ((object action-scheduler) (new-action game-action))
  (with-accessors ((current-action current-action)
		   (scheduled-actions scheduled-actions)) object
    (if (not current-action)
	(progn
	  (setf current-action new-action)
	  (funcall (launch-action-fn current-action))
	(qu:q-push scheduled-actions new-action)))))

(defmethod substitute-action ((object action-scheduler))
  (with-accessors ((current-action current-action)
		   (scheduled-actions scheduled-actions)) object
    (if (qu:q-empty-p scheduled-actions)
	nil
	(progn
	  (setf current-action
		(qu:q-pop scheduled-actions))
	  (funcall (launch-action-fn current-action))))))
