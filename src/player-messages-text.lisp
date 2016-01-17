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

(in-package :player-messages-text)

(defparameter *terror-recover*          nil)

(defparameter *berserk-recover*         nil)

(defparameter *faint-recover*           nil)

(defparameter *cancel-immune-berserk*   nil)

(defparameter *cancel-immune-faint*     nil)

(defparameter *cancel-immune-terror*    nil)

(defparameter *cancel-immune-poisoning* nil)

(defun init-player-messages-db ()
  (setf *terror-recover*          (list
				   (_ "I am fully recovered from terror now.")))
  (setf *berserk-recover*         (list
				   (_ "W-what happened?")
				   (_ "What have i done?")))
  (setf *faint-recover*           (list
				   (_ "I am ready for battle again!")))
  (setf *cancel-immune-berserk*   (list
				   (_ "I am immune to berserk no more")))
  (setf *cancel-immune-faint*     (list
				   (_ "I am immune to faint no more")))
  (setf *cancel-immune-terror*    (list
				   (_ "I am immune to terror no more")))
  (setf *cancel-immune-poisoning* (list
				   (_ "I am immune to poisoning no more"))))
