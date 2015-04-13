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

(in-package :queue)

(defparameter *queue* (misc:make-array-frame 0))

(defparameter *equal-function* #'equalp)

(defparameter *key-function* #'identity)

(defun push (val)
  (vector-push-extend val *queue*))

(defun pop ()
  (if (not (emptyp))
      (prog1
	  (alexandria:first-elt *queue*)
	(setf *queue* (misc:safe-delete@ *queue* 0)))
      nil))

(defun find (element)
  (cl:find element *queue* :key *key-function* :test *equal-function*)) 

(defun emptyp ()
  (not (> (length *queue*) 0)))

(defmacro with-queue ((equal key) &body body)
  `(let ((*queue* (misc:make-array-frame 0))
	 (*equal-function* ,equal)
	 (*key-function* ,key))
     ,@body))
