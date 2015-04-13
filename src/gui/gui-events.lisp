;; Dawn of the era: a tactical game.
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

(in-package :gui-events)

(defun mouse-button->code (code)
  (ecase code
    (1 :left)
    (2 :center)
    (3 :right)))

(defclass mouse-pressed ()
  ((x-event
    :initform 0.0
    :initarg :x-event
    :accessor x-event)
   (y-event
    :initform 0.0
    :initarg :y-event
    :accessor y-event)
   (button-event
    :initform nil
    :initarg :button-event
    :accessor button-event)))

(defmethod print-object ((object mouse-pressed) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "x ~a y ~a button ~a" (x-event object) (y-event object) (button-event object))))

(defclass mouse-dragged ()
  ((x-event
    :initform 0.0
    :initarg :x-event
    :accessor x-event)
   (y-event
    :initform 0.0
    :initarg :y-event
    :accessor y-event)
   (dx-event
    :initform 0.0
    :initarg :dx-event
    :accessor dx-event)
   (dy-event
    :initform 0.0
    :initarg :dy-event
    :accessor dy-event)
   (button-event
    :initform nil
    :initarg :button-event
    :accessor button-event)))

(defmethod print-object ((object mouse-dragged) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "x ~a y ~a dx ~a dy ~a"
	    (x-event object)  (y-event object)
	    (dx-event object) (dy-event object))))

(defclass key-pressed ()
  ((char-event
    :initform nil
    :initarg :char-event
    :accessor char-event)))

(defmethod print-object ((object mouse-dragged) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "char ~s" (char-event object))))
