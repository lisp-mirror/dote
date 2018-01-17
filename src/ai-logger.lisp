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

(in-package :ai-logger)

(define-constant +ai-log-clean-end-turn+         :end-turn       :test #'eq)

(define-constant +ai-log-clean-end-plan+         :end-plan       :test #'eq)

(define-constant +ai-log-pos-occupied+           :pos-occuped    :test #'eq)

(define-constant +ai-log-pc-entity-presence+     :entity-pc-pres :test #'eq)

(define-constant +ai-log-ai-entity-presence+     :entity-ai-pres :test #'eq)

(defstruct ai-log
  (clean-trigger +ai-log-clean-end-turn+)
  (data))

(defstruct entity-pres
  (id)
  (pos (ivec2:ivec2 -1 -1))
  (dir ivec2:+ivec2-zero+))

(defun equal-presence-p (a b)
  (and (=            (entity-pres-id  a) (entity-pres-id b))
       (ivec2:ivec2= (entity-pres-pos a) (entity-pres-pos b))
       (sb-cga:vec~  (entity-pres-dir a) (entity-pres-dir b))))

(defclass ai-logger ()
  ((logs
    :initarg :logs
    :initform (make-hash-table)
    :accessor logs
    :documentation "hastables of structure 'ai-log'")))

(defgeneric get-log (object key))

(defgeneric clean-log (object trigger))

(defmethod clean-log ((object ai-logger) trigger)
  (with-accessors ((logs logs)) object
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (eq (ai-log-clean-trigger v) trigger)
                   (setf (ai-log-data v) '())))
             logs)
    object))

(defmethod get-log ((object ai-logger) key)
  (gethash key (logs object)))
