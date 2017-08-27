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

(in-package :static-queue)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (alexandria:define-constant +queue-size+ 65536 :test #'=))

(alexandria:define-constant +queue-pick-random+ t :test #'eq)

(defparameter *queue* (make-array +queue-size+
                                  :fill-pointer    nil
                                  :adjustable      nil
                                  :initial-element nil
                                  :element-type      t))

(defparameter *queue-idx* 0)

(defstruct static-queue
  (container (make-array +queue-size+
                         :fill-pointer    nil
                         :adjustable      nil
                         :initial-element nil
                         :element-type      t))
  (idx       0))

(defun make-queue (initial-element)
  (make-static-queue :container (make-array +queue-size+
                                            :fill-pointer    nil
                                            :adjustable      nil
                                            :initial-element initial-element
                                            :element-type    (type-of initial-element))
                     :idx 0))

(defun %delete@ (queue position)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (static-queue queue))
  (declare (fixnum position))
  (let ((sequence (static-queue-container queue)))
    (declare ((simple-vector 65536) sequence))
    (rotatef (aref sequence position)
             (aref sequence (1- (the fixnum (static-queue-idx queue)))))
    (max 0 (decf (the fixnum (static-queue-idx queue))))))

(defun qpush (queue val)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (setf (aref (the (simple-vector 65536) (static-queue-container queue))
              (static-queue-idx queue))
        val)
  (incf (the fixnum (static-queue-idx queue))))

(defmacro qpick (queue)
  (if +queue-pick-random+
      `(random (the fixnum (static-queue-idx ,queue)))
      0))

(defun qpop (queue)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (if (not (qemptyp queue))
      (let ((elt-pos (qpick queue)))
        (prog1
            (aref (the (simple-vector 65536) (static-queue-container queue)) elt-pos)
          (%delete@ queue elt-pos)))
      nil))

(defun qemptyp (queue)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (= (the fixnum (static-queue-idx queue)) 0))

(defun qfind (element queue key-function equal-function)
  (cl:find element (static-queue-container queue)
           :key   key-function
           :test  equal-function
           :start 0
           :end   (1- (static-queue-idx queue))))
