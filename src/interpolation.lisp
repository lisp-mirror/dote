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

(in-package :interpolation)

(alexandria:define-constant +hermite-coeff+ (make-instance 'matrix:matrix :height 4 :width 4
							   :data #(1 0 -3  2
								   0 1 -2  1
								   0 0 -1  1
								   0 0  3 -2))
  :test #'(lambda (a b) (equalp (matrix:data a) (matrix:data b))))


(defun hermite-polynomial* (k0 k1 k2 k3 time &optional 
					      (v0 (vec/ (vec- k2 k0) 2.0))
					      (v1 (vec/ (vec- k3 k1) 2.0)))
  (let ((ctrl (gen-matrix-frame 4 3 0.0))
	(tmat (make-instance 'matrix :width 1 :height 4 
			     :data (vector 1.0 time (d* time time) (d* time time time)))))
    (loop for i from 0 below 3 do 
	 (setf (matrix-elt ctrl i 0) (elt k1 i)
	       (matrix-elt ctrl i 1) (elt v0 i)
	       (matrix-elt ctrl i 2) (elt v1 i)
	       (matrix-elt ctrl i 3) (elt k2 i)))
    (matrix-mult ctrl (matrix:matrix-mult +hermite-coeff+ tmat))))

(defun hermite-polynomial (k0 k1 time v0 v1)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec k0 k1 v0 v1))
  (declare (desired-type time))
  (let ((ctrl (gen-matrix-frame 4 3 0.0))
	(tmat (make-instance 'matrix :width 1 :height 4 
			     :data (vector 1.0 time (d* time time) (d* time time time)))))
    (declare (matrix ctrl tmat))
    (loop for i from 0 below 3 do 
	 (setf (matrix-elt ctrl i 0) (elt k0 i)
	       (matrix-elt ctrl i 1) (elt v0 i)
	       (matrix-elt ctrl i 2) (elt v1 i)
	       (matrix-elt ctrl i 3) (elt k1 i)))
    (matrix-mult ctrl (matrix:matrix-mult +hermite-coeff+ tmat))))

(defun catmul-roll-interpolation (&rest knots)
  (catmul-roll-interpolation* knots))

(defun catmul-roll-interpolation* (knots)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (list knots))
  (let* ((vs (misc:make-array-frame (1+ (length knots)) +zero-vec+ 'vec t))
	 (max-s (desired (- (length vs) 2))))
    (declare ((simple-array vec) vs))
    (declare (desired-type max-s))
    (loop for i from 1 below (- (length knots) 2) do 
	 (setf (elt vs i) (vec/ (vec- (elt knots (+ i 2)) (elt knots i)) 2.0)))
    #'(lambda (s)
	(declare (optimize (debug 0) (safety 0) (speed 3)))
	(declare (desired-type s))
	(multiple-value-bind (segment time)
	    (truncate (alexandria:clamp s 0.0 max-s))
	  (hermite-polynomial (elt knots segment) (elt knots (1+ segment))
			      time
			      (elt vs segment)
			      (elt vs (1+ segment)))))))
