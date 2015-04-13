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

(in-package euler)

(defmacro define-force (&body components)
  `(vector
    ,@(loop for comp in components collect
	   (if (numberp comp)
	       `(lambda (status) (declare (ignore status)) ,(desired comp))
	       comp))))

(defmacro define-force-single (component)
  (if (numberp component)
      `(vector
	(lambda (status) (declare (ignore status)) ,(desired component))
	(lambda (status) (declare (ignore status)) ,(desired component))
	(lambda (status) (declare (ignore status)) ,(desired component)))
      `(vector
	,@(loop for i from 0 below 3 collect
	       (misc:replace-e! component i)))))

(defclass status ()
  ((current-pos
    :initform (vec 0.0 0.0 0.0)
    :initarg  :current-pos
    :accessor current-pos)
   (velocity
    :initform (vec 0.0 0.0 0.0)
    :initarg  :velocity
    :accessor velocity)
   (acceleration
    :initform (vec 0.0 0.0 0.0)
    :initarg  :acceleration
    :accessor acceleration)
   (force
    :initform (define-force-single 0.0)
    :initarg  :force
    :accessor force)
   (mass
    :initform 1.0
    :initarg  :mass
    :accessor mass)
   (elapsed-time
    :initform 0.0
    :initarg  :elapsed-time
    :accessor elapsed-time)
   (dt
    :initform 0.0
    :initarg  :dt
    :accessor dt)))

(defgeneric integrate (object delta-t))

(defmethod integrate ((object status) delta-t)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (with-accessors ((current-pos current-pos) (accel acceleration)
		   (velo velocity) (dt dt) (force force)
		   (mass mass) (time elapsed-time)) object
    (declare ((simple-array function (3)) force))
    (declare (vec current-pos accel velo))
    (declare (desired-type dt time))
    (setf dt delta-t
	  time (d+ time delta-t))
    (setf accel (vec (d/ (funcall (elt force 0) object) mass)
		     (d/ (funcall (elt force 1) object) mass)
		     (d/ (funcall (elt force 2) object) mass))
	  velo (map 'vec #'(lambda (a v) (d+ v (d* a dt))) accel velo)
	  current-pos (map 'vec #'(lambda (p v) (d+ p (d* v dt))) current-pos velo))
    current-pos))
   
(defun test-euler ()
  (let ((status (make-instance 'status 
			       :force (define-force 0 0 #'(lambda (status)
							    (d+
							     (d* (d- 10.0) 
								 (elt (current-pos status) 2))
							     (d* (d- 1.0) 
								 (elt (velocity status) 2)))))
			       :velocity    (vec 0.0 0.0 0.0)
			       :current-pos (vec 0.0 0.0 5.0)
			       :mass        1.0)))
    (loop repeat 10000 do
	 (let ((current-pos (coerce (integrate status 0.01) 'list))
	       (time (elapsed-time status)))
	   (format t "~a ~a~%" time (nth 2 current-pos))))))

(defun test-friction ()
  (let* ((friction-coeff 0.01)
	 (starting-force (vec 0.0 0.0 1.0))
	 (friction-force (vec 0.0 0.0 1.0))
	 (status (make-instance 'status 
			       :force (define-force-single
					#'(lambda (status)
					    (declare (ignore status))
					    (let ((new-st (d- (elt starting-force :e!)
							      (d* friction-coeff
								  (elt friction-force :e!)))))
					      (setf (elt starting-force :e!) new-st)
					      new-st)))
			       :velocity    (vec 0.0 0.0 0.0)
			       :current-pos (vec 0.0 0.0 5.0)
			       :mass        1.0)))
    (misc:dbg "~a" (type-of (elt (force status) 0)))
    (loop repeat 10000 do
	 (let ((current-pos (integrate status 0.01))
	       (time (elapsed-time status)))
	   (if (d>= (elt (velocity status) 2) 0.0)
	       (format t "~a ~a ~a~%" time (elt (velocity status) 2) (elt current-pos 2)))))))

(defun test-oscillator ()
  (let* ((mass           .5)
	 (spring-k       100.0)
	 (damping-ratio  1.00)
	 (friction-coeff (d* damping-ratio 2.0 (dsqrt (d* mass spring-k))))
	 (forces         (define-force-single
			     #'(lambda (status)
				 (declare (optimize (safety 0) (speed 3) (debug 0)))
				 (with-accessors ((pos current-pos) (velocity velocity)) status
				   (declare (vec pos velocity))
				   (d+
				    (d- (d* spring-k       (elt pos      :e!)))
				    (d- (d* friction-coeff (elt velocity :e!))))))))
	 (status (make-instance 'status 
				:force forces
				:velocity (vec 0.0 0.0 0.0)
				:current-pos (vec 0.0 0.0 10.0)
				:mass mass)))

     (loop repeat 10000 do
	  (let ((current-pos (integrate status 0.01))
		(time (elapsed-time status)))
	    (format t "~a ~a ~a~%" time (elt (velocity status) 2) (elt current-pos 2))))))
