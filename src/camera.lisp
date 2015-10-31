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

(in-package :camera)

(alexandria:define-constant +drag-camera-ends-threshold+ 0.01 :test #'=)

(defclass camera (transformable entity)
  ((target
    :initform nil
    :accessor target
    :initarg :target
    :documentation "the desired position the camera is pointing to.")
   (mode
    :initform :path
    :accessor mode
    :initarg :mode)
   (minor-mode
    :accessor minor-mode
    :initarg :minor-mode
    :initform :free)
   (pos-interpolator
    :initform #'(lambda (dt) (declare (ignorable dt)) +zero-vec+)
    :accessor pos-interpolator
    :initarg :pos-interpolator
    :documentation "a function returning next position.")
   (dir-interpolator
    :initform nil
    :accessor dir-interpolator
    :initarg :dir-interpolator
    :documentation "a function returning next direction. Nil if the Direction does not change.")
   (orbit-interpolator
    :initform #'(lambda (dt) (declare (ignorable dt)) +zero-vec+)
    :accessor orbit-interpolator
    :initarg :orbit-interpolator
    :documentation "a function returning next position and direction for an orbiting camera.")
   (current-phi
    :accessor current-phi
    :initarg :current-phi
    :initform 0.0)
   (current-theta
    :accessor current-theta
    :initarg :current-theta
    :initform 0.0)
   (current-rotation
    :accessor current-rotation
    :initarg :current-rotation
    :initform (quat 0.0 0.0 0.0 1.0))
   (previous-pos
     :accessor previous-pos
     :initarg  previous-pos
     :initform +zero-vec+)
   (drag-target-pos
     :accessor drag-target-pos
     :initarg  drag-target-pos
     :initform +zero-vec+)
   (drag-equilibrium-pos
    :accessor drag-equilibrium-pos
    :initarg  drag-equilibrium-pos
    :initform +zero-vec+)
   (drag-interpolator
    :accessor drag-interpolator
    :initarg  :drag-interpolator
    :initform nil)
   (frustum-far
    :accessor frustum-far
    :initarg  :frustum-far
    :initform 0.0)
   (frustum-near
    :accessor frustum-near
    :initarg  :frustum-near
    :initform 0.0)
   (frustum-fov
    :accessor frustum-fov
    :initarg  :frustum-fov
    :initform 0.0)
   (frustum-planes
    :accessor frustum-planes
    :initarg  :frustum-planes
    :initform (misc:make-fresh-array 6 (vec4:vec4 0.0 0.0 0.0 0.0) 'vec4:vec4 t))
   (frustum-h-near
    :accessor frustum-h-near
    :initarg  :frustum-h-near
    :initform 0.0)
   (frustum-w-near
    :accessor frustum-w-near
    :initarg  :frustum-w-near
    :initform 0.0)
   (frustum-h-far
    :accessor frustum-h-far
    :initarg  :frustum-h-far
    :initform 0.0)
   (frustum-w-far
    :accessor frustum-w-far
    :initarg  :frustum-w-far
    :initform 0.0)
   (frustum-aabb
    :accessor frustum-aabb
    :initarg  :frustum-aabb
    :initform (make-instance '3d-utils:aabb))))

(defmethod game-state:current-time ((object camera))
  (game-state:current-time (state object)))

(defun gen-dir-interpolator (from-axe to-axe  angular-velocity)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (let* ((angle   (dacos (dot-product from-axe to-axe)))
	 (rot-axe (normalize (cross-product from-axe to-axe)))
	 (q1   (quat-norm (axis-rad->quat rot-axe 0.0)))
	 (q2   (quat-norm (axis-rad->quat rot-axe angle)))
	 (time (desired 0.0)))
    (declare (desired-type time))
    #'(lambda (dt)
	(declare (optimize (safety 0) (speed 3) (debug 0)))
	(let* ((int-q (quat-slerp q1 q2 (min time 1.0))))
	  (incf time (d* dt angular-velocity))
	  (quat-rotate-vec int-q from-axe)))))

(defmethod initialize-instance :after ((object camera) &key &allow-other-keys)
  (with-accessors ((pos pos) (target target) (up up) (dir dir)
		   (orbit-interpolator orbit-interpolator)
		   (dir-interpolator dir-interpolator)
		   (current-force current-force)
		   (drag-interpolator drag-interpolator)) object
    (setf (id object)     +id-camera+
	  (pos    object)          (vec  13.0 8.0 10.0)
	  (target object)          (vec  0.0 0.0 -1.0)
	  ;; really needed?
	  (previous-pos object)    (copy-vec pos)
	  (drag-target-pos object) (copy-vec target)
	  (up     object) (copy-vec +y-axe+)
	  (dir    object) (vec- (target object) (pos object)))
    (game-event:register-for-camera-drag-ends object)
    (look-at* object)))

(defmethod render ((object camera) renderer))

(defmethod calculate ((object camera) dt)
  (case (mode object)
    (:path
     (%draw-path-mode object dt))
    (:orbit
     (%draw-orbit-mode object dt))
    (:drag
     (%draw-drag-mode object dt))
    (:otherwise
     t)))

(defmethod on-game-event ((object camera) (event game-event:camera-drag-ends))
  (setf (mode object) :fp)
  nil)

(defgeneric %draw-path-mode (object dt))

(defgeneric %draw-orbit-mode (object dt))

(defgeneric %draw-drag-mode (object dt))

(defgeneric look-at* (object))

(defgeneric look-at (object eye-x eye-y eye-z target-x target-y target-z up-x up-y up-z))

(defgeneric orbit (object phi theta dist))

(defgeneric reorient-fp-camera (object offset))

(defgeneric actual-up-vector (object))

(defgeneric drag-camera (object offset))

(defgeneric install-path-interpolator (object &rest knots))

(defgeneric install-path-interpolator* (object knots))

(defgeneric install-orbit-interpolator (object phi-angular-velocity theta-angular-velocity dist))

(defgeneric install-drag-interpolator (object &key spring-k))

(defgeneric calculate-frustum (object))

(defgeneric calculate-aabb    (object))

(defgeneric containsp (object p))

(defmethod %draw-path-mode ((object camera) dt)
  (with-accessors ((target target) (pos pos)) object
    (setf pos (funcall (pos-interpolator object) dt))
    (when (dir-interpolator object)
      (let ((new-dir (funcall (dir-interpolator object) dt)))
	(setf target (vec+ (vec* new-dir (vec-length target)) pos))))
    (look-at* object)))

(defmethod %draw-orbit-mode ((object camera) dt)
  (multiple-value-bind (pos dir)
      (funcall (orbit-interpolator object) dt)
    (setf (pos object) pos
	  (dir object) dir)
    (look-at* object)))

(defmethod %draw-drag-mode ((object camera) dt)
  (with-accessors ((target target)
		   (pos pos)
		   (drag-interpolator drag-interpolator)
		   (current-force current-force)
		   (previous-pos previous-pos)
		   (drag-target-pos drag-target-pos)
		   (drag-equilibrium-pos drag-equilibrium-pos)) object
    (let* ((offset  (integrate drag-interpolator dt)))
      (setf pos               (vec+ previous-pos (vec- drag-equilibrium-pos offset))
	    target            (vec+ drag-target-pos (vec- drag-equilibrium-pos offset)))
      (look-at* object)
      (when (vec~ offset +zero-vec+ +drag-camera-ends-threshold+)
	(game-event:propagate-camera-drag-ends (make-instance 'game-event:camera-drag-ends))))))

(defmethod look-at* ((object camera))
  (setf (dir object) (normalize (vec- (target object) (pos object))))
  (setf (view-matrix object) (sb-cga-utils:look@ (pos object) (target object) (up object))))

(defmethod look-at ((object camera)
		    eye-x eye-y eye-z
		    target-x target-y target-z
		    up-x up-y up-z)
  (setf (pos    object) (vec  eye-x eye-y eye-z)
	(target object) (vec  target-x target-y target-z)
	(up     object) (normalize (vec  up-x up-y up-z))
	(dir    object) (normalize (vec- (target object) (pos object))))
  (look-at* object))

(defmethod orbit ((object camera) phi theta dist)
   "Orbits the camera around its target by phi (horizontal axis) and theta
    (vertical axis) at a radius dist"
   (with-accessors ((up up) (dir dir) (target target) (pos pos)) object
     (setf pos +zero-vec+)
     (let* ((x-axis  (normalize (cross-product up (vec- target pos))))
	    (y-axis  up)
	    (quat    (quat* (axis-rad->quat x-axis theta)
			    (axis-rad->quat y-axis phi)))
	    (new-dir (normalize (quat-rotate-vec quat dir)))
	    (new-pos (vec+ target (vec- +zero-vec+ (vec* new-dir dist)))))
       new-pos)))

(defmethod reorient-fp-camera ((object camera) offset)
  (let ((offset-scaled (vec2:vec2* offset 0.01)))
     (with-accessors ((dir dir) (up up) (pos pos)
		      (target target)
		      (current-rotation current-rotation)) object
       (let* ((qy           (axis-rad->quat +y-axe+ (elt offset-scaled 0)))
	      (qx           (axis-rad->quat (cross-product dir up) (elt offset-scaled 1)))
	      (qxy          (quat* qy qx))
	      (interpolated (quat-slerp current-rotation qxy 0.1))
	      (new-dir      (quat-rotate-vec interpolated dir)))
	 (setf target (vec+ (vec* new-dir (vec-length target)) pos))
	 (look-at* object)))))

(defmethod actual-up-vector ((object camera))
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (camera object))
  (with-accessors ((dir dir) (up up)) object
    (declare (vec dir up))
    (let* ((f (normalize dir))
	   (s (normalize (cross-product f up)))
	   (u (cross-product s f)))
      u)))

(defun %calc-push-y (camera up dir offset)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (camera camera))
  (declare (vec up dir))
  (declare (desired-type offset))
  (let* ((u      (actual-up-vector camera))
	 (push-y (vec* (normalize (map 'vec
				       #'(lambda (a b) (dlerp (dot-product up +y-axe+) a b))
				       u dir))
		       offset)))
    (declare (vec u push-y))
    (setf (elt push-y 1) 0.0)
    push-y))

(defmethod drag-camera ((object camera) (offset vector))
  "move camera on the x-z plane to position specified by vector"
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (camera object))
  (declare (vec offset))
  (with-accessors ((dir dir) (up up)
		   (pos pos)
		   (target target)
		   (previous-pos previous-pos)
		   (drag-interpolator drag-interpolator)
		   (drag-target-pos drag-target-pos)
		   (drag-equilibrium-pos drag-equilibrium-pos)) object
    (let* ((u      (actual-up-vector object))
	   (push-y (%calc-push-y object u dir (elt offset 2)))
	   (push-x (vec* (normalize (cross-product dir up)) (elt offset 0)))
	   (push   (vec+ (vec+ push-x push-y)
			 (vec* (vec-negate dir) (elt offset 1)))))
      (setf (current-pos drag-interpolator) push
	    drag-equilibrium-pos            push
	    previous-pos                    pos
	    drag-target-pos                 target))))

(defun gen-path-interpolator* (knots)
  (let* ((interpolator (interpolation:catmul-roll-interpolation* knots))
	 (time 0.0))
    #'(lambda (dt)
	(let ((res (funcall interpolator (incf time dt))))
	  (vec (matrix:matrix-elt res 0 0)
	       (matrix:matrix-elt res 1 0)
	       (matrix:matrix-elt res 2 0))))))

(defun gen-path-interpolator (&rest knots)
  (gen-path-interpolator* knots))

(defmethod install-path-interpolator* ((object camera) (knots sequence))
  (setf (pos-interpolator object) (gen-path-interpolator* knots)))

(defmethod install-path-interpolator ((object camera) &rest knots)
  (setf (pos-interpolator object) (gen-path-interpolator* knots)))

(defun gen-drag-interpolator (&key (spring-k 100.0))
  (let* ((mass           .05)
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
				:current-pos (vec 0.0 0.0 0.0)
				:mass mass)))
    status))

(defmethod install-drag-interpolator ((object camera) &key (spring-k 100.0))
  (setf (drag-interpolator object) (gen-drag-interpolator :spring-k spring-k)))

(defun gen-orbit-interpolator (camera phi-angular-velocity theta-angular-velocity dist)
  #'(lambda (dt) (with-accessors ((current-theta current-theta)
				  (current-phi   current-phi)) camera
		   (setf current-phi (d* dt phi-angular-velocity))
		   (setf current-theta (d* dt theta-angular-velocity))
		   (orbit camera current-phi current-theta dist))))

(defmethod install-orbit-interpolator ((object camera) phi-angular-velocity
				       theta-angular-velocity dist)
  (setf (orbit-interpolator object)
	(gen-orbit-interpolator object phi-angular-velocity theta-angular-velocity dist)))

(defmethod calculate-frustum ((object camera))
  (with-accessors ((frustum-planes frustum-planes)
		   (view-matrix view-matrix)
		   (projection-matrix projection-matrix)) object
    (let ((projection-view-matrix (matrix* (elt projection-matrix 0)
					   (elt view-matrix 0))))
      (extract-frustum-plane (elt frustum-planes 0) projection-view-matrix  1)
      (extract-frustum-plane (elt frustum-planes 1) projection-view-matrix -1)
      (extract-frustum-plane (elt frustum-planes 2) projection-view-matrix  2)
      (extract-frustum-plane (elt frustum-planes 3) projection-view-matrix -2)
      (extract-frustum-plane (elt frustum-planes 4) projection-view-matrix  3)
      (extract-frustum-plane (elt frustum-planes 5) projection-view-matrix -3))))

(defmethod build-projection-matrix ((object camera) near far fov ratio)
  (setf (frustum-fov object)  fov
	(frustum-near object) near
	(frustum-far object)  far)
  (let ((fov-radians (deg->rad fov)))
    (setf (frustum-h-near object) (d* 2.0 (tan (d/ fov-radians 2.0))  near))
    (setf (frustum-w-near object) (d* (frustum-h-near object) ratio))
    (setf (frustum-h-far  object) (d* 2.0 (tan (d/ fov-radians 2.0))  far))
    (setf (frustum-w-near object) (d* (frustum-h-far object) ratio)))
  (setf (projection-matrix object) (perspective fov ratio near far)))

(defmethod calculate-aabb ((object camera))
  (with-accessors ((up up) (dir dir) (target target) (pos pos)
		   (frustum-far frustum-far) (frustum-near frustum-near)
		   (frustum-h-near frustum-h-near)
		   (frustum-w-near frustum-w-near)
		   (frustum-h-far  frustum-h-far)
		   (frustum-w-far frustum-w-far)
		   (frustum-aabb frustum-aabb)) object
    (let* ((side         (normalize (cross-product dir up)))
	   (far-center   (vec+ pos (vec* (normalize dir) frustum-far)))
	   (near-center  (vec+ pos (vec* (normalize dir) frustum-near)))
	   (hfar/2       (d/ frustum-h-far 2.0))
	   (wfar/2       (d/ frustum-w-far 2.0))
	   (hnear/2      (d/ frustum-h-near 2.0))
	   (wnear/2      (d/ frustum-w-near 2.0))
	   (far-top-left  (vec- (vec+ far-center (vec* up hfar/2))
				(vec* side wfar/2)))
	   (far-top-right (vec+ (vec+ far-center (vec* up hfar/2))
				(vec* side wfar/2)))
	   (far-bottom-left  (vec- (vec- far-center (vec* up hfar/2))
				   (vec* side wfar/2)))
	   (far-bottom-right (vec+ (vec- far-center (vec* up hfar/2))
				   (vec* side wfar/2)))
	   (near-top-left  (vec- (vec+ near-center (vec* up hnear/2))
				 (vec* side wnear/2)))
	   (near-top-right (vec+ (vec+ near-center (vec* up hnear/2))
				 (vec* side wnear/2)))
	   (near-bottom-left  (vec- (vec- near-center (vec* up hnear/2))
				   (vec* side wnear/2)))
	   (near-bottom-right (vec+ (vec- near-center (vec* up hnear/2))
				    (vec* side wnear/2)))
	   (aabb (make-instance 'aabb)))
      (expand aabb far-top-left)
      (expand aabb far-top-right)
      (expand aabb far-bottom-left)
      (expand aabb far-bottom-right)
      (expand aabb near-top-left)
      (expand aabb near-top-right)
      (expand aabb near-bottom-left)
      (expand aabb near-bottom-right)
      (setf frustum-aabb aabb)
      object)))

(defmethod aabb-2d ((object camera))
  (flatten-to-aabb2-xz (frustum-aabb object)))

(defmethod containsp ((object camera) (p vector))
  (with-accessors ((frustum-planes frustum-planes)) object
    (loop for plane across frustum-planes do
	 (when (not (plane-point-same-side-p plane p))
	     (return-from containsp nil)))
    t))
