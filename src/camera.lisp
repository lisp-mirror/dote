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

(alexandria:define-constant +drag-camera-ends-threshold+   0.1             :test #'=)

(alexandria:define-constant +fit-to-aabb-offset+           0.01            :test #'=)

(alexandria:define-constant +fit-to-aabb-scale-pos+        2.0             :test #'=)

(alexandria:define-constant +angular-speed-level-up+       (deg->rad  30.0) :test #'=)

(alexandria:define-constant +angular-speed-rotate-command+ (deg->rad 150.0) :test #'=)

(alexandria:define-constant +reset-camera-vec+   (vec (d* +terrain-chunk-tile-size+ -8.0)
                                                      (d* +terrain-chunk-tile-size+ 15.0)
                                                      (d* +terrain-chunk-tile-size+ -8.0))
  :test #'vec=)

(defclass camera (transformable entity fading-away-entity)
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
    :documentation "a function returning next position for an orbiting camera
                    around (x-target y-axis z-target), nil if no more rotation will occurs.")
   (current-phi
    :accessor current-phi
    :initarg :current-phi
    :initform 0.0
    :documentation "angle of rotation around z axis")
   (current-theta
    :accessor current-theta
    :initarg :current-theta
    :initform 0.0
    :documentation "angle of rotation around x axis")
   (current-rotation
    :accessor current-rotation
    :initarg :current-rotation
    :initform (quat 0.0 0.0 0.0 1.0))
   (previous-pos
     :accessor previous-pos
     :initarg  :previous-pos
     :initform +zero-vec+)
   (followed-entity
     :accessor followed-entity
     :initarg  :followed-entity
     :initform nil)
   (drag-target-pos
     :accessor drag-target-pos
     :initarg  :drag-target-pos
     :initform +zero-vec+)
   (drag-equilibrium-pos
    :accessor drag-equilibrium-pos
    :initarg  :drag-equilibrium-pos
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
    :initform (make-instance '3d-utils:aabb))
   (frustum-sphere
    :accessor frustum-sphere
    :initarg  :frustum-sphere
    :initform (make-instance 'bounding-sphere))
   (frustum-cone
    :accessor frustum-cone
    :initarg  :frustum-cone
    :initform (make-instance 'cone))))

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
    (game-event:register-for-camera-drag-ends  object)
    (game-event:register-for-camera-orbit-ends object)
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
    (:follow
     (%draw-follow-mode object dt))
    (otherwise
     (look-at* object))))

(defmethod on-game-event ((object camera) (event game-event:camera-drag-ends))
  (setf (mode object) :fp)
  nil)

(defmethod on-game-event ((object camera) (event game-event:camera-orbit-ends))
  (setf (mode object) :fp)
  nil)

(defgeneric %draw-path-mode (object dt))

(defgeneric %draw-orbit-mode (object dt))

(defgeneric %draw-drag-mode (object dt))

(defgeneric %draw-follow-mode (object dt))

(defgeneric look-at* (object))

(defgeneric look-at (object eye-x eye-y eye-z target-x target-y target-z up-x up-y up-z))

(defgeneric reorient-fp-camera (object offset))

(defgeneric actual-up-vector (object))

(defgeneric drag-camera (object offset))

(defgeneric drag-camera-to (object destination))

(defgeneric install-path-interpolator (object &rest knots))

(defgeneric install-path-interpolator* (object knots))

(defgeneric install-orbit-interpolator (object pivot angular-velocity max-angle))

(defgeneric install-drag-interpolator (object &key spring-k))

(defgeneric calculate-frustum (object))

(defgeneric calculate-aabb    (object))

(defgeneric calculate-cone    (object))

(defgeneric calculate-sphere  (object))

(defgeneric containsp (object p))

(defgeneric fit-to-aabb (object aabb))

(defmethod %draw-path-mode ((object camera) dt)
  (with-accessors ((target target) (pos pos)) object
    (setf pos (funcall (pos-interpolator object) dt))
    (when (dir-interpolator object)
      (let ((new-dir (funcall (dir-interpolator object) dt)))
        (setf target (vec+ (vec* new-dir (vec-length target)) pos))))
    (look-at* object)))

(defmethod %draw-orbit-mode ((object camera) dt)
  (with-accessors ((pos pos)) object
    (let ((new-pos (funcall (orbit-interpolator object) dt)))
      (if new-pos ;; when nil rotation is complete
          (progn
            (setf pos new-pos)
            (look-at* object))
          (let ((event (make-instance 'game-event:camera-drag-ends)))
            (game-event:propagate-camera-orbit-ends event))))))

(defmethod %draw-drag-mode ((object camera) dt)
  (with-accessors ((target target)
                   (pos pos)
                   (drag-interpolator drag-interpolator)
                   (current-force current-force)
                   (previous-pos previous-pos)
                   (drag-target-pos drag-target-pos)
                   (drag-equilibrium-pos drag-equilibrium-pos)) object
    (let* ((offset  (integrate drag-interpolator dt)))
      (setf pos               (vec+ previous-pos    (vec- drag-equilibrium-pos offset))
            target            (vec+ drag-target-pos (vec- drag-equilibrium-pos offset)))
      (look-at* object)
      (when (vec~ offset +zero-vec+ +drag-camera-ends-threshold+)
        ;;(setf (euler:elapsed-time (drag-interpolator object)) 0.0)
        (game-event:propagate-camera-drag-ends (make-instance 'game-event:camera-drag-ends))))))

(defmethod %draw-follow-mode ((object camera) dt)
  (with-accessors ((target target)
                   (pos pos)
                   (followed-entity followed-entity)) object
    (when followed-entity
      (let* ((pos-saved    (copy-vec pos))
             (pos-followed (pos      followed-entity)))
        (setf pos (vec (d- (elt  pos-followed 0) (d* +terrain-chunk-tile-size+ 2.0))
                       (elt pos-saved 1)
                       (d- (elt pos-followed 2)  (d* +terrain-chunk-tile-size+ 2.0)))
              target   pos-followed)
        (look-at* object)))))

(defmethod look-at* ((object camera))
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (with-accessors ((up  up)
                   (pos pos)
                   (dir dir)
                   (target target)
                   (fading-away-fn fading-away-fn)) object
    (declare (function fading-away-fn))
    (let ((tremor-matrix      (funcall fading-away-fn object 0.033))
          (standard-vw-matrix (sb-cga-utils:look@ pos target up)))
      (declare (sb-cga:matrix tremor-matrix standard-vw-matrix))
      (setf dir (normalize (vec- target pos)))
      (setf (view-matrix object) (matrix* standard-vw-matrix tremor-matrix)))))

(defmethod look-at ((object camera)
                    eye-x eye-y eye-z
                    target-x target-y target-z
                    up-x up-y up-z)
  (setf (pos    object) (vec  eye-x eye-y eye-z)
        (target object) (vec  target-x target-y target-z)
        (up     object) (normalize (vec  up-x up-y up-z))
        (dir    object) (normalize (vec- (target object) (pos object))))
  (look-at* object))

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
  "move camera on the x-z plane to position specified by vector as offset"
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

(defmethod drag-camera-to ((object camera) (destination vector))
  "move camera on the x-z plane to position specified by destination"
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (camera object))
  (with-accessors ((dir dir) (up up)
                   (pos pos)
                   (target target)
                   (previous-pos previous-pos)
                   (drag-interpolator drag-interpolator)
                   (drag-target-pos drag-target-pos)
                   (drag-equilibrium-pos drag-equilibrium-pos)) object

    (let* ((offset (vec- destination pos)))
      (setf (current-pos drag-interpolator) offset
            drag-equilibrium-pos            offset
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

(defmethod reset-camera-view ((object camera) (point-to vector))
  (with-accessors ((target target)
                   (pos    pos)) object
    (setf target point-to)
    (setf pos    (vec+ point-to +reset-camera-vec+))
    (look-at* object)
    object))

(defun gen-orbit-interpolator-cw (camera pivot angular-velocity max-angle)
  (let ((current-angle 0.0))
    #'(lambda (dt)
        (if (d< current-angle max-angle)
            (with-accessors ((target target)
                             (pos    pos)) camera
              (let* ((dir         (vec- pos pivot)) ; pointing from target
                                                     ; to position of the camera
                     (rot-vector  +y-axe+)
                     (rot-matrix  (rotate-around rot-vector (d* angular-velocity dt)))
                     (rotated-dir (transform-direction dir rot-matrix))
                     (new-pos     (vec+ target rotated-dir)))
                (incf current-angle (d* angular-velocity dt))
                new-pos))
            nil))))

(defun gen-orbit-interpolator-ccw (camera pivot angular-velocity max-angle)
  (let ((current-angle max-angle))
    #'(lambda (dt)
        (if (d> current-angle 0.0)
            (with-accessors ((target target)
                             (pos    pos)) camera
              (let* ((dir         (vec- pos pivot)) ; pointing from target
                                                     ; to position of the camera
                     (rot-vector  +y-axe+)
                     (rot-matrix  (rotate-around rot-vector (d- (d* angular-velocity dt))))
                     (rotated-dir (transform-direction dir rot-matrix))
                     (new-pos     (vec+ target rotated-dir)))
                (decf current-angle (d* angular-velocity dt))
                new-pos))
            nil))))

(defmethod install-orbit-interpolator ((object camera) pivot angular-velocity max-angle)
  "x and y are screen coordinates"
  (setf (orbit-interpolator object)
        (gen-orbit-interpolator-cw object pivot angular-velocity max-angle)))

(defmethod calculate-frustum ((object camera))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-accessors ((frustum-planes frustum-planes)
                   (view-matrix view-matrix)
                   (projection-matrix projection-matrix)) object
    (declare ((simple-array vec4:vec4 (6)) frustum-planes))
    (declare ((simple-array sb-cga:matrix (1)) projection-matrix view-matrix))
    (let ((projection-view-matrix (matrix* (elt projection-matrix 0)
                                           (elt view-matrix 0))))
      (extract-frustum-plane (elt frustum-planes 0) projection-view-matrix  1)
      (extract-frustum-plane (elt frustum-planes 1) projection-view-matrix -1)
      (extract-frustum-plane (elt frustum-planes 2) projection-view-matrix  2)
      (extract-frustum-plane (elt frustum-planes 3) projection-view-matrix -2)
      (extract-frustum-plane (elt frustum-planes 4) projection-view-matrix  3)
      (extract-frustum-plane (elt frustum-planes 5) projection-view-matrix -3)
      object)))

(defmethod build-projection-matrix ((object camera) near far fov ratio)
  (setf (frustum-fov object)  fov
        (frustum-near object) near
        (frustum-far object)  far)
  (let ((fov-radians (deg->rad fov)))
    (setf (frustum-h-near object) (d* 2.0 (tan (d/ fov-radians 2.0)) near))
    (setf (frustum-w-near object) (d* (frustum-h-near object) ratio))
    (setf (frustum-h-far  object) (d* 2.0 (tan (d/ fov-radians 2.0)) far))
    (setf (frustum-w-far  object) (d* (frustum-h-far object) ratio)))
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

(defmethod calculate-cone ((object camera))
  (with-accessors ((up up) (dir dir) (target target) (pos pos)
                   (frustum-far frustum-far) (frustum-near frustum-near)
                   (frustum-h-near frustum-h-near)
                   (frustum-w-near frustum-w-near)
                   (frustum-h-far  frustum-h-far)
                   (frustum-w-far frustum-w-far)
                   (frustum-cone frustum-cone)) object
    (let* ((side         (normalize (cross-product dir up)))
           (far-center   (vec+ pos (vec* (normalize dir) frustum-far)))
           (near-center  (vec+ pos (vec* (normalize dir) frustum-near)))
           (height       (vec- far-center near-center))
           (hfar/2       (d/ frustum-h-far 2.0))
           (wfar/2       (d/ frustum-w-far 2.0))
           (far-top-left (vec- (vec+ far-center (vec* up hfar/2))
                               (vec* side wfar/2)))
           (directrix    (vec- far-top-left pos))
           (cone         (make-instance 'cone
                                        :cone-apex   pos
                                        :cone-height height
                                        :half-angle  (acos (dot-product (normalize directrix)
                                                                        dir)))))
      (setf frustum-cone cone)
      object)))

(defmethod calculate-sphere ((object camera))
  (with-accessors ((up up) (dir dir) (target target) (pos pos)
                   (frustum-far frustum-far) (frustum-near frustum-near)
                   (frustum-h-near frustum-h-near)
                   (frustum-w-near frustum-w-near)
                   (frustum-h-far  frustum-h-far)
                   (frustum-w-far  frustum-w-far)
                   (frustum-sphere frustum-sphere)) object
    (let* ((side         (normalize (cross-product dir up)))
           (far-center   (vec+ pos (vec* (normalize dir) frustum-far)))
           (near-center  (vec+ pos (vec* (normalize dir) frustum-near)))
           (height       (vec- far-center near-center))
           (hfar/2       (d/ frustum-h-far 2.0))
           (wfar/2       (d/ frustum-w-far 2.0))
           (far-top-left (vec- (vec+ far-center (vec* up hfar/2))
                               (vec* side wfar/2)))
           (sphere       (make-instance 'bounding-sphere
                                        :sphere-radius (vec-length (vec- far-top-left
                                                                         (vec* height 0.5)))
                                        :sphere-center (vec* height 0.5))))
      (setf frustum-sphere sphere)
      object)))

(defmethod aabb-2d ((object camera))
  (flatten-to-aabb2-xz (frustum-aabb object)))

(defmethod containsp ((object camera) (p vector))
  (with-accessors ((frustum-planes frustum-planes)) object
    (loop for plane across frustum-planes do
         (when (not (plane-point-same-side-p plane p))
             (return-from containsp nil)))
    t))

(defmethod fit-to-aabb ((object camera) aabb)
  (with-accessors ((pos pos)
                   (dir dir)
                   (target target)) object
    (setf pos (vec (elt pos 0) (elt (3d-utils:aabb-center aabb) 1) (elt pos 2)))
    (setf target (vec* pos +fit-to-aabb-scale-pos+))
    (camera:look-at* object)
    (do* ((scale 0.0 (d+ scale +fit-to-aabb-offset+))
          (offset  (vec-negate dir)
                   (vec* (vec-negate offset) scale))
          (updated-camera (camera:calculate-frustum object)
                          (camera:calculate-frustum updated-camera)))
         ((and (camera:containsp updated-camera
                                 (3d-utils:aabb-p2 aabb))
               (camera:containsp updated-camera
                                 (3d-utils:aabb-p1 aabb))))
      (setf pos (vec+ pos offset))
      (camera:look-at* updated-camera)
     object)))
