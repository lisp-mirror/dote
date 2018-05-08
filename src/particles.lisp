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

(in-package :particles)

(alexandria:define-constant +attribute-position-location+        0                  :test #'=)

(alexandria:define-constant +attribute-mass-location+            1                  :test #'=)

(alexandria:define-constant +attribute-v0-location+              2                  :test #'=)

(alexandria:define-constant +attribute-delay-feedback-location+  3                  :test #'=)

(alexandria:define-constant +attribute-force-feedback-location+  4                  :test #'=)

(alexandria:define-constant +attribute-center-pos-location+      1                  :test #'=)

(alexandria:define-constant +attribute-delay-location+           3                  :test #'=)

(alexandria:define-constant +attribute-life-location+            4                  :test #'=)

(alexandria:define-constant +attribute-scaling-location+         5                  :test #'=)

(alexandria:define-constant +attribute-alpha-location+           6                  :test #'=)

(alexandria:define-constant +attribute-rotation-location+        7                  :test #'=)

(alexandria:define-constant +attribute-color-location+           8                  :test #'=)

(alexandria:define-constant +transform-vbo-count+                9                  :test #'=)

(alexandria:define-constant +appended-vbo-count+                 7                  :test #'=)

(alexandria:define-constant +particle-gravity+                   (vec 0.0 -9.0 0.0) :test #'vec~)

(defun %constant-color-clsr (color)
  #'(lambda ()
      #'(lambda (particle dt)
          (declare (ignore particle dt))
          color)))

(defun %gradient-color-clsr (gradient scale)
  #'(lambda ()
      (let ((time 0.0))
        #'(lambda (particle dt)
            (declare (ignore particle))
            (prog1
                (color-utils:pick-color gradient (d* time scale))
              (incf time dt))))))

(defun %smooth-gradient-color-clsr (gradient duration)
  #'(lambda ()
      (let ((time 0.0))
        #'(lambda (particle dt)
            (declare (ignore particle))
            (prog1
                (color-utils:pick-color gradient
                                        (dlerp (smoothstep-interpolate 0 duration time)
                                               0.0 1.0))
              (incf time dt))))))

(defgeneric %uniform-rotation-clsr (m))

(defmethod %uniform-rotation-clsr ((m number))
  #'(lambda ()
      (let ((time 0.0))
        #'(lambda (p dt)
            (declare (ignore p))
            (incf time dt)
            (d+ 1.0 (d* m time))))))

(defmethod %uniform-rotation-clsr ((m function))
  #'(lambda ()
      (let ((time  0.0)
            (coeff (funcall m)))
        #'(lambda (p dt)
            (declare (ignore p))
            (incf time dt)
            (d+ 1.0 (d* coeff time))))))

(defun %no-rotation-clrs ()
  #'(lambda ()
      #'(lambda (p dt)
          (declare (ignore p dt))
          0.0)))

(defun %uniform-scaling-clsr (m)
  #'(lambda ()
      (let ((time 0.0))
        #'(lambda (p dt)
            (declare (ignore p))
            (incf time dt)
            (dmax 1e-6
                  (d+ 1.0 (d* m time)))))))

(defun %limited-scaling-clsr (rate max)
  #'(lambda ()
      (let ((time 0.0))
        #'(lambda (p dt)
            (declare (ignore p))
              (incf time dt)
              (num:enzyme-kinetics max rate time)))))

(defun %no-scaling-clsr ()
  #'(lambda ()
      #'(lambda (p dt)
          (declare (ignore p dt))
          1.0)))

(defun %smooth-alpha-fading-clsr (max)
  #'(lambda ()
      (let ((time 0.0))
        #'(lambda (p dt)
            (declare (ignore p))
            (incf time dt)
            (d- 1.0 (dlerp (smoothstep-interpolate 0.0 max time) 0.0 1.0))))))

(defun %exp-alpha-fading-clsr (m)
  #'(lambda ()
      (let ((time 0.0))
        #'(lambda (p dt)
            (declare (ignore p))
            (incf time dt)
            (max 0.0 (d+ 1.0 (d* (d- m) (dexpt time 2.0))))))))

(defun %sin-alpha-fading-clsr (freq)
  #'(lambda ()
      (let ((time 0.0))
        #'(lambda (p dt)
            (declare (ignore p))
            (incf time dt)
            (dsin (d* freq time))))))

(defun %uniform-from-clsr (lower-treshold starting-time m)
  #'(lambda ()
      (let ((time        0.0)
            (actual-time 0.0))
        #'(lambda (p dt)
            (declare (ignore p))
            (incf time dt)
            (if (d> time starting-time)
                (prog1
                    (d+ lower-treshold (d* m actual-time))
                  (incf actual-time dt))
                (d+ lower-treshold (d* (d* m 10.0) time)))))))

(defun gaussian-velocity-distribution-fn (dir max delta max-opening)
  "Assuming dir normalized"
  (assert (let ((*default-epsilon* 1e-3))
            (epsilon= 1.0 (vec-length dir))))
  #'(lambda ()
      (let* ((scale (gaussian-probability delta max))
             (rotation-y (rotate-around +y-axe+
                                        (num:lcg-next-upto (d* 2.0 +pi+))))
             (rotation-x (rotate-around +x-axe+
                                        (num:lcg-next-upto max-opening)))
             (reorient-axe      (if (vec~ (cross-product dir +y-axe+) +zero-vec+)
                                    +y-axe+
                                    (cross-product +y-axe+ dir)))
             (reorient-angle    (acos (dot-product +y-axe+ dir)))
             (rotation-reorient (rotate-around reorient-axe reorient-angle)))
        (vec* (normalize (transform-direction +y-axe+
                                              (matrix* rotation-reorient rotation-y rotation-x)))
              scale))))

(defun gaussian-velocity-constant-fn (dir intensity)
  "Assuming dir normalized"
  (let ((velo (vec* dir intensity)))
    #'(lambda () velo)))

(defun gaussian-distribution-fn (max delta)
  #'(lambda ()
      (max 0.0 (gaussian-probability delta max))))

(defun constant-delay-distribution-fn (delay &key (global-delay 0.0))
  (let ((ct global-delay))
    #'(lambda ()
        (prog1
            ct
          (setf ct (d+ ct delay))))))

(defstruct particle
  (forces    (vector (funcall (constant-force-clsr (vec 0.0 0.0 0.0)))) :type vector)
  (saved-forces  (vector (constant-force-clsr (vec 0.0 0.0 0.0)))       :type vector)
  (mass      1.0                                                        :type desired-type)
  (saved-v0  nil                                                        :type vec)
  (v0        (vec 0.0 0.0 0.0)                                          :type vec)
  (saved-position nil                                                   :type vec)
  (position  (vec 0.0 0.0 0.0)                                          :type vec)
  (saved-life nil                                                       :type function)
  (life      10.0                                                       :type desired-type)
  (saved-delay nil                                                      :type function)
  (delay     10.0                                                       :type desired-type)
  (saved-scaling  nil                                                   :type function)
  (scaling   #'(lambda (particle dt)
                 (declare (ignore particle dt))
                 1.0)
                                                                        :type function)
  (saved-rotation  nil                                                  :type function)
  (rotation   #'(lambda (particle dt)
                 (declare (ignore particle dt))
                 0.0)
                                                                        :type function)
  (saved-alpha nil                                                      :type function)
  (alpha   #'(lambda (particle dt)
               (declare (ignore particle dt))
               1.0)
                                                                        :type function)
  (saved-color nil                                                      :type function)
  (color   #'(lambda (particle dt)
               (declare (ignore particle dt))
               (vec4 1.0 1.0 1.0 1.0))
                                                                        :type function)
  (respawn   nil))

(defun respawn-particle (p)
  (setf (particle-v0       p) (copy-vec (particle-saved-v0       p))
        (particle-position p) (copy-vec (particle-saved-position p))
        (particle-life     p) (funcall  (particle-saved-life     p))
        (particle-delay    p) (funcall  (particle-saved-delay    p))
        (particle-scaling  p) (funcall  (particle-saved-scaling  p))
        (particle-rotation p) (funcall  (particle-saved-rotation p))
        (particle-alpha    p) (funcall  (particle-saved-alpha    p))
        (particle-color    p) (funcall  (particle-saved-color    p))
        (particle-forces   p) (map 'vector #'funcall (particle-saved-forces p))))

(defun constant-force-clsr (f)
  "- cluster: the particle cluster;
   - particle: this particle;
   - index: index of particle in cluster (useful to retrive position);
   - dt time elapsed from last call"
  #'(lambda ()
      #'(lambda (cluster particle index dt)
          (declare (ignore cluster particle index dt))
          f)))

(defun friction-force-clsr (friction-constant)
  "- cluster: the particle cluster;
   - particle: this particle;
   - index: index of particle in cluster (useful to retrive position);
   - dt time elapsed from last call"
  #'(lambda ()
      #'(lambda (cluster particle index dt)
          (declare (ignore dt))
          (let* ((i      (* index 3))
                 (all-vs (particles-v0 cluster))
                 (v (vec (d- (cl-gl-utils:fast-glaref all-vs i))
                         (d- (cl-gl-utils:fast-glaref all-vs (+ i 1)))
                         (d- (cl-gl-utils:fast-glaref all-vs (+ i 2)))))
                 (m (particle-mass particle)))
            (vec* v (d* m friction-constant))))))

(defun rotation-force-clsr ()
  "- cluster: the particle cluster;
   - particle: this particle;
   - index: index of particle in cluster (useful to retrive position);
   - dt time elapsed from last call"
  #'(lambda ()
      #'(lambda (cluster particle index dt)
          (declare (ignore dt))
          (let* ((i      (* index 3))
                 (m (particle-mass particle))
                 (all-vs  (particles-v0 cluster))
                 (all-pos (particles-positions cluster))
                 (v (vec (cl-gl-utils:fast-glaref all-vs i)
                         (cl-gl-utils:fast-glaref all-vs (+ i 1))
                         (cl-gl-utils:fast-glaref all-vs (+ i 2))))
                 (p (vec- (vec 0.0
                               (cl-gl-utils:fast-glaref all-pos (+ i 1))
                               0.0)
                          (vec (cl-gl-utils:fast-glaref all-pos i)
                               (cl-gl-utils:fast-glaref all-pos (+ i 1))
                               (cl-gl-utils:fast-glaref all-pos (+ i 2))))))
            (vec* (normalize p)
                  (d* m
                      (d/ (dexpt (vec-length v) 2.0)
                          (vec-length (vec (cl-gl-utils:fast-glaref all-pos i)
                                           (cl-gl-utils:fast-glaref all-pos (+ i 1))
                                           (cl-gl-utils:fast-glaref all-pos (+ i 2)))))))))))

(defun attraction-force-clsr (scale &optional (target +zero-vec+))
  "- cluster: the particle cluster;
   - particle: this particle;
   - index: index of particle in cluster (useful to retrive position);
   - dt time elapsed from last call"
  #'(lambda ()
      #'(lambda (cluster particle index dt)
          (declare (ignore particle dt))
          (let* ((i      (* index 3))
                 (all-pos (particles-positions cluster))
                 (p (vec- target
                          (vec (cl-gl-utils:fast-glaref all-pos i)
                               (cl-gl-utils:fast-glaref all-pos (+ i 1))
                               (cl-gl-utils:fast-glaref all-pos (+ i 2))))))
            (vec* (normalize p) scale)))))

(defclass particles-cluster (triangle-mesh inner-animation renderizable)
  ((integrator-shader
    :initarg  :integrator-shader
    :accessor integrator-shader)
   (transform-vao
    :initform '()
    :initarg  :transform-vao
    :accessor transform-vao)
   (transform-vbo-input
    :initform '()
    :initarg  :transform-vbo-input
    :accessor transform-vbo-input)
   (transform-vbo-output
    :initform '()
    :initarg  :transform-vbo-output
    :accessor transform-vbo-output)
   (particle-min-y
    :initform 0.0
    :initarg  :particle-min-y
    :accessor particle-min-y)
   (particle-width
    :initform 1.0
    :initarg  :particle-width
    :accessor particle-width)
   (particle-height
    :initform 1.0
    :initarg  :particle-height
    :accessor particle-height)
   (particles-positions
    :initform nil
    :initarg  :particles-positions
    :accessor particles-positions
    :type gl-array)
   (particles-scaling
    :initform nil
    :initarg  :particles-scaling
    :accessor particles-scaling
    :type gl-array)
   (particles-rotation
    :initform nil
    :initarg  :particles-rotation
    :accessor particles-rotation
    :type gl-array)
   (particles-alpha
    :initform nil
    :initarg  :particles-alpha
    :accessor particles-alpha
    :type gl-array)
   (particles-v0
    :initform nil
    :initarg  :particles-v0
    :accessor particles-v0
    :type gl-array)
   (particles-vt
    :initform nil
    :initarg  :particles-vt
    :accessor particles-vt
    :type gl-array)
   (particles-masses
    :initform nil
    :initarg  :particles-masses
    :accessor particles-masses
    :type gl-array)
   (particles-forces-feedback
    :initform nil
    :initarg  particles-forces-feedback
    :accessor particles-forces-feedback
    :type gl-array)
   (particles-output-positions
    :initform nil
    :initarg  :particles-output-positions
    :accessor particles-output-positions
    :type     gl-array)
   (particles-delay-feedback
    :initform nil
    :initarg  particles-delay-feedback
    :accessor particles-delay-feedback
    :type gl-array)
   (particles-delay
    :initform nil
    :initarg  particles-delay
    :accessor particles-delay
    :type gl-array)
   (particles-life
    :initform nil
    :initarg  particles-life
    :accessor particles-life
    :type gl-array)
   (particles-color
    :initform nil
    :initarg  particles-color
    :accessor particles-color
    :type gl-array)
   (particles
    :initform nil
    :initarg  :particles
    :accessor particles
    :type     (simple-array particles (*)))
   (remove-starting-delay
    :initform nil
    :initarg  :remove-starting-delay-p
    :accessor remove-starting-delay-p)
   (mark-for-remove
    :initform nil
    :initarg  :mark-for-remove
    :reader   mark-for-remove-p
    :writer  (setf mark-for-remove))))

(defun particles-cluster-p (a)
  (typep a 'particles-cluster))

(defmacro do-particles ((object particle) &body body)
  `(loop for ,particle across (particles ,object) do
        ,@body))

(defmethod initialize-instance :after ((object particles-cluster)
                                       &key
                                         (particle-pos-fn    nil)
                                         (particle-width-fn  nil)
                                         (particle-height-fn nil)
                                         &allow-other-keys)
  (with-accessors ((particles particles)
                   (particle-width particle-width)
                   (particle-height particle-height)
                   (remove-starting-delay remove-starting-delay)) object
    (loop repeat (length particles) do
         (let* ((actual-w (if particle-width-fn
                              (funcall particle-width-fn object)
                              particle-width))
                (actual-h (cond
                            ((and (not particle-width-fn)
                                  (not particle-height-fn))
                             particle-height)
                            ((and particle-width-fn
                                  (not particle-height-fn))
                             actual-w)
                            (t
                             (funcall particle-height-fn object))))
                (w/2 (d* actual-w 0.5))
                (h/2 (d* actual-h 0.5)))
           (quad object actual-w actual-h
                 0.0 0.0 1.0 1.0
                 (vec (d- w/2) (d- h/2) 0.0) ; centering
                 nil nil)))
    (when particle-pos-fn
      (do-particles (object particle)
        (let ((pos (funcall particle-pos-fn object)))
          (setf (particle-position       particle) pos
                (particle-saved-position particle) pos))))
    (remove-starting-delay object)))

(gen-populate-array-vec particles-cluster particles particles-positions      particle-position)

(gen-populate-array-vec particles-cluster particles particles-v0             particle-v0)

(gen-populate-array     particles-cluster particles particles-masses         particle-mass)

(gen-populate-array-vec particles-cluster particles particles-vt             particle-v0)

(gen-populate-array     particles-cluster particles particles-delay-feedback particle-delay)

(defgeneric populate-particles-output-positions-array (object))

(defgeneric populate-particles-delay-array (object))

(defgeneric populate-particles-life-array (object))

(defgeneric populate-particles-scaling-array (object dt &key force))

(defgeneric populate-particles-rotation-array (object dt &key force))

(defgeneric populate-particles-alpha-array (object dt &key force))

(defgeneric populate-particles-color-array (object dt &key force))

(defgeneric bind-computational-buffers (object))

(defgeneric feedback-output-array-size (object))

(defgeneric gather-forces (object dt))

(defgeneric set-respawn   (object value))

(defgeneric remove-starting-delay (object))

(defmethod populate-particles-output-positions-array ((object particles-cluster))
  (with-accessors ((particles particles)
                   (particles-positions particles-positions)
                   (particles-output-positions particles-output-positions)) object
    (gl-array-copy-multiply particles-positions particles-output-positions
                            (length particles)
                            3
                            6)))

(defmethod populate-particles-delay-array ((object particles-cluster))
  (with-accessors ((particles particles)
                   (particles-delay particles-delay)
                   (particles-delay-feedback particles-delay-feedback)) object
    (gl-array-copy-multiply particles-delay-feedback
                            particles-delay
                            (length particles)
                            1
                            6)))

(defmethod populate-particles-life-array ((object particles-cluster))
  (with-accessors ((particles particles)
                   (particles-life particles-life)) object
    (loop
       for particle across particles
       for ct from 0 by 6          do
         (loop for i from 0 below 6 by 1 do
              (setf (fast-glaref particles-life (+ ct i))
                    (particle-life particle))))))

(defmethod populate-particles-scaling-array ((object particles-cluster) dt &key (force nil))
  (with-accessors ((particles particles)
                   (particles-scaling particles-scaling)) object
    (loop
       for particle across particles
       for ct from 0 by 6          do
         (when (or force
                   (< (particle-delay particle) 0.0))
           (let ((scaling (funcall (particle-scaling particle) particle dt)))
             (loop for i from 0 below 6 by 1 do
                  (setf (fast-glaref particles-scaling (+ ct i))
                        scaling)))))))

(defmethod populate-particles-rotation-array ((object particles-cluster) dt &key (force nil))
  (with-accessors ((particles particles)
                   (particles-rotation particles-rotation)) object
    (loop
       for particle across particles
       for ct from 0 by 6          do
         (when (or force
                   (< (particle-delay particle) 0.0))
           (let ((rotation (funcall (particle-rotation particle) particle dt)))
             (loop for i from 0 below 6 by 1 do
                  (setf (fast-glaref particles-rotation (+ ct i))
                        rotation)))))))

(defmethod populate-particles-alpha-array ((object particles-cluster) dt &key (force nil))
  (with-accessors ((particles particles)
                   (particles-alpha particles-alpha)) object
    (loop
       for particle across particles
       for ct from 0 by 6          do
         (when (or force
                   (< (particle-delay particle) 0.0))
           (let ((alpha (funcall (particle-alpha particle) particle dt)))
             (loop for i from 0 below 6 by 1 do
                  (setf (fast-glaref particles-alpha (+ ct i))
                        alpha)))))))

(defmethod populate-particles-color-array ((object particles-cluster) dt &key (force nil))
  (with-accessors ((particles particles)
                   (particles-color particles-color)) object
    (loop
       for particle across particles
       for ct from 0 by 24          do
         (when (or force
                   (< (particle-delay particle) 0.0))
           (let ((color (funcall (particle-color particle) particle dt)))
             (loop for i from 0 below 24 by 4 do
                  (setf (fast-glaref particles-color (+ ct i))   (elt color 0))
                  (setf (fast-glaref particles-color (+ ct i 1)) (elt color 1))
                  (setf (fast-glaref particles-color (+ ct i 2)) (elt color 2))
                  (setf (fast-glaref particles-color (+ ct i 3)) (elt color 3))))))))

(defmethod removeable-from-world-p ((object particles-cluster))
  (loop for p across (particles object) do
       (when (> (particle-life p) 0.0)
         (return-from removeable-from-world-p nil)))
  t)

(defmethod aabb ((object particles-cluster))
  (with-slots (aabb) object
    (with-accessors ((model-matrix model-matrix)
                     (particles particles)
                     (particles-positions particles-positions)
                     (bounding-sphere bounding-sphere)) object
      (declare ((simple-vector 1) model-matrix))
      (let ((res (make-instance 'aabb)))
        (if particles-positions
            (loop for i from 0 below (length particles) by 3 do
                 (expand res (transform-point (vec (fast-glaref particles-positions    i)
                                                   (fast-glaref particles-positions (+ i 1))
                                                   (fast-glaref particles-positions (+ i 2)))
                                              (elt model-matrix 0))))
            (do-particles (object particle)
              (expand res (transform-point (particle-position particle)
                                           (elt model-matrix 0)))))
        (do-children-mesh (i object)
          (let ((child (aabb i)))
            (expand res (aabb-p1 child))
            (expand res (aabb-p2 child))))
        (setf bounding-sphere (aabb->bounding-sphere res))
        (setf aabb res)
        aabb))))

(defmethod reset-aabb ((object particles-cluster))
  "Recreate the aabb from object space vertices"
  (aabb object))

(defmethod destroy :after ((object particles-cluster))
  (with-accessors ((particles-positions particles-positions)
                   (particles-v0 particles-v0)
                   (particles-vt particles-vt)
                   (particles-masses particles-masses)
                   (particles-forces-feedback particles-forces-feedback)
                   (particles-output-positions particles-output-positions)
                   (particles-delay-feedback particles-delay-feedback)
                   (particles-delay particles-delay)
                   (particles-life  particles-life)
                   (particles-scaling particles-scaling)
                   (particles-rotation particles-rotation)
                   (particles-alpha particles-alpha)
                   (particles-color particles-color)
                   (transform-vao transform-vao)
                   (transform-vbo-input transform-vbo-input)
                   (transform-vbo-output transform-vbo-output)) object
    #+debug-mode (misc:dbg "destroy particle cluster ~a" (id object))
    (setf particles-positions        nil
          particles-v0               nil
          particles-vt               nil
          particles-masses           nil
          particles-forces-feedback  nil
          particles-delay-feedback   nil
          particles-delay            nil
          particles-life             nil
          particles-scaling          nil
          particles-rotation         nil
          particles-alpha            nil
          particles-color            nil
          particles-output-positions nil
          transform-vao              nil
          transform-vbo-input        nil
          transform-vbo-output       nil)))

(defmethod make-data-for-opengl :after ((object particles-cluster))
  (with-accessors ((particles-positions particles-positions)
                   (particles-v0               particles-v0)
                   (particles-vt               particles-vt)
                   (particles-masses           particles-masses)
                   (particles-forces-feedback  particles-forces-feedback)
                   (particles-delay-feedback   particles-delay-feedback)
                   (particles-delay            particles-delay)
                   (particles-life             particles-life)
                   (particles-scaling          particles-scaling)
                   (particles-rotation         particles-rotation)
                   (particles-alpha            particles-alpha)
                   (particles-color            particles-color)
                   (particles-output-positions particles-output-positions)
                   (transform-vao              transform-vao)
                   (transform-vbo-input        transform-vbo-input)
                   (particles                  particles)) object
    ;; initialize arrays
    (setf particles-positions        (gl:alloc-gl-array :float (* 3  (length particles))))
    (setf particles-output-positions (gl:alloc-gl-array :float (* 18 (length particles))))
    (setf particles-v0               (gl:alloc-gl-array :float (* 3  (length particles))))
    (setf particles-vt               (gl:alloc-gl-array :float (* 3  (length particles))))
    (setf particles-masses           (gl:alloc-gl-array :float (length particles)))
    (setf particles-forces-feedback  (gl:alloc-gl-array :float (* 3  (length particles))))
    (setf particles-delay-feedback   (gl:alloc-gl-array :float (length particles)))
    (setf particles-delay            (gl:alloc-gl-array :float (* 6  (length particles))))
    (setf particles-life             (gl:alloc-gl-array :float (* 6  (length particles))))
    (setf particles-scaling          (gl:alloc-gl-array :float (* 6  (length particles))))
    (setf particles-rotation         (gl:alloc-gl-array :float (* 6  (length particles))))
    (setf particles-alpha            (gl:alloc-gl-array :float (* 6  (length particles))))
    (setf particles-color            (gl:alloc-gl-array :float (* 24 (length particles))))
    (gather-forces object 0.0)
    (populate-particles-positions-array        object)
    (populate-particles-output-positions-array object)
    (populate-particles-v0-array               object)
    (populate-particles-vt-array               object)
    (populate-particles-masses-array           object)
    (populate-particles-delay-feedback-array   object)
    (populate-particles-delay-array            object)
    (populate-particles-life-array             object)
    (populate-particles-scaling-array          object 0.0 :force t)
    (populate-particles-rotation-array         object 0.0 :force t)
    (populate-particles-alpha-array            object 0.0 :force t)
    (populate-particles-color-array            object 0.0 :force t)
    ;; setup finalizer
    (let (#+debug-mode
          (id             (slot-value object 'id))
          (pos-in         (slot-value object 'particles-positions))
          (velo-t0        (slot-value object 'particles-v0))
          (velo-t         (slot-value object 'particles-vt))
          (masses         (slot-value object 'particles-masses))
          (forces         (slot-value object 'particles-forces-feedback))
          (delay-feedback (slot-value object 'particles-delay-feedback))
          (delay          (slot-value object 'particles-delay))
          (life           (slot-value object 'particles-life))
          (scaling        (slot-value object 'particles-scaling))
          (rotation       (slot-value object 'particles-rotation))
          (alpha          (slot-value object 'particles-alpha))
          (color          (slot-value object 'particles-color))
          (pos-out        (slot-value object 'particles-output-positions))
          (vao            (slot-value object 'transform-vao))
          (vbo-in         (slot-value object 'transform-vbo-input))
          (vbo-out        (slot-value object 'transform-vbo-output)))
      (tg:finalize object #'(lambda ()
                              #+debug-mode (misc:dbg "finalize destroy particles ~a" id)
                              (free-memory* (list pos-in
                                                  velo-t0
                                                  velo-t
                                                  masses
                                                  forces
                                                  delay-feedback
                                                  delay
                                                  life
                                                  scaling
                                                  rotation
                                                  alpha
                                                  color
                                                  pos-out)
                                            (append vbo-in vbo-out)
                                            vao)
                              (setf pos-in         nil
                                    velo-t0        nil
                                    velo-t         nil
                                    masses         nil
                                    forces         nil
                                    delay-feedback nil
                                    delay          nil
                                    life           nil
                                    scaling        nil
                                    rotation       nil
                                    alpha          nil
                                    color          nil
                                    pos-out        nil))))))

(defun vbo-masses-buffer-handle (vbo)
  (elt vbo 0))

(defun vbo-positions-buffer-handle (vbo)
  (elt vbo 1))

(defun vbo-v0-buffer-handle (vbo)
  (elt vbo 2))

(defun vbo-out-vt-buffer-handle (vbo)
  (elt vbo 3))

(defun vbo-out-position-buffer-handle (vbo)
  (elt vbo 4))

(defun vbo-delay-feedback-buffer-handle (vbo)
  (elt vbo 5))

(defun vbo-forces-buffer-handle (vbo)
  (elt vbo 6))

(defun vbo-color-buffer-handle (vbo)
  (elt vbo (- (length vbo) 7)))

(defun vbo-rotation-buffer-handle (vbo)
  (elt vbo (- (length vbo) 6)))

(defun vbo-alpha-buffer-handle (vbo)
  (elt vbo (- (length vbo) 5)))

(defun vbo-scaling-buffer-handle (vbo)
  (elt vbo (- (length vbo) 4)))

(defun vbo-life-buffer-handle (vbo)
  (elt vbo (- (length vbo) 3)))

(defun vbo-delay-buffer-handle (vbo)
  (elt vbo (- (length vbo) 2)))

(defun vbo-center-position-buffer-handle (vbo)
  (alexandria:last-elt vbo))

(defmethod bind-computational-buffers ((object particles-cluster))
  (with-accessors ((transform-vao              transform-vao)
                   (transform-vbo-input        transform-vbo-input)
                   (particles                  particles)
                   (particles-positions        particles-positions)
                   (particles-output-positions particles-output-positions)
                   (particles-v0               particles-v0)
                   (particles-vt               particles-vt)
                   (particles-delay-feedback   particles-delay-feedback)
                   (particles-masses           particles-masses)
                   (particles-forces-feedback  particles-forces-feedback)) object
    (with-unbind-vao
      (gl:bind-vertex-array (elt transform-vao 0))
      ;; input-position
      (gl:bind-buffer :array-buffer (vbo-positions-buffer-handle transform-vbo-input))
      (gl:buffer-data :array-buffer :static-draw particles-positions)
      (gl:vertex-attrib-pointer +attribute-position-location+ 3 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-position-location+)
      ;; input-masses
      (gl:bind-buffer :array-buffer (vbo-masses-buffer-handle transform-vbo-input))
      (gl:buffer-data :array-buffer :static-draw particles-masses)
      (gl:vertex-attrib-pointer +attribute-mass-location+ 1 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-mass-location+)
      ;; input-v0
      (gl:bind-buffer :array-buffer (vbo-v0-buffer-handle transform-vbo-input))
      (gl:buffer-data :array-buffer :static-draw particles-v0)
      (gl:vertex-attrib-pointer +attribute-v0-location+ 3 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-v0-location+)
      ;; input forces
      (gl:bind-buffer :array-buffer (vbo-forces-buffer-handle transform-vbo-input))
      (gl:buffer-data :array-buffer :dynamic-draw particles-forces-feedback)
      (gl:vertex-attrib-pointer +attribute-force-feedback-location+ 3 :float 0 0
                                (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-force-feedback-location+)
      ;; input-delay
      (gl:bind-buffer :array-buffer (vbo-delay-feedback-buffer-handle transform-vbo-input))
      (gl:buffer-data :array-buffer :static-draw particles-delay-feedback)
      (gl:vertex-attrib-pointer +attribute-delay-feedback-location+ 1 :float 0 0
                                (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-delay-feedback-location+)
      ;; output
      (gl:bind-buffer :array-buffer (vbo-out-position-buffer-handle transform-vbo-input))
      (gl:buffer-data :array-buffer :dynamic-copy (gl:make-null-gl-array :float)
                      :size (feedback-output-array-size object)))))

(defmethod feedback-output-array-size ((object particles-cluster))
  (with-accessors ((particles-vt particles-vt)
                   (particles-positions particles-positions)) object
    (+ (gl:gl-array-byte-size particles-positions)
       (gl:gl-array-byte-size particles-vt))))

(defmethod gather-forces ((object particles-cluster) dt)
  (with-accessors ((particles particles)
                   (particles-forces-feedback particles-forces-feedback)
                   (gravity gravity)) object
    (loop
       for particle across particles
         for i from 0                do
         (let ((force (reduce #'(lambda (a b) (vec+ a (funcall b object particle i dt)))
                              (particle-forces particle)
                              :initial-value +zero-vec+)))
           (setf (fast-glaref particles-forces-feedback       (f* i 3))  (elt force 0)
                 (fast-glaref particles-forces-feedback (f+ 1 (f* i 3))) (elt force 1)
                 (fast-glaref particles-forces-feedback (f+ 2 (f* i 3))) (elt force 2))))))

(defmethod set-respawn ((object particles-cluster) value)
  (do-particles (object particle)
    (setf (particle-respawn particle) value)))

(defmethod remove-starting-delay ((object particles-cluster))
  (with-accessors ((remove-starting-delay-p remove-starting-delay-p)
                   (particles particles)) object
    (when remove-starting-delay-p
      (let ((min-delay (find-min (map 'list #'particle-delay particles))))
        (do-particles (object particle)
          (setf (particle-delay       particle)
                (d- (particle-delay particle) min-delay)))))))

(defmethod prepare-for-rendering ((object particles-cluster))
  (with-accessors ((vao                        vao)
                   (vbo                        vbo)
                   (particle-width             particle-width)
                   (particle-height            particle-height)
                   (renderer-data-texture      renderer-data-texture)
                   (transform-vao              transform-vao)
                   (transform-vbo-input        transform-vbo-input)
                   (particles                  particles)
                   (particles-delay            particles-delay)
                   (particles-life             particles-life)
                   (particles-scaling          particles-scaling)
                   (particles-rotation         particles-rotation)
                   (particles-alpha            particles-alpha)
                   (particles-color            particles-color)
                   (particles-positions        particles-positions)
                   (particles-output-positions particles-output-positions)
                   (particles-v0               particles-v0)
                   (particles-masses           particles-masses)) object
    (call-next-method)
    (setf vbo (append vbo (gl:gen-buffers +appended-vbo-count+)))
    (setf transform-vbo-input  (gl:gen-buffers +transform-vbo-count+)
          transform-vao        (gl:gen-vertex-arrays 1))
    (mesh:make-data-for-opengl object)
    (bind-computational-buffers object)
    ;;rendering
    (with-unbind-vao
      (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
      ;; pos
      (gl:bind-buffer :array-buffer (vbo-center-position-buffer-handle vbo))
      (gl:buffer-data :array-buffer :dynamic-draw particles-output-positions)
      (gl:vertex-attrib-pointer +attribute-center-pos-location+ 3 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-center-pos-location+)
      ;; texture
      (gl:bind-buffer :array-buffer (vbo-texture-buffer-handle vbo))
      (gl:buffer-data :array-buffer :static-draw renderer-data-texture)
      (gl:vertex-attrib-pointer +attribute-texture-location+ 2 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-texture-location+)
      ;; delay
      (gl:bind-buffer :array-buffer (vbo-delay-buffer-handle vbo))
      (gl:buffer-data :array-buffer :dynamic-draw particles-delay)
      (gl:vertex-attrib-pointer +attribute-delay-location+ 1 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-delay-location+)
      ;; life
      (gl:bind-buffer :array-buffer (vbo-life-buffer-handle vbo))
      (gl:buffer-data :array-buffer :dynamic-draw particles-life)
      (gl:vertex-attrib-pointer +attribute-life-location+ 1 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-life-location+)
      ;; scaling
      (gl:bind-buffer :array-buffer (vbo-scaling-buffer-handle vbo))
      (gl:buffer-data :array-buffer :dynamic-draw particles-scaling)
      (gl:vertex-attrib-pointer +attribute-scaling-location+ 1 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-scaling-location+)
      ;; rotation
      (gl:bind-buffer :array-buffer (vbo-rotation-buffer-handle vbo))
      (gl:buffer-data :array-buffer :dynamic-draw particles-rotation)
      (gl:vertex-attrib-pointer +attribute-rotation-location+ 1 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-rotation-location+)
      ;; alpha
      (gl:bind-buffer :array-buffer (vbo-alpha-buffer-handle vbo))
      (gl:buffer-data :array-buffer :dynamic-draw particles-alpha)
      (gl:vertex-attrib-pointer +attribute-alpha-location+ 1 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-alpha-location+)
      ;; color
      (gl:bind-buffer :array-buffer (vbo-color-buffer-handle vbo))
      (gl:buffer-data :array-buffer :dynamic-draw particles-color)
      (gl:vertex-attrib-pointer +attribute-color-location+ 4 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-color-location+)
      object)))

(defmacro with-feedback-calculate ((object dt) &body body)
  (alexandria:with-gensyms (integrator-shader
                            compiled-shaders
                            transform-vao
                            particle-min-y
                            vbo
                            particles
                            particles-positions
                            particles-output-positions
                            particles-v0
                            particles-vt
                            particles-forces-feedback
                            particles-delay-feedback
                            particles-delay
                            particles-life
                            particles-scaling
                            particles-rotation
                            particles-alpha
                            particles-color
                            transform-vbo-input
                            res-array
                            particle
                            part-ct)
    `(with-accessors ((,integrator-shader          integrator-shader)
                      (,compiled-shaders           compiled-shaders)
                      (,transform-vao              transform-vao)
                      (,particle-min-y             particle-min-y)
                      (,vbo                        vbo)
                      (,particles                  particles)
                      (,particles-positions        particles-positions)
                      (,particles-output-positions particles-output-positions)
                      (,particles-v0               particles-v0)
                      (,particles-vt               particles-vt)
                      (,particles-forces-feedback  particles-forces-feedback)
                      (,particles-delay-feedback   particles-delay-feedback)
                      (,particles-delay            particles-delay)
                      (,particles-life             particles-life)
                      (,particles-scaling          particles-scaling)
                      (,particles-rotation         particles-rotation)
                      (,particles-alpha            particles-alpha)
                      (,particles-color            particles-color)
                      (,transform-vbo-input        transform-vbo-input)) object
       (bubbleup-modelmatrix ,object)
       (with-unbind-vao
         (bind-computational-buffers ,object)
         (use-program compiled-shaders ,integrator-shader)
         (uniformf compiled-shaders :dt dt)
         ,@body
         (cl-gl-utils:with-rasterizer-discard
           (%gl:bind-buffer-base :transform-feedback-buffer
                                 0
                                 (vbo-out-position-buffer-handle ,transform-vbo-input))
           (cl-gl-utils:with-transform-feedback (:points)
             (gl:bind-vertex-array (elt ,transform-vao 0))
             (gl:draw-arrays :points 0 (length ,particles)))
           (gl:flush)
           (cffi:with-foreign-object (,res-array :float (* 6 (length ,particles)))
             (%gl:get-buffer-sub-data :transform-feedback-buffer
                                      0
                                      (feedback-output-array-size ,object)
                                      ,res-array)
             (loop for i from 0 below (* 6 (length ,particles)) by 6 do
                ;; setting position
                  (setf (cl-gl-utils:fast-glaref ,particles-positions (/ i 2))
                        (cffi:mem-aref ,res-array :float i))
                  (setf (cl-gl-utils:fast-glaref ,particles-positions (+ (/ i 2) 1))
                        (cffi:mem-aref ,res-array :float (+ i 1)))
                  (setf (cl-gl-utils:fast-glaref ,particles-positions (+ (/ i 2) 2))
                        (cffi:mem-aref ,res-array :float (+ i 2)))
                ;; setting velocity
                  (setf (cl-gl-utils:fast-glaref ,particles-v0 (/ i 2))
                        (cffi:mem-aref ,res-array :float (+ i 3) ))
                  (setf (cl-gl-utils:fast-glaref ,particles-v0 (+ (/ i 2) 1))
                        (cffi:mem-aref ,res-array :float (+ i 4)))
                  (setf (cl-gl-utils:fast-glaref ,particles-v0 (+ (/ i 2) 2))
                        (cffi:mem-aref ,res-array :float (+ i 5)))))))
       (populate-particles-output-positions-array ,object)
       (populate-particles-delay-feedback-array   ,object)
       (populate-particles-delay-array            ,object)
       (populate-particles-life-array             ,object)
       (populate-particles-scaling-array          ,object ,dt :force nil)
       (populate-particles-rotation-array         ,object ,dt :force nil)
       (populate-particles-alpha-array            ,object ,dt :force nil)
       (populate-particles-color-array            ,object ,dt :force nil)
       (gather-forces                             ,object ,dt)
       ;; transform feedback
       (gl:bind-buffer :array-buffer (vbo-forces-buffer-handle ,transform-vbo-input))
       (gl:buffer-sub-data :array-buffer  ,particles-forces-feedback)
       ;; rendering
       (gl:bind-buffer :array-buffer (vbo-center-position-buffer-handle ,vbo))
       (gl:buffer-sub-data :array-buffer  ,particles-output-positions)
       (gl:bind-buffer :array-buffer (vbo-delay-feedback-buffer-handle ,vbo))
       (gl:buffer-sub-data :array-buffer  ,particles-delay-feedback)
       (gl:bind-buffer :array-buffer (vbo-delay-buffer-handle ,vbo))
       (gl:buffer-sub-data :array-buffer  ,particles-delay)
       (gl:bind-buffer :array-buffer (vbo-life-buffer-handle ,vbo))
       (gl:buffer-sub-data :array-buffer  ,particles-life)
       (gl:bind-buffer :array-buffer (vbo-scaling-buffer-handle ,vbo))
       (gl:buffer-sub-data :array-buffer  ,particles-scaling)
       (gl:bind-buffer :array-buffer (vbo-rotation-buffer-handle ,vbo))
       (gl:buffer-sub-data :array-buffer  ,particles-rotation)
       (gl:bind-buffer :array-buffer (vbo-alpha-buffer-handle ,vbo))
       (gl:buffer-sub-data :array-buffer  ,particles-alpha)
       (gl:bind-buffer :array-buffer (vbo-color-buffer-handle ,vbo))
       (gl:buffer-sub-data :array-buffer  ,particles-color)
       (loop
          for ,particle across ,particles
          for ,part-ct  from 0 by 3         do
            (decf (particle-delay ,particle) ,dt)
            (when (< (particle-delay ,particle) 0.0)
              (decf (particle-life  ,particle) dt))
            (when (and (particle-respawn ,particle)
                       (d< (particle-life  ,particle) 0.0))
              (respawn-particle ,particle)
              (setf (cl-gl-utils:fast-glaref ,particles-positions ,part-ct)
                    (elt (particle-position ,particle) 0))
              (setf (cl-gl-utils:fast-glaref ,particles-positions (f+ ,part-ct 1))
                    (elt (particle-position ,particle) 1))
              (setf (cl-gl-utils:fast-glaref ,particles-positions (f+ ,part-ct 2))
                    (elt (particle-position ,particle) 2))
              (setf (cl-gl-utils:fast-glaref ,particles-v0 ,part-ct)
                    (elt (particle-v0 ,particle) 0))
              (setf (cl-gl-utils:fast-glaref ,particles-v0 (f+ ,part-ct 1))
                    (elt (particle-v0 ,particle) 1))
              (setf (cl-gl-utils:fast-glaref ,particles-v0 (f+ ,part-ct 2))
                    (elt (particle-v0 ,particle) 2)))))))

(defmethod calculate ((object particles-cluster) dt)
  (do-children-mesh (i object)
    (calculate i dt)))

(defmethod render ((object particles-cluster) renderer)
  (do-children-mesh (c object)
    (render c renderer)))

(defclass  cluster-w-gravity (particles-cluster)
  ((gravity
   :initform +particle-gravity+
   :initarg :gravity
   :accessor gravity)))

(defclass cluster-w-global-life (particles-cluster)
  ((global-life
    :initform 1000
    :initarg  :global-life
    :accessor global-life)))

(defmethod calculate :before ((object cluster-w-global-life) dt)
  (with-accessors ((global-life global-life)) object
    (decf global-life)
    (when (< global-life 0)
      (set-respawn object nil))))

(defclass minimal-particle-effect (cluster-w-gravity)
  ((noise-scale
    :initform 0.0
    :initarg :noise-scale
    :accessor noise-scale)))

(defmethod initialize-instance :after ((object minimal-particle-effect)
                                       &key
                                         (texture (texture:get-texture
                                                   texture:+blood-particle+))
                                         &allow-other-keys)
  (setf (use-blending-p object) t)
  (setf (integrator-shader object) :blood-integrator)
  (setf (texture-object object) texture))

(defmethod calculate ((object minimal-particle-effect) dt)
  (with-accessors ((compiled-shaders compiled-shaders)
                   (particle-min-y   particle-min-y)
                   (gravity          gravity)
                   (noise-scale      noise-scale)) object
    (with-feedback-calculate (object dt)
      (uniformfv compiled-shaders :gravity gravity)
      (uniformf  compiled-shaders :noise-scale  noise-scale)
      (uniformf  compiled-shaders :min-y   particle-min-y)))
  (call-next-method)) ;; calculate children too

(defmethod render ((object minimal-particle-effect) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (el-time el-time)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (cl-gl-utils:with-blending
            (gl:blend-equation :func-subtract)
            (gl:blend-func :src-alpha :one-minus-src-alpha)
            (use-program compiled-shaders :particles-blood)
            (gl:active-texture :texture0)
            (texture:bind-texture texture-object)
            (uniformf  compiled-shaders :time  el-time)
            (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
            (uniform-matrix compiled-shaders :modelview-matrix 4
                            (vector (matrix* camera-vw-matrix
                                             (elt view-matrix 0)
                                             (elt model-matrix 0)))
                            nil)
            (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
            (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
            (gl:draw-arrays :triangles 0 (f* 3 (length triangles)))
            (gl:blend-equation :func-add)))))
    (call-next-method))) ;; render children too

(defclass blood (minimal-particle-effect end-life-trigger) ())

(defmethod initialize-instance :after ((object blood) &key (enqueuedp nil) &allow-other-keys)
  (if enqueuedp
      (action-scheduler:end-of-life-remove-from-action-scheduler object
                                                                 action-scheduler:blood-spill-action)
      (end-of-life-remove-from-world object)))

(defmethod calculate :after ((object blood) dt)
  (with-maybe-trigger-end-of-life (object (almost-removeable-from-world-p object))))

(defmethod almost-removeable-from-world-p ((object blood))
  (let ((all        (d (length (particles object))))
        (removeable (d (loop for p across (particles object)
                          when (< (particle-life p) 0.0)
                          sum 1))))
    (d> (d/ removeable all)
        0.1)))

(defun make-particles-cluster (class particles-count shaders-dict
                               &key
                                 (texture     (texture:get-texture texture:+blood-particle+))
                                 (forces      (vector (constant-force-clsr (vec 0.0 0.0 0.0))))
                                 (v0-fn       #'(lambda () (vec 0.0 40.0 0.0)))
                                 (mass-fn     #'(lambda () 1.0))
                                 (position-fn #'(lambda () (vec 0.0 0.1 0.0)))
                                 (life-fn     #'(lambda () 10.1))
                                 (delay-fn    #'(lambda () 10.0))
                                 (remove-starting-delay nil)
                                 (scaling-fn  #'(lambda ()
                                                  #'(lambda (particle dt)
                                                      (declare (ignore particle dt))
                                                      1.0)))
                                 (rotation-fn #'(lambda ()
                                                  #'(lambda (particle dt)
                                                      (declare (ignore particle dt))
                                                      0.0)))
                                 (alpha-fn  #'(lambda ()
                                                #'(lambda (particle dt)
                                                    (declare (ignore particle dt))
                                                    1.0)))
                                 (color-fn  #'(lambda ()
                                                #'(lambda (particle dt)
                                                    (declare (ignore particle dt))
                                                    (vec4 1.0 1.0 1.0 1.0))))
                                 (width       1.0)
                                 (height      1.0)
                                 (particle-width-fn  nil)
                                 (particle-height-fn nil)
                                 (pos         +zero-vec+)
                                 (particle-pos-fn nil)
                                 (min-y       0.0)
                                 (gravity     +zero-vec+)
                                 (respawn     nil))
  (let* ((particles (loop repeat particles-count collect
                         (let ((starting-velo  (funcall v0-fn))
                               (starting-pos   (funcall position-fn)))
                           (make-particle :forces (map 'vector #'funcall forces)
                                          :saved-forces forces
                                          :mass     (funcall mass-fn)
                                          :v0       starting-velo
                                          :saved-v0 (copy-vec starting-velo)
                                          :position starting-pos
                                          :saved-position (copy-vec starting-pos)
                                          :scaling  (funcall scaling-fn)
                                          :saved-scaling     scaling-fn
                                          :rotation  (funcall rotation-fn)
                                          :saved-rotation     rotation-fn
                                          :alpha    (funcall alpha-fn)
                                          :saved-alpha       alpha-fn
                                          :life     (funcall life-fn)
                                          :saved-life        life-fn
                                          :color    (funcall color-fn)
                                          :saved-color       color-fn
                                          :respawn  respawn
                                          :delay    (funcall delay-fn)
                                          :saved-delay  delay-fn))))
         (cluster  (make-instance class
                                  :remove-starting-delay-p remove-starting-delay
                                  :texture            texture
                                  :pos                pos
                                  :particle-pos-fn    particle-pos-fn
                                  :particle-min-y     min-y
                                  :compiled-shaders   shaders-dict
                                  :particle-width     width
                                  :particle-height    height
                                  :particle-width-fn  particle-width-fn
                                  :particle-height-fn particle-height-fn
                                  :particles          (list->simple-array particles nil 'particle)
                                  :gravity            gravity)))
    (prepare-for-rendering cluster)
    cluster))

(defun make-blood-level-0 (pos dir compiled-shaders)
  (make-particles-cluster 'blood
                          100
                          compiled-shaders
                          :pos     pos
                          :min-y   (d- +zero-height+ (elt pos 1))
                          :v0-fn   (gaussian-velocity-distribution-fn dir
                                                                      2.0
                                                                      1.0
                                                                      (d/ +pi/2+ 2.0))
                          :mass-fn  #'(lambda () 1.0)
                          :life-fn  (gaussian-distribution-fn .2 0.05)
                          :delay-fn (gaussian-distribution-fn 0.0 .1)
                          :gravity +particle-gravity+
                          :width  0.1
                          :height 0.1))

(defun make-blood-level-1 (pos dir compiled-shaders)
  (make-particles-cluster 'blood
                          100
                          compiled-shaders
                          :pos     pos
                          :min-y   (d- +zero-height+ (elt pos 1))
                          :v0-fn   (gaussian-velocity-distribution-fn dir
                                                                      2.0
                                                                      1.0
                                                                      (d/ +pi/2+ 3.0))
                          :mass-fn  #'(lambda () 1.0)
                          :life-fn  (gaussian-distribution-fn 1.1 0.1)
                          :delay-fn (gaussian-distribution-fn 0.0 .1)
                          :gravity +particle-gravity+
                          :width  0.1
                          :height 0.1))

(defun make-blood-level-2 (pos dir compiled-shaders)
  (make-particles-cluster 'blood
                          500
                          compiled-shaders
                          :pos     pos
                          :min-y   (d- +zero-height+ (elt pos 1))
                          :v0-fn   (gaussian-velocity-distribution-fn dir
                                                                      3.0
                                                                      1.0
                                                                      (d/ +pi/2+ 6.0))
                          :mass-fn  #'(lambda () 1.0)
                          :life-fn  (gaussian-distribution-fn 12.0 5.0)
                          :delay-fn (gaussian-distribution-fn 0.0 1.1)
                          :scaling-fn #'(lambda ()
                                          #'(lambda (p dt)
                                              (declare (ignore p dt))
                                              1.0))
                          :gravity +particle-gravity+
                          :width  .1
                          :height .1))

(defun make-blood-death (pos dir compiled-shaders)
  (make-particles-cluster 'blood
                          1500
                          compiled-shaders
                          :pos     pos
                          :min-y   (d- +zero-height+ (elt pos 1))
                          :v0-fn   (gaussian-velocity-distribution-fn dir
                                                                      3.0
                                                                      1.0
                                                                      (d/ +pi/2+ 3.0))
                          :mass-fn  (gaussian-distribution-fn 1.0 .1)
                          :life-fn  (gaussian-distribution-fn 12.0 5.0)
                          :delay-fn (gaussian-distribution-fn 0.0 1.1)
                          :scaling-fn #'(lambda ()
                                          #'(lambda (p dt)
                                              (declare (ignore p dt))
                                              1.0))
                          :gravity +particle-gravity+
                          :width  .1
                          :height .1))

(defmacro gen-add-blood (postfix)
  (let ((fn-name       (misc:format-fn-symbol t "add-blood-~a" postfix))
        (inner-fn-name (misc:format-fn-symbol t "make-blood-~a" postfix)))
    `(defun ,fn-name (entity pos dir)
       (with-accessors ((game-state state)
                        (compiled-shaders compiled-shaders)) entity
         (game-state:with-world (world game-state)
           (world:push-entity world (,inner-fn-name pos dir compiled-shaders)))))))

(gen-add-blood "level-0")

(gen-add-blood "level-1")

(gen-add-blood "level-2")

(gen-add-blood "death")

(defclass debris (minimal-particle-effect) ())

(defmethod render ((object debris) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (el-time el-time)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (cl-gl-utils:with-blending
            (gl:blend-equation :func-add)
            (gl:blend-func :src-alpha :one)
            (use-program compiled-shaders :particles-blood)
            (gl:active-texture :texture0)
            (texture:bind-texture texture-object)
            (uniformf  compiled-shaders :time  el-time)
            (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
            (uniform-matrix compiled-shaders :modelview-matrix 4
                            (vector (matrix* camera-vw-matrix
                                             (elt view-matrix 0)
                                             (elt model-matrix 0)))
                            nil)
            (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
            (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
            (gl:draw-arrays :triangles 0 (f* 3 (length triangles)))
            (gl:blend-equation :func-add)))))))

(defun debris-particles-number (damage)
  (alexandria:clamp (truncate (d* (dexpt damage
                                         2.0)))
                    5
                    150))

(defun make-debris (pos dir num texture compiled-shaders)
  (make-particles-cluster 'debris
                          num
                          compiled-shaders
                          :texture texture
                          :pos     pos
                          :min-y   (d- +zero-height+ (elt pos 1))
                          :v0-fn   (gaussian-velocity-distribution-fn dir
                                                                      5.0
                                                                      2.0
                                                                      (d/ +pi/2+ 3.0))
                          :mass-fn  (gaussian-distribution-fn 1.0 0.1)
                          :life-fn  (gaussian-distribution-fn 12.0 5.0)
                          :delay-fn (gaussian-distribution-fn 0.0 .0001)
                          :scaling-fn (%uniform-scaling-clsr -0.6)
                          :gravity +particle-gravity+
                          :width  .1
                          :height .1))

(defclass summon-particles (debris cluster-w-global-life end-life-trigger) ())

(defmethod calculate :after ((object summon-particles) dt)
  (with-maybe-trigger-end-of-life (object (removeable-from-world-p object))))

(defmethod render ((object summon-particles) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (el-time el-time)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (with-depth-mask-disabled
            (cl-gl-utils:with-blending
              (gl:blend-equation :func-add)
              (gl:blend-func :src-alpha :one)
              (use-program compiled-shaders :particles-aerial-explosion)
              (gl:active-texture :texture0)
              (texture:bind-texture texture-object)
              (uniformf  compiled-shaders :time  el-time)
              (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
              (uniform-matrix compiled-shaders :modelview-matrix 4
                              (vector (matrix* camera-vw-matrix
                                               (elt view-matrix 0)
                                               (elt model-matrix 0)))
                              nil)
              (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
              (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
              (gl:draw-arrays :triangles 0 (f* 3 (length triangles)))
              (gl:blend-equation :func-add))))))
    (do-children-mesh (c object)
      (render c renderer))))

(defun make-summon-player-fx (pos compiled-shaders)
  (let ((fx (make-particles-cluster 'summon-particles
                          5
                          compiled-shaders
                          :texture
                          (random-elt (list-of-texture-by-tag +texture-tag-teleport-particle+))
                          :pos        pos
                          :min-y      (d- +zero-height+ (elt pos 1))
                          :v0-fn      (gaussian-velocity-distribution-fn +y-axe+
                                                                         0.0
                                                                         .000001
                                                                         +pi+)
                          :mass-fn    (gaussian-distribution-fn 1.0 0.01)
                          :life-fn    (gaussian-distribution-fn 3.0 .1)
                          :delay-fn   (gaussian-distribution-fn 0.0 .0001)
                          :scaling-fn (%uniform-scaling-clsr 10.6)
                          :color-fn   (%gradient-color-clsr color-utils:+rainbow-gradient+
                                                            .5)
                          :alpha-fn   (%smooth-alpha-fading-clsr 4.0)
                          :gravity    +zero-vec+
                          :width      1.1
                          :height     1.1
                          :respawn    t)))
    (setf (global-life fx) 100)
    fx))

(defun make-repair-1-fx (pos compiled-shaders)
  (let* ((tex-res  (append +spell-texture-dir+ (list "misc")))
         (tex-file (res:get-resource-file texture:+repair-texture-1-fx+
                                          tex-res))
         (fx (make-particles-cluster 'summon-particles
                          5
                          compiled-shaders
                          :texture    (with-prepared-texture (texture tex-file)
                                        (setf (texture:interpolation-type texture) :linear))
                          :pos        pos
                          :min-y      (d- +zero-height+ (elt pos 1))
                          :v0-fn      (gaussian-velocity-distribution-fn +y-axe+
                                                                         10.0
                                                                         1.01
                                                                         +pi+)
                          :mass-fn    (gaussian-distribution-fn 1.0 0.01)
                          :life-fn    (gaussian-distribution-fn 3.0 .1)
                          :delay-fn   (gaussian-distribution-fn 0.0 .0001)
                          :scaling-fn (%uniform-scaling-clsr 10.6)
                          :color-fn   (%gradient-color-clsr color-utils:+rainbow-gradient+
                                                            .5)
                          :alpha-fn   (%smooth-alpha-fading-clsr 4.0)
                          :gravity    +zero-vec+
                          :width      1.1
                          :height     1.1
                          :respawn    t)))
    (setf (global-life fx) 100)
    fx))

(defclass smoke-puff (cluster-w-gravity) ())

(defmethod initialize-instance :after ((object smoke-puff)
                                       &key
                                         (texture (texture:get-texture
                                                   texture:+blood-particle+))
                                         &allow-other-keys)
  (setf (use-blending-p object) t)
  (setf (integrator-shader object) :blood-integrator)
  (setf (texture-object object) texture))

(defmethod calculate ((object smoke-puff) dt)
  (with-accessors ((compiled-shaders compiled-shaders)
                   (particle-min-y   particle-min-y)
                   (gravity          gravity)) object
    (with-feedback-calculate (object dt)
      (uniformfv compiled-shaders :gravity gravity)
      (uniformf  compiled-shaders :noise-scale  0.0)
      (uniformf  compiled-shaders :min-y   particle-min-y))))

(defmethod render ((object smoke-puff) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (el-time el-time)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (with-depth-mask-disabled
            (with-blending
              (gl:blend-equation :func-add)
              (gl:blend-func :src-alpha :one-minus-src-alpha)
              (use-program compiled-shaders :particles-blood)
            (gl:active-texture :texture0)
            (texture:bind-texture texture-object)
            (uniformf  compiled-shaders :time  el-time)
            (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
            (uniform-matrix compiled-shaders :modelview-matrix 4
                            (vector (matrix* camera-vw-matrix
                                             (elt view-matrix 0)
                                             (elt model-matrix 0)))
                            nil)
            (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
            (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
            (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
            (gl:blend-equation :func-add))))))))

(defun make-smoke-level-0 (pos dir compiled-shaders)
  (make-particles-cluster 'smoke-puff
                          100
                          compiled-shaders
                          :texture (random-elt (list-of-texture-by-tag +smoke-particle+))
                          :pos     pos
                          :v0-fn    (gaussian-velocity-constant-fn dir 1.0)
                          :mass-fn  (gaussian-distribution-fn 1.0 0.001)
                          :life-fn  (gaussian-distribution-fn 70.0 0.001)
                          :delay-fn (constant-delay-distribution-fn 1.0)
                          :scaling-fn  (%uniform-scaling-clsr 10.0)
                          :rotation-fn (%uniform-rotation-clsr 1.0)
                          :alpha-fn    (%smooth-alpha-fading-clsr 10.0)
                          :gravity   +zero-vec+
                          :width  .1
                          :height .1
                          :respawn nil))

(defun make-smoke-level-1 (pos dir compiled-shaders)
  (make-particles-cluster 'smoke-puff
                          1000
                          compiled-shaders
                          :texture (random-elt (list-of-texture-by-tag +smoke-particle+))
                          :pos     pos
                          :v0-fn    (gaussian-velocity-constant-fn dir 1.0)
                          :mass-fn  (gaussian-distribution-fn 1.0 0.001)
                          :life-fn  (gaussian-distribution-fn 200.0 0.001)
                          :delay-fn (constant-delay-distribution-fn .5)
                          :scaling-fn  (%uniform-scaling-clsr  (lcg-next-in-range 2.5 3.5))
                          :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range 1.0 2.0))
                          :alpha-fn    (%smooth-alpha-fading-clsr 18.0)
                          :gravity   +zero-vec+
                          :width  .2
                          :height .2
                          :respawn nil))

(defun %radial-v0-fn (num intensity)
  (let ((angle 0.0)
        (angle-incr (d/ +2pi+ num)))
    #'(lambda ()
        (let ((dir +x-axe+))
          (prog1
              (vec* (transform-direction dir (rotate-around +y-axe+ angle))
                    intensity)
            (setf angle (d+ angle angle-incr)))))))

(defun make-radial-expanding-level-1 (pos compiled-shaders
                                      &key
                                        (texture (random-elt (list-of-texture-by-tag +smoke-particle+)))
                                        (num 6)
                                        (life-fn (gaussian-distribution-fn 20.0 0.001))
                                        (v0-intensity 8.0))
  (make-particles-cluster 'smoke-puff
                          num
                          compiled-shaders
                          :texture texture
                          :pos     pos
                          :v0-fn    (%radial-v0-fn (d num) v0-intensity)
                          :mass-fn  (gaussian-distribution-fn 1.0 0.001)
                          :life-fn  life-fn
                          :delay-fn (constant-delay-distribution-fn 0.0)
                          :scaling-fn  (%uniform-scaling-clsr 200.0)
                          :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range 1.0 2.0))
                          :alpha-fn    (%smooth-alpha-fading-clsr 2.0)
                          :gravity   +zero-vec+
                          :width  .1
                          :height .1
                          :respawn nil))

(defun make-radial-expanding-level-0 (pos compiled-shaders
                                      &key
                                        (texture (random-elt (list-of-texture-by-tag +smoke-particle+)))
                                        (num 7)
                                        (life-fn (gaussian-distribution-fn 10.0 1.0))
                                        (v0-intensity 6.0))
  (make-particles-cluster 'smoke-puff
                          num
                          compiled-shaders
                          :texture texture
                          :pos     pos
                          :v0-fn    (%radial-v0-fn (d num) v0-intensity)
                          :mass-fn  (gaussian-distribution-fn 10.0 0.5)
                          :life-fn  life-fn
                          :delay-fn (constant-delay-distribution-fn 0.0)
                          :scaling-fn  (%uniform-scaling-clsr 50.0)
                          :rotation-fn (%uniform-rotation-clsr 10.0)
                          :alpha-fn    (%smooth-alpha-fading-clsr .5)
                          :gravity   +zero-vec+
                          :width  .1
                          :height .1
                          :respawn nil))

(defun make-radial-expanding (pos compiled-shaders
                              &key
                                (texture (random-elt (list-of-texture-by-tag +smoke-particle+)))
                                (num 4)
                                (life-fn (gaussian-distribution-fn 70.0 0.001))
                                (v0-intensity 4.0))
  (make-particles-cluster 'smoke-puff
                          num
                          compiled-shaders
                          :texture texture
                          :pos     pos
                          :v0-fn    (%radial-v0-fn (d num) v0-intensity)
                          :mass-fn  (gaussian-distribution-fn 1.0 0.001)
                          :life-fn  life-fn
                          :delay-fn (constant-delay-distribution-fn 0.0)
                          :scaling-fn  (%uniform-scaling-clsr 10.0)
                          :rotation-fn (%uniform-rotation-clsr 0.0)
                          :alpha-fn    (%smooth-alpha-fading-clsr 10.0)
                          :gravity   +zero-vec+
                          :width  .1
                          :height .1
                          :respawn nil))

(defun make-smoke-confusion (pos compiled-shaders
                             &key
                               (texture
                                (random-elt (list-of-texture-by-tag +smoke-particle+)))
                               (num 7)
                               (life-fn (gaussian-distribution-fn 150.0 10.0))
                               (v0-intensity 2.0))
  (make-particles-cluster 'smoke-puff
                          num
                          compiled-shaders
                          :texture texture
                          :pos     pos
                          :v0-fn    (%radial-v0-fn (d num) v0-intensity)
                          :mass-fn  (gaussian-distribution-fn 10.0 0.5)
                          :life-fn  life-fn
                          :delay-fn (constant-delay-distribution-fn 0.0)
                          :scaling-fn  (%uniform-scaling-clsr 100.0)
                          :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range 8.0 15.0))
                          :alpha-fn    (%smooth-alpha-fading-clsr 2.0)
                          :gravity   +zero-vec+
                          :width  .1
                          :height .1
                          :respawn nil))

(defun make-smoke-trail-element (pos compiled-shaders
                                 &key
                                   (scaling-clsr (%uniform-scaling-clsr (lcg-next-in-range 3.0
                                                                                           4.5))))
  (make-particles-cluster 'smoke-puff
                          1
                          compiled-shaders
                          :texture (texture:get-texture texture:+smoke-particle+)
                          :pos     pos
                          :v0-fn    (gaussian-velocity-constant-fn +y-axe+ 0.0)
                          :mass-fn  (gaussian-distribution-fn 1.0 0.001)
                          :life-fn  (gaussian-distribution-fn 200.0 1.0)
                          :delay-fn (constant-delay-distribution-fn 0.0)
                          :scaling-fn scaling-clsr
                          :rotation-fn (%uniform-rotation-clsr #'(lambda ()
                                                                   (lcg-next-in-range -0.25 0.25)))
                          :alpha-fn    (%smooth-alpha-fading-clsr 5.5)
                          :gravity   (vec 0.0 0.02 0.0)
                          :width  .5
                          :height .5
                          :respawn nil))

(defclass smoke-trail (minimal-particle-effect)
  ((frequency-smoke
    :initform 30
    :initarg  :frequency-smoke
    :accessor frequency-smoke)
   (scaling-trail-clsr
    :initform (%uniform-scaling-clsr (lcg-next-in-range 3.0 4.5))
    :initarg  :scaling-trail-clsr
    :accessor scaling-trail-clsr)
   (smoke-ct
    :initform 0
    :initarg  :smoke-ct
    :accessor smoke-ct)))

(defmethod calculate ((object smoke-trail) dt)
  (with-accessors ((frequency-smoke frequency-smoke)
                   (smoke-ct smoke-ct)
                   (children children)
                   (particles particles)
                   (pos pos)
                   (particles-positions particles-positions)
                   (particle-min-y particle-min-y)
                   (compiled-shaders compiled-shaders)
                   (scaling-trail-clsr scaling-trail-clsr)
                   (el-time el-time)) object
    (when (and (= (rem smoke-ct frequency-smoke) 0)
               (not (removeable-from-world-p object)))
      (loop
         for particle across particles
         for i from 0 by 3             do
           (let ((smoke-pos (vec (fast-glaref particles-positions i)
                                 (fast-glaref particles-positions (+ i 1))
                                 (fast-glaref particles-positions (+ i 2)))))
             (when (and (d> (particle-life particle) 0.0)
                        (d> (elt smoke-pos 1) 0.0))
               (mtree:add-child object (make-smoke-trail-element smoke-pos
                                                                 compiled-shaders
                                                                 :scaling-clsr
                                                                 scaling-trail-clsr))))))
    (setf smoke-ct (1+ smoke-ct)))
  (call-next-method))

(defun make-smoke-trail (pos dir num compiled-shaders
                         &key
                           (smoke-frequency 10)
                           (texture (texture:get-texture texture:+smoke-particle+))
                           (v0-fn   (gaussian-velocity-distribution-fn dir
                                                                       4.5
                                                                       1.0
                                                                       (d/ +pi/2+ 2.0)))
                           (scaling-trail-clsr (%uniform-scaling-clsr (lcg-next-in-range 3.0 4.5)))
                           (scaling-clsr       (%no-scaling-clsr)))
  (let ((res (make-particles-cluster 'smoke-trail
                                     num
                                     compiled-shaders
                                     :texture texture
                                     :pos     pos
                                     :min-y   0.0
                                     :v0-fn   v0-fn
                                     :mass-fn  (gaussian-distribution-fn 1.0 0.1)
                                     :life-fn  (gaussian-distribution-fn 10.0 5.0)
                                     :delay-fn (gaussian-distribution-fn 0.0 .0001)
                                     :scaling-fn scaling-clsr
                                     :gravity (vec* +particle-gravity+ 0.3)
                                     :width  .1
                                     :height .1)))
    (setf (scaling-trail-clsr res)  scaling-trail-clsr)
    (setf (frequency-smoke res) smoke-frequency)
    (setf (noise-scale     res) 0.01)
    res))

(defclass fire-dart (cluster-w-gravity) ())

(defmethod initialize-instance :after ((object fire-dart)
                                       &key
                                         (texture (texture:get-texture
                                                   texture:+fire-particle+))
                                         &allow-other-keys)
  (setf (use-blending-p object) t)
  (setf (integrator-shader object) :fire-dart-integrator)
  (setf (texture-object object) texture))

(defmethod calculate ((object fire-dart) dt)
  (with-accessors ((compiled-shaders compiled-shaders)
                   (gravity          gravity)
                   (el-time          el-time)) object
    (setf el-time
          (d+ (start-time object)
              (d* (animation-speed object) (current-time object))))
    (with-feedback-calculate (object dt)
      (uniformfv compiled-shaders :gravity gravity))
    (do-children-mesh (i object)
      (calculate i dt))))

(defmethod render ((object fire-dart) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)
                   (el-time el-time)
                   (renderp renderp)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (and renderp
               (> (length triangles) 0))
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (with-depth-mask-disabled
            (cl-gl-utils:with-blending
              (gl:blend-equation :func-add)
              (gl:blend-func :one :one)
              (use-program compiled-shaders :particles-fire-dart)
              (gl:active-texture :texture0)
              (texture:bind-texture texture-object)
              (uniformf  compiled-shaders :time  el-time)
              (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
              (uniform-matrix compiled-shaders :modelview-matrix 4
                              (vector (matrix* camera-vw-matrix
                                               (elt view-matrix 0)
                                               (elt model-matrix 0)))
                              nil)
              (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
              (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
              (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
              (gl:blend-equation :func-add))))))
    (do-children-mesh (c object)
      (render c renderer))))

;; (defmethod removeable-from-world-p ((object fire-dart))
;;   (mark-for-remove-p object))

(defun make-fire-dart (pos dir compiled-shaders)
  (make-particles-cluster 'fire-dart
                          1500
                          compiled-shaders
                          :texture (texture:get-texture texture:+fire-particle+)
                          :pos     pos
                          :v0-fn  (gaussian-velocity-distribution-fn (vec-negate dir)
                                                                     1.0
                                                                     .1
                                                                     (d/ +pi/2+ 5.0))
                          :mass-fn  (gaussian-distribution-fn 1.0 .1)
                          :life-fn  (gaussian-distribution-fn 5.0 1.1)
                          :delay-fn (gaussian-distribution-fn 0.0 10.1)
                          :gravity  dir
                          :scaling-fn  #'(lambda ()
                                           #'(lambda (p dt)
                                               (declare (ignore p dt))
                                               1.0))
                          :alpha-fn   (%smooth-alpha-fading-clsr 7.0)
                          :width  1.0
                          :height 1.0
                          :respawn t))

(defun make-fire-dart-level-2 (pos dir compiled-shaders)
  (make-particles-cluster 'fire-dart
                          1500
                          compiled-shaders
                          :texture (texture:get-texture texture:+fire-particle+)
                          :pos     pos
                          :v0-fn  (gaussian-velocity-distribution-fn (vec-negate dir)
                                                                     1.0
                                                                     .1
                                                                     (d/ +pi/2+ 5.0))
                          :mass-fn  (gaussian-distribution-fn 1.0 .1)
                          :life-fn  (gaussian-distribution-fn 8.0 1.1)
                          :delay-fn (gaussian-distribution-fn 0.0 10.1)
                          :gravity  dir
                          :scaling-fn  #'(lambda ()
                                           #'(lambda (p dt)
                                               (declare (ignore p dt))
                                               1.0))
                          :alpha-fn   (%smooth-alpha-fading-clsr 10.0)
                          :width  2.0
                          :height 2.0
                          :respawn t))

(defun make-fire-dart-level-1 (pos dir compiled-shaders)
  (make-particles-cluster 'fire-dart
                          1500
                          compiled-shaders
                          :texture (texture:get-texture texture:+fire-particle+)
                          :pos     pos
                          :v0-fn  (gaussian-velocity-distribution-fn (vec-negate dir)
                                                                     1.0
                                                                     .1
                                                                     (d/ +pi/2+ 5.0))
                          :mass-fn  (gaussian-distribution-fn 1.0 .1)
                          :life-fn  (gaussian-distribution-fn 5.0 1.1)
                          :delay-fn (gaussian-distribution-fn 0.0 10.1)
                          :gravity  dir
                          :scaling-fn  #'(lambda ()
                                           #'(lambda (p dt)
                                               (declare (ignore p dt))
                                               1.0))
                          :alpha-fn   (%smooth-alpha-fading-clsr 7.0)
                          :width  .5
                          :height .5
                          :respawn t))

(defun make-fire-dart-level-0 (pos dir compiled-shaders)
  (make-particles-cluster 'fire-dart
                          1500
                          compiled-shaders
                          :texture (texture:get-texture texture:+fire-particle+)
                          :pos     pos
                          :v0-fn  (gaussian-velocity-distribution-fn (vec-negate dir)
                                                                     .8
                                                                     .01
                                                                     (d/ +pi/2+ 5.0))
                          :mass-fn  (gaussian-distribution-fn 1.0 .1)
                          :life-fn  (gaussian-distribution-fn 5.0 1.1)
                          :delay-fn (gaussian-distribution-fn 0.0 10.1)
                          :gravity  dir
                          :scaling-fn (%uniform-scaling-clsr 1.0)
                          :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                          :width  .1
                          :height .1
                          :respawn t))

(defun make-fire-bulb (pos dir compiled-shaders)
  (make-particles-cluster 'fire-dart
                          700
                          compiled-shaders
                          :texture (texture:get-texture texture:+fire-particle+)
                          :pos     pos
                          :v0-fn  (gaussian-velocity-distribution-fn dir
                                                                     .1
                                                                     .1
                                                                     (d/ +pi/2+ 5.0))
                          :mass-fn  (gaussian-distribution-fn 1.0 .1)
                          :life-fn  (gaussian-distribution-fn 7.0 1.1)
                          :delay-fn (gaussian-distribution-fn 0.0 10.1)
                          :gravity  dir
                          :scaling-fn (%uniform-scaling-clsr 5.0)
                          :alpha-fn   (%smooth-alpha-fading-clsr 7.0)
                          :width  1.0
                          :height 1.0
                          :respawn nil))

(defclass spell-decal (minimal-particle-effect)
  ((blend-dst
    :initform :one
    :initarg  :blend-dst
    :accessor blend-dst)))

(defmethod bubbleup-modelmatrix ((object spell-decal))
  (let ((res      (matrix* (translate (pos     object))
                           (scale     (scaling object))
                           (quat->matrix (quat-rotate-to-vec +entity-forward-direction+
                                                             (dir object)
                                                             :fallback-axis +y-axe+))
                           (quat->matrix (quat-rotate-to-vec +entity-up-direction+
                                                             (up object)
                                                             :fallback-axis +z-axe+))))
        (par-mesh (mtree:parent object)))
    (when par-mesh
      (setf res (matrix* (elt (model-matrix par-mesh) 0) res)))
    (nremove-rotation  res)
    (setf (model-matrix object) res)))

(defmethod initialize-instance :after ((object spell-decal)
                                       &key
                                         (texture (texture:get-texture
                                                   texture:+blood-particle+))
                                         &allow-other-keys)
  (setf (use-blending-p object) t)
  (setf (integrator-shader object) :blood-integrator)
  (setf (texture-object object) texture)
  (transform-vertices object (rotate-around +x-axe+ +pi/2+)))

(defmethod render ((object spell-decal) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)
                   (blend-dst blend-dst)
                   (el-time el-time)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (with-depth-mask-disabled
            (with-blending
              (with-no-cull-face
                (gl:blend-equation :func-add)
                (gl:blend-func :src-alpha blend-dst)
                (use-program compiled-shaders :particles-spell-decals)
                (gl:active-texture :texture0)
                (texture:bind-texture texture-object)
                (uniformf compiled-shaders :time  el-time)
                (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
                (uniform-matrix compiled-shaders :modelview-matrix 4
                                (vector (matrix* camera-vw-matrix
                                                 (elt view-matrix 0)
                                                 (elt model-matrix 0)))
                                nil)
                (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
                (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
                (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
                (gl:blend-equation :func-add)))))))))

(defclass aerial-explosion (minimal-particle-effect end-life-trigger)
  ())

(defmethod initialize-instance :after ((object aerial-explosion)
                                       &key
                                         (texture (texture:get-texture
                                                   texture:+cross-particle+))
                                         &allow-other-keys)
  (setf (use-blending-p object) t)
  (setf (integrator-shader object) :blood-integrator)
  (setf (texture-object object) texture)
  (action-scheduler:end-of-life-remove-from-action-scheduler object
                                                             action-scheduler:particle-effect-action))

(defmethod calculate :after ((object aerial-explosion) dt)
  (with-maybe-trigger-end-of-life (object (removeable-from-world-p object))))

(defmethod render ((object aerial-explosion) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (el-time el-time)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (with-depth-mask-disabled
            (with-blending
              (gl:blend-equation :func-add)
              (gl:blend-func :src-alpha :one-minus-src-alpha)
              (use-program compiled-shaders :particles-aerial-explosion)
              (gl:active-texture :texture0)
              (texture:bind-texture texture-object)
              (uniformf  compiled-shaders :time  el-time)
              (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
              (uniform-matrix compiled-shaders :modelview-matrix 4
                              (vector (matrix* camera-vw-matrix
                                               (elt view-matrix 0)
                                               (elt model-matrix 0)))
                              nil)
              (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
              (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
              (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
              (gl:blend-equation :func-add))))))
    (do-children-mesh (c object)
      (render c renderer))))

(defun make-spell-decal (pos compiled-shaders
                         &key
                           (texture
                            (random-elt (list-of-texture-by-tag +texture-tag-decals-cure+)))
                           (color-fn  (%constant-color-clsr §cffffffff))
                           (delay-fn  (gaussian-distribution-fn 1.0 0.01))
                           (blend-dst :one))
  (let ((res (make-particles-cluster 'spell-decal
                                     1
                                     compiled-shaders
                                     :texture texture
                                     :pos     pos
                                     :v0-fn  (constantly +zero-vec+)
                                     :mass-fn  (gaussian-distribution-fn 1.0 .1)
                                     :life-fn  (gaussian-distribution-fn 30.0 0.0)
                                     :delay-fn delay-fn
                                     :gravity    (vec 0.0 -1e-5 0.0)
                                     :rotation-fn
                                     #'(lambda ()
                                         (let ((fn-enz (funcall (%limited-scaling-clsr .6 20.0)))
                                               (fn-lin (funcall (%uniform-scaling-clsr .2))))

                                           #'(lambda (p dt)
                                               (d+ (funcall fn-lin p dt)
                                                   (funcall fn-enz p dt)))))
                                     :scaling-fn  (%limited-scaling-clsr .4 120.0)
                                     :alpha-fn   (%sin-alpha-fading-clsr .20)
                                     :color-fn   color-fn
                                     :width  .1
                                     :height .1
                                     :respawn nil)))
    (setf (blend-dst res) blend-dst)
    res))

(defun make-circular-wave (pos compiled-shaders
                           &key
                             (fading-rate 10.8)
                             (scaling-rate 400.0)
                             (texture (random-elt (list-of-texture-by-tag +texture-tag-decals-circular-wave+)))
                             (delay-fn (constantly 0.0))
                             (gradient (color-utils:make-gradient
                                        (color-utils:make-gradient-color 0.0 §cffffffff)
                                        (color-utils:make-gradient-color 1.0 §cff0000ff)))
                             (life-fn   (gaussian-distribution-fn 10.0 0.0)))
  (make-particles-cluster 'spell-decal
                          1
                          compiled-shaders
                          :texture texture
                          :pos     pos
                          :v0-fn  (constantly +zero-vec+)
                          :mass-fn  (gaussian-distribution-fn 1.0 .1)
                          :life-fn  life-fn
                          :delay-fn delay-fn
                          :gravity    (vec 0.0 -1e-5 0.0)
                          :rotation-fn #'(lambda ()
                                           #'(lambda (p dt)
                                               (declare (ignore p dt))
                                               0.0))
                          :scaling-fn  (%uniform-scaling-clsr scaling-rate)
                          :alpha-fn    (%exp-alpha-fading-clsr fading-rate)
                          :color-fn    (%gradient-color-clsr gradient 2.0)
                          :width  .1
                          :height .1
                          :respawn nil))

(defun make-circular-wave-level-2 (pos compiled-shaders)
  (let ((texture (random-elt (list-of-texture-by-tag +texture-tag-decals-circular-wave+))))
    (make-circular-wave pos
                        compiled-shaders
                        :fading-rate 2.0
                        :scaling-rate 4000.0
                        :texture texture)))

(defun make-circular-wave-level-1 (pos compiled-shaders)
  (let ((texture (random-elt (list-of-texture-by-tag +texture-tag-decals-circular-wave+))))
    (make-circular-wave pos
                        compiled-shaders
                        :fading-rate 10.8
                        :scaling-rate 800.0
                        :life-fn (gaussian-distribution-fn 3.0 0.0)
                        :texture texture)))

(defun make-circular-wave-level-0 (pos compiled-shaders)
  (let ((texture (random-elt (list-of-texture-by-tag +texture-tag-decals-circular-wave+))))
    (make-circular-wave pos
                        compiled-shaders
                        :fading-rate 10.2
                        :scaling-rate 200.0
                        :texture texture)))

;; (defun %set-fireball-callback (ball trail)
;;   (let ((saved-callback (end-of-life-callback ball)))
;;     (setf (end-of-life-callback ball)
;;        #'(lambda ()
;;            (funcall saved-callback)
;;            (setf (triangles trail) '())))))

(defun %set-fireball-callback (ball trail)
  (setf (end-of-life-callback ball)
          #'(lambda ()
              (setf (triangles trail) '()))))

(defun make-fireball-level-2 (pos dir compiled-shaders)
  (let* ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
         (gradient-ball (color-utils:make-gradient
                         (color-utils:make-gradient-color 0.0  §cffffffff)
                         (color-utils:make-gradient-color 0.2  §cffd403ff)
                         (color-utils:make-gradient-color 0.5  §cff1403ff)
                         (color-utils:make-gradient-color 1.0  §c494949f0)))
         (gradient-trail (color-utils:make-gradient
                          (color-utils:make-gradient-color 0.0  §cffffff00)
                          (color-utils:make-gradient-color 0.5  §cffd403ff)
                          (color-utils:make-gradient-color 0.7  §cff1403ff)
                          (color-utils:make-gradient-color 1.0  §c494949f0)))
         (ball (make-particles-cluster 'aerial-explosion
                                       100
                                       compiled-shaders
                                       :forces (vector (friction-force-clsr 3.6))
                                       :texture texture
                                       :pos     pos
                                       :v0-fn  (gaussian-velocity-distribution-fn +y-axe+
                                                                                  3.8
                                                                                  3.0
                                                                                  +2pi+)
                                       :mass-fn  (gaussian-distribution-fn 1.0 .2)
                                       :life-fn  (gaussian-distribution-fn 3.0 1.1)
                                       :delay-fn (constantly 0.0)
                                       :gravity    (vec 0.0 -1e-5 0.0)
                                       :scaling-fn (%limited-scaling-clsr 0.2 70.0)
                                       :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -3.0
                                                                                               3.0))
                                       :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                                       :color-fn   (%gradient-color-clsr gradient-ball .15)
                                       :width  .1
                                       :height .1
                                       :respawn t))
         (trail (make-particles-cluster 'aerial-explosion
                                        10
                                        compiled-shaders
                                        :forces     (vector (constant-force-clsr dir))
                                        :texture    texture
                                        :pos        +zero-vec+
                                        :v0-fn      (gaussian-velocity-distribution-fn dir
                                                                                       1.0
                                                                                       0.1
                                                                                       0.001)
                                        :mass-fn     (gaussian-distribution-fn 1.0 .2)
                                        :life-fn     (constantly 5.0)
                                        :delay-fn    (gaussian-distribution-fn 5.0 1.0)
                                        :gravity     (vec 0.0 -1e-5 0.0)
                                        :scaling-fn  (%uniform-scaling-clsr 3.0)
                                        :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -2.0
                                                                                                2.0))
                                        :alpha-fn   (%smooth-alpha-fading-clsr 8.0)
                                        :color-fn   (%gradient-color-clsr gradient-trail 0.15)
                                        :width      .5
                                        :height     .5
                                        :respawn    t)))
    (mtree:add-child ball trail)
    (%set-fireball-callback ball trail)
    ball))

(defun make-fireball-level-1 (pos dir compiled-shaders)
  (let* ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
         (gradient-ball (color-utils:make-gradient
                         (color-utils:make-gradient-color 0.0  §cffffffff)
                         (color-utils:make-gradient-color 0.2  §cffd403ff)
                         (color-utils:make-gradient-color 0.5  §cff1403ff)
                         (color-utils:make-gradient-color 1.0  §c494949f0)))
         (gradient-trail (color-utils:make-gradient
                          (color-utils:make-gradient-color 0.0  §cffffff00)
                          (color-utils:make-gradient-color 0.5  §cffd403ff)
                          (color-utils:make-gradient-color 0.7  §cff1403ff)
                          (color-utils:make-gradient-color 1.0  §c494949f0)))
         (ball (make-particles-cluster 'aerial-explosion
                                       50
                                       compiled-shaders
                                       :forces (vector (friction-force-clsr 4.6))
                                       :texture texture
                                       :pos     pos
                                       :v0-fn  (gaussian-velocity-distribution-fn +y-axe+
                                                                                  1.8
                                                                                  0.5
                                                                                  +2pi+)
                                       :mass-fn  (gaussian-distribution-fn 1.0 .2)
                                       :life-fn  (gaussian-distribution-fn 3.0 1.1)
                                       :delay-fn (constantly 0.0)
                                       :gravity    (vec 0.0 -1e-5 0.0)
                                       :scaling-fn (%limited-scaling-clsr 0.2 70.0)
                                       :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -2.0
                                                                                               2.0))
                                       :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                                       :color-fn   (%gradient-color-clsr gradient-ball .15)
                                       :width  .03
                                       :height .03
                                       :respawn t))
         (trail (make-particles-cluster 'aerial-explosion
                                        100
                                        compiled-shaders
                                        :forces     (vector (constant-force-clsr dir))
                                        :texture    texture
                                        :pos        +zero-vec+
                                        :v0-fn      (gaussian-velocity-distribution-fn dir
                                                                                       1.0
                                                                                       0.001
                                                                                       1.5)
                                        :mass-fn     (gaussian-distribution-fn 1.0 .2)
                                        :life-fn     (constantly 5.0)
                                        :delay-fn    (gaussian-distribution-fn 5.0 1.0)
                                        :gravity     (vec 0.0 -1e-5 0.0)
                                        :scaling-fn  (%uniform-scaling-clsr 1.5)
                                        :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -2.0
                                                                                                2.0))
                                        :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                                        :color-fn   (%gradient-color-clsr gradient-trail 0.15)
                                        :width      .035
                                        :height     .035
                                        :respawn    t)))
    (mtree:add-child ball trail)
    (%set-fireball-callback ball trail)
    ball))

(defun make-fireball-level-0 (pos dir compiled-shaders)
  (let* ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
         (gradient-ball (color-utils:make-gradient
                         (color-utils:make-gradient-color 0.0  §cffffffff)
                         (color-utils:make-gradient-color 0.2  §cffd403ff)
                         (color-utils:make-gradient-color 0.5  §cff1403ff)
                         (color-utils:make-gradient-color 1.0  §c494949f0)))
         (gradient-trail (color-utils:make-gradient
                          (color-utils:make-gradient-color 0.0  §cffffff00)
                          (color-utils:make-gradient-color 0.5  §cffd403ff)
                          (color-utils:make-gradient-color 0.7  §cff1403ff)
                          (color-utils:make-gradient-color 1.0  §c494949f0)))
         (ball (make-particles-cluster 'aerial-explosion
                                       20
                                       compiled-shaders
                                       :forces (vector (friction-force-clsr 8.6))
                                       :texture texture
                                       :pos     pos
                                       :v0-fn  (gaussian-velocity-distribution-fn +y-axe+
                                                                                  1.8
                                                                                  0.5
                                                                                  +2pi+)
                                       :mass-fn  (gaussian-distribution-fn 1.0 .2)
                                       :life-fn  (gaussian-distribution-fn 5.0 0.1)
                                       :delay-fn (gaussian-distribution-fn 0.0 1.1)
                                       :gravity    (vec 0.0 -1e-5 0.0)
                                       :scaling-fn (%limited-scaling-clsr 0.01 70.0)
                                       :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -12.0
                                                                                               12.0))
                                       :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                                       :color-fn   (%gradient-color-clsr gradient-ball .15)
                                       :width  .02
                                       :height .02
                                       :respawn t))
         (trail (make-particles-cluster 'aerial-explosion
                                        50
                                        compiled-shaders
                                        :forces     (vector (constant-force-clsr dir))
                                        :texture    texture
                                        :pos        +zero-vec+
                                        :v0-fn      (gaussian-velocity-distribution-fn dir
                                                                                       1.0
                                                                                       0.1
                                                                                       0.01)
                                        :mass-fn     (gaussian-distribution-fn 1.0 .2)
                                        :life-fn     (constantly 5.0)
                                        :delay-fn    (gaussian-distribution-fn 5.0 1.0)
                                        :gravity     (vec 0.0 -1e-5 0.0)
                                        :scaling-fn  (%uniform-scaling-clsr 1.5)
                                        :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -2.0
                                                                                                2.0))
                                        :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                                        :color-fn   (%gradient-color-clsr gradient-trail 0.15)
                                        :width      .02
                                        :height     .02
                                        :respawn    t)))
    (mtree:add-child ball trail)
    (%set-fireball-callback ball trail)
    ball))

(defun make-burning-cloud (pos compiled-shaders)
  (let ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
        (gradient (color-utils:make-gradient
                   (color-utils:make-gradient-color 0.0  §cffffffff)
                   (color-utils:make-gradient-color 0.2  §cffd403ff)
                   (color-utils:make-gradient-color 0.5  §cff1403ff)
                   (color-utils:make-gradient-color 1.0  §c494949f0))))
    (make-particles-cluster 'aerial-explosion
                            100
                            compiled-shaders
                            :forces (vector (friction-force-clsr 3.6))
                            :texture texture
                            :pos     pos
                            :v0-fn  (gaussian-velocity-distribution-fn +y-axe+
                                                                       10.8
                                                                       5.0
                                                                       +2pi+)
                            :mass-fn  (gaussian-distribution-fn 1.0 .2)
                            :life-fn  (gaussian-distribution-fn 20.0 5.1)
                            :delay-fn (gaussian-distribution-fn 0.0 .01)
                            :gravity    (vec 0.0 -1e-5 0.0)
                            :scaling-fn (%limited-scaling-clsr 0.2 70.0)
                            :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -2.0 2.0))
                            :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                            :color-fn   (%smooth-gradient-color-clsr gradient .1)
                            :width  .1
                            :height .1
                            :respawn t)))

(defun make-aerial-explosion-level-0 (pos compiled-shaders)
  (let ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
        (gradient (color-utils:make-gradient
                   (color-utils:make-gradient-color 0.0  §cffffffff)
                   (color-utils:make-gradient-color 0.2  §cffd403ff)
                   (color-utils:make-gradient-color 0.5  §cff1403ff)
                   (color-utils:make-gradient-color 1.0  §c494949f0))))
    (make-particles-cluster 'aerial-explosion
                            10
                            compiled-shaders
                            :remove-starting-delay t
                            :forces (vector (friction-force-clsr 3.6))
                            :texture texture
                            :pos     pos
                            :v0-fn  (gaussian-velocity-distribution-fn +y-axe+
                                                                       5.8
                                                                       5.0
                                                                       +2pi+)
                            :mass-fn  (gaussian-distribution-fn 1.0 .2)
                            :life-fn  (gaussian-distribution-fn 2.5  0.75)
                            :delay-fn (gaussian-distribution-fn 0.5 .1)
                            :gravity    (vec 0.0 -1e-5 0.0)
                            :scaling-fn (%limited-scaling-clsr 0.2 70.0)
                            :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -1.0 1.0))
                            :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                            :color-fn   (%smooth-gradient-color-clsr gradient 1.0)
                            :width  .01
                            :height .01
                            :respawn nil)))

(defun make-aerial-explosion-level-1 (pos compiled-shaders)
  (let* ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 §cffffffff)
                    (color-utils:make-gradient-color 0.2 §cffd403ff)
                    (color-utils:make-gradient-color 0.5 §cff1403ff)
                    (color-utils:make-gradient-color 1.0 §c494949f0)))
         (flame     (make-particles-cluster 'aerial-explosion
                                            20
                                            compiled-shaders
                                            :remove-starting-delay t
                                            :forces (vector (friction-force-clsr 3.6))
                                            :texture texture
                                            :pos     pos
                                            :v0-fn  (gaussian-velocity-distribution-fn +y-axe+
                                                                                       4.8
                                                                                       5.0
                                                                                       +2pi+)
                                            :mass-fn  (gaussian-distribution-fn 1.0 .2)
                                            :life-fn  (gaussian-distribution-fn 2.5 0.75)
                                            :delay-fn (gaussian-distribution-fn 0.5 .1)
                                            :gravity    (vec 0.0 -1e-5 0.0)
                                            :scaling-fn (%limited-scaling-clsr 0.2 70.0)
                                            :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -1.0 1.0))
                                            :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                                            :color-fn   (%smooth-gradient-color-clsr gradient 1.0)
                                            :width  .03
                                            :height .03
                                            :respawn nil)))
    (mtree:add-child flame (make-smoke-trail +zero-vec+
                                             +y-axe+
                                             3
                                             compiled-shaders
                                             :scaling-trail-clsr
                                             (%uniform-scaling-clsr (lcg-next-in-range .5
                                                                                       0.2))))
    (mtree:add-child flame (make-circular-wave-level-0 +zero-vec+ compiled-shaders))
    flame))

(defun make-aerial-explosion-level-2 (pos compiled-shaders)
  (let* ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0  §cffffffff)
                    (color-utils:make-gradient-color 0.2  §cffd403ff)
                    (color-utils:make-gradient-color 0.5  §cff1403ff)
                    (color-utils:make-gradient-color 1.0  §c494949f0)))
         (flame (make-particles-cluster 'aerial-explosion
                                        100
                                        compiled-shaders
                                        :forces (vector (friction-force-clsr 3.6))
                                        :texture texture
                                        :pos     pos
                                        :v0-fn  (gaussian-velocity-distribution-fn +y-axe+
                                                                                   10.8
                                                                                   5.0
                                                                                   +2pi+)
                                        :mass-fn  (gaussian-distribution-fn 1.0 .2)
                                        :life-fn  (gaussian-distribution-fn 3.0 1.0)
                                        :delay-fn (gaussian-distribution-fn 0.5 .1)
                                        :gravity    (vec 0.0 -1e-5 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.2 70.0)
                                        :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -2.0
                                                                                                2.0))
                                        :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 1.0)
                                        :width  .1
                                        :height .1
                                        :respawn nil))
         (v0-fn-smoke (gaussian-velocity-distribution-fn +y-axe+ 9.0
                                                         1.0
                                                         (d/ +pi/2+ 2.0))))
    (mtree:add-child flame (make-smoke-trail +zero-vec+ +y-axe+ 3 compiled-shaders
                                             :v0-fn v0-fn-smoke
                                             :scaling-trail-clsr
                                             (%uniform-scaling-clsr (lcg-next-in-range .7
                                                                                       0.2))))
    (mtree:add-child flame (make-circular-wave-level-1 +zero-vec+ compiled-shaders))
    flame))

(defun make-aerial-explosion-level-3 (pos compiled-shaders)
  (let* ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0  §cffffffff)
                    (color-utils:make-gradient-color 0.2  §cffd403ff)
                    (color-utils:make-gradient-color 0.5  §cff1403ff)
                    (color-utils:make-gradient-color 1.0  §c494949f0)))
         (flame (make-particles-cluster 'aerial-explosion
                                        100
                                        compiled-shaders
                                        :forces (vector (friction-force-clsr 1.6))
                                        :texture texture
                                        :pos     pos
                                        :v0-fn  (gaussian-velocity-distribution-fn +y-axe+
                                                                                   25.8
                                                                                   5.0
                                                                                   +2pi+)
                                        :mass-fn  (gaussian-distribution-fn 1.0 .2)
                                        :life-fn  (gaussian-distribution-fn 3.0 1.5)
                                        :delay-fn (gaussian-distribution-fn 0.25 .1)
                                        :gravity    (vec 0.0 -1e-5 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 50.0)
                                        :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -2.0
                                                                                                2.0))
                                        :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 1.0)
                                        :width  .2
                                        :height .2
                                        :respawn nil))
         (debris (make-particles-cluster 'debris
                          100
                          compiled-shaders
                          :texture (random-elt (list-of-texture-by-tag
                                                +texture-tag-explosion-debris+))
                          :pos     +zero-vec+
                          :min-y   (d- (d- (elt pos 1) +zero-height+))
                          :v0-fn   (gaussian-velocity-distribution-fn +y-axe+
                                                                      70.0
                                                                      1.0
                                                                      (d/ +pi/2+ 2.0))
                          :mass-fn  (gaussian-distribution-fn 0.1 0.05)
                          :life-fn  (gaussian-distribution-fn 12.0 5.0)
                          :delay-fn (gaussian-distribution-fn 0.0 .0001)
                          :scaling-fn  #'(lambda ()
                                           (let ((time 0.0))
                                             #'(lambda (p dt)
                                                 (declare (ignore p))
                                                 (incf time dt)
                                                 (dmax 0.0 (d+ 1.0 (d* -0.2 time))))))

                          :gravity +particle-gravity+
                          :width  .22
                          :height .22))
         (flame2 (make-particles-cluster 'aerial-explosion
                                         100
                                         compiled-shaders
                                         :forces (vector (friction-force-clsr 1.6))
                                         :texture texture
                                         :pos     +zero-vec+
                                         :v0-fn  (gaussian-velocity-distribution-fn +y-axe+
                                                                                    20.8
                                                                                    5.0
                                                                                    +2pi+)
                                         :mass-fn  (gaussian-distribution-fn 1.0 .2)
                                         :life-fn  (gaussian-distribution-fn 20.0 5.1)
                                         :delay-fn (gaussian-distribution-fn .6 .1)
                                         :gravity    (vec 0.0 -1e-5 0.0)
                                         :scaling-fn (%limited-scaling-clsr 0.1 70.0)
                                         :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -2.0
                                                                                                 2.0))
                                         :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
                                         :color-fn   (%smooth-gradient-color-clsr gradient 1.0)
                                         :width  .2
                                         :height .2
                                         :respawn nil))
         (v0-fn-smoke (gaussian-velocity-distribution-fn +y-axe+ 11.0
                                                         1.0
                                                         (d/ +pi/2+ 2.0))))
    (mtree:add-child flame (make-smoke-trail +zero-vec+ +y-axe+ 5 compiled-shaders
                                             :v0-fn v0-fn-smoke
                                             :scaling-trail-clsr
                                             (%uniform-scaling-clsr (lcg-next-in-range 3.0
                                                                                       4.5))))
    (mtree:add-child flame debris)
    (mtree:add-child flame flame2)
    (mtree:add-child flame (make-circular-wave-level-2 +zero-vec+ compiled-shaders))
    flame))

(defclass cure-spark (minimal-particle-effect cluster-w-global-life end-life-trigger) ())

(defmethod initialize-instance :after ((object cure-spark) &key &allow-other-keys)
  (setf (noise-scale object) 0.01))

(defmethod calculate :after ((object cure-spark) dt)
  (with-maybe-trigger-end-of-life (object (removeable-from-world-p object))))

(defmethod render ((object cure-spark) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (el-time el-time)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (with-depth-mask-disabled
            (cl-gl-utils:with-blending
              (gl:blend-equation :func-add)
              (gl:blend-func :src-alpha :one)
              (use-program compiled-shaders :particles-aerial-explosion)
              (gl:active-texture :texture0)
              (texture:bind-texture texture-object)
              (uniformf  compiled-shaders :time  el-time)
              (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
              (uniform-matrix compiled-shaders :modelview-matrix 4
                              (vector (matrix* camera-vw-matrix
                                               (elt view-matrix 0)
                                               (elt model-matrix 0)))
                              nil)
              (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
              (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
              (gl:draw-arrays :triangles 0 (f* 3 (length triangles)))
              (gl:blend-equation :func-add))))))
    (do-children-mesh (c object)
      (render c renderer))))

(defclass teleport-particle (spell-decal cluster-w-global-life end-life-trigger) ())

(defmethod calculate :after ((object teleport-particle) dt)
  (with-maybe-trigger-end-of-life (object (removeable-from-world-p object))))

(defun make-teleport (pos compiled-shaders &optional (num 7))
  (let* ((actual-pos (vec (elt pos 0)
                          (d+ (elt pos 1)
                              (d* 2.0 +terrain-chunk-tile-size+))
                          (elt pos 2)))
         (min-y (d- (d- (elt actual-pos 1) (d- +zero-height+ 2.0))))
         (texture   (random-elt (list-of-texture-by-tag +texture-tag-teleport-particle+)))
         (size-fn   #'(lambda (c)
                       (declare (ignore c))
                       0.5))
         (gradient  (color-utils:make-gradient
                     (color-utils:make-gradient-color 0.0 billboard:+healing-color+)
                     (color-utils:make-gradient-color 0.5 §cc99ff99ff)
                     (color-utils:make-gradient-color 1.0 §cc99ffccff)))
         (particles (make-particles-cluster 'teleport-particle
                                        num
                                        compiled-shaders
                                        :remove-starting-delay nil
                                        :forces                #()
                                        :texture               texture
                                        :pos                   actual-pos
                                        :min-y                 min-y
                                        :particle-pos-fn nil
                                        :v0-fn           (gaussian-velocity-constant-fn
                                                          (vec-negate +y-axe+)
                                                          1.0)
                                        :mass-fn         #'(lambda () 1.0)

                                        :life-fn         (gaussian-distribution-fn 2.2 0.05)
                                        :delay-fn        (constant-delay-distribution-fn 0.8)
                                        :gravity         (vec 0.0 -1.1 0.0)
                                        :scaling-fn      (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn     (%no-rotation-clrs)
                                        :alpha-fn        (%smooth-alpha-fading-clsr 3.0)
                                        :color-fn        (%smooth-gradient-color-clsr gradient 2.0)
                                        :width           .2
                                        :height          .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t)))
    (setf (global-life particles) 2)
    particles))

(defun make-teleport-level-1 (pos compiled-shaders)
  (make-teleport pos compiled-shaders 1))

(defun make-teleport-level-2 (pos compiled-shaders)
  (make-teleport pos compiled-shaders 2))

(defun make-teleport-level-3 (pos compiled-shaders)
  (make-teleport pos compiled-shaders 7))

(defun make-cure-level-2 (pos compiled-shaders)
  (let* ((min-y (d- (d- (elt pos 1) +zero-height+)))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-cure-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .05 .1))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+healing-color+)
                    (color-utils:make-gradient-color 0.5 §cc99ff99ff)
                    (color-utils:make-gradient-color 1.0 §cc99ffccff)))
         (spark (make-particles-cluster 'cure-spark
                                        400
                                        compiled-shaders
                                        :remove-starting-delay t
                                        :forces   #()
                                        :texture  texture
                                        :pos      pos
                                        :min-y   min-y
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 2.0)
                                                                                                (d/ +terrain-chunk-tile-size+ 2.0)
                                                                                                1)
                                                                            0)))
                                                               (vec (elt xy 0)
                                                                    (particle-height cluster)
                                                                    (elt xy 1))))
                                        :v0-fn    (gaussian-velocity-distribution-fn +y-axe+
                                                                                     2.0
                                                                                     1.0
                                                                                     (d/ +pi/2+
                                                                                         5.0))
                                        :mass-fn  (gaussian-distribution-fn 0.8 .2)
                                        :life-fn  (gaussian-distribution-fn 2.0 0.2)
                                        :delay-fn (gaussian-distribution-fn 10.00 2.50)
                                        :gravity    (vec 0.0 1.1 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 2.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                        :width  .2
                                        :height .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t))
         (decal (make-spell-decal +zero-vec+
                                  compiled-shaders
                                  :texture (random-elt (list-of-texture-by-tag +texture-tag-decals-cure+))
                                  :color-fn (%constant-color-clsr §cc99ff99ff)
                                  :delay-fn (gaussian-distribution-fn 1.0 0.1))))

    (mtree:add-child spark decal)
    (setf (global-life spark) 200)
    spark))

(defun make-cure-level-1 (pos compiled-shaders)
  (let* ((min-y (d- (d- (elt pos 1) +zero-height+)))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-cure-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .05 .005))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+healing-color+)
                    (color-utils:make-gradient-color 0.5 §cc99ff99ff)
                    (color-utils:make-gradient-color 1.0 §cc99ffccff)))
         (spark (make-particles-cluster 'cure-spark
                                        400
                                        compiled-shaders
                                        :remove-starting-delay t
                                        :forces   #()
                                        :texture  texture
                                        :pos      pos
                                        :min-y   min-y
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 3.5)
                                                                                                (d/ +terrain-chunk-tile-size+ 3.5)
                                                                                                1)
                                                                            0)))
                                                               (vec (elt xy 0)
                                                                    (particle-height cluster)
                                                                    (elt xy 1))))
                                        :v0-fn    (gaussian-velocity-distribution-fn +y-axe+
                                                                                     2.0
                                                                                     1.0
                                                                                     (d/ +pi/2+
                                                                                         5.0))
                                        :mass-fn  (gaussian-distribution-fn 0.8 .2)
                                        :life-fn  (gaussian-distribution-fn 2.0 0.2)
                                        :delay-fn (gaussian-distribution-fn 10.5 1.0)
                                        :gravity    (vec 0.0 1.1 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 3.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                        :width  .2
                                        :height .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t)))
    (setf (global-life spark) 200)
    spark))

(defun make-cure-level-0 (pos compiled-shaders)
  (let* ((min-y (d- (d- (elt pos 1) +zero-height+)))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-cure-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .02 .001))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+healing-color+)
                    (color-utils:make-gradient-color 0.5 §cc99ff99ff)
                    (color-utils:make-gradient-color 1.0 §cc99ffccff)))
         (spark (make-particles-cluster 'cure-spark
                                        20
                                        compiled-shaders
                                        :forces   #()
                                        :texture  texture
                                        :pos      pos
                                        :min-y   min-y
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 4.0)
                                                                                                (d/ +terrain-chunk-tile-size+ 4.0)
                                                                                                1)
                                                                            0)))
                                                               (vec (elt xy 0)
                                                                    (particle-height cluster)
                                                                    (elt xy 1))))
                                        :v0-fn    (gaussian-velocity-distribution-fn +y-axe+
                                                                                     2.0
                                                                                     1.0
                                                                                     (d/ +pi/2+
                                                                                         5.0))
                                        :mass-fn  (gaussian-distribution-fn 0.8 .2)
                                        :life-fn  (gaussian-distribution-fn 1.0 0.2)
                                        :delay-fn (gaussian-distribution-fn 0.05 .001)
                                        :gravity    (vec 0.0 1.1 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 2.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                        :width  .2
                                        :height .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t)))
    (setf (global-life spark) 300)
    spark))

(defun make-poison-level-2 (pos compiled-shaders)
  (let* ((min-y (d- (d- (elt pos 1) +zero-height+)))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-poison-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .05 .1))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+poison-damage-color+)
                    (color-utils:make-gradient-color 0.2 §c7000a8ff)
                    (color-utils:make-gradient-color 1.0 §c380054ff)))
         (spark (make-particles-cluster 'cure-spark
                                        400
                                        compiled-shaders
                                        :remove-starting-delay t
                                        :forces   (vector (attraction-force-clsr 10.0))
                                        :texture  texture
                                        :pos      pos
                                        :min-y   min-y
                                        :particle-pos-fn
                                        #'(lambda (cluster)
                                            (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 1.5)
                                                                               (d/ +terrain-chunk-tile-size+ 1.5)
                                                                               1)
                                                           0)))
                                              (vec (elt xy 0)
                                                   (d* (particle-height cluster)
                                                       (lcg-next-upto (d* (particle-height cluster)
                                                                          50.0)))
                                                   (elt xy 1))))
                                        :v0-fn      (gaussian-velocity-distribution-fn +y-axe+
                                                                                       2.0
                                                                                       1.0
                                                                                       (d/ +pi/2+
                                                                                           5.0))
                                        :mass-fn    (gaussian-distribution-fn 0.8 .2)
                                        :life-fn    (gaussian-distribution-fn 2.0 0.2)
                                        :delay-fn   (gaussian-distribution-fn 10.00 2.50)
                                        :gravity    (vec 0.0 1e-5 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 2.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                        :width              .2
                                        :height             .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t))
         (decal (make-spell-decal +zero-vec+
                                  compiled-shaders
                                  :texture
                                  (random-elt (list-of-texture-by-tag +texture-tag-decals-poison+))
                                  :color-fn (%constant-color-clsr billboard:+poison-damage-color+)
                                  :delay-fn (gaussian-distribution-fn 1.0 0.1))))
    (mtree:add-child spark decal)
    (setf (global-life spark) 150)
    spark))

(defun %enqueue-effect-billboard (particles-mesh pos image-file texture-horizontal-offset)
  (billboard:enqueue-animated-billboard pos
                                        (res:get-resource-file image-file +animation-texture-dir+)
                                        (state            particles-mesh)
                                        (compiled-shaders particles-mesh)
                                        :w
                                        (d* +terrain-chunk-tile-size+ 6.0)
                                        :h
                                        (d* +terrain-chunk-tile-size+ 6.0)
                                        :duration/2                2.5
                                        :loop-p                    t
                                        :texture-horizontal-offset texture-horizontal-offset))

(defun %add-effect-billboard (particles-mesh pos image-file
                              texture-horizontal-offset
                              duration
                              &key
                                (frequency-animation 1.0)
                                (w (d* +terrain-chunk-tile-size+ 6.0))
                                (h (d* +terrain-chunk-tile-size+ 6.0))
                                (delay                           0))
  (billboard:add-animated-billboard pos
                                    (res:get-resource-file image-file +animation-texture-dir+)
                                    (state            particles-mesh)
                                    (compiled-shaders particles-mesh)
                                    :delay                     delay
                                    :frequency-animation       frequency-animation
                                    :w                         w
                                    :h                         h
                                    :duration/2                duration
                                    :loop-p                    nil
                                    :texture-horizontal-offset texture-horizontal-offset))

(defun add-hit-0-effect-billboard (mesh pos)
  (%add-effect-billboard mesh pos "hit-0.tga" 0.25 0.001
                         :frequency-animation 4
                         :w                   +terrain-chunk-tile-size+
                         :h                   +terrain-chunk-tile-size+))

(defun add-hit-1-effect-billboard (mesh pos)
  (%add-effect-billboard mesh pos "hit-1.tga" 0.0625 0.001 :frequency-animation 2))

(defun add-hit-2-effect-billboard (mesh pos)
 (%add-effect-billboard mesh pos "hit-2.tga" 0.0625 0.001 :frequency-animation 2))

(defun add-hit-3-effect-billboard (mesh pos)
  (loop for delay from 0 below 30 by 10 do
       (let ((size (d+ (d* +terrain-chunk-tile-size+ 6.0)
                       (d* 0.8 (d delay)))))
       (%add-effect-billboard mesh pos "hit-1.tga" 0.0625 0.001
                              :w                   (d* +terrain-chunk-tile-size+ 6.0)
                              :h                   size
                              :frequency-animation 2
                              :delay               delay)))
  (add-hit-2-effect-billboard mesh pos))

(defun %enqueue-poison-1-effect-billboard (particles-mesh pos)
  (%enqueue-effect-billboard particles-mesh pos "wasp.tga" 0.02631579))

(defun make-poison-level-1 (pos compiled-shaders)
  (let* ((min-y (d- (d- (elt pos 1) +zero-height+)))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-poison-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .05 .005))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+poison-damage-color+)
                    (color-utils:make-gradient-color 0.2 §c7000a8ff)
                    (color-utils:make-gradient-color 1.0 §c380054ff)))
         (spark (make-particles-cluster 'cure-spark
                                        400
                                        compiled-shaders
                                        :remove-starting-delay t
                                        :forces   (vector (attraction-force-clsr 10.0))
                                        :texture  texture
                                        :pos      pos
                                        :min-y   min-y
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 3.0)
                                                                                                (d/ +terrain-chunk-tile-size+ 3.0)
                                                                                                1)
                                                                            0)))
                                                               (vec (elt xy 0)
                                                                    (d* (particle-height cluster)
                                                                        (lcg-next-upto (d* (particle-height cluster)
                                                                                           60.0)))
                                                                    (elt xy 1))))
                                        :v0-fn    (gaussian-velocity-distribution-fn +y-axe+
                                                                                     3.0
                                                                                     1.0
                                                                                     (d/ +pi/2+
                                                                                         5.0))
                                        :mass-fn  (gaussian-distribution-fn 0.8 .2)
                                        :life-fn  (gaussian-distribution-fn 2.0 0.2)
                                        :delay-fn (gaussian-distribution-fn 10.00 2.50)
                                        :gravity    (vec 0.0 1e-5 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 2.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                        :width  .2
                                        :height .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t)))
    (setf (global-life spark) 200)
    (%enqueue-poison-1-effect-billboard spark pos)
    spark))

(defun make-poison-level-0 (pos compiled-shaders)
  (let* ((min-y (d- (d- (elt pos 1) +zero-height+)))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-poison-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .02 .005))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+poison-damage-color+)
                    (color-utils:make-gradient-color 0.2 §c7000a8ff)
                    (color-utils:make-gradient-color 1.0 §c380054ff)))
         (spark (make-particles-cluster 'cure-spark
                                        200
                                        compiled-shaders
                                        :remove-starting-delay t
                                        :forces   (vector (attraction-force-clsr 10.0))
                                        :texture  texture
                                        :pos      pos
                                        :min-y   min-y
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 3.5)
                                                                                                (d/ +terrain-chunk-tile-size+ 3.5)
                                                                                                1)
                                                                            0)))
                                                               (vec (elt xy 0)
                                                                    (d* (particle-height cluster)
                                                                        (lcg-next-upto (d* (particle-height cluster)
                                                                                           60.0)))
                                                                    (elt xy 1))))
                                        :v0-fn    (gaussian-velocity-distribution-fn +y-axe+
                                                                                     2.0
                                                                                     1.0
                                                                                     (d/ +pi/2+
                                                                                         5.0))
                                        :mass-fn  (gaussian-distribution-fn 0.8 .2)
                                        :life-fn  (gaussian-distribution-fn 2.0 0.2)
                                        :delay-fn (gaussian-distribution-fn 10.00 2.50)
                                        :gravity    (vec 0.0 1e-5 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 2.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                        :width  .2
                                        :height .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t)))
    (setf (global-life spark) 150)
    spark))

(defun make-vampire-level-0 (pos compiled-shaders)
  (flet ((particle-pos-fn ()
           #'(lambda (cluster)
               (let ((xy
                      (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 3.5)
                                               (d/ +terrain-chunk-tile-size+ 3.5)
                                               1)
                           0)))
                 (vec (elt xy 0)
                      (d* (particle-height cluster)
                          (lcg-next-upto (d* (particle-height cluster)
                                             60.0)))
                      (elt xy 1))))))
    (let* ((min-y (d- (d- (elt pos 1) +zero-height+)))
           (texture  (random-elt (list-of-texture-by-tag +texture-tag-poison-particle+)))
           (size-fn  #'(lambda (c)
                         (declare (ignore c))
                         (max 0.0 (gaussian-probability .02 .005))))
           (gradient (color-utils:make-gradient
                      (color-utils:make-gradient-color 0.0 billboard:+poison-damage-color+)
                      (color-utils:make-gradient-color 0.2 §c7000a8ff)
                      (color-utils:make-gradient-color 1.0 §c380054ff)))
           (spark (make-particles-cluster 'cure-spark
                                          20
                                          compiled-shaders
                                          :remove-starting-delay t
                                          :forces   (vector (attraction-force-clsr 10.0))
                                          :texture  texture
                                          :pos      pos
                                          :min-y   min-y
                                          :particle-pos-fn (particle-pos-fn)
                                          :v0-fn    (gaussian-velocity-distribution-fn +y-axe+
                                                                                       2.0
                                                                                       1.0
                                                                                       (d/ +pi/2+
                                                                                           5.0))
                                          :mass-fn  (gaussian-distribution-fn 0.8 .2)
                                          :life-fn  (gaussian-distribution-fn 2.0 0.2)
                                          :delay-fn (gaussian-distribution-fn 0.0 0.01)
                                          :gravity    (vec 0.0 1e-5 0.0)
                                          :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                          :rotation-fn (%no-rotation-clrs)
                                          :alpha-fn   (%smooth-alpha-fading-clsr 2.0)
                                          :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                          :width  .2
                                          :height .2
                                          :particle-height-fn nil    ;; will use particle-width-fn
                                          :particle-width-fn  size-fn
                                          :respawn t)))
      (setf (global-life spark) 15)
      spark)))

(defun make-vampire-level-2 (pos compiled-shaders)
  (let* ((min-y (d- (d- (elt pos 1) +zero-height+)))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-poison-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .05 .005))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+poison-damage-color+)
                    (color-utils:make-gradient-color 0.2 §c7000a8ff)
                    (color-utils:make-gradient-color 1.0 §c380054ff)))
         (spark (make-particles-cluster 'cure-spark
                                        400
                                        compiled-shaders
                                        :remove-starting-delay t
                                        :forces   (vector (attraction-force-clsr 10.0))
                                        :texture  texture
                                        :pos      pos
                                        :min-y   min-y
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 3.0)
                                                                                                (d/ +terrain-chunk-tile-size+ 3.0)
                                                                                                1)
                                                                            0)))
                                                               (vec (elt xy 0)
                                                                    (d* (particle-height cluster)
                                                                        (lcg-next-upto (d* (particle-height cluster)
                                                                                           60.0)))
                                                                    (elt xy 1))))
                                        :v0-fn    (gaussian-velocity-distribution-fn +y-axe+
                                                                                     3.0
                                                                                     1.0
                                                                                     (d/ +pi/2+
                                                                                         5.0))
                                        :mass-fn  (gaussian-distribution-fn 0.8 .2)
                                        :life-fn  (gaussian-distribution-fn 2.0 0.2)
                                        :delay-fn (gaussian-distribution-fn 10.00 2.50)
                                        :gravity    (vec 0.0 1e-5 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 2.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                        :width  .2
                                        :height .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t))
         (decal (make-spell-decal (vec 0.0 (d- (d- (elt pos 1) +zero-height+)) 0.0)
                                  compiled-shaders
                                  :texture
                                  (random-elt (list-of-texture-by-tag +texture-tag-decals-hellish+))
                                  :color-fn   (%constant-color-clsr billboard:+poison-damage-color+)
                                  :delay-fn   (gaussian-distribution-fn 1.0 0.1)
                                  :blend-dst  :one-minus-src-alpha)))
    (mtree:add-child spark decal)
    (setf (global-life spark) 200)
    spark))


(defun make-heal-level-2 (pos compiled-shaders)
  (let* ((actual-pos (vec (elt pos 0)
                          (d+ (elt pos 1)
                              (d* 10.0 +terrain-chunk-tile-size+))
                          (elt pos 2)))
         (min-y (d- (d- (elt actual-pos 1) (d- +zero-height+ 10.0))))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-poison-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .05 .1))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+blessing-color+)
                    (color-utils:make-gradient-color 1.0 §cffffff)))
                 (spark (make-particles-cluster 'cure-spark
                                        400
                                        compiled-shaders
                                        :remove-starting-delay t
                                        :forces   #()
                                        :texture  texture
                                        :pos      actual-pos
                                        :min-y    min-y
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (declare (ignore cluster))
                                                             (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 1.5)
                                                                                                (d/ +terrain-chunk-tile-size+ 1.5)
                                                                                                1)
                                                                            0)))
                                                               (vec (elt xy 0)
                                                                    0.0
                                                                    (elt xy 1))))
                                        :v0-fn      (gaussian-velocity-distribution-fn +y-axe+
                                                                                       0.0
                                                                                       0.1
                                                                                       (d/ +pi/2+
                                                                                           5.0))
                                        :mass-fn    (gaussian-distribution-fn 0.8 .2)
                                        :life-fn    (gaussian-distribution-fn 5.0 0.2)
                                        :delay-fn   (gaussian-distribution-fn 10.00 2.50)
                                        :gravity    (vec 0.0 -1.5 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 6.5)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                        :width  .2
                                        :height .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t))
         (decal (make-spell-decal (vec 0.0 (d- (d- (elt actual-pos 1) +zero-height+)) 0.0)
                                  compiled-shaders
                                  :texture
                                  (random-elt (list-of-texture-by-tag +texture-tag-decals-heal+))
                                  :color-fn (%constant-color-clsr billboard:+blessing-color+)
                                  :delay-fn (gaussian-distribution-fn 1.0 0.1))))
    (mtree:add-child spark decal)
    (bubbleup-modelmatrix spark)
    (billboard:apply-tooltip spark
                             billboard:+tooltip-revive-char+
                             :color     billboard:+blessing-color+
                             :font-type gui:+tooltip-font-handle+)
    (setf (global-life spark) 200)
    spark))

(defun make-heal-level-1 (pos compiled-shaders)
  (let* ((actual-pos (vec (elt pos 0)
                          (d+ (elt pos 1)
                              (d* 2.0 +terrain-chunk-tile-size+))
                          (elt pos 2)))
         (min-y (d- (d- (elt actual-pos 1) (d- +zero-height+ 10.0))))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-poison-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .025 .05))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+blessing-color+)
                    (color-utils:make-gradient-color 1.0 §cffffff)))
         (spark (make-particles-cluster 'cure-spark
                                        50
                                        compiled-shaders
                                        :remove-starting-delay t
                                        :forces   #()
                                        :texture  texture
                                        :pos      actual-pos
                                        :min-y    min-y
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (declare (ignore cluster))
                                                             (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 3.0)
                                                                                                (d/ +terrain-chunk-tile-size+ 3.0)
                                                                                                1)
                                                                            0)))
                                                               (vec (elt xy 0)
                                                                    0.0
                                                                    (elt xy 1))))
                                        :v0-fn    (gaussian-velocity-distribution-fn +y-axe+
                                                                                     0.0
                                                                                     0.1
                                                                                     (d/ +pi/2+
                                                                                         5.0))
                                        :mass-fn  (gaussian-distribution-fn 0.8 .2)
                                        :life-fn  (gaussian-distribution-fn 2.0 0.2)
                                        :delay-fn (gaussian-distribution-fn 5.00 1.50)
                                        :gravity    (vec 0.0 -1.5 0.0)
                                        :scaling-fn (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 4.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 3.0)
                                        :width  .2
                                        :height .2
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn t)))
    (setf (global-life spark) 100)
    spark))

(defun make-heal-level-0 (pos compiled-shaders)
  (let* ((actual-pos (vec (elt pos 0)
                          (d+ (elt pos 1)
                              +terrain-chunk-tile-size+)
                          (elt pos 2)))
         (min-y (d- (d- (elt actual-pos 1) (d- +zero-height+ 10.0))))
         (texture  (random-elt (list-of-texture-by-tag +texture-tag-poison-particle+)))
         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.0 (gaussian-probability .0125 .025))))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0 billboard:+blessing-color+)
                    (color-utils:make-gradient-color 1.0 §cffffff)))
         (spark (make-particles-cluster 'cure-spark
                                        20
                                        compiled-shaders
                                        :remove-starting-delay t
                                        :forces   #()
                                        :texture  texture
                                        :pos      actual-pos
                                        :min-y    min-y
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (declare (ignore cluster))
                                                             (let ((xy (elt (bivariate-sampling (d/ +terrain-chunk-tile-size+ 3.0)
                                                                                                (d/ +terrain-chunk-tile-size+ 3.0)
                                                                                                1)
                                                                            0)))
                                                               (vec (elt xy 0)
                                                                    0.0
                                                                    (elt xy 1))))
                                        :v0-fn    (gaussian-velocity-distribution-fn +y-axe+
                                                                                     0.0
                                                                                     0.1
                                                                                     (d/ +pi/2+
                                                                                         5.0))
                                        :mass-fn     (gaussian-distribution-fn 0.8 .2)
                                        :life-fn     (gaussian-distribution-fn 2.0 0.2)
                                        :delay-fn    (gaussian-distribution-fn 5.00 1.50)
                                        :gravity     (vec 0.0 -1.5 0.0)
                                        :scaling-fn  (%limited-scaling-clsr 0.1 10.0)
                                        :rotation-fn (%no-rotation-clrs)
                                        :alpha-fn    (%smooth-alpha-fading-clsr 4.0)
                                        :color-fn    (%smooth-gradient-color-clsr gradient
                                                                                  3.0)
                                        :width       .2
                                        :height      .2
                                        :particle-height-fn nil ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :respawn            t)))
    (setf (global-life spark) 10)
    spark))

(defun make-level-up (pos compiled-shaders)
  (let* ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
         (gradient (color-utils:make-gradient
                    (color-utils:make-gradient-color 0.0  §cffffffff)
                    (color-utils:make-gradient-color 0.2  §cffff00ff)
                    (color-utils:make-gradient-color 0.7  §cff0000ff)
                    (color-utils:make-gradient-color 1.0  §c00000000)))

         (size-fn  #'(lambda (c)
                       (declare (ignore c))
                       (max 0.2 (gaussian-probability .6 .1))))
         (flame (make-particles-cluster 'cure-spark
                                        1000
                                        compiled-shaders
                                        :min-y -300
                                        :remove-starting-delay t
                                        :forces (vector (rotation-force-clsr))
                                        :texture texture
                                        :pos     pos
                                        :particle-pos-fn #'(lambda (cluster)
                                                             (declare (ignore cluster))
                                                             (vec (lcg-next-in-range (d* +terrain-chunk-tile-size+
                                                                                         .1)
                                                                                     (d* 1.2
                                                                                         +terrain-chunk-tile-size+))
                                                                  0.0
                                                                  0.0))

                                        :v0-fn  #'(lambda ()
                                                    (vec 0.0
                                                         0.0
                                                         (lcg-next-in-range 11.0 1.2)))
                                        :mass-fn  (gaussian-distribution-fn 10.0 5.0)
                                        :life-fn  (gaussian-distribution-fn 3.5 0.1)
                                        :delay-fn (gaussian-distribution-fn 10.1 2.0)
                                        :gravity    (vec 0.0 20.0 0.0)
                                        :scaling-fn (%limited-scaling-clsr 1.0 2.0)
                                        :rotation-fn (%uniform-rotation-clsr 5.5)
                                        :alpha-fn   (%smooth-alpha-fading-clsr 10.0)
                                        :color-fn   (%smooth-gradient-color-clsr gradient 4.8)
                                        :particle-height-fn nil    ;; will use particle-width-fn
                                        :particle-width-fn  size-fn
                                        :width  1.1
                                        :height 1.1
                                        :respawn t)))
    (setf (global-life flame) 0)
    flame))

(defun add-level-up (entity)
  (game-state:with-world (world (state entity))
    (let ((particles (make-level-up (pos entity) (compiled-shaders entity))))
      (action-scheduler:end-of-life-remove-from-action-scheduler particles
                                                                 action-scheduler:particle-effect-action)
      (action-scheduler:with-enqueue-action (world action-scheduler:particle-effect-action)
        (world:push-entity world particles)))))

(defclass exp-increase-particles (minimal-particle-effect cluster-w-global-life end-life-trigger)
  ())

(defmethod initialize-instance :after ((object exp-increase-particles)
                                       &key
                                         (texture (texture:get-texture
                                                   texture:+cross-particle+))
                                         &allow-other-keys)
  (setf (use-blending-p    object) t)
  (setf (integrator-shader object) :blood-integrator)
  (setf (texture-object    object) texture)
  (action-scheduler:end-of-life-remove-from-action-scheduler object
                                                             action-scheduler:particle-effect-action))

(defmethod calculate :after ((object exp-increase-particles) dt)
  ;;(misc:dbg "global-life ~a ~a" (global-life object) (removeable-from-world-p object))
  (with-maybe-trigger-end-of-life (object (removeable-from-world-p object))))

(defmethod render ((object exp-increase-particles) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (el-time el-time)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (with-depth-mask-disabled
            (cl-gl-utils:with-blending
              (gl:blend-equation :func-add)
              (gl:blend-func :src-alpha :one)
              (use-program compiled-shaders :particles-up-spark)
              (gl:active-texture :texture0)
              (texture:bind-texture texture-object)
              (uniformf  compiled-shaders :time  el-time)
              (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
              (uniform-matrix compiled-shaders :modelview-matrix 4
                              (vector (matrix* camera-vw-matrix
                                               (elt view-matrix 0)
                                               (elt model-matrix 0)))
                              nil)
              (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
              (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
              (gl:draw-arrays :triangles 0 (f* 3 (length triangles)))
              (gl:blend-equation :func-add))))))))

(defun enqueue-exp-inc-tooltip (entity value)
  (let* ((value-label (text-utils:to-s (truncate value)))
         (label-width (d+ billboard:+default-tooltip-w+
                          (d* 0.3 (d (length value-label))))))
    (billboard:enqueue-tooltip entity
                               (text-utils:strcat billboard:+tooltip-exp-char+ value-label)
                               :width     label-width
                               :color     billboard:+blessing-color+
                               :font-type gui:+tooltip-font-handle+)))

(defun make-exp-increase-particles (pos compiled-shaders
                                    &key
                                      (radius +terrain-chunk-tile-size+)
                                      (num-particles 200))
  (flet ((position-gen-fn ()
           #'(lambda (a)
               (declare (ignore a))
               (let* ((angle         (lcg-next-in-range 0.0 +2pi+))
                      (actual-radius (gaussian-probability 0.1 radius))
                      (start         (vec actual-radius 0.0 0.0)))
                 (transform-point start (rotate-around +y-axe+ angle))))))
    (let* ((texture   (random-elt (list-of-texture-by-tag +texture-tag-increase-exp+)))
           (gradient  (color-utils:make-gradient
                       (color-utils:make-gradient-color 0.0 §cff00ffff)
                       (color-utils:make-gradient-color 0.5 §cffffffff)
                       (color-utils:make-gradient-color 1.0 §cffff00ff)))
           (particles (make-particles-cluster 'exp-increase-particles
                                              num-particles
                                              compiled-shaders
                                              :remove-starting-delay nil
                                              :texture               texture
                                              :pos                   pos
                                              :min-y                 -50.0
                                              :particle-pos-fn (position-gen-fn)
                                              :v0-fn           (gaussian-velocity-constant-fn
                                                                +y-axe+
                                                                5.0)
                                              :mass-fn   #'(lambda () 1.0)
                                              :life-fn   (gaussian-distribution-fn .2 0.1)
                                              :delay-fn
                                              (constant-delay-distribution-fn 0.001)
                                              :gravity   (vec 0.0 40.0 0.0)
                                              :color-fn
                                              (%smooth-gradient-color-clsr gradient 0.1)
                                              :alpha-fn  (%smooth-alpha-fading-clsr .5)
                                              :width               .05
                                              :height              .8
                                              :particle-height-fn nil
                                              :particle-width-fn  nil
                                              :respawn t)))
      (setf (global-life particles) 50)
      particles)))

(defun add-exp-increase (entity value)
  (cond
    ((<= 0 value  9)
     ;; TODO
     )
    ((<= 10 value 50)
     (enqueue-exp-inc-tooltip entity value))
    (t
     (game-state:with-world (world (state entity))
       (let ((sparks (make-exp-increase-particles (pos entity) (compiled-shaders entity))))
         (action-scheduler:with-enqueue-action (world action-scheduler:particle-effect-action)
           (world:push-entity world sparks)
           (enqueue-exp-inc-tooltip entity value)))))))
