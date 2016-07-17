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

(alexandria:define-constant +attribute-position-location+        0 :test #'=)

(alexandria:define-constant +attribute-mass-location+            1 :test #'=)

(alexandria:define-constant +attribute-v0-location+              2 :test #'=)

(alexandria:define-constant +attribute-delay-feedback-location+  3 :test #'=)

(alexandria:define-constant +attribute-force-feedback-location+  4 :test #'=)

(alexandria:define-constant +attribute-center-pos-location+      1 :test #'=)

(alexandria:define-constant +attribute-delay-location+           3 :test #'=)

(alexandria:define-constant +attribute-life-location+            4 :test #'=)

(alexandria:define-constant +attribute-scaling-location+         5 :test #'=)

(alexandria:define-constant +attribute-alpha-location+           6 :test #'=)

(alexandria:define-constant +attribute-rotation-location+        7 :test #'=)

(alexandria:define-constant +attribute-color-location+           8 :test #'=)

(alexandria:define-constant +transform-vbo-count+                9 :test #'=)

(alexandria:define-constant +appended-vbo-count+                 7 :test #'=)

(alexandria:define-constant +particle-gravity+                   (vec 0.0 -9.0 0.0)
  :test #'vec~)

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

(defun %uniform-rotation-clsr (m)
  #'(lambda ()
      (let ((time 0.0))
	#'(lambda (p dt)
	    (declare (ignore p))
	    (incf time dt)
	    (d+ 1.0 (d* m time))))))

(defun %uniform-scaling-clsr (m)
  #'(lambda ()
      (let ((time 0.0))
	#'(lambda (p dt)
	    (declare (ignore p))
	    (incf time dt)
	    (d+ 1.0 (d* m time))))))

(defun %limited-scaling-clsr (rate max)
  #'(lambda ()
      (let ((time 0.0))
	#'(lambda (p dt)
	    (declare (ignore p))
	      (incf time dt)
	      (num:enzyme-kinetics max rate time)))))

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
   (mark-for-remove
    :initform nil
    :initarg  :mark-for-remove
    :reader   mark-for-remove-p
    :writer  (setf mark-for-remove))))

(defmethod initialize-instance :after ((object particles-cluster) &key &allow-other-keys)
  (with-accessors ((particles particles)
		   (particle-width particle-width)
		   (particle-height particle-height)) object
    (let ((w/2 (d* particle-width 0.5))
	  (h/2 (d* particle-height 0.5)))
      (loop repeat (length particles) do
	   (quad object particle-width particle-height
		 0.0 0.0 1.0 1.0
		 (vec (d- w/2) (d- h/2) 0.0) ; centering
		 nil nil)))))

(defmacro do-particles ((object particle) &body body)
  `(loop for ,particle across (particles ,object) do
	,@body))

(defmacro gen-populate-array-vec (slot-array slot-struct)
  (let ((fn-name (format-fn-symbol t "populate-~a-array" slot-array)))
    `(progn
       (defgeneric ,fn-name (object))
       (defmethod ,fn-name  ((object particles-cluster))
	 (with-accessors ((particles particles)
			  (,slot-array ,slot-array)) object
	     (loop
		for particle across particles
		for i from 0 by 3             do
		  (setf (cl-gl-utils:fast-glaref ,slot-array i)
			(elt (,slot-struct particle) 0))
		  (setf (cl-gl-utils:fast-glaref ,slot-array (+ i 1))
			(elt (,slot-struct particle) 1))
		  (setf (cl-gl-utils:fast-glaref ,slot-array (+ i 2))
			(elt (,slot-struct particle) 2))))))))

(defmacro gen-populate-array (slot-array slot-struct)
  (let ((fn-name (format-fn-symbol t "populate-~a-array" slot-array)))
    `(progn
       (defgeneric ,fn-name (object))
       (defmethod ,fn-name  ((object particles-cluster))
	 (with-accessors ((particles particles)
			  (,slot-array ,slot-array)) object
	     (loop
		for particle across particles
		for i from 0 by 1             do
		  (setf (cl-gl-utils:fast-glaref ,slot-array i) (,slot-struct particle))))))))

(gen-populate-array-vec particles-positions particle-position)

(gen-populate-array-vec particles-v0             particle-v0)

(gen-populate-array     particles-masses         particle-mass)

(gen-populate-array-vec particles-vt             particle-v0)

(gen-populate-array     particles-delay-feedback particle-delay)

(defun gl-array-copy-multiply (from to length source-step copy-num)
  (loop for ct from 0 below (* source-step length) by source-step
     for ct2 from 0 below (* length source-step copy-num) by (* source-step copy-num) do
       (loop for ct3 from 0 below (* source-step copy-num) by 1 do
	    (setf (fast-glaref to (+ ct2 ct3))
		  (fast-glaref from (+ ct (mod ct3 source-step))))))
  to)

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

(defmethod removeable-from-world ((object particles-cluster))
  (loop for p across (particles object) do
       (when (> (particle-life p) 0.0)
	 (return-from removeable-from-world nil)))
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
    (when +debug-mode+
      (misc:dbg "destroy particle cluster ~a" (id object)))
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
    (let ((id             (slot-value object 'id))
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
			      (when +debug-mode+
				(misc:dbg "finalize destroy particles ~a" id))
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

(defclass blood (cluster-w-gravity)
  ((noise-scale
    :initform 0.0
    :initarg :noise-scale
    :accessor noise-scale)))

(defmethod initialize-instance :after ((object blood)
				       &key
					 (texture (texture:get-texture
						   texture:+blood-particle+))
					 &allow-other-keys)
  (setf (use-blending-p object) t)
  (setf (integrator-shader object) :blood-integrator)
  (setf (texture-object object) texture))

(defmethod calculate ((object blood) dt)
  (with-accessors ((compiled-shaders compiled-shaders)
		   (particle-min-y   particle-min-y)
		   (gravity          gravity)
		   (noise-scale      noise-scale)) object
    (with-feedback-calculate (object dt)
      (uniformfv compiled-shaders :gravity gravity)
      (uniformf  compiled-shaders :noise-scale  noise-scale)
      (uniformf  compiled-shaders :min-y   particle-min-y)))
  (call-next-method)) ;; calculate children too

(defmethod render ((object blood) renderer)
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

(defun make-particles-cluster (class particles-count shaders-dict
			       &key
				 (texture     (texture:get-texture texture:+blood-particle+))
				 (forces      (vector (constant-force-clsr (vec 0.0 0.0 0.0))))
				 (v0-fn       #'(lambda () (vec 0.0 40.0 0.0)))
				 (mass-fn     #'(lambda () 1.0))
				 (position-fn #'(lambda () (vec 0.0 0.1 0.0)))
				 (life-fn     #'(lambda () 10.1))
				 (delay-fn    #'(lambda () 10.0))
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
				 (pos         +zero-vec+)
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
				  :texture           texture
				  :pos               pos
				  :particle-min-y    min-y
				  :compiled-shaders  shaders-dict
				  :particle-width    width
				  :particle-height   height
				  :particles         (list->simple-array particles nil 'particle)
				  :gravity           gravity)))
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

(defclass debris (blood) ())

(defun make-debris (pos dir num texture compiled-shaders)
  (make-particles-cluster 'debris
			  num
			  compiled-shaders
			  :texture texture
			  :pos     pos
			  :min-y   (d- +zero-height+ (elt pos 1))
			  :v0-fn   (gaussian-velocity-distribution-fn dir
								      3.0
								      1.0
								      (d/ +pi/2+ 3.0))
			  :mass-fn  (gaussian-distribution-fn 0.1 0.05)
			  :life-fn  (gaussian-distribution-fn 12.0 5.0)
			  :delay-fn (gaussian-distribution-fn 0.0 .0001)
			  :scaling-fn #'(lambda ()
					  #'(lambda (p dt)
					      (declare (ignore p dt))
					      1.0))
			  :gravity +particle-gravity+
			  :width  .1
			  :height .1))

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

(defun make-smoke-trail-element (pos compiled-shaders &key
							(max-scaling    8.0)
							(scaling-rate   0.8)
							(scaling-sigma  1.0)
							(rate-sigma     0.1))
  (make-particles-cluster 'smoke-puff
			  1
			  compiled-shaders
			  :texture (texture:get-texture texture:+smoke-particle+)
			  :pos     pos
			  :v0-fn    (gaussian-velocity-constant-fn +y-axe+ 0.0)
			  :mass-fn  (gaussian-distribution-fn 1.0 0.001)
			  :life-fn  (gaussian-distribution-fn 200.0 1.0)
			  :delay-fn (constant-delay-distribution-fn 0.0)
			  :scaling-fn (%limited-scaling-clsr (gaussian-probability rate-sigma
										   scaling-rate)
							     (gaussian-probability scaling-sigma
										   max-scaling))
			  :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range .2 0.1))
			  :alpha-fn    (%smooth-alpha-fading-clsr 15.0)
			  :gravity   (vec 0.0 0.01 0.0)
			  :width  .5
			  :height .5
			  :respawn nil))

(defclass smoke-trail (blood)
  ((frequency-smoke
    :initform 30
    :initarg  :frequency-smoke
    :accessor frequency-smoke)
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
		   (el-time el-time)) object
    (when (and (= (rem smoke-ct frequency-smoke) 0)
	       (not (removeable-from-world object)))
      (loop
	 for particle across particles
	 for i from 0 by 3             do
	   (let ((smoke-pos (vec (fast-glaref particles-positions i)
				 (fast-glaref particles-positions (+ i 1))
				 (fast-glaref particles-positions (+ i 2)))))
	     (when (and (d> (particle-life particle) 0.0)
			(d> (elt smoke-pos 1) 0.0))
	       (mtree:add-child object (make-smoke-trail-element smoke-pos compiled-shaders))))))
    (setf smoke-ct (1+ smoke-ct))
    (call-next-method)))

(defun make-smoke-trail (pos dir num compiled-shaders
			 &key
			   (smoke-frequency 10)
			   (texture (texture:get-texture texture:+smoke-particle+)))
  (let ((res (make-particles-cluster 'smoke-trail
				     num
				     compiled-shaders
				     :texture texture
				     :pos     pos
				     :min-y   (d- +zero-height+ (elt pos 1))
				     :v0-fn   (gaussian-velocity-distribution-fn dir
										 4.5
										 1.0
										 (d/ +pi/2+ 2.0))
				     :mass-fn  (gaussian-distribution-fn 1.0 0.1)
				     :life-fn  (gaussian-distribution-fn 10.0 5.0)
				     :delay-fn (gaussian-distribution-fn 0.0 .0001)
				     :scaling-fn #'(lambda ()
						     #'(lambda (p dt)
							 (declare (ignore p dt))
							 1.0))
				     :gravity (vec* +particle-gravity+ 0.3)
				     :width  .1
				     :height .1)))
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
      (uniformfv compiled-shaders :gravity gravity))))

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
		   (el-time el-time)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
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
	      (gl:blend-equation :func-add))))))))

(defmethod removeable-from-world ((object fire-dart))
  (mark-for-remove-p object))

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
			  :width  1.0
			  :height 1.0
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

(defclass spell-decal (blood) ())

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
		   (el-time el-time)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
	(with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	  (with-blending
	    (with-no-cull-face
	      (gl:blend-equation :func-add)
	      (gl:blend-func :src-alpha :one)
	      (use-program compiled-shaders :particles-spell-decals)
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

(defclass aerial-explosion (blood) ())

(defmethod initialize-instance :after ((object aerial-explosion)
 				       &key
 					 (texture (texture:get-texture
 						   texture:+cross-particle+))
 					 &allow-other-keys)
   (setf (use-blending-p object) t)
   (setf (integrator-shader object) :blood-integrator)
   (setf (texture-object object) texture))

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
	      (gl:blend-equation :func-add))))))))

(defun make-spell-decal (pos compiled-shaders
			 &key
			   (texture (random-elt (list-of-texture-by-tag +texture-tag-decals-cure+)))
			   (color-fn (%constant-color-clsr §cffffffff)))
  (make-particles-cluster 'spell-decal
			  1
			  compiled-shaders
			  :texture texture
			  :pos     pos
			  :v0-fn  (constantly +zero-vec+)
			  :mass-fn  (gaussian-distribution-fn 1.0 .1)
			  :life-fn  (gaussian-distribution-fn 30.0 0.0)
			  :delay-fn (gaussian-distribution-fn 1.0 0.01)
			  :gravity    (vec 0.0 -1e-5 0.0)
			  :rotation-fn #'(lambda ()
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
			  :respawn nil))

(defun make-circular-wave (pos compiled-shaders
			   &key
			     (fading-rate 10.8)
			     (scaling-rate 400.0)
			     (texture (random-elt (list-of-texture-by-tag +texture-tag-decals-circular-wave+)))
			     (delay-fn (constantly 0.0))
			     (gradient (color-utils:make-gradient
					(color-utils:make-gradient-color 0.0 §cffffffff)
					(color-utils:make-gradient-color 1.0 §cff0000ff))))
  (make-particles-cluster 'spell-decal
			  1
			  compiled-shaders
			  :texture texture
			  :pos     pos
			  :v0-fn  (constantly +zero-vec+)
			  :mass-fn  (gaussian-distribution-fn 1.0 .1)
			  :life-fn  (gaussian-distribution-fn 10.0 0.0)
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
			:texture texture)))

(defun make-circular-wave-level-0 (pos compiled-shaders)
  (let ((texture (random-elt (list-of-texture-by-tag +texture-tag-decals-circular-wave+))))
    (make-circular-wave pos
			compiled-shaders
			:fading-rate 100.2
			:scaling-rate 800.0
			:texture texture)))

(defun make-aerial-explosion (pos compiled-shaders)
  (let ((texture  (random-elt (list-of-texture-by-tag +texture-tag-aerial-expl-particle+)))
	(gradient (color-utils:make-gradient
		   (color-utils:make-gradient-color 0.0  §cffffffff)
		   (color-utils:make-gradient-color 0.2  §cffd403ff)
		   (color-utils:make-gradient-color 0.5  §cff1403ff)
		   (color-utils:make-gradient-color 1.0  §c494949f0))))
    (pixmap::dump-gradient gradient)
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
			    :delay-fn (gaussian-distribution-fn 0.5 .1)
			    :gravity    (vec 0.0 -1e-5 0.0)
			    :scaling-fn (%limited-scaling-clsr 0.2 70.0)
			    :rotation-fn (%uniform-rotation-clsr (lcg-next-in-range -2.0 2.0))
			    :alpha-fn   (%smooth-alpha-fading-clsr 5.0)
			    :color-fn   (%smooth-gradient-color-clsr gradient 1.0)
			    :width  .1
			    :height .1
			    :respawn nil)))
