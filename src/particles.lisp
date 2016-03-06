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

(alexandria:define-constant +attribute-center-pos-location+      1 :test #'=)

(alexandria:define-constant +attribute-delay-location+           3 :test #'=)

(alexandria:define-constant +attribute-life-location+            4 :test #'=)

(alexandria:define-constant +attribute-scaling-location+         5 :test #'=)

(alexandria:define-constant +transform-vbo-count+                9 :test #'=)

(alexandria:define-constant +appended-vbo-count+                 4 :test #'=)

(defstruct particle
  (mass      1.0               :type desired-type)
  (v0        (vec 0.0 0.0 0.0) :type vec)
  (position  (vec 0.0 0.0 0.0) :type vec)
  (life      10.0              :type desired-type)
  (delay     10.0              :type desired-type)
  (scaling   #'(lambda (particle dt)
		 (declare (ignore particle dt))
		 1.0)
	                       :type function)
  (respawn   nil)) ; not used

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
   (particles
    :initform nil
    :initarg  :particles
    :accessor particles
    :type     (simple-array particles (*)))))

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

(defgeneric populate-particles-scaling-array (object dt))

(defgeneric bind-computational-buffers (object))

(defgeneric feedback-output-array-size (object))

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

(defmethod populate-particles-scaling-array ((object particles-cluster) dt)
  (with-accessors ((particles particles)
		   (particles-scaling particles-scaling)) object
    (loop
       for particle across particles
       for ct from 0 by 6          do
	 (let ((scaling (funcall (particle-scaling particle) particle dt)))
	   (loop for i from 0 below 6 by 1 do
		(setf (fast-glaref particles-scaling (+ ct i))
		      scaling))))))

(defmethod removeable-from-world ((object particles-cluster))
  (loop for p across (particles object) do
       (when (> (particle-life p) 0.0)
	 (return-from removeable-from-world nil)))
  t)

(defmethod aabb ((object particles-cluster))
  (with-accessors ((particles particles)) object
    (let ((res (make-instance 'aabb)))
      (loop for particle across particles do
	   (expand res (particle-position particle)))
      res)))

(defmethod destroy :after ((object particles-cluster))
  (with-accessors ((particles-positions particles-positions)
		   (particles-v0 particles-v0)
		   (particles-vt particles-vt)
		   (particles-masses particles-masses)
		   (particles-output-positions particles-output-positions)
		   (particles-delay-feedback particles-delay-feedback)
		   (particles-delay particles-delay)
		   (particles-life  particles-life)
		   (particles-scaling particles-scaling)
		   (transform-vao transform-vao)
		   (transform-vbo-input transform-vbo-input)
		   (transform-vbo-output transform-vbo-output)) object
    (when +debug-mode+
      (misc:dbg "destroy particle cluster ~a" (id object)))
    (setf particles-positions        nil
	  particles-v0               nil
	  particles-vt               nil
	  particles-masses           nil
	  particles-delay-feedback   nil
	  particles-delay            nil
	  particles-life             nil
	  particles-scaling          nil
	  particles-output-positions nil
	  transform-vao              nil
	  transform-vbo-input        nil
	  transform-vbo-output       nil)))

(defmethod make-data-for-opengl :after ((object particles-cluster))
  (with-accessors ((particles-positions particles-positions)
		   (particles-v0               particles-v0)
		   (particles-vt               particles-vt)
		   (particles-masses           particles-masses)
		   (particles-delay-feedback   particles-delay-feedback)
		   (particles-delay            particles-delay)
		   (particles-life             particles-life)
		   (particles-scaling          particles-scaling)
		   (particles-output-positions particles-output-positions)
		   (transform-vao              transform-vao)
		   (transform-vbo-input        transform-vbo-input)
		   (particles                  particles)) object
    ;; initialize arrays
    (setf particles-positions        (gl:alloc-gl-array :float (* 3 (length particles))))
    (setf particles-output-positions (gl:alloc-gl-array :float (* 18 (length particles))))
    (setf particles-v0               (gl:alloc-gl-array :float (* 3 (length particles))))
    (setf particles-vt               (gl:alloc-gl-array :float (* 3 (length particles))))
    (setf particles-masses           (gl:alloc-gl-array :float (length particles)))
    (setf particles-delay-feedback   (gl:alloc-gl-array :float (length particles)))
    (setf particles-delay            (gl:alloc-gl-array :float (* 6 (length particles))))
    (setf particles-life             (gl:alloc-gl-array :float (* 6 (length particles))))
    (setf particles-scaling          (gl:alloc-gl-array :float (* 6 (length particles))))
    (populate-particles-positions-array        object)
    (populate-particles-output-positions-array object)
    (populate-particles-v0-array               object)
    (populate-particles-vt-array               object)
    (populate-particles-masses-array           object)
    (populate-particles-delay-feedback-array   object)
    (populate-particles-delay-array            object)
    (populate-particles-life-array             object)
    (populate-particles-scaling-array          object 0.0)
    ;; setup finalizer
    (let ((id             (slot-value object 'id))
	  (pos-in         (slot-value object 'particles-positions))
	  (velo-t0        (slot-value object 'particles-v0))
	  (velo-t         (slot-value object 'particles-vt))
	  (masses         (slot-value object 'particles-masses))
	  (delay-feedback (slot-value object 'particles-delay-feedback))
	  (delay          (slot-value object 'particles-delay))
	  (life           (slot-value object 'particles-life))
	  (scaling        (slot-value object 'particles-scaling))
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
						  delay-feedback
						  delay
						  life
						  scaling
						  pos-out)
					    (append vbo-in vbo-out)
					    vao)
			      (setf pos-in         nil
				    velo-t0        nil
				    velo-t         nil
			      	    masses         nil
				    delay-feedback nil
				    delay          nil
				    life           nil
				    scaling        nil
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
		   (particles-masses           particles-masses)) object
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
      ;; input-delay
      (gl:bind-buffer :array-buffer (vbo-delay-feedback-buffer-handle transform-vbo-input))
      (gl:buffer-data :array-buffer :static-draw particles-delay-feedback)
      (gl:vertex-attrib-pointer +attribute-delay-feedback-location+ 1 :float 0 0 (mock-null-pointer))
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
		   (particles-positions        particles-positions)
		   (particles-output-positions particles-output-positions)
		   (particles-v0               particles-v0)
		   (particles-masses           particles-masses)) object
    ;; calculation
    (loop repeat (length particles) do
	 (quad object particle-width particle-height 0.0 0.0 1.0 1.0 +zero-vec+ nil nil))
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
      object)))

(defmethod calculate ((object particles-cluster) dt)
  (with-accessors ((integrator-shader integrator-shader)
		   (compiled-shaders           compiled-shaders)
		   (transform-vao              transform-vao)
		   (particle-min-y             particle-min-y)
		   (vbo                        vbo)
		   (particles                  particles)
		   (particles-positions        particles-positions)
		   (particles-output-positions particles-output-positions)
		   (particles-v0               particles-v0)
		   (particles-vt               particles-vt)
		   (particles-delay-feedback   particles-delay-feedback)
		   (particles-delay            particles-delay)
		   (particles-life             particles-life)
		   (particles-scaling          particles-scaling)
		   (transform-vbo-input        transform-vbo-input)) object
    (bubbleup-modelmatrix object)
    (with-unbind-vao
      (bind-computational-buffers object)
      (use-program compiled-shaders integrator-shader)
      (uniformf compiled-shaders :dt   dt)
      (uniformf compiled-shaders :min-y particle-min-y)
      (cl-gl-utils:with-rasterizer-discard
	(%gl:bind-buffer-base :transform-feedback-buffer
			      0
			      (vbo-out-position-buffer-handle transform-vbo-input))
	(cl-gl-utils:with-transform-feedback (:points)
	  (gl:bind-vertex-array (elt transform-vao 0))
	  (gl:draw-arrays :points 0 (length particles)))
	(gl:flush)
	(cffi:with-foreign-object (res-array :float (* 6 (length particles)))
	  (%gl:get-buffer-sub-data :transform-feedback-buffer
	   			   0
	   			   (feedback-output-array-size object)
	   			   res-array)
	  (loop for i from 0 below (* 6 (length particles)) by 6 do
	       ;; setting position
	       (setf (cl-gl-utils:fast-glaref particles-positions (/ i 2))
		     (cffi:mem-aref res-array :float i))
	       (setf (cl-gl-utils:fast-glaref particles-positions (+ (/ i 2) 1))
		     (cffi:mem-aref res-array :float (+ i 1)))
	       (setf (cl-gl-utils:fast-glaref particles-positions (+ (/ i 2) 2))
		     (cffi:mem-aref res-array :float (+ i 2)))
	       ;; setting velocity
	       (setf (cl-gl-utils:fast-glaref particles-v0 (/ i 2))
		     (cffi:mem-aref res-array :float (+ i 3) ))
	       (setf (cl-gl-utils:fast-glaref particles-v0 (+ (/ i 2) 1))
		     (cffi:mem-aref res-array :float (+ i 4)))
	       (setf (cl-gl-utils:fast-glaref particles-v0 (+ (/ i 2) 2))
		     (cffi:mem-aref res-array :float (+ i 5)))))))
    (populate-particles-output-positions-array object)
    (populate-particles-delay-feedback-array   object)
    (populate-particles-delay-array            object)
    (populate-particles-life-array             object)
    (populate-particles-scaling-array          object dt)
    (gl:bind-buffer :array-buffer (vbo-center-position-buffer-handle vbo))
    (gl:buffer-sub-data :array-buffer  particles-output-positions)
    (gl:bind-buffer :array-buffer (vbo-delay-feedback-buffer-handle vbo))
    (gl:buffer-sub-data :array-buffer  particles-delay-feedback)
    (gl:bind-buffer :array-buffer (vbo-delay-buffer-handle vbo))
    (gl:buffer-sub-data :array-buffer  particles-delay)
    (gl:bind-buffer :array-buffer (vbo-life-buffer-handle vbo))
    (gl:buffer-sub-data :array-buffer  particles-life)
    (gl:bind-buffer :array-buffer (vbo-scaling-buffer-handle vbo))
    (gl:buffer-sub-data :array-buffer  particles-scaling)
    (loop for particle across particles do
	 (decf (particle-delay particle) dt)
	 (when (< (particle-delay particle) 0.0)
	   (decf (particle-life  particle) dt)))))

(defclass blood (particles-cluster) ())

(defmethod initialize-instance :after ((object blood)
				       &key
					 (texture (texture:get-texture
						   texture:+blood-particle+))
					 &allow-other-keys)
  (setf (use-blending-p object) t)
  (setf (integrator-shader object) :blood-integrator)
  (setf (texture-object object) texture))

(defmethod render ((object blood) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
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
	    (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	    (uniform-matrix compiled-shaders :modelview-matrix 4
			    (vector (matrix* camera-vw-matrix
					     (elt view-matrix 0)
					     (elt model-matrix 0)))
			    nil)
	    (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
	    (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	    (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
	    (gl:blend-equation :func-add)))))))

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

(defun gaussian-distribution-fn (max delta)
  #'(lambda ()
      (max 0.0 (gaussian-probability delta max))))

(defun make-particles-cluster (class particles-count shaders-dict
			       &key
				 (texture     (texture:get-texture
					       texture:+blood-particle+))
				 (v0-fn       #'(lambda () (vec 0.0 40.0 0.0)))
				 (mass-fn     #'(lambda () 1.0))
				 (position-fn #'(lambda () (vec 0.0 0.1 0.0)))
				 (life-fn     #'(lambda () 10.1))
				 (delay-fn    #'(lambda () 10.0))
				 (scaling-fn  #'(lambda (particle dt)
						  (declare (ignore particle dt))
						  1.0))
				 (width       1.0)
				 (height      1.0)
				 (pos         +zero-vec+)
				 (min-y       0.0))
  (let* ((particles (loop repeat particles-count collect
			 (make-particle :mass     (funcall mass-fn)
					:v0       (funcall v0-fn)
					:position (funcall position-fn)
					:scaling  scaling-fn
					:life     (funcall life-fn)
					:delay    (funcall delay-fn))))
	 (cluster  (make-instance class
				  :texture           texture
				  :pos               pos
				  :particle-min-y    min-y
				  :compiled-shaders  shaders-dict
				  :particle-width    width
				  :particle-height   height
				  :particles         (list->simple-array particles nil 'particle))))
    (prepare-for-rendering cluster)))

(defun make-blood-level-0 (pos dir compiled-shaders)
  (particles:make-particles-cluster 'particles:blood
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
				    :width  0.1
				    :height 0.1))

(defun make-blood-level-1 (pos dir compiled-shaders)
  (particles:make-particles-cluster 'particles:blood
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
				    :width  0.1
				    :height 0.1))

(defun make-blood-level-2 (pos dir compiled-shaders)
  (particles:make-particles-cluster 'particles:blood
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
				    :scaling-fn #'(lambda (p dt)
						    (declare (ignore p dt))
						    1.0)

				    :width  .1
				    :height .1))

(defun make-blood-death (pos dir compiled-shaders)
  (particles:make-particles-cluster 'particles:blood
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
				    :scaling-fn #'(lambda (p dt)
						    (declare (ignore p dt))
						    1.0)

				    :width  .1
				    :height .1))

(defun make-debris (pos dir num texture compiled-shaders)
  (particles:make-particles-cluster 'particles:blood
				    num
				    compiled-shaders
				    :texture texture
				    :pos     pos
				    :min-y   (d- +zero-height+ (elt pos 1))
				    :v0-fn   (gaussian-velocity-distribution-fn dir
										3.0
										1.0
										(d/ +pi/2+ 3.0))
				    :mass-fn  (gaussian-distribution-fn 1.0 1.0)
				    :life-fn  (gaussian-distribution-fn 12.0 5.0)
				    :delay-fn (gaussian-distribution-fn 0.0 .0001)
				    :scaling-fn #'(lambda (p dt)
						    (declare (ignore p dt))
						    1.0)

				    :width  .1
				    :height .1))
