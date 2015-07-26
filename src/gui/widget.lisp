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

(in-package :widget)

(alexandria:define-constant +texture-unit-overlay+ 1 :test #'=)

(alexandria:define-constant +spacing+ 2.0 :test #'=)

(alexandria:define-constant +portrait-size+ 64.0 :test #'=)

(alexandria:define-constant +checkbutton-h+ 12.0 :test #'=)

(alexandria:define-constant +input-text-w+ 128.0 :test #'=)

(alexandria:define-constant +input-text-h+ 12.8 :test #'=)

(alexandria:define-constant +slots-per-page-side-size+ 4 :test #'=)

(defparameter *square-button-size* (d/ (d *window-w*) 10.0))

(defparameter *small-square-button-size* (d/ (d *window-w*) 20.0))

(defparameter *file-chooser-w* (d/ (d *window-w*) 3.0))

(defparameter *file-chooser-h* (d/ (d *window-h*) 3.0))

(defparameter *inventory-window-w* (d/ (d *window-w*) 3.0))

(defparameter *inventory-window-h* (d/ (d *window-h*) 2.0))

(defparameter *child-flip-y* t)

(definline widgetp (w)
  (typep w 'widget))

(defmacro with-parent-widget ((par) child &body body)
  `(let ((,par (parent ,child)))
     ,@body))

(defun find-root-widget (widget)
  (if (parent widget)
      (find-root-widget (parent widget))
      widget))

(defclass font-mesh-shell (triangle-mesh-shell) ())

(defmethod render ((object font-mesh-shell) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (texture-object texture-object)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
	(with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	  (use-program compiled-shaders :gui)
	  (gl:active-texture :texture0)
	  (texture:bind-texture texture-object)
	  (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	  (uniformfv compiled-shaders :ia #(1.0 1.0 1.0))
	  (uniform-matrix compiled-shaders :modelview-matrix 4
				   (vector (sb-cga:matrix* camera-vw-matrix
							   (elt view-matrix 0)
							   (elt model-matrix 0)))
				   nil)
	  (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	  (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	  (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))

(defclass simple-gui-mesh (triangle-mesh) ())

(defmethod render ((object simple-gui-mesh) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (texture-object texture-object)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
	(with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	  (use-program compiled-shaders :gui)
	  (gl:active-texture :texture0)
	  (texture:bind-texture texture-object)
	  (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	  (uniformfv compiled-shaders :ia #(1.0 1.0 1.0))
	  (uniform-matrix compiled-shaders :modelview-matrix 4
				   (vector (sb-cga:matrix* camera-vw-matrix
							   (elt view-matrix 0)
							   (elt model-matrix 0)))
				   nil)
	  (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	  (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	  (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))

(defclass reference-sizes ()
  ((top-bar-h
    :initform 30.0
    :initarg  :top-bar-h
    :accessor top-bar-h)
   (top-bar-relative-offset
    :initform 0.1
    :initarg  :top-bar-relative-offset
    :accessor top-bar-relative-offset)
   (top-bar-space-for-title
    :initform 0.25
    :initarg  :top-bar-space-for-title
    :accessor top-bar-space-for-title)
   (frame-relative-offset
    :initform 0.1
    :initarg  :frame-relative-offset
    :accessor frame-relative-offset)
   (button-x-relative
    :initform 0.05
    :initarg  :button-x-relative
    :accessor button-x-relative)
   (button-y-relative
    :initform 0.1
    :initarg  :button-y-relative
    :accessor button-y-relative)
   (button-h-relative
    :initform 0.7
    :initarg  :button-h-relative
    :accessor button-h-relative)
   (title-font-size
    :initform 10.0
    :initarg  :title-font-size
    :accessor title-font-size)
   (title-font-size-scaling
    :initform 0.6
    :initarg  :title-font-size-scaling
    :accessor title-font-size-scaling)
   (left-frame-offset
    :initform 0.062
    :initarg  :left-frame-offset
    :accessor left-frame-offset)
   (top-frame-offset
    :initform 0.062
    :initarg  :top-frame-offset
    :accessor top-frame-offset)
   (bottom-frame-offset
    :initform 0.090
    :initarg  :bottom-frame-offset
    :accessor bottom-frame-offset)
   (h1-font-size
    :initform 20.0
    :initarg  :h1-font-size
    :accessor h1-font-size)
   (h2-font-size
    :initform 18.0
    :initarg  :h2-font-size
    :accessor h2-font-size)
   (h3-font-size
    :initform 15.0
    :initarg  :h3-font-size
    :accessor h3-font-size)
   (h4-font-size
    :initform 12.0
    :initarg  :h4-font-size
    :accessor h4-font-size)
   (standard-font-size
    :initform 10.0
    :initarg  :standard-font-size
    :accessor standard-font-size)
   (button-text-offset-x
    :initform 0.1
    :initarg :button-text-offset-x
    :accessor button-text-offset-x)
   (button-text-fit-height
    :initform 0.6
    :initarg :button-text-fit-height
    :accessor button-text-fit-height)
   (button-label-max-size
    :initform 8.0
    :initarg  :button-label-max-size
    :accessor button-label-max-size)
   (checkbutton-h
    :initform 8.0
    :initarg  :checkbutton-h
    :accessor checkbutton-h)
   (input-text-w
    :initform 128.0
    :initarg  :input-text-w
    :accessor input-text-w)
   (input-text-h
    :initform 12.0
    :initarg  :input-text-h
    :accessor input-text-h)
   (spacing
    :initform 2.0
    :initarg  :spacing
    :accessor spacing)
   (square-button-size
    :initform (d/ (d *window-w*) 10.0)
    :initarg  :square-button-size
    :accessor square-button-size)
   (small-square-button-size
    :initform (d/ (d *window-w*) 20.0)
    :initarg  :small-square-button-size
    :accessor small-square-button-size)
   (tiny-square-button-size
    :initform (d/ (d *window-w*) 30.0)
    :initarg  :tiny-square-button-size
    :accessor tiny-square-button-size)))

(defmethod initialize-instance :after ((object reference-sizes)
				       &key &allow-other-keys)
  (setf (title-font-size object) (d* (top-bar-h object) (title-font-size-scaling object))))

(defgeneric relative-title-space (object))

(defmethod relative-title-space ((object reference-sizes))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (d+ (top-bar-space-for-title object) (d* 2.0 (top-bar-relative-offset object))))

(defparameter *reference-sizes* (make-instance 'reference-sizes))

(defclass widget (triangle-mesh)
  ((width
    :initform 10.0
    :initarg  :width
    :writer (setf width))
   (height
    :initform 10.0
    :initarg  :height
    :writer (setf height))
   (label
    :initform nil
    :initarg  :label
    :accessor label)
   (label-font
    :initform +default-font-handle+
    :initarg  :label-font
    :accessor label-font)
   (label-font-size
    :initform 10.0
    :initarg  :label-font-size
    :accessor label-font-size)
   (shown
    :initform t
    :initarg  :shown
    :accessor shown)
   (focus
    :initform nil
    :initarg  :focus
    :accessor focus)))

(defgeneric width (object))

(defgeneric height (object))

(defmethod width ((object widget))
  (with-slots (width) object
    (d* (elt (scaling object) 0) width)))

(defmethod height ((object widget))
  (with-slots (height) object
    (d* (elt (scaling object) 1) height)))

(defmethod initialize-instance :after ((object widget) &key (x 0.0) (y 0.0) &allow-other-keys)
  (setf (pos object) (sb-cga:vec x y 0.0)))

(defgeneric x (object))

(defgeneric y (object))

(defgeneric (setf x) (new-value object))

(defgeneric (setf y) (new-value object))

(defmethod x ((object widget))
  (elt (pos object) 0))

(defmethod y ((object widget))
  (elt (pos object) 1))

(defmethod (setf x) (new-value (object widget))
  (setf (elt (pos object) 0) new-value))

(defmethod (setf y) (new-value (object widget))
  (setf (elt (pos object) 1) new-value))

(defmethod render ((object widget) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (texture-object texture-object)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)
		   (shown shown)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when shown
      (when (> (length triangles) 0)
	(with-camera-view-matrix (camera-vw-matrix renderer)
	  (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	    (use-program compiled-shaders :gui)
	    (gl:active-texture :texture0)
	    (texture:bind-texture texture-object)
	    (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	    (uniformfv compiled-shaders :ia #(1.0 1.0 1.0))
	    (uniform-matrix compiled-shaders :modelview-matrix 4
				     (vector (sb-cga:matrix* camera-vw-matrix
							     (elt view-matrix 0)
							     (elt model-matrix 0)))
				     nil)
	    (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	    (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	    (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))
      (do-children-mesh (c object)
	(render c renderer)))))

(defgeneric setup-label (object new-label))

(defgeneric hide (object))

(defgeneric show (object))

(defgeneric mouse-over (object x y))

(defgeneric label-width (object))

(defgeneric flip-y (object child))

(defgeneric on-mouse-pressed (object event))

(defgeneric on-mouse-released (object event))

(defgeneric on-mouse-dragged (object event))

(defgeneric on-key-pressed (object event))

(defmethod hide ((object widget))
  (with-accessors ((shown shown)) object
    (setf shown nil)
    (do-children-mesh (child object)
      (setf (shown object) nil))))

(defmethod show ((object widget))
  (with-accessors ((shown shown)) object
    (setf shown t)
    (do-children-mesh (child object)
      (setf (shown object) t))))

(defmethod mouse-over ((object widget) x y)
  (let* ((aabb (aabb object))
	 (aabb2 (vec4 (elt (aabb-p1 aabb) 0) (elt (aabb-p1 aabb) 1)
		      (elt (aabb-p2 aabb) 0) (elt (aabb-p2 aabb) 1))))
    (2d-utils:inside-aabb2-p aabb2 x y)))

(defmethod label-width ((object widget))
  (d* (d (length (label object))) (label-font-size object)))

(defmethod flip-y ((object widget) (child widget))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (d- (height object) (d+ (y child) (height child))))

(defmethod on-mouse-pressed ((object widget) event)
  (top-down-visit object #'(lambda (a) (when (widgetp a) (setf (focus a) nil))))
  (loop for w across (children object) do
       (when (and (widgetp w)
		  (on-mouse-pressed w event))
      	 (return-from on-mouse-pressed t)))
  nil)

(defmethod on-mouse-released ((object widget) event)
  (loop for w across (children object) do
       (when (and (widgetp w)
		  (on-mouse-released w event))
	 (return-from on-mouse-released t)))
  nil)

(defmethod on-mouse-dragged ((object widget) event)
  (loop for w across (children object) do
       (when (and (widgetp w)
		  (on-mouse-dragged w event))
	 (return-from on-mouse-dragged t)))
  nil)

(defmethod on-key-pressed ((object widget) event)
  (loop for w across (children object) do
       (when (and (widgetp w)
		  (on-key-pressed w event))
	 (return-from on-key-pressed t)))
  nil)

(defun common-setup-label (widget new-label)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font label-font)
		   (label-font-size label-font-size)
		   (children children)) widget
    (declare (desired-type label-font-size))
    (with-slots (label) widget
      (declare (simple-string label))
      (remove-all-children widget)
      (setf label new-label)
      (loop for c across label do
	   (let* ((mesh  (get-char-mesh label-font c))
		  (shell (if mesh
			     (fill-shell-from-mesh mesh 'font-mesh-shell)
			     nil)))
	     (when shell
	       (add-child widget shell)))))))

(defun add-quad-for-widget (mesh)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (triangle-mesh mesh))
  ;;  - c +-------+ d
  ;;  |   |\      |     c has coordinates (s, 1.0)
  ;;  |   | \ T2  |
  ;;h |   |  \    |
  ;;  |   |   \   |
  ;;  |   |    \  |      ^ t
  ;;  |   | T1  \ |      |
  ;;  |   |      \|      |    s
  ;;  -   +-------+      +---->
  ;;      a        b
  (quad-w-explicit-texture-coords mesh
				  1.0 1.0
				  (vector (vec2 0.0 0.0)  ; a
					  (vec2 1.0 0.0)  ; b
					  (vec2 0.0 1.0)  ; c
					  (vec2 1.0 0.0)  ; b
					  (vec2 1.0 1.0)  ; d
					  (vec2 0.0 1.0)) ; c
				  +zero-vec+ nil t))

(defclass naked-button (widget)
  ((callback
    :initform #'(lambda (o e) (declare (ignore o e)) nil)
    :initarg  :callback
    :accessor callback)
   (texture-pressed
    :initform nil
    :initarg  :texture-pressed
    :accessor texture-pressed)
   (texture-overlay
    :initform nil
    :initarg  :texture-overlay
    :accessor texture-overlay)
   (current-texture
    :initform nil
    :initarg  :current-texture
    :accessor current-texture)))

(defun hide-parent-cb (widget event)
  (declare (naked-button widget))
  (declare (ignore event))
  (hide (parent widget)))

(defun hide-grandparent-cb (widget event)
  (declare (naked-button widget))
  (declare (ignore event))
  (let ((grandparent (parent (parent widget))))
    (when grandparent
      (hide grandparent))))

(defmethod initialize-instance :after ((object naked-button) &key &allow-other-keys)
  (with-accessors ((width width) (height height)
		   (label-font-size label-font-size)) object
    (when (or (null (texture-object object))
	      (null (texture-pressed object))
	      (null (texture-overlay object)))
      (setf (texture-object  object) (get-texture +button-texture-name+))
      (setf (texture-pressed object) (get-texture +button-pressed-texture-name+))
      (setf (texture-overlay object) (get-texture +button-pressed-texture-name+)))
    (setf (current-texture object) (texture-object object))
    (add-quad-for-widget object)
    (transform-vertices object (sb-cga:scale* (width object) (height object) 1.0))
    (prepare-for-rendering object)))

(defmethod render ((object naked-button) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (current-texture current-texture)
		   (texture-overlay texture-overlay)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)
		   (shown shown)) object
    (declare (texture current-texture texture-overlay))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when shown
      (when (> (length triangles) 0)
	(with-camera-view-matrix (camera-vw-matrix renderer)
	  (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	    (use-program compiled-shaders :gui-naked-button)
	    (gl:active-texture :texture0)
	    (texture:bind-texture current-texture)
	    (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	    (gl:active-texture :texture1)
	    (texture:bind-texture texture-overlay)
	    (uniformi compiled-shaders :texture-overlay +texture-unit-overlay+)
	    (uniformfv compiled-shaders :ia #(1.0 1.0 1.0))
	    (uniform-matrix compiled-shaders :modelview-matrix 4
				     (vector (sb-cga:matrix* camera-vw-matrix
							     (elt view-matrix 0)
							     (elt model-matrix 0)))
				     nil)
	    (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	    (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	    (gl:draw-arrays :triangles 0 (* 3 (length triangles)))))))))

(defmethod on-mouse-pressed ((object naked-button) event)
  (if (mouse-over object (x-event event) (y-event event))
      (setf (current-texture object) (texture-pressed object))
      nil))

(defmethod on-mouse-released ((object naked-button) event)
  (with-accessors ((callback callback)) object
    (cond
      ((and callback
	    (mouse-over object (x-event event) (y-event event)))
       (setf (current-texture object) (texture-object object))
       (funcall callback object event))
      ((mouse-over object (x-event event) (y-event event))
       (setf (current-texture object) (texture-object object)))
      (t
       nil))))

(defmethod on-mouse-dragged ((object naked-button) event)
  (declare (ignore object event))
  nil)

(defclass toggle-button (naked-button)
  ((button-state
    :initform nil
    :initarg  :button-state
    :reader   button-state)
   (group
    :initform nil
    :initarg  :group
    :accessor   group)))

(defmethod initialize-instance :after ((object toggle-button) &key &allow-other-keys))

(defgeneric (setf button-state) (new-state object))

(defgeneric flip-state (object))

(defmethod (setf button-state) (new-state (object toggle-button))
  (with-slots (button-state) object
    (with-accessors ((current-texture current-texture)
		     (texture-pressed texture-pressed)
		     (texture-object  texture-object)) object
      (setf button-state      new-state)
      (setf current-texture   (if button-state
				  texture-pressed
				  texture-object)))))

(defmethod flip-state ((object toggle-button))
  (with-slots (button-state) object
    (with-accessors ((group group)) object
      (setf (button-state object) (not button-state))
      (map nil #'(lambda (a)
		   (when (/= (id a) (id object))
		     (setf (button-state a) (not (button-state object)))))
	   group))))

(defmethod on-mouse-pressed ((object toggle-button) event)
  (if (mouse-over object (x-event event) (y-event event))
      (progn
	(flip-state object)
	(when (callback object)
	  (funcall (callback object) object event))
	t)
      nil))

(defmethod on-mouse-released ((object toggle-button) event)
  nil)

(defclass check-button (toggle-button) ())

(defmethod initialize-instance :after ((object check-button) &key
							       (theme :green)
							       &allow-other-keys)
  (with-accessors ((width width) (height height)
		   (label-font-size label-font-size)) object
    (case theme
      (:green
       (setf (texture-object  object) (get-texture +check-button-texture-name+))
       (setf (texture-pressed object) (get-texture +check-button-checked-green+))
       (setf (texture-overlay object) (get-texture +check-button-overlay+))
       (setf (current-texture object) (texture-object object))))
    (setf (current-texture object) (texture-object object))))

(defmethod flip-state ((object check-button))
  (with-slots (button-state) object
    (with-accessors ((group group)) object
      (when (not (button-state object))
	(setf (button-state object) t)
	(map nil #'(lambda (a)
		     (when (/= (id a) (id object))
		       (setf (button-state a) nil)))
	     group)))))

(defclass signalling-light (check-button) ())

(defmethod initialize-instance :after ((object signalling-light) &key
								   (texture-name nil)
								   &allow-other-keys)
  (when texture-name
    (setf (texture-object  object) (get-texture texture-name))
    (setf (texture-pressed object) nil)
    (setf (texture-overlay object) nil)
    (setf (current-texture object) (get-texture texture-name))))

(defmethod render ((object signalling-light) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (texture-object texture-object)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)
		   (shown shown)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when shown
      (when (> (length triangles) 0)
	(with-camera-view-matrix (camera-vw-matrix renderer)
	  (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	    (use-program compiled-shaders :gui)
	    (gl:active-texture :texture0)
	    (texture:bind-texture texture-object)
	    (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	    (uniformfv compiled-shaders :ia #(1.0 1.0 1.0))
	    (uniform-matrix compiled-shaders :modelview-matrix 4
				     (vector (sb-cga:matrix* camera-vw-matrix
							     (elt view-matrix 0)
							     (elt model-matrix 0)))
				     nil)
	    (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	    (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	    (gl:draw-arrays :triangles 0 (* 3 (length triangles)))))))))

(defmethod (setf button-state) (new-state (object signalling-light))
  (with-slots (button-state) object
    (with-accessors ((current-texture current-texture)
		     (shown           shown)
		     (texture-pressed texture-pressed)
		     (texture-object  texture-object)) object
      (setf button-state      new-state)
      (setf shown             new-state))))

(defmethod on-mouse-pressed ((object signalling-light) event)
  nil)

(defclass button (naked-button) ())

(defmethod initialize-instance :after ((object button) &key &allow-other-keys)
  (setf (label-font-size object) (button-label-max-size *reference-sizes*))
  (with-slots (label) object
    (when label
      (setf (label object) label))))

(defmethod (setf label) (new-label (object button))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font-size label-font-size)
		   (height height)
		   (width width)
		   (children children)) object
    (declare (desired-type width height label-font-size))
    (common-setup-label object new-label)
    (let* ((label-width   (label-width object))
	   (scaling-width (d/ (d- width (d* 2.0
					    (button-text-offset-x *reference-sizes*)
					    width))
			      label-width)))
      (when (d< scaling-width 1.0)
	(setf label-font-size (d* label-font-size scaling-width)))
      (loop
	 for l across (the (simple-array triangle-mesh (*)) children)
	 for xf from 0.0 by label-font-size do
	   (setf (scaling l) (sb-cga:vec label-font-size label-font-size 0.0))
	   (setf (pos     l) (sb-cga:vec (d+ (d- (d/ width 2.0)
						 (d/ (label-width object) 2.0))
					     xf)
					 (d+ (d/ height 2.0)
					     (d- (d* label-font-size 0.5)))
					 0.0))))))

(defmethod render ((object button) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (current-texture current-texture)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)
		   (shown shown)) object
    (declare (texture current-texture))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when shown
      (when (> (length triangles) 0)
	(with-camera-view-matrix (camera-vw-matrix renderer)
	  (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	    (use-program compiled-shaders :gui)
	    (gl:active-texture :texture0)
	    (texture:bind-texture current-texture)
	    (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	    (uniformfv compiled-shaders :ia #(1.0 1.0 1.0))
	    (uniform-matrix compiled-shaders :modelview-matrix 4
				     (vector (sb-cga:matrix* camera-vw-matrix
							     (elt view-matrix 0)
							     (elt model-matrix 0)))
				     nil)
	    (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	    (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	    (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))
      (do-children-mesh (c object)
	(render c renderer)))))

(defclass text-field (button) ())

(defmethod initialize-instance :after ((object text-field) &key &allow-other-keys)
  (with-accessors ((width width) (height height)
		   (label-font-size label-font-size)) object
    (setf (texture-object  object) (get-texture +text-field-texture-name+))
    (setf (texture-pressed object) (get-texture +text-field-focused-texture-name+))
    (setf (texture-overlay object) (get-texture +text-field-overlay-texture-name+))
    (setf (current-texture object) (texture-object object))))

(defmethod render ((object text-field) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (current-texture current-texture)
		   (texture-overlay texture-overlay)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)
		   (shown shown)) object
    (declare (texture current-texture texture-overlay))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when shown
      (when (> (length triangles) 0)
	(with-camera-view-matrix (camera-vw-matrix renderer)
	  (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	    (use-program compiled-shaders :gui-naked-button)
	    (gl:active-texture :texture0)
	    (texture:bind-texture current-texture)
	    (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	    (gl:active-texture :texture1)
	    (texture:bind-texture texture-overlay)
	    (uniformi compiled-shaders :texture-overlay +texture-unit-overlay+)
	    (uniformfv compiled-shaders :ia #(1.0 1.0 1.0))
	    (uniform-matrix compiled-shaders :modelview-matrix 4
				     (vector (sb-cga:matrix* camera-vw-matrix
							     (elt view-matrix 0)
							     (elt model-matrix 0)))
				     nil)
	    (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	    (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	    (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))
      (do-children-mesh (c object)
	(render c renderer)))))

(defmethod (setf label) (new-label (object text-field))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font-size label-font-size)
		   (height height)
		   (width width)
		   (children children)) object
    (declare (desired-type width height label-font-size))
    (common-setup-label object new-label)
    (let* ((label-width     (label-width object))
	   (scaling-width   (d/ (d- width (d* 2.0
					      (button-text-offset-x *reference-sizes*)
					      width))
				label-width))
	   (new-font-size   (d* label-font-size scaling-width))
	   (max-font-height (d* (button-text-fit-height *reference-sizes*) (height object))))
      (if (d<= new-font-size max-font-height)
	  (setf label-font-size new-font-size)
	  (setf label-font-size max-font-height))
      (loop
	 for l across (the (simple-array triangle-mesh (*)) children)
	 for xf from 0.0 by label-font-size do
	   (setf (scaling l) (sb-cga:vec label-font-size label-font-size 0.0))
	   (setf (pos     l) (sb-cga:vec (d+ (d- (d/ width 2.0)
						 (d/ (label-width object) 2.0))
					     xf)
					 (d+ (d/ height 2.0)
					     (d- (d* label-font-size 0.5)))
					 0.0))))))

(defmethod (setf focus) (new-state (object text-field))
  (with-slots (focus) object
    (with-accessors ((current-texture current-texture)
		     (texture-pressed texture-pressed)
		     (texture-object  texture-object)) object
      (setf focus             new-state)
      (setf current-texture   (if focus
				  texture-pressed
				  texture-object)))))

(defmethod on-mouse-pressed ((object text-field) event)
  (if (mouse-over object (x-event event) (y-event event))
      (progn
	(setf (focus object) t)
	t)
      nil))

(defmethod on-mouse-released ((object text-field) event)
  (mouse-over object (x-event event) (y-event event)))

(defmethod on-key-pressed ((object text-field) event)
  (let ((old-label (label object)))
    (declare (simple-string old-label))
    (if (focus object)
	(progn
	  (cond
	    ((gui-printable-p (char-event event))
	     (setf (label object) (strcat old-label (string (char-event event)))))
	    (old-label
	     (setf (label object) (subseq old-label 0 (f- (length old-label) 1)))))
	  t)
      nil)))

(defclass simple-label (widget) ())

(defun calculate-text-scaling (widget)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (widget widget))
  (min (the desired-type (label-font-size widget))
       (d* (label-font-size widget)
	   (d/ (width widget)
	       (label-width widget)))))

(defmethod initialize-instance :after ((object simple-label) &key &allow-other-keys)
  (with-slots (label) object
    (when label
      (setf (label object) label))))

(defmethod (setf label) (new-label (object simple-label))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font-size label-font-size)
		   (children children)) object
    (common-setup-label object new-label)
    (setf (label-font-size object) (calculate-text-scaling object))
    (loop
       for l across (the (simple-array triangle-mesh (*)) children)
       for xf single-float from 0.0 by label-font-size do
	 (setf (scaling l) (sb-cga:vec label-font-size label-font-size 0.0))
	 (setf (pos     l) (sb-cga:vec xf 0.0 0.0)))))

(defclass simple-label-prefixed (simple-label)
  ((prefix
    :initform ""
    :initarg  :prefix
    :accessor prefix)))

(defmethod (setf label) (new-label (object simple-label-prefixed))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font-size label-font-size)
		   (children children)
		   (prefix prefix)) object
    (common-setup-label object (format nil "~a~a" prefix new-label))
    (setf (label-font-size object) (calculate-text-scaling object))
    (loop
       for l across (the (simple-array triangle-mesh (*)) children)
       for xf single-float from 0.0 by label-font-size do
	 (setf (scaling l) (sb-cga:vec label-font-size label-font-size 0.0))
	 (setf (pos     l) (sb-cga:vec xf 0.0 0.0)))))

(defclass static-text (widget)
  ((justified
    :initform t
    :initarg  :justified
    :accessor justified)))

(defmethod initialize-instance :after ((object static-text) &key &allow-other-keys)
  (with-slots (label) object
    (when label
      (setf (label object) label))))

(defmethod (setf label) (new-label (object static-text))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font-size label-font-size)
		   (label-font label-font)
		   (height height)
		   (width width)
		   (children children)
		   (justified justified)
		   (y y)) object
    (declare (desired-type width height label-font-size))
    (let* ((char-width     (ftruncate (d/ width label-font-size)))
	   (lines          (reverse (alexandria:flatten
				     (map 'list #'(lambda (a)
						    (if justified
							(justify-monospaced-text a char-width)
							a))
					  (cl-ppcre:split +static-text-delim+ new-label)))))
	   (raw-height     (d* label-font-size (d (length lines))))
	   (scaling-height (d/ height raw-height))
	   (actual-height      (if (d< raw-height height)
				   label-font-size
				   (d/ (d* raw-height scaling-height) (d (length lines)))))
	   (actual-text-height (d- height (d* actual-height (d (length lines))))))
      (declare (list lines))
      (do ((line-count (d 0.0) (d+ line-count 1.0))
	   (line       lines   (rest line)))
	  ((not line))
	(declare (list line))
	(declare (desired-type line-count))
	(loop
	   for c across (the simple-string (elt line 0))
	   for xf single-float from  0.0 by label-font-size do
	   (let* ((mesh  (get-char-mesh label-font c))
		  (shell (if mesh
			     (fill-shell-from-mesh mesh 'font-mesh-shell)
			     nil)))
	     (when shell
	       (setf (scaling shell) (sb-cga:vec label-font-size actual-height 0.0))
	       (setf (pos     shell) (sb-cga:vec xf (d+ (d* line-count actual-height)
							actual-text-height)
						 0.0))
	       (add-child object shell))))))
    ;; set sizes
    (let* ((aabb (aabb object))
	   (p1   (aabb-p1 aabb))
	   (p2   (aabb-p2 aabb))
	   (aabb2 (vec4 (elt p1 0) (elt p1 1)
			(elt p2 0) (elt p2 1)))
	   (rect  (aabb2->rect2 aabb2)))
      (declare (sb-cga:vec p1 p2))
      (declare (vec4 aabb2 rect))
      (setf width  (elt rect 2)
	    height (elt rect 3)))))

(defmethod label-width ((object static-text))
  (width object))

(defclass h-bar (widget)
  ((fill-level
    :initform nil
    :initarg  :fill-level
    :reader fill-level)
   (actual-bar
    :initform (make-instance 'simple-gui-mesh)
    :initarg  :actual-bar
    :accessor actual-bar)
   (label-shells
    :initform (make-fresh-array 0 nil 'font-mesh-shell nil)
    :initarg  :labels-shells
    :accessor label-shells)))

(defmethod initialize-instance :after ((object h-bar) &key (color :blue) &allow-other-keys)
  (with-accessors ((actual-bar actual-bar)
		   (label-width label-width)) object
    (with-slots (fill-level label) object
      (when color
	(case color
	  (:blue
	   (setf (texture-object actual-bar) (get-texture +blue-h-bar+)))
	  (:red
	   (setf (texture-object actual-bar) (get-texture +red-h-bar+)))
	  (:green
	   (setf (texture-object actual-bar) (get-texture +green-h-bar+)))
	  (otherwise
	   (setf (texture-object actual-bar) (get-texture +blue-h-bar+)))))
      (add-quad-for-widget actual-bar)
      (prepare-for-rendering object)
      (add-child object actual-bar)
      (when label
	(setf (label object) label))
      (when fill-level
	(setf (fill-level object) fill-level))
      (transform-vertices actual-bar (sb-cga:scale* (d- (width object) label-width)
						    (height object) 1.0))
      (prepare-for-rendering actual-bar)
      (setf (pos actual-bar) (sb-cga:vec (label-width object) 0.0 0.0)))))

(defmethod render :after ((object h-bar) renderer)
  (do-children-mesh (c object)
    (render c renderer)))

(defgeneric (setf fill-level) (value object))

(defmethod (setf fill-level) (value (object h-bar))
  (with-accessors ((actual-bar actual-bar)) object
    (with-slots (fill-level) object
      (setf fill-level (alexandria:clamp value 0.0 1.0))
      (setf (scaling actual-bar) (sb-cga:vec fill-level 1.0 1.0)))))

(defmethod setup-label ((object h-bar) new-label)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font label-font)
		   (label-font-size label-font-size)
		   (children children)
		   (label-shells label-shells)) object
    (declare (desired-type label-font-size))
    (with-slots (label) object
      (declare (simple-string label))
      (setf label new-label)
      (loop for c across label do
	   (let* ((mesh  (get-char-mesh label-font c))
		  (shell (if mesh
			     (fill-shell-from-mesh mesh 'font-mesh-shell)
			     nil)))
	     (when shell
	       (vector-push-extend shell label-shells)
	       (add-child object shell)))))))

(defmethod (setf label) (new-label (object h-bar))
  (declare (optimize (debug 0) (speed 1) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font label-font)
		   (label-font-size label-font-size)
		   (height height)
		   (width width)
		   (label-shells label-shells)) object
    (declare (desired-type width height label-font-size))
    (declare ((array font-mesh-shell (*)) label-shells))
    (setup-label object new-label)
    (loop
       for l across label-shells
       for xf single-float from 0.0 by label-font-size do
	 (setf (scaling l) (sb-cga:vec label-font-size height 0.0))
	 (setf (pos l)     (sb-cga:vec xf 0.0 0.0)))))

(defclass window (widget)
  ((top-bar
    :initform (make-instance 'widget)
    :initarg  :top-bar
    :accessor top-bar)
   (frame
    :initform (make-instance 'widget)
    :initarg  :frame
    :accessor frame)
   (close-button
    :initform (make-instance 'naked-button :callback #'hide-parent-cb)
    :initarg  :close-button
    :accessor close-button)
   (dragging-mode
    :initform nil
    :initarg  :dragging-mode
    :accessor dragging-mode)))

(defparameter *topbar-h* 30.0)

(defparameter *frame-relative-offseth* 30.0)

(defmethod initialize-instance :after ((object window) &key &allow-other-keys)
  (with-slots (label) object
    (with-accessors ((width width) (height height)
		     (top-bar top-bar)
		     (frame frame)) object
      (let ((top-bar-h (top-bar-h *reference-sizes*))
	    (title-font-size-scaling (title-font-size-scaling *reference-sizes*))
	    (button-y-relative (button-y-relative *reference-sizes*))
	    (button-x-relative (button-x-relative *reference-sizes*))
	    (button-h-relative (button-h-relative *reference-sizes*)))
	(prepare-for-rendering object)
	(add-quad-for-widget top-bar)
	(transform-vertices top-bar     (sb-cga:scale* (width object) top-bar-h 1.0))
	(setf (texture-object top-bar)  (get-texture +window-top-bar-texture-name+))
	(setf (pos top-bar)             (sb-cga:vec 0.0 (d- height top-bar-h) 0.0))
	(setf (label-font-size top-bar) (d* top-bar-h title-font-size-scaling))
	(when label
	  (setf (label object) label))
	;; add button
	(let ((button (make-instance 'naked-button
				     :x        (d- width
						   (d+ (d* top-bar-h button-h-relative)
						       (d* width button-x-relative)))
				     :y        (d* button-y-relative top-bar-h)
				     :width    (d* button-h-relative top-bar-h)
				     :height   (d* button-h-relative top-bar-h)
				     :callback #'hide-grandparent-cb)))
	  (setf (texture-object  button) (get-texture +window-close-button-texture-name+))
	  (setf (texture-pressed button) (get-texture +window-close-button-pressed-texture-name+))
	  (setf (texture-overlay button) (get-texture +button-cancel-texture-name+))
	  (setf (current-texture button) (texture-object button))
	  (prepare-for-rendering button)
	  (add-child top-bar button)
	  (prepare-for-rendering top-bar)
	  ;; add main frame
	  (add-quad-for-widget frame)
	  (transform-vertices frame    (sb-cga:scale* (width object) (d- height top-bar-h) 1.0))
	  (setf (texture-object frame) (get-texture +frame-texture-name+)
	        (pos            frame) (sb-cga:vec 0.0 0.0 0.0)
		(width          frame) (width object)
		(height         frame) (d- height top-bar-h))
	  (prepare-for-rendering frame)
	  (add-child object frame)
	  (add-child object top-bar))))))

(defmethod add-child :after ((object window) child
 			     &optional (child-pos (length (children object))))
   (when (> child-pos 2) ;; skip the titlebar , the button and the close-button
     (setf (x child) (d+ (x child)
 			(d* (left-frame-offset *reference-sizes*)
 			    (width object))))
     (when *child-flip-y*
       (setf (y child) (flip-y object child)))))

(defmethod (setf label) (new-label (object window))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font label-font)
		   (top-bar top-bar)
		   (height height)
		   (width width)) object
    (declare (desired-type width height))
    (with-accessors ((bar-label-font-size label-font-size)
		     (bar-children children))                  top-bar
      (declare (desired-type bar-label-font-size))
      (let ((top-bar-h (top-bar-h *reference-sizes*))
	    (top-bar-relative-offset (top-bar-relative-offset *reference-sizes*)))
	(declare (desired-type top-bar-h top-bar-relative-offset))
	(common-setup-label top-bar new-label)
	(let* ((label-width   (label-width top-bar))
	       (scaling-width (d/ (d- width (d* width (relative-title-space *reference-sizes*)))
				  label-width)))
	  (when (d< scaling-width 1.0)
	    (setf bar-label-font-size (d* bar-label-font-size scaling-width)))
	  (loop
	     for l across (the (simple-array triangle-mesh (*)) bar-children)
	     for xf from 0.0 by bar-label-font-size do
	       (setf (scaling l) (sb-cga:vec bar-label-font-size bar-label-font-size 0.0))
	       (setf (pos l)     (sb-cga:vec (d+ (d* width top-bar-relative-offset) xf)
					     (d- (d* top-bar-h 0.5)
						 (d* bar-label-font-size 0.5))
					     0.0))))))))

(defmethod render ((object window) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (texture-object texture-object)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)
		   (shown shown)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when shown
      (when (> (length triangles) 0)
	(with-camera-view-matrix (camera-vw-matrix renderer)
	  (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	    (use-program compiled-shaders :gui)
	    (gl:active-texture :texture0)
	    (texture:bind-texture texture-object)
	    (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	    (uniformfv compiled-shaders :ia #(1.0 1.0 1.0))
	    (uniform-matrix compiled-shaders :modelview-matrix 4
				     (vector (sb-cga:matrix* camera-vw-matrix
							     (elt view-matrix 0)
							     (elt model-matrix 0)))
				     nil)
	    (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	    (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	    (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))
      (do-children-mesh (c object)
	(render c renderer)))))

(defmethod flip-y ((object window) (child widget))
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (let* ((scale    (d/ (d- (height object)
			   (y child))
		     (height object)))
	  (h-frame (d- (height (frame object))
		       (d* (top-frame-offset *reference-sizes*) (height (frame object)))
		       (d* (bottom-frame-offset *reference-sizes*) (height (frame object)))))
	 (saved-height (height child)))
    (setf (scaling child) (sb-cga:vec 1.0 (d/ h-frame (height object))  0.0))
    (d+ (d- (d* scale h-frame)
	    (d* saved-height (d/ h-frame (height object))))
	(d* (bottom-frame-offset *reference-sizes*) (height (frame object))))))

(defmethod on-mouse-pressed ((object window) event)
  (if (and (shown object)
	   (mouse-over object (x-event event) (y-event event)))
      (progn
	(loop for w across (children object) do
	     (when (and (widgetp w)
			(on-mouse-pressed w event))
	       (return-from on-mouse-pressed t)))
	t)
      nil))

(defmethod on-mouse-released ((object window) event)
  (with-accessors ((dragging-mode dragging-mode)) object
    (when dragging-mode
      (setf dragging-mode nil)))
  (if (and (shown object)
	   (mouse-over object (x-event event) (y-event event)))
      (progn
	(loop for w across (children object) do
	     (when (and (widgetp w)
			(on-mouse-released w event))
	       (return-from on-mouse-released t)))
	t)
      nil))

(defmethod on-mouse-dragged ((object window) event)
  (with-accessors ((dragging-mode dragging-mode)) object
    (if (or dragging-mode
	    (mouse-over (top-bar object) (x-event event) (y-event event)))
	(progn
	  (setf (pos object) (sb-cga:vec+ (pos object)
					  (sb-cga:vec (dx-event event) (dy-event event) 0.0)))
	  (when (not dragging-mode)
	    (setf dragging-mode t))
	  t)
	(progn
	  (loop for w across (children object) do
	       (when (and (widgetp w)
			  (on-mouse-dragged w event))
		 (return-from on-mouse-dragged t)))
	  nil))))

(defclass labeled-check-button (widget)
  ((button
    :initform nil
    :initarg  :button
    :accessor button)
   (text
    :initform nil
    :initarg  :text
    :accessor text)))

(defmethod initialize-instance :after ((object labeled-check-button) &key
								       (color :green)
								       &allow-other-keys)
  (with-accessors ((button button) (text text)
		   (width width) (height height)
		   (label label)
		   (label-font-size label-font-size)) object
    (setf button (make-instance 'check-button :width height :height height
				:color color
				:x (d- width height) :y 0.0))
    (let ((text-font-size (min label-font-size
			       (d* (label-font-size object)
				   (d/ (d- width height) (label-width object))))))
      (setf text (make-instance 'simple-label
				:x 0.0
				:y (d- (d/ height 2.0) (d/ text-font-size 2.0))
				:width (d- width height)
				:label label
				:justified nil
				:label-font-size text-font-size)))
    (add-child object text)
    (add-child object button)))

(defmethod render :after ((object labeled-check-button) renderer)
  (do-children-mesh (c object)
    (render c renderer)))

(defmethod group ((object labeled-check-button))
  (group (button object)))

(defmethod (setf group) (new-group (object labeled-check-button))
  (setf (group (button object)) new-group))

(defmethod flip-state ((object labeled-check-button))
  (flip-state (button object)))

(defmethod button-state ((object labeled-check-button))
  (button-state (button object)))

(defmethod (setf button-state) (value (object labeled-check-button))
  (setf (button-state (button object)) value))

(defmethod callback ((object labeled-check-button))
  (callback (button object)))

(defmethod (setf callback) (value (object labeled-check-button))
  (setf (callback (button object)) value))

(defmethod on-mouse-pressed ((object labeled-check-button) event)
  (on-mouse-pressed (button object) event))

(defmethod on-mouse-released ((object labeled-check-button) event)
  nil)

(defun make-check-group (buttons)
  (let ((res (make-fresh-array (length buttons) t 'check-button t)))
    (loop
       for b in buttons
       for i from 0 by 1 do
	 (setf (elt res i)
	        (if (typep b 'labeled-check-button)
		    (button b)
		    b)))
    res))

(defun make-check-group* (&rest buttons)
  (make-check-group buttons))

(alexandria:define-constant +file-button-w+ 12.0 :test #'=)

(alexandria:define-constant +file-button-action-size+ 24.0 :test #'=)

(alexandria:define-constant +file-visible-slot+ 5 :test #'=)

(defun fchooser-init-dirs-file-slots ()
  (make-fresh-array 0 t 'button nil))

(defun make-file-chooser-square-button (overlay x y callback)
  (make-instance 'naked-button
		 :x x
		 :y y
		 :width  +file-button-action-size+
		 :height +file-button-action-size+
		 :texture-object  (get-texture +square-button-texture-name+)
		 :texture-pressed (get-texture +square-button-pressed-texture-name+)
		 :texture-overlay (get-texture overlay)
		 :callback        callback))

(defclass file-chooser (window)
  ((b-updir
    :initform (make-instance 'button
			     :height (d* +file-button-w+ 2.0)
			     :width  (d/ *file-chooser-w* 2.5)
			     :x 0.0
			     :y 0.0
			     :callback #'dir-up-cb
			     :label "..")
    :initarg  :b-updir
    :accessor b-updir)
   (b-ok
    :initform (make-instance 'naked-button
			     :x 0.0
			     :y (d- *file-chooser-h* 24.0)
			     :width  24.0
			     :height 24.0
			     :texture-object  (get-texture +square-button-texture-name+)
			     :texture-pressed (get-texture +square-button-pressed-texture-name+)
			     :texture-overlay (get-texture +button-ok-texture-name+)
			     :callback        nil) ;TODO)
    :initarg  :b-ok
    :accessor b-ok)
   (b-close
    :initform (make-instance 'naked-button
			     :x 24.0
			     :y (d- *file-chooser-h* 24.0)
			     :width  24.0
			     :height 24.0
			     :texture-object  (get-texture +square-button-texture-name+)
			     :texture-pressed (get-texture +square-button-pressed-texture-name+)
			     :texture-overlay (get-texture +button-cancel-texture-name+)
			     :callback        #'hide-parent-cb)
    :initarg  :b-close
    :accessor b-close)
   (b-dir-scroll-up
    :initform (make-file-chooser-square-button +up-overlay-texture-name+
					       0.0
					       (d- *file-chooser-h*
						   (d* 3.0 +file-button-action-size+)
						   (d* +input-text-h+   1.5))
					       #'dir-scroll-up-cb)
    :initarg  :b-dir-scroll-up
    :accessor b-dir-scroll-up)
   (b-dir-scroll-down
    :initform (make-file-chooser-square-button +down-overlay-texture-name+
					       +file-button-action-size+
					       (d- *file-chooser-h*
						   (d* 3.0 +file-button-action-size+)
						   (d* +input-text-h+   1.5))
					       #'dir-scroll-down-cb)
    :initarg  :b-dir-scroll-down
    :accessor b-dir-scroll-down)
   (b-file-scroll-up
    :initform (make-file-chooser-square-button +up-overlay-texture-name+
					       (d/ *file-chooser-w* 2.5)
					       (d- *file-chooser-h*
						   (d* 3.0 +file-button-action-size+)
						   (d* +input-text-h+   1.5))
					       #'file-scroll-up-cb)
    :initarg  :b-file-scroll-up
    :accessor b-file-scroll-up)
   (b-file-scroll-down
    :initform (make-file-chooser-square-button +down-overlay-texture-name+
					       (d+ (d/ *file-chooser-w* 2.5)
						   +file-button-action-size+)
					       (d- *file-chooser-h*
						   (d* 3.0 +file-button-action-size+)
						   (d* +input-text-h+   1.5))
					       #'file-scroll-down-cb)
    :initarg  :b-file-scroll-down
    :accessor b-file-scroll-down)
   (input-path
    :initform (make-instance 'text-field
			     :width  (d* *file-chooser-w* 0.8)
			     :height (d* +input-text-h+   1.5)
			     :x 0.0
			     :y (d- *file-chooser-h*
				    (d* +input-text-h+   1.5)
				    (d* 2.0 +file-button-action-size+))
			     :label nil)
    :initarg :input-path
    :accessor input-path)
   (display-hidden
    :initform nil
    :initarg  :display-hidden
    :accessor display-hidden)
   (all-dirs
    :initform (fchooser-init-dirs-file-slots)
    :initarg  all-dirs
    :accessor all-dirs)
   (all-files
    :initform (fchooser-init-dirs-file-slots)
    :initarg  all-files
    :accessor all-files)
   (from-dir
    :initform 0
    :initarg  from-dir
    :accessor from-dir)
   (from-file
    :initform 0
    :initarg  from-file
    :accessor from-file)
   (current-dir
    :initform (strcat (uiop:getenvp "HOME") *directory-sep*)
    :initarg  current-dir
    :accessor current-dir)))

(defmethod initialize-instance :after ((object file-chooser) &key &allow-other-keys)
  (with-accessors ((b-ok b-ok)
		   (b-close b-close)
		   (b-updir b-updir)
		   (b-dir-scroll-down b-dir-scroll-down)
		   (b-dir-scroll-up b-dir-scroll-up)
		   (b-file-scroll-down b-file-scroll-down)
		   (b-file-scroll-up b-file-scroll-up)
		   (input-path input-path))              object
    (regenerate-dir-buttons object)
    (regenerate-file-buttons object)
    (setf (label input-path) (current-dir object))
    (add-children* object b-updir
		   input-path
		   b-close b-ok
		   b-dir-scroll-up   b-dir-scroll-down
		   b-file-scroll-up  b-file-scroll-down)))

(defgeneric fetch-file-chooser-path (object))

(defgeneric refresh-dir-buttons (object))

(defgeneric refresh-file-buttons (object))

(defgeneric regenerate-dir-buttons (object))

(defgeneric regenerate-file-buttons (object))

(defmethod fetch-file-chooser-path ((object file-chooser))
  (label (input-path object)))

(defmethod regenerate-dir-buttons ((object file-chooser))
  (remove-file-slot-button-by-label object)
  (setf (all-dirs object) (fchooser-init-dirs-file-slots))
  (loop for d in (sort (uiop:subdirectories (current-dir object))
		       #'string< :key #'uiop:native-namestring)    do
       (let ((phys-name (uiop:native-namestring d)))
	 (when (or (display-hidden object)
		   (not (filesystem-utils:path-to-hidden-file-p phys-name)))
	   (vector-push-extend phys-name (all-dirs object)))))
  (setf (from-dir object) 0)
  (refresh-dir-buttons object))

(defmethod refresh-dir-buttons ((object file-chooser))
  (remove-file-slot-button-by-label object)
  (loop
     for i from (from-dir object) below (min (length (all-dirs object))
					     (f+ (from-dir object) +file-visible-slot+))
     for y from (d* +file-button-w+ 2.0) by (d* +file-button-w+ 2.0) do
       (let ((b-dir (make-instance 'button
				   :height (d* +file-button-w+ 2.0)
				   :width  (d/ *file-chooser-w* 2.5)
				   :x 0.0
				   :y y
				   :callback #'dir-enter-cb
				   :compiled-shaders (compiled-shaders object)
				   :label (elt (all-dirs object) i))))
	 (add-child object b-dir))))

(defmethod refresh-file-buttons ((object file-chooser))
  (remove-file-slot-button-by-label object)
  (loop
     for i from (from-file object) below (min (length (all-files object))
					      (f+ (from-file object) +file-visible-slot+))
     for y from (d* +file-button-w+ 2.0) by (d* +file-button-w+ 2.0) do
       (let* ((filename (elt (all-files object) i))
	      (b-file (make-instance 'button
				     :height (d* +file-button-w+ 2.0)
				     :width  (d/ *file-chooser-w* 2.5)
				     :x (d/ *file-chooser-w* 2.5)
				     :y y
				     :callback #'file-click-cb
				     :compiled-shaders (compiled-shaders object)
				     :label filename)))
	 (add-child object b-file))))

(defun remove-file-slot-button-by-label (chooser)
  (setf (children chooser)
	(remove-if #'(lambda (c) (and (typep c 'button)
				      (find (label c) (all-files chooser) :test #'string=)))
		   (children chooser))))

(defun remove-dir-slot-button-by-label (chooser)
  (setf (children chooser)
	(remove-if #'(lambda (c) (and (typep c 'button)
				      (find (label c) (all-dirs chooser) :test #'string=)))
		   (children chooser))))

(defmethod regenerate-file-buttons ((object file-chooser))
  (remove-file-slot-button-by-label object)
  (setf (all-files object) (fchooser-init-dirs-file-slots))
  (loop for d in (sort (uiop:directory-files (current-dir object))
		       #'string< :key #'uiop:native-namestring)    do
       (let ((phys-name (uiop:native-namestring d)))
	 (when (or (display-hidden object)
		   (not (filesystem-utils:path-to-hidden-file-p phys-name)))
	   (vector-push-extend (filesystem-utils:strip-dirs-from-path phys-name)
			       (all-files object)))))
  (setf (from-file object) 0)
  (refresh-file-buttons object))

(defun dir-scroll-down-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (setf (from-dir win)
	  (alexandria:clamp (1+ (from-dir win))
			    0
			    (max 0 (- (length (all-dirs win)) +file-visible-slot+))))
    (refresh-dir-buttons win)))

(defun dir-scroll-up-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (setf (from-dir win) (max (1- (from-dir win)) 0))
    (refresh-dir-buttons win)))

(defun file-scroll-down-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (setf (from-file win)
	  (alexandria:clamp (1+ (from-file win))
			    0
			    (max 0 (- (length (all-files win)) +file-visible-slot+))))
    (refresh-file-buttons win)))

(defun file-scroll-up-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (setf (from-file win) (max (1- (from-file win)) 0))
    (refresh-file-buttons win)))

(defun dir-up-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((current-dir current-dir) (input-path input-path)) win
      (remove-file-slot-button-by-label win)
      (remove-dir-slot-button-by-label win)
      (setf current-dir (filesystem-utils:parent-dir-path current-dir))
      (setf (label input-path) current-dir)
      (regenerate-dir-buttons win)
      (regenerate-file-buttons win))))

(defun dir-enter-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((current-dir current-dir) (input-path input-path)) win
      (remove-file-slot-button-by-label win)
      (remove-dir-slot-button-by-label win)
      (setf current-dir (label button))
      (setf (label input-path) current-dir)
      (regenerate-dir-buttons win)
      (regenerate-file-buttons win))))

(defun file-click-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((current-dir current-dir) (input-path input-path)) win
      (setf (label input-path) (strcat current-dir (label button))))))

(defun make-file-chooser (&optional (ok-callback #'hide-parent-cb))
  (let ((win (make-instance 'file-chooser
			    :x (d- (d/ (d *window-w*) 2.0)
				   (d/ *file-chooser-w* 2.0))
			    :y (d- (d/ (d *window-h*) 2.0)
				   (d/ *file-chooser-h* 2.0))
			    :width  *file-chooser-w*
			    :height *file-chooser-h*
			    :label  (_ "Choose file"))))
    (setf (callback (b-ok win)) ok-callback)
    win))

(defun rel->abs (s)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (desired-type s))
  (d* s *square-button-size*))

(defun make-square-button (x y overlay callback &key (small nil))
  (let ((size (if small
		  *small-square-button-size*
		  *square-button-size*)))
    (make-instance 'naked-button
		   :x x :y y
		   :width size :height size
		   :texture-object  (get-texture +square-button-texture-name+)
		   :texture-pressed (get-texture +square-button-pressed-texture-name+)
		   :texture-overlay (get-texture overlay)
		   :callback        callback)))

(defun make-rect-button (x y scale-w scale-h overlay callback)
  (make-instance 'naked-button
		 :x x :y y
		 :width  (rel->abs scale-w)
		 :height (rel->abs scale-h)
		 :texture-object  (get-texture +square-button-texture-name+)
		 :texture-pressed (get-texture +square-button-pressed-texture-name+)
		 :texture-overlay (get-texture overlay)
		 :callback        callback))

(defun make-health-condition (x y size texture-name)
  (make-instance 'signalling-light
		 :x x :y y
		 :width  (rel->abs size)
		 :height (rel->abs size)
		 :texture-name texture-name
		 :button-status t))

(defclass main-toolbar (widget)
  ;; first row
  ((s-coma
    :initform (make-health-condition (d* 4.0 *small-square-button-size*)
				     (d+ *small-square-button-size*
					 (d* 0.5 *small-square-button-size*))
				     0.25 +coma-texture-name+)
    :initarg  :s-coma
    :accessor s-coma)
   (s-poisoned
    :initform (make-health-condition (d+ (d* 4.0 *small-square-button-size*)
					 (d* 0.5 *small-square-button-size*))
				     (d+ *small-square-button-size*
					 (d* 0.5 *small-square-button-size*))
				     0.25 +poison-texture-name+)
    :initarg  :s-poisoned
    :accessor s-poisoned)
   (s-terrorized
    :initform (make-health-condition (d* 4.0 *small-square-button-size*)
				     *small-square-button-size*
				     0.25 +terror-texture-name+)
    :initarg  :s-terrorized
    :accessor s-terrorized)
   (s-berserk
    :initform (make-health-condition (d+ (d* 4.0 *small-square-button-size*)
					 (d* 0.5 *small-square-button-size*))
				     *small-square-button-size*
				     0.25 +berserk-texture-name+)
    :initarg  :s-berserk
    :accessor s-berserk)

   (b-attack-short
    :initform (make-square-button (d* 3.0 *square-button-size*)
				  *small-square-button-size*
				  +attack-short-range-overlay-texture-name+
				  nil ;; TODO callback
				  :small t)
    :initarg  :b-attack-short
    :accessor b-attack-short)
   (b-attack-long
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
				       *small-square-button-size*)
				  *small-square-button-size*
				  +attack-long-range-overlay-texture-name+
				  nil ;; TODO callback
				  :small t)
    :initarg  :b-attack-long
    :accessor b-attack-long)
   (b-attack-long-imprecise
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
				      (d* 2.0 *small-square-button-size*))
				  *small-square-button-size*
				  +attack-long-range-imprecise-overlay-texture-name+
				  nil ;; TODO callback
				  :small t)
    :initarg  :b-attack-long-imprecise
    :accessor b-attack-long-imprecise)
   (b-conversation
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
				      (d* 3.0 *small-square-button-size*))
				  *small-square-button-size*
				  +conversation-overlay-texture-name+
				  nil ;; TODO callback
				  :small t)
    :initarg  :b-conversation
    :accessor b-conversation)

   (text-communication
    :initform (make-instance 'widget:static-text
			     :height *square-button-size*
			     :width  (d* *small-square-button-size* 5.0)
			     :x (d+ (d* *square-button-size* 7.0)
				    *small-square-button-size*)
			     :y 0.0
			     :font-size (d* 0.1 *square-button-size*)
			     :label "Elfic sword.§+1 dexterity. Poison enemy (+2 dmg each turn)"
			     :justified t)
    :initarg  :text-communication
    :accessor text-communication)
   ;; second row
   (b-save
    :initform (make-square-button 0.0 *small-square-button-size*
				  +save-overlay-texture-name+
				  nil ;; TODO callback
				  :small t)
    :initarg :b-save
    :accessor b-save)
   (b-load
    :initform (make-square-button *small-square-button-size* *small-square-button-size*
				  +load-overlay-texture-name+
				  nil  ;; TODO callback
				  :small t)
    :initarg :b-load
    :accessor b-load)
   (b-options
    :initform (make-square-button 0.0 0.0
				  +option-overlay-texture-name+
				  nil  ;; TODO callback
				  :small t)
    :initarg :b-options
    :accessor b-options)
   (b-quit
    :initform (make-square-button *small-square-button-size* 0.0
				  +quit-overlay-texture-name+
				  nil  ;; TODO callback
				  :small t)
    :initarg :b-quit
    :accessor b-quit)
   (b-next
    :initform (make-square-button (d* 2.0 *small-square-button-size*) *small-square-button-size*
				  +next-overlay-texture-name+
				  nil  ;; TODO callback
				  :small t)
    :initarg :b-next
    :accessor b-next)
   (b-previous
    :initform (make-square-button (d* 3.0 *small-square-button-size*) *small-square-button-size*
				  +previous-overlay-texture-name+
				  nil  ;; TODO callback
				  :small t)
    :initarg :b-previous
    :accessor b-previous)
   (b-next-turn
    :initform (make-rect-button   *square-button-size* 0.0
				  1.0 0.5
				  +next-turn-overlay-texture-name+
				  nil)  ;; TODO callback
    :initarg :b-next-turn
    :accessor b-next-turn)
   (b-spell
    :initform (make-square-button (d* 3.0 *square-button-size*)

				  0.0
				  +magic-staff-overlay-texture-name+
				  nil ;; TODO callback
				  :small t)
    :initarg  :b-spell
    :accessor b-spell)
   (b-open
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
				      *small-square-button-size*)
				  0.0
				  +open-overlay-texture-name+
				  nil ;; TODO callback
				  :small t)
    :initarg  :b-open
    :accessor b-open)
   (b-close
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
				      (d* 2.0 *small-square-button-size*))
				  0.0
				  +close-overlay-texture-name+
				  nil ;; TODO callback
				  :small t)
    :initarg  :b-close
    :accessor b-close)
   (b-zoom
    :initform (make-square-button  (d+ (d* 3.0 *square-button-size*)
				       (d* 3.0 *small-square-button-size*))
				   0.0
				   +zoom-overlay-texture-name+
				   nil  ;; TODO callback
				   :small t)
     :initarg :b-zoom
     :accessor b-zoom)
   (b-unzoom
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
				      (d* 4.0 *small-square-button-size*))
				  0.0
				  +unzoom-overlay-texture-name+
				  nil  ;; TODO callback
				  :small t)
    :initarg :b-unzoom
    :accessor b-unzoom)
   (bar-mp
    :initform (make-instance 'widget:h-bar
			     :height (d/ *square-button-size* 8.0)
			     :width  (d+ *square-button-size* *small-square-button-size*)
			     :x (d* *small-square-button-size* 11.0)
			     :y (d* 3.0 (d/ *square-button-size* 8.0))
			     :label "MP: "
			     :color :green)
    :initarg  :bar-mp
    :accessor bar-mp)
   (bar-dmg
    :initform (make-instance 'widget:h-bar
			     :height (d/ *square-button-size* 8.0)
			     :width  (d+ *square-button-size* *small-square-button-size*)
			     :x (d* *small-square-button-size* 11.0)
			     :y (d* 2.0 (d/ *square-button-size* 8.0))
			     :label "DMG:"
			     :color :red
			    :fill-level 0.5)
    :initarg  :bar-dmg
    :accessor bar-dmg)
   (bar-sp
    :initform (make-instance 'widget:h-bar
			     :height (d/ *square-button-size* 8.0)
			     :width  (d+ *square-button-size* *small-square-button-size*)
			     :x (d* *small-square-button-size* 11.0)
			     :y (d/ *square-button-size* 8.0)
			     :label "SP: "
			     :color :blue
			     :fill-level 1.0)
    :initarg  :bar-sp
    :accessor bar-sp)
   (text-mp
    :initform (make-instance 'widget:static-text
			     :height (d/ *square-button-size* 6.0)
			     :width  *small-square-button-size*
			     :x (d+ (d+ *square-button-size* *small-square-button-size*)
				    (d* *small-square-button-size* 11.0))
			     :y (d- (d* 3.0 (d/ *square-button-size* 8.0))
				    5.0)
			     :font-size 10.0
			     :label "23"
			     :justified nil)
    :initarg  :text-mp
    :accessor text-mp)
   (text-dmg
    :initform (make-instance 'widget:static-text
			     :height (d/ *square-button-size* 6.0)
			     :width  *small-square-button-size*
			     :x (d+ (d+ *square-button-size* *small-square-button-size*)
				    (d* *small-square-button-size* 11.0))
			     :y (d- (d* 2.0 (d/ *square-button-size* 8.0))
				    5.0)
			     :font-size 10.0
			     :label "22"
			     :justified nil)
    :initarg  :text-dmg
    :accessor text-dmg)
   (text-sp
    :initform (make-instance 'widget:static-text
			     :height (d/ *square-button-size* 6.0)
			     :width  *small-square-button-size*
			     :x (d+ (d+ *square-button-size* *small-square-button-size*)
				    (d* *small-square-button-size* 11.0))
			     :y (d- (d/ *square-button-size* 8.0)
				    5.0)
			     :font-size 10.0
			     :label "21"
			     :justified nil)
    :initarg  :text-sp
    :accessor text-sp)
   (text-fps
    :initform (make-instance 'widget:simple-label
			     :label "0.0"
			     :font-size 10.0
			     :width     50.0
			     :x         0.0
			     :y         (d* 2.0 *small-square-button-size*))
    :initarg  :text-fps
    :accessor text-fps)))

(defmethod initialize-instance :after ((object main-toolbar) &key &allow-other-keys)
  ;; first row
  (add-child object (s-coma                  object))
  (add-child object (s-poisoned              object))
  (add-child object (s-terrorized            object))
  (add-child object (s-berserk               object))
  (add-child object (b-attack-short          object))
  (add-child object (b-attack-long           object))
  (add-child object (b-attack-long-imprecise object))
  (add-child object (b-conversation          object))
  (add-child object (text-communication      object))
  ;; second row
  (add-child object (b-save        object))
  (add-child object (b-load        object))
  (add-child object (b-options     object))
  (add-child object (b-quit        object))
  (add-child object (b-next        object))
  (add-child object (b-previous    object))
  (add-child object (b-next-turn   object))
  (add-child object (b-spell       object))
  (add-child object (b-open        object))
  (add-child object (b-close       object))
  (add-child object (bar-mp        object))
  (add-child object (bar-dmg       object))
  (add-child object (bar-sp        object))
  (add-child object (text-mp       object))
  (add-child object (text-dmg      object))
  (add-child object (text-sp       object))
  (add-child object (b-zoom        object))
  (add-child object (b-unzoom      object))
  (add-child object (text-fps      object)))

(defun make-pgen-button (x y plus)
  (make-instance 'naked-button
		 :x               x
		 :y               y
		 :width           +input-text-h+
		 :height          +input-text-h+
		 :texture-object  (get-texture +square-button-texture-name+)
		 :texture-pressed (get-texture +square-button-pressed-texture-name+)
		 :texture-overlay (get-texture (if plus
						   +plus-overlay-texture-name+
						   +minus-overlay-texture-name+))
		 :callback        nil))

(defun add-callback-to-pgen-button (widget player slot widget-capital widget-destination
				    plus &optional (scale-plus 1.0) (scale-minus 1.0))
  (setf (callback widget)
	#'(lambda (w e)
	    (declare (ignore w e))
	    (let* ((capital (d (parse-number:parse-number
				(cl-ppcre:scan-to-strings +float-regexp+
							  (label widget-capital)))))
		   (current (d (parse-number:parse-number
				(cl-ppcre:scan-to-strings +float-regexp+
							  (label widget-destination)))))
		   (new-capital (if plus (d- capital 1.0)  (d+ capital 1.0)))
		   (new-current (if plus
				    (d+ current scale-plus)
				    (d- current scale-minus))))
	      (when (and (d>= new-capital 0.0)
			 (or plus
			     (d>= new-current 0.0)))
		(setf (label widget-capital)               (format nil "~,2f" new-capital)
		      (label widget-destination)           (format nil "~,2f" new-current)
		      (slot-value player slot)             new-current
		      (player-character:exp-points player) new-capital))))))

(defclass player-generator (window)
  ((player
    :initform  nil
    :initarg  :player
    :accessor player)
   (lb-class
    :initform (make-instance 'simple-label
			     :label     (_ "Class")
			     :font-size (h1-font-size *reference-sizes*)
			     :width  (d/ (d *window-w*) 12.0)
			     :x         0.0
			     :y         0.0)
    :initarg  :lb-class
    :accessor lb-class)
   (checkb-warrior
    :initform (make-instance 'labeled-check-button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (h2-font-size *reference-sizes*)
			     :label (_ "Warrior ")
			     :color :green)
    :initarg  :checkb-warrior
    :accessor checkb-warrior)
   (checkb-wizard
    :initform (make-instance 'labeled-check-button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (d+ (h2-font-size *reference-sizes*)
				    +checkbutton-h+)
			     :label (_ "Wizard ")
			     :color :green)
    :initarg  :checkb-wizard
    :accessor checkb-wizard)
   (checkb-healer
    :initform (make-instance 'labeled-check-button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (d+ (h2-font-size *reference-sizes*)
				    (d* 2.0 +checkbutton-h+))
			     :label (_ "Healer ")
			     :color :green)
    :initarg  :checkb-healer
    :accessor checkb-healer)
   (checkb-archer
    :initform (make-instance 'labeled-check-button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (d+ (h2-font-size *reference-sizes*)
				    (d* 3.0 +checkbutton-h+))
			     :label (_ "Archer ")
			     :color :green)
    :initarg  :checkb-archer
    :accessor checkb-archer)
   (checkb-ranger
    :initform (make-instance 'labeled-check-button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (d+ (h2-font-size *reference-sizes*)
				    (d* 4.0 +checkbutton-h+))
			     :label (_ "Ranger ")
			     :color :green)
    :initarg  :checkb-ranger
    :accessor checkb-ranger)
   (lb-gender
    :initform (make-instance 'simple-label
			     :label     (_ "Gender")
			     :font-size (h1-font-size *reference-sizes*)
			     :width  (d/ (d *window-w*) 12.0)
			     :x         0.0
			     :y         (d+ (h2-font-size *reference-sizes*)
					    (d* 5.0 +checkbutton-h+)))
    :initarg  :lb-gender
    :accessor lb-gender)
   (checkb-male
    :initform (make-instance 'labeled-check-button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (d+ (d* 2.0 (h2-font-size *reference-sizes*))
				    (d* 5.0 +checkbutton-h+))
			     :label (_ "Male ")
			     :color :green)
    :initarg  :checkb-male
    :accessor checkb-male)
   (checkb-female
    :initform (make-instance 'labeled-check-button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (d+ (d* 2.0 (h2-font-size *reference-sizes*))
				    (d* 6.0 +checkbutton-h+))
			     :label (_ "Female ")
			     :color :green)
    :initarg  :checkb-female
    :accessor checkb-female)
   (b-generate
    :initform (make-instance 'button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (d+ (d* 2.0 (h2-font-size *reference-sizes*))
				    +spacing+
				    (d* 7.0 +checkbutton-h+))
			     :callback #'generate-cb
			     :label (_ "Generate new"))

    :initarg  :b-generate
    :accessor b-generate)
   (b-save
    :initform (make-instance 'button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (d+ (d* 2.0 (h2-font-size *reference-sizes*))
				    (d* 2.0 +spacing+)
				    (d* 8.0 +checkbutton-h+))
			     :callback #'save-cb
			     :label (_ "Save"))

    :initarg  :b-save
    :accessor b-save)
   (b-load
    :initform (make-instance 'button
			     :height +checkbutton-h+
			     :width  (d/ (d *window-w*) 12.0)
			     :x 0.0
			     :y (d+ (d* 2.0 (h2-font-size *reference-sizes*))
				    (d* 3.0 +spacing+)
				    (d* 9.0 +checkbutton-h+))
			     :label (_ "Load"))

    :initarg  :b-load
    :accessor b-load)
   (img-portrait
    :initform (make-instance 'signalling-light
			     :width  +portrait-size+
			     :height +portrait-size+
			     :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			     :y 0.0
			     :texture-name +portrait-unknown-texture-name+
			     :button-status t)
    :initarg :img-portrait
    :accessor img-portrait)
   (lb-damage-pt
    :initform (make-instance 'simple-label-prefixed
			     :width  (d* 2.0 +input-text-w+)
			     :height +input-text-h+
			     :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			     :y (d* 1.8 +portrait-size+)
			     :prefix (_ "Damage points: ")
			     :label "")
    :initarg :lb-damage-pt
    :accessor lb-damage-pt)
   (b-inc-damage-pt
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d* 1.8 +portrait-size+)
				t)
    :initarg :b-inc-damage-pt
    :accessor b-inc-damage-pt)
   (b-dec-damage-pt
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d* 1.8 +portrait-size+)
				nil)
    :initarg :b-dec-damage-pt
    :accessor b-dec-damage-pt)
   (lb-movement-pt
    :initform (make-instance 'simple-label-prefixed
			     :width  (d* 2.0 +input-text-w+)
			     :height +input-text-h+
			     :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			     :y (d+ (d* 1.8 +portrait-size+) +input-text-h+)
			     :prefix (_ "Movement points: ")
			     :label "")
    :initarg :lb-movement-pt
    :accessor lb-movement-pt)
   (b-inc-movement-pt
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) +input-text-h+)
				t)
    :initarg :b-inc-movement-pt
    :accessor b-inc-movement-pt)
   (b-dec-movement-pt
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) +input-text-h+)
				nil)
    :initarg :b-dec-movement-pt
    :accessor b-dec-movement-pt)
   (lb-magic-pt
    :initform (make-instance 'simple-label-prefixed
			     :width  (d* 2.0 +input-text-w+)
			     :height +input-text-h+
			     :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			     :y (d+ (d* 1.8 +portrait-size+) (d* 2.0 +input-text-h+))
			     :prefix (_ "Magic points: ")
			     :label "")
    :initarg :lb-magic-pt
    :accessor lb-magic-pt)
   (b-inc-magic-pt
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 2.0 +input-text-h+))
				t)
    :initarg :b-inc-magic-pt
    :accessor b-inc-magic-pt)
   (b-dec-magic-pt
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 2.0 +input-text-h+))
				nil)
    :initarg :b-dec-magic-pt
    :accessor b-dec-magic-pt)
   (lb-dodge-ch
    :initform (make-instance 'simple-label-prefixed
			     :width  (d* 2.0 +input-text-w+)
			     :height +input-text-h+
			     :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			     :y (d+ (d* 1.8 +portrait-size+) (d* 3.0 +input-text-h+))
			     :prefix (_ "Dodge chance: ")
			     :label "")
    :initarg :lb-dodge-ch
    :accessor lb-dodge-ch)
   (b-inc-dodge-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 3.0 +input-text-h+))
				t)
    :initarg :b-inc-dodge-ch
    :accessor b-inc-dodge-ch)
   (b-dec-dodge-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 3.0 +input-text-h+))
				nil)
    :initarg :b-dec-dodge-ch
    :accessor b-dec-dodge-ch)
   (lb-melee-atk-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 4.0 +input-text-h+))
			      :prefix (_ "Short range attack chance: ")
			      :label "")
     :initarg  :lb-melee-atk-ch
     :accessor lb-melee-atk-ch)
   (b-inc-melee-atk-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 4.0 +input-text-h+))
				t)
    :initarg :b-inc-melee-atk-ch
    :accessor b-inc-melee-atk-ch)
   (b-dec-melee-atk-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 4.0 +input-text-h+))
				nil)
    :initarg :b-dec-melee-atk-ch
    :accessor b-dec-melee-atk-ch)
   (lb-range-atk-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 5.0 +input-text-h+))
			      :prefix (_ "Long range attack chance: ")
			      :label "")
     :initarg  :lb-range-atk-ch
     :accessor lb-range-atk-ch)
   (b-inc-range-atk-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 5.0 +input-text-h+))
				t)
    :initarg :b-inc-range-atk-ch
    :accessor b-inc-range-atk-ch)
   (b-dec-range-atk-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 5.0 +input-text-h+))
				nil)
    :initarg :b-dec-range-atk-ch
    :accessor b-dec-range-atk-ch)
   (lb-melee-atk-dmg
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 6.0 +input-text-h+))
			      :prefix (_ "Short Range attack damage: ")
			      :label "")
     :initarg  :lb-melee-atk-dmg
     :accessor lb-melee-atk-dmg)
   (b-inc-melee-atk-dmg
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 6.0 +input-text-h+))
				t)
    :initarg :b-inc-melee-atk-dmg
    :accessor b-inc-melee-atk-dmg)
   (b-dec-melee-atk-dmg
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 6.0 +input-text-h+))
				nil)
    :initarg :b-dec-melee-atk-dmg
    :accessor b-dec-melee-atk-dmg)
   (lb-range-atk-dmg
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 7.0 +input-text-h+))
			      :prefix (_ "Long Range attack damage: ")
			      :label "")
     :initarg  :lb-range-atk-dmg
     :accessor lb-range-atk-dmg)
   (b-inc-range-atk-dmg
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 7.0 +input-text-h+))
				t)
    :initarg :b-inc-range-atk-dmg
    :accessor b-inc-range-atk-dmg)
   (b-dec-range-atk-dmg
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 7.0 +input-text-h+))
				nil)
    :initarg :b-dec-range-atk-dmg
    :accessor b-dec-range-atk-dmg)
   (lb-edge-wpn-ch-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 8.0 +input-text-h+))
			      :prefix (_ "Edge weapon chance bonus: ")
			      :label "")
     :initarg  :lb-edge-wpn-ch-bonus
     :accessor lb-edge-wpn-ch-bonus)
   (b-inc-edge-wpn-ch-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 8.0 +input-text-h+))
				t)
    :initarg :b-inc-edge-wpn-ch-bonus
    :accessor b-inc-edge-wpn-ch-bonus)
   (b-dec-edge-wpn-ch-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 8.0 +input-text-h+))
				nil)
    :initarg :b-dec-edge-wpn-ch-bonus
    :accessor b-dec-edge-wpn-ch-bonus)
   (lb-edge-wpn-dmg-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 9.0 +input-text-h+))
			      :prefix (_ "Edge weapon damage bonus: ")
			      :label "")
     :initarg  :lb-edge-wpn-dmg-bonus
     :accessor lb-edge-wpn-dmg-bonus)
   (b-inc-edge-wpn-dmg-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 9.0 +input-text-h+))
				t)
    :initarg :b-inc-edge-wpn-dmg-bonus
    :accessor b-inc-edge-wpn-dmg-bonus)
   (b-dec-edge-wpn-dmg-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 9.0 +input-text-h+))
				nil)
    :initarg :b-dec-edge-wpn-dmg-bonus
    :accessor b-dec-edge-wpn-dmg-bonus)
   (lb-impact-wpn-ch-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 10.0 +input-text-h+))
			      :prefix (_ "Impact weapon chance bonus: ")
			      :label "")
     :initarg  :lb-impact-wpn-ch-bonus
     :accessor lb-impact-wpn-ch-bonus)
   (b-inc-impact-wpn-ch-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 10.0 +input-text-h+))
				t)
    :initarg :b-inc-impact-wpn-ch-bonus
    :accessor b-inc-impact-wpn-ch-bonus)
   (b-dec-impact-wpn-ch-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 10.0 +input-text-h+))
				nil)
    :initarg :b-dec-impact-wpn-ch-bonus
    :accessor b-dec-impact-wpn-ch-bonus)
   (lb-impact-wpn-dmg-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 11.0 +input-text-h+))
			      :prefix (_ "Impact weapon damage bonus: ")
			      :label "")
     :initarg  :lb-impact-wpn-dmg-bonus
     :accessor lb-impact-wpn-dmg-bonus)
   (b-inc-impact-wpn-dmg-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 11.0 +input-text-h+))
				t)
    :initarg :b-inc-impact-wpn-dmg-bonus
    :accessor b-inc-impact-wpn-dmg-bonus)
   (b-dec-impact-wpn-dmg-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 11.0 +input-text-h+))
				nil)
    :initarg :b-dec-impact-wpn-dmg-bonus
    :accessor b-dec-impact-wpn-dmg-bonus)
   (lb-pole-wpn-ch-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 12.0 +input-text-h+))
			      :prefix (_ "Pole weapon chance bonus: ")
			      :label "")
     :initarg  :lb-pole-wpn-ch-bonus
     :accessor lb-pole-wpn-ch-bonus)
   (b-inc-pole-wpn-ch-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 12.0 +input-text-h+))
				t)
    :initarg :b-inc-pole-wpn-ch-bonus
    :accessor b-inc-pole-wpn-ch-bonus)
   (b-dec-pole-wpn-ch-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 12.0 +input-text-h+))
				nil)
    :initarg :b-dec-pole-wpn-ch-bonus
    :accessor b-dec-pole-wpn-ch-bonus)
   (lb-pole-wpn-dmg-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 13.0 +input-text-h+))
			      :prefix (_ "Pole weapon damage bonus: ")
			      :label "")
     :initarg  :lb-pole-wpn-dmg-bonus
     :accessor lb-pole-wpn-dmg-bonus)
   (b-inc-pole-wpn-dmg-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 13.0 +input-text-h+))
				t)
    :initarg :b-inc-pole-wpn-dmg-bonus
    :accessor b-inc-pole-wpn-dmg-bonus)
   (b-dec-pole-wpn-dmg-bonus
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 13.0 +input-text-h+))
				nil)
    :initarg :b-dec-pole-wpn-dmg-bonus
    :accessor b-dec-pole-wpn-dmg-bonus)
   (lb-unlock-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 14.0 +input-text-h+))
			      :prefix (_ "Unlock chance: ")
			      :label "")
     :initarg  :lb-unlock-ch
     :accessor lb-unlock-ch)
   (b-inc-unlock-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 14.0 +input-text-h+))
				t)
    :initarg :b-inc-unlock-ch
    :accessor b-inc-unlock-ch)
   (b-dec-unlock-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 14.0 +input-text-h+))
				nil)
    :initarg :b-dec-unlock-ch
    :accessor b-dec-unlock-ch)
   (lb-deactivate-trap-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 15.0 +input-text-h+))
			      :prefix (_ "Deactivate trap chance: ")
			      :label "")
     :initarg  :lb-deactivate-trap-ch
     :accessor lb-deactivate-trap-ch)
   (b-inc-deactivate-trap-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 15.0 +input-text-h+))
				t)
    :initarg :b-inc-deactivate-trap-ch
    :accessor b-inc-deactivate-trap-ch)
   (b-dec-deactivate-trap-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 15.0 +input-text-h+))
				nil)
    :initarg :b-dec-deactivate-trap-ch
    :accessor b-dec-deactivate-trap-ch)
   (lb-reply-attack-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 16.0 +input-text-h+))
			      :prefix (_ "Reply to attack chance: ")
			      :label "")
     :initarg  :lb-reply-attack-ch
     :accessor lb-reply-attack-ch)
   (b-inc-reply-attack-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 16.0 +input-text-h+))
				t)
    :initarg :b-inc-reply-attack-ch
    :accessor b-inc-reply-attack-ch)
   (b-dec-reply-attack-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 16.0 +input-text-h+))
				nil)
    :initarg :b-dec-reply-attack-ch
    :accessor b-dec-reply-attack-ch)
   (lb-ambush-attack-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 17.0 +input-text-h+))
			      :prefix (_ "Ambush attack chance: ")
			      :label "")
     :initarg  :lb-ambush-attack-ch
     :accessor lb-ambush-attack-ch)
   (b-inc-ambush-attack-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 17.0 +input-text-h+))
				t)
    :initarg :b-inc-ambush-attack-ch
    :accessor b-inc-ambush-attack-ch)
   (b-dec-ambush-attack-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 17.0 +input-text-h+))
				nil)
    :initarg :b-dec-ambush-attack-ch
    :accessor b-dec-ambush-attack-ch)
   (lb-spell-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 18.0 +input-text-h+))
			      :prefix (_ "Spell chance: ")
			      :label "")
     :initarg  :lb-spell-ch
     :accessor lb-spell-ch)
   (b-inc-spell-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 18.0 +input-text-h+))
				t)
    :initarg :b-inc-spell-ch
    :accessor b-inc-spell-ch)
   (b-dec-spell-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 18.0 +input-text-h+))
				nil)
    :initarg :b-dec-spell-ch
    :accessor b-dec-spell-ch)
   (lb-attack-spell-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 19.0 +input-text-h+))
			      :prefix (_ "Attack spell chance: ")
			      :label "")
     :initarg  :lb-attack-spell-ch
     :accessor lb-attack-spell-ch)
   (b-inc-attack-spell-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+))
				(d+ (d* 1.8 +portrait-size+) (d* 19.0 +input-text-h+))
				t)
    :initarg :b-inc-attack-spell-ch
    :accessor b-inc-attack-spell-ch)
   (b-dec-attack-spell-ch
    :initform (make-pgen-button (d+ (d+ (d* 2.0 +spacing+) (d/ (d *window-w*) 12.0))
				    (d* 2.0 +input-text-w+)
				    +input-text-h+)
				(d+ (d* 1.8 +portrait-size+) (d* 19.0 +input-text-h+))
				nil)
    :initarg :b-dec-attack-spell-ch
    :accessor b-dec-attack-spell-ch)
   (lb-level
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 20.0 +input-text-h+))
			      :prefix (_ "Level: ")
			      :label "")
     :initarg  :lb-level
     :accessor lb-level)
   (lb-exp-points
     :initform (make-instance 'simple-label-prefixed
			      :width  (d* 2.0 +input-text-w+)
			      :height +input-text-h+
			      :x (d+ +spacing+ (d/ (d *window-w*) 12.0))
			      :y (d+ (d* 1.8 +portrait-size+) (d* 21.0 +input-text-h+))
			      :prefix (_ "Experience points: ")
			      :label "")
     :initarg  :lb-exp-points
     :accessor lb-exp-points)
   (input-name
    :initform (make-instance 'text-field
			     :width  +input-text-w+
			     :height +input-text-h+
			     :x (d+ (d* 2.0 +spacing+)
				    +portrait-size+
				    (d/ (d *window-w*) 12.0))
			     :y 0.0
			     :label (_ "Name"))
    :initarg :input-name
    :accessor input-name)
   (input-last-name
    :initform (make-instance 'text-field
			     :width  +input-text-w+
			     :height +input-text-h+
			     :x (d+ (d* 2.0 +spacing+)
				    +portrait-size+
				    (d/ (d *window-w*) 12.0))
			     :y (d+ +spacing+ +input-text-h+)
			     :label (_ "Last name"))
    :initarg :input-last-name
    :accessor input-last-name)
   (lb-strength
    :initform (make-instance 'simple-label-prefixed
			     :width  +input-text-w+
			     :height +input-text-h+
			     :x (d+ (d* 2.0 +spacing+)
				    +portrait-size+
				    +input-text-w+
				    (d/ (d *window-w*) 12.0))
			     :y 0.0
			     :prefix (_ "STR: ")
			     :label "")
    :initarg  :lb-strength
    :accessor lb-strength)
   (lb-stamina
    :initform (make-instance 'simple-label-prefixed
			     :width  +input-text-w+
			     :height +input-text-h+
			     :x (d+ (d* 2.0 +spacing+)
				    +portrait-size+
				    +input-text-w+
				    (d/ (d *window-w*) 12.0))
			     :y +input-text-h+
			     :prefix (_ "ST:  ")
			     :label "")
    :initarg  :lb-stamina
    :accessor lb-stamina)
   (lb-dexterity
    :initform (make-instance 'simple-label-prefixed
			     :width  +input-text-w+
			     :height +input-text-h+
			     :x (d+ (d* 2.0 +spacing+)
				    +portrait-size+
				    +input-text-w+
				    (d/ (d *window-w*) 12.0))
			     :y (d* 2.0 (d+ +spacing+ +input-text-h+))
			     :prefix (_ "DX:  ")
			     :label "")
    :initarg  :lb-dexterity
    :accessor lb-dexterity)
   (lb-agility
    :initform (make-instance 'simple-label-prefixed
			     :width  +input-text-w+
			     :height +input-text-h+
			     :x (d+ (d* 2.0 +spacing+)
				    +portrait-size+
				    +input-text-w+
				    (d/ (d *window-w*) 12.0))
			     :y (d* 3.0 (d+ +spacing+ +input-text-h+))
			     :prefix (_ "AG:  ")
			     :label "")
    :initarg  :lb-agility
    :accessor lb-agility)
   (lb-smartness
    :initform (make-instance 'simple-label-prefixed
			     :width  +input-text-w+
			     :height +input-text-h+
			     :x (d+ (d* 2.0 +spacing+)
				    +portrait-size+
				    +input-text-w+
				    (d/ (d *window-w*) 12.0))
			     :y (d* 4.0 (d+ +spacing+ +input-text-h+))
			     :prefix (_ "SM:  ")
			     :label "")
    :initarg  :lb-smartness
    :accessor lb-smartness)
   (lb-empaty
    :initform (make-instance 'simple-label-prefixed
			     :width  +input-text-w+
			     :height +input-text-h+
			     :x (d+ (d* 2.0 +spacing+)
				    +portrait-size+
				    +input-text-w+
				    (d/ (d *window-w*) 12.0))
			     :y (d* 5.0 (d+ +spacing+ +input-text-h+))
			     :prefix (_ "EM:  ")
			     :label "")
    :initarg  :lb-empaty
    :accessor lb-empaty)
   (lb-weight
    :initform (make-instance 'simple-label-prefixed
			     :width  +input-text-w+
			     :height +input-text-h+
			     :x (d+ (d* 2.0 +spacing+)
				    +portrait-size+
				    +input-text-w+
				    (d/ (d *window-w*) 12.0))
			     :y (d* 6.0 (d+ +spacing+ +input-text-h+))
			     :prefix (_ "WG:  ")
			     :label "")
    :initarg  :lb-weight
    :accessor lb-weight)))

(defmethod initialize-instance :after ((object player-generator) &key &allow-other-keys)
  (with-accessors ((player player)
		   (lb-class lb-class)
		   (checkb-warrior checkb-warrior)
		   (checkb-wizard checkb-wizard)
		   (checkb-healer checkb-healer)
		   (checkb-archer checkb-archer)
		   (checkb-ranger checkb-ranger)
		   (lb-gender lb-gender)
		   (checkb-male checkb-male)
		   (checkb-female checkb-female)
		   (b-generate b-generate)
		   (b-save b-save)
		   (b-load b-load)
		   (img-portrait img-portrait)
		   (lb-damage-pt lb-damage-pt)
 		   (b-inc-damage-pt b-inc-damage-pt)
		   (b-dec-damage-pt b-dec-damage-pt)
		   (lb-movement-pt lb-movement-pt)
		   (b-inc-movement-pt b-inc-movement-pt)
		   (b-dec-movement-pt b-dec-movement-pt)
		   (lb-magic-pt lb-magic-pt)
		   (b-inc-magic-pt b-inc-magic-pt)
		   (b-dec-magic-pt b-dec-magic-pt)
		   (lb-dodge-ch lb-dodge-ch)
		   (b-inc-dodge-ch b-inc-dodge-ch)
		   (b-dec-dodge-ch b-dec-dodge-ch)
		   (lb-melee-atk-ch lb-melee-atk-ch)
		   (b-inc-melee-atk-ch b-inc-melee-atk-ch)
		   (b-dec-melee-atk-ch b-dec-melee-atk-ch)
		   (lb-range-atk-ch lb-range-atk-ch)
		   (b-inc-range-atk-ch b-inc-range-atk-ch)
		   (b-dec-range-atk-ch b-dec-range-atk-ch)
		   (lb-melee-atk-dmg lb-melee-atk-dmg)
		   (b-inc-melee-atk-dmg b-inc-melee-atk-dmg)
		   (b-dec-melee-atk-dmg b-dec-melee-atk-dmg)
		   (lb-range-atk-dmg lb-range-atk-dmg)
		   (b-inc-range-atk-dmg b-inc-range-atk-dmg)
		   (b-dec-range-atk-dmg b-dec-range-atk-dmg)
		   (lb-edge-wpn-ch-bonus lb-edge-wpn-ch-bonus)
		   (b-inc-edge-wpn-ch-bonus b-inc-edge-wpn-ch-bonus)
		   (b-dec-edge-wpn-ch-bonus b-dec-edge-wpn-ch-bonus)
		   (lb-edge-wpn-dmg-bonus lb-edge-wpn-dmg-bonus)
		   (b-inc-edge-wpn-dmg-bonus b-inc-edge-wpn-dmg-bonus)
		   (b-dec-edge-wpn-dmg-bonus b-dec-edge-wpn-dmg-bonus)
		   (lb-impact-wpn-ch-bonus lb-impact-wpn-ch-bonus)
		   (b-inc-impact-wpn-ch-bonus b-inc-impact-wpn-ch-bonus)
		   (b-dec-impact-wpn-ch-bonus b-dec-impact-wpn-ch-bonus)
		   (lb-impact-wpn-dmg-bonus lb-impact-wpn-dmg-bonus)
		   (b-inc-impact-wpn-dmg-bonus b-inc-impact-wpn-dmg-bonus)
		   (b-dec-impact-wpn-dmg-bonus b-dec-impact-wpn-dmg-bonus)
		   (lb-pole-wpn-ch-bonus lb-pole-wpn-ch-bonus)
		   (b-inc-pole-wpn-ch-bonus b-inc-pole-wpn-ch-bonus)
		   (b-dec-pole-wpn-ch-bonus b-dec-pole-wpn-ch-bonus)
		   (lb-pole-wpn-dmg-bonus lb-pole-wpn-dmg-bonus)
		   (b-inc-pole-wpn-dmg-bonus b-inc-pole-wpn-dmg-bonus)
		   (b-dec-pole-wpn-dmg-bonus b-dec-pole-wpn-dmg-bonus)
		   (lb-unlock-ch lb-unlock-ch)
		   (b-inc-unlock-ch b-inc-unlock-ch)
		   (b-dec-unlock-ch b-dec-unlock-ch)
		   (lb-deactivate-trap-ch lb-deactivate-trap-ch)
		   (b-inc-deactivate-trap-ch b-inc-deactivate-trap-ch)
		   (b-dec-deactivate-trap-ch b-dec-deactivate-trap-ch)
		   (lb-reply-attack-ch lb-reply-attack-ch)
		   (b-inc-reply-attack-ch b-inc-reply-attack-ch)
		   (b-dec-reply-attack-ch b-dec-reply-attack-ch)
		   (lb-ambush-attack-ch lb-ambush-attack-ch)
		   (b-inc-ambush-attack-ch b-inc-ambush-attack-ch)
		   (b-dec-ambush-attack-ch b-dec-ambush-attack-ch)
		   (lb-spell-ch lb-spell-ch )
		   (b-inc-spell-ch b-inc-spell-ch)
		   (b-dec-spell-ch b-dec-spell-ch)
		   (lb-attack-spell-ch lb-attack-spell-ch)
		   (b-inc-attack-spell-ch b-inc-attack-spell-ch)
		   (b-dec-attack-spell-ch b-dec-attack-spell-ch)
		   (lb-level lb-level)
		   (lb-exp-points lb-exp-points)
		   (input-name input-name)
		   (input-last-name input-last-name)
		   (lb-strength lb-strength)
		   (lb-stamina lb-stamina)
		   (lb-dexterity lb-dexterity)
		   (lb-agility lb-agility)
		   (lb-smartness lb-smartness)
		   (lb-empaty lb-empaty)
		   (lb-weight lb-weight)) object
    (let ((group-class (make-check-group* checkb-warrior
					  checkb-wizard
					  checkb-healer
					  checkb-archer
					  checkb-ranger)))
      (setf (group checkb-warrior) group-class)
      (setf (group checkb-wizard)  group-class)
      (setf (group checkb-healer)  group-class)
      (setf (group checkb-archer)  group-class)
      (setf (group checkb-ranger)  group-class))
    (flip-state checkb-warrior)
    (add-child object checkb-warrior)
    (add-child object checkb-wizard)
    (add-child object checkb-healer)
    (add-child object checkb-archer)
    (add-child object checkb-ranger)
    (add-child object lb-class)
    ;; gender
    (let ((group-gender (make-check-group* checkb-male checkb-female)))
      (setf (group checkb-male)   group-gender)
      (setf (group checkb-female) group-gender))
    (flip-state checkb-male)
    (add-child object lb-gender)
    (add-child object checkb-male)
    (add-child object checkb-female)
    ;; actions
    (add-child object b-generate)
    (add-child object b-save)
    (add-child object b-load)
    (setf (callback b-load) #'player-load-cb)
    ;; second column
    (add-child object img-portrait)
    (add-child object lb-damage-pt)
    (add-child object b-inc-damage-pt)
    (add-child object b-dec-damage-pt)
    (add-child object lb-movement-pt)
    (add-child object b-inc-movement-pt)
    (add-child object b-dec-movement-pt)
    (add-child object lb-magic-pt)
    (add-child object b-inc-magic-pt)
    (add-child object b-dec-magic-pt)
    (add-child object lb-dodge-ch)
    (add-child object b-inc-dodge-ch)
    (add-child object b-dec-dodge-ch)
    (add-child object lb-melee-atk-ch)
    (add-child object b-inc-melee-atk-ch)
    (add-child object b-dec-melee-atk-ch)
    (add-child object lb-range-atk-ch)
    (add-child object b-inc-range-atk-ch)
    (add-child object b-dec-range-atk-ch)
    (add-child object lb-melee-atk-dmg)
    (add-child object b-inc-melee-atk-dmg)
    (add-child object b-dec-melee-atk-dmg)
    (add-child object lb-range-atk-dmg)
    (add-child object b-inc-range-atk-dmg)
    (add-child object b-dec-range-atk-dmg)
    (add-child object lb-edge-wpn-ch-bonus)
    (add-child object b-inc-edge-wpn-ch-bonus)
    (add-child object b-dec-edge-wpn-ch-bonus)
    (add-child object lb-edge-wpn-dmg-bonus)
    (add-child object b-inc-edge-wpn-dmg-bonus)
    (add-child object b-dec-edge-wpn-dmg-bonus)
    (add-child object lb-impact-wpn-ch-bonus)
    (add-child object b-inc-impact-wpn-ch-bonus)
    (add-child object b-dec-impact-wpn-ch-bonus)
    (add-child object lb-impact-wpn-dmg-bonus)
    (add-child object b-inc-impact-wpn-dmg-bonus)
    (add-child object b-dec-impact-wpn-dmg-bonus)
    (add-child object lb-pole-wpn-ch-bonus)
    (add-child object b-inc-pole-wpn-ch-bonus)
    (add-child object b-dec-pole-wpn-ch-bonus)
    (add-child object lb-pole-wpn-dmg-bonus)
    (add-child object b-inc-pole-wpn-dmg-bonus)
    (add-child object b-dec-pole-wpn-dmg-bonus)
    (add-child object lb-unlock-ch)
    (add-child object b-inc-unlock-ch)
    (add-child object b-dec-unlock-ch)
    (add-child object lb-deactivate-trap-ch)
    (add-child object b-inc-deactivate-trap-ch)
    (add-child object b-dec-deactivate-trap-ch)
    (add-child object lb-reply-attack-ch)
    (add-child object b-inc-reply-attack-ch)
    (add-child object b-dec-reply-attack-ch)
    (add-child object lb-ambush-attack-ch)
    (add-child object b-inc-ambush-attack-ch)
    (add-child object b-dec-ambush-attack-ch)
    (add-child object lb-spell-ch)
    (add-child object b-inc-spell-ch)
    (add-child object b-dec-spell-ch)
    (add-child object lb-attack-spell-ch)
    (add-child object b-inc-attack-spell-ch)
    (add-child object b-dec-attack-spell-ch)
    (add-child object lb-level)
    (add-child object lb-exp-points)
    ;; third column
    (add-child object input-name)
    (add-child object input-last-name)
    ;; fourth column
    (add-child object lb-strength)
    (add-child object lb-stamina)
    (add-child object lb-dexterity)
    (add-child object lb-agility)
    (add-child object lb-smartness)
    (add-child object lb-empaty)
    (add-child object lb-weight)))

(defun setup-player-character (window &key (from-player nil))
  (setf (player window)
	(or from-player
	    (cond
	      ((button-state (checkb-warrior window))
	       (player-character:make-warrior :human))
	      ((button-state (checkb-wizard window))
	       (player-character:make-wizard :human))
	      ((button-state (checkb-healer window))
	       (player-character:make-healer :human))
	      ((button-state (checkb-archer window))
	       (player-character:make-archer :human))
	      ((button-state (checkb-ranger window))
	       (player-character:make-ranger :human))
	      (t
	       (player-character:make-warrior :human))))))

(defun setup-portrait (window &key (from-player nil))
  (let ((new-portrait (if from-player
			  (player-character:portrait from-player)
			  (cond
			    ((button-state (checkb-male window))
			     (avatar-portrait:build-avatar "m"))
			    (t
			     (avatar-portrait:build-avatar "f")))))
	(texture     (get-texture +portrait-unknown-texture-name+)))
    (setf (pixmap:data texture) (pixmap:data new-portrait))
    (pixmap:sync-data-to-bits texture)
    (update-for-rendering texture)))

(defmacro %add-callback-to-pgen-buttons (b-inc b-dec player slot widget-capital widget-dest
					 &optional (scale-plus 1.0) (scale-minus 1.0))
  `(progn
     (add-callback-to-pgen-button ,b-inc ,player ,slot ,widget-capital ,widget-dest t
				  ,scale-plus ,scale-minus)
     (add-callback-to-pgen-button ,b-dec ,player ,slot ,widget-capital ,widget-dest nil
				  ,scale-plus ,scale-minus)))

(defun generate-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (%setup-character win :new-player nil))
  t)

(defmacro with-file-chooser ((widget fchooser-window) &body body)
  (alexandria:with-gensyms (file-cb file-chooser act-widget)
    `(let* ((,act-widget ,widget)
	    (,file-cb  (lambda (w e)
			 (declare (ignore e))
			 (let ((,fchooser-window (parent w)))
			   ,@body
			   (hide ,fchooser-window)
			   (remove-child (find-root-widget ,act-widget)
					 ,fchooser-window
					 :key #'id
					 :test #'=))))
	    (,file-chooser (widget:make-file-chooser ,file-cb)))
       (setf (compiled-shaders ,file-chooser) (compiled-shaders ,act-widget))
       (add-child (find-root-widget ,act-widget) ,file-chooser))))

(defun player-load-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-file-chooser (button fchooser-window)
      (let ((new-player (deserialize (make-instance 'player-character:player-character)
				     (fetch-file-chooser-path fchooser-window))))
	(%setup-character win :new-player new-player))))
  t)

(defun save-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((input-name input-name) (input-last-name input-last-name)
		     (portrait portrait) (player player)) win
      (with-file-chooser (button fchooser-window)
	(setf (player-character:first-name player) (label input-name)
	      (player-character:last-name  player) (label input-last-name))
	(setf (player-character:portrait   player)
	      (clone (get-texture +portrait-unknown-texture-name+)))
	(with-open-file (stream (fetch-file-chooser-path fchooser-window)
				:direction :output :if-exists :supersede
				:if-does-not-exist :create)
	  (format stream "~a" (serialize player))))))
  t)

(defun %setup-character (win &key (new-player nil))
  (with-accessors ((input-name input-name)
		   (input-last-name input-last-name)
		   (lb-strength lb-strength)
		   (lb-stamina lb-stamina)
		   (lb-dexterity lb-dexterity)
		   (lb-agility lb-agility)
		   (lb-smartness lb-smartness)
		   (lb-empaty lb-empaty)
		   (lb-weight lb-weight)
		   (img-portrait img-portrait)
		   (lb-damage-pt lb-damage-pt)
		   (b-inc-damage-pt b-inc-damage-pt)
		   (b-dec-damage-pt b-dec-damage-pt)
		   (lb-movement-pt lb-movement-pt)
		   (b-inc-movement-pt b-inc-movement-pt)
		   (b-dec-movement-pt b-dec-movement-pt)
		   (lb-magic-pt lb-magic-pt)
		   (b-inc-magic-pt b-inc-magic-pt)
		   (b-dec-magic-pt b-dec-magic-pt)
		   (lb-dodge-ch lb-dodge-ch)
		   (b-inc-dodge-ch b-inc-dodge-ch)
		   (b-dec-dodge-ch b-dec-dodge-ch)
		   (lb-melee-atk-ch lb-melee-atk-ch)
		   (b-inc-melee-atk-ch b-inc-melee-atk-ch)
		   (b-dec-melee-atk-ch b-dec-melee-atk-ch)
		   (lb-range-atk-ch lb-range-atk-ch)
		   (b-inc-range-atk-ch b-inc-range-atk-ch)
		   (b-dec-range-atk-ch b-dec-range-atk-ch)
		   (lb-melee-atk-dmg lb-melee-atk-dmg)
		   (b-inc-melee-atk-dmg b-inc-melee-atk-dmg)
		   (b-dec-melee-atk-dmg b-dec-melee-atk-dmg)
		   (lb-range-atk-dmg lb-range-atk-dmg)
		   (b-inc-range-atk-dmg b-inc-range-atk-dmg)
		   (b-dec-range-atk-dmg b-dec-range-atk-dmg)
		   (lb-edge-wpn-ch-bonus lb-edge-wpn-ch-bonus)
		   (b-inc-edge-wpn-ch-bonus b-inc-edge-wpn-ch-bonus)
		   (b-dec-edge-wpn-ch-bonus b-dec-edge-wpn-ch-bonus)
		   (lb-edge-wpn-dmg-bonus lb-edge-wpn-dmg-bonus)
		   (b-inc-edge-wpn-dmg-bonus b-inc-edge-wpn-dmg-bonus)
		   (b-dec-edge-wpn-dmg-bonus b-dec-edge-wpn-dmg-bonus)
		   (lb-impact-wpn-ch-bonus lb-impact-wpn-ch-bonus)
		   (b-inc-impact-wpn-ch-bonus b-inc-impact-wpn-ch-bonus)
		   (b-dec-impact-wpn-ch-bonus b-dec-impact-wpn-ch-bonus)
		   (lb-impact-wpn-dmg-bonus lb-impact-wpn-dmg-bonus)
		   (b-inc-impact-wpn-dmg-bonus b-inc-impact-wpn-dmg-bonus)
		   (b-dec-impact-wpn-dmg-bonus b-dec-impact-wpn-dmg-bonus)
		   (lb-pole-wpn-ch-bonus lb-pole-wpn-ch-bonus)
		   (b-inc-pole-wpn-ch-bonus b-inc-pole-wpn-ch-bonus)
		   (b-dec-pole-wpn-ch-bonus b-dec-pole-wpn-ch-bonus)
		   (lb-pole-wpn-dmg-bonus lb-pole-wpn-dmg-bonus)
		   (b-inc-pole-wpn-dmg-bonus b-inc-pole-wpn-dmg-bonus)
		   (b-dec-pole-wpn-dmg-bonus b-dec-pole-wpn-dmg-bonus)
		   (lb-unlock-ch lb-unlock-ch)
		   (b-inc-unlock-ch b-inc-unlock-ch)
		   (b-dec-unlock-ch b-dec-unlock-ch)
		   (lb-deactivate-trap-ch lb-deactivate-trap-ch)
		   (b-inc-deactivate-trap-ch b-inc-deactivate-trap-ch)
		   (b-dec-deactivate-trap-ch b-dec-deactivate-trap-ch)
		   (lb-reply-attack-ch lb-reply-attack-ch)
		   (b-inc-reply-attack-ch b-inc-reply-attack-ch)
		   (b-dec-reply-attack-ch b-dec-reply-attack-ch)
		   (lb-ambush-attack-ch lb-ambush-attack-ch)
		   (b-inc-ambush-attack-ch b-inc-ambush-attack-ch)
		   (b-dec-ambush-attack-ch b-dec-ambush-attack-ch)
		   (lb-spell-ch lb-spell-ch )
		   (b-inc-spell-ch b-inc-spell-ch)
		   (b-dec-spell-ch b-dec-spell-ch)
		   (lb-attack-spell-ch lb-attack-spell-ch)
		   (b-inc-attack-spell-ch b-inc-attack-spell-ch)
		   (b-dec-attack-spell-ch b-dec-attack-spell-ch)
		   (lb-level lb-level)
		   (lb-exp-points lb-exp-points)
		   (player player)) win
    (setup-player-character win :from-player new-player)
    (when (not new-player)
      (random-names:load-db +random-first-names-filename+))
    (setf (label input-name) (if new-player
				 (player-character:first-name new-player)
				 (random-names:generate)))
    (when (not new-player)
      (random-names:load-db +random-last-names-filename+))
    (setf (label input-last-name) (if new-player
				      (player-character:last-name new-player)
				      (random-names:generate)))
    (when (not new-player)
      (setf (player-character:exp-points player) player-character:+starting-exp-points+))
    (setup-portrait win :from-player new-player)
    (setf (label lb-strength)  (format nil "~,2f" (player-character:strength player)))
    (setf (label lb-stamina)   (format nil "~,2f" (player-character:stamina player)))
    (setf (label lb-dexterity) (format nil "~,2f" (player-character:dexterity player)))
    (setf (label lb-agility)   (format nil "~,2f" (player-character:agility player)))
    (setf (label lb-smartness) (format nil "~,2f" (player-character:smartness player)))
    (setf (label lb-empaty)    (format nil "~,2f" (player-character:empaty player)))
    (setf (label lb-weight)    (format nil "~,2f" (player-character:weight player)))
    (setf (label lb-damage-pt)           (format nil "~,2f"
						 (player-character:damage-points player)))
    (%add-callback-to-pgen-buttons b-inc-damage-pt b-dec-damage-pt
				   player 'player-character:damage-points
				   lb-exp-points lb-damage-pt 0.1 1.0)
    (setf (label lb-movement-pt)	   (format nil "~,2f"
						   (player-character:movement-points player)))
    (%add-callback-to-pgen-buttons b-inc-movement-pt b-dec-movement-pt
				   player 'player-character:movement-points
				   lb-exp-points lb-movement-pt 0.5 1.0)

    (setf (label lb-magic-pt)		    (format nil "~,2f"
						    (player-character:magic-points player)))
    (%add-callback-to-pgen-buttons b-inc-magic-pt b-dec-magic-pt
				   player 'player-character:magic-points
				   lb-exp-points lb-magic-pt 0.5 1.0)
    (setf (label lb-dodge-ch)		    (format nil "~,2f"
						    (player-character:dodge-chance player)))
    (%add-callback-to-pgen-buttons b-inc-dodge-ch b-dec-dodge-ch
				   player 'player-character:dodge-chance
				   lb-exp-points lb-dodge-ch 0.5 1.0)
    (setf (label lb-melee-atk-ch)	    (format nil "~,2f"
						    (player-character:melee-attack-chance player)))
    (%add-callback-to-pgen-buttons b-inc-melee-atk-ch b-dec-melee-atk-ch
				   player 'player-character:melee-attack-chance
				   lb-exp-points lb-melee-atk-ch)
    (setf (label lb-range-atk-ch)	    (format nil "~,2f"
						    (player-character:range-attack-chance player)))
    (%add-callback-to-pgen-buttons b-inc-range-atk-ch b-dec-range-atk-ch
				   player 'player-character:range-attack-chance
				   lb-exp-points lb-range-atk-ch 0.5 1.0)
    (setf (label lb-melee-atk-dmg)	    (format nil "~,2f"
						    (player-character:melee-attack-damage player)))
    (%add-callback-to-pgen-buttons b-inc-melee-atk-dmg b-dec-melee-atk-dmg
				   player 'player-character:melee-attack-damage
				   lb-exp-points lb-melee-atk-dmg 0.25 1.0)
    (setf (label lb-range-atk-dmg)	    (format nil "~,2f"
						    (player-character:range-attack-damage player)))
    (%add-callback-to-pgen-buttons b-inc-range-atk-dmg b-dec-range-atk-dmg
				   player 'player-character:range-attack-damage
				   lb-exp-points lb-range-atk-dmg 0.25 1.0)
    (setf (label lb-edge-wpn-ch-bonus)    (format nil "~,2f"
						  (player-character:edge-weapons-chance-bonus player)))
    (%add-callback-to-pgen-buttons b-inc-edge-wpn-ch-bonus b-dec-edge-wpn-ch-bonus
				   player 'player-character:edge-weapons-chance-bonus
				   lb-exp-points lb-edge-wpn-ch-bonus 0.25 1.0)
    (setf (label lb-edge-wpn-dmg-bonus)   (format nil "~,2f"
						  (player-character:edge-weapons-damage-bonus player)))
    (%add-callback-to-pgen-buttons b-inc-edge-wpn-dmg-bonus b-dec-edge-wpn-dmg-bonus
				   player 'player-character:edge-weapons-damage-bonus
				   lb-exp-points lb-edge-wpn-dmg-bonus 0.25 1.0)
    (setf (label lb-impact-wpn-ch-bonus)  (format nil "~,2f"
						  (player-character:impact-weapons-chance-bonus player)))
    (%add-callback-to-pgen-buttons b-inc-impact-wpn-ch-bonus b-dec-impact-wpn-ch-bonus
				   player 'player-character:impact-weapons-chance-bonus
				   lb-exp-points lb-impact-wpn-ch-bonus 0.25 1.0)
    (setf (label lb-impact-wpn-dmg-bonus) (format nil "~,2f"
						  (player-character:impact-weapons-damage-bonus player)))
    (%add-callback-to-pgen-buttons b-inc-impact-wpn-dmg-bonus b-dec-impact-wpn-dmg-bonus
				   player 'player-character:impact-weapons-damage-bonus
				   lb-exp-points lb-impact-wpn-dmg-bonus 0.25 1.0)
    (setf (label lb-pole-wpn-ch-bonus)    (format nil "~,2f"
						  (player-character:pole-weapons-chance-bonus player)))
    (%add-callback-to-pgen-buttons b-inc-pole-wpn-ch-bonus b-dec-pole-wpn-ch-bonus
				   player 'player-character:pole-weapons-chance-bonus
				   lb-exp-points lb-pole-wpn-ch-bonus 0.25 1.0)
    (setf (label lb-pole-wpn-dmg-bonus)   (format nil "~,2f"
						  (player-character:pole-weapons-damage-bonus player)))
    (%add-callback-to-pgen-buttons b-inc-pole-wpn-dmg-bonus b-dec-pole-wpn-dmg-bonus
				   player 'player-character:pole-weapons-damage-bonus
				   lb-exp-points lb-pole-wpn-dmg-bonus 0.25 1.0)

    (setf (label lb-unlock-ch)	    (format nil "~,2f"
					    (player-character:unlock-chance player)))
    (%add-callback-to-pgen-buttons b-inc-unlock-ch b-dec-unlock-ch
				   player 'player-character:unlock-chance
				   lb-exp-points lb-unlock-ch 0.5 1.0)
    (setf (label lb-deactivate-trap-ch)   (format nil "~,2f"
						  (player-character:deactivate-trap-chance player)))
    (%add-callback-to-pgen-buttons b-inc-deactivate-trap-ch b-dec-deactivate-trap-ch
				   player 'player-character:deactivate-trap-chance
				   lb-exp-points lb-deactivate-trap-ch 0.5 1.0)

    (setf (label lb-reply-attack-ch)	    (format nil "~,2f"
						    (player-character:reply-attack-chance player)))
    (%add-callback-to-pgen-buttons b-inc-reply-attack-ch b-dec-reply-attack-ch
				   player 'player-character:reply-attack-chance
				   lb-exp-points lb-reply-attack-ch 0.33 1.0)
    (setf (label lb-ambush-attack-ch)	    (format nil "~,2f"
						    (player-character:ambush-attack-chance player)))
    (%add-callback-to-pgen-buttons b-inc-ambush-attack-ch b-dec-ambush-attack-ch
				   player 'player-character:ambush-attack-chance
				   lb-exp-points lb-ambush-attack-ch 0.33 1.0)
    (setf (label lb-spell-ch)	    (format nil "~,2f"
					    (player-character:spell-chance player)))
    (%add-callback-to-pgen-buttons b-inc-spell-ch b-dec-spell-ch
				   player 'player-character:spell-chance
				   lb-exp-points lb-spell-ch 0.25 1.0)
    (setf (label lb-attack-spell-ch)  (format nil "~,2f"
					      (player-character:attack-spell-chance player)))
    (%add-callback-to-pgen-buttons b-inc-attack-spell-ch b-dec-attack-spell-ch
				   player 'player-character:attack-spell-chance
				   lb-exp-points lb-attack-spell-ch 0.25 1.0)
    (setf (label lb-level)	    (format nil "~d"
					    (player-character:level player)))
    (setf (label lb-exp-points)	    (format nil "~d" (player-character:exp-points player)))
    t))


(defun make-player-generator ()
  (make-instance 'player-generator
		 :x 0.0 :y 200.0
		 :width  (d/ (d *window-w*) 2.0)
		 :height (d/ (d *window-h*) 2.0)
		 :label  (_ "Generate character")))

(defclass inventory-slot-button (signalling-light)
  ((contained-id
    :initform nil
    :initarg  :contained-id
    :accessor contained-id)))

(defun y-just-under-slot-page ()
  (d+ (d* (d +slots-per-page-side-size+)
	  (small-square-button-size *reference-sizes*))
      (spacing                      *reference-sizes*)))

(defun make-inventory-slot-button (x y)
  (make-instance 'check-button
		 :theme           nil
		 :x               x
		 :y               y
		 :width           (small-square-button-size *reference-sizes*)
		 :height          (small-square-button-size *reference-sizes*)
		 :texture-object  (get-texture +inventory-slot-texture-name+)
		 :texture-pressed (get-texture +inventory-slot-selected-texture-name+)
		 :texture-overlay (get-texture +plus-overlay-texture-name+)
		 :contained-id    nil
		 :callback        nil))

(defun show/hide-chest-slots-cb (w e)
  (declare (ignore e))
  (let* ((parent (parent w))
	 (ch-0   (chest-slot-0 parent))
	 (ch-1   (chest-slot-1 parent))
	 (ch-2   (chest-slot-2 parent)))
    (if (button-state w)
	(progn
	  (show ch-0)
	  (show ch-1)
	  (show ch-2))
	(progn
	  (hide ch-0)
	  (hide ch-1)
	  (hide ch-2)))))

(defclass inventory-window (window)
  ((owner
   :initform nil
   :initarg  :owner
   :accessor owner)
   (slots-pages
   :initform '()
   :initarg  :slots-pages
   :accessor slots-pages)
   (chest-slot-0
    :initform (make-inventory-slot-button (small-square-button-size *reference-sizes*)
					  (y-just-under-slot-page))
    :initarg  :chest-slot-0
    :accessor chest-slot-0)
   (chest-slot-1
    :initform (make-inventory-slot-button (d* 2.0 (small-square-button-size *reference-sizes*))
					  (y-just-under-slot-page))
    :initarg  :chest-slot-1
    :accessor chest-slot-1)
   (chest-slot-2
    :initform (make-inventory-slot-button (d* 3.0 (small-square-button-size *reference-sizes*))
					  (y-just-under-slot-page))
    :initarg  :chest-slot-2
    :accessor chest-slot-2)
   (current-slot-page
    :initform nil
    :initarg  :current-slot-page
    :accessor current-slot-page)
   (img-chest
    :initform (make-instance 'toggle-button
			     :width  (small-square-button-size *reference-sizes*)
			     :height (small-square-button-size *reference-sizes*)
			     :x      0.0
			     :y      (y-just-under-slot-page)
			     :texture-object  (get-texture +chest-closed-texture-name+)
			     :texture-pressed (get-texture +chest-opened-texture-name+)
			     :texture-overlay (get-texture +transparent-texture-name+)
			     :callback        #'show/hide-chest-slots-cb
			     :button-status   nil)
    :initarg  :img-chest
    :accessor img-chest)
   (b-close
    :initform (make-instance 'naked-button
			     :x 0.0
			     :y (d- *inventory-window-h*
				    (tiny-square-button-size *reference-sizes*))
			     :width  (tiny-square-button-size *reference-sizes*)
			     :height (tiny-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +square-button-texture-name+)
			     :texture-pressed (get-texture +square-button-pressed-texture-name+)
			     :texture-overlay (get-texture +button-cancel-texture-name+)
			     :callback        #'hide-parent-cb)
    :initarg  :b-close
    :accessor b-close)))

(defmethod initialize-instance :after ((object inventory-window) &key &allow-other-keys)
  (with-accessors ((slots-pages slots-pages) (current-slot-page current-slot-page)
		   (owner owner) (b-close b-close) (img-chest img-chest)
		   (chest-slot-0 chest-slot-0) (chest-slot-1 chest-slot-1)
		   (chest-slot-2 chest-slot-2))
      object
    (let ((page-count (if owner
			  (player-character:inventory-slot-pages-number owner)
			  1)))
      (setf slots-pages
	    (loop repeat page-count collect
		 (let ((page '())
		       (button-size (small-square-button-size *reference-sizes*)))
		   (loop for i from 0 below +slots-per-page-side-size+ do
			(loop for j from 0 below  +slots-per-page-side-size+ do
			     (let ((button (make-inventory-slot-button (* i button-size)
								       (* j button-size))))
			       (push button page))))
		   page)))
      (setf current-slot-page (elt slots-pages 0))
      (let ((group-slots (make-check-group (alexandria:flatten slots-pages))))
	(loop for i in (alexandria:flatten slots-pages) do
	     (setf (group i) group-slots)))
      (loop for slot in current-slot-page do
	   (add-child object slot))
      (let ((group-chest (make-check-group* chest-slot-0 chest-slot-1 chest-slot-2)))
	(setf (group chest-slot-0) group-chest
	      (group chest-slot-1) group-chest
	      (group chest-slot-2) group-chest))
      (add-child object chest-slot-0)
      (add-child object chest-slot-1)
      (add-child object chest-slot-2)
      (hide chest-slot-0)
      (hide chest-slot-1)
      (hide chest-slot-2)
      (add-child object img-chest)
      (add-child object b-close))))

(defun make-inventory-window (character)
  (make-instance 'inventory-window
		 :owner character
		 :x 0.0
		 :y 200.0
		 :width  (d/ (d *window-w*) 2.0)
		 :height (d/ (d *window-h*) 2.0)
		 :label  (_ "Inventory")))
