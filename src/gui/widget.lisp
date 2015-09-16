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

(alexandria:define-constant +texture-unit-overlay+ 1     :test #'=)

(alexandria:define-constant +portrait-size+ 64.0         :test #'=)

(alexandria:define-constant +slots-per-page-side-size+ 4 :test #'=)

(defparameter *square-button-size* (d/ (d *window-w*) 10.0))

(defparameter *small-square-button-size* (d/ (d *window-w*) 20.0))

(alexandria:define-constant +file-visible-slot+ 5 :test #'=)

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

(defclass font-mesh-shell (triangle-mesh-shell)
  ((font-color
    :initform §cffffffff
    :initarg  :font-color
    :accessor font-color)))

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
		   (font-color font-color)
		   (material-params material-params)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (declare (vec4 font-color))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
	(with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	  (use-program compiled-shaders :gui-fonts)
	  (gl:active-texture :texture0)
	  (texture:bind-texture texture-object)
	  (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	  (uniformfv compiled-shaders :mult-color font-color)
	  (uniform-matrix compiled-shaders :modelview-matrix 4
				   (vector (sb-cga:matrix* camera-vw-matrix
							   (elt view-matrix 0)
							   (elt model-matrix 0)))
				   nil)
	  (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	  (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	  (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))

(defun fill-font-mesh-shell (mesh &key (color §cffffffff))
  (let ((shell (fill-shell-from-mesh mesh 'font-mesh-shell)))
    (setf (font-color shell) color)
    shell))

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
    :initform 14.0
    :initarg  :checkbutton-h
    :accessor checkbutton-h)
   (input-text-w
    :initform (d/ (d *window-w*) 8.0)
    :initarg  :input-text-w
    :accessor input-text-w)
   (input-text-h
    :initform (d/ (d *window-h*) 54.857143)
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
   (label-font-color
    :initform §cffffffff
    :initarg  :label-font-color
    :accessor label-font-color)
   (shown
    :initform t
    :initarg  :shown
    :accessor shown)
   (last-window-added-to
    :initform nil
    :initarg  :last-window-added-to
    :accessor last-window-added-to)
   (focus
    :initform nil
    :initarg  :focus
    :accessor focus)))

(defmethod initialize-instance :after ((object widget) &key (x 0.0) (y 0.0) &allow-other-keys)
  (setf (pos object) (sb-cga:vec x y 0.0)))

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

(defgeneric flip-y (object child))

(defgeneric width (object))

(defgeneric height (object))

(defgeneric x (object))

(defgeneric y (object))

(defgeneric (setf x) (new-value object))

(defgeneric (setf y) (new-value object))

(defgeneric setup-label (object new-label))

(defgeneric hide (object))

(defgeneric show (object))

(defgeneric mouse-over (object x y))

(defgeneric label-width (object))

(defgeneric on-mouse-pressed (object event))

(defgeneric on-mouse-released (object event))

(defgeneric on-mouse-dragged (object event))

(defgeneric on-key-pressed (object event))

(defmethod x ((object widget))
  (elt (pos object) 0))

(defmethod y ((object widget))
  (elt (pos object) 1))

(defmethod (setf x) (new-value (object widget))
  (setf (elt (pos object) 0) new-value))

(defmethod (setf y) (new-value (object widget))
  (setf (elt (pos object) 1) new-value))

(defmethod flip-y ((object widget) (child widget))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (d- (height object) (d+ (y child) (height child))))

(defmethod width ((object widget))
  (with-slots (width) object
    (d* (elt (scaling object) 0) width)))

(defmethod height ((object widget))
  (with-slots (height) object
    (d* (elt (scaling object) 1) height)))

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

(defmethod on-mouse-pressed ((object widget) event)
  (top-down-visit object #'(lambda (a)
			     (when (widgetp a)
			       (setf (focus a) nil))))
  (do-children-from-end (w object)
       (when (and (widgetp w)
		  (on-mouse-pressed w event))
      	 (return-from on-mouse-pressed t)))
  nil)

(defmethod on-mouse-released ((object widget) event)
  (top-down-visit (find-root-widget object)
		  #'(lambda (a) (when (and (windowp a)
					   (dragging-mode a))
				  (setf (dragging-mode a) nil))))
  (do-children-from-end (w object)
    (when (and (widgetp w)
	       (on-mouse-released w event))
      (return-from on-mouse-released t)))
  nil)

(defmethod on-mouse-dragged ((object widget) event)
  (do-children-from-end (w object)
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
		   (label-font-color label-font-color)
		   (children children)) widget
    (declare (desired-type label-font-size))
    (with-slots (label) widget
      (declare (simple-string label))
      (remove-all-children widget)
      (setf label new-label)
      (loop for c across label do
	   (let* ((mesh  (get-char-mesh label-font c))
		  (shell (if mesh
			     (fill-font-mesh-shell mesh :color label-font-color)
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
       (setf (current-texture object) (texture-object object))
       nil)
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
  (mouse-over object (x-event event) (y-event event)))

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

(defmethod on-mouse-released ((object signalling-light) event)
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

(defun split-text-lines (text width justify)
  (reverse (or
	    (alexandria:flatten
	     (map 'list #'(lambda (a)
			    (if justify
				(justify-monospaced-text a width)
				a))
		  (cl-ppcre:split +gui-static-text-delim+ text)))
	    '(""))))

(defun remove-nbrk-space (lines)
  (mapcar #'(lambda (a)
	      (cl-ppcre:regex-replace-all +gui-static-text-nbsp+ a " "))
	  lines))

(defmethod (setf label) (new-label (object static-text))
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (declare (simple-string new-label))
  (with-accessors ((label-font-size label-font-size)
		   (label-font label-font)
		   (label-font-color label-font-color)
		   (height height)
		   (width width)
		   (children children)
		   (justified justified)) object
    (declare (desired-type width height label-font-size))
    (remove-all-children object)
    (let* ((char-width (ftruncate (d/ width label-font-size)))
	   (lines      (remove-nbrk-space (split-text-lines new-label char-width justified)))
	   (wanted-height      (d* label-font-size (d (length lines))))
	   (scaling-height     (d/ height wanted-height))
	   (actual-height-font (if (d< wanted-height height)
				   label-font-size
				   (d/ (d* wanted-height scaling-height) (d (length lines)))))
	   (actual-height      (d- (d height)
				   (d* actual-height-font (d (length lines))))))
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
			     (fill-font-mesh-shell mesh :color label-font-color)
			     nil)))
	     (when shell
	       (setf (scaling shell) (sb-cga:vec label-font-size actual-height-font 0.0))
	       (setf (pos     shell) (sb-cga:vec xf
						 (d+ (d* line-count actual-height-font)
						     actual-height)
						 0.0))
	       (add-child object shell))))))))

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

(defmethod setup-label ((object h-bar) new-label)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font label-font)
		   (label-font-size label-font-size)
		   (label-font-color label-font-color)
		   (children children)
		   (label-shells label-shells)) object
    (declare (desired-type label-font-size))
    (with-slots (label) object
      (declare (simple-string label))
      (setf label new-label)
      (loop for c across label do
	   (let* ((mesh  (get-char-mesh label-font c))
		  (shell (if mesh
			     (fill-font-mesh-shell mesh :color label-font-color)
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

(defgeneric (setf fill-level) (value object))

(defmethod (setf fill-level) (value (object h-bar))
  (with-accessors ((actual-bar actual-bar)) object
    (with-slots (fill-level) object
      (setf fill-level (alexandria:clamp value 0.0 1.0))
      (setf (scaling actual-bar) (sb-cga:vec fill-level 1.0 1.0)))))

(defclass window (widget)
  ((top-bar
    :initform (make-instance 'widget
			     :height (top-bar-h *reference-sizes*))
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

(defun windowp (w)
  (typep w 'window))

(defmethod initialize-instance :after ((object window) &key &allow-other-keys)
  (with-slots (label) object
    (with-accessors ((width width) (height height)
		     (top-bar top-bar)
		     (frame frame)) object
      (let ((top-bar-h (height top-bar))
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
  (with-accessors ((width width)) object
    (with-accessors ((x-child x) (y-child y)
		     (last-window-added-to last-window-added-to)) child
      (when (not (and last-window-added-to
		      (eq  object last-window-added-to)))
	(setf last-window-added-to object)
	(when (> child-pos 2) ;; skip the titlebar , the frame and the close-button
	  (setf x-child (d+ x-child
			    (d* (left-frame-offset *reference-sizes*)
				(width object))))
	  (when *child-flip-y*
	    (setf y-child (flip-y object child))))))))

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
  (with-accessors ((parent-h height) (frame frame)) object
    (with-accessors ((child-y y) (child-h height)) child
      (let* ((frame-h (d- (height frame)
			  (d* (top-frame-offset *reference-sizes*) (height (frame object)))))
	     (scale   (d/ (d- frame-h child-y)
			  frame-h))
	     (child-new-y  (d- (d* scale frame-h)
			       child-h)))
	(if (< child-y frame-h)
	    child-new-y
	    (d+ (d- (d- parent-h child-y) child-h)
		(d* (bottom-frame-offset *reference-sizes*) (height (frame object)))))))))

(defmethod on-mouse-pressed ((object window) event)
  (with-accessors ((dragging-mode dragging-mode)) object
    (if (and (shown object)
	     (mouse-over object (x-event event) (y-event event)))
	(progn
	  (loop for w across (children object) do
	       (when (and (widgetp w)
			  (on-mouse-pressed w event))
		 (return-from on-mouse-pressed t)))
	  nil)
	nil)))

(defmethod on-mouse-released ((object window) event)
  (if (and (shown object)
	   (mouse-over object (x-event event) (y-event event)))
      (progn
	(loop for w across (children object) do
	     (when (and (widgetp w)
			(on-mouse-released w event))
	       (return-from on-mouse-released t)))
	nil)
      nil))

(defun other-window-dragging-mode-p (me)
  (find-child-if (find-root-widget me)
		 #'(lambda (a) (and
				(windowp a)
				(dragging-mode a)
				(not (= (id a) (id me)))))))

(defmethod on-mouse-dragged ((object window) event)
  (with-accessors ((dragging-mode dragging-mode)) object
    (if (and dragging-mode
	     (not (other-window-dragging-mode-p object)))
	(progn
	  (setf (pos object) (sb-cga:vec+ (pos object)
					  (sb-cga:vec (dx-event event)
						      (dy-event event)
						      0.0)))
	  t)
	(progn
	  (loop for w across (children object) do
	       (when (and (widgetp w)
			  (on-mouse-dragged w event))
		 (return-from on-mouse-dragged t)))
	  (if (and
	       (mouse-over (top-bar object) (x-event event) (y-event event))
	       (not (other-window-dragging-mode-p object)))
	      (progn
		(setf dragging-mode t)
		t)
	      nil)))))

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

(defun fchooser-init-dirs-file-slots ()
  (make-fresh-array 0 t 'button nil))

(defun make-file-chooser-square-button (overlay x y callback)
  (make-instance 'naked-button
		 :x x
		 :y y
		 :width  (fchooser-button-size)
		 :height (fchooser-button-size)
		 :texture-object  (get-texture +square-button-texture-name+)
		 :texture-pressed (get-texture +square-button-pressed-texture-name+)
		 :texture-overlay (get-texture overlay)
		 :callback        callback))

(defun file-chooser-w ()
  (d/ (d *window-w*) 2.0))

(defun file-chooser-h ()
  (d+
   (fchooser-input-y)
   (d* 6.0 (fchooser-input-h))))

(defun fchooser-file-button-h ()
  (d* (fchooser-button-size) 1.5))

(defun fchooser-file-button-w ()
  (d/ (file-chooser-w) 2.5))

(defun fchooser-input-y ()
  (d+
   (spacing *reference-sizes*)
   (fchooser-cumulative-file-buttons-h)
   (fchooser-button-size)))

(defun fchooser-input-w ()
  (d* (file-chooser-w) 0.8))

(defun fchooser-input-h ()
  (d* (input-text-h *reference-sizes*) 1.5))

(defun fchooser-cumulative-file-buttons-h ()
  (d* (d+ 1.0 (d +file-visible-slot+))
      (fchooser-file-button-h)))

(defun fchooser-button-size ()
  24.0)

(defun fchooser-ok/close-y ()
  (d+ (spacing *reference-sizes*)
      (fchooser-input-y)
      (fchooser-input-h)))

(defclass file-chooser (window)
  ((b-updir
    :initform (make-instance 'button
			     :height (fchooser-file-button-h)
			     :width  (fchooser-file-button-w)
			     :x 0.0
			     :y 0.0
			     :callback #'dir-up-cb
			     :label "..")
    :initarg  :b-updir
    :accessor b-updir)
   (b-dir-scroll-up
    :initform (make-file-chooser-square-button +up-overlay-texture-name+
					       0.0
					       (fchooser-cumulative-file-buttons-h)
					       #'dir-scroll-up-cb)
    :initarg  :b-dir-scroll-up
    :accessor b-dir-scroll-up)
   (b-dir-scroll-down
    :initform (make-file-chooser-square-button +down-overlay-texture-name+
					       (fchooser-button-size)
					       (fchooser-cumulative-file-buttons-h)
					       #'dir-scroll-down-cb)
    :initarg  :b-dir-scroll-down
    :accessor b-dir-scroll-down)
   (b-file-scroll-up
    :initform (make-file-chooser-square-button +up-overlay-texture-name+
					       (d/ (file-chooser-w) 2.5)
					       (fchooser-cumulative-file-buttons-h)
					       #'file-scroll-up-cb)
    :initarg  :b-file-scroll-up
    :accessor b-file-scroll-up)
   (b-file-scroll-down
    :initform (make-file-chooser-square-button +down-overlay-texture-name+
					       (d+ (d/ (file-chooser-w) 2.5)
						   (fchooser-button-size))
					       (fchooser-cumulative-file-buttons-h)
					       #'file-scroll-down-cb)
    :initarg  :b-file-scroll-down
    :accessor b-file-scroll-down)
   (b-ok
    :initform (make-instance 'naked-button
			     :x 0.0
			     :y (fchooser-ok/close-y)
			     :width  (fchooser-button-size)
			     :height (fchooser-button-size)
			     :texture-object  (get-texture +square-button-texture-name+)
			     :texture-pressed (get-texture +square-button-pressed-texture-name+)
			     :texture-overlay (get-texture +button-ok-texture-name+)
			     :callback        nil) ;TODO)
    :initarg  :b-ok
    :accessor b-ok)
   (b-close
    :initform (make-instance 'naked-button
			     :x (fchooser-button-size)
			     :y (fchooser-ok/close-y)
			     :width  (fchooser-button-size)
			     :height (fchooser-button-size)
			     :texture-object  (get-texture +square-button-texture-name+)
			     :texture-pressed (get-texture +square-button-pressed-texture-name+)
			     :texture-overlay (get-texture +button-cancel-texture-name+)
			     :callback        #'hide-parent-cb)
    :initarg  :b-close
    :accessor b-close)

   (input-path
    :initform (make-instance 'text-field
			     :width  (fchooser-input-w)
			     :height (fchooser-input-h)
			     :x      0.0
			     :y      (fchooser-input-y)
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
     for y from (fchooser-file-button-h) by (fchooser-file-button-h) do
       (let ((b-dir (make-instance 'button
				   :height (fchooser-file-button-h)
				   :width  (fchooser-file-button-w)
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
     for y from  (fchooser-file-button-h) by (fchooser-file-button-h) do
       (let* ((filename (elt (all-files object) i))
	      (b-file (make-instance 'button
				     :height (fchooser-file-button-h)
				     :width  (fchooser-file-button-w)
				     :x (d/ (file-chooser-w) 2.5)
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
				   (d/ (file-chooser-w) 2.0))
			    :y (d- (d/ (d *window-h*) 2.0)
				   (d/ (file-chooser-h) 2.0))
			    :width  (file-chooser-w)
			    :height (file-chooser-h)
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

(defun next-turn-cb (w e)
  (declare (ignore w e))
  (game-event:propagate-end-turn (make-instance 'game-event:end-turn))
  t)

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
				  #'next-turn-cb)
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
		 :width           (pgen-inc/dec-button-size)
		 :height          (pgen-inc/dec-button-size)
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
		(setf (label widget-capital)               (format nil +standard-float-print-format+
								   new-capital)
		      (label widget-destination)           (format nil +standard-float-print-format+
								   new-current)
		      (slot-value player slot)             new-current
		      (character:exp-points player) new-capital))))))

(defun pgen-window-w ()
  (d* (d *window-w*) 0.75))

(defun pgen-window-h ()
  (d* (d *window-h*) 0.75))

(defun pgen-chk-button-w ()
  (d/ (d *window-w*) 9.0))

(defun pgen-chk-button-y (row)
  (d+ (h2-font-size *reference-sizes*)
      (d* (d row) (checkbutton-h *reference-sizes*))))

(defun pgen-label-ability-w ()
  (d* 4.0 (input-text-w *reference-sizes*)))

(defun pgen-label-ability-h ()
  (input-text-h *reference-sizes*))

(defun pgen-label-ability-x ()
  (d+ (spacing *reference-sizes*)
      (pgen-chk-button-w)))

(defun pgen-label-ability-y (row)
  (d+ (d* 1.8 +portrait-size+)
      (d* (d row)
	  (input-text-h *reference-sizes*))))

(defun pgen-inc-button-x ()
  (d+ (d+ (d* 2.0 (spacing *reference-sizes*))
	  (pgen-chk-button-w))
      (pgen-label-ability-w)))

(defun pgen-inc/dec-button-size ()
  (pgen-label-ability-h))

(defun pgen-dec-button-x ()
  (d+ (pgen-inc-button-x)
      (pgen-inc/dec-button-size)))

(defun pgen-inc-button-y (row)
  (pgen-label-ability-y row))

(defun pgen-characteristics-x ()
  (d+ (d* 2.0 (spacing *reference-sizes*))
      +portrait-size+
      (input-text-w *reference-sizes*)
      (pgen-chk-button-w)))

(defun pgen-characteristics-y (row)
  (d* (d row)
      (d+ (spacing *reference-sizes*)
	  (input-text-h *reference-sizes*))))

(defclass player-generator (window)
  ((player
    :initform  nil
    :initarg  :player
    :accessor player)
   (lb-class
    :initform (make-instance 'simple-label
			     :label     (_ "Class")
			     :font-size (h1-font-size *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x         0.0
			     :y         0.0)
    :initarg  :lb-class
    :accessor lb-class)
   (checkb-warrior
    :initform (make-instance 'labeled-check-button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (pgen-chk-button-y 0.0)
			     :label (_ "Warrior ")
			     :color :green)
    :initarg  :checkb-warrior
    :accessor checkb-warrior)
   (checkb-wizard
    :initform (make-instance 'labeled-check-button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (pgen-chk-button-y 1.0)
			     :label (_ "Wizard ")
			     :color :green)
    :initarg  :checkb-wizard
    :accessor checkb-wizard)
   (checkb-healer
    :initform (make-instance 'labeled-check-button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (pgen-chk-button-y 2.0)
			     :label (_ "Healer ")
			     :color :green)
    :initarg  :checkb-healer
    :accessor checkb-healer)
   (checkb-archer
    :initform (make-instance 'labeled-check-button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (pgen-chk-button-y 3.0)
			     :label (_ "Archer ")
			     :color :green)
    :initarg  :checkb-archer
    :accessor checkb-archer)
   (checkb-ranger
    :initform (make-instance 'labeled-check-button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (pgen-chk-button-y 4.0)
			     :label (_ "Ranger ")
			     :color :green)
    :initarg  :checkb-ranger
    :accessor checkb-ranger)
   (lb-gender
    :initform (make-instance 'simple-label
			     :label     (_ "Gender")
			     :font-size (h2-font-size *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x      0.0
			     :y      (pgen-chk-button-y 5.0))
    :initarg  :lb-gender
    :accessor lb-gender)
   (checkb-male
    :initform (make-instance 'labeled-check-button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (pgen-chk-button-y 6.0)
			     :label (_ "Male ")
			     :color :green)
    :initarg  :checkb-male
    :accessor checkb-male)
   (checkb-female
    :initform (make-instance 'labeled-check-button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (pgen-chk-button-y 7.0)
			     :label (_ "Female ")
			     :color :green)
    :initarg  :checkb-female
    :accessor checkb-female)
   (b-generate
    :initform (make-instance 'button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (pgen-chk-button-y 9.0)
			     :callback #'generate-cb
			     :label (_ "Generate new"))
    :initarg  :b-generate
    :accessor b-generate)
   (b-save
    :initform (make-instance 'button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (d+ (spacing *reference-sizes*)
				    (pgen-chk-button-y 10.0))
			     :callback #'save-cb
			     :label (_ "Save"))
    :initarg  :b-save
    :accessor b-save)
   (b-load
    :initform (make-instance 'button
			     :height (checkbutton-h *reference-sizes*)
			     :width  (pgen-chk-button-w)
			     :x 0.0
			     :y (d+ (d* 2.0 (spacing *reference-sizes*))
				    (pgen-chk-button-y 11.0))
			     :label (_ "Load"))
    :initarg  :b-load
    :accessor b-load)
   (img-portrait
    :initform (make-instance 'signalling-light
			     :width  +portrait-size+
			     :height +portrait-size+
			     :x (pgen-label-ability-x)
			     :y 0.0
			     :texture-name +portrait-unknown-texture-name+
			     :button-status t)
    :initarg :img-portrait
    :accessor img-portrait)
   (lb-damage-pt
    :initform (make-instance 'simple-label-prefixed
			     :width  (pgen-label-ability-w)
			     :height (pgen-label-ability-h)
			     :x (pgen-label-ability-x)
			     :y (pgen-label-ability-y 0.0)
			     :prefix (_ "Damage points: ")
			     :label "")
    :initarg :lb-damage-pt
    :accessor lb-damage-pt)
   (b-inc-damage-pt
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 0.0)
				t)
    :initarg :b-inc-damage-pt
    :accessor b-inc-damage-pt)
   (b-dec-damage-pt
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 0.0)
				nil)
    :initarg :b-dec-damage-pt
    :accessor b-dec-damage-pt)
   (lb-movement-pt
    :initform (make-instance 'simple-label-prefixed
			     :width  (pgen-label-ability-w)
			     :height (pgen-label-ability-h)
			     :x (pgen-label-ability-x)
			     :y (pgen-label-ability-y 1.0)
			     :prefix (_ "Movement points: ")
			     :label "")
    :initarg :lb-movement-pt
    :accessor lb-movement-pt)
   (b-inc-movement-pt
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 1.0)
				t)
    :initarg :b-inc-movement-pt
    :accessor b-inc-movement-pt)
   (b-dec-movement-pt
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 1.0)
				nil)
    :initarg :b-dec-movement-pt
    :accessor b-dec-movement-pt)
   (lb-magic-pt
    :initform (make-instance 'simple-label-prefixed
			     :width  (pgen-label-ability-w)
			     :height (pgen-label-ability-h)
			     :x (pgen-label-ability-x)
			     :y (pgen-label-ability-y 2.0)
			     :prefix (_ "Magic points: ")
			     :label "")
    :initarg :lb-magic-pt
    :accessor lb-magic-pt)
   (b-inc-magic-pt
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 2.0)
				t)
    :initarg :b-inc-magic-pt
    :accessor b-inc-magic-pt)
   (b-dec-magic-pt
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 2.0)
				nil)
    :initarg :b-dec-magic-pt
    :accessor b-dec-magic-pt)
   (lb-dodge-ch
    :initform (make-instance 'simple-label-prefixed
			     :width  (pgen-label-ability-w)
			     :height (pgen-label-ability-h)
			     :x (pgen-label-ability-x)
			     :y (pgen-label-ability-y 3.0)
			     :prefix (_ "Dodge chance: ")
			     :label "")
    :initarg :lb-dodge-ch
    :accessor lb-dodge-ch)
   (b-inc-dodge-ch
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 3.0)
				t)
    :initarg :b-inc-dodge-ch
    :accessor b-inc-dodge-ch)
   (b-dec-dodge-ch
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 3.0)
				nil)
    :initarg :b-dec-dodge-ch
    :accessor b-dec-dodge-ch)
   (lb-melee-atk-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 4.0)
			      :prefix (_ "Short range attack chance: ")
			      :label "")
     :initarg  :lb-melee-atk-ch
     :accessor lb-melee-atk-ch)
   (b-inc-melee-atk-ch
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 4.0)
				t)
    :initarg :b-inc-melee-atk-ch
    :accessor b-inc-melee-atk-ch)
   (b-dec-melee-atk-ch
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 4.0)
				nil)
    :initarg :b-dec-melee-atk-ch
    :accessor b-dec-melee-atk-ch)
   (lb-range-atk-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 5.0)
			      :prefix (_ "Long range attack chance: ")
			      :label "")
     :initarg  :lb-range-atk-ch
     :accessor lb-range-atk-ch)
   (b-inc-range-atk-ch
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 5.0)
				t)
    :initarg :b-inc-range-atk-ch
    :accessor b-inc-range-atk-ch)
   (b-dec-range-atk-ch
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 5.0)
				nil)
    :initarg :b-dec-range-atk-ch
    :accessor b-dec-range-atk-ch)
   (lb-melee-atk-dmg
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 6.0)
			      :prefix (_ "Short Range attack damage: ")
			      :label "")
     :initarg  :lb-melee-atk-dmg
     :accessor lb-melee-atk-dmg)
   (b-inc-melee-atk-dmg
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 6.0)
				t)
    :initarg :b-inc-melee-atk-dmg
    :accessor b-inc-melee-atk-dmg)
   (b-dec-melee-atk-dmg
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 6.0)
				nil)
    :initarg :b-dec-melee-atk-dmg
    :accessor b-dec-melee-atk-dmg)
   (lb-range-atk-dmg
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 7.0)
			      :prefix (_ "Long Range attack damage: ")
			      :label "")
     :initarg  :lb-range-atk-dmg
     :accessor lb-range-atk-dmg)
   (b-inc-range-atk-dmg
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 7.0)
				t)
    :initarg :b-inc-range-atk-dmg
    :accessor b-inc-range-atk-dmg)
   (b-dec-range-atk-dmg
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 7.0)
				nil)
    :initarg :b-dec-range-atk-dmg
    :accessor b-dec-range-atk-dmg)
   (lb-edge-wpn-ch-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 8.0)
			      :prefix (_ "Edge weapon chance bonus: ")
			      :label "")
     :initarg  :lb-edge-wpn-ch-bonus
     :accessor lb-edge-wpn-ch-bonus)
   (b-inc-edge-wpn-ch-bonus
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 8.0)
				t)
    :initarg :b-inc-edge-wpn-ch-bonus
    :accessor b-inc-edge-wpn-ch-bonus)
   (b-dec-edge-wpn-ch-bonus
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 8.0)
				nil)
    :initarg :b-dec-edge-wpn-ch-bonus
    :accessor b-dec-edge-wpn-ch-bonus)
   (lb-edge-wpn-dmg-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 9.0)
			      :prefix (_ "Edge weapon damage bonus: ")
			      :label "")
     :initarg  :lb-edge-wpn-dmg-bonus
     :accessor lb-edge-wpn-dmg-bonus)
   (b-inc-edge-wpn-dmg-bonus
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 9.0)
				t)
    :initarg :b-inc-edge-wpn-dmg-bonus
    :accessor b-inc-edge-wpn-dmg-bonus)
   (b-dec-edge-wpn-dmg-bonus
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 9.0)
				nil)
    :initarg :b-dec-edge-wpn-dmg-bonus
    :accessor b-dec-edge-wpn-dmg-bonus)
   (lb-impact-wpn-ch-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 10.0)
			      :prefix (_ "Impact weapon chance bonus: ")
			      :label "")
     :initarg  :lb-impact-wpn-ch-bonus
     :accessor lb-impact-wpn-ch-bonus)
   (b-inc-impact-wpn-ch-bonus
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 10.0)
				t)
    :initarg :b-inc-impact-wpn-ch-bonus
    :accessor b-inc-impact-wpn-ch-bonus)
   (b-dec-impact-wpn-ch-bonus
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 10.0)
				nil)
    :initarg :b-dec-impact-wpn-ch-bonus
    :accessor b-dec-impact-wpn-ch-bonus)
   (lb-impact-wpn-dmg-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 11.0)
			      :prefix (_ "Impact weapon damage bonus: ")
			      :label "")
     :initarg  :lb-impact-wpn-dmg-bonus
     :accessor lb-impact-wpn-dmg-bonus)
   (b-inc-impact-wpn-dmg-bonus
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 11.0)
				t)
    :initarg :b-inc-impact-wpn-dmg-bonus
    :accessor b-inc-impact-wpn-dmg-bonus)
   (b-dec-impact-wpn-dmg-bonus
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 11.0)
				nil)
    :initarg :b-dec-impact-wpn-dmg-bonus
    :accessor b-dec-impact-wpn-dmg-bonus)
   (lb-pole-wpn-ch-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 12.0)
			      :prefix (_ "Pole weapon chance bonus: ")
			      :label "")
     :initarg  :lb-pole-wpn-ch-bonus
     :accessor lb-pole-wpn-ch-bonus)
   (b-inc-pole-wpn-ch-bonus
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 12.0)
				t)
    :initarg :b-inc-pole-wpn-ch-bonus
    :accessor b-inc-pole-wpn-ch-bonus)
   (b-dec-pole-wpn-ch-bonus
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 12.0)
				nil)
    :initarg :b-dec-pole-wpn-ch-bonus
    :accessor b-dec-pole-wpn-ch-bonus)
   (lb-pole-wpn-dmg-bonus
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 13.0)
			      :prefix (_ "Pole weapon damage bonus: ")
			      :label "")
     :initarg  :lb-pole-wpn-dmg-bonus
     :accessor lb-pole-wpn-dmg-bonus)
   (b-inc-pole-wpn-dmg-bonus
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 13.0)
				t)
    :initarg :b-inc-pole-wpn-dmg-bonus
    :accessor b-inc-pole-wpn-dmg-bonus)
   (b-dec-pole-wpn-dmg-bonus
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 13.0)
				nil)
    :initarg :b-dec-pole-wpn-dmg-bonus
    :accessor b-dec-pole-wpn-dmg-bonus)
   (lb-unlock-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 14.0)
			      :prefix (_ "Unlock chance: ")
			      :label "")
     :initarg  :lb-unlock-ch
     :accessor lb-unlock-ch)
   (b-inc-unlock-ch
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 14.0)
				t)
    :initarg :b-inc-unlock-ch
    :accessor b-inc-unlock-ch)
   (b-dec-unlock-ch
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 14.0)
				nil)
    :initarg :b-dec-unlock-ch
    :accessor b-dec-unlock-ch)
   (lb-deactivate-trap-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 15.0)
			      :prefix (_ "Deactivate trap chance: ")
			      :label "")
     :initarg  :lb-deactivate-trap-ch
     :accessor lb-deactivate-trap-ch)
   (b-inc-deactivate-trap-ch
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 15.0)
				t)
    :initarg :b-inc-deactivate-trap-ch
    :accessor b-inc-deactivate-trap-ch)
   (b-dec-deactivate-trap-ch
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 15.0)
				nil)
    :initarg :b-dec-deactivate-trap-ch
    :accessor b-dec-deactivate-trap-ch)
   (lb-reply-attack-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 16.0)
			      :prefix (_ "Reply to attack chance: ")
			      :label "")
     :initarg  :lb-reply-attack-ch
     :accessor lb-reply-attack-ch)
   (b-inc-reply-attack-ch
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 16.0)
				t)
    :initarg :b-inc-reply-attack-ch
    :accessor b-inc-reply-attack-ch)
   (b-dec-reply-attack-ch
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 16.0)
				nil)
    :initarg :b-dec-reply-attack-ch
    :accessor b-dec-reply-attack-ch)
   (lb-ambush-attack-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 17.0)
			      :prefix (_ "Ambush attack chance: ")
			      :label "")
     :initarg  :lb-ambush-attack-ch
     :accessor lb-ambush-attack-ch)
   (b-inc-ambush-attack-ch
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 17.0)
				t)
    :initarg :b-inc-ambush-attack-ch
    :accessor b-inc-ambush-attack-ch)
   (b-dec-ambush-attack-ch
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 17.0)
				nil)
    :initarg :b-dec-ambush-attack-ch
    :accessor b-dec-ambush-attack-ch)
   (lb-spell-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 18.0)
			      :prefix (_ "Spell chance: ")
			      :label "")
     :initarg  :lb-spell-ch
     :accessor lb-spell-ch)
   (b-inc-spell-ch
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 18.0)
				t)
    :initarg :b-inc-spell-ch
    :accessor b-inc-spell-ch)
   (b-dec-spell-ch
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 18.0)
				nil)
    :initarg :b-dec-spell-ch
    :accessor b-dec-spell-ch)
   (lb-attack-spell-ch
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 19.0)
			      :prefix (_ "Attack spell chance: ")
			      :label "")
     :initarg  :lb-attack-spell-ch
     :accessor lb-attack-spell-ch)
   (b-inc-attack-spell-ch
    :initform (make-pgen-button (pgen-inc-button-x)
				(pgen-inc-button-y 19.0)
				t)
    :initarg :b-inc-attack-spell-ch
    :accessor b-inc-attack-spell-ch)
   (b-dec-attack-spell-ch
    :initform (make-pgen-button (pgen-dec-button-x)
				(pgen-inc-button-y 19.0)
				nil)
    :initarg :b-dec-attack-spell-ch
    :accessor b-dec-attack-spell-ch)
   (lb-level
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (pgen-label-ability-h)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 20.0)
			      :prefix (_ "Level: ")
			      :label "")
     :initarg  :lb-level
     :accessor lb-level)
   (lb-exp-points
     :initform (make-instance 'simple-label-prefixed
			      :width  (pgen-label-ability-w)
			      :height (input-text-h *reference-sizes*)
			      :x (pgen-label-ability-x)
			      :y (pgen-label-ability-y 21.0)
			      :prefix (_ "Experience points: ")
			      :label "")
     :initarg  :lb-exp-points
     :accessor lb-exp-points)
   (input-name
    :initform (make-instance 'text-field
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (d+ (d* 2.0 (spacing *reference-sizes*))
				    +portrait-size+
				    (pgen-chk-button-w))
			     :y 0.0
			     :label (_ "Name"))
    :initarg :input-name
    :accessor input-name)
   (input-last-name
    :initform (make-instance 'text-field
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (d+ (d* 2.0 (spacing *reference-sizes*))
				    +portrait-size+
				    (pgen-chk-button-w))
			     :y (d+ (spacing *reference-sizes*)
				    (input-text-h *reference-sizes*))
			     :label (_ "Last name"))
    :initarg :input-last-name
    :accessor input-last-name)
   (lb-strength
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (pgen-characteristics-x)
			     :y (pgen-characteristics-y 0.0)
			     :prefix (_ "STR: ")
			     :label "")
    :initarg  :lb-strength
    :accessor lb-strength)
   (lb-stamina
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (pgen-characteristics-x)
			     :y (pgen-characteristics-y 1.0)
			     :prefix (_ "ST:  ")
			     :label "")
    :initarg  :lb-stamina
    :accessor lb-stamina)
   (lb-dexterity
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (pgen-characteristics-x)
			     :y (pgen-characteristics-y 2.0)
			     :prefix (_ "DX:  ")
			     :label "")
    :initarg  :lb-dexterity
    :accessor lb-dexterity)
   (lb-agility
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (pgen-characteristics-x)
			     :y (pgen-characteristics-y 3.0)
			     :prefix (_ "AG:  ")
			     :label "")
    :initarg  :lb-agility
    :accessor lb-agility)
   (lb-smartness
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (pgen-characteristics-x)
			     :y (pgen-characteristics-y 4.0)
			     :prefix (_ "SM:  ")
			     :label "")
    :initarg  :lb-smartness
    :accessor lb-smartness)
   (lb-empaty
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (pgen-characteristics-x)
			     :y (pgen-characteristics-y 5.0)
			     :prefix (_ "EM:  ")
			     :label "")
    :initarg  :lb-empaty
    :accessor lb-empaty)
   (lb-weight
    :initform (make-instance 'simple-label-prefixed
			     :width  (input-text-w *reference-sizes*)
			     :height (input-text-h *reference-sizes*)
			     :x (pgen-characteristics-x)
			     :y (pgen-characteristics-y 6.0)
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
	       (character:make-warrior :human))
	      ((button-state (checkb-wizard window))
	       (character:make-wizard :human))
	      ((button-state (checkb-healer window))
	       (character:make-healer :human))
	      ((button-state (checkb-archer window))
	       (character:make-archer :human))
	      ((button-state (checkb-ranger window))
	       (character:make-ranger :human))
	      (t
	       (character:make-warrior :human))))))

(defun setup-portrait (window &key (from-player nil))
  (let ((new-portrait (if from-player
			  (character:portrait from-player)
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
      (let ((new-player (deserialize (make-instance 'character:player-character)
				     (fetch-file-chooser-path fchooser-window))))
	(%setup-character win :new-player new-player))))
  t)

(defun save-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((input-name input-name) (input-last-name input-last-name)
		     (portrait portrait) (player player)) win
      (with-file-chooser (button fchooser-window)
	(setf (character:first-name player) (label input-name)
	      (character:last-name  player) (label input-last-name))
	(setf (character:portrait   player)
	      (clone (get-texture +portrait-unknown-texture-name+)))
	(with-open-file (stream (fetch-file-chooser-path fchooser-window)
				:direction :output :if-exists :supersede
				:if-does-not-exist :create)
	  (format stream "~a" (serialize player))))))
  t)

(defun %find-max-lenght-ability-prefix (win)
  (with-accessors ((lb-damage-pt lb-damage-pt)
		   (lb-movement-pt lb-movement-pt)
		   (lb-magic-pt lb-magic-pt)
		   (lb-dodge-ch lb-dodge-ch)
		   (lb-melee-atk-ch lb-melee-atk-ch)
		   (lb-range-atk-ch lb-range-atk-ch)
		   (lb-melee-atk-dmg lb-melee-atk-dmg)
		   (lb-range-atk-dmg lb-range-atk-dmg)
		   (lb-edge-wpn-ch-bonus lb-edge-wpn-ch-bonus)
		   (lb-edge-wpn-dmg-bonus lb-edge-wpn-dmg-bonus)
		   (lb-impact-wpn-ch-bonus lb-impact-wpn-ch-bonus)
		   (lb-impact-wpn-dmg-bonus lb-impact-wpn-dmg-bonus)
		   (lb-pole-wpn-ch-bonus lb-pole-wpn-ch-bonus)
		   (lb-pole-wpn-dmg-bonus lb-pole-wpn-dmg-bonus)
		   (lb-unlock-ch lb-unlock-ch)
		   (lb-deactivate-trap-ch lb-deactivate-trap-ch)
		   (lb-reply-attack-ch lb-reply-attack-ch)
		   (lb-ambush-attack-ch lb-ambush-attack-ch)
		   (lb-spell-ch lb-spell-ch )
		   (lb-attack-spell-ch lb-attack-spell-ch)
		   (lb-level lb-level)
		   (lb-exp-points lb-exp-points)) win
    (let ((all-labels (list lb-damage-pt
			    lb-movement-pt
			    lb-magic-pt
			    lb-dodge-ch
			    lb-melee-atk-ch
			    lb-range-atk-ch
			    lb-melee-atk-dmg
			    lb-range-atk-dmg
			    lb-edge-wpn-ch-bonus
			    lb-edge-wpn-dmg-bonus
			    lb-impact-wpn-ch-bonus
			    lb-impact-wpn-dmg-bonus
			    lb-pole-wpn-ch-bonus
			    lb-pole-wpn-dmg-bonus
			    lb-unlock-ch
			    lb-deactivate-trap-ch
			    lb-reply-attack-ch
			    lb-ambush-attack-ch
			    lb-spell-ch lb-spell-ch
			    lb-attack-spell-ch
			    lb-level lb-level
			    lb-exp-points lb-exp-points)))
      (find-max (mapcar #'(lambda (a) (length (prefix a))) all-labels)))))

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
				 (character:first-name new-player)
				 (random-names:generate)))
    (when (not new-player)
      (random-names:load-db +random-last-names-filename+))
    (setf (label input-last-name) (if new-player
				      (character:last-name new-player)
				      (random-names:generate)))
    (when (not new-player)
      (setf (character:exp-points player) character:+starting-exp-points+))
    (setup-portrait win :from-player new-player)
    (setf (label lb-strength)  (format nil +standard-float-print-format+ (character:strength player)))
    (setf (label lb-stamina)   (format nil +standard-float-print-format+ (character:stamina player)))
    (setf (label lb-dexterity) (format nil +standard-float-print-format+ (character:dexterity player)))
    (setf (label lb-agility)   (format nil +standard-float-print-format+ (character:agility player)))
    (setf (label lb-smartness) (format nil +standard-float-print-format+ (character:smartness player)))
    (setf (label lb-empaty)    (format nil +standard-float-print-format+ (character:empaty player)))
    (setf (label lb-weight)    (format nil +standard-float-print-format+ (character:weight player)))
    (let ((max-length-prefix (%find-max-lenght-ability-prefix win)))
      (setf (prefix lb-damage-pt) (right-padding (prefix lb-damage-pt) max-length-prefix)
	    (label lb-damage-pt) (format nil +standard-float-print-format+ (character:damage-points player)))
      (%add-callback-to-pgen-buttons b-inc-damage-pt b-dec-damage-pt
				     player 'character:damage-points
				     lb-exp-points lb-damage-pt 0.1 1.0)
      (setf (prefix lb-movement-pt) (right-padding (prefix lb-movement-pt) max-length-prefix)
	    (label  lb-movement-pt) (format nil +standard-float-print-format+ (character:movement-points player)))
      (%add-callback-to-pgen-buttons b-inc-movement-pt b-dec-movement-pt
				     player 'character:movement-points
				     lb-exp-points lb-movement-pt 0.5 1.0)
      (setf (prefix lb-magic-pt) (right-padding (prefix lb-magic-pt) max-length-prefix)
	    (label  lb-magic-pt) (format nil +standard-float-print-format+ (character:magic-points player)))
      (%add-callback-to-pgen-buttons b-inc-magic-pt b-dec-magic-pt
				     player 'character:magic-points
				     lb-exp-points lb-magic-pt 0.5 1.0)
      (setf (prefix lb-dodge-ch) (right-padding (prefix lb-dodge-ch) max-length-prefix)
	    (label  lb-dodge-ch) (format nil +standard-float-print-format+ (character:dodge-chance player)))
      (%add-callback-to-pgen-buttons b-inc-dodge-ch b-dec-dodge-ch
				     player 'character:dodge-chance
				     lb-exp-points lb-dodge-ch 0.5 1.0)
      (setf (prefix lb-melee-atk-ch) (right-padding (prefix lb-melee-atk-ch) max-length-prefix)
	    (label  lb-melee-atk-ch) (format nil +standard-float-print-format+ (character:melee-attack-chance player)))
      (%add-callback-to-pgen-buttons b-inc-melee-atk-ch b-dec-melee-atk-ch
				     player 'character:melee-attack-chance
				     lb-exp-points lb-melee-atk-ch)
      (setf (prefix lb-range-atk-ch) (right-padding (prefix lb-range-atk-ch) max-length-prefix)
	    (label  lb-range-atk-ch) (format nil +standard-float-print-format+ (character:range-attack-chance player)))
      (%add-callback-to-pgen-buttons b-inc-range-atk-ch b-dec-range-atk-ch
				     player 'character:range-attack-chance
				     lb-exp-points lb-range-atk-ch 0.5 1.0)
      (setf (prefix lb-melee-atk-dmg) (right-padding (prefix lb-melee-atk-ch) max-length-prefix)
	    (label  lb-melee-atk-dmg) (format nil +standard-float-print-format+
					      (character:melee-attack-damage player)))
      (%add-callback-to-pgen-buttons b-inc-melee-atk-dmg b-dec-melee-atk-dmg
				     player 'character:melee-attack-damage
				     lb-exp-points lb-melee-atk-dmg 0.25 1.0)
      (setf (prefix lb-range-atk-dmg) (right-padding (prefix lb-range-atk-dmg) max-length-prefix)
	    (label  lb-range-atk-dmg) (format nil +standard-float-print-format+
					      (character:range-attack-damage player)))
      (%add-callback-to-pgen-buttons b-inc-range-atk-dmg b-dec-range-atk-dmg
				     player 'character:range-attack-damage
				   lb-exp-points lb-range-atk-dmg 0.25 1.0)
      (setf (prefix lb-edge-wpn-ch-bonus) (right-padding (prefix lb-edge-wpn-ch-bonus)
							 max-length-prefix)
	    (label lb-edge-wpn-ch-bonus) (format nil +standard-float-print-format+
						 (character:edge-weapons-chance-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-edge-wpn-ch-bonus b-dec-edge-wpn-ch-bonus
				     player 'character:edge-weapons-chance-bonus
				     lb-exp-points lb-edge-wpn-ch-bonus 0.25 1.0)
      (setf (prefix lb-edge-wpn-dmg-bonus) (right-padding (prefix lb-edge-wpn-dmg-bonus)
							  max-length-prefix)
	    (label lb-edge-wpn-dmg-bonus) (format nil +standard-float-print-format+
						  (character:edge-weapons-damage-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-edge-wpn-dmg-bonus b-dec-edge-wpn-dmg-bonus
				     player 'character:edge-weapons-damage-bonus
				     lb-exp-points lb-edge-wpn-dmg-bonus 0.25 1.0)
      (setf (prefix lb-impact-wpn-ch-bonus) (right-padding (prefix lb-impact-wpn-ch-bonus)
							   max-length-prefix)
	    (label lb-impact-wpn-ch-bonus) (format nil +standard-float-print-format+
						   (character:impact-weapons-chance-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-impact-wpn-ch-bonus b-dec-impact-wpn-ch-bonus
				     player 'character:impact-weapons-chance-bonus
				     lb-exp-points lb-impact-wpn-ch-bonus 0.25 1.0)
      (setf (prefix lb-impact-wpn-dmg-bonus) (right-padding (prefix lb-impact-wpn-dmg-bonus)
							    max-length-prefix)
	    (label lb-impact-wpn-dmg-bonus) (format nil +standard-float-print-format+
						    (character:impact-weapons-damage-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-impact-wpn-dmg-bonus b-dec-impact-wpn-dmg-bonus
				     player 'character:impact-weapons-damage-bonus
				     lb-exp-points lb-impact-wpn-dmg-bonus 0.25 1.0)
      (setf (prefix lb-pole-wpn-ch-bonus) (right-padding (prefix lb-pole-wpn-ch-bonus)
							 max-length-prefix)
	    (label lb-pole-wpn-ch-bonus) (format nil +standard-float-print-format+
						    (character:pole-weapons-chance-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-pole-wpn-ch-bonus b-dec-pole-wpn-ch-bonus
				     player 'character:pole-weapons-chance-bonus
				     lb-exp-points lb-pole-wpn-ch-bonus 0.25 1.0)
      (setf (prefix lb-pole-wpn-dmg-bonus) (right-padding (prefix lb-pole-wpn-dmg-bonus)
							  max-length-prefix)
	    (label lb-pole-wpn-dmg-bonus) (format nil +standard-float-print-format+
						  (character:pole-weapons-damage-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-pole-wpn-dmg-bonus b-dec-pole-wpn-dmg-bonus
				     player 'character:pole-weapons-damage-bonus
				     lb-exp-points lb-pole-wpn-dmg-bonus 0.25 1.0)
      (setf (prefix lb-unlock-ch) (right-padding (prefix lb-unlock-ch) max-length-prefix)
	    (label lb-unlock-ch) (format nil +standard-float-print-format+
					 (character:unlock-chance player)))
      (%add-callback-to-pgen-buttons b-inc-unlock-ch b-dec-unlock-ch
				     player 'character:unlock-chance
				     lb-exp-points lb-unlock-ch 0.5 1.0)
      (setf (prefix lb-deactivate-trap-ch) (right-padding (prefix lb-deactivate-trap-ch)
							  max-length-prefix)
	    (label lb-deactivate-trap-ch) (format nil +standard-float-print-format+
						  (character:deactivate-trap-chance player)))
      (%add-callback-to-pgen-buttons b-inc-deactivate-trap-ch b-dec-deactivate-trap-ch
				     player 'character:deactivate-trap-chance
				     lb-exp-points lb-deactivate-trap-ch 0.5 1.0)
      (setf (prefix lb-reply-attack-ch) (right-padding (prefix lb-reply-attack-ch)
						       max-length-prefix)
	    (label lb-reply-attack-ch) (format nil +standard-float-print-format+
					       (character:reply-attack-chance player)))
      (%add-callback-to-pgen-buttons b-inc-reply-attack-ch b-dec-reply-attack-ch
				     player 'character:reply-attack-chance
				     lb-exp-points lb-reply-attack-ch 0.33 1.0)
      (setf (prefix lb-ambush-attack-ch) (right-padding (prefix lb-ambush-attack-ch)
							max-length-prefix)
	    (label lb-ambush-attack-ch) (format nil +standard-float-print-format+
						(character:ambush-attack-chance player)))
      (%add-callback-to-pgen-buttons b-inc-ambush-attack-ch b-dec-ambush-attack-ch
				     player 'character:ambush-attack-chance
				     lb-exp-points lb-ambush-attack-ch 0.33 1.0)
      (setf (prefix lb-spell-ch) (right-padding (prefix lb-spell-ch) max-length-prefix)
	    (label lb-spell-ch) (format nil +standard-float-print-format+
					(character:spell-chance player)))
      (%add-callback-to-pgen-buttons b-inc-spell-ch b-dec-spell-ch
				     player 'character:spell-chance
				     lb-exp-points lb-spell-ch 0.25 1.0)
      (setf (prefix lb-attack-spell-ch) (right-padding (prefix lb-attack-spell-ch)
						       max-length-prefix)
	    (label lb-attack-spell-ch) (format nil +standard-float-print-format+
					       (character:attack-spell-chance player)))
      (%add-callback-to-pgen-buttons b-inc-attack-spell-ch b-dec-attack-spell-ch
				     player 'character:attack-spell-chance
				     lb-exp-points lb-attack-spell-ch 0.25 1.0)
      (setf (prefix lb-level) (right-padding (prefix lb-level) max-length-prefix)
	    (label lb-level) (format nil "~d" (character:level player)))
      (setf (prefix lb-exp-points) (right-padding (prefix lb-exp-points) max-length-prefix)
	    (label lb-exp-points)  (format nil "~d" (character:exp-points player))))))

(defun make-player-generator ()
  (make-instance 'player-generator
		 :x 0.0
		 :y 200.0
		 :width  (pgen-window-w)
		 :height (pgen-window-h)
		 :label  (_ "Generate character")))

(defclass inventory-slot-button (check-button)
  ((contained-entity
    :initform nil
    :initarg  :contained-entity
    :accessor contained-entity)))

(defun y-just-under-slot-page ()
  (d+ (d* (d +slots-per-page-side-size+)
	  (small-square-button-size *reference-sizes*))
      (small-square-button-size *reference-sizes*)
      (spacing                      *reference-sizes*)))

(defun make-inventory-slot-button (x y &key (callback nil))
  (make-instance 'inventory-slot-button
		 :theme            nil
		 :x                x
		 :y                y
		 :width            (small-square-button-size *reference-sizes*)
		 :height           (small-square-button-size *reference-sizes*)
		 :texture-object   (get-texture +inventory-slot-texture-name+)
		 :texture-pressed  (get-texture +inventory-slot-selected-texture-name+)
		 :texture-overlay  (get-texture +transparent-texture-name+)
		 :contained-entity nil
		 :callback         callback))

(defun show/hide-chest-slots-cb (w e)
  (declare (ignore e))
  (let* ((parent (parent w))
	 (chest-slots (chest-slots parent)))
    (if (button-state w)
	(loop for slot in chest-slots do
	     (show slot))
	(loop for slot in chest-slots do
	     (hide slot)))))

(defun remove-inventory-page (window)
  (let ((current-slots-page (current-slots-page window)))
    (setf (children window)
	  (remove-if #'(lambda (c)
			 (find-if #'(lambda (s) (= (id s) (id c))) current-slots-page))
		     (children window)))))

(defun add-containded-item (slot item)
  (if item
      (progn
	(setf (texture-overlay  slot)
	      (or (and item
		       (character:portrait item))
		  (get-texture +transparent-texture-name+)))
	(setf (contained-entity slot) item))
      (remove-containded-item slot)))

(defun remove-containded-item (slot)
  (setf (texture-overlay  slot) (get-texture +transparent-texture-name+)
	(contained-entity slot) nil))

(defun empty-slot-p (slot)
  (null (contained-entity slot)))

(defun inventory-update-description-cb (widget e)
  (declare (ignore e))
  (with-parent-widget (win) widget
    (when (contained-entity widget)
      (setf (label (text-description win))
	    (description-for-humans (contained-entity widget))))
    t))

(defun pick-item-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (multiple-value-bind (chest-slot item)
	(get-selected-chest-item win)
      (when (and chest-slot item)
	(let ((available-slot (find-if #'(lambda (s) (empty-slot-p s))
				       (alexandria:flatten (slots-pages win)))))
	  (when (and available-slot
		     chest-slot)
	    (add-containded-item     available-slot item)
	    (remove-containded-item  chest-slot)
	    (remove-child (chest win) item :test #'= :key #'id)))))))

(defun find-available-slot (inventory-win)
  (find-if #'(lambda (s) (empty-slot-p s))
	   (alexandria:flatten (slots-pages inventory-win))))

(defun remove-worn-item (slot-from slot-to character character-slot)
  (when (and (empty-slot-p slot-to)
	     (not (empty-slot-p slot-from)))
    (let ((saved (contained-entity slot-from)))
      (setf (slot-value character character-slot) nil)
      (add-containded-item slot-from nil)
      (add-containded-item slot-to   saved))))

(defun remove-worn-item-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (multiple-value-bind (selected-worn-slot worn-item)
	(get-selected-worn-item win)
      (when (and selected-worn-slot
		 worn-item)
	(let ((available-slot (find-available-slot win)))
	  (when available-slot
	    (let ((c-slot (item->player-character-slot win worn-item)))
	      (when c-slot
		(remove-worn-item selected-worn-slot
				  available-slot
				  (owner win)
				  c-slot)))))))))

(defun dismiss-item-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (with-accessors ((chest-slots chest-slots) (owner owner)) win
      (multiple-value-bind (slot item)
	  (get-selected-item win)
	(let ((available-chest-slot (loop
				       named inner
				       for slot in chest-slots do
					 (when (empty-slot-p slot)
					   (return-from inner slot)))))
	  (when (and slot
		     available-chest-slot)
	    (add-containded-item available-chest-slot item)
	    (remove-containded-item  slot)
	    (setf (character:inventory owner)
		  (remove-if #'(lambda (a) (= (id item) a))
			     (character:inventory owner) :key #'id))))))))

(defun next-slot-page-cb (w e)
  (declare (ignore e))
  (let* ((window             (parent w))
	 (slots              (slots-pages window))
	 (pages-count        (length (slots-pages window)))
	 (page-no            (current-slots-page-number window))
	 (next-page-no       (mod (1+ page-no) pages-count)))
    (remove-inventory-page window)
    (setf (current-slots-page window) (elt slots next-page-no))
    (map nil #'(lambda (s) (setf (compiled-shaders s) (compiled-shaders window)))
	 (current-slots-page window))
    (map nil #'(lambda (s) (add-child window s)) (current-slots-page window))
    (update-page-counts window next-page-no)))

(defun prev-slot-page-cb (w e)
  (declare (ignore e))
  (let* ((window       (parent w))
	 (slots        (slots-pages window))
	 (page-no      (current-slots-page-number window))
	 (next-page-no (if (< (1- page-no) 0)
			   (1- (length (slots-pages window)))
			   (1- page-no))))
    (remove-inventory-page window)
    (setf (current-slots-page window) (elt slots next-page-no))
    (map nil #'(lambda (s) (setf (compiled-shaders s) (compiled-shaders window)))
	 (current-slots-page window))
    (map nil #'(lambda (s) (add-child window s)) (current-slots-page window))
    (update-page-counts window next-page-no)))

(defun item->player-character-slot (window item)
  (with-accessors ((left-hand-slot  left-hand-slot)
		   (right-hand-slot right-hand-slot)) window
    (and item
	 (cond
	   ((character:ringp item)
	    'character:ring)
	   ((character:armorp item)
	    'character:armor)
	   ((character:elmp item)
	    'character:elm)
	   ((character:shoesp item)
	    'character:shoes)
	   ((or (character:weaponp item)
		(character:shieldp item))
	    (if (empty-slot-p left-hand-slot)
		'character:left-hand
		'character:right-hand))))))

(defun item->window-accessor (window item)
  (with-accessors ((elm-slot        elm-slot)
		   (shoes-slot      shoes-slot)
		   (armor-slot      armor-slot)
		   (left-hand-slot  left-hand-slot)
		   (right-hand-slot right-hand-slot)
		   (ring-slot       ring-slot))      window
    (and item
	 (cond
	   ((character:ringp item)
	    ring-slot)
	   ((character:armorp item)
	    armor-slot)
	   ((character:elmp item)
	    elm-slot)
	   ((character:shoesp item)
	    shoes-slot)
	   ((or (character:weaponp item)
		(character:shieldp item))
	    (if (empty-slot-p left-hand-slot)
		left-hand-slot
		right-hand-slot))))))

(defun swap-worn-item (slot-from slot-to character character-slot)
  (let ((saved (contained-entity slot-from)))
    (setf (slot-value character character-slot) (contained-entity slot-from))
    (add-containded-item slot-from (contained-entity slot-to))
    (add-containded-item slot-to   saved)))

(defun worn-item (slot-from slot-to character character-slot)
  (when (and (empty-slot-p slot-to)
	     (not (empty-slot-p slot-from)))
    (let ((saved (contained-entity slot-from)))
      (setf (slot-value character character-slot) (contained-entity slot-from))
      (add-containded-item slot-from (contained-entity slot-to))
      (add-containded-item slot-to   saved))))

(defun wear-item-cb (widget e)
  (declare (ignore e))
  (with-parent-widget (win) widget
    (with-accessors ((owner           owner)
		     (elm-slot        elm-slot)
		     (shoes-slot      shoes-slot)
		     (armor-slot      armor-slot)
		     (left-hand-slot  left-hand-slot)
		     (right-hand-slot right-hand-slot)
		     (ring-slot       ring-slot))      win
      (multiple-value-bind (slot item)
	  (get-selected-item win)
	(when item
	  (let ((c-slot       (item->player-character-slot win item))
		(win-accessor (item->window-accessor       win item)))
	    (when (and c-slot
		       win-accessor)
	      (worn-item slot win-accessor owner c-slot))))))))

(defun sort-items (pages)
  (let ((all (mapcar #'contained-entity (alexandria:flatten pages))))
    (remove-if-null
     (nconc
      (remove-if #'(lambda (a) (not (character:weaponp a))) all)
      (remove-if #'(lambda (a) (not (character:shieldp a))) all)
      (remove-if #'(lambda (a) (not (character:elmp a))) all)
      (remove-if #'(lambda (a) (not (character:armorp a))) all)
      (remove-if #'(lambda (a) (not (character:shoesp a))) all)
      (remove-if #'(lambda (a) (not (character:potionp a))) all)
      (remove-if #'(lambda (a) (not (character:ringp a))) all)
      (remove-if #'(lambda (a) (not (character:keyp a))) all)))))

(defun sort-items-enter-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (let ((sorted-items (sort-items (slots-pages win))))
      (map nil
	   #'(lambda (a) (remove-containded-item a))
	   (alexandria:flatten (slots-pages win)))
      (map nil #'(lambda (s i) (add-containded-item s i))
	   (alexandria:flatten (slots-pages win))
	   sorted-items))))

(defclass inventory-window (window)
  ((owner
   :initform nil
   :initarg  :owner
   :accessor owner)
   (slots-pages
   :initform '()
   :initarg  :slots-pages
   :accessor slots-pages)
   (b-next-page
    :initform (make-instance 'naked-button
			     :x               (d* 3.0 (small-square-button-size *reference-sizes*))
			     :y               0.0
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +right-overlay-texture-name+)
			     :callback        #'next-slot-page-cb)
    :initarg  :b-next-page
    :accessor b-next-page)
   (b-prev-page
    :initform (make-instance 'naked-button
			     :x               0.0
			     :y               0.0
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +left-overlay-texture-name+)
			     :callback         #'prev-slot-page-cb)

    :accessor b-prev-page)
   (lb-page-count
    :initform (make-instance 'simple-label-prefixed
			     :width  (d* 1.9 (small-square-button-size *reference-sizes*))
			     :height (input-text-h *reference-sizes*)
			     :x      (small-square-button-size *reference-sizes*)
			     :y      (d- (d/ (small-square-button-size *reference-sizes*) 2.0)
					 (d/ (input-text-h *reference-sizes*)))
			     :prefix (_ "Page ")
			     :label "")
    :initarg  :lb-page-count
    :accessor lb-page-count)
   (chest
    :initform nil
    :initarg  :chest
    :accessor chest)
   (chest-slots
    :initform (loop for x from 1.0 to (d +container-capacity+) by 1.0 collect
		   (make-inventory-slot-button (d* x (small-square-button-size *reference-sizes*))
					       (y-just-under-slot-page)
					       :callback #'inventory-update-description-cb))
    :initarg  :chest-slots
    :accessor chest-slots)
   (current-slots-page
    :initform nil
    :initarg  :current-slots-page
    :accessor current-slots-page)
   (current-slots-page-number
    :initform  0
    :initarg  :current-slots-page-number
    :accessor current-slots-page-number)
   (b-chest
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
    :initarg  :b-chest
    :accessor b-chest)
   (text-description
    :initform (make-instance 'widget:static-text
			     :height (d* 2.0
					 (small-square-button-size *reference-sizes*))
			     :width  (d* 5.0
					 (small-square-button-size *reference-sizes*))
			     :x      0.0
			     :y      (d+ (y-just-under-slot-page)
					 (small-square-button-size *reference-sizes*))
			     :font-size (d* 0.1 *square-button-size*)
			     :label ""
			     :justified t)
    :initarg  :text-description
    :accessor text-description)
   (b-sort
    :initform (make-instance 'button
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y      0.0
			     :callback #'sort-items-enter-cb
			     :label (_ "sort"))
    :initarg  b-sort
    :accessor b-sort)
   (b-use
    :initform (make-instance 'naked-button
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y               (small-square-button-size *reference-sizes*)
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +use-overlay-texture-name+)
			     :callback        nil) ; TODO
    :initarg :b-use
    :accessor b-use)
   (b-wear
    :initform (make-instance 'naked-button
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y               (d* 2.0
						  (small-square-button-size *reference-sizes*))
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +wear-overlay-texture-name+)
			     :callback        #'wear-item-cb)
    :initarg :b-wear
    :accessor b-wear)
   (b-pick
    :initform (make-instance 'naked-button
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y               (d* 3.0
						  (small-square-button-size *reference-sizes*))
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +up-arrow-overlay-texture-name+)
			     :callback        #'pick-item-cb)
    :initarg :b-pick
    :accessor b-pick)
   (b-dismiss
    :initform (make-instance 'naked-button
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y               (d* 4.0
						  (small-square-button-size *reference-sizes*))
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +down-arrow-overlay-texture-name+)
			     :callback        #'dismiss-item-cb)
    :initarg :b-dismiss
    :accessor b-dismiss)
   (img-silhouette
    :initform (make-instance 'signalling-light
			     :x             (d- (d* 0.66 (inventory-window-width))
						(d/ (inventory-silhouette-w) 2.0))
			     :y             (small-square-button-size *reference-sizes*)
			     :width         (inventory-silhouette-w)
			     :height        (inventory-silhouette-h)
			     :texture-name  +silhouette-texture-name+
			     :button-status t)
    :initarg  :img-silhouette
    :accessor img-silhouette)
   (elm-slot
    :initform (make-inventory-slot-button (d- (d* 0.66 (inventory-window-width))
					      (d/ (small-square-button-size *reference-sizes*)
						  2.0))
					  (d/ (small-square-button-size *reference-sizes*)
					      2.0)
					  :callback #'inventory-update-description-cb)
    :initarg  :elm-slot
    :accessor elm-slot)
   (shoes-slot
    :initform (make-inventory-slot-button (d- (d* 0.66 (inventory-window-width))
					      (d/ (small-square-button-size *reference-sizes*)
						  2.0))
					  (d- (inventory-window-height)
					      (small-square-button-size *reference-sizes*))
					  :callback #'inventory-update-description-cb)
    :initarg  :shoes-slot
    :accessor shoes-slot)
   (armor-slot
    :initform (make-inventory-slot-button (d- (d* 0.66 (inventory-window-width))
					      (d/ (small-square-button-size *reference-sizes*)
						  2.0))
					  (d- (d/ (inventory-window-height) 2.0)
					      (d* 1.5
						  (small-square-button-size *reference-sizes*)))
					  :callback #'inventory-update-description-cb)
    :initarg  :armor-slot
    :accessor armor-slot)
    (right-hand-slot
    :initform (make-inventory-slot-button (d- (d* 0.66 (inventory-window-width))
					      (d* 2.0
						  (small-square-button-size *reference-sizes*)))
					  (d/ (inventory-window-height) 2.0)
					  :callback #'inventory-update-description-cb)
    :initarg  :left-hand-slot
    :accessor left-hand-slot)
   (left-hand-slot
    :initform (make-inventory-slot-button (d+ (d* 0.66 (inventory-window-width))
					      (small-square-button-size *reference-sizes*))
					  (d/ (inventory-window-height) 2.0)
					  :callback #'inventory-update-description-cb)
    :initarg  :right-hand-slot
    :accessor right-hand-slot)
   (ring-slot
    :initform (make-inventory-slot-button (d+ (d* 0.66 (inventory-window-width))
					      (small-square-button-size *reference-sizes*))
					  (d- (d/ (inventory-window-height) 2.0)
					      (small-square-button-size *reference-sizes*)
					      (spacing *reference-sizes*))
					  :callback #'inventory-update-description-cb)
    :initarg  :ring-slot
    :accessor ring-slot)
   (b-remove-worn-item
    :initform (make-instance 'naked-button
			     :x               (d- (d* 0.66 (inventory-window-width))
					      (d* 2.0
						  (small-square-button-size *reference-sizes*)))
			     :y               (d- (d/ (inventory-window-height) 2.0)
						  (small-square-button-size *reference-sizes*)
						  (spacing *reference-sizes*))
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +add-to-bag-texture-name+)
			     :callback        #'remove-worn-item-cb)
    :initarg  :b-remove-worn-item
    :accessor b-remove-worn-item)))

(defmethod initialize-instance :after ((object inventory-window) &key &allow-other-keys)
  (with-accessors ((slots-pages slots-pages) (current-slots-page current-slots-page)
		   (owner owner) (b-use b-use) (text-description text-description)
		   (b-sort b-sort)
		   (b-wear b-wear) (b-pick b-pick) (b-dismiss b-dismiss)
		   (b-chest b-chest) (b-next-page b-next-page)
		   (current-slots-page-number current-slots-page-number)
		   (b-prev-page b-prev-page) (lb-page-count lb-page-count)
		   (chest-slots chest-slots)
		   (img-silhouette img-silhouette)
		   (elm-slot elm-slot) (shoes-slot shoes-slot)
		   (armor-slot armor-slot) (left-hand-slot left-hand-slot)
		   (right-hand-slot right-hand-slot) (ring-slot ring-slot)
		   (b-remove-worn-item b-remove-worn-item))             object
    (let ((page-count (if owner
			  (character:inventory-slot-pages-number owner)
			  1))
	  (starting-y (small-square-button-size *reference-sizes*)))
      (setf slots-pages
	    (loop repeat page-count collect
		 (let ((page '())
		       (button-size (small-square-button-size *reference-sizes*)))
		   (loop for i from 0 below +slots-per-page-side-size+ do
			(loop for j from 0 below  +slots-per-page-side-size+ do
			     (let* ((button (make-inventory-slot-button (* i button-size)
									(d+ starting-y
									    (* j button-size))
									:callback #'inventory-update-description-cb)))
			       (push button page))))
		   (reverse page))))
      (setf current-slots-page (elt slots-pages 0))
      (let ((group-slots (make-check-group (alexandria:flatten slots-pages))))
	(loop for i in (alexandria:flatten slots-pages) do
	     (setf (group i) group-slots)))
      (loop for slot in current-slots-page do
	   (add-child object slot))
      (let ((group-chest (make-check-group chest-slots)))
	(map nil
	     #'(lambda (a) (setf (group a) group-chest))
	     chest-slots))
      (add-child object b-prev-page)
      (add-child object b-next-page)
      (add-child object lb-page-count)
      (loop for slot in chest-slots do
	   (add-child object slot)
	   (hide slot))
      (add-child object b-sort)
      (add-child object b-use)
      (add-child object b-wear)
      (add-child object b-pick)
      (add-child object b-dismiss)
      (add-child object b-chest)
      (add-child object text-description)
      (add-child object img-silhouette)
      (let ((group-worn (make-check-group* elm-slot
					   shoes-slot
					   armor-slot
					   left-hand-slot
					   right-hand-slot
					   ring-slot)))
	(setf (group elm-slot) group-worn)
	(setf (group shoes-slot) group-worn)
	(setf (group armor-slot) group-worn)
	(setf (group left-hand-slot) group-worn)
	(setf (group right-hand-slot) group-worn)
	(setf (group ring-slot) group-worn))
      (add-child object elm-slot)
      (add-child object shoes-slot)
      (add-child object armor-slot)
      (add-child object left-hand-slot)
      (add-child object right-hand-slot)
      (add-child object ring-slot)
      (add-child object b-remove-worn-item)
      (update-page-counts object current-slots-page-number)
      (add-inventory-objects object))))

(defgeneric get-selected-chest-item (object))

(defgeneric get-selected-worn-item  (object))

(defgeneric get-selected-item (object))

(defgeneric update-page-counts (object pos))

(defgeneric add-inventory-objects (object))

(defmacro gen-get-selected-item ((name) &rest slots)
  (alexandria:with-gensyms (selected)
    `(defmethod  ,(alexandria:format-symbol t "~@:(get-selected-~a-item~)" name)
	 ((object inventory-window))
       (let ((,selected (or
			 ,@(loop for slot in slots collect
				`(and (,slot object)
				      (button-state (,slot object))
				      (contained-entity (,slot object))
				      (,slot object))))))
	 (and ,selected
	      (values ,selected (contained-entity ,selected)))))))

(defmethod get-selected-chest-item ((object inventory-window))
  (let ((selected (loop
		     named inner
		     for slot in (chest-slots object) do
		       (when (and slot
				  (button-state slot))
			 (return-from inner slot)))))
    (and selected
         (values selected (contained-entity selected)))))

(gen-get-selected-item (worn)
		       elm-slot
		       armor-slot
		       shoes-slot
		       left-hand-slot
		       right-hand-slot
		       ring-slot)

(defmethod get-selected-item ((object inventory-window))
  (with-accessors ((current-slots-page current-slots-page)) object
    (let ((found (find-if #'(lambda (a) (and (button-state     a)
					     (contained-entity a)))
			  current-slots-page)))
      (when found
	(values found (contained-entity found))))))

(defmethod update-page-counts ((object inventory-window) pos)
  (with-accessors ((lb-page-count lb-page-count)
		   (slots-pages slots-pages)
		   (current-slots-page-number current-slots-page-number)) object
    (setf current-slots-page-number pos)
    (setf (label lb-page-count)
	  (format nil (_ "~a of ~a") (1+ current-slots-page-number) (length slots-pages)))))

(defmethod add-inventory-objects ((object inventory-window))
  (with-accessors ((slots-pages slots-pages) (owner owner) (chest chest)
		   (chest-slots chest-slots)) object
    (when owner
      (assert (<= (length (character:inventory owner))
		  (length (alexandria:flatten slots-pages))))
      (loop
	 for slot in (alexandria:flatten slots-pages)
	 for obj  in (character:inventory owner) do
	   (setf (contained-entity slot) obj
		 (texture-overlay  slot) (character:portrait obj))))
    ;; display stuff inside the chest, if any
    (when chest
      (assert (or (null (children chest))
		  (<= (length (children chest)) 3)))
      (loop for i from 0 below (length (children chest)) do
	   (setf (contained-entity (elt chest-slots i)) (elt (children chest) i))
	   (setf (texture-overlay  (elt chest-slots i))
		 (character:portrait (elt (children chest) i)))))))

(defun inventory-window-width ()
  (d+ (d* 13.0 (small-square-button-size *reference-sizes*))
      (d* 2.0
	  (left-frame-offset *reference-sizes*)
	  (d* 5.0 (small-square-button-size *reference-sizes*)))))

(defun inventory-silhouette-w ()
  (d* 0.25 (inventory-window-width)))

(defun inventory-silhouette-h ()
  (d- (inventory-window-height)
      (d* 3.0 (small-square-button-size *reference-sizes*))))

(defun inventory-window-height ()
    (d+ (d* 10.0 (small-square-button-size *reference-sizes*))))

(defun make-inventory-window (character &optional (chest nil))
  (make-instance 'inventory-window
		 :owner  character
		 :chest  chest
		 :x      0.0
		 :y      200.0
		 :width  (inventory-window-width)
		 :height (inventory-window-height)
		 :label  (_ "Inventory")))

(defun message-window-w ()
  (d* 8.0 (small-square-button-size *reference-sizes*)))

(defun message-window-h ()
  (d* 4.0 (small-square-button-size *reference-sizes*)))

(defun message-window-pictogram-w ()
  (d* 2.0 (small-square-button-size *reference-sizes*)))

(defun message-window-pictogram-h ()
  (d* 2.0 (small-square-button-size *reference-sizes*)))

(defun message-window-button-h  ()
  (tiny-square-button-size *reference-sizes*))

(defun message-window-button-w  ()
  (d* 2.0 (tiny-square-button-size *reference-sizes*)))

(defun message-window-text-x ()
  (d+ (message-window-pictogram-w)
      (spacing *reference-sizes*)))

(defclass message-window (window)
  ((img-pictogram
    :initform (make-instance 'signalling-light
			     :x             0.0
			     :y             0.0
			     :width         (message-window-pictogram-w)
			     :height        (message-window-pictogram-h)
			     :texture-name  +window-close-button-texture-name+
			     :button-status t)
    :initarg  :img-pictogram
    :accessor img-pictogram)
   (text-message
    :initform (make-instance 'widget:static-text
			     :height    (d* 2.0
					    (small-square-button-size *reference-sizes*))
			     :width     (d* 5.0
					    (small-square-button-size *reference-sizes*))
			     :x         (message-window-text-x)
			     :y         0.0
			     :font-size (h4-font-size *reference-sizes*)
			     :label     "test test"
			     :justified t)
    :initarg  :text-message
    :accessor text-message)))

(defgeneric accomodate-message (object new-label))

(defgeneric accomodate-message-text (object new-label))

(defgeneric accomodate-message-img (object))

(defun set-texture-pictogram (pictogram texture-name)
  (when texture-name
    (setf (texture-object  pictogram) (get-texture texture-name))
    (setf (texture-pressed pictogram) nil)
    (setf (texture-overlay pictogram) nil)
    (setf (current-texture pictogram) (get-texture texture-name))))

(defmethod initialize-instance :after ((object message-window) &key
								 (message "")
								 (type :error)
								 &allow-other-keys)
  (with-accessors ((img-pictogram img-pictogram)) object
    (with-accessors ((img-pictogram img-pictogram)
		     (text-message text-message)
		     (frame frame)) object
      (add-child object img-pictogram)
      (add-child object text-message)
      (when message
	(accomodate-message object message))
      (case type
	(:error
	 (set-texture-pictogram img-pictogram +message-16-error-texture-name+))
	(:warning
	 (set-texture-pictogram img-pictogram +message-16-warning-texture-name+))
	(:info
	 (set-texture-pictogram img-pictogram +message-16-info-texture-name+))
	(:question
	 (set-texture-pictogram img-pictogram +message-16-help-texture-name+))
	(otherwise
	 (set-texture-pictogram img-pictogram +message-16-help-texture-name+))))))

(defmethod accomodate-message ((object message-window) new-label)
  (accomodate-message-text object new-label)
  (accomodate-message-img  object))

(defmethod accomodate-message-text ((object message-window) new-label)
  (with-accessors ((text-message text-message)
		   (frame frame)
		   (top-bar top-bar)
		   (window-height height)) object
    (with-accessors ((label-font-size label-font-size)
		     (label-font label-font)
		     (label-font-color label-font-color)
		     (textarea-height height)
		     (width width)
		     (children children)
		     (justified justified)) text-message
      (remove-all-children text-message)
      (let* ((char-width    (ftruncate (d/ width label-font-size)))
	     (lines         (remove-nbrk-space (split-text-lines new-label
								 char-width
								 justified)))
	     (wanted-height (d* label-font-size (d (length lines)))))
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
				 (fill-font-mesh-shell mesh :color label-font-color)
				 nil)))
		 (when shell
		   (setf (scaling shell) (sb-cga:vec label-font-size label-font-size 0.0))
		   (setf (pos     shell) (sb-cga:vec xf
						     (d* line-count label-font-size)

						     0.0))
		   (add-child text-message shell)))))
	(let ((new-frame-h (d+ wanted-height
			       (spacing *reference-sizes*)
			       (d* 2.0 (message-window-button-h)))))
	  (transform-vertices frame (sb-cga:scale* 1.0
						   (d/ new-frame-h
						       (height frame))
						   1.0))
	  (prepare-for-rendering frame)
	  (setf (y top-bar)      new-frame-h)
	  (setf (height frame)   new-frame-h)
	  (setf window-height    (d+ new-frame-h (height top-bar)))
	  (setf (y text-message) (d- window-height
				     (height top-bar)
				     (d* (top-frame-offset *reference-sizes*)
					 (height frame))
				     wanted-height)))))))

(defmethod accomodate-message-img ((object message-window))
  (with-accessors ((img-pictogram img-pictogram)
		   (frame frame)
		   (top-bar top-bar)
		   (text-message text-message)
		   (window-height height)) object
    (let* ((wanted-height-scale (d/ (min (d- (height frame)
					     (message-window-button-h)
					     (d* (top-frame-offset *reference-sizes*)
						 (height frame))
					     (d* (bottom-frame-offset *reference-sizes*)
						 (height frame))
					     (spacing *reference-sizes*))
					 (d- (message-window-text-x)
					     (spacing *reference-sizes*)))
				    (height img-pictogram)))
	   (wanted-height       (d* wanted-height-scale (height img-pictogram))))
      (transform-vertices img-pictogram (sb-cga:scale* wanted-height-scale
						       wanted-height-scale
						       1.0))
      (prepare-for-rendering img-pictogram)
      (setf (height   img-pictogram) wanted-height
	    (width    img-pictogram) wanted-height
	    (y        img-pictogram) (d- window-height
				       (height top-bar)
				       (d* (top-frame-offset *reference-sizes*)
				           (height frame))
				       wanted-height)
	    (x        img-pictogram)  (d* (left-frame-offset *reference-sizes*)
					  (width object))))))

(defun make-message-box (text title type &rest buttons-callbacks)
  "Car of each element of buttons-callbacks is the label, cdr the callback function"
  (let* ((widget (make-instance 'message-window
				:type    type
				:label   title
				:message text
				:x       0.0
				:y       0.0
				:width   (message-window-w)
				:height  (message-window-h)))
	 (button-w (d/ (d* 0.8 (width widget)) (d (length buttons-callbacks)))))
    (loop
       for b-cb in buttons-callbacks
       for x    from 0.0 by button-w do
	 (add-child widget
		    (make-instance 'button
				   :width    button-w
				   :height   (message-window-button-h)
				   :x        x
				   :y        (d- (height widget) (message-window-button-h))
				   :label    (car b-cb)
				   :callback (cdr b-cb))))
    widget))
