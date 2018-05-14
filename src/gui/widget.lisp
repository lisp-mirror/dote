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

(alexandria:define-constant +texture-unit-overlay+             1                   :test #'=)

(alexandria:define-constant +portrait-size+                   64.0                 :test #'=)

(alexandria:define-constant +slots-per-page-side-size+         4                   :test #'=)

(alexandria:define-constant +action-move+                        :move             :test #'eq)

(alexandria:define-constant +action-attack-short-range+          :attack-srg       :test #'eq)

(alexandria:define-constant +action-attack-long-range+           :attack-lrg       :test #'eq)

(alexandria:define-constant +action-attack-long-range-imprecise+ :attack-lrg-imp   :test #'eq)

(alexandria:define-constant +action-launch-spell+                :launch-spell     :test #'eq)

(defparameter *square-button-size* (d/ (d *window-w*) 10.0))

(defparameter *small-square-button-size* (d/ (d *window-w*) 20.0))

(alexandria:define-constant +file-visible-slot+ 5 :test #'=)

(defparameter *child-remap-y* t)

(definline widgetp (w)
  (typep w 'widget))

(defmacro with-grandparent-widget ((grandparent) w &body body)
  (alexandria:with-gensyms (parent)
    `(with-parent-widget (,parent) ,w
       (with-parent-widget (,grandparent) ,parent
         ,@body))))

(defmacro with-parent-widget ((par) child &body body)
  `(let ((,par (parent ,child)))
     ,@body))

(defun find-root-widget (widget)
  (if (parent widget)
      (find-root-widget (parent widget))
      widget))

(defmacro with-root-widget ((root child) &body body)
  `(let ((,root (find-root-widget ,child)))
     ,@body))

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
   (top-bar-tile-height-scaling
    :initform 0.60
    :initarg  :top-bar-tile-height-scaling
    :accessor top-bar-tile-height-scaling)
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
    :initform 0.070
    :initarg  :top-frame-offset
    :accessor top-frame-offset)
   (bottom-frame-offset
    :initform 0.070
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
   (button-label-color
    :initform §cffc400ff
    :initarg  :button-label-color
    :accessor button-label-color)
   (checkbutton-h
    :initform 16.0
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
   (spacing-rel
    :initform 0.3
    :initarg  :spacing-rel
    :accessor spacing-rel)
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
    :initform (button-label-color *reference-sizes*)
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

(defmethod remove-entity-if ((object widget) predicate)
  (mtree:remove-child-if object predicate))

(defmethod remove-entity-by-id ((object widget) id)
  (mtree:remove-child-if object #'(lambda (a) (= (id a) id))))

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

;;(defgeneric inner-frame-h (object))

(defgeneric flip-y (object child))

(defgeneric width (object))

(defgeneric height (object))

(defgeneric x (object))

(defgeneric y (object))

(defgeneric (setf x) (new-value object))

(defgeneric (setf y) (new-value object))

(defgeneric setup-label (object new-label &key reset-label-slot))

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

(defun common-setup-label (widget new-label &key (reset-label-slot t))
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
      (when reset-label-slot
        (setf label new-label))
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
  (declare (ignore event))
  (hide (parent widget)))

(defun hide-grandparent-cb (widget event)
  (declare (ignore event))
  (let ((grandparent (parent (parent widget))))
    (when grandparent
      (hide grandparent))))

(defun hide-and-remove-from-parent-cb (widget event)
  (declare (ignore event))
  (with-parent-widget (win) widget
    (hide widget)
    (remove-child win
                  widget
                  :key  #'identity
                  :test #'(lambda (a b)
                            (= (id a) (id b))))))

(defalias hide-and-remove-from-parent #'hide-and-remove-from-parent-cb)

(defun hide-and-remove-parent-cb (widget event)
  (declare (ignore event))
  (with-parent-widget (win) widget
    (hide win)
    (remove-child (parent win)
                  ;;(make-instance (class-of win))
                  win
                  :key #'identity
                  :test #'(lambda (a b)
                            (= (id a) (id b))))))

(defalias hide-and-remove-parent #'hide-and-remove-parent-cb)

(defun hide-and-remove-grandparent-cb (widget event)
  (declare (ignore event))
  (with-parent-widget (parent) widget
    (with-parent-widget (grandparent) parent
      (hide grandparent)
      (remove-child (parent grandparent)
                    (class-of grandparent)
                    :key #'identity
                    :test #'(lambda (a b)
                              (declare (ignore a))
                              (typep b (class-of grandparent)))))))

(defun add-window-button-cb-hide-remove (w)
  (setf (callback (close-button w)) #'hide-and-remove-grandparent-cb))

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

(defmethod setup-label ((object naked-button) new-label &key (reset-label-slot t))
  "does nothing"
  (declare (ignore object  new-label reset-label-slot)))

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

(defmethod initialize-instance :after ((object toggle-button) &key
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

(gen-type-p check-button)

(defmethod initialize-instance :after ((object check-button) &key &allow-other-keys))

(defmethod flip-state ((object check-button))
  (with-slots (button-state) object
    (with-accessors ((group group)) object
      (when (not (button-state object))
        (setf (button-state object) t)
        (map nil #'(lambda (a)
                     (when (/= (id a) (id object))
                       (setf (button-state a) nil)))
             group)))))

(defclass labeled-check-button (widget)
  ((button
    :initform nil
    :initarg  :button
    :accessor button)
   (text
    :initform nil
    :initarg  :text
    :accessor text)))

(defmethod initialize-instance :after ((object labeled-check-button)
                                       &key
                                         (callback           nil)
                                         (color              :green)
                                         (button-toggle-type nil)
                                         (initial-state      nil)
                                         &allow-other-keys)
  (with-accessors ((button button) (text text)
                   (width width) (height height)
                   (label label)
                   (label-font-size label-font-size)) object
    (let ((button-type (if button-toggle-type
                           'toggle-button
                           'check-button)))
      (setf button (make-instance button-type
                                  :width    height
                                  :height   height
                                  :color    color
                                  :callback callback
                                  :x        (d- width height)
                                  :y        0.0)))
    (setf (button-state button) initial-state)
    (let ((text-font-size (min label-font-size
                               (d* (label-font-size object)
                                   (d/ (d- width height) ;; checkbox is square
                                       (label-width object))))))
      (setf text (make-instance 'simple-label
                                :x               0.0
                                :y               0.0
                                :width           (d- width height)
                                :height          height
                                :label           label
                                :justified       nil
                                :label-font-size text-font-size)))
    (add-child object text)
    (add-child object button)))

(defmethod setup-label ((object labeled-check-button) new-label &key (reset-label-slot t))
  (with-accessors ((text text)) object
    (setup-label text new-label :reset-label-slot reset-label-slot)))

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
  (on-mouse-released (button object) event))

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
      (setf button-state new-state)
      (setf shown        new-state))))

(defmethod on-mouse-pressed ((object signalling-light) event)
  nil)

(defmethod on-mouse-released ((object signalling-light) event)
  nil)

(defclass health-status-icon (signalling-light inner-animation) ())

(defmethod calculate :before ((object health-status-icon) dt)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((el-time el-time)
                   (scaling scaling)) object
    (let ((scale-factor (d+ 1.0 (num:bounce-step-interpolate-rev 0.0 10.0 el-time))))
      (setf scaling (sb-cga:vec scale-factor scale-factor scale-factor))
      (setf el-time
            (d+ el-time
                (d* (animation-speed object) dt))))))

(defclass animated-icon (signalling-light inner-animation animated-spritesheet)
  ())

(defmethod initialize-instance :after ((object animated-icon) &key &allow-other-keys)
  (with-accessors ((texture-window-width texture-window-width)
                   (width width)
                   (height height)) object
    (remove-mesh-data object)
    (destroy object)
    (setf (use-blending-p object) t)
    (quad object width height
          0.0 0.0 texture-window-width 1.0
          +zero-vec+
          nil t)
    (remove-orphaned-vertices object)
    (prepare-for-rendering object)))

(defmethod calculate :before ((object animated-icon) dt)
  (declare (optimize (debug 0) (speed 1) (safety 0)))
  (with-accessors ((calculatep                calculatep)
                   (anim-delay                anim-delay)
                   (frequency-animation       frequency-animation)
                   (el-time                   el-time)
                   (texture-window-width      texture-window-width)
                   (animation-loop-p          animation-loop-p)
                   (animation-speed           animation-speed)
                   (frames-number             frames-number)
                   (frame-idx                 frame-idx)
                   (frame-count               frame-count)) object
    (declare (fixnum anim-delay frame-count frame-idx))
    (declare (desired-type el-time animation-speed dt))
    (incf el-time (d* dt animation-speed))
    (incf frame-count)
    (when (or animation-loop-p
              (not (animated-billboard-last-frame-reached-p object)))
      (when (= (rem frame-count frequency-animation) 0)
        (incf frame-idx)))))

(defmethod render ((object animated-icon) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((duration/2           duration/2)
                   (projection-matrix    projection-matrix)
                   (compiled-shaders     compiled-shaders)
                   (el-time              el-time)
                   (frame-idx            frame-idx)
                   (texture-window-width texture-window-width)
                   (model-matrix         model-matrix)
                   (triangles            triangles)
                   (texture-object       texture-object)
                   (vao                  vao)
                   (view-matrix          view-matrix)
                   (shown                shown)) object
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles))
    (when shown
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (cl-gl-utils:with-blending
            (gl:blend-func                :src-alpha :one-minus-src-alpha)
            (use-program compiled-shaders :gui-animated-icon)
            (gl:active-texture            :texture0)
            (texture:bind-texture texture-object)
            (uniformi  compiled-shaders :texture-object        +texture-unit-diffuse+)
            (uniformi  compiled-shaders :frame-idx             frame-idx)
            (uniformf  compiled-shaders :texture-window-width  texture-window-width)
            (uniform-matrix compiled-shaders :modelview-matrix 4
                            (vector (sb-cga:matrix* camera-vw-matrix
                                                    (elt view-matrix 0)
                                                    (elt model-matrix 0)))
                            nil)
            (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
            (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
            (gl:draw-arrays :triangles 0 (* 3 (length triangles)))))))))

(defun make-loader-icon (x y &key
                               (texture-name          +elaborating-plan-spinner-texture-name+)
                               (w (square-button-size *reference-sizes*))
                               (h (square-button-size *reference-sizes*))
                               (animation-loop t)
                               (h-offset       0.25)
                               (freq-anim      20)
                               (compiled-shaders nil)
                               (shown            t))
  (make-instance 'animated-icon
                 :compiled-shaders          compiled-shaders
                 :texture-name              texture-name
                 :x                         x
                 :y                         y
                 :width                     w
                 :height                    h
                 :texture-window-width      h-offset
                 :animation-loop-p          animation-loop
                 :frequency-animation       freq-anim
                 :shown                     shown))

(defun make-planning-progress-icon (&key (shown nil) (compiled-shaders nil))
  (make-loader-icon (d- (d *window-w*) (square-button-size *reference-sizes*))
                     (square-button-size *reference-sizes*)
                     :compiled-shaders compiled-shaders
                     :texture-name     +elaborating-plan-spinner-texture-name+
                     :w                (square-button-size *reference-sizes*)
                     :h                (square-button-size *reference-sizes*)
                     :animation-loop   t
                     :h-offset         0.25
                     :freq-anim        15
                     :shown            shown))

(defclass button (naked-button) ())

(defmethod initialize-instance :after ((object button)
                                       &key
                                         (use-label-global-style t) &allow-other-keys)
  (when use-label-global-style
    (setf (label-font-size  object) (button-label-max-size *reference-sizes*))
    (setf (label-font-color object) (button-label-color    *reference-sizes*)))
  (with-slots (label) object
    (when label
      (setf (label object) label))))

(defmethod setup-label ((object button) new-label &key (reset-label-slot t))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font-size label-font-size)
                   (height height)
                   (width width)
                   (children children)) object
    (declare (desired-type width height label-font-size))
    (common-setup-label object new-label :reset-label-slot reset-label-slot)
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

(defmethod (setf label) (new-label (object button))
  (setup-label object new-label :reset-label-slot t))

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

(defmethod setup-label ((object text-field) new-label &key (reset-label-slot t))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font-size label-font-size)
                   (height height)
                   (width width)
                   (children children)) object
    (declare (desired-type width height label-font-size))
    (common-setup-label object new-label :reset-label-slot reset-label-slot)
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


(defmethod (setf label) (new-label (object text-field))
  (setup-label object new-label :reset-label-slot t))

(defmethod (setf focus) (new-state (object text-field))
  (with-slots (focus) object
    (with-accessors ((current-texture current-texture)
                     (texture-pressed texture-pressed)
                     (texture-object  texture-object)) object
      (setf focus            new-state)
      (setf current-texture  (if focus
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
             (setf (label object) (subseq old-label 0 (max 0 (f- (length old-label) 1))))))
          t)
      nil)))

(defclass char-field (text-field) ())

(defmethod on-key-pressed ((object char-field) event)
  (if (focus object)
      (when (gui-printable-p (char-event event))
        (setf (label object) (string (char-event event)))
        t)
      nil))

(defmethod (setf label) (new-label (object char-field))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  #+debug-mode (assert new-label)
  #+debug-mode (assert (> (length new-label) 0))
  (call-next-method new-label object))

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

(defmethod setup-label ((object simple-label) new-label &key (reset-label-slot t))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((label-font-size label-font-size)
                   (children children)) object
    (common-setup-label object new-label :reset-label-slot reset-label-slot)
    (setf (label-font-size object) (calculate-text-scaling object))
    (loop
       for l across (the (simple-array triangle-mesh (*)) children)
       for xf single-float from 0.0 by label-font-size do
         (setf (scaling l) (sb-cga:vec label-font-size
                                       label-font-size
                                       0.0))
         (setf (pos     l) (sb-cga:vec xf 0.0 0.0)))))

(defmethod (setf label) (new-label (object simple-label))
  (setup-label object new-label :reset-label-slot t))

(defclass simple-label-prefixed (simple-label)
  ((prefix
    :initform ""
    :initarg  :prefix
    :accessor prefix)))

(defmethod (setf label) (new-label (object simple-label-prefixed))
  (with-accessors ((prefix prefix)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (simple-string new-label prefix))
    (setup-label object
                 (format nil "~a~a" prefix new-label)
                 :reset-label-slot t)))

(defun common-lush-*-label-setup (widget new-label x-shift-fn &key (reset-label-slot t))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (declare (function x-shift-fn))
  (with-accessors ((label-font-size label-font-size)
                   (label-font label-font)
                   (label-font-color label-font-color)
                   (height height)
                   (width width)
                   (children children)) widget
    (declare (desired-type width height label-font-size))
    (remove-all-children widget)
    (when reset-label-slot
      (with-slots (label) widget
        (setf label new-label)))
    (let* ((char-width    (ftruncate (d/ width label-font-size)))
           (lines         (reverse (flush-left-mono-text (split-words new-label) char-width)))
           (actual-height (d* label-font-size (d (length lines)))))
      (declare (list lines))
      (setf height actual-height)
      (loop
         for line-count from 0.0 by 1.0
         for line in lines do
        (loop
           for c across (the simple-string line)
           for xf single-float from  0.0 by label-font-size do
           (let* ((mesh  (get-char-mesh label-font c))
                  (shell (if mesh
                             (fill-font-mesh-shell mesh :color label-font-color)
                             nil)))
             (when shell
               (setf (scaling shell) (sb-cga:vec label-font-size label-font-size 0.0))
               (setf (pos     shell) (sb-cga:vec (funcall x-shift-fn
                                                          widget
                                                          xf
                                                          line)
                                                 (d* line-count label-font-size)
                                                 0.0))
               (add-child widget shell))))))))

(defclass flush-left-label (simple-label) ())

(defmethod setup-label ((object flush-left-label) new-label &key (reset-label-slot t))
  (with-accessors ((label-font-size label-font-size)
                   (label-font label-font)
                   (label-font-color label-font-color)
                   (height height)
                   (width width)
                   (children children)) object
    (declare (desired-type width height label-font-size))
    (let ((x-shift-fn #'(lambda (widget xf line)
                          (declare (ignore widget line))
                          xf)))
      (common-lush-*-label-setup object new-label x-shift-fn
                                 :reset-label-slot reset-label-slot))))

(defmethod (setf label) (new-label (object flush-left-label))
  (setup-label object new-label :reset-label-slot t))

(defclass flush-center-label (simple-label) ())

(defmethod setup-label ((object flush-center-label) new-label &key (reset-label-slot t))
  (with-accessors ((label-font-size label-font-size)
                   (label-font label-font)
                   (label-font-color label-font-color)
                   (height height)
                   (width width)
                   (children children)) object
    (declare (desired-type width height label-font-size))
    (let ((x-shift-fn #'(lambda (widget xf line)
                          (declare (ignore widget))
                          (d- (d+ xf
                                  (d/ width 2.0))
                              (d/ (d* label-font-size
                                      (d (length line)))
                                  2.0)))))
      (common-lush-*-label-setup object new-label x-shift-fn
                                 :reset-label-slot reset-label-slot))))

(defmethod (setf label) (new-label (object flush-center-label))
  (setup-label object new-label :reset-label-slot t))

(defclass static-text (#+debug-gui naked-button
                       #-debug-gui widget)
  ((justified
    :initform t
    :initarg  :justified
    :accessor justified)))

(defmethod initialize-instance :after ((object static-text) &key &allow-other-keys)
  (with-slots (label) object
    (when label
      (setf (label object) label))))

(defun split-text-lines (text width justify)
  (reverse (or (alexandria:flatten
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

(defmethod setup-label ((object static-text) new-label &key (reset-label-slot t))
  (with-accessors ((label-font-size label-font-size)
                   (label-font label-font)
                   (label-font-color label-font-color)
                   (height height)
                   (width width)
                   (children children)
                   (justified justified)) object
    (declare (desired-type width height label-font-size))
    (remove-all-children object)
    (when reset-label-slot
      (with-slots (label) object
        (setf label new-label)))
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
                                                 0.0)))
             (add-child object shell)))))))

(defmethod (setf label) (new-label (object static-text))
  (setup-label object new-label :reset-label-slot t))

(defmethod label-width ((object static-text))
  (width object))

#+debug-gui
(defmethod render ((object static-text) renderer)
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

(defmethod setup-label ((object h-bar) new-label &key (reset-label-slot t))
  (declare (simple-string new-label))
  (with-accessors ((label-font label-font)
                   (label-font-size label-font-size)
                   (label-font-color label-font-color)
                   (children children)
                   (height height)
                   (width width)
                   (label-shells label-shells)) object
    (declare (desired-type width height label-font-size))
    (declare ((array font-mesh-shell (*)) label-shells))
    (with-slots (label) object
      (declare (simple-string label))
      (when reset-label-slot
        (setf label new-label))
      (loop for c across label do
           (let* ((mesh  (get-char-mesh label-font c))
                  (shell (if mesh
                             (fill-font-mesh-shell mesh :color label-font-color)
                             nil)))
             (when shell
               (vector-push-extend shell label-shells)
               (add-child object shell)))))
    (loop
       for l across label-shells
       for xf single-float from 0.0 by label-font-size do
         (setf (scaling l) (sb-cga:vec label-font-size height 0.0))
         (setf (pos l)     (sb-cga:vec xf 0.0 0.0)))))

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
    (setup-label object new-label :reset-label-slot t)))

(defgeneric (setf fill-level) (value object))

(defmethod (setf fill-level) (value (object h-bar))
  (with-accessors ((actual-bar actual-bar)) object
    (with-slots (fill-level) object
      (setf fill-level (alexandria:clamp value 0.0 1.0))
      (setf (scaling actual-bar) (sb-cga:vec fill-level 1.0 1.0)))))

(defun adjust-window-h (frame)
  "use this function to have the inner frame height matching the size of the whole frame"
   (d+ frame
        (top-bar-h               *reference-sizes*)
        (d* (top-frame-offset    *reference-sizes*) frame)
        (d* (bottom-frame-offset *reference-sizes*) frame)))

(defun adjust-window-w (frame)
  "use this function to have the inner frame width matching the size of the whole frame"
  (d+ frame
      (d* 2.0 (left-frame-offset *reference-sizes*) frame)))

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
    :initform nil
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
          (setf (close-button object) button)
          (prepare-for-rendering top-bar)
          ;; add main frame
          (add-quad-for-widget  frame)
          (transform-vertices   frame  (sb-cga:scale* (width object) (d- height top-bar-h) 1.0))
          (setf (texture-object frame) (get-texture +frame-texture-name+)
                (pos            frame) (sb-cga:vec 0.0 0.0 0.0)
                (width          frame) (width object)
                (height         frame) (d- height top-bar-h))
          (prepare-for-rendering frame)
          (add-child object frame)
          (add-child object top-bar))))))

(defun remap-x-child-in-window (window child)
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (with-accessors ((parent-w width)
                   (frame    frame)) window
    (with-accessors ((orig-w-frame width)) frame
      (with-accessors ((child-x x)
                       (child-w width)) child
        (let* ((offset    (d* 2.0 (left-frame-offset *reference-sizes*) orig-w-frame))
               (frame-w   (d- orig-w-frame offset))
               (scale-child (d/ frame-w parent-w))
               (scale       (alexandria:clamp (normalize-value-in-range child-x
                                                                       0.0
                                                                       parent-w)
                                              0.0 1.0))

               (child-new-w (d* scale-child child-w))
               (child-new-x (d+ (d* scale frame-w)
                                (d/ offset 2.0))))
          (values scale-child child-new-w child-new-x))))))

(defun remap-y-child-in-window (window child)
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (with-accessors ((parent-h height)
                   (frame    frame)) window
    (with-accessors ((orig-h-frame height)) frame
      (with-accessors ((child-y y)
                       (child-h height)) child
        (let* ((top-offset    (d* (top-frame-offset *reference-sizes*)    orig-h-frame))
               (bottom-offset (d* (bottom-frame-offset *reference-sizes*) orig-h-frame))
               (frame-h       (d- orig-h-frame
                                  top-offset
                                  bottom-offset))
               (scale   (- 1.0
                           (alexandria:clamp (normalize-value-in-range child-y
                                                                       0.0
                                                                       parent-h)
                                             0.0 1.0)))
               (scale-child (d/ frame-h parent-h))
               (child-new-h (d* scale-child child-h))
               (child-new-y (d- (d+  (d* scale frame-h)
                                     bottom-offset)
                                child-new-h)))
          (values scale-child child-new-h child-new-y))))))

(defmethod fit-into-window ((object window) (window window))
  (declare (ignore object window)))

(defmethod fit-into-window ((object widget) (window window))
  (with-accessors ((child-y y)
                   (child-x x)
                   (child-h height)
                   (child-w width)) object
    (multiple-value-bind (scale-y child-new-h child-new-y)
        (remap-y-child-in-window window object)
      (multiple-value-bind (scale-x child-new-w child-new-x)
          (remap-x-child-in-window window object)
        (recursively-transform-vertices object (sb-cga:scale* scale-x scale-y 1.0))
        (prepare-for-rendering object)
        (setf child-x child-new-x
              child-y child-new-y
              child-h child-new-h
              child-w child-new-w)
        (values object window)))))

(defmethod fit-into-window :after ((object labeled-check-button) (window window))
  (with-accessors ((child-y y)
                   (child-x x)
                   (child-h height)
                   (child-w width)) object
    (let* ((scale-y (remap-y-child-in-window window object))
           (scale-x (remap-x-child-in-window window object)))
      (with-accessors ((text text)
                       (width width)
                       (height height)
                       (button button)) object
        (with-accessors ((label-width  width)
                         (label-height height)) text
          (with-accessors ((x-button x)
                           (button-width  width)
                           (button-height height)) button
            (let* ((new-button-width  (d* button-width  scale-x))
                   (new-button-height (d* button-height scale-y))
                   (new-label-width   (d- width (max new-button-width new-button-height))))
              (setf x-button      new-label-width
                    button-width  new-button-width
                    button-height new-button-height
                    label-width   new-label-width
                    label-height  height)))))))
  (values object window))

(defmethod add-child :after ((object window) child
                             &optional (child-pos (length (children object))))
  (with-accessors ((width width)) object
    (with-accessors ((x-child x) (y-child y)
                     (last-window-added-to last-window-added-to)) child
      (when (not (and last-window-added-to
                      (eq  object last-window-added-to)))
        (setf last-window-added-to object)
        (when (> child-pos 2) ;; skip the titlebar, the frame and the close-button
          (remap-x-child-in-window object child)
          (remap-y-child-in-window object child)
          (fit-into-window child object)
          (setup-label child (label child) :reset-label-slot nil))))))

(defun common-setup-label-window (top-bar new-label &key (reset-label-slot t))
  (with-accessors ((label-font label-font)
                   (label-font-size label-font-size)
                   (label-font-color label-font-color)
                   (children children)) top-bar
    (with-slots (label) top-bar
      (remove-child-if top-bar #'(lambda (a) (not (typep a 'naked-button))))
      (when reset-label-slot
        (setf label new-label))
      (loop for c across label do
           (let* ((mesh  (get-char-mesh label-font c))
                  (shell (if mesh
                             (fill-font-mesh-shell mesh :color label-font-color)
                             nil)))
             (when shell
               (add-child top-bar shell)))))))

(defmethod setup-label ((object window) new-label &key (reset-label-slot t))
  (with-accessors ((label-font label-font)
                   (top-bar top-bar)
                   (height height)
                   (width width)) object
    (with-accessors ((bar-label-font-size label-font-size)
                     (bar-children children)) top-bar
      (let* ((top-bar-h (top-bar-h *reference-sizes*))
             (top-bar-relative-offset (top-bar-relative-offset *reference-sizes*))
             (font-height-scaling     (top-bar-tile-height-scaling *reference-sizes*))
             (font-height              (d* font-height-scaling top-bar-h)))
        (common-setup-label-window top-bar new-label :reset-label-slot reset-label-slot)
        (let* ((label-width   (label-width top-bar))
               (scaling-width (d/ (d- width (d* width (relative-title-space *reference-sizes*)))
                                  label-width)))
          (when (d< scaling-width 1.0)
            (setf bar-label-font-size (d* bar-label-font-size scaling-width)))
          (loop
             for l across bar-children
             for xf from 0.0 by bar-label-font-size
             when (not (typep l 'naked-button))     do
               (setf (scaling l) (sb-cga:vec bar-label-font-size
                                             font-height
                                             0.0))
               (setf (pos l)     (sb-cga:vec (d+ (d* width top-bar-relative-offset) xf)
                                             (d- (d* top-bar-h 0.5)
                                                 (d* 0.5 font-height))
                                             0.0))))))))

(defmethod (setf label) (new-label (object window))
  (setup-label object new-label :reset-label-slot t))

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

;; (defmethod inner-frame-h ((object window))
;;   (with-accessors ((frame frame)) object
;;     (d- (height frame)
;;         (d* (top-frame-offset *reference-sizes*) (height (frame object))))))

(defmethod on-mouse-pressed ((object window) event)
  (with-accessors ((dragging-mode dragging-mode)) object
    (if (and (shown object)
             (mouse-over object (x-event event) (y-event event)))
        (progn
          (do-children-from-end (w object)
            (when (and (widgetp w)
                       (on-mouse-pressed w event))
              (return-from on-mouse-pressed t)))
          t)
        nil)))

(defmethod on-mouse-released ((object window) event)
  (if (and (shown object)
           (mouse-over object (x-event event) (y-event event)))
      (progn
        (do-children-from-end (w object)
             (when (and (widgetp w)
                        (on-mouse-released w event))
               (return-from on-mouse-released t)))
        t)
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
  (loop for d in (sort (fs:directory-files (current-dir object))
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

(defmacro with-toolbar-world ((world) toolbar &body body)
  `(with-accessors ((,world bound-world)) , toolbar
     ,@body))

(defclass toolbar-modal-button (naked-button) ())

(defmethod on-mouse-released ((object toolbar-modal-button) event)
  (with-parent-widget (toolbar) object
    (with-toolbar-world (world) toolbar
      (with-accessors ((main-state main-state)) world
        (if (world:human-interaction-allowed-p world)
            (call-next-method)
            (if (mouse-over object (x-event event) (y-event event))
                (progn
                  (setf (current-texture object) (texture-object object))
                  t)
                nil))))))

(defun make-square-button (x y overlay callback &key (small nil) (modal nil))
  (let ((size (if small
                  *small-square-button-size*
                  *square-button-size*))
        (type (if modal
                  'toolbar-modal-button
                  'naked-button)))
    (make-instance type
                   :x               x
                   :y               y
                   :width           size
                   :height          size
                   :texture-object  (get-texture +square-button-texture-name+)
                   :texture-pressed (get-texture +square-button-pressed-texture-name+)
                   :texture-overlay (get-texture overlay)
                   :callback        callback)))

(defun make-rect-button (x y scale-w scale-h overlay callback &key (modal nil))
  (let ((type (if modal
                  'toolbar-modal-button
                  'naked-button)))
    (make-instance type
                   :x               x
                   :y               y
                   :width           (rel->abs scale-w)
                   :height          (rel->abs scale-h)
                   :texture-object  (get-texture +square-button-texture-name+)
                   :texture-pressed (get-texture +square-button-pressed-texture-name+)
                   :texture-overlay (get-texture overlay)
                   :callback        callback)))

(defun make-health-condition (x y size texture-name)
  (make-instance 'health-status-icon
                 :animation-speed 2.0
                 :x               x
                 :y               y
                 :width           (rel->abs size)
                 :height          (rel->abs size)
                 :texture-name    texture-name
                 :shown           nil
                 :button-status   nil))

(defun make-change-character-cb (change-fn)
  #'(lambda (w e)
      (declare (ignore e))
      (with-parent-widget (toolbar) w
        (with-accessors ((bound-world  bound-world)) toolbar
          (funcall change-fn bound-world)))))

(defun next-turn-cb (w e)
  (declare (ignore e))
    (with-parent-widget (toolbar) w
      (with-accessors ((bound-player bound-player)) toolbar
        (with-accessors ((state state)) bound-player
          (game-state:increase-game-turn state)
          t))))

(defun %rotate (w e dir)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)
                     (bound-world  bound-world)) toolbar
      (with-accessors ((main-state main-state)) bound-world
        (when bound-player
          (let ((id-player (id bound-player)))
            (if (eq dir :cw)
                (let ((event (make-instance 'game-event:rotate-entity-cw-event
                                            :id-destination id-player)))
                  (game-event:propagate-rotate-entity-cw-event event))
                (let ((event (make-instance 'game-event:rotate-entity-ccw-event
                                            :id-destination id-player)))
                  (game-event:propagate-rotate-entity-ccw-event event)))
            (game-state:reset-costs-from-pc main-state))))))
  t)

(defun rotate-cw-cb (w e)
  (%rotate w e :cw))

(defun rotate-ccw-cb (w e)
  (%rotate w e :ccw))

(defun facing-door (game-state pos dir)
  "Pos in cost space, dir in world space"
  (let ((all-doors (game-state:get-neighborhood game-state
                                                (elt pos 1)
                                                (elt pos 0)
                                                #'(lambda (a p)
                                                    (declare (ignore p))
                                                    (or (eq (game-state:el-type a)
                                                            +door-n-type+)
                                                        (eq (game-state:el-type a)
                                                            +door-s-type+)
                                                        (eq (game-state:el-type a)
                                                            +door-w-type+)
                                                        (eq (game-state:el-type a)
                                                            +door-e-type+)))))
        (facing-door                           nil))
    (loop for door across all-doors do
         (when (map-utils:facingp pos dir (cdr door))
           (setf facing-door door)))
    facing-door))

(defun open-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)) toolbar
      (when bound-player
        (with-accessors ((dir dir)) bound-player
          (let* ((game-state  (mesh::state bound-player))
                 (pos         (mesh:calculate-cost-position bound-player))
                 (facing-door (facing-door game-state pos dir)))
            (when facing-door
              (let* ((id-door (entity-id (car facing-door)))
                     (door-event (game-event:make-simple-event-w-dest 'game-event:open-door-event
                                                                      (id bound-player)
                                                                      id-door)))
                (game-event:propagate-open-door-event door-event))))))))
  t)

(defun close-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)) toolbar
      (when bound-player
        (with-accessors ((dir dir)) bound-player
          (let* ((game-state  (mesh::state bound-player))
                 (pos         (mesh:calculate-cost-position bound-player))
                 (facing-door (facing-door game-state pos dir)))
            (when facing-door
              (let* ((id-door (entity-id (car facing-door)))
                     (door-event (game-event:make-simple-event-w-dest 'game-event:close-door-event
                                                                      (id bound-player)
                                                                      id-door)))
                (game-event:propagate-close-door-event door-event))))))))
  t)


(defun %find-entity (state pos)
  (game-state:find-entity-by-id state
                                (game-state:entity-id (matrix:matrix-elt (map-state state)
                                                                         (elt pos 1)
                                                                         (elt pos 0)))))

(defun lookup-chest (player)
  (let* ((map-state  (map-state (state player)))
         (pos        (map-utils:pos-entity-chunk->cost-pos (pos player)))
         (near-tiles (matrix:gen-4-neighbour-counterclockwise (elt pos 0)
                                                              (elt pos 1)
                                                              :add-center nil))
         (chests-pos (remove-if-not
                      #'(lambda (p) (and (matrix:element@-inside-p map-state
                                                                   (elt p 1)
                                                                   (elt p 0))
                                         (%find-entity (state player) p)
                                         (containerp (ghost (%find-entity (state player) p)))))
                      near-tiles)))
    #+debug-mode (misc:dbg "chest pos ~a" chests-pos)
    (and chests-pos
         (%find-entity (state player) (first chests-pos)))))

(defun toolbar-open-inventory-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)) toolbar
      (when bound-player
        (with-accessors ((ghost ghost)) bound-player
          ;; lookup for chest
          (let* ((chest (lookup-chest bound-player))
                 (inventory (make-inventory-window bound-player chest)))
            (setf (compiled-shaders inventory) (compiled-shaders toolbar))
            (add-child toolbar inventory))))))
  t)

(defun toolbar-move-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)
                     (bound-world  bound-world)
                     (selected-action selected-action)) toolbar
      (with-accessors ((main-state main-state)) bound-world
        (when bound-player
          (game-state:reset-costs-from-pc main-state)
          (setf selected-action +action-move+))))))

(defun toolbar-attack-short-range-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)
                     (selected-action selected-action)) toolbar
      (when bound-player
        (setf selected-action +action-attack-short-range+)))))

(defun toolbar-attack-long-range-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)
                     (selected-action selected-action)) toolbar
      (when bound-player
        (setf selected-action +action-attack-long-range+)))))

(defun toolbar-attack-long-range-imprecise-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)
                     (selected-action selected-action)) toolbar
      (when bound-player
        (setf selected-action +action-attack-long-range-imprecise+)))))

(defun toolbar-activation-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)
                     (selected-action selected-action)) toolbar
      (when bound-player
        (multiple-value-bind (facing-entity map-element)
            (mesh:entity-facing bound-player)
          (when (and facing-entity
                     (eq (game-state:el-type map-element)
                         +magic-furniture-type+))
            (game-event:send-activate-switch-event bound-player facing-entity)))))))

(defun toolbar-launch-spell-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-player bound-player)
                     (selected-action selected-action)) toolbar
      (when bound-player
        (setf selected-action +action-launch-spell+)))))

(defun toolbar-zoom-in-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-toolbar-world (world) toolbar
      (let ((camera (world:camera world)))
        (setf (mode camera) :drag)
        (camera:drag-camera (world:camera world) (sb-cga:vec .0 (d- +gui-zoom-entity+) .0))))))

(defun toolbar-zoom-out-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-toolbar-world (world) toolbar
      (let ((camera (world:camera world)))
        (setf (mode camera) :drag)
        (camera:drag-camera (world:camera world) (sb-cga:vec .0 +gui-zoom-entity+ .0))))))

(defun toolbar-open-configuration-window (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (add-child toolbar
               (configuration-windows:make-main-window (compiled-shaders toolbar)))))

(defun toolbar-save-game-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-toolbar-world (world) toolbar
      (with-accessors ((main-state main-state)) world
        (let ((window  (load-save-window:make-window (compiled-shaders w) :save)))
          (add-child toolbar window))))))

(defun toolbar-load-game-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-toolbar-world (world) toolbar
      (with-accessors ((main-state main-state)) world
        (tg:gc)
        (let ((window  (load-save-window:make-window (compiled-shaders w) :load)))
          (add-child toolbar window))))))

(defun toolbar-quit-cb (w e)
  (declare (ignore e))
  (with-parent-widget (toolbar) w
    (with-accessors ((bound-world bound-world)) toolbar
      #+ debug-mode (assert bound-world)
      (with-accessors ((main-state main-state)) bound-world
        (flet ((quit-cb (a b)
                 (declare (ignore a b))
                 (sdl2.kit:close-window (game-state:fetch-render-window main-state)))
               (close-cb (w e)
                 (hide-and-remove-parent-cb w e)))
          (let ((dialog-window (make-message-box* (_ "Are you sure you want to quit?")
                                                  "Question"
                                                  :question
                                                  (list (cons (_ "Yes") #'quit-cb)
                                                        (cons (_ "No")  #'close-cb)))))
            (setf (compiled-shaders dialog-window) (compiled-shaders w))
            (mtree:add-child toolbar dialog-window)))))))

(defclass main-toolbar (widget)
  (#+ (and debug-mode debug-ai)
      (influence-map-dump :initform (make-instance 'signalling-light
                                                   :x             (d 0)
                                                   :y             (d (- *window-h*
                                                                        +influence-map-h+))
                                                   :width         (d +influence-map-w+)
                                                   :height        (d +influence-map-h+)
                                                   :texture-name  +influence-map+
                                                   :shown         t
                                                   :button-status t)
                          :initarg  :influence-map-dump
                          :accessor influence-map-dump)
   (selected-action
    :initform nil
    :initarg  :selected-action
    :accessor selected-action)
   (bound-player
    :initform nil
    :initarg  :bound-player
    :accessor bound-player
    :type md2::md2-mesh)
   (bound-world
    :initform nil
    :initarg  :bound-world
    :accessor bound-world)
   ;; first row
   (s-faint
    :initform (make-health-condition (d* 4.0 *small-square-button-size*)
                                     (d+ *small-square-button-size*
                                         (d* 0.5 *small-square-button-size*))
                                     0.25 +coma-texture-name+)
    :initarg  :s-faint
    :accessor s-faint)
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
   (s-immune-faint
    :initform (make-health-condition (d* 4.0 *small-square-button-size*)
                                     (d+ *small-square-button-size*
                                         (d* 0.5 *small-square-button-size*))
                                     0.25 +immune-coma-texture-name+)
    :initarg  :s-immune-faint
    :accessor s-immune-faint)
   (s-immune-poisoned
    :initform (make-health-condition (d+ (d* 4.0 *small-square-button-size*)
                                         (d* 0.5 *small-square-button-size*))
                                     (d+ *small-square-button-size*
                                         (d* 0.5 *small-square-button-size*))
                                     0.25 +immune-poison-texture-name+)
    :initarg  :s-immune-poisoned
    :accessor s-immune-poisoned)
   (s-immune-terrorized
    :initform (make-health-condition (d* 4.0 *small-square-button-size*)
                                     *small-square-button-size*
                                     0.25 +immune-terror-texture-name+)
    :initarg  :s-immune-terrorized
    :accessor s-immune-terrorized)
   (s-immune-berserk
    :initform (make-health-condition (d+ (d* 4.0 *small-square-button-size*)
                                         (d* 0.5 *small-square-button-size*))
                                     *small-square-button-size*
                                     0.25 +immune-berserk-texture-name+)
    :initarg  :s-immune-berserk
    :accessor s-immune-berserk)
   (b-portrait
    :initform (make-instance 'naked-button
                             :x (d* 5.0 *small-square-button-size*)
                             :y *small-square-button-size*
                             :width  *small-square-button-size*
                             :height *small-square-button-size*
                             :texture-object  (get-texture +preview-unknown-texture-name+)
                             :texture-pressed (get-texture +preview-unknown-texture-name+)
                             :texture-overlay (get-texture +transparent-texture-name+)
                             :callback        #'toolbar-open-inventory-cb)
   :initarg  :b-portrait
   :accessor b-portrait)
   (b-attack-short
    :initform (make-square-button (d* 3.0 *square-button-size*)
                                  *small-square-button-size*
                                  +attack-short-range-overlay-texture-name+
                                  #'toolbar-attack-short-range-cb
                                  :small t
                                  :modal t)
    :initarg  :b-attack-short
    :accessor b-attack-short)
   (b-attack-long
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
                                       *small-square-button-size*)
                                  *small-square-button-size*
                                  +attack-long-range-overlay-texture-name+
                                  #'toolbar-attack-long-range-cb
                                  :small t
                                  :modal t)
    :initarg  :b-attack-long
    :accessor b-attack-long)
   (b-attack-long-imprecise
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
                                      (d* 2.0 *small-square-button-size*))
                                  *small-square-button-size*
                                  +attack-long-range-imprecise-overlay-texture-name+
                                  #'toolbar-attack-long-range-imprecise-cb
                                  :small t
                                  :modal t)
    :initarg  :b-attack-long-imprecise
    :accessor b-attack-long-imprecise)
   (b-activation
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
                                      (d* 3.0 *small-square-button-size*))
                                  *small-square-button-size*
                                  +activation-overlay-texture-name+
                                  #'toolbar-activation-cb
                                  :small t
                                  :modal t)
    :initarg  :b-activation
    :accessor b-activation)
   (b-move
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
                                      (d* 4.0 *small-square-button-size*))
                                  *small-square-button-size*
                                  +move-overlay-texture-name+
                                  #'toolbar-move-cb
                                  :small t
                                  :modal t)
    :initarg  :b-move
    :accessor b-move)

   (text-communication
    :initform (make-instance 'widget:static-text
                             :height    *square-button-size*
                             :width     (d* *small-square-button-size* 5.0)
                             :x         (d+ (d+ *square-button-size*
                                                *small-square-button-size*)
                                            (d* *small-square-button-size* 12.0))
                             :y         0.0
                             :font-size (d* 0.1 *square-button-size*)
                             :label
                             #+debug-gui
                             "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
                             #-debug-gui ""
                             :justified t)
    :initarg  :text-communication
    :accessor text-communication)
   ;; second row
   (b-save
    :initform (make-square-button 0.0
                                  *small-square-button-size*
                                  +save-overlay-texture-name+
                                  #'toolbar-save-game-cb
                                  :small t
                                  :modal t)
    :initarg :b-save
    :accessor b-save)
   (b-load
    :initform (make-square-button *small-square-button-size* *small-square-button-size*
                                  +load-overlay-texture-name+
                                  #'toolbar-load-game-cb
                                  :small t
                                  :modal nil)
    :initarg :b-load
    :accessor b-load)
   (b-options
    :initform (make-square-button 0.0 0.0
                                  +option-overlay-texture-name+
                                  #'toolbar-open-configuration-window
                                  :small t
                                  :modal t)
    :initarg :b-options
    :accessor b-options)
   (b-quit
    :initform (make-square-button *small-square-button-size* 0.0
                                  +quit-overlay-texture-name+
                                  #'toolbar-quit-cb
                                  :small t
                                  :modal nil)
    :initarg :b-quit
    :accessor b-quit)
   (b-next
    :initform (make-square-button (d* 2.0 *small-square-button-size*) *small-square-button-size*
                                  +next-overlay-texture-name+
                                  (make-change-character-cb
                                   #'keyboard-world-navigation:select-next-player)
                                  :small t
                                  :modal nil)
    :initarg :b-next
    :accessor b-next)
   (b-previous
    :initform (make-square-button (d* 3.0 *small-square-button-size*) *small-square-button-size*
                                  +previous-overlay-texture-name+
                                  (make-change-character-cb
                                   #'keyboard-world-navigation:select-previous-player)
                                  :small t
                                  :modal nil)
    :initarg :b-previous
    :accessor b-previous)
   (b-next-turn
    :initform (make-rect-button   *square-button-size*
                                  0.0
                                  1.0 0.5
                                  +next-turn-overlay-texture-name+
                                  #'next-turn-cb
                                  :modal t)
    :initarg :b-next-turn
    :accessor b-next-turn)
   (b-rotate-cw
    :initform (make-square-button (d* 4.0 *small-square-button-size*)
                                  0.0
                                  +rotate-char-cw-overlay-texture-name+
                                  #'rotate-cw-cb
                                  :small t
                                  :modal t)
    :initarg :b-rotate-cw
    :accessor b-rotate-cw)
   (b-rotate-ccw
    :initform (make-square-button (d* 5.0 *small-square-button-size*)
                                  0.0
                                  +rotate-char-ccw-overlay-texture-name+
                                  #'rotate-ccw-cb
                                  :small t
                                  :modal t)
    :initarg :b-rotate-ccw
    :accessor b-rotate-ccw)
   (b-spell
    :initform (make-square-button (d* 3.0 *square-button-size*)

                                  0.0
                                  +magic-staff-overlay-texture-name+
                                  #'toolbar-launch-spell-cb
                                  :small t
                                  :modal t)
    :initarg  :b-spell
    :accessor b-spell)
   (b-open
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
                                      *small-square-button-size*)
                                  0.0
                                  +open-overlay-texture-name+
                                  #'open-cb
                                  :small t
                                  :modal t)
    :initarg  :b-open
    :accessor b-open)
   (b-close
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
                                      (d* 2.0 *small-square-button-size*))
                                  0.0
                                  +close-overlay-texture-name+
                                  #'close-cb
                                  :small t
                                  :modal t)
    :initarg  :b-close
    :accessor b-close)
   (b-zoom
    :initform (make-square-button  (d+ (d* 3.0 *square-button-size*)
                                       (d* 3.0 *small-square-button-size*))
                                   0.0
                                   +zoom-overlay-texture-name+
                                   #'toolbar-zoom-in-cb
                                   :small t
                                   :modal nil)
     :initarg :b-zoom
     :accessor b-zoom)
   (b-unzoom
    :initform (make-square-button (d+ (d* 3.0 *square-button-size*)
                                      (d* 4.0 *small-square-button-size*))
                                  0.0
                                  +unzoom-overlay-texture-name+
                                  #'toolbar-zoom-out-cb
                                  :small t
                                  :modal nil)
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
                             :height (d/ *square-button-size* 8.0)
                             :width  *small-square-button-size*
                             :x (d+ (d+ *square-button-size* *small-square-button-size*)
                                    (d* *small-square-button-size* 11.0))
                             :y (d* 3.0 (d/ *square-button-size* 8.0))

                             :font-size 10.0
                             :label "-"
                             :justified nil)
    :initarg  :text-mp
    :accessor text-mp)
   (text-dmg
    :initform (make-instance 'widget:static-text
                             :height (d/ *square-button-size* 8.0)
                             :width  *small-square-button-size*
                             :x (d+ (d+ *square-button-size* *small-square-button-size*)
                                    (d* *small-square-button-size* 11.0))
                             :y (d* 2.0 (d/ *square-button-size* 8.0))
                             :font-size 10.0
                             :label "-"
                             :justified nil)
    :initarg  :text-dmg
    :accessor text-dmg)
   (text-sp
    :initform (make-instance 'widget:static-text
                             :height (d/ *square-button-size* 8.0)
                             :width  *small-square-button-size*
                             :x (d+ (d+ *square-button-size* *small-square-button-size*)
                                    (d* *small-square-button-size* 11.0))
                             :y  (d/ *square-button-size* 8.0)
                             :font-size 10.0
                             :label "-"
                             :justified nil)
    :initarg  :text-sp
    :accessor text-sp)
   (icon-planner-running
    :initform (widget:make-planning-progress-icon :shown nil)
    :initarg  :icon-planner-running
    :accessor icon-planner-running)
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
  ;; other
  #+(and debug-mode debug-ai)
  (add-child object (influence-map-dump      object))
  ;; first row
  (add-child object (s-faint                 object))
  (add-child object (s-poisoned              object))
  (add-child object (s-terrorized            object))
  (add-child object (s-berserk               object))
  (add-child object (s-immune-faint          object))
  (add-child object (s-immune-poisoned       object))
  (add-child object (s-immune-terrorized     object))
  (add-child object (s-immune-berserk        object))
  (add-child object (b-portrait              object))
  (add-child object (b-attack-short          object))
  (add-child object (b-attack-long           object))
  (add-child object (b-attack-long-imprecise object))
  (add-child object (b-activation            object))
  (add-child object (b-move                  object))
  (add-child object (text-communication      object))
  ;; second row
  (add-child object (b-save       object))
  (add-child object (b-load       object))
  (add-child object (b-options    object))
  (add-child object (b-quit       object))
  (add-child object (b-next       object))
  (add-child object (b-previous   object))
  (add-child object (b-next-turn  object))
  (add-child object (b-rotate-cw  object))
  (add-child object (b-rotate-ccw object))
  (add-child object (b-spell      object))
  (add-child object (b-open       object))
  (add-child object (b-close      object))
  (add-child object (bar-mp       object))
  (add-child object (bar-dmg      object))
  (add-child object (bar-sp       object))
  (add-child object (text-mp      object))
  (add-child object (text-dmg     object))
  (add-child object (text-sp      object))
  (add-child object (b-zoom       object))
  (add-child object (b-unzoom     object))
  ;; spinners
  (add-child object (icon-planner-running object))
  ;; debug
  (add-child object (text-fps      object)))

(defmethod destroy :after ((object main-toolbar))
   (setf (bound-player object) nil))

(defgeneric sync-with-player (object &key reset-health-animation))

(defgeneric sync-influence-map (object map))

(defgeneric reset-toolbar-selected-action (object))

(defgeneric activate-planner-icon (object))

(defgeneric deactivate-planner-icon (object))

(defun sync-bar-with-player (bar bar-label value-fn current-value-fn ghost)
  (let* ((slot-value          (funcall value-fn ghost))
         (current-slots-value (funcall current-value-fn ghost))
         (new-fill-level      (if (> slot-value 0)
                                  (d (/ current-slots-value slot-value))
                                  0.0)))
    (setf (fill-level bar)       new-fill-level)
    (setf (label      bar-label) (format nil
                                         +standard-float-print-format+
                                         current-slots-value))))

(defmethod sync-with-player ((object main-toolbar) &key (reset-health-animation nil))
  (with-accessors ((bound-player bound-player)
                   (bar-mp bar-mp)      (text-mp text-mp)
                   (bar-dmg bar-dmg)    (text-dmg text-dmg)
                   (bar-sp bar-sp)      (text-sp text-sp)
                   (s-faint             s-faint)
                   (s-poisoned          s-poisoned)
                   (s-terrorized        s-terrorized)
                   (s-berserk           s-berserk)
                   (s-immune-faint      s-immune-faint)
                   (s-immune-poisoned   s-immune-poisoned)
                   (s-immune-terrorized s-immune-terrorized)
                   (s-immune-berserk    s-immune-berserk)
                   (b-portrait          b-portrait)) object
    (when bound-player
      (with-accessors ((ghost ghost)) bound-player
        (sync-bar-with-player bar-mp
                              text-mp
                              #'actual-movement-points
                              #'current-movement-points ghost)
        (sync-bar-with-player bar-dmg
                              text-dmg
                              #'actual-damage-points
                              #'current-damage-points ghost)
        (sync-bar-with-player bar-sp
                              text-sp
                              #'actual-spell-points
                              #'current-spell-points ghost)
        (case (status ghost)
          (:faint
           (setf (button-state s-faint)      t)
           (when reset-health-animation
             (setf (el-time s-faint) 0.0))
           (setf (button-state s-poisoned)   nil)
           (setf (button-state s-terrorized) nil)
           (setf (button-state s-berserk)    nil))
          (:poisoned
           (setf (button-state s-poisoned)   t)
           (when reset-health-animation
             (setf (el-time s-poisoned) 0.0))
           (setf (button-state s-faint)      nil)
           (setf (button-state s-terrorized) nil)
           (setf (button-state s-berserk)    nil))
          (:terror
           (setf (button-state s-terrorized) t)
           (when reset-health-animation
             (setf (el-time s-terrorized) 0.0))
           (setf (button-state s-faint)      nil)
           (setf (button-state s-poisoned)   nil)
           (setf (button-state s-berserk)    nil))
          (:berserk
           (setf (button-state s-berserk)    t)
           (when reset-health-animation
             (setf (el-time s-berserk) 0.0))
           (setf (button-state s-faint)      nil)
           (setf (button-state s-poisoned)   nil)
           (setf (button-state s-terrorized) nil))
          (t
           (setf (button-state s-berserk)    nil)
           (setf (button-state s-faint)      nil)
           (setf (button-state s-poisoned)   nil)
           (setf (button-state s-terrorized) nil)))
        (cond
          ((immune-faint-status ghost)
           (setf (button-state s-immune-faint) t)
           (when reset-health-animation
             (setf (el-time s-immune-faint) 0.0))
           (setf (button-state s-immune-poisoned)   nil)
           (setf (button-state s-immune-terrorized) nil)
           (setf (button-state s-immune-berserk)    nil))
          ((immune-terror-status ghost)
           (setf (button-state s-immune-terrorized) t)
           (when reset-health-animation
             (setf (el-time s-immune-terrorized) 0.0))
           (setf (button-state s-immune-faint)      nil)
           (setf (button-state s-immune-poisoned)   nil)
           (setf (button-state s-immune-berserk)    nil))
          ((immune-berserk-status ghost)
           (setf (button-state s-immune-berserk)    t)
           (when reset-health-animation
             (setf (el-time s-immune-berserk) 0.0))
           (setf (button-state s-immune-faint)      nil)
           (setf (button-state s-immune-poisoned)   nil)
           (setf (button-state s-immune-terrorized) nil))
          ((immune-poison-status ghost)
           (setf (button-state s-immune-poisoned)   t)
           (when reset-health-animation
             (setf (el-time s-immune-poisoned) 0.0))
           (setf (button-state s-immune-faint)      nil)
           (setf (button-state s-immune-terrorized) nil)
           (setf (button-state s-immune-berserk)    nil))
          (t
           (setf (button-state s-immune-poisoned)   nil)
           (setf (button-state s-immune-faint)      nil)
           (setf (button-state s-immune-terrorized) nil)
           (setf (button-state s-immune-berserk)    nil)))
        (setf (texture-pressed b-portrait) (portrait ghost)
              (texture-object  b-portrait) (portrait ghost)
              (current-texture b-portrait) (portrait ghost))))))

(defmethod sync-influence-map ((object main-toolbar) (map pixmap:pixmap))
  (let ((texture    (get-texture +influence-map+))
        (new-pixmap (matrix:scale-matrix-nearest map
                                                 (d/ (d texture:+influence-map-w+)
                                                     (d (pixmap:width  map)))
                                                 (d/ (d texture:+influence-map-h+)
                                                     (d (pixmap:height map))))))
    (setf (pixmap:data texture) (pixmap:data new-pixmap))
    (pixmap:sync-data-to-bits texture)
    (update-for-rendering texture)))

(defmethod reset-toolbar-selected-action ((object main-toolbar))
  (setf (selected-action object) nil))

(defmethod activate-planner-icon ((object main-toolbar))
  (with-accessors ((icon-planner-running icon-planner-running)) object
    (setf (shown icon-planner-running) t)))

(defmethod deactivate-planner-icon ((object main-toolbar))
  (with-accessors ((icon-planner-running icon-planner-running)) object
    (setf (shown icon-planner-running) nil)))

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
                   (new-capital (if plus
                                    (d- capital +exp-capital-delta+)
                                    (d+ capital +exp-capital-delta+)))
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
  (adjust-window-w (d* (d *window-w*) 0.75)))

(defun pgen-window-h ()
  (adjust-window-h (d* (d *window-h*) 0.65)))

(defun pgen-chk-button-w ()
  (d/ (d *window-w*) 9.0))

(defun pgen-button-h ()
  (d/ (d *window-w*) 35.0))

(defun pgen-chk-button-y (row)
  (d+ (h2-font-size *reference-sizes*)
      (d* (d row) (checkbutton-h *reference-sizes*))))

(defun pgen-label-ability-w ()
  (d* 0.8 (pgen-window-w)))

(defun pgen-label-ability-h ()
  (d* 0.035 (pgen-window-h)))

(defun pgen-label-ability-x ()
  (d+ (spacing *reference-sizes*)
      (pgen-chk-button-w)))

(defun pgen-label-ability-y (row)
  (d+ (d* 1.8 +portrait-size+)
      (d* (d row)
          (pgen-label-ability-h))))

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

(defun pgen-preview-x ()
  (d+ (pgen-characteristics-x)
      (input-text-w *reference-sizes*)))

(defun pgen-characteristics-y (row)
  (d* (d row)
      (d+ (spacing *reference-sizes*)
          (input-text-h *reference-sizes*))))

(defun pgen-text-entry-h ()
  (pgen-label-ability-h))

(defun rotate-preview-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((model-preview-paths model-preview-paths)) win
      (when model-preview-paths
        (setf model-preview-paths (alexandria:rotate model-preview-paths 1))
        (let ((new-preview (pixmap:slurp-pixmap 'pixmap:tga
                                                (res:get-resource-file (elt model-preview-paths 0)
                                                                       +human-player-models-resource+
                                                                       :if-does-not-exists :error)))
              (texture     (get-texture +preview-unknown-texture-name+)))
          (setf (pixmap:data texture) (pixmap:data new-preview))
          (pixmap:sync-data-to-bits texture)
          (update-for-rendering texture)))
      t)))

(defun update-preview-class-cb (type)
  #'(lambda (button event)
      (declare (ignore event))
      (with-parent-widget (check-button) button
        (with-parent-widget (win) check-button
          (with-accessors ((checkb-male checkb-male) (checkb-female checkb-female)
                           (model-preview-paths model-preview-paths)) win
            (setf model-preview-paths
                  (if (button-state checkb-female)
                      (previews-path type :female)
                      (previews-path type :male)))
            t)))))

(defun update-preview-gender-cb (gender)
  #'(lambda (button event)
      (declare (ignore event))
      (with-parent-widget (check-button) button
        (with-parent-widget (win) check-button
          (with-accessors ((checkb-warrior checkb-warrior)
                           (checkb-wizard  checkb-wizard)
                           (checkb-healer  checkb-healer)
                           (checkb-archer  checkb-archer)
                           (checkb-ranger  checkb-ranger)
                           (model-preview-paths model-preview-paths)) win
            (setf model-preview-paths
                  (cond
                    ((button-state checkb-warrior)
                     (previews-path :warrior gender))
                    ((button-state checkb-wizard)
                     (previews-path :wizard  gender))
                    ((button-state checkb-healer)
                     (previews-path :healer  gender))
                    ((button-state checkb-archer)
                     (previews-path :archer  gender))
                    ((button-state checkb-ranger)
                     (previews-path :ranger  gender))))
            t)))))

(defun previews-path (type gender)
  (let ((re (text-utils:strcat
             (ecase type
               (:warrior
                +model-preview-warrior-re+)
               (:archer
                +model-preview-archer-re+)
               (:wizard
                +model-preview-wizard-re+)
               (:healer
                +model-preview-healer-re+)
               (:ranger
                +model-preview-ranger-re+))
             (ecase gender
               (:male
                "-male")
               (:female
                "-female"))
             +model-preview-ext-re+)))
    (mapcar #'(lambda (a)
                (res:strip-off-resource-path +human-player-models-resource+ a))
            (fs:search-matching-file (res:get-resource-file ""
                                                            +human-player-models-resource+)
                                     :name re))))

(defun pad-ability-label (l max)
  (setf (prefix l)
        (right-padding (prefix l) max)))

(defclass player-generator (window)
  ((world
    :initform nil
    :initarg  :world
    :accessor world)
   (max-player-count
    :initform 10
    :initarg  :max-player-count
    :accessor max-player-count)
   (player
    :initform  nil
    :initarg  :player
    :accessor player)
   (model-preview-paths
    :initform  nil
    :initarg  :model-preview-paths
    :accessor model-preview-paths)
   (backup-data-texture-portrait
    :initform nil
    :initarg  :backup-data-texture-portrait
    :accessor backup-data-texture-portrait)
   (backup-data-texture-preview
    :initform nil
    :initarg  :backup-data-texture-preview
    :accessor backup-data-texture-preview)
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
                             :height   (checkbutton-h *reference-sizes*)
                             :width    (pgen-chk-button-w)
                             :x        0.0
                             :y        (pgen-chk-button-y 0.0)
                             :label    (_ "Warrior ")
                             :callback (update-preview-class-cb :warrior)
                             :color    :green)
    :initarg  :checkb-warrior
    :accessor checkb-warrior)
   (checkb-wizard
    :initform (make-instance 'labeled-check-button
                             :height   (checkbutton-h *reference-sizes*)
                             :width    (pgen-chk-button-w)
                             :x        0.0
                             :y        (pgen-chk-button-y 1.0)
                             :label    (_ "Wizard ")
                             :callback (update-preview-class-cb :wizard)
                             :color    :green)
    :initarg  :checkb-wizard
    :accessor checkb-wizard)
   (checkb-healer
    :initform (make-instance 'labeled-check-button
                             :height   (checkbutton-h *reference-sizes*)
                             :width    (pgen-chk-button-w)
                             :x        0.0
                             :y        (pgen-chk-button-y 2.0)
                             :label    (_ "Healer ")
                             :callback (update-preview-class-cb :healer)
                             :color    :green)
    :initarg  :checkb-healer
    :accessor checkb-healer)
   (checkb-archer
    :initform (make-instance 'labeled-check-button
                             :height (checkbutton-h *reference-sizes*)
                             :width  (pgen-chk-button-w)
                             :x 0.0
                             :y (pgen-chk-button-y 3.0)
                             :label (_ "Archer ")
                             :callback (update-preview-class-cb :archer)
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
                             :callback (update-preview-class-cb :ranger)
                             :color :green)
    :initarg  :checkb-ranger
    :accessor checkb-ranger)
   (lb-gender
    :initform (make-instance 'simple-label
                             :label     (_ "Gender")
                             :font-size (h2-font-size *reference-sizes*)
                             :width  (pgen-chk-button-w)
                             :x      0.0
                             :y      (pgen-chk-button-y 6.0))
    :initarg  :lb-gender
    :accessor lb-gender)
   (checkb-male
    :initform (make-instance 'labeled-check-button
                             :height (checkbutton-h *reference-sizes*)
                             :width  (pgen-chk-button-w)
                             :x 0.0
                             :y (pgen-chk-button-y 7.0)
                             :label (_ "Male ")
                             :callback (update-preview-gender-cb :male)
                             :color :green)
    :initarg  :checkb-male
    :accessor checkb-male)
   (checkb-female
    :initform (make-instance 'labeled-check-button
                             :height (checkbutton-h *reference-sizes*)
                             :width  (pgen-chk-button-w)
                             :x 0.0
                             :y (pgen-chk-button-y 8.0)
                             :label (_ "Female ")
                             :callback (update-preview-gender-cb :female)
                             :color :green)
    :initarg  :checkb-female
    :accessor checkb-female)
   (b-generate
    :initform (make-instance 'button
                             :height   (pgen-button-h)
                             :width    (pgen-chk-button-w)
                             :x        0.0
                             :y        (pgen-chk-button-y 10.0)
                             :callback #'generate-cb
                             :label    (_ "Generate"))
    :initarg  :b-generate
    :accessor b-generate)
   (b-save
    :initform (make-instance 'button
                             :height   (pgen-button-h)
                             :width    (pgen-chk-button-w)
                             :x        0.0
                             :y        (d+ (pgen-button-h)
                                           (pgen-chk-button-y 10.0))
                             :callback #'player-save-cb
                             :label    (_ "Save"))
    :initarg  :b-save
    :accessor b-save)
   (b-load
    :initform (make-instance 'button
                             :height   (pgen-button-h)
                             :width    (pgen-chk-button-w)
                             :x        0.0
                             :y        (d+ (d* 2.0 (pgen-button-h))
                                           (pgen-chk-button-y 10.0))
                             :callback #'player-load-cb
                             :label    (_ "Load"))
    :initarg  :b-load
    :accessor b-load)
   (b-accept
    :initform (make-instance 'button
                             :height   (pgen-button-h)
                             :width    (pgen-chk-button-w)
                             :x        0.0
                             :y        (d+ (d* 3.0 (pgen-button-h))
                                           (pgen-chk-button-y 10.0))
                             :callback #'player-accept-cb
                             :label    (_ "Accept"))
    :initarg  :b-accept
    :accessor b-accept)
   (img-portrait
    :initform (make-instance 'signalling-light
                             :width         +portrait-size+
                             :height        +portrait-size+
                             :x             (pgen-label-ability-x)
                             :y             0.0
                             :texture-name  +portrait-unknown-texture-name+
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
                              :height (pgen-label-ability-h)
                              :x (pgen-label-ability-x)
                              :y (pgen-label-ability-y 21.0)
                              :prefix (_ "Experience points: ")
                              :label "")
     :initarg  :lb-exp-points
     :accessor lb-exp-points)
   (input-name
    :initform (make-instance 'text-field
                             :width  (input-text-w *reference-sizes*)
                             :height (pgen-text-entry-h)
                             :x      (d+ (d* 2.0 (spacing *reference-sizes*))
                                         +portrait-size+
                                         (pgen-chk-button-w))
                             :y      0.0
                             :label  (_ "Name"))
    :initarg :input-name
    :accessor input-name)
   (input-last-name
    :initform (make-instance 'text-field
                             :width  (input-text-w *reference-sizes*)
                             :height (pgen-text-entry-h)
                             :x      (d+ (d* 2.0 (spacing *reference-sizes*))
                                         +portrait-size+
                                         (pgen-chk-button-w))
                             :y     (pgen-text-entry-h)
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
                             :x      (pgen-characteristics-x)
                             :y      (pgen-characteristics-y 5.0)
                             :prefix (_ "EM:  ")
                             :label  "")
    :initarg  :lb-empaty
    :accessor lb-empaty)
   (lb-weight
    :initform (make-instance 'simple-label-prefixed
                             :width  (input-text-w *reference-sizes*)
                             :height (input-text-h *reference-sizes*)
                             :x      (pgen-characteristics-x)
                             :y      (pgen-characteristics-y 6.0)
                             :prefix (_ "WG:  ")
                             :label  "")
    :initarg  :lb-weight
    :accessor lb-weight)
   (img-preview
    :initform (make-instance 'signalling-light
                             :width         +portrait-size+
                             :height        +portrait-size+
                             :x             (pgen-preview-x)
                             :y             0.0
                             :texture-name  +preview-unknown-texture-name+
                             :button-status t)
    :initarg :img-preview
    :accessor img-preview)
   (b-next-preview
    :initform (make-instance 'button
                             :height   (pgen-button-h)
                             :width    (pgen-chk-button-w)
                             :x        (pgen-preview-x)
                             :y        (d+ +portrait-size+
                                           (spacing *reference-sizes*))
                             :callback #'rotate-preview-cb
                             :label    (_ "Next"))
    :initarg  :b-next-preview
    :accessor b-next-preview)))

(defmethod initialize-instance :after ((object player-generator) &key &allow-other-keys)
  (with-accessors ((player player)
                   (lb-class lb-class)
                   (backup-data-texture-portrait backup-data-texture-portrait)
                   (backup-data-texture-preview backup-data-texture-preview)
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
                   (b-accept b-accept)
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
                   (lb-weight lb-weight)
                   (img-preview img-preview)
                   (b-next-preview b-next-preview)) object
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
    (add-child object b-accept)
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
    (add-child object lb-weight)
    (add-child object img-preview)
    (add-child object b-next-preview)
    (setf backup-data-texture-portrait
          (alexandria:copy-array (pixmap:data (get-texture +portrait-unknown-texture-name+))))
    (setf backup-data-texture-preview
          (alexandria:copy-array (pixmap:data (get-texture +preview-unknown-texture-name+))))))

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
        (%setup-character win
                          :new-player                    new-player
                          :force-rendering-modify-widget t))))
  t)

(defun player-save-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((input-name input-name)
                     (input-last-name input-last-name)
                     (player player)) win
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

(defun player-accept-error-message-cb (widget event)
  (declare (ignore event))
  (with-parent-widget (win) widget
    (hide win)
    (remove-child win
                  (make-instance 'message-window)
                  :key  #'identity
                  :test #'(lambda (a b)
                            (declare (ignore a))
                            (typep b 'message-window)))))

(defun player-accept-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((player                       player)
                     (max-player-count             max-player-count)
                     (img-portrait                 img-portrait)
                     (input-name                   input-name)
                     (input-last-name              input-last-name)
                     (world                        world)
                     (model-preview-paths          model-preview-paths)
                     (backup-data-texture-portrait backup-data-texture-portrait)
                     (backup-data-texture-preview  backup-data-texture-preview)) win
      (flet ((show-error-message (msg)
               (let ((error-message (make-message-box msg
                                                      (_ "Error")
                                                      :error
                                                      (cons (_ "OK")
                                                            #'player-accept-error-message-cb))))
                 (setf (compiled-shaders error-message) (compiled-shaders win))
                 (add-child win error-message))))
      (if (<= max-player-count 0)
          (show-error-message (_ "Limit of available player reached!"))
          (with-accessors ((main-state main-state)) world
            (if (null model-preview-paths)
                (show-error-message (_ "Mesh not specified"))
                (progn
                  ;; copy some new points to current
                  (setf (current-damage-points   player) (damage-points player))
                  (setf (current-movement-points player) (movement-points player))
                  (setf (current-spell-points    player) (spell-points player))
                  ;; setup model
                  (let* ((dir (strcat (fs:path-first-element (first model-preview-paths))
                                      fs:*directory-sep*))
                         (model            (md2:load-md2-player player
                                                                dir
                                                                (compiled-shaders world)
                                                                +human-player-models-resource+))
                         (original-texture (get-texture +portrait-unknown-texture-name+))
                         (portrait-texture (texture:gen-name-and-inject-in-database
                                            (texture:clone original-texture))))
                    (pixmap:sync-data-to-bits portrait-texture)
                    (texture:prepare-for-rendering portrait-texture)
                    (setf (character:first-name player) (label input-name))
                    (setf (character:last-name  player) (label input-last-name))
                    (setf (character:model-origin-dir player) dir)
                    ;;(setf (entity:ghost model) player)
                    (setf (portrait (entity:ghost model)) portrait-texture)
                    (decf max-player-count)
                    (world:build-inventory model +pc-type+ (character:player-class player))
                    (world:place-player-on-map world model game-state:+pc-type+ ;#(61 109)))
                                               :position #(0 0)))
                  ;; restore preview
                  (setf (pixmap:data (get-texture +preview-unknown-texture-name+))
                        backup-data-texture-preview)
                  (pixmap:sync-data-to-bits (get-texture +preview-unknown-texture-name+))
                  (setf (pixmap:data (get-texture +portrait-unknown-texture-name+))
                        backup-data-texture-portrait)
                  (pixmap:sync-data-to-bits (get-texture +portrait-unknown-texture-name+))
                  (update-for-rendering (get-texture +portrait-unknown-texture-name+))
                  (update-for-rendering (get-texture +preview-unknown-texture-name+)))))))))
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
                   (lb-spell-ch lb-spell-ch)
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

(defun %setup-character (win &key
                               (new-player nil)
                               (force-rendering-modify-widget nil))
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
                   (lb-exp-points   lb-exp-points)
                   (b-generate      b-generate)
                   (b-save          b-save)
                   (b-load          b-load)
                   (b-accept        b-accept)
                   (b-next-preview  b-next-preview)
                   (checkb-warrior  checkb-warrior)
                   (checkb-wizard   checkb-wizard)
                   (checkb-healer   checkb-healer)
                   (checkb-archer   checkb-archer)
                   (checkb-ranger   checkb-ranger)
                   (checkb-male     checkb-male)
                   (checkb-female   checkb-female)
                   (lb-gender       lb-gender)
                   (lb-class        lb-class)
                   (img-preview     img-preview)
                   (close-button    close-button)
                   (player          player)) win
    (setup-player-character win :from-player new-player)
    (if new-player
        (progn
          (setf (label input-name)      (character:first-name new-player))
          (setf (label input-last-name) (character:last-name new-player))
          (when (not force-rendering-modify-widget)
            (remove-child-if win #'(lambda (a) (find a (list b-generate
                                                             b-save
                                                             b-load
                                                             b-accept
                                                             b-next-preview
                                                             checkb-warrior
                                                             checkb-wizard
                                                             checkb-healer
                                                             checkb-archer
                                                             checkb-ranger
                                                             checkb-male
                                                             checkb-female
                                                             lb-gender
                                                             lb-class
                                                             img-preview)))))
          (setf (callback close-button)
                #'(lambda (widget event)
                    (setf (character:exp-points new-player) 0.0)
                    (hide-and-remove-grandparent-cb widget event))))
        (progn
          (random-names:load-db +random-first-names-filename+)
          (setf (label input-name) (random-names:generate))
          (random-names:load-db +random-last-names-filename+)
          (setf (label input-last-name) (random-names:generate))
          (setf (character:exp-points player) interactive-entity:+starting-exp-points+)))
    (setup-portrait win :from-player new-player)
    (setf (label lb-strength)
          (format nil +standard-float-print-format+ (character:strength player)))
    (setf (label lb-stamina)
          (format nil +standard-float-print-format+ (character:stamina player)))
    (setf (label lb-dexterity)
          (format nil +standard-float-print-format+ (character:dexterity player)))
    (setf (label lb-agility)
          (format nil +standard-float-print-format+ (character:agility player)))
    (setf (label lb-smartness)
          (format nil +standard-float-print-format+ (character:smartness player)))
    (setf (label lb-empaty)
          (format nil +standard-float-print-format+ (character:empaty player)))
    (setf (label lb-weight)
          (format nil +standard-float-print-format+ (character:weight player)))
    (let ((max-length-prefix (%find-max-lenght-ability-prefix win)))
      (setf (prefix lb-damage-pt) (right-padding (prefix lb-damage-pt)
                                                 max-length-prefix)
            (label lb-damage-pt)  (format nil +standard-float-print-format+
                                          (character:damage-points player)))
      (%add-callback-to-pgen-buttons b-inc-damage-pt b-dec-damage-pt
                                     player 'damage-points
                                     lb-exp-points lb-damage-pt 0.1 1.0)
      (setf (prefix lb-movement-pt) (right-padding (prefix lb-movement-pt) max-length-prefix)
            (label  lb-movement-pt) (format nil +standard-float-print-format+ (movement-points player)))
      (%add-callback-to-pgen-buttons b-inc-movement-pt b-dec-movement-pt
                                     player 'movement-points
                                     lb-exp-points lb-movement-pt 0.5 1.0)
      (setf (prefix lb-magic-pt) (right-padding (prefix lb-magic-pt) max-length-prefix)
            (label  lb-magic-pt) (format nil +standard-float-print-format+ (spell-points player)))
      (%add-callback-to-pgen-buttons b-inc-magic-pt b-dec-magic-pt
                                     player 'spell-points
                                     lb-exp-points lb-magic-pt 0.5 1.0)
      (setf (prefix lb-dodge-ch) (right-padding (prefix lb-dodge-ch) max-length-prefix)
            (label  lb-dodge-ch) (format nil +standard-float-print-format+ (dodge-chance player)))
      (%add-callback-to-pgen-buttons b-inc-dodge-ch b-dec-dodge-ch
                                     player 'dodge-chance
                                     lb-exp-points lb-dodge-ch 0.5 1.0)
      (setf (prefix lb-melee-atk-ch) (right-padding (prefix lb-melee-atk-ch) max-length-prefix)
            (label  lb-melee-atk-ch) (format nil +standard-float-print-format+ (melee-attack-chance player)))
      (%add-callback-to-pgen-buttons b-inc-melee-atk-ch b-dec-melee-atk-ch
                                     player 'melee-attack-chance
                                     lb-exp-points lb-melee-atk-ch)
      (setf (prefix lb-range-atk-ch) (right-padding (prefix lb-range-atk-ch) max-length-prefix)
            (label  lb-range-atk-ch) (format nil +standard-float-print-format+ (range-attack-chance player)))
      (%add-callback-to-pgen-buttons b-inc-range-atk-ch b-dec-range-atk-ch
                                     player 'range-attack-chance
                                     lb-exp-points lb-range-atk-ch 0.5 1.0)
      (setf (prefix lb-melee-atk-dmg) (right-padding (prefix lb-melee-atk-dmg) max-length-prefix)
            (label  lb-melee-atk-dmg) (format nil +standard-float-print-format+
                                              (melee-attack-damage player)))
      (%add-callback-to-pgen-buttons b-inc-melee-atk-dmg b-dec-melee-atk-dmg
                                     player 'melee-attack-damage
                                     lb-exp-points lb-melee-atk-dmg 0.25 1.0)
      (setf (prefix lb-range-atk-dmg) (right-padding (prefix lb-range-atk-dmg) max-length-prefix)
            (label  lb-range-atk-dmg) (format nil +standard-float-print-format+
                                              (range-attack-damage player)))
      (%add-callback-to-pgen-buttons b-inc-range-atk-dmg b-dec-range-atk-dmg
                                     player 'range-attack-damage
                                   lb-exp-points lb-range-atk-dmg 0.25 1.0)
      (setf (prefix lb-edge-wpn-ch-bonus) (right-padding (prefix lb-edge-wpn-ch-bonus)
                                                         max-length-prefix)
            (label lb-edge-wpn-ch-bonus) (format nil +standard-float-print-format+
                                                 (edge-weapons-chance-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-edge-wpn-ch-bonus b-dec-edge-wpn-ch-bonus
                                     player 'edge-weapons-chance-bonus
                                     lb-exp-points lb-edge-wpn-ch-bonus 0.25 1.0)
      (setf (prefix lb-edge-wpn-dmg-bonus) (right-padding (prefix lb-edge-wpn-dmg-bonus)
                                                          max-length-prefix)
            (label lb-edge-wpn-dmg-bonus) (format nil +standard-float-print-format+
                                                  (edge-weapons-damage-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-edge-wpn-dmg-bonus b-dec-edge-wpn-dmg-bonus
                                     player 'edge-weapons-damage-bonus
                                     lb-exp-points lb-edge-wpn-dmg-bonus 0.25 1.0)
      (setf (prefix lb-impact-wpn-ch-bonus) (right-padding (prefix lb-impact-wpn-ch-bonus)
                                                           max-length-prefix)
            (label lb-impact-wpn-ch-bonus) (format nil +standard-float-print-format+
                                                   (impact-weapons-chance-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-impact-wpn-ch-bonus b-dec-impact-wpn-ch-bonus
                                     player 'impact-weapons-chance-bonus
                                     lb-exp-points lb-impact-wpn-ch-bonus 0.25 1.0)
      (setf (prefix lb-impact-wpn-dmg-bonus) (right-padding (prefix lb-impact-wpn-dmg-bonus)
                                                            max-length-prefix)
            (label lb-impact-wpn-dmg-bonus) (format nil +standard-float-print-format+
                                                    (impact-weapons-damage-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-impact-wpn-dmg-bonus b-dec-impact-wpn-dmg-bonus
                                     player 'impact-weapons-damage-bonus
                                     lb-exp-points lb-impact-wpn-dmg-bonus 0.25 1.0)
      (setf (prefix lb-pole-wpn-ch-bonus) (right-padding (prefix lb-pole-wpn-ch-bonus)
                                                         max-length-prefix)
            (label lb-pole-wpn-ch-bonus) (format nil +standard-float-print-format+
                                                    (pole-weapons-chance-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-pole-wpn-ch-bonus b-dec-pole-wpn-ch-bonus
                                     player 'pole-weapons-chance-bonus
                                     lb-exp-points lb-pole-wpn-ch-bonus 0.25 1.0)
      (setf (prefix lb-pole-wpn-dmg-bonus) (right-padding (prefix lb-pole-wpn-dmg-bonus)
                                                          max-length-prefix)
            (label lb-pole-wpn-dmg-bonus) (format nil +standard-float-print-format+
                                                  (pole-weapons-damage-bonus player)))
      (%add-callback-to-pgen-buttons b-inc-pole-wpn-dmg-bonus b-dec-pole-wpn-dmg-bonus
                                     player 'pole-weapons-damage-bonus
                                     lb-exp-points lb-pole-wpn-dmg-bonus 0.25 1.0)
      (setf (prefix lb-unlock-ch) (right-padding (prefix lb-unlock-ch) max-length-prefix)
            (label lb-unlock-ch) (format nil +standard-float-print-format+
                                         (unlock-chance player)))
      (%add-callback-to-pgen-buttons b-inc-unlock-ch b-dec-unlock-ch
                                     player 'unlock-chance
                                     lb-exp-points lb-unlock-ch 0.5 1.0)
      (setf (prefix lb-deactivate-trap-ch) (right-padding (prefix lb-deactivate-trap-ch)
                                                          max-length-prefix)
            (label lb-deactivate-trap-ch) (format nil +standard-float-print-format+
                                                  (deactivate-trap-chance player)))
      (%add-callback-to-pgen-buttons b-inc-deactivate-trap-ch b-dec-deactivate-trap-ch
                                     player 'deactivate-trap-chance
                                     lb-exp-points lb-deactivate-trap-ch 0.5 1.0)
      (setf (prefix lb-reply-attack-ch) (right-padding (prefix lb-reply-attack-ch)
                                                       max-length-prefix)
            (label lb-reply-attack-ch) (format nil +standard-float-print-format+
                                               (reply-attack-chance player)))
      (%add-callback-to-pgen-buttons b-inc-reply-attack-ch b-dec-reply-attack-ch
                                     player 'reply-attack-chance
                                     lb-exp-points lb-reply-attack-ch 0.33 1.0)
      (setf (prefix lb-ambush-attack-ch) (right-padding (prefix lb-ambush-attack-ch)
                                                        max-length-prefix)
            (label lb-ambush-attack-ch) (format nil +standard-float-print-format+
                                                (ambush-attack-chance player)))
      (%add-callback-to-pgen-buttons b-inc-ambush-attack-ch b-dec-ambush-attack-ch
                                     player 'ambush-attack-chance
                                     lb-exp-points lb-ambush-attack-ch 0.33 1.0)
      (setf (prefix lb-spell-ch) (right-padding (prefix lb-spell-ch) max-length-prefix)
            (label lb-spell-ch) (format nil +standard-float-print-format+
                                        (spell-chance player)))
      (%add-callback-to-pgen-buttons b-inc-spell-ch b-dec-spell-ch
                                     player 'spell-chance
                                     lb-exp-points lb-spell-ch 0.25 1.0)
      (setf (prefix lb-attack-spell-ch) (right-padding (prefix lb-attack-spell-ch)
                                                       max-length-prefix)
            (label lb-attack-spell-ch) (format nil +standard-float-print-format+
                                               (attack-spell-chance player)))
      (%add-callback-to-pgen-buttons b-inc-attack-spell-ch b-dec-attack-spell-ch
                                     player 'attack-spell-chance
                                     lb-exp-points lb-attack-spell-ch 0.25 1.0)
      (setf (prefix lb-level) (right-padding (prefix lb-level) max-length-prefix)
            (label lb-level) (format nil "~d" (level player)))
      (setf (prefix lb-exp-points) (right-padding (prefix lb-exp-points) max-length-prefix)
            (label lb-exp-points)  (format nil "~,2f" (exp-points player))))))

(defun make-player-generator (world &optional (max-player-count 10))
  (let ((window (make-instance 'player-generator
                               :max-player-count max-player-count
                               :world            world
                               :x                0.0
                               :y                (d- (d *window-h*)
                                                     (pgen-window-h))
                               :width            (pgen-window-w)
                               :height           (pgen-window-h)
                               :label            (_ "Generate character"))))
    (add-window-button-cb-hide-remove window)
    (setf (compiled-shaders window) (compiled-shaders world))
    window))

(defun message-window-w ()
  (adjust-window-w (d* 8.0 (small-square-button-size *reference-sizes*))))

(defun message-window-h ()
  (adjust-window-h (d* 2.0 (small-square-button-size *reference-sizes*))))

(defun message-window-pictogram-w ()
  (d* 0.33 (message-window-h)))

(defun message-window-pictogram-h ()
  (d* 0.33 (message-window-h)))

(defun message-window-button-h  ()
  (d* 0.30 (message-window-h)))

(defun message-window-button-w  ()
  (d* 0.125 (message-window-h)))

(defun message-window-text-x ()
  (num:add-epsilon-rel (message-window-pictogram-w)
                       0.1))

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
                             :height    (d* 0.7
                                            (message-window-h))
                             :width     (d- (message-window-w)
                                            (message-window-pictogram-w))
                             :x         (message-window-text-x)
                             :y         0.0
                             :font-size (h4-font-size *reference-sizes*)
                             :label     "test test"
                             :justified t)
    :initarg  :text-message
    :accessor text-message)))

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
        (setf (label text-message) message))
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
         (set-texture-pictogram img-pictogram type))))))

(defun make-message-box (text title type &rest buttons-callbacks)
  "Car of each element of buttons-callbacks is the label, cdr the callback function"
  (make-message-box* text title type buttons-callbacks))

(defun make-message-box* (text title type buttons-callbacks)
  "Car of each element of buttons-callbacks is the label, cdr the callback function"
  (let* ((widget        (make-instance 'message-window
                                       :type    type
                                       :label   title
                                       :message text
                                       :x       (d (- (/ *window-w* 2) (/ (message-window-w) 2.0)))
                                       :y       (d (- (/ *window-h* 2) (/ (message-window-h) 2.0)))
                                       :width   (message-window-w)
                                       :height  (message-window-h)))
         (button-w      (dmin
                         (d* 0.25 (width widget))
                         (d/ (d* 0.8 (width widget)) (d (length buttons-callbacks)))))
         (all-buttons-w (d* (d (length buttons-callbacks)) button-w))
         (buttons-start  (d- (d/ (width widget) 2.0)
                             (d/ all-buttons-w  2.0)
                             (d* (left-frame-offset *reference-sizes*)
                                 (width widget)))))
    (loop
       for b-cb in buttons-callbacks
       for x    from buttons-start by button-w do
         (add-child widget
                    (make-instance 'button
                                   :width    button-w
                                   :height   (message-window-button-h)
                                   :x        x
                                   :y        (d- (height widget) (message-window-button-h))
                                   :label    (car b-cb)
                                   :callback (cdr b-cb))))
    widget))

(defun append-error-box-to-window (win error-text)
  (with-accessors ((compiled-shaders compiled-shaders)) win
    (let* ((error-msg  (widget:make-message-box error-text
                                                (_ "Error")
                                                :error
                                                (cons (_ "OK")
                                                      #'widget:hide-and-remove-parent-cb))))
      (setf (compiled-shaders error-msg) compiled-shaders)
      (mtree:add-child win error-msg))))
