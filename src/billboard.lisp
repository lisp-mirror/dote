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

(in-package :billboard)

(define-constant +damage-color+             §cff0000ff                :test #'vec4=)

(define-constant +poison-damage-color+      §ca50db3ff                :test #'vec4=)

(define-constant +healing-color+            §c1dba12ff                :test #'vec4=)

(define-constant +blessing-color+           §ccdf7ffff                :test #'vec4=)

(define-constant +tooltip-w+                +terrain-chunk-tile-size+ :test #'=)

(define-constant +tooltip-h+                +terrain-chunk-tile-size+ :test #'=)

(define-constant +tooltip-v-speed+           0.06                     :test #'=)

(define-constant +tooltip-poison-char+          "#"                   :test #'string=)

(define-constant +tooltip-terror-char+          "%"                   :test #'string=)

(define-constant +tooltip-berserk-char+         "{"                   :test #'string=)

(define-constant +tooltip-immune-faint-char+    "\\"                  :test #'string=)

(define-constant +tooltip-immune-poison-char+   "["                   :test #'string=)

(define-constant +tooltip-immune-terror-char+   "]"                   :test #'string=)

(define-constant +tooltip-immune-berserk-char+  "^"                   :test #'string=)

(define-constant +tooltip-faint-char+           "&"                   :test #'string=)

(define-constant +tooltip-heal-char+            "\""                  :test #'string=)

(define-constant +tooltip-revive-char+          "$"                   :test #'string=)

(define-constant +tooltip-surprise-attack-char+ "???"                 :test #'string=)

(define-constant +impostor-default-size+        512                   :test #'=)

(defclass tooltip (triangle-mesh inner-animation end-life-trigger)
  ((duration/2
    :initform (lcg-next-in-range 3.0 4.0)
    :initarg  :duration/2
    :accessor duration/2)
   (gravity
    :initform (num:lcg-next-in-range 1.0 2.0)
    :initarg  :gravity
    :accessor gravity)
   (font-type
    :initform +tooltip-font-handle+
    :initarg  :font-type
    :accessor font-type)
   (font-color
    :initform +damage-color+
    :initarg  :font-color
    :accessor font-color))
  (:documentation "Note: the tooltip will add and remove itself from action-queue automatically
                   see: make instance and keyworld enqueuedp"))

(defmethod initialize-instance :after ((object tooltip)
                                       &key (enqueuedp nil) &allow-other-keys)
  (setf (use-blending-p object) t)
  (when enqueuedp
    (action-scheduler:end-of-life-remove-from-action-scheduler object
                                                               action-scheduler:tooltip-show-action)))

(defun setup-label-tooltip (host new-label)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((children children)
                    (font-type font-type)) host
     (with-slots (label) host
       (declare (simple-string label))
       (loop
          for c across new-label
          for i from   0.0  by 1.0  do
            (let* ((mesh (clone (gui:get-char-mesh font-type c))))
              (when mesh
                (transform-vertices mesh (translate (vec i 0.0 0.0)))
                (setf (texture-object host) (texture-object mesh))
                (merge-mesh host mesh :manifold nil)))))))

(defmethod (setf label) (new-label (object tooltip))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (with-accessors ((pos pos)) object
    (declare (vec pos))
    (setup-label-tooltip object new-label)
    (let ((w-tooltip/2 (d/ +tooltip-w+ 2.0)))
      (setf (scaling object) (vec (d (/ +tooltip-w+ (length new-label))) +tooltip-h+ 0.0))
      (setf (elt pos 0) (d+ (elt pos 0) w-tooltip/2)))))

(defmethod calculate ((object tooltip) dt)
  (with-accessors ((calculatep calculatep)) object
    (when calculatep
      (incf (el-time object) (d* dt (animation-speed object)))
      (bubbleup-modelmatrix object)
      (with-maybe-trigger-end-of-life (object (removeable-from-world-p object))))))

(defmethod removeable-from-world-p ((object tooltip))
  (with-accessors ((duration/2 duration/2)
                   (el-time el-time)) object
    (d> el-time (d* 2.0 duration/2))))

(defmethod render ((object tooltip) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((duration/2 duration/2)
                   (projection-matrix projection-matrix)
                   (compiled-shaders compiled-shaders)
                   (font-color font-color)
                   (el-time el-time)
                   (gravity  gravity)
                   (model-matrix model-matrix)
                   (triangles triangles)
                   (scaling scaling)
                   (texture-object texture-object)
                   (vao vao)
                   (view-matrix view-matrix)
                   (renderp renderp)) object
    (declare (vec4 font-color))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles))
    (when renderp
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (cl-gl-utils:with-depth-disabled
            (cl-gl-utils:with-blending
              (gl:blend-func                :src-alpha :one)
              (use-program compiled-shaders :tooltip)
              (gl:active-texture            :texture0)
              (texture:bind-texture texture-object)
              (uniformi  compiled-shaders :texture-object          +texture-unit-diffuse+)
              (uniformf  compiled-shaders :duration                duration/2)
              (uniformf  compiled-shaders :vert-displacement-speed +tooltip-v-speed+)
              (uniformf  compiled-shaders :time                    el-time)
              (uniformf  compiled-shaders :gravity                 gravity)
              (uniformfv compiled-shaders :mult-color              font-color)
              (uniform-matrix compiled-shaders
                              :post-scaling 4
                              (vector (scale scaling))
                              nil)
              (uniform-matrix compiled-shaders
                              :modelview-matrix 4
                              (vector (matrix* camera-vw-matrix
                                               (elt view-matrix  0)
                                               (elt model-matrix 0)))
                              nil)
              (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
              (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
              (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))))

(defgeneric activate-tooltip (object))

(defmethod activate-tooltip ((object tooltip))
  (setf (renderp    object) t
        (calculatep object) t))

(defun make-tooltip (label pos shaders
                     &key
                       (color     +damage-color+)
                       (font-type gui:+default-font-handle+)
                       (gravity   (num:lcg-next-in-range 1.0 24.0))
                       (activep   t)
                       (enqueuedp nil))
  (let ((tooltip (make-instance 'billboard:tooltip
                                :animation-speed 1.0
                                :font-color      color
                                :font-type       font-type
                                :gravity         gravity
                                :renderp         activep
                                :calculatep      activep
                                :enqueuedp       enqueuedp)))
    (setf (interfaces:compiled-shaders tooltip) shaders)
    (setf (entity:pos tooltip) pos)
    (setf (label tooltip) label)
    (mesh:prepare-for-rendering tooltip)
    tooltip))

(defgeneric apply-tooltip (object label &key
                                          color
                                          font-type
                                          gravity
                                          activep
                                          enqueuedp))

(defgeneric enqueue-tooltip (object label &key
                                            color
                                            font-type
                                            gravity
                                            additional-action-enqueued-fn
                                            activep))

(defmethod apply-tooltip ((object mesh:triangle-mesh) label
                          &key
                            (color     +damage-color+)
                            (font-type gui:+default-font-handle+)
                            (gravity   1.0)
                            (activep   t)
                            (enqueuedp nil))
  (with-accessors ((ghost ghost)
                   (id id)
                   (state state)) object
    (with-accessors ((recurrent-effects recurrent-effects)
                     (immune-poison-status immune-poison-status)
                     (status status)) ghost
      (game-state:with-world (world state)
        (let* ((mesh-pos       (aabb-top-center (aabb object)))
               (tooltip        (make-tooltip label
                                             (vec+ mesh-pos
                                                   (vec 0.0
                                                        (d* 1.5 +tooltip-h+)
                                                        0.0))
                                             (compiled-shaders object)
                                             :color      color
                                             :font-type  font-type
                                             :gravity    gravity
                                             :activep    activep
                                             :enqueuedp  enqueuedp)))
          (world:push-entity world tooltip))))))

(defmethod enqueue-tooltip ((object mesh:triangle-mesh) label
                            &key
                              (color     +damage-color+)
                              (font-type gui:+default-font-handle+)
                              (gravity   1.0)
                              (additional-action-enqueued-fn nil)
                              (activep    t))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action (world action-scheduler:tooltip-show-action)
        (apply-tooltip object label
                       :color     color
                       :font-type font-type
                       :gravity   gravity
                       :activep   activep
                       :enqueuedp t)
        (when additional-action-enqueued-fn
          (funcall additional-action-enqueued-fn))))))

(defclass animated-billboard (triangle-mesh inner-animation end-life-trigger animated-spritesheet)
  ((duration/2
    :initform (lcg-next-in-range 3.0 4.0)
    :initarg  :duration/2
    :accessor duration/2)
   (gravity
    :initform (num:lcg-next-in-range 1.0 2.0)
    :initarg  :gravity
    :accessor gravity))
  (:documentation "Note: the tooltip will add and remove itself from action-queue automatically
                   see: make instance and keyworld enqueuedp"))

(defmethod initialize-instance :after ((object animated-billboard)
                                       &key
                                         (enqueuedp nil)
                                         (w         1.0)
                                         (h         1.0)
                                         &allow-other-keys)
  (with-accessors ((texture-window-width texture-window-width)) object
    (setf (use-blending-p object) t)
    (let ((w/2 (d* w 0.5))
          (h/2 (d* h 0.5)))
      (quad object w h
                 0.0 0.0 texture-window-width 1.0
                 (vec (d- w/2) (d- h/2) 0.0) ; centering
                 nil t)
      (remove-orphaned-vertices object)
      (prepare-for-rendering object)
      (when enqueuedp
        (action-scheduler:end-of-life-remove-from-action-scheduler object
                                                                   action-scheduler:animated-billboard-show-action)))))

(defmethod calculate ((object animated-billboard) dt)
  (with-accessors ((calculatep calculatep)
                   (frequency-animation frequency-animation)
                   (el-time el-time)
                   (texture-window-width texture-window-width)
                   (texture-horizontal-offset texture-horizontal-offset)
                   (animation-loop-p animation-loop-p)
                   (frame-count frame-count)) object
    (when calculatep
      (incf (el-time object) (d* dt (animation-speed object)))
      (incf frame-count)
      (when (or animation-loop-p
                (not (animated-billboard-last-frame-reached-p object)))
        (when (= (rem frame-count frequency-animation) 0)
          (incf texture-horizontal-offset texture-window-width)))
      (bubbleup-modelmatrix object)
      (with-maybe-trigger-end-of-life (object (removeable-from-world-p object))))))

(defmethod removeable-from-world-p ((object animated-billboard))
  (with-accessors ((duration/2       duration/2)
                   (el-time          el-time)
                   (animation-loop-p animation-loop-p)) object
    (if animation-loop-p
        (d> el-time (d* 2.0 duration/2))
        (animated-billboard-last-frame-reached-p object))))

(defmethod render ((object animated-billboard) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((duration/2 duration/2)
                   (projection-matrix projection-matrix)
                   (compiled-shaders compiled-shaders)
                   (el-time el-time)
                   (gravity  gravity)
                   (texture-horizontal-offset texture-horizontal-offset)
                   (model-matrix model-matrix)
                   (triangles triangles)
                   (scaling scaling)
                   (texture-object texture-object)
                   (vao vao)
                   (view-matrix view-matrix)
                   (renderp renderp)) object
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles))
    (when renderp
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (cl-gl-utils:with-depth-disabled
            (cl-gl-utils:with-blending
              (gl:blend-func                :src-alpha :one-minus-src-alpha)
              (use-program compiled-shaders :animated-billboard)
              (gl:active-texture            :texture0)
              (texture:bind-texture texture-object)
              (uniformi  compiled-shaders :texture-object            +texture-unit-diffuse+)
              (uniformf  compiled-shaders :duration                  duration/2)
              (uniformf  compiled-shaders :vert-displacement-speed   +tooltip-v-speed+)
              (uniformf  compiled-shaders :time                      el-time)
              (uniformf  compiled-shaders :gravity                   gravity)
              (uniformf  compiled-shaders :texture-horizontal-offset texture-horizontal-offset)
              (uniform-matrix compiled-shaders
                              :post-scaling 4
                              (vector (scale scaling))
                              nil)
              (uniform-matrix compiled-shaders
                              :modelview-matrix 4
                              (vector (matrix* camera-vw-matrix
                                               (elt view-matrix  0)
                                               (elt model-matrix 0)))
                              nil)
              (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
              (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
              (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))))

(defun make-animated-billboard (pos shaders texture-name
                              &key
                                (duration/2                .0001)
                                (w                         (d* 2.0 +terrain-chunk-tile-size+))
                                (h                         (d* 2.0 +terrain-chunk-tile-size+))
                                (loop-p                    nil)
                                (frequency-animation       5)
                                (texture-horizontal-offset 0.125)
                                (gravity                   0.0)
                                (activep                   t)
                                (enqueuedp                 nil))
  (let ((tooltip (make-instance 'animated-billboard
                                :duration/2                duration/2
                                :w                         w
                                :h                         h
                                :animation-loop-p          loop-p
                                :animation-speed           1.0
                                :frequency-animation       frequency-animation
                                :texture-horizontal-offset texture-horizontal-offset
                                :texture-window-width      texture-horizontal-offset
                                :gravity                   gravity
                                :renderp                   activep
                                :calculatep                activep
                                :enqueuedp                 enqueuedp)))
    (setf (interfaces:compiled-shaders tooltip) shaders)
    (setf (entity:pos tooltip) pos)
    (multiple-value-bind (texture errors)
        (texture:get-texture texture-name)
      (if (not errors)
          (progn
            (setf (texture-object tooltip) texture)
            (texture:prepare-for-rendering (mesh:texture-object tooltip)))
          (error 'conditions:invalid-texture
                 :text (format nil
                               "Can not load texture ~a"
                               texture-name))))
    (mesh:prepare-for-rendering tooltip)
    tooltip))

(defun enqueue-animated-billboard (pos texture-name game-state shaders
                                   &key
                                     (duration/2                .0001)
                                     (w                         (d* 2.0
                                                                    +terrain-chunk-tile-size+))
                                     (h                         (d* 2.0
                                                                    +terrain-chunk-tile-size+))
                                     (loop-p                    nil)
                                     (frequency-animation       5)
                                     (texture-horizontal-offset 0.125)
                                     (gravity                   0.0)
                                     (activep                   t)
                                     (enqueuedp                 t)
                                     (additional-action-enqueued-fn nil))
  (game-state:with-world (world game-state)
    (action-scheduler:with-enqueue-action (world
                                           action-scheduler:animated-billboard-show-action)
      (let ((billboard (make-animated-billboard pos
                                                shaders
                                                texture-name
                                                :duration/2                duration/2
                                                :w                         w
                                                :h                         h
                                                :loop-p                    loop-p
                                                :frequency-animation       frequency-animation
                                                :texture-horizontal-offset
                                                texture-horizontal-offset
                                                :gravity                   gravity
                                                :activep                   activep
                                                :enqueuedp                 enqueuedp)))
        (world:push-entity world billboard)
        (when additional-action-enqueued-fn
          (funcall additional-action-enqueued-fn))))))

(defclass tree-impostor-shell (triangle-mesh-shell) ())

(defmethod initialize-instance :after ((object tree-impostor-shell) &key &allow-other-keys)
  (setf (start-time object) (d (lcg-next-upto 5))))

(defmethod calculate :after ((object tree-impostor-shell) dt)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (ignore dt))
  (setf (el-time object)
        (d+ (start-time object)
            (d* (animation-speed object) (current-time object)))))

(defmethod render ((object tree-impostor-shell) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((projection-matrix projection-matrix)
                   (compiled-shaders compiled-shaders)
                   (font-color font-color)
                   (el-time el-time)
                   (gravity  gravity)
                   (model-matrix model-matrix)
                   (triangles triangles)
                   (scaling scaling)
                   (texture-object texture-object)
                   (vao vao)
                   (view-matrix view-matrix)) object
    (declare (vec4 font-color))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles))
    (with-camera-view-matrix (camera-vw-matrix renderer)
      (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
        (use-program compiled-shaders :tree-impostor)
        (gl:active-texture :texture0)
        (texture:bind-texture texture-object)
        (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
        (uniform-matrix compiled-shaders
                        :modelview-matrix 4
                        (vector (matrix* camera-vw-matrix
                                         (elt view-matrix  0)
                                         (elt model-matrix 0)))
                        nil)
        (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
        (uniformf  compiled-shaders :time  el-time)
        (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
        (gl:draw-arrays :triangles 0 (* 3 (length triangles)))))))

(defun make-impostor-pixmap (renderer mesh
                             &key (w +impostor-default-size+) (h  +impostor-default-size+))
  ;; render to texture
  (let ((pixmap (cl-gl-utils:with-render-to-pixmap (w h)
                  (cl-gl-utils:with-no-cull-face
                    (gl:clear-color 0 0 0 1)
                    (gl:clear :color-buffer)
                    (gl:clear :depth-buffer)
                    (interfaces:calculate mesh 0.0)
                    (interfaces:render mesh renderer)
                    (gl:viewport 0.0 0.0 *window-w* *window-h*)))))
    ;; set alpha to zero where pixel color is black
    (matrix:ploop-matrix (pixmap x y)
      (let*  ((px (matrix:pixel@ pixmap x y))
              (rgb (vector (elt px 0) (elt px 1) (elt px 2))))
        (when (num:with-epsilon (1)
                (uivec:uivec~ rgb #(0 0 0)))
          ;; set alpha to zero
          (setf (elt (matrix:pixel@ pixmap x y) 3) 0))))
    ;; remove transparent color
    (setf pixmap (matrix:clip-to-bounding-box pixmap))
    (pixmap:sync-data-to-bits pixmap)
    pixmap))

(defun make-impostor-texture (renderer mesh
                              &key (w +impostor-default-size+) (h  +impostor-default-size+))
  (let ((texture (gen-name-and-inject-in-database (make-impostor-pixmap renderer
                                                                        mesh
                                                                        :w w
                                                                        :h h))))
    (setf (texture:use-mipmap texture) nil)
    (setf (texture:interpolation-type texture) :nearest)
    (prepare-for-rendering texture)
    texture))

(defun make-impostor-mesh (aabb texture)
  (let ((mesh (make-instance 'triangle-mesh))
        (w    (aabb-width aabb))
        (h    (aabb-height aabb)))
    (quad mesh w h 0.0 0.0 1.0 1.0 +zero-vec+ nil t)
    (transform-vertices mesh (translate (vec (d- (d/ w 2.0)) 0.0 0.0)))
    (setf (texture-object mesh) texture)
    mesh))
