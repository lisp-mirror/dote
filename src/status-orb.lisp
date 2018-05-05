;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :status-orb)

(define-constant +orb-texture-window-width+     0.25                           :test #'= )

(define-constant +orb-size+                     (d* 0.25
                                                    +terrain-chunk-tile-size+)
  :test #'= )

(define-constant +orb-frequency-animation+      6                              :test #'= )

(define-constant +texture-active+               "orb-active.tga"
  :test #'string= )

(define-constant +texture-inactive+             "orb-inactive.tga"
  :test #'string= )

(defparameter *orb* nil)

(defun get-orb-texture (name)
  (let ((tex (texture:get-texture (res:get-resource-file name
                                                         +animation-texture-dir+))))
    (setf (texture:filename tex) name)
    (prepare-for-rendering tex)
    tex))

(defun init-textures ()
  (get-orb-texture +texture-active+)
  (get-orb-texture +texture-inactive+))

(defun init-db ()
  (init-textures)
  (let* ((active (make-instance 'status-orb
                                :texture-window-width +orb-texture-window-width+
                                :frequency-animation  +orb-frequency-animation+
                                :w                    +orb-size+
                                :h                    +orb-size+
                                :texture-object       (texture:get-texture +texture-active+))))
    (prepare-for-rendering active)
    (setf *orb* active)))

(defun clean-db ()
;;    (destroy *orb*)
  (setf *orb* nil))

(defclass status-orb (triangle-mesh inner-animation animated-spritesheet) ())

(defmethod initialize-instance :after ((object status-orb)
                                       &key
                                         (w         1.0)
                                         (h         1.0)
                                         &allow-other-keys)
  (with-accessors ((texture-window-width texture-window-width)
                   (frames-number        frames-number)
                   (frequency-animation  frequency-animation)) object
    (setf (use-blending-p object) t)
    (setf frames-number (truncate (* frequency-animation
                                     (/ 1 texture-window-width))))
    (let ((w/2 (d* w 0.5))
          (h/2 (d* h 0.5)))
      (quad object w h
                 0.0 0.0 texture-window-width 1.0
                 (vec (d- w/2) (d- h/2) 0.0) ; centering
                 nil t)
      (remove-orphaned-vertices object)
      (prepare-for-rendering object))))

(defmethod calculate ((object status-orb) dt)
  (declare (optimize (debug 0) (speed 1) (safety 0)))
  (with-accessors ((calculatep                calculatep)
                   (frequency-animation       frequency-animation)
                   (el-time                   el-time)
                   (texture-window-width      texture-window-width)
                   (animation-speed           animation-speed)
                   (frames-number             frames-number)
                   (frame-idx                 frame-idx)
                   (frame-count               frame-count)) object
    (declare (fixnum frame-count frame-idx frequency-animation))
    (declare (desired-type el-time animation-speed dt))
    (when calculatep
      (setf el-time (d+ el-time (d* dt animation-speed)))
      (incf frame-count)
      (when (= (rem frame-count frequency-animation) 0)
        (setf frame-idx (+ 1 frame-idx)))
      (bubbleup-modelmatrix object))))

(defmethod removeable-from-world-p ((object status-orb))
  nil)

(defmethod render ((object status-orb) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((duration/2           duration/2)
                   (anim-delay           anim-delay)
                   (projection-matrix    projection-matrix)
                   (compiled-shaders     compiled-shaders)
                   (el-time              el-time)
                   (gravity              gravity)
                   (texture-window-width texture-window-width)
                   (frame-idx            frame-idx)
                   (model-matrix         model-matrix)
                   (triangles            triangles)
                   (scaling              scaling)
                   (texture-object       texture-object)
                   (vao                  vao)
                   (view-matrix          view-matrix)
                   (renderp              renderp)) object
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles))
    (declare (fixnum anim-delay))
    (when renderp
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (cl-gl-utils:with-depth-disabled
            (cl-gl-utils:with-blending
              (gl:blend-func                :src-alpha :one-minus-src-alpha)
              (use-program compiled-shaders :status-orb)
              (gl:active-texture            :texture0)
              (texture:bind-texture texture-object)
              (uniformi  compiled-shaders :texture-object           +texture-unit-diffuse+)
              (uniformi  compiled-shaders :frame-idx                frame-idx)
              (uniformf  compiled-shaders :texture-window-width     texture-window-width)
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

(defclass status-orb-shell (triangle-mesh-shell inner-animation animated-spritesheet) ())

(defmethod calculate ((object status-orb-shell) dt)
  (declare (optimize (debug 0) (speed 1) (safety 0)))
  (with-accessors ((calculatep                calculatep)
                   (frequency-animation       frequency-animation)
                   (el-time                   el-time)
                   (texture-window-width      texture-window-width)
                   (animation-speed           animation-speed)
                   (frames-number             frames-number)
                   (frame-idx                 frame-idx)
                   (frame-count               frame-count)) object
    (declare (fixnum frame-count frame-idx frequency-animation))
    (declare (desired-type el-time animation-speed dt))
    (when calculatep
      (setf el-time (d+ el-time (d* dt animation-speed)))
      (incf frame-count)
      (when (= (rem frame-count frequency-animation) 0)
        (setf frame-idx (+ 1 frame-idx)))
      (bubbleup-modelmatrix object))))

(defmethod removeable-from-world-p ((object status-orb-shell))
  nil)

(defmethod render ((object status-orb-shell) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((duration/2           duration/2)
                   (anim-delay           anim-delay)
                   (projection-matrix    projection-matrix)
                   (compiled-shaders     compiled-shaders)
                   (el-time              el-time)
                   (gravity              gravity)
                   (texture-window-width texture-window-width)
                   (frame-idx            frame-idx)
                   (model-matrix         model-matrix)
                   (triangles            triangles)
                   (scaling              scaling)
                   (texture-object       texture-object)
                   (vao                  vao)
                   (view-matrix          view-matrix)
                   (renderp              renderp)) object
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles))
    (declare (fixnum anim-delay))
    (when renderp
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (cl-gl-utils:with-depth-disabled
            (cl-gl-utils:with-blending
              (gl:blend-func                :src-alpha :one-minus-src-alpha)
              (use-program compiled-shaders :status-orb)
              (gl:active-texture            :texture0)
              (texture:bind-texture texture-object)
              (uniformi  compiled-shaders :texture-object           +texture-unit-diffuse+)
              (uniformi  compiled-shaders :frame-idx                frame-idx)
              (uniformf  compiled-shaders :texture-window-width     texture-window-width)
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

(defun get-orb (pos compiled-shaders texture-name
                &optional (calculatep nil) (renderp nil))

  (let ((shell (make-instance 'status-orb-shell
                              :texture-window-width +orb-texture-window-width+
                              :texture-object       (texture:get-texture texture-name)
                              :frequency-animation  +orb-frequency-animation+)))
    (fill-mesh-data shell *orb*)
    (setf (pos              shell) pos
          (compiled-shaders shell) compiled-shaders
          (texture-object   shell) (get-orb-texture texture-name)
          (calculatep       shell) calculatep
          (renderp          shell) renderp)
    shell))

(defun orbp (a)
  (or (typep a 'status-orb)
      (typep a 'status-orb-shell)))
