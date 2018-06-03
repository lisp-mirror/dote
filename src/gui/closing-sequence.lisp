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

(in-package :closing-sequence)

(defclass closing-curtain (signalling-light inner-animation) ())

(defmethod initialize-instance :after ((object closing-curtain) &key &allow-other-keys)
  (setf (use-blending-p object) t))

(defmethod calculate ((object closing-curtain) dt)
  (with-accessors ((animation-speed      animation-speed)
                   (el-time              el-time)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (desired-type dt el-time animation-speed))
    (setf el-time (d+ el-time (d* animation-speed dt)))))

(defmethod render ((object closing-curtain) renderer)
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
            (gl:blend-func :src-alpha :one-minus-src-alpha)
            (use-program compiled-shaders :closing-curtain)
            (gl:active-texture :texture0)
            (texture:bind-texture texture-object)
            (uniformf compiled-shaders :alpha  (smoothstep-interpolate 0.0 1.0 el-time))
            (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
            (uniform-matrix compiled-shaders :modelview-matrix 4
                            (vector (sb-cga:matrix* camera-vw-matrix
                                                    (elt view-matrix 0)
                                                    (elt model-matrix 0)))
                            nil)
            (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
            (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
            (gl:draw-arrays :triangles 0 (f* 3 (length triangles)))
            (gl:blend-equation :func-add)))))))

(defun restart-game (widget)
  (with-accessors ((state state)) widget
    (let ((render-window (game-state:fetch-render-window state)))
      (sdl2.kit-utils:close-game render-window))))

(defmethod on-mouse-pressed ((object closing-curtain) event)
  (declare (ignore event))
  (restart-game object)
  t)

(defmethod on-key-pressed ((object closing-curtain) event)
  (declare (ignore event))
  (restart-game object)
  t)

(defun make-closing (world texture-name)
  (let* ((w                (d *window-w*))
         (h                (d *window-h*))
         (compiled-shaders (world:compiled-shaders world))
         (bg               (make-instance 'closing-curtain
                                          :compiled-shaders compiled-shaders
                                          :x                0.0
                                          :y                0.0
                                          :width            w
                                          :height           h
                                          :texture-name     texture-name)))
    bg))

(defun start-closing-sequence (world texture-name)
  (mtree:add-child (world:gui world)
                   (make-closing world texture-name)))

(defun start-game-over-sequence (world)
  (mtree:add-child (world:gui world)
                   (make-closing world +game-over-texture-name+)))

(defun start-victory-sequence (world)
  (mtree:add-child (world:gui world)
                   (make-closing world +victory-texture-name+)))
