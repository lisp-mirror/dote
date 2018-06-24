;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

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

(in-package :mesh)

(alexandria:define-constant +tree-clip-plane-no-clip+ (vec4:vec4 0.0 1.0 0.0
                                                                 +terrain-noclip-plane-dist+)
  :test #'vec4:vec4~)

(alexandria:define-constant +tree-clip-plane+ (vec4:vec4 0.0 -1.0 0.0 (d/ +wall-h+ 2.0))
  :test #'vec4:vec4~)

(defparameter *tree-clip-plane* (vec4:vec4 0.0 1.0 0.0 +terrain-noclip-plane-dist+))

(defun set-tree-clip ()
  (setf *tree-clip-plane* +tree-clip-plane+))

(defun unset-tree-clip ()
  (setf *tree-clip-plane* +tree-clip-plane-no-clip+))

(defun flip-tree-clip ()
  (if (vec4~ *tree-clip-plane* +tree-clip-plane-no-clip+)
      (set-tree-clip)
      (unset-tree-clip)))

(defclass tree-mesh-shell (triangle-mesh-shell) ())

(defun tree-mesh-shell-p (a)
  (typep a 'mesh:tree-mesh-shell))

(defmethod initialize-instance :after ((object tree-mesh-shell) &key &allow-other-keys)
  (setf (start-time object) (d (lcg-next-upto 5))))

(defmethod rendering-needed-p ((object tree-mesh-shell) renderer)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (world:frustum-aabb-intersects-p renderer object))

(defmethod calculate :after ((object tree-mesh-shell) dt)
  (declare (optimize (debug 0) (speed 1) (safety 0)))
  (with-accessors ((impostor impostor)) object
    (and impostor (calculate impostor dt))
    ;; we use 'rem' here to make  the 'el-time' periodic, otherwise
    ;; we will get non-fluid animation.
    ;; my best guess  is  that sin  function  in  the shader  give  wrong
    ;; results when argument is large (as it was no more periodic)
    (setf (el-time object)
          (d+ (start-time object)
              (rem (d* (animation-speed object) (current-time object))
                   100.0)))))

(defmethod render-phong ((object tree-mesh-shell) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (impostor impostor)
                   (triangles triangles)
                   (material-params material-params)
                   (el-time el-time)
                   (fog-density fog-density)
                   (animation-speed animation-speed)
                   (fading-away-fn fading-away-fn)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (declare (function fading-away-fn))
    (declare (desired-type animation-speed el-time))
    (if (use-lod-p object 2.0 renderer)
        (render impostor renderer)
        (when (> (length triangles) 0)
          (with-clip-plane
            (with-camera-view-matrix (camera-vw-matrix renderer)
              (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
                (with-no-cull-face
                  (use-program compiled-shaders :tree)
                  (gl:active-texture            :texture0)
                  (texture:bind-texture texture-object)
                  (uniformi  compiled-shaders :texture-object +texture-unit-diffuse+)
                  (uniformfv compiled-shaders :light-pos
                             (the vec (main-light-pos-eye-space renderer)))
                  (uniformfv compiled-shaders :ia    #(1.0 1.0 1.0))
                  (uniformfv compiled-shaders :id    (the vec (main-light-color renderer)))
                  (uniformfv compiled-shaders :is    (the vec (main-light-color renderer)))
                  (uniformfv compiled-shaders :clip-plane (the vec4 *tree-clip-plane*))
                  (uniformf  compiled-shaders :ka    (ka material-params))
                  (uniformf  compiled-shaders :kd    (kd material-params))
                  (uniformf  compiled-shaders :ks    (ks material-params))
                  (uniformf  compiled-shaders :shine (shininess material-params))
                  (uniformf  compiled-shaders :time  el-time)
                  (uniformf  compiled-shaders :fog-density fog-density)
                  (uniform-matrix compiled-shaders :model-matrix 4 model-matrix nil)
                  (let ((tremor-matrix (funcall fading-away-fn object animation-speed)))
                    (declare (sb-cga:matrix tremor-matrix))
                    (uniform-matrix compiled-shaders :modelview-matrix 4
                                    (vector (matrix* camera-vw-matrix
                                                     (elt view-matrix 0)
                                                     (elt model-matrix 0)
                                                     tremor-matrix))
                                    nil))
                  (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
                  (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
                  (gl:draw-arrays :triangles 0 (f* 3 (length triangles)))))))
          (render-debug object renderer)))))

(defgeneric tree-trunk-aabb (object))

(defmethod tree-trunk-aabb ((object tree-mesh-shell))
  (standard-aabb object))

(defmethod actual-aabb-for-bullets ((object tree-mesh-shell))
  (tree-trunk-aabb object))

(defmethod actual-aabb-for-visibility ((object tree-mesh-shell))
  (tree-trunk-aabb object))
