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

(alexandria:define-constant +gauge-texture+ "splash-progress-gauge.tga" :test #' string=)

(defclass progress-gauge (widget)
  ((progress
    :initform (d 0.9)
    :initarg  :progress
    :accessor progress)))

(defmethod initialize-instance :after ((object progress-gauge) &key &allow-other-keys)
  (with-accessors ((width width) (height height)
                   (label-font-size label-font-size)) object
    (add-quad-for-widget object)
    (transform-vertices object (sb-cga:scale* (width object) (height object) 1.0))
    (prepare-for-rendering object)))

(defmethod render ((object progress-gauge) renderer)
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
                   (progress progress)
                   (shown shown)) object
    (declare (desired-type progress))
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when shown
      (when (> (length triangles) 0)
        (with-camera-view-matrix (camera-vw-matrix renderer)
          (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
            (use-program compiled-shaders :gui-splash-progress)
            (gl:active-texture :texture0)
            (texture:bind-texture texture-object)
            (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
            (uniformf compiled-shaders :progress (d progress))
            (uniform-matrix compiled-shaders :modelview-matrix 4
                                     (vector (sb-cga:matrix* camera-vw-matrix
                                                             (elt view-matrix 0)
                                                             (elt model-matrix 0)))
                                     nil)
            (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
            (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
            (gl:draw-arrays :triangles 0 (* 3 (length triangles)))))))))

(defun make-splash-progress-gauge (&optional (progress 0.0))
  (let ((instance (make-instance 'progress-gauge
                                 :width    (d *window-w*)
                                 :height   (d *window-h*)
                                 :progress progress))
        (gauge-texture  (texture:get-texture (res:get-resource-file +gauge-texture+
                                                              +default-gui-resource+
                                                              :if-does-not-exists :error))))
    (texture:prepare-for-rendering gauge-texture)
    (setf (texture-object instance) gauge-texture)
    instance))
