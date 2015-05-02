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

(in-package :building-floor-mesh)

(defclass building-floor-mesh (pickable-mesh)
  ((texture-coord-scaling
    :initform 1.0
    :initarg :texture-coord-scaling
    :accessor texture-coord-scaling)))

(defgeneric setup-texture-coord-scaling (object))

(defmethod setup-texture-coord-scaling ((object building-floor-mesh))
  (bubbleup-modelmatrix object)
  (let* ((aabb  (aabb object))
	 (min-x (min-x aabb))
	 (max-x (max-x aabb)))
    (setf (texture-coord-scaling object)
	  (d/ (d- max-x min-x) (d* 5.0 +terrain-chunk-tile-size+)))))

(defmethod render-phong ((object building-floor-mesh) renderer)
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
		   (texture-coord-scaling texture-coord-scaling)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (declare (desired-type texture-coord-scaling))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
	(with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	  (use-program compiled-shaders :mesh-ads)
	  (gl:active-texture :texture0)
	  (texture:bind-texture texture-object)
	  (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	  (uniformfv compiled-shaders
		     :light-pos
		     (the vec (main-light-pos-eye-space renderer)))
	  (uniformf  compiled-shaders :scale-text-coord texture-coord-scaling)
	  (uniformfv compiled-shaders :ia    #(1.0 1.0 1.0))
	  (uniformfv compiled-shaders :id    (the vec (main-light-color renderer)))
	  (uniformfv compiled-shaders :is    (the vec (main-light-color renderer)))
	  (uniformf  compiled-shaders :ka    (ka material-params))
	  (uniformf  compiled-shaders :kd    (kd material-params))
	  (uniformf  compiled-shaders :ks    (ks material-params))
	  (uniformf  compiled-shaders :shine (shininess material-params))
	  (uniform-matrix compiled-shaders :modelview-matrix 4
				   (vector (matrix* camera-vw-matrix
						    (elt view-matrix 0)
						    (elt model-matrix 0)))
				   nil)
	  (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	  (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	  (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
	  (render-debug object renderer))))))

(defmethod render-normalmap ((object building-floor-mesh) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (texture-object texture-object)
		   (normal-map normal-map)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)
		   (texture-coord-scaling texture-coord-scaling)) object
    (declare (texture:texture texture-object normal-map))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (declare (desired-type texture-coord-scaling))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
	(with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	  (use-program compiled-shaders :building-floor)
	  (gl:active-texture :texture0)
	  (texture:bind-texture texture-object)
	  (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	  (gl:active-texture :texture1)
	  (texture:bind-texture normal-map)
	  (uniformf  compiled-shaders :scale-text-coord texture-coord-scaling)
	  (uniformi  compiled-shaders :normal-map +texture-unit-normalmap+)
	  (uniformfv compiled-shaders :light-pos
			      (the vec (main-light-pos-eye-space renderer)))
	  (uniformfv compiled-shaders :ia    #(1.0 1.0 1.0))
	  (uniformfv compiled-shaders :id    #(1.0 1.0 1.0))
	  (uniformfv compiled-shaders :is    #(1.0 1.0 1.0))
	  (uniformf  compiled-shaders :ka    (ka material-params))
	  (uniformf  compiled-shaders :kd    (kd material-params))
	  (uniformf  compiled-shaders :ks    (ks material-params))
	  (uniformf  compiled-shaders :shine (shininess material-params))
	  (uniform-matrix compiled-shaders :modelview-matrix 4
				   (vector (matrix* camera-vw-matrix
						    (elt view-matrix 0)
						    (elt model-matrix 0)))
				   nil)
	  (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	  (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	  (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
	  (render-debug object renderer))))))

(defmethod clone-into :after ((from building-floor-mesh) (to building-floor-mesh))
  to)

(defmethod clone ((object building-floor-mesh))
  (let ((res (make-instance 'building-floor-mesh)))
    (clone-into object res)
    res))

(defun floor-tile (size-x size-z &key
				   (wrapper-transformation (identity-matrix)))
  (let* ((mesh (make-instance 'building-floor-mesh)))
    (with-pushed-matrix (mesh :what :modelview)
      (load-matrix mesh wrapper-transformation)
      (quads-plane mesh size-x size-z
		   (d/ size-x +terrain-chunk-tile-size+)
		   (d/ size-z +terrain-chunk-tile-size+)
		   0.0)
      (loop for i from 0 below (length (triangles mesh)) do
	   (push-pickable-attribute mesh 0.0)
	   (set-pickable-attribute  mesh :triangle-index i :pick-index 0)))
    (remove-orphaned-vertices mesh)
    (loop for i from 0 below (length (normals mesh)) do
	 (setf (elt (normals mesh) i) +y-axe+))
    mesh))
