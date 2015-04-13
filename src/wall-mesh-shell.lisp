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

(in-package :mesh)

(defclass wall-mesh-shell (triangle-mesh-shell) ())

(defmethod render-phong ((object wall-mesh-shell) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (texture-object texture-object)
		   (texture-projector texture-projector)
		   (projection-matrix projection-matrix)
		   (projector projector)
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
	(use-program compiled-shaders :wall-decorated)
	(gl:active-texture :texture0)
	(uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	(texture:bind-texture texture-object)
	(uniformi compiled-shaders :texture-projector +texture-unit-projector+)
	(gl:active-texture :texture4)
	(texture:bind-texture texture-projector)
	(uniformfv compiled-shaders :light-pos
			    (the vec (main-light-pos-eye-space renderer)))
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
	(uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
	(uniform-matrix compiled-shaders :proj-texture-matrix 4
				 (vector (matrix* +projective-scale-bias+
						  (perspective 70.0 1.0 0.00001 .01)
						  projector
						  (elt model-matrix 0)))
				 nil)
	(gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	(gl:draw-arrays :triangles 0 (* 3 (length triangles))))
	(render-debug object renderer)))))

(defmethod setup-projective-texture ((object wall-mesh-shell))
  (let* ((mesh-center (pos  object))
	 (aabb        (aabb object))
	 (y           (d* 0.5 (dabs (elt (vec- (aabb-p2 aabb) (aabb-p1 aabb)) 1))))
	 (h           (vec 0.0 y 0.0))
	 (translation (d- (lcg-next-upto (d* 0.25 +terrain-chunk-tile-size+))))
 	 (pos         (transform-point
		       (transform-point (vec+ mesh-center h)
					(translate* 0.0 0.0 translation))
		       (translate* 0.0
				   (d- (lcg-next-upto (d* 0.25 +terrain-chunk-tile-size+))
				       (d* 0.125 +terrain-chunk-tile-size+))
				   0.0)))
	 (eye         (transform-point pos (translate* 0.0 0.0 0.001))))
    (setf (projector object) (look@ pos eye +y-axe+))
    pos))
