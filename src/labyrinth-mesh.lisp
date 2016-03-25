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

(defclass instanced-mesh (triangle-mesh)
  ((renderer-data-position
    :initform nil
    :initarg  :renderer-data-position
    :accessor renderer-data-position)
   (renderer-data-count-position
    :initform 0
    :initarg  :renderer-data-count-position
    :accessor renderer-data-count-position)
   (parent-labyrinth
    :initform nil
    :initarg  :parent-labyrinth
    :writer (setf parent-labyrinth))))

(defmethod parent-labyrinth ((object instanced-mesh))
  (slot-value object 'parent-labyrinth))

(defgeneric all-position (object))

(defgeneric %destroy-no-children (object))

(defmethod %destroy-no-children ((object instanced-mesh))
  (with-accessors ((renderer-data-position renderer-data-position)
		   (renderer-data-vertices renderer-data-vertices)
		   (renderer-data-texture renderer-data-texture)
		   (renderer-data-normals renderer-data-normals)
		   (renderer-data-tangents renderer-data-tangents)
		   (renderer-data-tangents-obj-space renderer-data-tangents-obj-space)
		   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
		   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
		   (vbo vbo) (vao vao)) object
    (when +debug-mode+
      (misc:dbg "destroy instanced-mesh ~a" (id object)))
    (setf renderer-data-position           nil
	  vbo                              nil
	  vao                              nil
	  renderer-data-vertices           nil
	  renderer-data-texture            nil
	  renderer-data-normals            nil
	  renderer-data-normals-obj-space  nil
	  renderer-data-tangents           nil
	  renderer-data-tangents-obj-space nil
	  renderer-data-aabb-obj-space     nil)))

(defmethod destroy :after ((object instanced-mesh))
  (%destroy-no-children object))

(defun vbo-pos-vertex-buffer-handle (vbo)
  (alexandria:last-elt vbo))

(defun mesh-prototype (labyrinth)
  (elt (children labyrinth) 0))

(defmethod make-data-for-opengl ((object instanced-mesh))
  (with-accessors ((renderer-data-count-position renderer-data-count-position)
		   (renderer-data-position renderer-data-position)
		   (renderer-data-count-vertices renderer-data-count-vertices)
		   (renderer-data-count-texture renderer-data-count-texture)
		   (renderer-data-count-normals renderer-data-count-normals)
		   (renderer-data-count-tangents renderer-data-count-tangents)
		   (renderer-data-vertices renderer-data-vertices)
		   (renderer-data-texture renderer-data-texture)
		   (renderer-data-normals renderer-data-normals)
		   (renderer-data-tangents renderer-data-tangents)
		   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
		   (renderer-data-tangents-obj-space renderer-data-tangents-obj-space)
		   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
		   (vertices vertices)
		   (texture-coord texture-coord)
		   (normals normals)
		   (tangents tangents)
		   (triangles triangles)
		   (vbo vbo)
		   (vao vao)) object
    (let* ((raw-pos (all-position object))
	   (pos     (gl:alloc-gl-array :float (* 3 (length raw-pos)))))
      (loop
	 for vec-pos across raw-pos
	 for ct  from 0 by 1    do
	   (setf (fast-glaref pos (* ct 3)) (elt vec-pos 0))
	   (setf (fast-glaref pos (+ (* ct 3) 1)) (elt vec-pos 1))
	   (setf (fast-glaref pos (+ (* ct 3) 2)) (elt vec-pos 2)))
      (setf renderer-data-position pos)
      (setf renderer-data-count-position (* 3 (length raw-pos))))
    ;; setup finalizer
    (let ((gl-arr-pos (slot-value object 'renderer-data-position))
	  (id            (slot-value object 'id)))
      (tg:finalize object #'(lambda ()
			      (when +debug-mode+
				(misc:dbg "finalize destroy instanced-mesh ~a" id))
			      (free-memory* (list (gl:free-gl-array gl-arr-pos)) nil nil)
			      (setf gl-arr-pos nil))))))

(defmethod update-for-rendering ((object instanced-mesh))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (renderer-data-position renderer-data-position)
		   (renderer-data-count-position renderer-data-count-position)) object
    ;; let the gc collects the old gl-array
    (setf renderer-data-position nil)
    (make-data-for-opengl object)
    ;; update positions buffer
    (gl:bind-buffer     :array-buffer (vbo-pos-vertex-buffer-handle vbo))
    (gl:buffer-sub-data :array-buffer renderer-data-position)))

(defmethod prepare-for-rendering ((object instanced-mesh))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (renderer-data-position renderer-data-position)
		   (renderer-data-vertices renderer-data-vertices)
		   (renderer-data-normals renderer-data-normals)
		   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
		   (renderer-data-tangents renderer-data-tangents)
		   (renderer-data-tangents-obj-space renderer-data-tangents-obj-space)
		   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
		   (renderer-data-texture renderer-data-texture)
		   (texture-object texture-object)
		   (normal-map normal-map)) object
    (%destroy-no-children object)
    (setf vbo (gl:gen-buffers       (1+ +vbo-count+))
	  vao (gl:gen-vertex-arrays +vao-count+))
    (setf texture-object (texture-object (mesh-prototype object)))
    (setf normal-map (normal-map (mesh-prototype object)))
    (mesh:make-data-for-opengl object)
    (with-unbind-vao
      ;; vertices
      (gl:bind-buffer :array-buffer (vbo-vertex-buffer-handle vbo))
      (gl:buffer-data :array-buffer :static-draw
		      (renderer-data-vertices (mesh-prototype object)))
      ;; normals
      (gl:bind-buffer :array-buffer (vbo-normals-buffer-handle vbo))
      (gl:buffer-data :array-buffer :static-draw
		      (renderer-data-normals (mesh-prototype object)))
      ;; tangents
      (gl:bind-buffer :array-buffer (vbo-tangents-buffer-handle vbo))
      (gl:buffer-data :array-buffer :static-draw
		      (renderer-data-tangents (mesh-prototype object)))
      ;; texture coordinates
      (gl:bind-buffer :array-buffer (vbo-texture-buffer-handle vbo))
      (gl:buffer-data :array-buffer :static-draw
		      (renderer-data-texture (mesh-prototype object)))
      ;; positions
      (gl:bind-buffer :array-buffer (vbo-pos-vertex-buffer-handle vbo))
      (gl:buffer-data :array-buffer :static-draw renderer-data-position)
      (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
      (gl:bind-buffer :array-buffer (vbo-pos-vertex-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-pick-pos-location+ 3 :float 0 0
				(mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-pick-pos-location+)
      (%gl:vertex-attrib-divisor +attribute-pick-pos-location+ 1))
    (if normal-map
	(prepare-for-rendering-normal-map object)
	(prepare-for-rendering-phong      object))
    object))

(defmethod render ((object instanced-mesh) renderer)
  (with-accessors ((normal-map normal-map)) object
    (if normal-map
	(render-normalmap object renderer)
	(render-phong object renderer))))

(defmethod render-phong ((object instanced-mesh) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (renderer-data-count-position renderer-data-count-position)
		   (renderer-data-position renderer-data-position)
		   (texture-object texture-object)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)
		   (current-time current-time)
		   (fog-density fog-density)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (with-camera-view-matrix (camera-vw-matrix renderer)
      (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	(with-unbind-vao
	  (use-program compiled-shaders :mesh-ads-inst)
	  (gl:active-texture :texture0)
	  (texture:bind-texture texture-object)
	  (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	  (uniformfv compiled-shaders :light-pos
		     (the vec (main-light-pos-eye-space renderer)))
	  (uniformf  compiled-shaders :scale-text-coord 1.0)
	  (uniformfv compiled-shaders :ia    #(1.0 1.0 1.0))
	  (uniformfv compiled-shaders :id    (the vec (main-light-color renderer)))
	  (uniformfv compiled-shaders :is    (the vec (main-light-color renderer)))
	  (uniformf  compiled-shaders :ka    (ka (material-params (mesh-prototype object))))
	  (uniformf  compiled-shaders :kd    (kd (material-params (mesh-prototype object))))
	  (uniformf  compiled-shaders :ks    (ks (material-params (mesh-prototype object))))
	  (uniformf  compiled-shaders :shine (shininess (material-params (mesh-prototype object))))
	  (uniformf  compiled-shaders :time  current-time)
	  (uniformf  compiled-shaders :fog-density (fog-density (mesh-prototype object)))
	  (uniform-matrix compiled-shaders :model-matrix 4 model-matrix nil)
	  (uniform-matrix compiled-shaders :modelview-matrix 4
			  (vector (matrix* camera-vw-matrix
					   (elt view-matrix 0)
					   (elt model-matrix 0)))
			  nil)
	  (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	  (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	  ;;(gl:draw-arrays :triangles 0 (* 3 (length (triangles (elt (children object) 0))))))))))
	  (%gl:draw-arrays-instanced :triangles 0
				     (f* 3 (the fixnum
						(length (the list
							     (triangles (mesh-prototype object))))))
				     (f/ (the fixnum renderer-data-count-position) 3)))))))

(defmethod render-normalmap ((object instanced-mesh) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (renderer-data-count-position renderer-data-count-position)
		   (texture-object texture-object)
		   (normal-map normal-map)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)
		   (material-params material-params)
		   (current-time current-time)
		   (fog-density fog-density)) object
    (declare (texture:texture texture-object normal-map))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (with-camera-view-matrix (camera-vw-matrix renderer)
      (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	(use-program compiled-shaders :mesh-bump-inst)
	(gl:active-texture :texture0)
	(texture:bind-texture texture-object)
	(uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	(gl:active-texture :texture1)
	(texture:bind-texture normal-map)
	(uniformi compiled-shaders :normal-map +texture-unit-normalmap+)
	(uniformfv compiled-shaders :light-pos
		   (the vec (main-light-pos-eye-space renderer)))
	(uniformfv compiled-shaders :ia    #(1.0 1.0 1.0))
	(uniformfv compiled-shaders :id    (the vec (main-light-color renderer)))
	(uniformfv compiled-shaders :is    (the vec (main-light-color renderer)))
	(uniformf  compiled-shaders :ka    (ka (material-params (mesh-prototype object))))
	(uniformf  compiled-shaders :kd    (kd (material-params (mesh-prototype object))))
	(uniformf  compiled-shaders :ks    (ks (material-params (mesh-prototype object))))
	(uniformf  compiled-shaders :shine (shininess (material-params (mesh-prototype object))))
	(uniformf  compiled-shaders :time  current-time)
	(uniformf  compiled-shaders :fog-density (fog-density (mesh-prototype object)))
	(uniform-matrix compiled-shaders :model-matrix 4 model-matrix nil)
	(uniform-matrix compiled-shaders :modelview-matrix 4
			(vector (matrix* camera-vw-matrix
					 (elt view-matrix 0)
					 (elt model-matrix 0)))
			nil)
	(uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
	(gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	;;(gl:draw-arrays :triangles 0 (* 3 (length (triangles (elt (children object) 0))))))))))
	(%gl:draw-arrays-instanced :triangles 0
				   (f* 3 (the fixnum
					      (length (the list
							   (triangles (mesh-prototype object))))))
				   (f/ (the fixnum renderer-data-count-position) 3))))))

(defmethod all-position ((object instanced-mesh))
  (let ((res (make-fresh-array 0 nil 'vec nil)))
    (loop for a across (children object) do
	 (when (renderp a)
	   (vector-push-extend (pos a) res)))
    res))

(defclass labyrinth-mesh (triangle-mesh)
  ((wall-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :wall-instanced
    :accessor wall-instanced)
   (window-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :window-instanced
    :accessor window-instanced)
   (pillar-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :pillar-instanced
    :accessor pillar-instanced)
   (door-n-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :door-n-instanced
    :accessor door-n-instanced)
   (door-s-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :door-s-instanced
    :accessor door-s-instanced)
   (door-e-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :door-e-instanced
    :accessor door-e-instanced)
   (door-w-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :door-w-instanced
    :accessor door-w-instanced)
   (table-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :table-instanced
    :accessor table-instanced)
   (chair-n-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :chair-n-instanced
    :accessor chair-n-instanced)
   (chair-s-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :chair-s-instanced
    :accessor chair-s-instanced)
   (chair-e-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :chair-e-instanced
    :accessor chair-e-instanced)
   (chair-w-instanced
    :initform (make-instance 'instanced-mesh)
    :initarg  :chair-w-instanced
    :accessor chair-w-instanced)))

(defun labyrinth-mesh-p (a)
  (typep a 'mesh:labyrinth-mesh))

(defmethod aabb ((object labyrinth-mesh))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-slots (aabb) object
    aabb))

(defun %grow-aabb (aabb accessor)
  (loop for a across (children accessor) do
       (when (renderp a)
	 (expand aabb (aabb-p1 (aabb a)))
	 (expand aabb (aabb-p2 (aabb a))))))

(defmethod reset-aabb ((object labyrinth-mesh))
  "Recreate the aabb from object space vertices"
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-accessors ((wall-instanced   wall-instanced)
		   (window-instanced window-instanced)
		   (pillar-instanced pillar-instanced)
		   (door-n-instanced door-n-instanced)
		   (door-s-instanced door-s-instanced)
		   (door-e-instanced door-e-instanced)
		   (door-w-instanced door-w-instanced)
		   (table-instanced table-instanced)
		   (chair-n-instanced chair-n-instanced)
		   (chair-s-instanced chair-s-instanced)
		   (chair-e-instanced chair-e-instanced)
		   (chair-w-instanced chair-w-instanced)
		   (bounding-sphere bounding-sphere)) object
    (with-slots (aabb) object
      (map nil #'bubbleup-modelmatrix (children wall-instanced))
      (map nil #'bubbleup-modelmatrix (children window-instanced))
      (map nil #'bubbleup-modelmatrix (children pillar-instanced))
      (map nil #'bubbleup-modelmatrix (children door-n-instanced))
      (map nil #'bubbleup-modelmatrix (children door-s-instanced))
      (map nil #'bubbleup-modelmatrix (children door-e-instanced))
      (map nil #'bubbleup-modelmatrix (children door-w-instanced))
      (map nil #'bubbleup-modelmatrix (children table-instanced))
      (map nil #'bubbleup-modelmatrix (children chair-n-instanced))
      (map nil #'bubbleup-modelmatrix (children chair-s-instanced))
      (map nil #'bubbleup-modelmatrix (children chair-e-instanced))
      (map nil #'bubbleup-modelmatrix (children chair-w-instanced))
      (setf aabb (make-instance 'aabb))
      (%grow-aabb aabb wall-instanced)
      (%grow-aabb aabb window-instanced)
      (%grow-aabb aabb pillar-instanced)
      (%grow-aabb aabb door-n-instanced)
      (%grow-aabb aabb door-s-instanced)
      (%grow-aabb aabb door-e-instanced)
      (%grow-aabb aabb door-w-instanced)
      (%grow-aabb aabb table-instanced)
      (%grow-aabb aabb chair-n-instanced)
      (%grow-aabb aabb chair-s-instanced)
      (%grow-aabb aabb chair-e-instanced)
      (%grow-aabb aabb chair-w-instanced)
      (expand aabb (vec+ (aabb-p1 aabb) (vec 0.0 +wall-h+ 0.0)))
      (setf bounding-sphere (aabb->bounding-sphere aabb))
      object)))

(defmethod destroy :after ((object labyrinth-mesh))
  (destroy (wall-instanced object))
  (destroy (window-instanced object))
  (destroy (pillar-instanced object))
  (destroy (door-n-instanced object))
  (destroy (door-s-instanced object))
  (destroy (door-e-instanced object))
  (destroy (door-w-instanced object))
  (destroy (table-instanced object))
  (destroy (chair-n-instanced object))
  (destroy (chair-s-instanced object))
  (destroy (chair-e-instanced object))
  (destroy (chair-w-instanced object)))

(defmethod prepare-for-rendering ((object labyrinth-mesh))
  (with-accessors ((wall-instanced   wall-instanced)
		   (window-instanced window-instanced)
		   (pillar-instanced pillar-instanced)
		   (door-n-instanced door-n-instanced)
		   (door-s-instanced door-s-instanced)
		   (door-e-instanced door-e-instanced)
		   (door-w-instanced door-w-instanced)
		   (table-instanced table-instanced)
		   (chair-n-instanced chair-n-instanced)
		   (chair-s-instanced chair-s-instanced)
		   (chair-e-instanced chair-e-instanced)
		   (chair-w-instanced chair-w-instanced)) object
    (and (not (vector-empty-p (children wall-instanced)))
	 (prepare-for-rendering wall-instanced))
    (and (not (vector-empty-p (children window-instanced)))
	 (prepare-for-rendering window-instanced))
    (and (not (vector-empty-p (children pillar-instanced)))
	 (prepare-for-rendering pillar-instanced))
    (and (not (vector-empty-p (children door-n-instanced)))
	 (prepare-for-rendering door-n-instanced))
    (and (not (vector-empty-p (children door-s-instanced)))
	 (prepare-for-rendering door-s-instanced))
    (and (not (vector-empty-p (children door-e-instanced)))
	 (prepare-for-rendering door-e-instanced))
    (and (not (vector-empty-p (children door-w-instanced)))
	 (prepare-for-rendering door-w-instanced))
    (and (not (vector-empty-p (children table-instanced)))
	 (prepare-for-rendering table-instanced))
    (and (not (vector-empty-p (children chair-n-instanced)))
	 (prepare-for-rendering chair-n-instanced))
    (and (not (vector-empty-p (children chair-s-instanced)))
	 (prepare-for-rendering chair-s-instanced))
    (and (not (vector-empty-p (children chair-e-instanced)))
	 (prepare-for-rendering chair-e-instanced))
    (and (not (vector-empty-p (children chair-w-instanced)))
	 (prepare-for-rendering chair-w-instanced))
    (reset-aabb object)
    (setf (labyrinth-parents-instanced object) object)))

(defmethod update-for-rendering ((object labyrinth-mesh))
  (with-accessors ((wall-instanced   wall-instanced)
		   (window-instanced window-instanced)
		   (pillar-instanced pillar-instanced)
		   (door-n-instanced door-n-instanced)
		   (door-s-instanced door-s-instanced)
		   (door-e-instanced door-e-instanced)
		   (door-w-instanced door-w-instanced)
		   (table-instanced table-instanced)
		   (chair-n-instanced chair-n-instanced)
		   (chair-s-instanced chair-s-instanced)
		   (chair-e-instanced chair-e-instanced)
		   (chair-w-instanced chair-w-instanced)) object
      (and (not (vector-empty-p (children wall-instanced)))
	   (update-for-rendering wall-instanced))
      (and (not (vector-empty-p (children window-instanced)))
	   (update-for-rendering window-instanced))
      (and (not (vector-empty-p (children pillar-instanced)))
	   (update-for-rendering pillar-instanced))
      (and (not (vector-empty-p (children door-n-instanced)))
	   (update-for-rendering door-n-instanced))
      (and (not (vector-empty-p (children door-s-instanced)))
	   (update-for-rendering door-s-instanced))
      (and (not (vector-empty-p (children door-e-instanced)))
	   (update-for-rendering door-e-instanced))
      (and (not (vector-empty-p (children door-w-instanced)))
	   (update-for-rendering door-w-instanced))
      (and (not (vector-empty-p (children table-instanced)))
	   (update-for-rendering table-instanced))
      (and (not (vector-empty-p (children chair-n-instanced)))
	   (update-for-rendering chair-n-instanced))
      (and (not (vector-empty-p (children chair-s-instanced)))
	   (update-for-rendering chair-s-instanced))
      (and (not (vector-empty-p (children chair-e-instanced)))
	   (update-for-rendering chair-e-instanced))
      (and (not (vector-empty-p (children chair-w-instanced)))
	   (update-for-rendering chair-w-instanced))
      (reset-aabb object)))

(defmethod render ((object labyrinth-mesh) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((wall-instanced   wall-instanced)
		   (window-instanced window-instanced)
		   (pillar-instanced pillar-instanced)
		   (door-n-instanced door-n-instanced)
		   (door-s-instanced door-s-instanced)
		   (door-e-instanced door-e-instanced)
		   (door-w-instanced door-w-instanced)
		   (table-instanced table-instanced)
		   (chair-n-instanced chair-n-instanced)
		   (chair-s-instanced chair-s-instanced)
		   (chair-e-instanced chair-e-instanced)
		   (chair-w-instanced chair-w-instanced)) object
    (when (rendering-needed-p object renderer)
      (and (not (vector-empty-p (children wall-instanced)))
	   (render wall-instanced renderer))
      (and (not (vector-empty-p (children window-instanced)))
	   (render window-instanced renderer))
      (and (not (vector-empty-p (children pillar-instanced)))
	   (render pillar-instanced renderer))
      (and (not (vector-empty-p (children door-n-instanced)))
	   (render door-n-instanced renderer))
      (and (not (vector-empty-p (children door-s-instanced)))
	   (render door-s-instanced renderer))
      (and (not (vector-empty-p (children door-e-instanced)))
	   (render door-e-instanced renderer))
      (and (not (vector-empty-p (children door-w-instanced)))
	   (render door-w-instanced renderer))
      (and (not (vector-empty-p (children table-instanced)))
	   (render table-instanced renderer))
      (and (not (vector-empty-p (children chair-n-instanced)))
	   (render chair-n-instanced renderer))
      (and (not (vector-empty-p (children chair-s-instanced)))
	   (render chair-s-instanced renderer))
      (and (not (vector-empty-p (children chair-e-instanced)))
	   (render chair-e-instanced renderer))
      (and (not (vector-empty-p (children chair-w-instanced)))
	   (render chair-w-instanced renderer)))))

(defmethod (setf compiled-shaders) (val (object labyrinth-mesh))
  (with-accessors ((wall-instanced   wall-instanced)
		   (window-instanced window-instanced)
		   (pillar-instanced pillar-instanced)
		   (door-n-instanced door-n-instanced)
		   (door-s-instanced door-s-instanced)
		   (door-e-instanced door-e-instanced)
		   (door-w-instanced door-w-instanced)
		   (table-instanced table-instanced)
		   (chair-n-instanced chair-n-instanced)
		   (chair-s-instanced chair-s-instanced)
		   (chair-e-instanced chair-e-instanced)
		   (chair-w-instanced chair-w-instanced)) object
    (setf (compiled-shaders wall-instanced) val)
    (setf (compiled-shaders window-instanced) val)
    (setf (compiled-shaders pillar-instanced) val)
    (setf (compiled-shaders door-n-instanced) val)
    (setf (compiled-shaders door-s-instanced) val)
    (setf (compiled-shaders door-e-instanced) val)
    (setf (compiled-shaders door-w-instanced) val)
    (setf (compiled-shaders table-instanced)  val)
    (setf (compiled-shaders chair-n-instanced) val)
    (setf (compiled-shaders chair-s-instanced) val)
    (setf (compiled-shaders chair-e-instanced) val)
    (setf (compiled-shaders chair-w-instanced) val)))

(defmethod rendering-needed-p ((object labyrinth-mesh) renderer)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (world:cone-aabb-intersects-p renderer object))

(defgeneric (setf labyrinth-parents-instanced) (val object))

(defmethod (setf labyrinth-parents-instanced) (val (object labyrinth-mesh))
    (with-accessors ((wall-instanced   wall-instanced)
		     (window-instanced window-instanced)
		     (pillar-instanced pillar-instanced)
		     (door-n-instanced door-n-instanced)
		     (door-s-instanced door-s-instanced)
		     (door-e-instanced door-e-instanced)
		     (door-w-instanced door-w-instanced)
		     (table-instanced table-instanced)
		     (chair-n-instanced chair-n-instanced)
		     (chair-s-instanced chair-s-instanced)
		     (chair-e-instanced chair-e-instanced)
		     (chair-w-instanced chair-w-instanced)) object
      (and (not (vector-empty-p (children wall-instanced)))
	   (setf (parent-labyrinth wall-instanced) val))
      (and (not (vector-empty-p (children window-instanced)))
	   (setf (parent-labyrinth window-instanced) val))
      (and (not (vector-empty-p (children pillar-instanced)))
	   (setf (parent-labyrinth pillar-instanced) val))
      (and (not (vector-empty-p (children door-n-instanced)))
	   (setf (parent-labyrinth door-n-instanced) val))
      (and (not (vector-empty-p (children door-s-instanced)))
	   (setf (parent-labyrinth door-s-instanced) val))
      (and (not (vector-empty-p (children door-e-instanced)))
	   (setf (parent-labyrinth door-e-instanced) val))
      (and (not (vector-empty-p (children door-w-instanced)))
	   (setf (parent-labyrinth door-w-instanced) val))
      (and (not (vector-empty-p (children table-instanced)))
	   (setf (parent-labyrinth table-instanced) val))
      (and (not (vector-empty-p (children chair-n-instanced)))
	   (setf (parent-labyrinth chair-n-instanced) val))
      (and (not (vector-empty-p (children chair-s-instanced)))
	   (setf (parent-labyrinth chair-s-instanced) val))
      (and (not (vector-empty-p (children chair-e-instanced)))
	   (setf (parent-labyrinth chair-e-instanced) val))
      (and (not (vector-empty-p (children chair-w-instanced)))
	   (setf (parent-labyrinth chair-w-instanced) val))))
