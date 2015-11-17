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

(in-package :terrain-chunk)

(define-constant +texture-tag-shore-terrain+        "shore-terrain"      :test #'string= )

(define-constant +texture-tag-grass-terrain+        "grass-terrain"      :test #'string= )

(define-constant +texture-tag-snow-terrain+         "snow-terrain"       :test #'string= )

(define-constant +texture-tag-soil-terrain-level-1+ "soil-level-1"       :test #'string= )

(define-constant +texture-tag-soil-terrain-level-2+ "soil-level-2"       :test #'string= )

(define-constant +decals-weights-size+              512.0                :test #'=)

(define-constant +attribute-decal-texture+          :texture-decals      :test #'eq)

(define-constant +clip-plane-no-clip+ (vec4:vec4 0.0 1.0 0.0 +terrain-noclip-plane-dist+)
  :test #'vec4:vec4~)

(define-constant +clip-plane-for-water+ (vec4:vec4 0.0 1.0 0.0
						   (d+ (d- +water-mesh-starting-y+) 1.0))
  :test #'vec4:vec4~)

(defparameter *clip-plane* +clip-plane-no-clip+)

(defun initform-terrain (tag)
  (let ((l (texture:list-of-texture-by-tag tag)))
    (if l
	(elt l (lcg-next-upto (length l)))
	nil)))

(defclass terrain-chunk (pickable-mesh)
  ((heightmap
    :initform nil
    :initarg  :heightmap
    :accessor heightmap)
   (origin-offset
    :initform (vec 0.0 0.0 0.0)
    :initarg  :origin-offset
    :accessor origin-offset)
   (text-coord-weights
    :initform (misc:make-fresh-array 0 (num:desired 0.0) t nil)
    :initarg  :text-coord-weights
    :accessor text-coord-weights)
   (renderer-data-count-weights
    :initform 0
    :initarg  :renderer-data-count-weights
    :accessor renderer-data-count-weights)
   (renderer-data-text-weights
    :initform nil
    :initarg  :renderer-data-text-weights
    :accessor renderer-data-text-weights)
   (texture-shore
    :initform (initform-terrain texture:+texture-tag-shore-terrain+)
    :initarg  :texture-shore
    :accessor  texture-shore)
   (texture-grass
    :initform (initform-terrain texture:+texture-tag-grass-terrain+)
    :initarg  :texture-grass
    :accessor  texture-grass)
   (texture-snow
    :initform (initform-terrain texture:+texture-tag-snow-terrain+)
    :initarg  :texture-snow
    :accessor  texture-snow)
   (texture-soil-level-1
    :initform (initform-terrain texture:+texture-tag-soil-terrain-level-1+)
    :initarg  :texture-soil-level-1
    :accessor  texture-soil-level-1)
   (texture-soil-level-2
    :initform (initform-terrain texture:+texture-tag-soil-terrain-level-2+)
    :initarg  :texture-soil-level-2
    :accessor  texture-soil-level-2)
   (texture-soil-decal
    :initform (initform-terrain texture:+texture-tag-soil-decal+)
    :initarg  :texture-soil-decal
    :accessor texture-soil-decal)
   (texture-road-decal
    :initform (initform-terrain texture:+texture-tag-road-decal+)
    :initarg  :texture-road-decal
    :accessor texture-road-decal)
   (texture-building-decal
    :initform (initform-terrain texture:+texture-tag-building-decal+)
    :initarg  :texture-building-decal
    :accessor texture-building-decal)
   (decal-weights
    :initform nil
    :reader decal-weights)))

(defmethod marshal:class-persistant-slots ((object terrain-chunk))
  (append '(origin-offset)
	  (call-next-method)))

(defmethod find-index-by-value ((object terrain-chunk) value &key (what :vertex) (from-end t))
   (let ((sequence (ecase what
		    (:vertex
		     (vertices object))
		    (:texture
		     (texture-coord object))
		    (:normal
		     (normals object))
		    (:tangent
		     (tangents object)))))
    (position-if #'(lambda (a) (every #'epsilon= a value)) sequence :from-end from-end)))

(defmethod destroy :after ((object terrain-chunk))
  (with-accessors ((renderer-data-text-weights renderer-data-text-weights)) object
    (when +debug-mode+
      (misc:dbg "destroy terrain-chunk ~a" (identificable:id object)))
      (setf renderer-data-text-weights nil)))

(defmethod make-data-for-opengl :after ((object terrain-chunk))
  (with-accessors ((renderer-data-count-weights renderer-data-count-weights)
		   (renderer-data-text-weights renderer-data-text-weights)
		   (text-coord-weights text-coord-weights)) object
    (let ((weights (gl:alloc-gl-array :float (* 6 (length (triangles object))))))
      (loop
	 for triangle in (triangles object)
	 for ct from 0 by 6                 do
	   (let ((decal-indices (get-custom-attribute triangle
						      +attribute-decal-texture+)))
	     (loop
		for i across decal-indices
		for offset from 0 by 2 do
		  (let ((coords (elt text-coord-weights i)))
		    (setf (gl-utils:fast-glaref weights (+ ct offset))
			  (elt coords 0))
		    (setf (gl-utils:fast-glaref weights (+ ct offset 1))
			  (elt coords 1))))))
      (setf renderer-data-text-weights  weights)
      (setf renderer-data-count-weights (* 6 (length (triangles object)))))
    ;; setup finalizer
    (let ((gl-arr-weight (slot-value object 'renderer-data-text-weights))
	  (id            (slot-value object 'id)))
      (tg:finalize object #'(lambda ()
			      (when +debug-mode+
				(misc:dbg "finalize destroy terrain ~a" id))
			      (free-memory* (list (gl:free-gl-array gl-arr-weight)) nil nil)
			      (setf gl-arr-weight nil))))))

(defmethod render ((object terrain-chunk) renderer)
  (actual-render object renderer))

(defmethod render-for-reflection ((object terrain-chunk) renderer)
  (let ((*clip-plane* +clip-plane-for-water+))
    (actual-render object renderer)))

(defmethod prepare-for-rendering :after ((object terrain-chunk))
  (with-accessors ((vbo vbo)
 		   (vao vao)
 		   (renderer-data-text-weights renderer-data-text-weights)) object
    (setf vbo (append vbo (gl:gen-buffers 1))
 	  vao (append vao (gl:gen-vertex-arrays 1)))
    (mesh:make-data-for-opengl object)
    (with-unbind-vao
      ;; decals-weights
      (gl:bind-buffer :array-buffer (last-elt vbo))
      (gl:buffer-data :array-buffer :static-draw renderer-data-text-weights)
      (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
      ;; vertices
      (gl:bind-buffer :array-buffer (vbo-vertex-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-position-location+ 3 :float 0 0
				(gl-utils:mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-position-location+)
      ;; normals
      (gl:bind-buffer :array-buffer (vbo-normals-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-normal-location+ 3 :float 0 0
				(gl-utils:mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-normal-location+)
      ;; texture
      (gl:bind-buffer :array-buffer (vbo-texture-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-texture-location+ 2 :float 0 0
				(gl-utils:mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-texture-location+)
      ;; decals texture
      (gl:bind-buffer :array-buffer (last-elt vbo))
      (gl:vertex-attrib-pointer +attribute-texture-decals-location+ 2 :float 0 0
				(gl-utils:mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-texture-decals-location+))
    object))

(defmethod clone-into :after ((from terrain-chunk) (to terrain-chunk))
  (setf (heightmap              to)     (matrix:clone (heightmap  from))
	(text-coord-weights     to)     (copy-array (text-coord-weights from))
	(texture-shore          to)     (texture-shore          from)
	(texture-grass          to)     (texture-grass          from)
	(texture-snow           to)     (texture-snow           from)
	(texture-soil-level-1   to)     (texture-soil-level-1   from)
	(texture-soil-level-2   to)     (texture-soil-level-2   from)
	(texture-soil-decal     to)     (texture-soil-decal     from)
	(texture-road-decal     to)     (texture-road-decal     from)
	(texture-building-decal to)     (texture-building-decal from)
	;; we need to use the slot because the accessor is *not* a mere setf, see below.
	(slot-value to 'decal-weights) (decal-weights          from))
  to)

(defmethod clone ((object terrain-chunk))
  (with-simple-clone (object 'terrain-chunk)))

(defmethod copy-flat-into :after ((from terrain-chunk) (to terrain-chunk))
  (setf (heightmap              to)     (heightmap              from)
	(text-coord-weights     to)     (text-coord-weights     from)
	(texture-shore          to)     (texture-shore          from)
	(texture-grass          to)     (texture-grass          from)
	(texture-snow           to)     (texture-snow           from)
	(texture-soil-level-1   to)     (texture-soil-level-1   from)
	(texture-soil-level-2   to)     (texture-soil-level-2   from)
	(texture-soil-decal     to)     (texture-soil-decal     from)
	(texture-road-decal     to)     (texture-road-decal     from)
	(texture-building-decal to)     (texture-building-decal from)
	;; we need to use the slot because the accessor is *not* a mere setf, see below.
	(slot-value to 'decal-weights) (decal-weights          from))
  to)

(defmethod copy-flat ((object terrain-chunk))
  (with-simple-copy-flat (object 'terrain-chunk)))

(defgeneric actual-render (object renderer))

(defgeneric build-mesh (object))

(defgeneric build-mesh-dbg (object))

(defgeneric (setf decal-weights) (pixmap object))

(defgeneric nclip-with-aabb (object aabb &key regenerate-rendering-data clip-if-inside))

(defgeneric clip-with-aabb (object aabb &key regenerate-rendering-data clip-if-inside))

(defgeneric clip-triangles (object aabb predicate))

(defgeneric push-decal-weights-text-coord (object s-coord t-coord))

(defgeneric set-decals-triangle-attribute (object))

(defgeneric triangle-matrix-elt (object row col))

(defgeneric approx-terrain-height (object x z))

(defmethod actual-render ((object terrain-chunk) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
		   (vao vao)
		   (texture-shore texture-shore)
		   (texture-grass texture-grass)
		   (texture-snow texture-snow)
		   (texture-soil-level-1 texture-soil-level-1)
		   (texture-soil-level-2 texture-soil-level-2)
		   (texture-soil-decal texture-soil-decal)
		   (texture-road-decal texture-road-decal)
		   (texture-building-decal texture-building-decal)
		   (decal-weights decal-weights)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (compiled-shaders compiled-shaders)
		   (triangles triangles)) object
    (declare (texture:texture texture-shore texture-grass texture-snow
			      texture-soil-level-1   texture-soil-level-2
			      texture-soil-decal     texture-road-decal
			      texture-building-decal decal-weights ))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
	(with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	  (gl:enable :clip-distance0)
	  (use-program compiled-shaders :terrain)
	  (gl:active-texture :texture0)
	  (texture:bind-texture texture-shore)
	  (uniformi compiled-shaders :texture-terrain-level-1 0)
	  (gl:active-texture :texture1)
	  (texture:bind-texture texture-grass)
	  (uniformi compiled-shaders :texture-terrain-level-2 1)
	  (gl:active-texture :texture2)
	  (texture:bind-texture texture-snow)
	  (uniformi compiled-shaders :texture-terrain-level-3 2)
	  (gl:active-texture :texture3)
	  (texture:bind-texture texture-soil-level-1)
	  (uniformi compiled-shaders :texture-terrain-rock-level-1 3)
	  (gl:active-texture :texture4)
	  (texture:bind-texture texture-soil-level-2)
	  (uniformi compiled-shaders :texture-terrain-rock-level-2 4)
	  (gl:active-texture :texture5)
	  (texture:bind-texture texture-soil-decal)
	  (uniformi compiled-shaders :texture-soil-decal 5)
	  (gl:active-texture :texture6)
	  (texture:bind-texture texture-road-decal)
	  (uniformi compiled-shaders :texture-roads-decal 6)
	  (gl:active-texture :texture7)
	  (texture:bind-texture texture-building-decal)
	  (uniformi compiled-shaders :texture-building-decal 7)
	  (gl:active-texture :texture8)
	  (texture:bind-texture decal-weights)
	  (uniformi compiled-shaders :decals-weights 8)
	  (uniformfv compiled-shaders :light-pos
			      (the vec (main-light-pos-eye-space renderer)))
	  (uniformfv compiled-shaders :ia        (the vec (main-light-color renderer)))
	  (uniformfv compiled-shaders :id        (the vec (main-light-color renderer)))
	  (uniformfv compiled-shaders :is        (the vec (main-light-color renderer)))
	  (uniformf  compiled-shaders :ka        0.5)
	  (uniformf  compiled-shaders :kd        1.0)
	  (uniformf  compiled-shaders :ks        1.0)
	  (uniformf  compiled-shaders :shine    50.0)
	  (uniformfv compiled-shaders :pick-color +color-tile-pick-can-move+)
	  (uniformfv compiled-shaders :clip-plane (the vec4 *clip-plane*))
	  (uniform-matrix compiled-shaders :modelview-matrix 4
				   (vector (matrix* camera-vw-matrix
						    (elt view-matrix 0)
						    (elt model-matrix 0)))
				   nil)
	  (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
	  (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
	  (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
	  (gl:enable :clip-distance0)
	  (render-debug object renderer)))))
  (do-children-mesh (i object)
    (render i renderer)))

(defmethod (setf decal-weights) (pixmap (object terrain-chunk))
  "Note: this method is not  a mere writer, it copy and scale
   the pixmap to 512x512 pixel"
  (with-slots (decal-weights) object
    (let ((copied (interfaces:clone pixmap)))
      (matrix:h-mirror-matrix copied)
      (pixmap:ncopy-matrix-into-pixmap
       copied
       (matrix:scale-matrix copied
			    (num:d/ +decals-weights-size+
				    (num:desired (matrix:width  copied)))
			    (num:d/ +decals-weights-size+
				    (num:desired (matrix:height copied))))
       4)
      (setf decal-weights (texture:gen-name-and-inject-in-database copied)))))

(defun %put-vertex (map x z s-tex t-tex inc-x inc-z inc-tex-s inc-tex-t)
  (let* ((d-width  (desired (matrix:width  (heightmap map))))
	 (d-height (desired (matrix:height (heightmap map))))
	 (delta-x  (d/ 1.0 d-width))
	 (delta-z  (d/ 1.0 d-height))
	 (nx       (d+ x inc-x))
	 (nz       (d+ z inc-z))
	 (fx       (matrix:sample@ (heightmap map) nx nz
				   :behaivour-on-border-fn #'num:clamp-0->max-less-one
				   :interpolation t
				   :clamp t
				   :interpolate-fn #'(lambda (w a b) (dlerp w a b))))
	 (fx-dx    (matrix:sample@ (heightmap map) (d- nx delta-x) nz
				   :behaivour-on-border-fn #'num:clamp-0->max-less-one
				   :interpolation t
				   :clamp t
				   :interpolate-fn #'(lambda (w a b) (dlerp w a b))))
	 (fz-dz    (matrix:sample@ (heightmap map) nx (d- nz delta-z)
				   :behaivour-on-border-fn #'num:clamp-0->max-less-one
				   :interpolation t
				   :clamp t
				   :interpolate-fn #'(lambda (w a b) (dlerp w a b))))
	 (fx+dx    (matrix:sample@ (heightmap map) (d+ nx delta-x) nz
				   :behaivour-on-border-fn #'num:clamp-0->max-less-one
				   :interpolation t
				   :clamp t
				   :interpolate-fn
				   #'(lambda (w a b) (dlerp w a b))))
	 (fz+dz    (matrix:sample@ (heightmap map) nx (d+ nz delta-z)
				   :behaivour-on-border-fn #'num:clamp-0->max-less-one
				   :interpolation t
				   :clamp t
				   :interpolate-fn #'(lambda (w a b) (dlerp w a b))))
	 (delta-fx (d- fx-dx fx+dx))
	 (delta-fz (d- fz-dz fz+dz))
	 (normal   (normalize (vec (d* delta-fx 1.0) 2.0 (d* delta-fz 1.0))))
	 (world-x  (d* nx d-width  +terrain-chunk-tile-size+ 2.0))
	 (world-z  (d* nz d-height +terrain-chunk-tile-size+ 2.0)))
    (normal-v map normal)
    (tangent-v map +x-axe+)
    (texel map (d+ s-tex inc-tex-s) (d+ t-tex inc-tex-t))
    (mesh:vertex map world-x fx world-z
		 :gen-normal nil :gen-triangle t
		 :compact-vertices t :manifoldp nil)))

(defmethod push-decal-weights-text-coord ((object terrain-chunk) s-coord t-coord)
  (vector-push-extend (vec2 s-coord t-coord) (text-coord-weights object)))

(defmethod set-decals-triangle-attribute ((object terrain-chunk))
  (let* ((first-triangle (elt (triangles object) 0))
	 (c-index        (- (length (text-coord-weights object)) 1))
	 (b-index        (- (length (text-coord-weights object)) 2))
	 (a-index        (- (length (text-coord-weights object)) 3))
	 (indices        (uivec a-index b-index c-index)))
    (set-custom-attribute first-triangle +attribute-decal-texture+ indices)))

(defmethod build-mesh ((object terrain-chunk))
  "                    d
             a\--------\--------+ ...
	      |\-      |\-      |
	      |  \-    |  \-    |
	      |    \-  |    \-  |
	      |      \-|c     \-|
	     b\--------\--------\ ...
	      |\-      |\-      |
	      |  \-    |  \-    |
	      |    \-  |    \-  |
	      |      \-|      \-|
	      +--------\--------\ ...
"
  (with-accessors ((heightmap heightmap) (lookup-tile-triangle lookup-tile-triangle)) object
    (let* ((d-width  (desired (matrix:width heightmap)))
	   (d-height (desired (matrix:height heightmap)))
	   (inc-x   (d/ 1.0 (d* d-width  +terrain-chunk-size-scale+)))
	   (inc-z   (d/ 1.0 (d* d-height +terrain-chunk-size-scale+)))
	   (inc-tex 0.1))
      (setf lookup-tile-triangle
	    (matrix:gen-matrix-frame (* (truncate +terrain-chunk-size-scale+)
					(matrix:width heightmap))
				     (* (truncate +terrain-chunk-size-scale+)
					(matrix:height heightmap))))
      (loop
	 for x     from 0.0 below 1.0 by inc-x
	 for s-tex from 0.0           by inc-tex do
	   (loop
	      for z     from 0.0 below 1.0 by inc-z
	      for t-tex from 0.0           by inc-tex do
	      ;; a
	      ;;put texture coordinates for decals
		(push-decal-weights-text-coord object x z)
		(%put-vertex object x z s-tex t-tex 0.0 0.0 0.0 0.0)
	      ;; b
		(push-decal-weights-text-coord object x (d+ z inc-z))
		(%put-vertex object x z s-tex t-tex 0.0 inc-z 0.0 inc-tex)
	      ;; c
		(push-decal-weights-text-coord object (d+ x inc-x) (d+ z inc-z))
		(%put-vertex object x z s-tex t-tex inc-x inc-z inc-tex inc-tex)
	      ;; assign texture-decals coordinates for triangle a b c
		(set-decals-triangle-attribute object)
	      ;; a
		(push-decal-weights-text-coord object x z)
		(%put-vertex object x z s-tex t-tex 0.0 0.0 0.0 0.0)
	      ;; c
		(push-decal-weights-text-coord object (d+ x inc-x) (d+ z inc-z))
		(%put-vertex object x z s-tex t-tex inc-x inc-z inc-tex inc-tex)
	      ;; d
		(push-decal-weights-text-coord object (d+ x inc-x) z)
		(%put-vertex object x z s-tex t-tex inc-x 0.0 inc-tex 0.0)
	      ;; assign texture-decals coordinates for triangle a c d
		(set-decals-triangle-attribute object)
	      ;; assign pick weight value
		(loop repeat 2 do
		     (push-pickable-attribute object 0.0))
		(let ((pick-index (- (length (pick-overlay-values object)) 1)))
		  (setup-pickable-attribute object
					    :triangle-index 0
					    :pick-index pick-index)
		  (setup-pickable-attribute object :triangle-index 1
					    :pick-index (1- pick-index))))))))

(defmethod build-mesh-dbg ((object terrain-chunk))
  (with-accessors ((heightmap heightmap)) object
    (let* ((res (matrix:gen-matrix-frame (* (truncate +terrain-chunk-size-scale+)
					    (matrix:width heightmap))
					 (* (truncate +terrain-chunk-size-scale+)
					    (matrix:height heightmap))))
	   (d-width  (desired (matrix:width heightmap)))
	   (d-height (desired (matrix:height heightmap)))
	   (inc-x (d/ 1.0 (d* d-width  +terrain-chunk-size-scale+)))
	   (inc-z (d/ 1.0 (d* d-height +terrain-chunk-size-scale+))))
      (loop for x from 0.0 below 1.0 by inc-x do
	   (loop for z single-float from 0.0 below 1.0 by inc-z do
		(setf (matrix:matrix-elt res
					 (ceiling (* z (1- (* +terrain-chunk-tile-size+
							      (matrix:height heightmap)))))
					 (ceiling (* x (1- (* +terrain-chunk-tile-size+
							      (matrix:width  heightmap))))))
		      (matrix:sample@ heightmap x z
				      :behaivour-on-border-fn #'matrix:confine-coord
				      :interpolation t
				      :clamp t
				      :interpolate-fn #'(lambda (w a b) (dlerp w a b))))))

      res)))

(defmethod clip-triangles ((object terrain-chunk) aabb predicate)
  (setf (triangles object)
	(lparallel:premove-if
	 #'(lambda (triangle)
	     (funcall predicate
		      #'identity
		      (map 'vector
			   #'(lambda (idx)
			       (2d-utils:inside-aabb2-p aabb
							(elt (elt (vertices object) idx) 0)
							(elt (elt (vertices object) idx) 2)))
			   (vertex-index triangle))))
	 (triangles object))))

(defmethod nclip-with-aabb ((object terrain-chunk) aabb &key
							  (clip-if-inside t)
							  (regenerate-rendering-data t))
  (let ((predicate (if clip-if-inside
		       #'every
		       #'notevery)))
    (clip-triangles object aabb predicate)
    (when regenerate-rendering-data
      (prepare-for-rendering object))
    object))

(defmethod clip-with-aabb ((object terrain-chunk) aabb &key
							 (clip-if-inside t)
							 (regenerate-rendering-data t))
  (let* ((results   (copy-flat object))
	 (predicate #'notevery))
    (setf (lookup-tile-triangle results)
	  (matrix:submatrix (lookup-tile-triangle results)
			    (coord-chunk->matrix (aabb2-min-x aabb))
			    (coord-chunk->matrix (aabb2-min-y aabb))
			    (f- (coord-chunk->matrix (aabb2-max-x aabb))
				(coord-chunk->matrix (aabb2-min-x aabb)))
			    (f- (coord-chunk->matrix (aabb2-max-y aabb))
				(coord-chunk->matrix (aabb2-min-y aabb)))))
    (if (not clip-if-inside)
	(let ((mat (lookup-tile-triangle results))
	      (nw-triangles '()))
	  (matrix:loop-matrix (mat x y)
	    (when (matrix:matrix-elt mat y x)
	      (push (triangle-1 (matrix:matrix-elt mat y x)) nw-triangles)
	      (push (triangle-2 (matrix:matrix-elt mat y x)) nw-triangles)))
	  (setf (triangles results) nw-triangles))
	(clip-triangles results aabb predicate))
    (remove-orphaned-vertices results)
    (reset-aabb results)
    (when regenerate-rendering-data
      (prepare-for-rendering results))
    results))

(defmethod triangle-matrix-elt ((object terrain-chunk) row col)
  (with-accessors ((aabb aabb) (triangle triangles)) object
    (let* ((min-x  (min-x aabb))
	   (min-z  (min-z aabb))
	   (max-x  (max-x aabb))
	   (max-z  (max-z aabb))
	   (width  (truncate (coord-chunk->matrix (d- max-x min-x))))
	   (height (truncate (coord-chunk->matrix (d- max-z min-z)))))
      (elt (triangles object)
	   (f+ (f* width (f- height row) (f- width col)))))))

(defmethod approx-terrain-height ((object terrain-chunk) x z)
  "x y in world space"
  (with-accessors ((lookup-tile-triangle lookup-tile-triangle)) object
    (let* ((x-cost (misc:coord-chunk->costs x))
	   (y-cost (misc:coord-chunk->costs z))
	   (lookup-tile-coord (cost-coord->lookup-tile object y-cost x-cost))
	   (lookup-tile       (matrix:matrix-elt (pickable-mesh:lookup-tile-triangle object)
						 (elt lookup-tile-coord 1)
						 (elt lookup-tile-coord 0)))
	   (first-triangle           (elt (triangles object)
					  (pickable-mesh:index-tr-1 lookup-tile)))
	   (first-triangle-indices   (vertex-index first-triangle))
	   (first-triangle-vertices  (vector (find-value-by-index object
								  (elt first-triangle-indices 0)
								  :what :vertex)
					     (find-value-by-index object
								  (elt first-triangle-indices 1)
								  :what :vertex)
					     (find-value-by-index object
								  (elt first-triangle-indices 2)
								  :what :vertex)))
	   (barycenter-first-triangle (triangle-centroid (elt first-triangle-vertices 0)
							 (elt first-triangle-vertices 1)
							 (elt first-triangle-vertices 2)))
	   (second-triangle           (elt (triangles object)
					   (pickable-mesh:index-tr-1 lookup-tile)))
	   (second-triangle-indices   (vertex-index second-triangle))
	   (second-triangle-vertices  (vector (find-value-by-index object
								   (elt second-triangle-indices 0)
								   :what :vertex)
					      (find-value-by-index object
								   (elt second-triangle-indices 1)
								   :what :vertex)
					      (find-value-by-index object
								   (elt second-triangle-indices 2)
								   :what :vertex)))
	   (barycenter-second-triangle (triangle-centroid (elt second-triangle-vertices 0)
							  (elt second-triangle-vertices 1)
							  (elt second-triangle-vertices 2))))
      (elt (vec-average barycenter-first-triangle barycenter-second-triangle) 1))))

(defun make-terrain-chunk (map shaders &key (generate-rendering-data nil))
  (let ((res (make-instance 'terrain-chunk:terrain-chunk
			    :heightmap        (random-terrain:matrix map)
			    :compiled-shaders shaders)))
    (setf (decal-weights res) (random-terrain:texture-weights map))
    (setf (texture:use-mipmap (texture-road-decal res)) t)
    (setf (texture:interpolation-type (texture-road-decal res)) :linear)
    (texture:prepare-for-rendering (texture-road-decal res))
    (setf (texture:interpolation-type (decal-weights res)) :linear)
    (terrain-chunk:build-mesh res)
    (texture:prepare-for-rendering (decal-weights res))
    (when generate-rendering-data
      (mesh:prepare-for-rendering res))
    (setf (mesh:render-normals res) nil)
    res))
