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

(in-package :md2-mesh)

(alexandria:define-constant +magic-number+ "IDP2"                :test #'string=)

(alexandria:define-constant +version+ 8                          :test #'=)

(alexandria:define-constant +element-type+ '(unsigned-byte 8)    :test 'equalp)

(alexandria:define-constant +size-elem-texture-coord+ 2          :test #'=)

(alexandria:define-constant +size-textures-struct+ 4             :test #'=)

(alexandria:define-constant +size-elem-triangles+ 2              :test #'=)

(alexandria:define-constant +size-triangles-struct+ 12           :test #'=)

(alexandria:define-constant +size-triangles-chunk+ 6             :test #'=)

(alexandria:define-constant +size-frame-name+ 16                 :test #'=)

(alexandria:define-constant +size-vertex-struct+ 4               :test #'=)

(alexandria:define-constant +tag-head-key+ "tag_head"            :test #'string=)

(alexandria:define-constant +tag-left-weapon-key+ "tag_lweapon"  :test #'string=)

(alexandria:define-constant +tag-right-weapon-key+ "tag_rweapon" :test #'string=)

(alexandria:define-constant +default-animation-table+ 
    '((stand 0 39 20) ; name starting-frame ending-frame fps
      (move 40 45 20)
      (attack 46 53 20)
      (bored 123 134 20)
      (pain 54 65 20)
      (death 178 197 20)
      (critical 160 168 20)
      (critical2 84 94 20)
      (critical3 72 83))
  :test #'equalp)

(defmacro define-header-offsets ((&rest names))
  `(misc:define-offset-size md2-mesh md2 
     ,@(loop 
	  for name in names
	  for start from 0 by 4 collect
	    `(,name ,start 4))))

(defclass md2-mesh (triangle-mesh)
  ((frames
    :initform (misc:make-array-frame 0)
    :initarg :frames
    :accessor frames)
   (parsing-errors
    :initform '()
    :initarg :parsing-errors
    :accessor parsing-errors)
   (starting-frame
    :initform 0
    :initarg :starting-frame
    :accessor starting-frame
    :type fixnum)
   (end-frame
    :initform 39
    :initarg :end-frame
    :accessor end-frame
    :type fixnum)
   (tags-table
    :initform '()
    :initarg  :tags-table
    :accessor tags-table
    :type cons)
   (tags-matrices
    :initform '()
    :initarg  :tags-matrices
    :accessor tags-matrices
    :type cons)
   (tag-key-parent
    :initform +tag-head-key+
    :initarg  :tag-key-parent
    :accessor tag-key-parent
    :type string)
   (animation-table
    :initform +default-animation-table+
    :initarg :animation-table
    :accessor animation-table)
   (current-frame-offset
    :initform 0
    :initarg :current-frame-offset
    :accessor current-frame-offset
    :type fixnum)
   (current-time
    :initform 0
    :initarg :current-time
    :accessor current-time
    :type single-float)
   (fps
    :initform 20
    :initarg :fps
    :accessor fps
    :type single-float)
   (stop-animation
    :initform nil
    :initarg :stop-animation
    :accessor stop-animation)
   (cycle-animation
    :initform t
    :initarg :cycle-animation
    :accessor cycle-animation)))
 
(defgeneric push-errors (object the-error))

(defgeneric load (object file))

(defgeneric load-texture (object file))

(defgeneric load-animations (object file))

(defgeneric load-tags (object file))

(defgeneric bind-vbo (object &optional refresh))

(defgeneric set-animation (object animation))

(defgeneric %alloc-arrays (object))

(defmethod destroy :after ((object md2-mesh)))
 
(define-header-offsets (magic-number
			version
			texture-width
			texture-height
			frame-size
			num-texture-names
			num-vertices
			num-texture-coords
			num-triangles
			num-gl-cmds
			num-frames
			offset-texture-names
			offset-texture-coords
			offset-triangles
			offset-frames
			offset-gl-cmds
			offset-ends))

(defmacro define-header-function ((&rest names))
  `(progn
     ,@(loop for name in names collect
	    `(misc:define-parse-header-chunk 
		 (,name 
		  ,(alexandria:format-symbol t "~@:(+md2-~a-offset+~)" name)
		  ,(alexandria:format-symbol t "~@:(+md2-~a-size+~)" name)
		  md2-mesh nil)))))

(define-header-function (magic-number
			 version
			 texture-width
			 texture-height
			 frame-size
			 num-texture-names
			 num-vertices
			 num-texture-coords
			 num-triangles
			 num-gl-cmds
			 num-frames
			 offset-texture-names
			 offset-texture-coords
			 offset-triangles
			 offset-frames
			 offset-gl-cmds
			 offset-ends))

(defmethod push-errors ((object md2-mesh) the-error)
  (push the-error (parsing-errors object)))

(defun read-texture-coords (stream offset size w-texture h-texture)
  (let ((raw-texture (misc:read-array stream size :offset offset)))
    (declare ((simple-array (unsigned-byte 8)) raw-texture))
    (loop 
       for i from 0 below (1- (length raw-texture)) by +size-textures-struct+ collect
	 (list 
	  (num:d/ (num:desired (misc:2byte->word (elt raw-texture i)
						 (elt raw-texture (+ i 1))))
		  (num:desired w-texture))
	  (num:d/ (num:desired (misc:2byte->word (elt raw-texture (+ i 2))
						 (elt raw-texture (+ i 3))))
		  (num:desired h-texture))))))

(defun read-triangles-index (stream offset size)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (let ((raw-triangles (misc:read-array stream size :offset offset)))
    (declare ((simple-array (unsigned-byte 8)) raw-triangles))
    (alexandria:flatten
     (loop 
	for i from 0 below (1- (length raw-triangles)) by +size-triangles-struct+ collect
	  (loop 
	     for j from i below (+ i +size-triangles-struct+) by +size-elem-triangles+ collect
	       (misc:2byte->word (elt raw-triangles j)
				 (elt raw-triangles (+ j 1))))))))

(defstruct md2-vertex
  pos 
  normal)

(defparameter *read-frame-queue* (lparallel.queue:make-queue :initial-contents '(t) 
							     :fixed-capacity 1))

(defun read-frame (stream num-vertices offset)
  (lparallel.queue:pop-queue *read-frame-queue*)
  (file-position stream offset)
  (let* ((scale-x (misc:read-ieee-float-32 stream))
	 (scale-y (misc:read-ieee-float-32 stream))
	 (scale-z (misc:read-ieee-float-32 stream))
	 (translate-x (misc:read-ieee-float-32 stream))
	 (translate-y (misc:read-ieee-float-32 stream))
	 (translate-z (misc:read-ieee-float-32 stream))
	 (name (text-utils:clean-unprintable-chars
		(misc:bytes->string (misc:read-list stream +size-frame-name+))))
	 (vertices-normals (misc:read-array stream (* +size-vertex-struct+ num-vertices)))
	 (vertices (misc:make-array-frame (floor (/ (length vertices-normals) 4))
					  nil 'md2-vertex t))
	 
	 (normals (misc:make-array-frame (floor (/ (length vertices-normals) 4))
					  nil 'vec t)))
    (loop 
       for i from 0 below (length vertices-normals) by 4 
       for j from 0 by 1 do
	 (setf (elt vertices j)
	       (make-md2-vertex 
		:pos (vec 
		      (+ (* (elt vertices-normals i) scale-x) translate-x)
		      (+ (* (elt vertices-normals (+ i 1)) scale-y) translate-y)
		      (+ (* (elt vertices-normals (+ i 2)) scale-z) translate-z)))))
    (loop 
       for i from 3 below (length vertices-normals) by 4
       for j from 0 by 1 do	 
	 (setf (elt normals j)
	       (elt +md2-normal-lut+ (elt vertices-normals i))))
    (dotimes (ct (length vertices))
      (setf (md2-vertex-normal (elt vertices ct)) (elt normals ct)))
    (lparallel.queue:push-queue t *read-frame-queue*)
    (values vertices name)))

(defmethod %alloc-arrays ((object md2-mesh))
  (with-accessors ((renderer-data-vertices renderer-data-vertices)
		   (renderer-data-count-vertices renderer-data-count-vertices)
		   (renderer-data-texture renderer-data-texture)
		   (renderer-data-count-texture renderer-data-count-texture)
		   (renderer-data-normals renderer-data-normals)
		   (renderer-data-count-normals renderer-data-count-normals)
		   (renderer-data-tangents renderer-data-tangents)
		   (renderer-data-count-tangents renderer-data-count-tangents)
		   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
		   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
		   (starting-frame starting-frame)
		   (triangles-mesh triangles)
		   (end-frame end-frame)
		   (frames frames)) object
    (let ((start-frame (elt frames starting-frame)))
      (setf renderer-data-vertices (gl:alloc-gl-array
				    :float
				    (mesh:renderer-data-count-vertices start-frame))
	    renderer-data-texture (gl:alloc-gl-array
				   :float
				   (mesh:renderer-data-count-texture start-frame))
	    renderer-data-normals (gl:alloc-gl-array
				   :float
				   (mesh:renderer-data-count-normals start-frame))
	    renderer-data-tangents (gl:alloc-gl-array
				    :float
				    (mesh:renderer-data-count-tangents start-frame))
	    renderer-data-aabb-obj-space (gl:alloc-gl-array :float +aabb-vertex-count+)
	    renderer-data-count-vertices (mesh:renderer-data-count-vertices start-frame)
	    renderer-data-count-texture (mesh:renderer-data-count-texture start-frame)
	    renderer-data-count-normals (mesh:renderer-data-count-normals start-frame)
	    renderer-data-count-tangents (mesh:renderer-data-count-tangents start-frame))
      (gl-utils:copy-gl-array (mesh:renderer-data-texture start-frame)
			      renderer-data-texture 
			      renderer-data-count-texture)
      (gl-utils:copy-gl-array (mesh:renderer-data-tangents start-frame)
			      renderer-data-tangents 
			      renderer-data-count-tangents)
      (setf triangles-mesh (triangles start-frame)
	    renderer-data-normals-obj-space
	    (gl:alloc-gl-array :float (normals-obj-space-vertex-count object))))))

(defmethod load ((object md2-mesh) file)
  (with-accessors ((renderer-data-vertices renderer-data-vertices)
		   (renderer-data-count-vertices renderer-data-count-vertices)
		   (renderer-data-texture renderer-data-texture)
		   (renderer-data-count-texture renderer-data-count-texture)
		   (renderer-data-normals renderer-data-normals)
		   (renderer-data-count-normals renderer-data-count-normals)
		   (renderer-data-tangents renderer-data-tangents)
		   (renderer-data-count-tangents renderer-data-count-tangents)
		   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
		   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
		   (starting-frame starting-frame)
		   (triangles-mesh triangles)
		   (end-frame end-frame)
		   (frames frames)) object
      (with-open-file (stream file :if-does-not-exist :error :element-type +element-type+)
	(macrolet ((parse-header-integer (name)
		     `(misc:byte->int 
		       (,(alexandria:format-symbol t "~@:(parse-~a~)" name)
			 object stream))))
	  (let ((magic-num (misc:bytes->string (parse-magic-number object stream))))
	    (if (string= magic-num +magic-number+)
		(let* ((*read-frame-queue* (lparallel.queue:make-queue 
					       :initial-contents '(t)
					       :fixed-capacity 1))
		       (version (parse-header-integer version))
		       (texture-width (parse-header-integer texture-width))
		       (texture-height (parse-header-integer texture-height))
		       (frame-size (parse-header-integer frame-size))
		       (num-texture-names (parse-header-integer num-texture-names))
		       (num-vertices (parse-header-integer num-vertices))
		       (num-texture-coords (parse-header-integer num-texture-coords))
		       (num-triangles (parse-header-integer num-triangles))
		       (num-frames (parse-header-integer num-frames))
		       (offset-texture-coords (parse-header-integer offset-texture-coords))
		       (offset-triangles (parse-header-integer offset-triangles))
		       (offset-frames (parse-header-integer offset-frames))
		       (texture-coords (read-texture-coords stream offset-texture-coords
							    (* +size-textures-struct+ 
							       num-texture-coords)
							    texture-width texture-height))
		       ;; triangles = (v v v tex tex tex)
		       (triangles (read-triangles-index stream offset-triangles
							(* +size-triangles-struct+
							   num-triangles))))
		  (declare (ignore version num-texture-names)) 
		  (setf frames (misc:make-array-frame num-frames nil 'triangle-mesh t))
		  (file-position stream offset-frames)
		    (lparallel:pdotimes (count num-frames)
		      (let ((vertices (read-frame stream num-vertices (+ offset-frames
									 (* count frame-size)))))
			(let ((frame-mesh (make-instance 'triangle-mesh)))
			  (loop 
			     for i from 0 below (length triangles) by +size-triangles-chunk+ do
			     ;; triangles = (v v v tex tex tex)
			     ;; first triangle
			       (texel-v frame-mesh (nth (nth (+ i 5) triangles) texture-coords))
			       (normal-v frame-mesh (md2-vertex-normal 
						     (elt 
						      vertices
						      (nth (+ i 2) triangles))))
			       (tangent-v frame-mesh +y-axe+) ;; no normalmap yet for md2 mesh
			       (vertex-v frame-mesh (md2-vertex-pos 
						     (elt vertices (nth (+ i 2) triangles)))
					 :gen-triangle t :gen-normal nil :manifoldp nil
					 :compact-vertices nil)
			     ;; second one
			       (texel-v frame-mesh (nth (nth (+ i 4) triangles) texture-coords))
			       (normal-v frame-mesh (md2-vertex-normal 
						     (elt
						      vertices
						      (nth (+ i 1) triangles))))
			       (tangent-v frame-mesh +y-axe+)
			       (vertex-v frame-mesh (md2-vertex-pos 
						     (elt 
						      vertices
						      (nth (+ i 1) triangles)))
					 :gen-triangle t :gen-normal nil :manifoldp nil
					 :compact-vertices nil)
			     ;; third
			       (texel-v frame-mesh (nth (nth (+ i 3) triangles) texture-coords))
			       (normal-v frame-mesh (md2-vertex-normal 
						     (elt vertices (nth i triangles))))
			       (tangent-v frame-mesh +y-axe+)
			       (vertex-v frame-mesh (md2-vertex-pos 
						     (elt
						      vertices
						      (nth i triangles)))
					 :gen-triangle t :gen-normal nil :manifoldp nil
					 :compact-vertices nil))
			  (setf (elt frames count) frame-mesh))))
		  (map nil #'prepare-for-rendering (frames object))
		  (%alloc-arrays object))
		(push-errors object ;; (if (string= magic-num +magic-number+) else...
			     (format nil "Wrong magic number ~a expected ~a found instead"
				     +magic-number+ magic-num))))))))

(defmethod load-texture ((object md2-mesh) file)
  (multiple-value-bind (texture errors)
      (texture:get-texture file)
    (if (null errors)
	(progn
	  (setf (texture-object object) texture)
	  (texture:prepare-for-rendering (texture-object object))
	  (loop for i across (frames object) do (setf (texture-object i) texture)))
	(mapc #'(lambda (e) (push-errors object e)) errors))))

(defmethod load-animations ((object md2-mesh) file)
  (when (filesystem-utils:file-exists-p file)
    (with-open-file (stream file)
      (setf (animation-table object) (read stream)))
    (setf (animation-table object)
	  (mapcar #'(lambda (r)
		      (list (first r)
		      	    (coerce (second r) 'fixnum)
		      	    (coerce (third r) 'fixnum)
		      	    (coerce (fourth r) 'single-float)))
		  (animation-table object)))))

(defmethod load-tags ((object md2-mesh) file)
  (with-accessors ((tags-table tags-table) (tags-matrices tags-matrices)) object
    (restart-case
	(progn
	  (setf tags-table (load-tag-file file))
	  (when tags-table
	    (loop for name in tags-table do
		 (push (cons (car name) (sb-cga:identity-matrix)) tags-matrices))))
      (use-value (v) v))))
	    
(defmethod prepare-for-rendering ((object md2-mesh))
  (with-accessors ((vbo vbo) (vao vao)
		   (renderer-data-vertices renderer-data-vertices)
		   (renderer-data-normals renderer-data-normals)
		   (renderer-data-texture renderer-data-texture)
		   (texture-object texture-object)) object
    (destroy object)
    (map nil #'prepare-for-rendering (frames object))
    (setf vbo (gl:gen-buffers +vbo-count+)
	  vao (gl:gen-vertex-arrays +vao-count+))
    (%alloc-arrays object)
    (bind-vbo object nil)))

(defmethod calculate ((object md2-mesh) dt)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (single-float dt))
  (with-accessors ((renderer-data-vertices renderer-data-vertices)
		   (renderer-data-count-vertices renderer-data-count-vertices)
		   (renderer-data-normals renderer-data-normals)
		   (renderer-data-count-normals renderer-data-count-normals)
		   (tags-table tags-table)
		   (tags-matrices tags-matrices)
		   (tag-key-parent tag-key-parent)
		   (frames frames)
		   (starting-frame starting-frame)
		   (end-frame end-frame)
		   (current-frame-offset current-frame-offset)
		   (fps fps)
		   (current-time current-time)
		   (stop-animation stop-animation)
		   (cycle-animation cycle-animation)
		   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
		   (data-aabb renderer-data-aabb-obj-space)) object
    (declare (single-float current-time fps))
    (declare (fixnum end-frame starting-frame current-frame-offset))
    (declare (simple-array frames))
    (declare (string tag-key-parent))
    (declare (list tags-table tags-matrices))
    (unless stop-animation
      (let ((next-time (+ current-time dt))
	    (frames-numbers (the fixnum (1+ (- end-frame starting-frame))))
	    (frame-freq (/ 1.0 fps)))
	(declare (single-float next-time frame-freq))
	(declare (fixnum frames-numbers))
	(if (> next-time frame-freq)
	    (setf current-frame-offset (mod (1+ current-frame-offset) frames-numbers)
		  current-time 0.0)
	    (setf current-time next-time))
	(let* ((next-frame-offset    (mod (the fixnum (1+ current-frame-offset)) frames-numbers))
	       (interpolation-factor (d* fps current-time))
	       (starting-frame-idx   (f+ starting-frame current-frame-offset))
	       (next-frame-idx       (f+ starting-frame next-frame-offset)))
	  (declare (single-float interpolation-factor))
	  (declare (fixnum next-frame-offset starting-frame-idx next-frame-idx))
	  (when (and (not cycle-animation)
		     (= next-frame-offset 0))
	    (setf stop-animation t))
	  (gl-utils:lerp-gl-array (renderer-data-vertices
	  			   (svref frames starting-frame-idx))
	  			  (renderer-data-vertices 
	  			   (svref frames next-frame-idx))
	  			  renderer-data-vertices
	  			  renderer-data-count-vertices
	  			  interpolation-factor)
	  (gl-utils:lerp-gl-array (renderer-data-normals
	  			   (svref frames starting-frame-idx))
	  			  (renderer-data-normals 
	  			   (svref frames next-frame-idx))
	  			  renderer-data-normals
	  			  renderer-data-count-normals
	  			  interpolation-factor)
	  (when tags-table
	    (loop for i in tags-matrices do
		 (let* ((orn1   (elt (the (simple-array (simple-array * (*)))
					  (find-tag-cdr (car i) tags-table))
				     starting-frame-idx))
			(orn2   (elt (the (simple-array (simple-array * (*)))
					  (find-tag-cdr (car i) tags-table))
				     next-frame-idx))
			(orn (vector (vec-lerp (elt orn1 0) (elt orn2 0) interpolation-factor)
				     (vec-lerp (elt orn1 1) (elt orn2 1) interpolation-factor)
				     (vec-lerp (elt orn1 2) (elt orn2 2) interpolation-factor)
				     (vec-lerp (elt orn1 3) (elt orn2 3) interpolation-factor))))
		   (declare ((simple-array (simple-array single-float (3)) (4)) orn1 orn2))
		   (nsetup-tag-matrix (cdr i) orn))))
	  (when (render-normals object)
	    (gl-utils:lerp-gl-array (renderer-data-normals-obj-space
	  			   (svref frames starting-frame-idx))
	  			  (renderer-data-normals-obj-space
	  			   (svref frames next-frame-idx))
	  			  renderer-data-normals-obj-space
				  (normals-obj-space-vertex-count object)
	  			  interpolation-factor))
	  (when (render-aabb object)
	    (with-slots (aabb) object
	      (reset aabb)
	      (loop for i fixnum from 0 below renderer-data-count-vertices by 3 do
		   (let* ((pos (the (unsigned-byte 32) i))
			  (vert (vec (gl:glaref renderer-data-vertices pos) 
				     (gl:glaref renderer-data-vertices (+ pos 1))
				     (gl:glaref renderer-data-vertices (+ pos 2)))))
		     (expand aabb vert)))
	      (make-data-for-opengl-aabb-obj-space object))))))
    (when (not (mtree-utils:rootp object))
      (let ((tag-matrix (find-tag-cdr tag-key-parent (tags-matrices (mtree-utils:parent object)))))
	(when tag-matrix
	  (setf (model-matrix object) tag-matrix))))
    (bind-vbo object t)
    (do-children-mesh (i object)
      (calculate i dt))))

(defmethod bind-vbo ((object md2-mesh) &optional (refresh nil))
  (with-accessors ((vbo vbo) (vao vao)
		   (renderer-data-tangents renderer-data-tangents)
		   (renderer-data-texture renderer-data-texture)
		   (renderer-data-vertices renderer-data-vertices)
		   (renderer-data-normals renderer-data-normals)
		   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
		   (renderer-data-normals-obj-space renderer-data-normals-obj-space)) object
    ;; vertices
    (gl:bind-buffer :array-buffer (vbo-vertex-buffer-handle vbo))
    (if refresh
	(gl:buffer-sub-data :array-buffer renderer-data-vertices)
	(gl:buffer-data :array-buffer :dynamic-draw renderer-data-vertices))
    ;; normals
    (gl:bind-buffer :array-buffer (vbo-normals-buffer-handle vbo))
    (if refresh
	(gl:buffer-sub-data :array-buffer renderer-data-normals)
	(gl:buffer-data :array-buffer :dynamic-draw renderer-data-normals))
    ;; tangents
    (gl:bind-buffer :array-buffer (vbo-tangents-buffer-handle vbo))
    (if refresh
	(gl:buffer-sub-data :array-buffer renderer-data-tangents)
	(gl:buffer-data :array-buffer :dynamic-draw renderer-data-tangents))
    ;; texture
    (gl:bind-buffer :array-buffer (vbo-texture-buffer-handle vbo))
    (gl:buffer-data :array-buffer :static-draw renderer-data-texture)
    (when (render-normals object)
      (gl:bind-buffer :array-buffer (vbo-normals-object-space-buffer-handle vbo))
      (if refresh
	  (gl:buffer-sub-data :array-buffer renderer-data-normals-obj-space)
	  (gl:buffer-data :array-buffer :dynamic-draw renderer-data-normals-obj-space)))
    (when (render-aabb object)
      (gl:bind-buffer :array-buffer (vbo-aabb-object-space-buffer-handle vbo))
      (if refresh
	  (gl:buffer-sub-data :array-buffer renderer-data-aabb-obj-space)
	  (gl:buffer-data :array-buffer :dynamic-draw renderer-data-aabb-obj-space)))
    (prepare-for-rendering-phong object)))

(defmethod set-animation ((object md2-mesh) animation)
  (with-accessors ((starting-frame starting-frame)
		   (end-frame end-frame)
		   (current-frame-offset current-frame-offset)
		   (fps fps)
		   (current-time current-time)
		   (animation-table animation-table)) object
    (let ((anim-spec (assoc animation animation-table :test #'eql)))
      (when anim-spec
	(setf starting-frame (second anim-spec)
	      end-frame (third anim-spec)
	      fps (fourth anim-spec)
	      current-time 0.0)
	(calculate object 0.0)))))

(defun load-md2-model (modeldir &key
				  (material (make-mesh-material .1 1.0 0.1 0.0 128.0))
				  (mesh-file +model-filename+)
				  (texture-file +model-texture-filename+)
				  (animation-file +model-animations-filename+)
				  (tags-file      nil))
  (let ((model (make-instance 'md2-mesh :render-normals t :render-aabb t)))
    (setf (material-params model) material)
    (load-texture model (res:get-resource-file (text-utils:strcat modeldir texture-file)
					       +models-resource+
					       :if-does-not-exists :error))
    (load-animations model (res:get-resource-file (text-utils:strcat modeldir animation-file)
						  +models-resource+
						  :if-does-not-exists :error))
    (load model (res:get-resource-file (text-utils:strcat modeldir mesh-file)
				       +models-resource+
				       :if-does-not-exists :error))
    (when tags-file
      (load-tags model (res:get-resource-file (text-utils:strcat modeldir tags-file)
					      +models-resource+
					      :if-does-not-exists :error)))
    (misc:dbg "error ~a" (parsing-errors model))
    (prepare-for-rendering model)
    (set-animation model :stand)
    model))

(alexandria:define-constant +magic-num-md2-tag-file '(74 68 80 50) :test #'equalp)

(alexandria:define-constant +tag-file-name-size+ 64 :test #'=)

(define-condition md2-tag-error (text-error)
  ()
  (:report (lambda (condition stream) 
	     (format stream "~a" (text condition)))))

(defclass md2-tag () ())

(misc:define-offset-size md2-mesh tag (id 0 4) (version 4 4) (num-tags 8 4)
			 (num-frames 12 4) (offset-names 16 4) (offset-tags 20 4)
			 (offset-end 24 4) (offset-extract-end 28 4))

(misc:define-parse-header-chunk (tag-id +tag-id-offset+ +tag-id-size+ md2-tag nil))

(misc:define-parse-header-chunk (tag-num +tag-num-tags-offset+ +tag-num-tags-size+ md2-tag nil))

(misc:define-parse-header-chunk (frames-num +tag-num-frames-offset+ +tag-num-frames-size+
					    md2-tag nil))

(misc:define-parse-header-chunk (offset-names +tag-offset-names-offset+
					      +tag-offset-names-size+
					      md2-tag nil))

(misc:define-parse-header-chunk (offset-tags +tag-offset-tags-offset+
					      +tag-offset-tags-size+
					      md2-tag nil))

(defun load-tag-file (file)
  "returns ((\"tag\" . #(rotation1 rotation2 rotation3 origin) ...))"
  (let ((tag-results (make-instance 'md2-tag))
	(results '()))
      (with-open-file (stream file :direction :input :if-does-not-exist :error
			      :element-type '(unsigned-byte 8))
	(if (equalp +magic-num-md2-tag-file (parse-tag-id tag-results stream))
	    (let* ((num-tags     (misc:byte->int (parse-tag-num tag-results stream)))
		   (num-frames   (misc:byte->int (parse-frames-num tag-results stream)))
		   (offset-names (misc:byte->int (parse-offset-names tag-results stream)))
		   (offset-tags  (misc:byte->int (parse-offset-tags tag-results stream)))
		   (names (loop
			    repeat num-tags
			    for seek-pos from offset-names by +tag-file-name-size+ collect
			      (text-utils:clean-unprintable-chars
			       (misc:bytes->string (misc:read-list stream +tag-file-name-size+
								   :offset seek-pos))))))
	      (file-position stream offset-tags)
	      (loop for name in names do
		   (let ((orientation (loop for j from 0 below num-frames collect
					   (vector
					    (vec (misc:read-ieee-float-32 stream) ;; rotation1
						 (misc:read-ieee-float-32 stream)
						 (misc:read-ieee-float-32 stream))
					    (vec (misc:read-ieee-float-32 stream) ;; rotation2
						 (misc:read-ieee-float-32 stream)
						 (misc:read-ieee-float-32 stream))
					    (vec (misc:read-ieee-float-32 stream) ;; rotation3
						 (misc:read-ieee-float-32 stream)
						 (misc:read-ieee-float-32 stream))
					    (vec (misc:read-ieee-float-32 stream) ;; origin
						 (misc:read-ieee-float-32 stream)
						 (misc:read-ieee-float-32 stream))))))
		     (push (cons name
				 (list->simple-array orientation nil
						     '(simple-array
						       (simple-array single-float (3)) (4))))
			   results)))
	      results)
	    (error 'md2-tag-error
		   :text (format nil "Wrong magic number ~a"
				 (parse-tag-id tag-results stream)))))))

(defun tag->matrix (orientation)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare ((simple-array (simple-array single-float (3)) (4)) orientation))
  (let ((orien (elt orientation 0))
	(r1    (elt orientation 1))
	(r2    (elt orientation 2))
	(r3    (elt orientation 3)))
    (matrix (elt r1 0)    (elt r2 1)     (elt r3 2)    (elt orien 0)
	    (elt r1 0)    (elt r2 1)     (elt r3 2)    (elt orien 1)
	    (elt r1 0)    (elt r2 1)     (elt r3 2)    (elt orien 2)
	          0.0           0.0            0.0              1.0)))

(defun nsetup-tag-matrix (matrix orientation)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare ((simple-array (simple-array single-float (3)) (4)) orientation))
  (declare (sb-cga:matrix matrix))
  (let ((r1    (elt orientation 0))
	(r2    (elt orientation 1))
	(r3    (elt orientation 2))
	(orien (elt orientation 3)))
    (setf (mref matrix 0 0) (elt r1 0)
	  (mref matrix 1 0) (elt r1 1)
	  (mref matrix 2 0) (elt r1 2)
	  (mref matrix 3 0) 0.0
	  ;; second column
	  (mref matrix 0 1) (elt r2 0)
	  (mref matrix 1 1) (elt r2 1)
	  (mref matrix 2 1) (elt r2 2)
	  (mref matrix 3 1) 0.0
	  ;; third column
	  (mref matrix 0 2) (elt r3 0)
	  (mref matrix 1 2) (elt r3 1)
	  (mref matrix 2 2) (elt r3 2)
	  (mref matrix 3 2) 0.0
	  ;; fourth colum
	  (mref matrix 0 3) (elt orien 0)
	  (mref matrix 1 3) (elt orien 1)
	  (mref matrix 2 3) (elt orien 2)
	  (mref matrix 3 3) 1.0)
    matrix))

(defun find-tag-cdr (key tags-list)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (cdr (find-tag key tags-list)))

(defun find-tag (key tags-list)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (assoc key tags-list :test #'string=))
