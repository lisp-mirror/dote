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

(defclass tooltip (triangle-mesh inner-animation)
  ((duration
    :initform 3.0
    :initarg  :duration
    :accessor duration)
   (gravity
    :initform 1.0
    :initarg  :gravity
    :accessor gravity)
   (font-type
    :initform +tooltip-font-handle+
    :initarg  :font-type
    :accessor font-type)
   (font-color
    :initform +damage-color+
    :initarg  :font-color
    :accessor font-color)))

(defmethod initialize-instance :after ((object tooltip) &key &allow-other-keys)
  (setf (use-blending-p object) t))

(defun setup-label-tooltip (host new-label)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (simple-string new-label))
  (time
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
	       (merge-mesh host mesh :manifold nil))))))))

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
  (incf (el-time object) (d* dt (animation-speed object)))
  (bubbleup-modelmatrix object))

(defmethod render ((object tooltip) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((duration duration)
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
		   (view-matrix view-matrix)) object
    (declare (vec4 font-color))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles))
    (with-camera-view-matrix (camera-vw-matrix renderer)
      (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
	(use-program compiled-shaders :tooltip)
	(gl:active-texture :texture0)
	(texture:bind-texture texture-object)
	(uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
	(uniformf  compiled-shaders :duration duration)
	(uniformf  compiled-shaders :vert-displacement-speed +tooltip-v-speed+)
	(uniformf  compiled-shaders :time   el-time)
	(uniformf  compiled-shaders :gravity gravity)
	(uniformfv compiled-shaders :mult-color font-color)
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
	(gl:draw-arrays :triangles 0 (* 3 (length triangles)))
	(gl:disable :blend)))))

(defun make-tooltip (label pos shaders
		     &key
		       (color +damage-color+)
		       (font-type gui:+default-font-handle+)
		       (gravity 1.0))
  (let ((tooltip (make-instance 'billboard:tooltip
				:animation-speed 1.0
				:font-color      color
				:font-type       font-type
				:gravity         gravity)))
    (setf (interfaces:compiled-shaders tooltip) shaders)
    (setf (entity:pos tooltip) pos)
    (setf (label tooltip) label)
    (mesh:prepare-for-rendering tooltip)
    tooltip))

(defgeneric apply-tooltip (object label &key color))

(defmethod apply-tooltip ((object mesh:triangle-mesh) label
			  &key
			    (color +damage-color+)
			    (font-type gui:+default-font-handle+)
			    (gravity 1.0))
  (with-accessors ((ghost ghost)
		   (id id)
		   (state state)) object
    (with-accessors ((recurrent-effects recurrent-effects)
		     (immune-poison-status immune-poison-status)
		     (status status)) ghost
      (game-state:with-world (world state)
	(let ((tooltip (billboard:make-tooltip label
					       (aabb-top-center (aabb object))
					       (compiled-shaders object)
					       :color color
					       :font-type font-type
					       :gravity   gravity)))
	  (world:push-entity world tooltip))))))
