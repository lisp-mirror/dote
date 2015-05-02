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

(in-package :pickable-mesh)

(define-constant +attribute-pick-overlay+           :pick-overlay        :test #'eq)

(define-constant +color-tile-pick-can-move+ (vec4 0.408 0.737 0.945 1.0) :test #'vec4~)

(define-constant +color-tile-pick-cannot-move+ (vec4 1.0 0.0 0.0 1.0)    :test #'vec4~)

(define-constant +pick-color-lerp-weight+      0.2                       :test #'=)

(defclass pickable-mesh (triangle-mesh)
  ((pick-overlay-values
    :initform (misc:make-fresh-array 0 (num:desired 0.0) 'desired-type nil)
    :initarg  :pick-overlay-values
    :accessor pick-overlay-values)
   (renderer-data-count-pick-overlay
    :initform 0
    :initarg  :renderer-data-count-pick-overlay
    :accessor renderer-data-count-pick-overlay)
   (renderer-data-pick-overlay
    :initform nil
    :initarg  :renderer-data-pick-overlay
    :accessor renderer-data-pick-overlay)))

(defmethod clone-into :after ((from pickable-mesh) (to pickable-mesh))
  (setf (pick-overlay-values to) (alexandria:copy-array (pick-overlay-values from)))
  to)

(defmethod clone ((object pickable-mesh))
  (let ((res (make-instance 'pickable-mesh)))
    (clone-into object res)
    res))

(defmethod destroy :after ((object pickable-mesh))
  (with-accessors ((renderer-data-pick-overlay renderer-data-pick-overlay)) object
    (when +debug-mode+
      (misc:dbg "destroy pickable-mesh ~a" (id object)))
    (when renderer-data-pick-overlay
      (gl:free-gl-array renderer-data-pick-overlay)
      (setf renderer-data-pick-overlay nil))))

(defmethod make-data-for-opengl :after ((object pickable-mesh))
  (with-accessors ((renderer-data-count-pick-overlay renderer-data-count-pick-overlay)
		   (renderer-data-pick-overlay renderer-data-pick-overlay)
		   (pick-overlay-values pick-overlay-values)) object
    (let ((weights (gl:alloc-gl-array :float (* 3 (length (triangles object))))))
      (loop
	 for triangle in (triangles object)
	 for ct from 0 by 3                 do
	   (let ((pick-indices (get-custom-attribute triangle +attribute-pick-overlay+)))
	     (loop
		for i across pick-indices
		for offset from 0 by 1 do
		  (let ((weight (elt pick-overlay-values i)))
		    (setf (gl-utils:fast-glaref weights (+ ct offset)) weight)))))
      (setf renderer-data-pick-overlay  weights)
      (setf renderer-data-count-pick-overlay (* 3 (length (triangles object)))))
    ;; setup finalizer
    (let ((gl-arr-weight (slot-value object 'renderer-data-pick-overlay))
	  (id            (slot-value object 'id)))
      (tg:finalize object #'(lambda ()
			      (when +debug-mode+
				(misc:dbg "finalize destroy pickable ~a" id))
			      (gl:free-gl-array gl-arr-weight))))))

(defmethod prepare-for-rendering :after ((object pickable-mesh))
  (with-accessors ((vbo vbo)
 		   (vao vao)
 		   (renderer-data-pick-overlay renderer-data-pick-overlay)) object
    (setf vbo (append vbo (gl:gen-buffers 1))
 	  vao (append vao (gl:gen-vertex-arrays 1)))
    (mesh:make-data-for-opengl object)
    (with-unbind-vao
      ;; pick-weights
      (gl:bind-buffer :array-buffer (alexandria:last-elt vbo))
      (gl:buffer-data :array-buffer :dynamic-draw renderer-data-pick-overlay)
      (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
      (gl:bind-buffer :array-buffer (alexandria:last-elt vbo))
      (gl:vertex-attrib-pointer +attribute-pick-weight-location+ 1 :float 0 0
				(gl-utils:mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-pick-weight-location+))
    object))

(defgeneric push-pickable-attribute (object value))

(defgeneric set-pickable-attribute (object &key triangle-index pick-index))

(defmethod push-pickable-attribute ((object pickable-mesh) value)
  (declare (desired-type value))
  (vector-push-extend value (pick-overlay-values object)))

(defmethod set-pickable-attribute ((object pickable-mesh) &key
							    (triangle-index 0)
							    (pick-index 0))
  (let* ((first-triangle (elt (triangles object) triangle-index))
	 (indices        (uivec pick-index pick-index pick-index)))
    (set-custom-attribute first-triangle +attribute-pick-overlay+ indices)))
