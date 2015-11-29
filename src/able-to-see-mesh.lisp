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

(in-package :able-to-see-mesh)

(defclass able-to-see-mesh (triangle-mesh)
  ((visibility-cone
    :initform (make-instance 'cone)
    :initarg  :visibility-cone
    :accessor visibility-cone)))

(defgeneric other-visible-p (object target))

(defgeneric update-visibility-cone (object))

(defgeneric visible-players (object &key predicate))

(defgeneric other-visible-p (object target))

(defgeneric other-visible-cone-p (object triangle-mesh))

(defgeneric other-visible-ray-p (object target))

(defmethod update-visibility-cone ((object able-to-see-mesh))
  (with-accessors ((visibility-cone visibility-cone)
		   (pos  pos)
		   (dir dir)
		   (aabb aabb)) object
    (setf (cone-apex   visibility-cone) (aabb-center aabb))
    (setf (cone-height visibility-cone) (vec* dir (vec-length (cone-height visibility-cone))))))

(defmethod visible-players ((object able-to-see-mesh) &key (predicate #'identity))
  "A list containing all visible pc satisfing predicate"
  (with-accessors ((dir dir)
		   (pos pos)) object
    (misc:dbg "~a" (visibility-cone object))
    (loop for ent being the hash-value in (game-state:player-entities (state object))
	 when (and (other-visible-p object ent)
		   (funcall predicate ent))
	 collect
       ent)))

(defmethod other-visible-p ((object able-to-see-mesh) (target triangle-mesh))
  (let ((in-cone-p (other-visible-cone-p object target)))
    (when in-cone-p
      (misc:dbg "visible for cone ~a" (id target))
      (if (other-visible-ray-p object target)
	  t
	  nil))))

(defmethod other-visible-cone-p ((object able-to-see-mesh) (target triangle-mesh))
  (with-accessors ((visibility-cone visibility-cone)) object
    (let ((center (aabb-center (aabb target))))
      (point-in-cone-p visibility-cone center))))

(defmethod other-visible-ray-p ((object able-to-see-mesh) (target triangle-mesh))
   (with-accessors ((dir dir)
		   (pos pos)
		   (state state)
		   (visibility-cone visibility-cone)) object
    ;; launch a ray
    (let* ((ray (make-instance 'ray
			       :ray-direction (normalize (vec- (aabb-center (aabb target))
							       (aabb-center (aabb object))))))
	   (world-ref (game-state:fetch-world (state object)))
	   (quad-tree (world:entities world-ref)))
      (loop
	 for dt from 0.0
	 below (vec-length (cone-height visibility-cone))
	 by +visibility-ray-displ-incr+ do
	   (incf (displacement ray) dt)
	   (let* ((ends (ray-ends ray pos))
		  (leaf (quad-tree:query-leaf-in-point quad-tree
						       (vec2 (elt ends 0)
							     (elt ends 2)))))
	     (when leaf
	       (loop for d across (quad-tree:data leaf) do
		    (cond
		      ((typep d 'terrain-chunk:terrain-chunk)
		       (let* ((x-chunk (elt ends 0))
			      (z-chunk (elt ends 2))
			      (y   (game-state:approx-terrain-height@pos state
									 x-chunk
									 z-chunk)))
			 (when (and y (< (elt ends 1) y))
			   (return-from other-visible-ray-p nil))))
		      ((and (typep d 'mesh:door-mesh-shell)
			    (openp d))
		       ;;does nothing, continue to the next iteration
		       )
		      (t
		       (when (insidep (aabb d) (ray-ends ray pos))
			 (if (= (id d) (id target))
			     (progn
			       (return-from other-visible-ray-p t))
			     (when (not (= (id d) (id object)))
			       (return-from other-visible-ray-p nil)))))))))))))
