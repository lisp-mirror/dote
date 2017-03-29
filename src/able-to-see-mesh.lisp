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

(defgeneric other-faction-visible-players (object))

(defgeneric other-visible-p (object target))

(defgeneric other-visible-cone-p (object triangle-mesh))

(defgeneric other-visible-ray-p (object target))

(defgeneric labyrinth-element-hitted-by-ray (object target))

(defgeneric nonlabyrinth-element-hitted-by-ray (object target))

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
		   (pos pos)
		   (id id)
		   (state state)) object
    ;;(misc:dbg "~a" (visibility-cone object))
    (let ((others (if (faction-player-p state id)
		      (game-state:ai-entities     state)
		      (game-state:player-entities state)))
	  (mines  (if (faction-player-p state id)
		      (game-state:player-entities state)
		      (game-state:ai-entities     state))))
      (nconc
       (loop for ent being the hash-value in others
	  when (and (other-visible-p object ent)
		    (funcall predicate ent))
	  collect ent)
       (loop for ent being the hash-value in mines
	  when (funcall predicate ent)
	  collect ent)))))

(defmethod other-faction-visible-players ((object able-to-see-mesh))
  (visible-players object
                   :predicate #'(lambda (a)
                                  (not (eq (my-faction object)
                                           (my-faction a))))))

(defmethod other-visible-p ((object able-to-see-mesh) (target triangle-mesh))
  (let ((in-cone-p (other-visible-cone-p object target)))
    (when in-cone-p
      ;;(misc:dbg "visible for cone ~a" (id target))
      (if (other-visible-ray-p object target)
	  t
	  nil))))

(defmethod other-visible-cone-p ((object able-to-see-mesh) (target triangle-mesh))
  (with-accessors ((visibility-cone visibility-cone)) object
    (let ((center (aabb-center (aabb target))))
      (point-in-cone-p visibility-cone center))))

(defmethod other-visible-ray-p ((object able-to-see-mesh) (target triangle-mesh))
  (let ((lab-hitted-p    (labyrinth-element-hitted-by-ray object target))
	(nonlab-hitted-p (nonlabyrinth-element-hitted-by-ray object target)))
    ;; (when +debug-mode+
    ;;   (misc:dbg "labyrinth hitted? ~a" lab-hitted-p)
    ;;   (misc:dbg "non labyrinth hitted? ~a" lab-hitted-p))
    (cond
      ((null nonlab-hitted-p)
       nil)
      ((null lab-hitted-p)
       nonlab-hitted-p)
      (t ;hitted both labirinth and player
       (if (d< (displacement lab-hitted-p)
	       (displacement nonlab-hitted-p))
	   nil
	   t)))))

(defmethod nonlabyrinth-element-hitted-by-ray ((object able-to-see-mesh) (target triangle-mesh))
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
		      ((terrain-chunk:terrain-chunk-p d)
		       (let* ((x-chunk (elt ends 0))
			      (z-chunk (elt ends 2))
			      (y   (game-state:approx-terrain-height@pos state
									 x-chunk
									 z-chunk)))
			 (when (and y (< (elt ends 1) y))
			   (return-from nonlabyrinth-element-hitted-by-ray nil))))
		      ((labyrinth-mesh-p d)
		       ;;does nothing, continue to the next iteration
		       )
		      ((arrows:arrowp d)
		       ;;does nothing, continue to the next iteration
		       )
		      ((trap-mesh-shell-p d)
		       ;;does nothing, continue to the next iteration
		       )

		      ((tree-mesh-shell-p d)
		       ;;(misc:dbg "tree trunk ~%~a ~a -> ~a" (tree-trunk-aabb d)
		       ;;	 ends (insidep (tree-trunk-aabb d) ends))
		       (when (and (insidep (aabb d) ends)
				  (insidep (tree-trunk-aabb d) ends))
			 (return-from nonlabyrinth-element-hitted-by-ray nil)))
		      (t
		       (when (insidep (aabb d) (ray-ends ray pos)) ;; O_O
			 (if (= (id d) (id target))
                             (return-from nonlabyrinth-element-hitted-by-ray (values ray d))
			     (when (not (= (id d) (id object)))
			       (return-from
                                nonlabyrinth-element-hitted-by-ray nil)))))))))))))

(defun %blocked-by-ray-p (ray-ends vec-object)
  (loop for a across vec-object do
       (when (insidep (aabb a) ray-ends)
	 (return-from %blocked-by-ray-p a)))
  nil)

(defmethod labyrinth-element-hitted-by-ray ((object able-to-see-mesh) (target triangle-mesh))
  (with-accessors ((dir dir)
		   (pos pos)
		   (state state)
		   (visibility-cone visibility-cone)) object
    ;; launch a ray
    (let* ((ray (make-instance 'ray
			       :ray-direction (normalize (vec- (aabb-center (aabb target))
							       (aabb-center (aabb object))))))
	   (all-labyrinths (loop for l being the hash-value in
				(game-state:labyrinth-entities (state object))
			      collect l)))
      (loop
	 for dt from 0.0
	 below (vec-length (cone-height visibility-cone))
	 by +visibility-ray-displ-incr+ do
	   (incf (displacement ray) dt)
	   (let* ((ends (ray-ends ray pos)))
	     (loop for lab in all-labyrinths do
		  (when (insidep (aabb lab) ends)
		    (let ((walls    (children (wall-instanced    lab)))
			  (windows  (children (window-instanced  lab)))
			  (pillars  (children (pillar-instanced  lab)))
			  (doors-n  (remove-if #'openp (children (door-n-instanced lab))))
			  (doors-s  (remove-if #'openp (children (door-s-instanced lab))))
			  (doors-e  (remove-if #'openp (children (door-e-instanced lab))))
			  (doors-w  (remove-if #'openp (children (door-w-instanced lab))))
			  (tables   (children (table-instanced   lab)))
			  (chairs-n (children (chair-n-instanced lab)))
			  (chairs-s (children (chair-s-instanced lab)))
			  (chairs-e (children (chair-e-instanced lab)))
			  (chairs-w (children (chair-w-instanced lab))))
		      (let ((res (or (%blocked-by-ray-p ends walls)
				     (%blocked-by-ray-p ends windows)
				     (%blocked-by-ray-p ends pillars)
				     (%blocked-by-ray-p ends doors-n)
				     (%blocked-by-ray-p ends doors-s)
				     (%blocked-by-ray-p ends doors-e)
				     (%blocked-by-ray-p ends doors-w)
				     (%blocked-by-ray-p ends tables)
				     (%blocked-by-ray-p ends chairs-n)
				     (%blocked-by-ray-p ends chairs-s)
				     (%blocked-by-ray-p ends chairs-e)
				     (%blocked-by-ray-p ends chairs-w))))
			(when res
			  (return-from labyrinth-element-hitted-by-ray (values ray res))))))))))))
