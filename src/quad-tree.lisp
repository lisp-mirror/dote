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

(in-package :quad-tree)

(defclass quad-tree ()
  ((nw
    :initform nil
    :initarg :nw
    :accessor nw)
   (ne
    :initform nil
    :initarg :ne
    :accessor ne)
   (sw
    :initform nil
    :initarg :sw
    :accessor sw)
   (se
    :initform nil
    :initarg :se
    :accessor se)
   (parent
    :initform nil
    :initarg :parent
    :accessor parent)
   (aabb
    :initform +vec4-zero+
    :initarg :aabb
    :accessor aabb)
   (data
    :initform (misc:make-fresh-array 0 nil 'entity:entity nil)
    :initarg :data
    :accessor data)))

(defmethod initialize-instance :after ((object quad-tree) &key &allow-other-keys)
  (with-accessors ((aabb aabb)) object
    (assert (and (vec4p aabb) (not (null aabb))))))

(defmethod print-object ((object quad-tree) stream)
  (with-accessors ((aabb aabb) (nw nw) (ne ne) (sw sw) (se se) (data data)) object
    (format stream "aabb ~a data ~a nw ~a sw ~a se ~a ne ~a~%~%~%~%~%" aabb
	    data nw sw se ne)))

(defmethod leafp ((object quad-tree))
  (with-accessors ((nw nw) (ne ne) (sw sw) (se se)) object
    (not (or nw ne sw se))))

(defun make-leaf-quad-tree (aabb parent)
  (make-instance 'quad-tree :aabb aabb :parent parent))

(defun calculate-subaabb (aabb)
  ;;               X
  ;;    +-------------------->
  ;;    |       a     b     c
  ;;    |	+-----+-----+
  ;;    | 	|  u  |  i  |
  ;;    |  	|     | x   |
  ;;    |    h  +-----.-----+ d
  ;; Z  | 	|     |     |
  ;;    |	|  p  |  o  |
  ;;    |	+-----+-----+
  ;;    |       g     f     e
  ;;    |
  ;;    V

  (let* ((a (vec2 (desired (elt aabb 0)) (desired (elt aabb 1))))
	 (c (vec2 (desired (elt aabb 2)) (desired (elt aabb 1))))
	 (e (vec2 (desired (elt aabb 2)) (desired (elt aabb 3))))
	 (g (vec2 (desired (elt aabb 0)) (desired (elt aabb 3))))
	 (b (vec2+ a (vec2* (vec2- c a) 0.5)))
	 (d (vec2+ c (vec2* (vec2- e c) 0.5)))
	 (f (vec2+ g (vec2* (vec2- e g) 0.5)))
	 (h (vec2+ a (vec2* (vec2- g a) 0.5)))
	 (x (vec2+ a (vec2+ (vec2- b a) (vec2- h a))))
	 (u (vec4 (elt a 0) (elt a 1) (elt x 0) (elt x 1)))
	 (i (vec4 (elt b 0) (elt b 1) (elt d 0) (elt d 1)))
	 (o (vec4 (elt x 0) (elt x 1) (elt e 0) (elt e 1)))
	 (p (vec4 (elt h 0) (elt h 1) (elt f 0) (elt f 1))))
    (values u i o p)))

(defgeneric subdivide (object level))

(defgeneric query-aabb2-intersect-p      (object probe))

(defgeneric query-smallest-intersect-aabb (object aabb))

(defgeneric path-to (object &optional path))

(defgeneric node-quadrant (object))

(defgeneric map-quadtree-intersect (object function probe))

(defgeneric rootp (object))

(defmethod subdivide ((object quad-tree) level)
  (with-accessors ((aabb aabb) (nw nw) (ne ne) (sw sw) (se se) (data data)) object
    (if (> level 0)
	(multiple-value-bind (u i o p)
	    (calculate-subaabb aabb)
	  (setf nw (make-leaf-quad-tree u object)
	   	sw (make-leaf-quad-tree p object)
	   	se (make-leaf-quad-tree o object)
	   	ne (make-leaf-quad-tree i object))
	  (subdivide nw (1- level))
	  (subdivide ne (1- level))
	  (subdivide sw (1- level))
	  (subdivide se (1- level)))
	object)))

(defmethod query-aabb2-intersect-p ((object quad-tree) probe)
  (with-accessors ((aabb aabb)) object
    (aabb2-intersect-p probe aabb)))

(defmethod query-smallest-intersect-aabb ((object quad-tree) probe)
  (let ((results '()))
    (labels ((%query (object)
	       (with-accessors ((aabb aabb) (nw nw) (ne ne) (sw sw) (se se)
				(data data)) object
		 (when (aabb2-intersect-p probe aabb)
		   (if (leafp object)
		       (push object results)
		       (let ((intersect-nw (aabb2-intersect-p probe (aabb nw)))
			     (intersect-ne (aabb2-intersect-p probe (aabb ne)))
			     (intersect-sw (aabb2-intersect-p probe (aabb sw)))
			     (intersect-se (aabb2-intersect-p probe (aabb se))))
			 (and intersect-nw (%query nw))
			 (and intersect-ne (%query ne))
			 (and intersect-sw (%query sw))
			 (and intersect-se (%query se))))))))
      (%query object)
      results)))

(defmethod path-to ((object quad-tree) &optional (path '()))
  (if (rootp object)
      (reverse (nconc path (list object)))
      (path-to (parent object) (nconc path (list object)))))

(defmacro %cond-node->pos (object slots)
  `(cond
     ,@(loop for i in slots collect
	    `((eq ,object (,i (parent object))) ,(alexandria:make-keyword i)))))

(defmethod node-quadrant ((object quad-tree))
  (if (rootp object)
      nil
      (%cond-node->pos object (nw ne sw se))))

(defmethod map-quadtree-intersect ((object quad-tree) function probe)
  (with-accessors ((nw nw) (ne ne) (sw sw) (se se)) object
    (funcall function object)
    (when (and nw (query-aabb2-intersect-p nw probe))
      (map-quadtree-intersect nw function probe))
    (when (and ne (query-aabb2-intersect-p ne probe))
      (map-quadtree-intersect ne function probe))
    (when (and sw (query-aabb2-intersect-p sw probe))
      (map-quadtree-intersect sw function probe))
    (when (and se (query-aabb2-intersect-p se probe))
      (map-quadtree-intersect se function probe))))

(defmethod rootp ((object quad-tree))
  (not (parent object)))

(defmacro %cond-push-down (object entity aabb-entity slots)
  `(cond
     ,@(append
	(loop for i in slots collect
	     `((aabb2-inglobe-p (aabb ,i) ,aabb-entity)
	       (push-down (,i ,object) ,entity ,aabb-entity)))
	`((t
	   (vector-push-extend ,entity (data ,object)))))))

(defmethod push-down ((object quad-tree) entity &optional (ent-aabb (entity:aabb-2d entity)))
  (with-accessors ((nw nw) (ne ne) (sw sw) (se se) (data data)) object
    (if (leafp object)
	(vector-push-extend entity (data object))
	(%cond-push-down object entity ent-aabb  (nw ne sw se)))))
