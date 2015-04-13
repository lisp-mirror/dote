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

(in-package :kd-tree)
	  
(defclass kd-tree-node (node) 
  ((split-plane
    :initform 0
    :initarg :split-plane
    :accessor split-plane)
   (maximum-split-plane 
    :initform 3
    :initarg :maximum-split-plane
    :accessor maximum-split-plane)))

(defgeneric 3d-query-aabb (object aabb &key inside-aabb-fn key just-peek-first))

(defgeneric 3d-query-range (object datum range-x range-y range-z &key inside-aabb-fn key key-datum 
								   just-peek-first))

(defgeneric p3d-query-range (object datum range-x range-y range-z &key inside-aabb-fn key key-datum 
								    just-peek-first))

(defun make-kd-node (data left right parent split-plane maximum-split-plane)
  (make-instance 'kd-tree-node :left left :right right 
		 :data data :parent parent
		 :split-plane split-plane
		 :maximum-split-plane maximum-split-plane))
  
(defun make-kd-leaf (parent split-plane maximum-split-plane)
  (make-instance 'kd-tree-node :parent parent :split-plane split-plane
		 :maximum-split-plane maximum-split-plane
		 :left nil :right nil))

(defun make-root-kd-node (datum maximum-split-plane)
  (let* ((tree (make-kd-node datum nil nil nil -1 maximum-split-plane))
	 (l-leaf (make-kd-leaf tree 1 maximum-split-plane))
	 (r-leaf (make-kd-leaf tree 1 maximum-split-plane)))
    (setf (left tree) l-leaf
	  (right tree) r-leaf)
    tree))

(defun kd-tree-compare (a b dim)
  (num:d< (elt a dim) (elt b dim)))

(defun kd-tree-equal-insert (a b)
  (declare (ignore a b))
  nil)

(defmethod node->dot ((object kd-tree-node))
  (labels ((nodes ()
	     (append
	      (list
	       `(:node ((:id ,(format nil "~a" (data object)))
			(:label ,(format nil "~ap~a" (data object)
					 (split-plane object)))
			(:style "filled")
			(:fillcolor "#ffffff"))))
	      (if (not (leafp (left object)))
		(node->dot (left object))
		(list
		 `(:node ((:id ,(format nil "nil-l~a" (data object)))
			  (:label "nil")
			  (:style "filled")
			  (:fillcolor "#ffffff")))))
	      (if (not (leafp (right object)))
		(node->dot (right object))
		(list
		 `(:node ((:id ,(format nil "nil-r~a" (data object)))
			  (:label "nil")
			  (:style "filled")
			  (:fillcolor "#ffffff")))))))
	   (edges ()
	     (append 
	      (if (data (left object))
		(list `(:edge 
			((:from ,(format nil "~a" (data object)))
			 (:to ,(format nil "~a" (data (left object)))))))
		(list `(:edge 
			((:from ,(format nil "~a" (data object)))
			 (:to ,(format nil "nil-l~a" (data object)))))))
		
	      (if (data (right object))
		(list `(:edge 
			((:from ,(format nil "~a" (data object)))
			 (:to ,(format nil "~a" (data (right object)))))))
		(list `(:edge 
			((:from ,(format nil "~a" (data object)))
			 (:to ,(format nil "nil-r~a" (data object))))))))))
    (append (nodes) (edges))))

(defmethod insert ((object kd-tree-node) datum &key (key #'identity) (key-datum #'identity)
						 (compare #'<) (equal #'=) (level 0))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (function key key-datum compare equal))
  (declare (fixnum level))
  (labels ((%insert (node datum key key-datum compare equal level)
	     (declare (optimize (debug 0) (safety 0) (speed 3)))
	     (declare (function key key-datum compare equal))
	     (declare (fixnum level))
	     (if (leafp node)
		 (let* ((maximum-split-plane (the fixnum (maximum-split-plane node)))
			(new-node (make-kd-node datum nil nil (parent node) level
						maximum-split-plane))
			(l-leaf (make-kd-leaf new-node (num:f+ level 1) maximum-split-plane))
			(r-leaf (make-kd-leaf new-node (num:f+ level 1) maximum-split-plane)))
		   (declare (fixnum maximum-split-plane))
		   (declare (kd-tree-node new-node l-leaf r-leaf))
		   (setf (data new-node) datum
			 (left new-node) l-leaf
			 (right new-node) r-leaf)
		   new-node)
		 (let ((maximum-split-plane (maximum-split-plane node)))
		   (declare (fixnum maximum-split-plane))
		   (cond
		     ((funcall equal (%key key (data node)) (%key key-datum datum))
		      node)
		     ((funcall compare (%key key-datum datum)
			       (%key key (data node)) level)
		      (let ((new-node
			     (make-kd-node (data node)
					   (%insert (left node) datum key key-datum
						    compare equal 
						    (mod (1+ level) maximum-split-plane))
					   (right node) (parent node) level
					   maximum-split-plane)))
			(setf (parent (right new-node)) new-node
			      (parent (left new-node)) new-node)
			new-node))
		     (t
		      (let ((new-node
			     (make-kd-node (data node) 
					   (left node)
					   (%insert (right node) datum key key-datum
						    compare equal 
						    (mod (1+ level) maximum-split-plane))
					   (parent node)
					   level
					   maximum-split-plane)))
			(setf (parent (right new-node)) new-node
			      (parent (left new-node)) new-node)
			new-node)))))))
    (%insert object datum key key-datum compare equal level)))

(defun half-3d-box (p dim aabb-min aabb-max)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec p aabb-min aabb-max))
  (declare ((unsigned-byte 8) dim))
  (let ((more (num:d< (elt p dim) (elt aabb-max dim))) ;; go right
	(less (num:d>= (elt p dim) (elt aabb-min dim)))) ;; go left
    (if (and more less)
	:both
	(if more
	    :right
	    :left))))

(defun test-half-box ()
  (let ((min (vec -10.0 -10.0 -10.0))
	(max (vec 10.0 10.0 10.0))
	(p (vec 10.0 -11.0 0.0)))
    (half-3d-box p 1 min max)))

(defun inside-2d-aabb-p (aabb position)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (aabb aabb))
  (declare (vec position))
  (and
   (num:d>= (elt position 0) (num:desired (min-x aabb)))
   (num:d<= (elt position 0) (num:desired (max-x aabb)))
   (num:d>= (elt position 1) (num:desired (min-y aabb)))
   (num:d<= (elt position 1) (num:desired (max-y aabb)))))

(defmethod 3d-query-aabb ((object kd-tree-node) (aabb aabb) &key
							      (inside-aabb-fn #'insidep)
							      (key #'identity)
							      (just-peek-first nil))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (kd-tree-node object))
  (declare (aabb aabb))
  (declare (function inside-aabb-fn key))
  (declare (boolean just-peek-first))
  (let* ((aabb-min (aabb-p1 aabb))
	 (aabb-max (aabb-p2 aabb))
	 (results '()))
    (labels ((%query (node)
	       (declare (optimize (debug 0) (safety 0) (speed 3)))
	       (declare (kd-tree-node node))
	       (let ((position (funcall key (data node))))
		 (when (not (leafp node))
		   (let ((half-box (half-3d-box position (split-plane node)
						aabb-min aabb-max)))
		     (when (funcall inside-aabb-fn aabb position)
		       (push (data node) results)
		       (when just-peek-first (return-from 3d-query-aabb results)))
		     (case half-box
		       (:both
			(%query (left node))
			(%query (right node)))
		       (:left 
			(%query (left node)))
		       (:right
			(%query (right node)))))))))
      (%query object)
      results)))

(defmethod 3d-query-range ((object kd-tree-node) datum range-x range-y range-z
			   &key (inside-aabb-fn #'insidep) (key #'identity) (key-datum #'identity)
			   (just-peek-first nil))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (kd-tree-node object))
  (declare (single-float range-x range-y range-z))
  (declare (function inside-aabb-fn key key-datum))
  (declare (boolean just-peek-first))
  (let* ((datum (funcall key-datum datum))
	 (aabb (make-instance 'aabb 
			      :aabb-p1 (vec+ datum (vec (num:d- range-x) 
							(num:d- range-y) 
							(num:d- range-z)))
			      :aabb-p2 (vec+ datum (vec range-x range-y range-z)))))
    (declare (vec datum))
    (declare (aabb aabb))
    (3d-query-aabb object aabb :inside-aabb-fn inside-aabb-fn
		   :just-peek-first just-peek-first :key key)))
	 
(defmethod p3d-query-range ((object kd-tree-node) datum range-x range-y range-z 
			    &key
			      (inside-aabb-fn #'insidep) (key #'identity)
			      (key-datum #'identity)
			      (just-peek-first nil))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (kd-tree-node object))
  (declare (single-float range-x range-y range-z))
  (declare (function key key-datum inside-aabb-fn))
  (declare (boolean just-peek-first))
  (let* ((datum (funcall key-datum datum))
	 (aabb (make-instance 'aabb 
			      :aabb-p1 (vec+ datum (vec (num:d- range-x) 
							(num:d- range-y) 
							(num:d- range-z)))
			      :aabb-p2 (vec+ datum (vec range-x range-y range-z))))
	 (aabb-min (aabb-p1 aabb))
	 (aabb-max (aabb-p2 aabb))
	 (results '()))
    (labels ((%query (node)
	       (declare (optimize (debug 0) (safety 0) (speed 3)))
	       (declare (kd-tree-node node))
	       (let ((position (funcall key (data node))))
		 (when (not (leafp node))
		   (let ((half-box (half-3d-box position (split-plane node)
						aabb-min aabb-max)))
		     (when (funcall inside-aabb-fn aabb position)
		       (push (data node) results)
		       (when just-peek-first (return-from p3d-query-range results)))
		     (case half-box
		       (:both
			(let ((l (lparallel:future (%query (left node))))
			      (r (lparallel:future (%query (right node)))))
			  (lparallel:force l)
			  (lparallel:force r)))
		       (:left 
			(%query (left node)))
		       (:right
			(%query (right node)))))))))
      (%query object)
      results)))
