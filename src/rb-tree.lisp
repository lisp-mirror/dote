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

(in-package :rb-tree)

(alexandria:define-constant +red+ :red :test #'eq)

(alexandria:define-constant +black+ :black :test #'eq)

(alexandria:define-constant +color+ :color :test #'eq)

(defclass rb-node (node)
  ((color
    :initarg :color
    :initform :black
    :accessor color)))

(defgeneric balance (object))

(defgeneric left-balance (object))

(defgeneric right-balance (object))

(defmethod node->string ((object rb-node))
  (if (null (data object))
      ""
      (format nil "~a (~a)~% [~a] [~a]" 
	      (data object) 
	      (color object)
	      (node->string (left object))
	      (node->string (right object)))))

(defun make-rb-node (color data left right parent)
  (make-instance 'rb-node :color color :left left :right right :data data :parent parent))
  
(defun make-rb-leaf (color parent)
  (make-instance 'rb-node :color color :parent parent :left nil :right nil))


(defun make-root-rb-node (datum color)
  (let* ((tree (make-rb-node color datum nil nil nil))
	 (l-leaf (make-rb-leaf :black tree))
	 (r-leaf (make-rb-leaf :black tree)))
    (setf (left tree) l-leaf
	  (right tree) r-leaf)
    tree))

(defmethod insert ((object rb-node) datum &key (key #'identity) (key-datum #'identity)
		   (compare #'<) (equal #'=))
  (macrolet ((make-leaf-node (datum left right parent)
	       `(make-rb-node :red ,datum ,left ,right ,parent))
	     (make-leaf (new-node)
	       `(make-rb-leaf :black ,new-node))
	     (make-node (data left right parent)
	       `(make-rb-node (color node) ,data ,left ,right ,parent)))
    (with-insert-local-function (make-node make-node make-leaf-node make-leaf 
					   left-balance right-balance)
      (let ((balanced (%insert object datum key key-datum compare equal)))
	(setf (color balanced) :black)
	balanced))))
  
(defmacro with-match-tree ((color left data right) tree &body body)
  `(and (eq ,color (color ,tree))
	(let ((,data (data ,tree)))
	  (declare (ignorable ,data))
	  ,(if (consp left)
	       `(with-match-tree ,left (left ,tree)
		  ,(if (consp right)
		       `(with-match-tree ,right (right ,tree)
			  ,@body)
		       `(let ((,right (right ,tree)))
			  (declare (ignorable ,right))
			  ,@body)))
	       `(let ((,left (left ,tree)))
		  (declare (ignorable ,left))
		  ,(if (consp right)
		       `(with-match-tree ,right (right ,tree)
			  ,@body)
		       `(let ((,right (right ,tree)))
			  (declare (ignorable ,right))
			  ,@body)))))))

(defmethod balance ((object rb-node))
  (macrolet ((setf-parent (new-node)
	       `(setf (parent (left ,new-node)) ,new-node
		      (parent (right ,new-node)) ,new-node
		      (parent (left (left ,new-node))) (left ,new-node)
		      (parent (right (left ,new-node))) (left ,new-node)
		      (parent (right (right ,new-node))) (right ,new-node)
		      (parent (left (right ,new-node))) (right ,new-node))))
    (with-match-tree (:black (:red (:red a x b) y c) z d) object
      (return-from balance
	(let ((new-node (make-rb-node :red y
				   (make-rb-node :black x a b nil) 
				   (make-rb-node :black z c d nil) 
				   (parent object))))
	  (setf-parent new-node)
	  new-node)))

    (with-match-tree (:black (:red a x (:red b y c)) z d) object
      (return-from balance
	(let ((new-node (make-rb-node :red y
				   (make-rb-node :black x a b nil) 
				   (make-rb-node :black z c d nil) 
				   (parent object))))
	  (setf-parent new-node)
	  new-node)))

    (with-match-tree (:black a x (:red (:red b y c) z d)) object
      (return-from balance
	(let ((new-node (make-rb-node :red y
				   (make-rb-node :black x a b nil) 
				   (make-rb-node :black z c d nil) 
				   (parent object))))
	  (setf-parent new-node)
	  new-node)))

    (with-match-tree (:black a x (:red b y (:red c z d))) object
      (return-from balance
	(let ((new-node (make-rb-node :red y 
				   (make-rb-node :black x a b nil) 
				   (make-rb-node :black z c d nil) 
				   (parent object))))
	  (setf-parent new-node)
	  new-node)))
    object))

(defmethod left-balance ((object rb-node))
  (macrolet ((setf-parent (new-node)
	       `(setf (parent (left ,new-node)) ,new-node
		      (parent (right ,new-node)) ,new-node
		      (parent (left (left ,new-node))) (left ,new-node)
		      (parent (right (left ,new-node))) (left ,new-node)
		      (parent (right (right ,new-node))) (right ,new-node)
		      (parent (left (right ,new-node))) (right ,new-node))))
    (with-match-tree (:black (:red (:red a x b) y c) z d) object
      (return-from left-balance 
	(let ((new-node (make-rb-node :red y 
				   (make-rb-node :black x a b nil) 
				   (make-rb-node :black z c d nil) 
				   (parent object))))
	  (setf-parent new-node)
	  new-node)))
    (with-match-tree (:black (:red a x (:red b y c)) z d) object
      (return-from left-balance 
	(let ((new-node (make-rb-node :red y 
				   (make-rb-node :black x a b nil) 
				   (make-rb-node :black z c d nil) 
				   (parent object))))
	  (setf-parent new-node)
	  new-node)))
    object))

(defmethod right-balance ((object rb-node))
  (macrolet ((setf-parent (new-node)
	       `(setf (parent (left ,new-node)) ,new-node
		      (parent (right ,new-node)) ,new-node
		      (parent (left (left ,new-node))) (left ,new-node)
		      (parent (right (left ,new-node))) (left ,new-node)
		      (parent (right (right ,new-node))) (right ,new-node)
		      (parent (left (right ,new-node))) (right ,new-node))))
    (with-match-tree (:black a x (:red (:red b y c) z d)) object
      (return-from right-balance
	(let ((new-node (make-rb-node :red y 
				   (make-rb-node :black x a b nil) 
				   (make-rb-node :black z c d nil) 
				   (parent object))))
	  (setf-parent new-node)
	  new-node)))
    (with-match-tree (:black a x (:red b y (:red c z d))) object
      (return-from right-balance
	(let ((new-node (make-rb-node :red y 
				   (make-rb-node :black x a b nil) 
				   (make-rb-node :black z c d nil) 
				   (parent object))))
	  (setf-parent new-node)
	  new-node)))
    object))

(defmethod map ((object rb-node) function)
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (if (leafp object)
	(make-rb-leaf color nil)
	(make-rb-node color (funcall function data) 
		      (map left function) 
		   (map right function) nil))))

(defmethod map-node ((object rb-node) function)
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (if (leafp object)
	(funcall function object (make-rb-leaf color object))
	(funcall function object (make-rb-node color data 
					    (map-node left function) 
					    (map-node right function) nil)))))

(defmethod reconstruct-parent ((object rb-node) &optional (parent (parent object)))
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (if (leafp object)
	(make-rb-leaf color parent)
	(make-rb-node color data 
		   (reconstruct-parent left object) 
		   (reconstruct-parent right object) parent))))

(defmethod node->dot ((object rb-node))
  (labels ((nodes ()
	     (append
	      (list
	       `(:node ((:id ,(format nil "~a" (data object)))
			      (:label ,(format nil "~ap~a" (data object)
					       (data (parent object))))
			      (:style "filled")
			      (:fillcolor ,(cond 
					    ((eq (color object) :red)
					     "#ff0000")
					    (t
					     "#ffffff"))))))
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

(alexandria:define-constant +color+ :color :test #'eq)

(defmethod to-sexp ((object rb-node))
  (let ((*print-circle* t))
    (list +data+ (to-sexp (data object))
	  +color+ (color object)
	  +left+ (to-sexp (left object))
	  +right+ (to-sexp (right object))
	  +parent+ (to-sexp (data (parent object))))))

(defmethod from-sexp ((object rb-node) sexp)
  (declare (ignorable object))
  (labels ((%from-sexp (sexp)
	     (if (null sexp)
		 (make-rb-leaf :black nil)
		 (make-rb-node (getf sexp +color+)
			    (getf sexp +data+)
			    (from-sexp object (getf sexp +left+))
			    (from-sexp object (getf sexp +right+)) nil))))
    (let ((new-tree (%from-sexp sexp)))
      (reconstruct-parent new-tree))))
