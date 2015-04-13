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

(in-package :priority-queue)

(defparameter *equal-function* #'equalp)

(defparameter *compare-function* #'<)

(defparameter *key-fun* #'identity)

(defparameter *heap* (misc:make-array-frame 1 nil))

(defun get-children-pos (parent-pos)
  (declare (integer parent-pos))
  (list (and (< (* 2 parent-pos) (fill-pointer *heap*))
		(* 2 parent-pos))
	(and (< (1+ (* 2 parent-pos)) (fill-pointer *heap*))
	     (1+ (* 2 parent-pos)))))

(defun get-parent-pos (pos)
  (floor (/ pos 2)))

(defun rearrange-bottom-up (&optional (pos (1- (length *heap*))))
  (let ((parent-pos (get-parent-pos pos)))
    (when (and (> parent-pos 0)
	       (funcall *compare-function* 
			(funcall *key-fun* (elt *heap* pos)) 
			(funcall *key-fun* (elt *heap* parent-pos))))
      (let ((swp (elt *heap* parent-pos)))
	(setf (elt *heap* parent-pos) (elt *heap* pos))
	(setf (elt *heap* pos) swp))
      (rearrange-bottom-up parent-pos))))

(defun rearrange-top-bottom (&optional (root-pos 1))
  (let ((children (remove-if #'null (get-children-pos root-pos))))
    (let ((maximum-child (cond
			   ((null children) 
			    root-pos)
			   ((= (length children) 1)
			    (first children))
			   (t
			    (if (funcall *compare-function* 
					 (funcall *key-fun* (elt *heap* (first children)))
					 (funcall *key-fun* (elt *heap* (second children))))
				(first children)
				(second children))))))
      (when (not (funcall *equal-function* 
			  (funcall *key-fun* (elt *heap* maximum-child))
			  (funcall *key-fun* (elt *heap* root-pos))))
	(let ((swp (elt *heap* root-pos)))
	  (setf (elt *heap* root-pos) (elt *heap* maximum-child))
	  (setf (elt *heap* maximum-child) swp))
	(rearrange-top-bottom maximum-child)))))
	  
(defun push (val)
  (vector-push-extend val *heap*)
  (rearrange-bottom-up))

(defun emptyp ()
   (<= (length *heap*) 1))

(defmacro with-min-queue ((compare sort key) &body body)
  `(let ((*heap* (misc:make-array-frame 1 nil))
 	 (*equal-function* ,compare)
 	 (*compare-function* ,sort)
 	 (*key-fun* ,key))
      ,@body))

(defun pop ()
  (if (emptyp)
      nil
      (prog1
	  (elt *heap* 1)
	(if (= (length *heap*) 2)
	    (setf (fill-pointer *heap*) (1- (fill-pointer *heap*)))
	    (progn
	      (setf (elt *heap* 1) (alexandria:last-elt *heap*))
	      (setf (fill-pointer *heap*) (1- (fill-pointer *heap*)))
	      (rearrange-top-bottom))))))

(defun find (element)
   (cl:find element *heap* :key *key-fun* :test *equal-function*)) 
