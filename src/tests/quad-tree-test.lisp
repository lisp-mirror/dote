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

(in-package :quad-tree-test)

(defsuite quadtree-suite (all-suite))


(deftest test-subdivision-aabb (quadtree-suite)
  (assert-equality
      #'(lambda (a b) (every #'2d-utils:aabb2~ a b))
      (list
       (vec4 10.0 10.0 15.0 15.0)
       (vec4 15.0 10.0 20.0 15.0)
       (vec4 15.0 15.0 20.0 20.0)
       (vec4 10.0 15.0 15.0 20.0))
      (multiple-value-list (calculate-subaabb (vec4 10.0 10.0 20.0 20.0)))))

(defun %subdivide (level)
  (let ((quad (make-leaf-quad-tree (vec4 10.0 10.0 20.0 20.0) nil)))
    (subdivide quad level)
    quad))

(deftest test-subdivision (quadtree-suite)
  (assert-equality  #'2d-utils:aabb2~
      (vec4 15.0 15.0 17.5 17.5)
      (aabb (nw (se (%subdivide 2))))))

(deftest test-subdivision-not-equal (quadtree-suite)
  (assert-equality  #'(lambda (a b) (not (2d-utils:aabb2~ a b)))
      (vec4 17.5 12.5 20.0 15.0)
      (aabb (nw (se (%subdivide 2))))))

(deftest test-query-aabb (quadtree-suite)
  (assert-equality  #'num:epsilon=
      12.5
      (elt
       (aabb
	(first
	 (query-smallest-intersect-aabb (%subdivide 2) (vec4 11.0 11.0 11.5 11.5))))
       2)))

(defun dump-quad-tree-to-pixmap (quad-tree levels &optional (level 0))
  (let ((pixmap (make-pixmap-frame (floor (+ (elt (aabb quad-tree) 0)
				       (elt (aabb2->rect2 (aabb quad-tree)) 2)))
			     (floor (+ (elt (aabb quad-tree) 1)
				       (elt (aabb2->rect2 (aabb quad-tree)) 3))))))
    (loop for i from 0 below (length (pixmap:data pixmap)) do
	 (setf (elt (pixmap:data pixmap) i)
	       #(0 0 0 255)))
    (labels ((paint (quad level)
	       (when (<= level levels)
		 (with-accessors ((aabb aabb) (nw nw) (ne ne)
				  (sw sw) (se se)) quad
		   (let* ((rect (aabb2->rect2 aabb))
			  (color (pick-color +rainbow-gradient+
					     (num:desired (* (/ level levels) 1.0))))
			  (x (round (1+ (elt (aabb quad) 0))))
			  (y (round (1+ (elt (aabb quad) 1))))
			  (w (round (- (elt rect 2) 2)))
			  (h (round (- (elt rect 3) 2))))
		     (matrix:matrix-rect pixmap x y w h
					 (map 'vector #'color-utils:float->byte color))
		     (and nw (paint nw (1+ level)))
		     (and ne (paint ne (1+ level)))
		     (and nw (paint sw (1+ level)))
		     (and ne (paint se (1+ level))))))))
      (paint quad-tree level)
      pixmap)))

(defun test-quad-pixmap (level)
  (let ((quad (make-leaf-quad-tree (vec4 0.0 0.0 512.0 512.0) nil)))
    (subdivide quad 5)
    (let ((pixmap (dump-quad-tree-to-pixmap quad level))
	  (hitted (query-smallest-intersect-aabb quad (vec4 32.5 32.5 40.0 70.2))))
      (loop for i in hitted do
      	   (let ((rect (num:round-all (map 'vector #'identity (aabb2->rect2 (aabb i))))))
      	     (matrix:matrix-rect pixmap (elt rect 0) (elt rect 1) (elt rect 2) (elt rect 3)
				 (map 'vector #'color-utils:float->byte §cff00ffff))))
      pixmap)))


(deftest query-subdivide-dump-test (quadtree-suite)
  (assert-true
      (equalp
       (pixmap:data (load-test-tga "data/quad.tga"))
       (pixmap:data (test-quad-pixmap 5)))))

(defun map-paths (aabb)
  (let ((quad (make-leaf-quad-tree (vec4 10.0 10.0 20.0 20.0) nil)))
    (subdivide quad 2)
    (let ((paths '()))
      (iterate-nodes-intersect quad #'(lambda (q)
				       (push (mapcar #'node-quadrant (path-to q)) paths))
			      aabb)
      paths)))

(deftest map-test (quadtree-suite)
  (assert-equalp
      '((nil :se :nw) (nil :se) (nil :sw :ne) (nil :sw) (nil :ne :sw) (nil :ne)
	(nil :nw :se) (nil :nw) (nil))
      (map-paths (vec4 14.0 14.0 16.0 16.0))))

(defclass dummy (entity:entity)
  ((aabb
    :initform (vec4 0.0 0.0 5.5 5.5)
    :initarg :aabb)))

(defmethod entity:aabb-2d ((object dummy))
  (slot-value object 'aabb))

(defun %push-down (aabb)
  (let ((quad (make-leaf-quad-tree (vec4 10.0 10.0 20.0 20.0) nil)))
    (subdivide quad 3)
    (push-down quad (make-instance 'dummy :aabb aabb))
    (let ((paths '()))
      (iterate-nodes-intersect quad #'(lambda (q)
				       (when (not (misc:vector-empty-p (data q)))
					 (setf paths (mapcar #'node-quadrant (path-to q)))))
			      (vec4 10.0 10.0 20.0 20.0))
      paths)))

(deftest push-down-test (quadtree-suite)
  (assert-equalp
      '(nil :nw :nw :nw)
      (%push-down (vec4 10.0 10.0 11.1 10.1)))
  (assert-equalp
      '(nil :se :nw :nw)
      (%push-down (vec4 15.0 15.0 15.1 16.1))))
