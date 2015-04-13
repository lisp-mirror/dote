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

(in-package :kd-tree-test)

(defsuite kd-tree-suite (all-suite))

(defun kd-tree-compare (a b dim)
  (num:d< (elt a dim) (elt b dim)))

(defun kd-tree-equal-insert (a b)
  (declare (ignore a b))
  nil)

(defun make-test-3d-tree-find ()
  (let ((time (get-universal-time)))
    (num:with-lcg-seed (time)
      (let ((vertices (loop repeat 100000 collect (vec (num:lcg-next-upto 2.0)
						       (num:lcg-next-upto 2.0)
						       (num:lcg-next-upto 2.0))))
	    (test-vec (vec (num:lcg-next-upto 2.0)
			   (num:lcg-next-upto 2.0)
			   (num:lcg-next-upto 2.0)))
	    (tree (make-root-kd-node nil 3))
	    (epsilon 1e-1))
	(loop for i in vertices do
	     (setf tree (insert tree i 
				:equal #'kd-tree-equal-insert
				:compare #'kd-tree-compare
				:key-datum #'identity
				:key #'identity)))
	(let* ((num:*default-epsilon* epsilon)
	       (raw-linear-res (mapcar #'(lambda (a) (if (every #'num:epsilon= a test-vec)
						     a
						     nil))
					     vertices))
	       (linear-res (remove-if #'null raw-linear-res))
	       (tree-res (3d-query-range tree test-vec
					 epsilon epsilon epsilon
					 :just-peek-first nil)))
	  (not (set-exclusive-or linear-res tree-res
				 :test #'(lambda (a b) (every #'num:epsilon= a b)))))))))

(deftest test-3d-tree-query (kd-tree-suite)
  (assert-true (make-test-3d-tree-find)))
