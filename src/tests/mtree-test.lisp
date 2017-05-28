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

(in-package :mtree-test)

(defsuite mtree-suite (all-suite))

(defun tree-sample ()
  (let ((tree        (make-instance 'm-tree :data 2))
        (subtree     (make-instance 'm-tree :data 4))
        (sub-subtree (make-instance 'm-tree :data 3)))
    (add-child tree (make-instance 'm-tree :data 8))
    (add-child tree (make-instance 'm-tree :data 16))
    (add-child subtree sub-subtree)
    (add-child tree subtree)
    tree))

(deftest test-find-3 (mtree-suite)
  (assert-equality
      #'= 3
      (data (find-child (tree-sample) 3 :compare #'=))))

(deftest test-find-if-odd (mtree-suite)
  (assert-equality
      #'= 3
      (data (first (find-child-if (tree-sample)
                                  #'(lambda (a) (oddp (data a))))))))
