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

(in-package :matrix-test)

(defsuite matrix-suite (all-suite))

(defun test-matrix-mult ()
  (let ((a (gen-matrix-frame  4 3))
        (b (gen-matrix-frame  4 4)))
    (setf (data a) #(1 0 0 0
                     0 1 0 0
                     0 0 1 0))
    (setf (data b) #(1 0 -3 2
                     0 1 -2 1
                     0 0 -1 1
                     0 0 3 -2 ))
    (matrix:matrix-mult a b)))

(deftest matrix-mult (matrix-suite)
  (assert-true
      (equalp
         (matrix:data (test-matrix-mult))
         #(1  0  -3  2
           0  1  -2  1
           0  0  -1  1))))

(deftest matrix-fresh (matrix-suite)
  (let ((mat (make-matrix 4 4 (sb-cga:vec 0.0 1.0 2.0))))
    (assert-false
        (eq (matrix-elt mat 0 0) (matrix-elt mat 1 1)))))
