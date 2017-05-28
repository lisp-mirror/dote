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

(in-package :quaternion-test)

(defsuite quaternion-suite (all-suite))

(deftest creation-test (quaternion-suite)
  (assert-true
      (quat~
       (quat 0.0 1.0 2.0 3.0)
       #(0.0 1.0 2.0 3.0)))
  (assert-false
      (quat~ (quat 0.0 1.0 2.0 3.0)
             #(0.0 1.0 2.0 3.001))))


(deftest copy-test (quaternion-suite)
  (let ((orig (quat (num:lcg-next-upto 1000.0) (num:lcg-next-upto 1000.0)
                    (num:lcg-next-upto 1000.0) (num:lcg-next-upto 1000.0))))
    (assert-true
        (quat~
         orig
         (copy-quat orig)))))

(deftest mult-test (quaternion-suite)
  (assert-equality #'quat~
      (quat* (quat 0.060 -0.257 -0.935 0.233)
             (quat 0.286 0.347 0.459 -0.752))
      (quat 0.22800002 -0.02083502 0.904389 0.325968)))


(deftest mat-quat-test (quaternion-suite)
  (let ((matrot (sb-cga:rotate-around (sb-cga:normalize (sb-cga:vec 1.0 3.0 2.0)) 2.1))
        (q (euler->quat 0.0 +pi+ +pi/2+)))
    (assert-true
        (sb-cga:matrix~ (quat->matrix (matrix->quat matrot)) matrot 1e-3))
    (assert-true
        (or
         (quat~ (matrix->quat (quat->matrix q)) q)
         (quat~ (matrix->quat (quat->matrix q)) (quat-conjugate q))))))
