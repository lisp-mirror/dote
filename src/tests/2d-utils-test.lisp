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

(in-package :2d-utils-test)

(defsuite 2d-utils-suite (all-suite))

(deftest test-aabb-overlap (2d-utils-suite)
  (let ((aabb1 (make-aabb2 10.0 10.0 20.0 20.0))
        (aabb2 (make-aabb2  5.0 11.0 35.0 12.0))
        (aabb3 (make-aabb2  7.0  9.0 10.0 13.0))
        (aabb4 (make-aabb2 19.0 11.0 20.0 12.0)))
    (assert-true  (aabb2-intersect-p aabb1 aabb2))
    (assert-true  (aabb2-intersect-p aabb2 aabb3))
    (assert-true  (aabb2-intersect-p aabb1 aabb4))
    (assert-false (aabb2-intersect-p aabb1 aabb3))))
