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

(in-package :vec4-test)

(defsuite vec4-suite (all-suite))

(deftest creation-test (vec4-suite)
  (assert-true
      (vec4~
       (vec4 0.0 1.0 2.0 3.0)
       #(0.0 1.0 2.0 3.0)))
  (assert-false
      (vec4~ (vec4 0.0 1.0 2.0 3.0)
             #(0.0 1.0 2.0 3.001))))

(deftest copy-test (vec4-suite)
  (let ((orig (vec4 (num:lcg-next-upto 1000.0) (num:lcg-next-upto 1000.0)
                    (num:lcg-next-upto 1000.0) (num:lcg-next-upto 1000.0))))
    (assert-true
        (vec4~
         orig
         (copy-vec4 orig)))))
