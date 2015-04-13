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

(in-package :vec2-test)

(defsuite vec2-suite (all-suite))

(deftest creation-test (vec2-suite)
  (assert-true
      (vec2~ (vec2 0.0 1.0)
	     #(0.0 1.0)))
  (assert-false
      (vec2~ (vec2 0.0 3.0)
	     #(0.0 3.001))))
      
(deftest copy-test (vec2-suite)
  (let ((orig (vec2 (num:lcg-next-upto 1000.0) (num:lcg-next-upto 1000.0))))
    (assert-true
	(vec2~
	 orig
	 (copy-vec2 orig)))))
