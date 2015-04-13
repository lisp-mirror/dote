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

(in-package :color-utils-test)

(defsuite color-utils-suite (all-suite))

(deftest test-rgb->hsv (color-utils-suite)
  (assert-true
      (vec~
       (rgb->hsv* (vec 0.211 0.149 0.597))
       (vec 248.30357 0.7504188 0.597)))
  (assert-true 
      (vec~
       (rgb->hsv* (vec 0.116 0.675 0.255))
       (vec 134.91951 0.8281481 0.675))))

(deftest pick-color (color-utils-suite)
  (assert-equality #'vec4~
      (pick-color +rainbow-gradient+ 2.0)
      §cffdd00ff)
    (assert-equality #'vec4~
      (pick-color +rainbow-gradient+ -2.0)
      §c00000ff)
    (assert-equality #'vec4~
      (pick-color +rainbow-gradient+ 0.24)
      (vec4 0.7272727 0.0 0.0 1.0)))


