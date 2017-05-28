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

(in-package :sb-cga-utils-test)

(defsuite sb-cga-utils-suite (all-suite))

(deftest test-look@ (sb-cga-utils-suite)
  (assert-true
      (sb-cga:matrix~
       (look@ (sb-cga:vec 0.0 2.0 0.3) (sb-cga:vec 1.0 2.0 3.0) (sb-cga:vec 0.0 1.0 0.0))
       (sb-cga:matrix -0.93797606 0.0 0.34739855 -0.10421957
                      0.0 0.9999966 -0.0 -1.9999932
                      -0.347229 -0.0 -0.9375183 0.2812555
                      0.0 0.0 0.0 1.0))))
