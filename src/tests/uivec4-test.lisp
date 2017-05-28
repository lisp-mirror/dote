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

(in-package :uivec4-test)

(defsuite uivec4-suite (all-suite))

(deftest creation-test (uivec4-suite)
  (assert-equalp
      (uivec4 0 1 2 3)
      #(0 1 2 3)))

(deftest copy-test (uivec4-suite)
  (let ((orig (uivec4 (num:lcg-next-upto 1000) (num:lcg-next-upto 1000)
                      (num:lcg-next-upto 1000) (num:lcg-next-upto 1000))))
    (assert-equalp
        orig
        (copy-uivec4 orig))))
