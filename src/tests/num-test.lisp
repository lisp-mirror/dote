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

(in-package :numeric-test)

(defsuite numeric-suite (all-suite))

(deftest test-random-pick (numeric-suite)
  (assert-false
      (let* ((bag '(0 1 2 3 4))
	     (picker (make-instance 'bag-picker :bag bag)))
	(set-difference
	 (loop repeat 5 collect (elt bag (random-pick-from-set picker)))
	 bag)))
  (assert-false
      (let* ((bag (loop repeat 100 collect (lcg-next-upto 100)))
	     (picker (make-instance 'bag-picker :bag bag)))
	(set-difference
	 (loop repeat 5 collect (elt bag (random-pick-from-set picker)))
	 bag))))


