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

(in-package :resource-cache-test)

(defsuite resource-cache-test-suite (all-suite))

(deftest fail-if-cache-system-is-down (resource-cache-test-suite)
  (assert-condition cache-error
      (make-cache-key (make-cache-key-element :name "123"    :file-type :directory)
		      (make-cache-key-element :name "qwerty" :file-type :directory)
		      (make-cache-key-element :name "a.png"  :file-type :file))))
