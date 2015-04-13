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

(in-package :base64-test)

(defsuite base64-suite (all-suite))

(deftest equalp-static (base64-suite)
  (let* ((str "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
	 (encoded (encode (map 'vector #'char-code (coerce str 'list)))))
    (assert-true
	(equalp (decode encoded) (map 'vector #'char-code (coerce str 'list))))))

(deftest equalp-dynamic (base64-suite)
  (let* ((v (misc:random-num-filled-vector 1024 256))  
	 (encoded (encode v)))
    (assert-true
	(equalp (decode encoded) v))))

