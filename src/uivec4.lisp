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

(in-package :uivec4)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype uivec4-type ()
    '(unsigned-byte 32))

  (deftype uivec4 ()
    "A 4d vector of unsigned integer."
    `(simple-array uivec4-type (4)))

  (defun uivec4p (a)
    (typep a 'uivec4))

  (defun uivec4 (x y z &optional (w 0))
    (let ((res (misc:make-array-frame 4 0 'uivec4-type t)))
      (setf (elt res 0) x
            (elt res 1) y
            (elt res 2) z
            (elt res 3) w)
      res))

  (defun copy-uivec4 (old)
    (let ((res (misc:make-array-frame 4 0 'uivec4-type t)))
      (setf (elt res 0) (elt old 0)
            (elt res 1) (elt old 1)
            (elt res 2) (elt old 2)
            (elt res 3) (elt old 3))
      res))

  (defun uivec4= (a b)
    (every #'= a b))

  (alexandria:define-constant +uivec4-zero+ (uivec4 0 0 0 0)
    :test #'uivec4=))
