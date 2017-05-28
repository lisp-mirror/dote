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

(in-package :ivec4)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype ivec4-type ()
    '(signed-byte 32))

  (deftype ivec4 ()
    "A 4d vector of unsigned integer."
    `(simple-array ivec4-type (4)))

  (defun ivec4p (a)
    (typep a 'ivec4))

  (defun ivec4 (x y z &optional (w 0))
    (let ((res (make-array-frame 4 0 'ivec4-type t)))
      (setf (elt res 0) x
            (elt res 1) y
            (elt res 2) z
            (elt res 3) w)
      res))

  (defun copy-ivec4 (old)
    (let ((res (make-array-frame 4 0 'ivec4-type t)))
      (declare (ivec4 res))
      (setf (elt res 0) (elt old 0)
            (elt res 1) (elt old 1)
            (elt res 2) (elt old 2)
            (elt res 3) (elt old 3))
      res))

  (defun ivec4= (a b)
    (every #'= a b))

  (alexandria:define-constant +ivec4-zero+ (ivec4 0 0 0 0)
    :test #'ivec4=)

  (defun-inline-function make-fresh-ivec4 ()
    (make-array-frame 4 0 'ivec4-type t)))

(defun-inline-function ivec4* (vec val)
  (ivec4 (* (elt vec 0) val)
         (* (elt vec 1) val)
         (* (elt vec 2) val)
         (* (elt vec 3) val)))

(define-compiler-macros ivec4* vec val)

(defun-inline-function ivec4/ (vec val)
  (ivec4 (round (/ (elt vec 0) val))
         (round (/ (elt vec 1) val))
         (round (/ (elt vec 2) val))
         (round (/ (elt vec 3) val))))

(define-compiler-macros ivec4/ vec val)

(defun-inline-function ivec4~ (a b)
  (every #'num:epsilon= a b))

(define-compiler-macros ivec4~ a b)

(defun-inline-function ivec4+ (a b)
  (ivec4 (+ (elt a 0) (elt b 0))
         (+ (elt a 1) (elt b 1))
         (+ (elt a 2) (elt b 2))
         (+ (elt a 3) (elt b 3))))

(define-compiler-macros ivec4+ a b)

(defun-inline-function ivec4- (a b)
  (ivec4 (- (elt a 0) (elt b 0))
         (- (elt a 1) (elt b 1))
         (- (elt a 2) (elt b 2))
         (- (elt a 3) (elt b 3))))

(define-compiler-macros ivec4- a b)

(defun-inline-function ivec4-length (a)
  (sqrt (+ (expt (elt a 0) 2)
           (expt (elt a 1) 2)
           (expt (elt a 2) 2)
           (expt (elt a 3) 2))))

(define-compiler-macros ivec4-length a)

(defun-inline-function ivec4-normalize (a)
  (let ((length (ivec4-length a)))
    (ivec4 (/ (elt a 0) length)
           (/ (elt a 1) length)
           (/ (elt a 2) length)
           (/ (elt a 3) length))))

(define-compiler-macros ivec4-normalize a)

(defun-inline-function ivec4-dot-product (a b)
  (+
   (* (elt a 0) (elt b 0))
   (* (elt a 1) (elt b 1))
   (* (elt a 2) (elt b 2))
   (* (elt a 3) (elt b 3))))

(define-compiler-macros ivec4-dot-product a b)
