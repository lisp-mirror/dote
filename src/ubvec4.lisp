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

(in-package :ubvec4)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype ubvec4-type ()
    '(unsigned-byte 8))

  (deftype ubvec4 ()
    "A 4d vector of unsigned integer."
    `(simple-array ubvec4-type (4)))

  (defun ubvec4p (a)
    (typep a 'ubvec4))

  (defun ubvec4 (x y z &optional (w 0))
    (declare (optimize (safety 0) (speed 3) (debug 0)))
    (declare (ubvec4-type x y z w))
    (let ((res (make-array-frame 4 0 'ubvec4-type t)))
      (declare (ubvec4 res))
      (setf (elt res 0) x
            (elt res 1) y
            (elt res 2) z
            (elt res 3) w)
      res))

  (defun copy-ubvec4 (old)
    (declare (optimize (safety 0) (speed 3) (debug 0)))
    (declare (ubvec4 old))
    (let ((res (make-array-frame 4 0 'ubvec4-type t)))
      (declare (ubvec4 res))
      (setf (elt res 0) (elt old 0)
            (elt res 1) (elt old 1)
            (elt res 2) (elt old 2)
            (elt res 3) (elt old 3))
      res))

  (defun ubvec4= (a b)
    (every #'= a b))

  (alexandria:define-constant +ubvec4-zero+ (ubvec4 0 0 0 0)
    :test #'ubvec4=)

  (defun-inline-function make-fresh-ubvec4 ()
    (make-array-frame 4 0 'ubvec4-type t)))

(defun-inline-function ubvec4* (vec val)
  (ubvec4 (* (elt vec 0) val)
           (* (elt vec 1) val)
           (* (elt vec 2) val)
           (* (elt vec 3) val)))

(define-compiler-macros ubvec4* vec val)

(defun-inline-function ubvec4/ (vec val)
  (ubvec4 (round (/ (elt vec 0) val))
           (round (/ (elt vec 1) val))
           (round (/ (elt vec 2) val))
           (round (/ (elt vec 3) val))))

(define-compiler-macros ubvec4/ vec val)

(defun-inline-function ubvec4~ (a b)
  (every #'num:epsilon= a b))

(define-compiler-macros ubvec4~ a b)

(defun-inline-function ubvec4+ (a b)
  (ubvec4 (+ (elt a 0) (elt b 0))
           (+ (elt a 1) (elt b 1))
           (+ (elt a 2) (elt b 2))
           (+ (elt a 3) (elt b 3))))

(define-compiler-macros ubvec4+ a b)

(defun-inline-function ubvec4- (a b)
  (ubvec4 (- (elt a 0) (elt b 0))
           (- (elt a 1) (elt b 1))
           (- (elt a 2) (elt b 2))
           (- (elt a 3) (elt b 3))))

(define-compiler-macros ubvec4- a b)

(defun-inline-function ubvec4-length (a)
  (sqrt (+ (expt (elt a 0) 2)
           (expt (elt a 1) 2)
           (expt (elt a 2) 2)
           (expt (elt a 3) 2))))

(define-compiler-macros ubvec4-length a)

(defun-inline-function ubvec4-normalize (a)
  (let ((length (ubvec4-length a)))
    (ubvec4 (/ (elt a 0) length)
             (/ (elt a 1) length)
             (/ (elt a 2) length)
             (/ (elt a 3) length))))

(define-compiler-macros ubvec4-normalize a)

(defun-inline-function ubvec4-dot-product (a b)
  (+
   (* (elt a 0) (elt b 0))
   (* (elt a 1) (elt b 1))
   (* (elt a 2) (elt b 2))
   (* (elt a 3) (elt b 3))))

(define-compiler-macros ubvec4-dot-product a b)
