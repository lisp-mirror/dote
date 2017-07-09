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

(in-package :vec2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype vec2-type ()
    'single-float)

  (deftype vec2 ()
    "A 2d vector of single floats."
    `(simple-array vec2-type (2)))

  (defun vec2p (a)
    (typep a 'vec2))

  (defun vec2 (x y)
    (declare (optimize (debug 0) (safety 0) (speed 3)))
    (let ((v (make-array-frame 2 0.0 'vec2-type t)))
      (declare (vec2 v))
      (setf (elt v 0) x
            (elt v 1) y)
      v))

  (defun vec2= (a b)
    (and (= (elt a 0) (elt b 0))
         (= (elt a 1) (elt b 1))))

  (alexandria:define-constant +vec2-zero+ (vec2 0.0 0.0)
    :test #'vec2=)

  (definline sequence->vec2 (vec)
    (vec2 (num:d (elt vec 0))
          (num:d (elt vec 1))))

  (defun-inline-function make-fresh-vec2 ()
    (make-array-frame 2 0 'vec2-type t)))

(defun copy-vec2 (old)
  (let ((res (make-array-frame 2 0.0 'vec2-type t)))
    (declare (vec2 res))
    (setf (elt res 0) (elt old 0)
          (elt res 1) (elt old 1))
    res))

(defun-inline-function vec2* (vec val)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 vec))
  (vec2 (num:d* (elt vec 0) val)
        (num:d* (elt vec 1) val)))

(define-compiler-macros vec2* vec val)

(defun-inline-function vec2/ (vec val)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 vec))
  (vec2 (num:d/ (elt vec 0) val)
        (num:d/ (elt vec 1) val)))

(define-compiler-macros vec2/ vec val)

(defun-inline-function vec2~ (a b)
  (and (num:epsilon= (elt a 0) (elt b 0))
       (num:epsilon= (elt a 1) (elt b 1))))

(define-compiler-macros vec2~ a b)

(defun-inline-function vec2+ (a b)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a b))
  (vec2 (num:d+ (elt a 0) (elt b 0))
        (num:d+ (elt a 1) (elt b 1))))

(define-compiler-macros vec2+ a b)

(defun-inline-function vec2- (a b)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a b))
  (vec2 (num:d- (elt a 0) (elt b 0))
        (num:d- (elt a 1) (elt b 1))))

(define-compiler-macros vec2-negate a)

(defun-inline-function vec2-negate (a)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a))
  (vec2 (num:d- (elt a 0))
        (num:d- (elt a 1))))

(define-compiler-macros vec2- a b)

(defun-inline-function vec2-length (a)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a))
  (num:dsqrt (num:d+ (num:dexpt (elt a 0) 2.0)
                     (num:dexpt (elt a 1) 2.0))))

(define-compiler-macros vec2-length a)

(defun-inline-function vec2-normalize (a)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a))
  (let ((length (vec2-length a)))
    (vec2 (num:d/ (elt a 0) length)
          (num:d/ (elt a 1) length))))

(define-compiler-macros vec2-normalize a)

(defun vec2-dot-product (a b)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a b))
  (num:d+ (num:d* (elt a 0) (elt b 0)) (num:d* (elt a 1) (elt b 1))))

(define-compiler-macros vec2-dot-product a b)
