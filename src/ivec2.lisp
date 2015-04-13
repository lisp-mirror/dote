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

(in-package :ivec2)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (deftype ivec2-type ()
    '(signed-byte 32))

  (deftype ivec2 ()
    "A 2d vector of single floats."
    `(simple-array ivec2-type (2)))

  (defun ivec2p (a)
    (typep a 'ivec2))
  
  (defun ivec2 (x y)
    (declare (optimize (debug 0) (safety 0) (speed 3)))
    (let ((v (make-array-frame 2 0 'ivec2-type t)))
      (declare (ivec2 v))
      (setf (elt v 0) x
	    (elt v 1) y)
      v))

  (defun ivec2= (a b)
    (and (= (elt a 0) (elt b 0))
	 (= (elt a 1) (elt b 1))))

  (defun copy-ivec2 (old)
    (let ((res (make-array-frame 2 0 'ivec2-type t)))
      (declare (ivec2 res))
      (setf (elt res 0) (elt old 0)
	    (elt res 1) (elt old 1))
      res))
  
  (alexandria:define-constant +ivec2-zero+ (ivec2 0.0 0.0) 
    :test #'ivec2=)

  (defun-inline-function make-fresh-ivec2 ()
    (make-array-frame 2 0 'ivec2-type t)))

(defun-inline-function ivec2* (vec val)
  (declare (ivec2 vec))
  (declare ((signed-byte 32) val))
  (ivec2 (* (elt vec 0) val)
	 (* (elt vec 1) val)))

(define-compiler-macros ivec2* vec val)

(defun-inline-function ivec2/ (vec val)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (ivec2 vec))
  (declare ((signed-byte 32) val))
  (ivec2 (/ (elt vec 0) val)
	 (/ (elt vec 1) val)))

(define-compiler-macros ivec2/ vec val)

(defun-inline-function ivec2~ (a b)
  (and (num:epsilon= (elt a 0) (elt b 0))
       (num:epsilon= (elt a 1) (elt b 1))))

(define-compiler-macros ivec2~ a b)

(defun-inline-function ivec2+ (a b)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (ivec2 a b))
  (ivec2 (+ (elt a 0) (elt b 0))
	 (+ (elt a 1) (elt b 1))))

(define-compiler-macros ivec2+ a b)

(defun-inline-function ivec2- (a b)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (ivec2 a b))
  (ivec2 (- (elt a 0) (elt b 0))
	 (- (elt a 1) (elt b 1))))

(define-compiler-macros ivec2- a b)

(defun-inline-function ivec2-length (a)
  (declare (ivec2 a))
  (round (sqrt (+ (expt (elt a 0) 2)
		  (expt (elt a 1) 2)))))

(define-compiler-macros ivec2-length a)

(defun-inline-function ivec2-normalize (a)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (ivec2 a))
  (let ((length (ivec2-length a)))
    (declare ((signed-byte 32) length))
    (ivec2 (/ (elt a 0) length)
	   (/ (elt a 1) length))))

(define-compiler-macros ivec2-normalize a)

