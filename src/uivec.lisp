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

(in-package :uivec)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype uivec-type ()
    '(unsigned-byte 32))

  (deftype uivec ()
    "A 3d vector of unsigned integer."
    `(simple-array uivec-type (3)))

  (defun uivecp (a)
    (typep a 'uivec))

  (defun uivec (x y z)
    (let ((res (make-array-frame 3 0 'uivec-type t)))
      (setf (elt res 0) x
            (elt res 1) y
            (elt res 2) z)
      res))

  (defun copy-uivec (old)
    (let ((res (make-array-frame 3 0 'uivec-type t)))
      (declare (uivec res))
      (setf (elt res 0) (elt old 0)
            (elt res 1) (elt old 1)
            (elt res 2) (elt old 2))
      res))

  (defun uivec= (a b)
    (every #'= a b))

  (alexandria:define-constant +uivec-zero+ (uivec 0 0 0)
    :test #'uivec=)

  (defun-inline-function make-fresh-uivec ()
    (make-array-frame 3 0 'uivec-type t)))


(defun-inline-function uivec* (vec val)
  (uivec (* (elt vec 0) val)
         (* (elt vec 1) val)
         (* (elt vec 2) val)))

(define-compiler-macros uivec* vec val)

(defun-inline-function uivec/ (vec val)
  (uivec (round (/ (elt vec 0) val))
         (round (/ (elt vec 1) val))
         (round (/ (elt vec 2) val))))

(define-compiler-macros uivec/ vec val)

(defun-inline-function uivec~ (a b)
  (every #'num:epsilon= a b))

(define-compiler-macros uivec~ a b)

(defun-inline-function uivec+ (a b)
  (uivec (+ (elt a 0) (elt b 0))
         (+ (elt a 1) (elt b 1))
         (+ (elt a 2) (elt b 2))))

(define-compiler-macros uivec+ a b)

(defun-inline-function uivec- (a b)
  (uivec (- (elt a 0) (elt b 0))
         (- (elt a 1) (elt b 1))
         (- (elt a 2) (elt b 2))))

(define-compiler-macros uivec- a b)

(defun-inline-function uivec-length (a)
  (sqrt (+ (expt (elt a 0) 2)
           (expt (elt a 1) 2)
           (expt (elt a 2) 2))))

(define-compiler-macros uivec-length a)

(defun-inline-function uivec-normalize (a)
  (let ((length (uivec-length a)))
    (uivec (/ (elt a 0) length)
           (/ (elt a 1) length)
           (/ (elt a 2) length))))

(define-compiler-macros uivec-normalize a)

(defun-inline-function uivec-dot-product (a b)
  (+
   (* (elt a 0) (elt b 0))
   (* (elt a 1) (elt b 1))
   (* (elt a 2) (elt b 2))))

(define-compiler-macros uivec-dot-product a b)
