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

(in-package :vec4)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype vec4-type ()
    'single-float)

  (deftype vec4 ()
    "A 4d vector of single floats."
    `(simple-array vec4-type (4)))

  (defun vec4p (a)
    (typep a 'vec4))

  (defun vec4 (x y z &optional (w 1.0))
    (let ((res (make-array-frame 4 0.0 'vec4-type t)))
      (setf (elt res 0) x
            (elt res 1) y
            (elt res 2) z
            (elt res 3) w)
      res))

  (definline vec->vec4 (vec &optional (w 1.0))
    (vec4 (elt vec 0) (elt vec 1) (elt vec 2) w))

  (definline vec4->vec (v)
    (vec (elt v 0) (elt v 1) (elt v 2)))

  (defun-inline-function vec4= (a b)
    (and  (= (elt a 0) (elt b 0))
          (= (elt a 1) (elt b 1))
          (= (elt a 2) (elt b 2))
          (= (elt a 3) (elt b 3))))

  (define-compiler-macros vec4= a b)

  (alexandria:define-constant +vec4-zero+ (vec4 0.0 0.0 0.0 0.0)
    :test #'vec4=)

  (defun-inline-function make-fresh-vec4 ()
    (make-array-frame 4 0.0 'vec4-type t)))

(defun copy-vec4 (old)
  (let ((res (make-array-frame 4 0.0 'vec4-type t)))
    (declare (vec4 res))
    (setf (elt res 0) (elt old 0)
          (elt res 1) (elt old 1)
          (elt res 2) (elt old 2)
          (elt res 3) (elt old 3))
    res))

(defun-inline-function transform-vec4 (vec matrix)
  (labels ((sum (col)
             (+ (* (elt vec 0) (mref matrix col 0))
                (* (elt vec 1) (mref matrix col 1))
                (* (elt vec 2) (mref matrix col 2))
                (* (elt vec 3) (mref matrix col 3)))))
    (vec4 (sum 0) (sum 1) (sum 2) (sum 3))))

(define-compiler-macros transform-vec4 vec matrix)

(defun-inline-function vec4* (vec val)
  (vec4 (* (elt vec 0) val)
        (* (elt vec 1) val)
        (* (elt vec 2) val)
        (* (elt vec 3) val)))

(define-compiler-macros vec4* vec val)

(defun-inline-function vec4/ (vec val)
  (vec4 (/ (elt vec 0) val)
        (/ (elt vec 1) val)
        (/ (elt vec 2) val)
        (/ (elt vec 3) val)))

(define-compiler-macros vec4/ vec val)


(defun-inline-function vec4~ (a b)
  (every #'num:epsilon= a b))

(define-compiler-macros vec4~ a b)

(defun-inline-function vec4+ (a b)
  (vec4 (+ (elt a 0) (elt b 0))
        (+ (elt a 1) (elt b 1))
        (+ (elt a 2) (elt b 2))
        (+ (elt a 3) (elt b 3))))

(define-compiler-macros vec4+ a b)

(defun-inline-function vec4- (a b)
  (vec4 (- (elt a 0) (elt b 0))
        (- (elt a 1) (elt b 1))
        (- (elt a 2) (elt b 2))
        (- (elt a 3) (elt b 3))))

(define-compiler-macros vec4- a b)

(defun-inline-function vec4-length (a)
  (sqrt (+ (expt (elt a 0) 2)
           (expt (elt a 1) 2)
           (expt (elt a 2) 2)
           (expt (elt a 3) 2))))

(define-compiler-macros vec4-length a)

(defun-inline-function vec4-normalize (a)
  (let ((length (vec4-length a)))
    (vec4 (/ (elt a 0) length)
          (/ (elt a 1) length)
          (/ (elt a 2) length)
          (/ (elt a 3) length))))

(define-compiler-macros vec4-normalize a)

(defun-inline-function vec4-dot-product (a b)
  (+
   (* (elt a 0) (elt b 0))
   (* (elt a 1) (elt b 1))
   (* (elt a 2) (elt b 2))
   (* (elt a 3) (elt b 3))))

(define-compiler-macros vec4-dot-product a b)

(defun-inline-function vec4-negate (a)
  (vec4 (- (elt a 0))
        (- (elt a 1))
        (- (elt a 2))
        (- (elt a 3))))

(define-compiler-macros vec4-negate a)


(definline vec4-average (&rest vecs)
  (vec4/ (reduce #'(lambda (a b) (vec4+ a b)) vecs :initial-value +vec4-zero+)
         (coerce (length vecs) 'vec4-type)))

(definline vec4-average* (vecs)
  (vec4/ (reduce #'(lambda (a b) (vec4+ a b)) vecs :initial-value +vec4-zero+)
         (coerce (length vecs) 'vec4-type)))
