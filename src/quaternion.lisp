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

(in-package :quaternion)

;;;;
;;;; Quaternions
;;;;   Do not seek to understand them, the denizens of Quaternia, for they are
;;;;   far beyond mortal ken.  Know only well that they do a mean twist.
;;;;
;;;; for our purposes, we will represent quaternions in the same form as
;;;; points <x y z w> where <x y z> == qv and w == qw.  This will make it easy
;;;; to do operations on quaternions and convert points to quaternions for
;;;; rotation (though ideally this should be done on hardware with matrices,
;;;; unless copying matrices onto the hardware proves too expensive)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype quat () 'vec4)

  (defun quat (x y z w)
    (vec4 x y z w))

  (defun quat~ (q1 q2) (vec4~ q1 q2))

  (alexandria:define-constant +quat-identity+ (quat 0.0 0.0 0.0 1.0)
    :test #'quat~)

  (defun copy-quat (old)
    (declare (optimize (debug 0) (safety 0) (speed 3)))
    (declare (quat old))
    (let ((res (vec4 0.0 0.0 0.0)))
      (declare (quat res))
      (setf (elt res 0) (elt old 0)
            (elt res 1) (elt old 1)
            (elt res 2) (elt old 2)
            (elt res 3) (elt old 3))
      res)))

(defun %set-qv (q v)
  (setf (elt q 0) (elt v 0)
        (elt q 1) (elt v 1)
        (elt q 2) (elt v 2)))

(defsetf qv %set-qv)

(defun qv (q)
  (vec (elt q 0) (elt q 1) (elt q 2)))

(defmacro qw (q)
  `(elt ,q 3))

(defun make-quat (x y z w)
  (quat x y z w))

(defun make-quat-from-vw (v w)
  (quat (elt v 0) (elt v 1) (elt v 2) w))

(defun vec->quat (p)
  (make-quat (elt p 0) (elt p 1) (elt p 2) 0.0))

(defun axis-rad->quat (axis &optional (rad (dsqrt (d+ (dexpt (elt axis 0) 2.0)
                                                      (dexpt (elt axis 1) 2.0)))))
  (make-quat-from-vw (vec* axis (dsin (d/ rad 2.0)))
                     (dcos (d/ rad 2.0))))

(defun euler->quat (roll pitch yaw)
  "Takes euler angles (roll pitch and yaw and converts them
   to a quaternion.  Assumes order is r->p->y"
  (let* ((cos-r (dcos (d/ roll 2.0)))
         (cos-p (dcos (d/ pitch 2.0)))
         (cos-y (dcos (d/ yaw 2.0)))
         (sin-r (dsin (d/ roll 2.0)))
         (sin-p (dsin (d/ pitch 2.0)))
         (sin-y (dsin (d/ yaw 2.0)))
         (cpcy (d* cos-p cos-y))
         (spsy (d* sin-p sin-y)))
    (make-quat (d+ (d* cos-r cpcy) (d* sin-r spsy))
               (d- (d* sin-r cpcy) (d* cos-r spsy))
               (d+ (d* cos-r sin-p cos-y) (d* sin-r cos-p sin-y))
               (d- (d* cos-r cos-p sin-y) (d* sin-r sin-p cos-y)))))

(defun vec-neg3 (v)
  (vec (d- (elt v 0))
       (d- (elt v 1))
       (d- (elt v 2))))

(defun vec-neg4 (v)
  (vec4 (d- (elt v 0))
        (d- (elt v 1))
        (d- (elt v 2))
        (d- (elt v 3))))

(defun sq (x) (d* x x))

(defun quat-conjugate (q)
  (make-quat-from-vw (vec-neg3 (qv q)) (qw q)))

(defun quat-norm (q)
  (vec4-normalize q))

(defun quat+ (q r)
  (make-quat
    (d+ (elt q 0) (elt r 0))
    (d+ (elt q 1) (elt r 1))
    (d+ (elt q 2) (elt r 2))
    (d+ (elt q 3) (elt r 3))))

(defun quat* (q r)
  (let ((q-v (qv q))
        (q-w (qw q))
        (r-v (qv r))
        (r-w (qw r)))
    (make-quat-from-vw
     (vec+ (vec+ (cross-product q-v r-v)
                 (vec* q-v r-w))
           (vec* r-v q-w))
     (d- (d* q-w r-w) (dot-product q-v r-v)))))

(defun quat-scale (q s)
  (make-quat (d* s (elt q 0))
             (d* s (elt q 1))
             (d* s (elt q 2))
             (d* s (elt q 3))))

(defun quat-inverse (q)
  (quat-scale (quat-conjugate q) (d/ 1.0 (sq (vec4-length q)))))

(defun quat-rotate-vec (q v)
  "Rotates a vector or point v by quaternion q.
   Calculated Q' = QVQ* where V = <v, 0.0>
   @arg[q]{quaternion to rotate by}
   @arg[v]{point or vector being rotated}
   @return{a point}"
  (qv (quat* q (quat* (vec->quat v) (quat-conjugate q)))))

(defun quat->matrix (q)
  "Convert a quaternion into a matrix (if this weren't lisp, i'd be more exited
   at the lack of trig functions)
   @return{a matrix that can be used to rotate objects according to
   the quaternion}"
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (quat q))
  (let* ((x (elt q 0))
         (y (elt q 1))
         (z (elt q 2))
         (w (elt q 3))
         (m11 (d- 1.0 (d* 2.0 (d+ (sq y) (sq z)))))
         (m12 (d* 2.0 (d+ (d* x y) (d* w z))))
         (m13 (d* 2.0 (d- (d* x z) (d* w y))))
         (m21 (d* 2.0 (d- (d* x y) (d* w z))))
         (m22 (d- 1.0 (d* 2.0 (d+ (sq x) (sq z)))))
         (m23 (d* 2.0 (d+ (d* y z) (d* w x))))
         (m31 (d* 2.0 (d+ (d* x z) (d* w y))))
         (m32 (d* 2.0 (d- (d* y z) (d* w x))))
         (m33 (d- 1.0 (d* 2.0 (d+ (sq x) (sq y))))))
    (matrix m11 m21 m31 0.0
            m12 m22 m32 0.0
            m13 m23 m33 0.0
            0.0 0.0 0.0 1.0)))

(defun matrix->quat (mat)
  (let*((w-squared-1 (d+ (mref mat 0 0) (mref mat 1 1) (mref mat 2 2)))
        (x-squared-1 (d- (mref mat 0 0) (mref mat 1 1) (mref mat 2 2)))
        (y-squared-1 (d- (mref mat 1 1) (mref mat 0 0) (mref mat 2 2)))
        (z-squared-1 (d- (mref mat 2 2) (mref mat 0 0) (mref mat 1 1)))
        (biggest-idx 0)
        (biggest w-squared-1))
    (cond
      ((d> x-squared-1 biggest)
       (setf biggest     x-squared-1
             biggest-idx 1))
      ((d> y-squared-1 biggest)
       (setf biggest     y-squared-1
             biggest-idx 2))
      ((d> z-squared-1 biggest)
       (setf biggest     z-squared-1
             biggest-idx 3)))
    (let* ((biggest-val (d/ (dsqrt (d+ 1.0 biggest)) 2.0))
           (mult (d/ 0.25 biggest-val)))
      (case biggest-idx
        (0
         (quat (d* (d- (mref mat 2 1) (mref mat 1 2)) mult)
               (d* (d- (mref mat 0 2) (mref mat 2 0)) mult)
               (d* (d- (mref mat 1 0) (mref mat 0 1)) mult)
               biggest-val))
        (1
         (quat biggest-val
               (d* (d+ (mref mat 1 0) (mref mat 0 1)) mult)
               (d* (d+ (mref mat 0 2) (mref mat 2 0)) mult)
               (d* (d- (mref mat 2 1) (mref mat 1 2)) mult)))
        (2
         (quat (d* (d+ (mref mat 1 0) (mref mat 0 1)) mult)
               biggest-val
               (d* (d+ (mref mat 2 1) (mref mat 1 2)) mult)
               (d* (d- (mref mat 0 2) (mref mat 2 0)) mult)))
        (3
         (quat (d* (d+ (mref mat 0 2) (mref mat 2 0)) mult)
               (d* (d+ (mref mat 2 1) (mref mat 1 2)) mult)
               biggest-val
               (d* (d- (mref mat 1 0) (mref mat 0 1)) mult)))))))

(defun quat-rotate-to-vec (src-vec dest-vec &key (fallback-axis +y-axe+))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (let* ((ns   (normalize src-vec))
         (nt   (normalize dest-vec))
         (u    (cross-product ns nt))
         (e    (dot-product   ns nt))
         (disc (d* 2.0 (d+ 1.0 e))))
    (with-epsilon (5e-4)
      (if (d>= e 1.0)
          +quat-identity+
          (progn
            (when (minusp disc)
              (format t "ZOMG COMPLEXITY~% src: ~a dest: ~a~% dot: ~a~%"
                      src-vec dest-vec e))
            (if (epsilon= e -1.0)
                (axis-rad->quat fallback-axis +pi+)
                (let* ((radical (dsqrt disc)))
                  (if (zerop radical)
                      +quat-identity+
                      (make-quat-from-vw (vec* u (/ 1.0 radical))  ; qv
                                         (/ radical 2.0))))))))))  ; qw

(alexandria:define-constant +slerp-delta+ 1.0e-3 :test #'=)

(defun quat-slerp (q1 q2 s)
  (let ((cos-omega (vec4-dot-product q1 q2))
        scale1 scale2)
    (when (minusp cos-omega)
      (setf cos-omega (d- cos-omega))
      (setf q2 (vec-neg4 q2)))
    (if (< (- 1.0 cos-omega) +slerp-delta+)
        ;; We do linear interpolation to avoid divide-by-zero
        (setf scale1 (- 1.0 s)
              scale2 s)
        ;; normal slerp
        (let* ((omega (dacos cos-omega))
               (sin-omega (dsin omega)))
          (setf scale1 (d/ (dsin (d* omega (d- 1.0 s)))
                           sin-omega)
                scale2 (d/ (dsin (d* omega s))
                           sin-omega))))
    (quat+ (quat-scale q1 scale1)
           (quat-scale q2 scale2))))

(defun spherical->quat (spherical)
  (labels ((%spherical->cartesian (sphere-coords)
             (let* ((phi   (elt sphere-coords 0))
                    (theta (elt sphere-coords 1))
                    (r     (elt sphere-coords 2))
                    (sin-theta (dsin (d- (d/ +pi+ 2.0) theta))))
               (vec4 (d* r (dsin phi) sin-theta)
                     (d* r (dcos (- (/ +pi+ 2) theta)))
                     (d* r (dcos phi) sin-theta)
                     1.0))))
    (let* ((cart-vec (vec4-normalize (%spherical->cartesian spherical)))
           (quat (quat-norm (quat-rotate-to-vec +z-axe+ cart-vec))))
      quat)))
