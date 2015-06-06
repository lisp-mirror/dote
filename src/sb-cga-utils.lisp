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

(in-package :sb-cga-utils)

(defun vec-negate (v)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec v))
  (vec (d- (elt v 0))
       (d- (elt v 1))
       (d- (elt v 2))))

(defun extract-traslation-vec (mat)
  (vec (mref mat 0 3) (mref mat 1 3) (mref mat 2 3)))

(defun extract-traslation-mat (mat)
  (let ((res (identity-matrix)))
    (setf (mref res 0 3) (mref mat 0 3)
	  (mref res 1 3) (mref mat 1 3)
	  (mref res 2 3) (mref mat 2 3))
    res))

(defun safe-normalize (a &key (epsilon 1e-7))
  (if (vec~ a constants:+zero-vec+ epsilon)
      a
      (normalize a)))

(alexandria:define-constant +safe-p1-aabb+ (vec 1e10 1e10 1e10) :test #'vec=)

(alexandria:define-constant +safe-p2-aabb+ (vec -1.0 -1.0 -1.0) :test #'vec=)

(defclass aabb ()
  ((aabb-p1
    :initform (copy-vec +safe-p1-aabb+)
    :initarg :aabb-p1
    :accessor aabb-p1)
   (aabb-p2
    :initform  (copy-vec +safe-p2-aabb+)
    :initarg :aabb-p2
    :accessor aabb-p2)))

(defmethod print-object ((object aabb) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "p1: ~a p2: ~a" (aabb-p1 object) (aabb-p2 object))))

(defmethod marshal:class-persistant-slots ((object aabb))
  '(aabb-p1 aabb-p2))

(defmethod clone ((object aabb))
  (make-instance 'aabb :aabb-p1 (copy-vec (aabb-p1 object)) :aabb-p2 (copy-vec (aabb-p2 object))))

(defmethod copy-flat ((object aabb))
  (make-instance 'aabb :aabb-p1 (aabb-p1 object) :aabb-p2 (aabb-p2 object)))

(defgeneric expand (object v))

(defgeneric insidep (object v))

(defgeneric overlapp (object1 object2))

(defgeneric flatten-to-aabb2-xz (object))

(defgeneric reset (object))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro %with-optimized-vector ((aabb p accessor) &body body)
    `(let ((,p (,accessor ,aabb)))
       (declare (vec ,p))
       ,@body)))

(defmacro gen-aabb-shortcut (name vec pos)
  (let ((name-fun (alexandria:format-symbol t "~:@(setf-~a~)" name)))
    `(progn
       (misc:definline ,name (aabb)
	 (declare (optimize (debug 0) (safety 0) (speed 3)))
	 (declare (aabb aabb))
	 (%with-optimized-vector (aabb p ,vec)
	   (elt p ,pos)))
       (misc:definline ,name-fun (aabb v)
	 (declare (optimize (debug 0) (safety 0) (speed 3)))
	 (declare (aabb aabb))
	 (%with-optimized-vector (aabb p ,vec)
	   (setf (elt p ,pos) v)))
       (defsetf ,name ,name-fun))))

(gen-aabb-shortcut min-x aabb-p1 0)

(gen-aabb-shortcut min-y aabb-p1 1)

(gen-aabb-shortcut min-z aabb-p1 2)

(gen-aabb-shortcut max-x aabb-p2 0)

(gen-aabb-shortcut max-y aabb-p2 1)

(gen-aabb-shortcut max-z aabb-p2 2)

(defmethod expand ((object aabb) v)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec v))
  (declare (aabb object))
  (let ((max-x (max-x object))
	(max-y (max-y object))
	(max-z (max-z object))
	(min-x (min-x object))
	(min-y (min-y object))
	(min-z (min-z object)))
    (declare (desired-type max-x max-y min-x min-y))
    (when (d> (elt v 0) max-x)
      (setf (max-x object) (elt v 0)))
    (when (d> (elt v 1) max-y)
      (setf (max-y object) (elt v 1)))
    (when (d> (elt v 2) max-z)
      (setf (max-z object) (elt v 2)))
    (when (d< (elt v 0) min-x)
      (setf (min-x object) (elt v 0)))
    (when (d< (elt v 1) min-y)
      (setf (min-y object) (elt v 1)))
    (when (d< (elt v 2) min-z)
      (setf (min-z object) (elt v 2)))))

(defmacro with-aabb-ends ((p1 p2) object &body body)
  `(with-accessors ((,p1 aabb-p1) (,p2 aabb-p2)) ,object
     ,@body))

(defmethod insidep ((object aabb) v)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec v))
  (declare (aabb object))
  (with-aabb-ends (p1 p2) object
    (flet ((test (v pa pb element)
	     (declare (vec v pa pb))
	     (declare ((unsigned-byte 8) element))
	     (d<= (elt pa element) (elt v element) (elt pb element))))
      (and (test v p1 p2 0)
	   (test v p1 p2 1)
	   (test v p1 p2 2)))))

(defmethod overlapp ((a aabb) (b aabb))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (aabb a b))
  (not
   (or (d> (min-x a) (max-x b))
       (d> (min-y a) (max-y b))
       (d> (min-z a) (max-z b))
       (d< (max-x a) (min-x b))
       (d< (max-y a) (min-y b))
       (d< (max-z a) (min-z b)))))

(defmethod flatten-to-aabb2-xz ((object aabb))
  (vec4 (min-x object) (min-z object) (max-x object) (max-z object)))

(defmethod reset ((object aabb))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (aabb object))
  (setf (aabb-p1 object) (copy-vec +safe-p1-aabb+)
	(aabb-p2 object) (copy-vec +safe-p2-aabb+)))

(defclass bounding-sphere ()
  ((sphere-center
    :initform (vec 0.0 0.0 0.0)
    :initarg  :sphere-center
    :accessor sphere-center)
   (sphere-radius
    :initform 1.0
    :initarg  :sphere-radius
    :accessor sphere-radius)))

(defmethod print-object ((object bounding-sphere) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "center ~a radius ~a" (sphere-center object) (sphere-radius object))))

(defun aabb->bounding-sphere (aabb)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (let* ((center (vec/ (vec+ (aabb-p1 aabb) (aabb-p2 aabb)) 2.0))
	 (radius (vec-length (vec- (aabb-p2 aabb) center))))
    (make-instance 'bounding-sphere :sphere-radius radius :sphere-center center)))

(defun triangle-normal (a b c &key (normalize t))
  "counterclockwise"
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec a b c))
  (let* ((a->b (vec- b a))
	 (b->c (vec- c b))
	 (cross-product (cross-product a->b b->c)))
    (if normalize
	(normalize cross-product)
	cross-product)))

(defun triangle-centroid (a b c)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec a b c))
  (vec/ (vec+ a (vec+ b c)) 3.0))

(defun tangent-TBN (a b c tex-a tex-b tex-c)
  "counterclockwise"
  (let* ((e1 (vec- b a))
	 (e2 (vec- c b))
	 (normal (triangle-normal a b c :normalize t))
	 (delta-texture-1 (vec2:vec2- tex-b tex-a))
	 (delta-texture-2 (vec2:vec2- tex-c tex-b))
	 (du1 (elt delta-texture-1 0))
	 (dv1 (elt delta-texture-1 1))
	 (du2 (elt delta-texture-2 0))
	 (dv2 (elt delta-texture-2 1))
	 (k   (d/ 1.0 (d- (d* du1 dv2) (d* du2 dv1))))
	 (tangent (normalize (vec (d* k (d+ (d* dv2 (elt e1 0)) (d* (d- dv1) (elt e2 0))))
				  (d* k (d+ (d* dv2 (elt e1 1)) (d* (d- dv1) (elt e2 1))))
				  (d* k (d+ (d* dv2 (elt e1 2)) (d* (d- dv1) (elt e2 2)))))))
	 (bitangent (normalize (vec (d* k (d+ (d* (d- du2) (elt e1 0)) (d* du1 (elt e2 0))))
				    (d* k (d+ (d* (d- du2) (elt e1 1)) (d* du1 (elt e2 1))))
				    (d* k (d+ (d* (d- du2) (elt e1 2)) (d* du1 (elt e2 2))))))))
    (matrix (elt tangent 0)   (elt tangent 1)   (elt tangent 2)   1.0
	    (elt bitangent 0) (elt bitangent 1) (elt bitangent 2) 1.0
	    (elt normal 0)    (elt normal 1)    (elt normal 2)    1.0
	    1.0               1.0               1.0               1.0)))

(defun tangent-in-normal-space (a b c tex-a tex-b tex-c &key (normalize t))
  "counterclockwise"
  (let* ((e1 (vec- b a))
	 (e2 (vec- c b))
	 (delta-texture-1 (vec2:vec2- tex-b tex-a))
	 (delta-texture-2 (vec2:vec2- tex-c tex-b))
	 (du1 (elt delta-texture-1 0))
	 (dv1 (elt delta-texture-1 1))
	 (du2 (elt delta-texture-2 0))
	 (dv2 (elt delta-texture-2 1))
	 (k   (d/ 1.0 (d- (d* du1 dv2) (d* du2 dv1))))
	 (t1 (d* k (d+ (d* dv2 (elt e1 0)) (d* (d- dv1) (elt e2 0)))))
	 (t2 (d* k (d+ (d* dv2 (elt e1 1)) (d* (d- dv1) (elt e2 1)))))
	 (t3 (d* k (d+ (d* dv2 (elt e1 2)) (d* (d- dv1) (elt e2 2)))))
	 (res (vec t1 t2 t3)))
    (if normalize
	(normalize res)
	res)))

(definline vec-average (&rest vecs)
  (vec/ (reduce #'(lambda (a b) (vec+ a b)) vecs :initial-value constants:+zero-vec+)
       (coerce (length vecs) 'single-float)))

(definline vec-average* (vecs)
  (vec/ (reduce #'(lambda (a b) (vec+ a b)) vecs :initial-value constants:+zero-vec+)
       (coerce (length vecs) 'single-float)))

(defun ccw-poly-fannify (vertices)
  (if (= (length vertices) 3)
      (list vertices)
      (nconc (list
	      (vector (elt vertices 0)
		      (elt vertices 1)
		      (elt vertices 2)))
	     (ccw-poly-fannify (delete@ vertices 1)))))

(defun plane-equation (a b c)
  (let* ((n (triangle-normal a b c :normalize t))
	 (d (dot-product n a)))
    (values n d)))

(defun plane-equation-as-vec4 (a b c)
  (let* ((n (triangle-normal a b c :normalize t))
	 (d (dot-product n a)))
    (vec->vec4 n d)))

(defun same-plane-p (vertex &optional (tolerance 1e-6))
  (cond
    ((<= (length vertex) 3)
     t)
    (t
     (let ((*default-epsilon* tolerance))
       (multiple-value-bind (n d)
	   (plane-equation (first vertex) (second vertex) (third vertex))
	 (every #'identity
		(mapcar #'(lambda(a) (epsilon= (dot-product a n) d))
			vertex)))))))

(defun same-plane-p* (&rest vertex)
  (same-plane-p vertex))

(defun plane-point-same-side-p (plane p)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec4 plane))
  (declare (vec p))
  (d> (d+ (d* (elt plane 0) (elt p 0))
	   (d* (elt plane 1) (elt p 1))
	   (d* (elt plane 2) (elt p 2))
	   (elt plane 3))
       0.0))

(defun extract-frustum-plane (plane matrix row)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec4 plane))
  (declare (matrix matrix))
  (declare (fixnum row))
  (let ((scale (if (< row 0)
		   -1.0
		   1.0))
	(act-row (f- (abs row) 1)))
    (setf (elt plane 0) (d+ (elt matrix 3)  (d* scale (elt matrix act-row)))
	  (elt plane 1) (d+ (elt matrix 7)  (d* scale (elt matrix (f+ act-row 4))))
	  (elt plane 2) (d+ (elt matrix 11) (d* scale (elt matrix (f+ act-row 8))))
	  (elt plane 3) (d+ (elt matrix 15) (d* scale (elt matrix (f+ act-row 12)))))))

(defun 3-planes-intersection (a b c)
  (let ((n1 (vec (elt a 0) (elt a 1) (elt a 2)))
	(n2 (vec (elt b 0) (elt b 1) (elt b 2)))
	(n3 (vec (elt c 0) (elt c 1) (elt c 2)))
	(d1 (elt a 3))
	(d2 (elt b 3))
	(d3 (elt c 3)))
    (vec* (vec+
	   (vec+
	    (vec* (cross-product n2 n3) d1)
	    (vec* (cross-product n3 n1) d2))
	   (vec* (cross-product n1 n2) d3))
	  (d/ 1.0
	      (dot-product (cross-product n1 n2) n3)))))

(defun copy-matrix-element (from to row column)
  (setf (mref to row column) (mref from row column)))

(defmacro clone-matrix (matrix)
  (alexandria:with-gensyms (results)
    `(let ((,results (zero-matrix)))
       ,@(loop for r from 0 below 4 collect
	      `(progn
		 ,@(loop for c from 0 below 4 collect
			`(copy-matrix-element ,matrix ,results ,r ,c))))
       ,results)))

(alexandria:define-constant +projective-scale-bias+ (matrix 0.5 0.0 0.0 0.5
							    0.0 0.5 0.0 0.5
							    0.0 0.0 0.5 0.5
							    0.0 0.0 0.0 1.0)
  :test #'matrix~)

(defun-inline-function ortho (left right bottom top near far)
  (matrix (d/ 2.0 (d- right left)) 0.0 0.0 (d/ (d- (d+ right left)) (d- right left))
 	  0.0 (d/ 2.0 (d- top  bottom)) 0.0 (d/ (d- (d+ top bottom)) (d- top bottom))
 	  0.0 0.0 (d/ -2.0 (d- far near)) (d/ (d- (d+ far near)) (d- far near))
 	  0.0 0.0 0.0 1.0))

(define-compiler-macros ortho left right bottom top near far)

(defun-inline-function ortho* (left right bottom top)
  (matrix (d/ 2.0 (d- right left)) 0.0 0.0 (d/ (d- (d+ right left)) (d- right  left))
	  0.0 (d/ 2.0 (d- top bottom)) 0.0 (d/ (d+ top  bottom) (d- top  bottom))
	  0.0 0.0 -1.0 0.0
	  0.0 0.0 0.0 1.0))

(define-compiler-macros ortho* left right bottom top)

(defun-inline-function perspective (fovy aspect near far)
  (let* ((rad (deg->rad fovy))
	 (tan-half-fovy (dtan (d/ rad 2.0)))
	 (el-0-0 (d/ 1.0 (d* aspect tan-half-fovy)))
	 (el-1-1 (d/ 1.0 tan-half-fovy))
	 (el-2-2 (d- (d/ (d+ near far) (d- far near))))
	 (el-3-2 (d- (/ (d* 2.0 near far) (d- far near)))))
    (matrix el-0-0 0.0     0.0    0.0
	    0.0    el-1-1  0.0    0.0
	    0.0    0.0     el-2-2 el-3-2
	    0.0    0.0    -1.0    0.0   )))

(define-compiler-macros perspective fovy aspect near far)

(defun-inline-function perspective-fov (fov width height near far)
  (let* ((rad (deg->rad fov))
	 (h   (desired (d/ (cos (d* 0.5 * rad)) (sin (d* 0.5 * rad)))))
	 (w   (d/ (d* h height) width))
	 (el-3-2 (d- (d/ (d* 2.0 far near) (d- far near))))
	 (el-2-2 (d- (d/ (d+ far near) (d- far near)))))
    (matrix w   0.0  0.0    0.0
	    0.0 h    0.0    0.0
	    0.0 0.0  el-2-2 el-3-2
	    0.0 0.0 -1.0    0.0)))

(define-compiler-macros perspective-fov fovy width height near far)

(defun-inline-function infinite-perspective (fovy aspect near)
  (let* ((range (d* (tan (deg->rad (d/ fovy 2.0))) near))
	 (left (d* (d- range) aspect))
	 (right (d* range aspect))
	 (bottom (d- range))
	 (top range)
	 (el-0-0 (d/ (d* 2.0 near) (d- right left)))
	 (el-1-1 (d/ (d* 2.0 near) (d- top bottom)))
	 (el-3-2 (d* -2.0 near)))
    (matrix el-0-0 0.0     0.0  0.0
	    0.0    el-1-1  0.0  0.0
	    0.0    0.0    -1.0  el-3-2
	    0.0    0.0    -1.0  0.0   )))

(define-compiler-macros infinite-perspective fovy aspect near)

(defun-inline-function frustum (left right bottom top near far)
  (let ((el-0-0 (d/ (d* 2.0 near) (d- right left)))
	(el-1-1 (d/ (d* 2.0 near) (d- top bottom)))
	(el-2-0 (d/ (d+ right left) (d- right left)))
	(el-2-1 (d/ (d+ top bottom) (d- top bottom)))
	(el-2-2 (d/ (d- (d+ far  near)) (d- far - near)))
	(el-3-2 (d/ (d- (d* 2.0  far near)) (d- far near))))
    (matrix el-0-0 0.0     el-2-0 0.0
	    0.0    el-1-1  el-2-1 0.0
	    0.0    0.0     el-2-2 el-3-2
	    0.0    0.0    -1.0    0.0)))

(define-compiler-macros frustum left right bottom top near far)

(defun-inline-function project (obj model proj viewport)
  (let ((vect (transform-vec4
	       (transform-vec4 (vec4 (elt obj 0) (elt obj 1) (elt obj 2) 1.0) model)
	       proj)))
    (setf vect (vec4/ vect (elt vect 3)))
    (setf vect (vec4
		(d+ (d* (elt vect 0) 0.5) 0.5)
		(d+ (d* (elt vect 1) 0.5) 0.5)
		(d+ (d* (elt vect 2) 0.5) 0.5)
		(d+ (d* (elt vect 3) 0.5) 0.5)))
    (setf (elt vect 0) (d+ (d* (elt vect 0) (elt viewport 2)) (elt viewport 0)))
    (setf (elt vect 1) (d+ (d* (elt vect 0) (elt viewport 3)) (elt viewport 1)))
    (vec (elt vect 0) (elt vect 1) (elt vect 2))))

(defun-inline-function look@ (eye center up)
  (let* ((f (normalize (vec- center eye)))
	 (s (normalize (cross-product f up)))
	 (u (cross-product s f)))
    (matrix (elt s 0) (elt s 1) (elt s 2) (d- (dot-product s eye))
	    (elt u 0) (elt u 1) (elt u 2) (d- (dot-product u eye))
	    (d- (elt f 0)) (d- (elt f 1)) (d- (elt f 2)) (dot-product f eye)
	    0.0 0.0 0.0 1.0)))

(defun-inline-function look@* (eye-x eye-y eye-z center-x center-y center-z up-x up-y up-z)
  (look@ (vec eye-x eye-y eye-z) (vec center-x center-y center-z)
	   (vec up-x up-y up-z)))

(define-compiler-macros look@* eye-x eye-y eye-z center-x center-y center-z up-x up-y up-z)

(defun-inline-function unproject (x y z model-view proj win-x win-y win-w win-h)
  (let* ((inv (inverse-matrix (matrix* proj model-view)))
	 (tmp (vec4- (vec4* (vec4 (d/ (d- x win-x) win-w) (d/ (d- y  win-y) win-h) z 1.0)
			    2.0)
		     (vec4 1.0 1.0 1.0 1.0)))
	 (obj (transform-vec4 tmp inv))
	 (w   (elt obj 3)))
    (vec/ (vec (elt obj 0) (elt obj 1) (elt obj 2)) w)))

(define-compiler-macros unproject x y z model-view proj win-x win-y win-w win-h)
