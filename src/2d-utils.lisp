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

(in-package :2d-utils)

(deftype uivec2 ()
  "A 2d vector of unsigned integer."
  `(simple-array (unsigned-byte 32) (2)))

(defun uivec2 (x y)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (let ((v (misc:make-array-frame 2 0 '(unsigned-byte 32) t)))
    (declare (uivec2 v))
    (setf (elt v 0) x
          (elt v 1) y)
    v))

(defun 90deg->rad (deg)
  (ecase deg
    (:90  +pi/2+)
    (:-90 (- +pi/2+))))

(misc:definline iaabb2-min-x (aabb)
  (elt aabb 0))

(misc:definline iaabb2-max-x (aabb)
  (elt aabb 2))

(misc:definline iaabb2-min-y (aabb)
  (elt aabb 1))

(misc:definline iaabb2-max-y (aabb)
  (elt aabb 3))

(defmacro gen-make-aabb (name vec-type)
  `(defun ,(misc:format-fn-symbol t "~a" name) (min-x min-y max-x max-y)
     (,vec-type min-x min-y max-x max-y)))

(gen-make-aabb make-iaabb2 ivec4)

(defun iaabb2~ (a b)
  (and
   (= (elt a 0) (elt b 0))
   (= (elt a 1) (elt b 1))
   (= (elt a 2) (elt b 2))
   (= (elt a 3) (elt b 3))))

(defun valid-iaabb2 (aabb)
  (and (>= (elt aabb 0) 0)
       (>= (elt aabb 1) 0)
       (>= (elt aabb 2) 0)
       (>= (elt aabb 3) 0)
       (> (elt aabb 2) (elt aabb 0))
       (> (elt aabb 3) (elt aabb 1))))

(defun expand-iaabb2 (aabb coord)
  (let ((cp (copy-ivec4 aabb)))
    (when (< (elt coord 0) (elt aabb 0))
      (setf (elt cp 0) (elt coord 0)))

    (when (> (elt coord 0) (elt aabb 2))
      (setf (elt cp 2) (elt coord 0)))

    (when (< (elt coord 1) (elt aabb 1))
      (setf (elt cp 1) (elt coord 1)))

    (when (> (elt coord 1) (elt aabb 3))
      (setf (elt cp 3) (elt coord 1)))
    cp))

(defun union-iaabb2 (aabb aabb2)
  (let ((cp (copy-ivec4 aabb)))
    (setf cp (expand-iaabb2 cp (subseq aabb2 0 2)))
    (setf cp (expand-iaabb2 cp (list (elt aabb2 2) (elt aabb2 1))))
    (setf cp (expand-iaabb2 cp (list (elt aabb2 2) (elt aabb2 3))))
    (setf cp (expand-iaabb2 cp (list (elt aabb2 0) (elt aabb2 3))))
    cp))

(defun iaabb2->irect2 (coords)
  "(upper-left-x upper-left-y bottom-right-x bottom-right-y) to
   (upper-left-x upper-left-y  w h)"
  (let ((x1 (elt coords 0))
        (y1 (elt coords 1))
        (x2 (elt coords 2))
        (y2 (elt coords 3)))
  (ivec4 x1 y1 (- x2 x1) (- y2 y1))))

(defun irect2->iaabb2 (coords)
  "(upper-left-x upper-left-y  w h) to
   (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  (let ((x1 (elt coords 0))
        (y1 (elt coords 1))
        (w (elt coords 2))
        (h (elt coords 3)))
  (ivec4 x1 y1 (+ x1 w) (+ y1 h))))

(defun inside-iaabb2-p (aabb x y)
  "t if x y is inside this bounding box
   aabb is: (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  (and
   (>= x (elt aabb 0))
   (<= x (elt aabb 2))
   (>= y (elt aabb 1))
   (<= y (elt aabb 3))))

(defun iaabb2-intersect-p (aabb1 aabb2)
  (if
   (or
    (>= (elt aabb1 0) (elt aabb2 2))
    (<= (elt aabb1 2) (elt aabb2 0))
    (>= (elt aabb1 1) (elt aabb2 3))
    (<= (elt aabb1 3) (elt aabb2 1)))
   nil
   t))

(defun iaabb2-inglobe-p (host guest)
  (and (inside-iaabb2-p host (iaabb2-min-x guest) (iaabb2-min-x guest))
       (inside-iaabb2-p host (iaabb2-max-x guest) (iaabb2-max-x guest))))

(defun iaabb2-null-p (aabb)
  (let ((rect (iaabb2->irect2 aabb)))
    (and (= 0 (elt rect 2))
         (= 0 (elt rect 3)))))

(defun trasl-iaabb2 (aabb &optional (dx (- (elt aabb 0))) (dy (- (elt aabb 1))))
  (ivec4 (+ (elt aabb 0) dx)
         (+ (elt aabb 1) dy)
         (+ (elt aabb 2) dx)
         (+ (elt aabb 3) dy)))

(defun trasl-irect2 (rect &optional (dx (- (elt rect 0))) (dy (- (elt rect 1))))
  (ivec4 (+ (elt rect 0) dx)
         (+ (elt rect 1) dy)
         (elt rect 2)
         (elt rect 3)))

(defun rotate-iaabb2* (aabb angle &key (rounding-fn #'round))
  (let* ((vertices (list
                    (2d-vector-rotate (list (elt aabb 0) (elt aabb 1)) angle)
                    (2d-vector-rotate (list (elt aabb 2) (elt aabb 1)) angle)
                    (2d-vector-rotate (list (elt aabb 2) (elt aabb 3)) angle)
                    (2d-vector-rotate (list (elt aabb 0) (elt aabb 3)) angle)))
         (all-x (mapcar #'(lambda (v) (funcall rounding-fn (elt v 0))) vertices))
         (all-y (mapcar #'(lambda (v) (funcall rounding-fn (elt v 1))) vertices)))
    (ivec4 (find-min all-x) (find-min all-y)
           (find-max all-x) (find-max all-y))))

(defun center-iaabb2 (aabb)
  (let ((rect (iaabb2->irect2 aabb)))
    (ivec2 (+ (elt rect 0) (/ (elt rect 2) 2))
           (+ (elt rect 1) (/ (elt rect 3) 2)))))

(defun rotate-iaabb2 (aabb angle &optional (pivot (list 0 0)))
  (let ((traslated (trasl-iaabb2 aabb (- (elt pivot 0)) (- (elt pivot 1)))))
    (trasl-iaabb2 (rotate-iaabb2* traslated angle) (elt pivot 0) (elt pivot 1))))

(defparameter *sigma-rand* 0.001)

(defun random-sub-iaabb2 (aabb &optional (sigmaw 1) (sigmah 1)
                        (randomfunc #'aabb-safe-random))
  (let ((trasl-aabb (trasl-iaabb2 aabb)))
    (ivec4 0 0
          (let ((*sigma-rand* sigmaw))
            (funcall randomfunc (elt trasl-aabb 2)))
          (let ((*sigma-rand* sigmah))
            (funcall randomfunc (elt trasl-aabb 3))))))

(defun random-sub-irect2 (rect &optional (sigmaw 1) (sigmah 1)
                                 (randomfunc #'aabb-safe-random)
                                 (rounding-fn #'round))
  (ivec4 0 0
          (funcall rounding-fn
                   (let ((*sigma-rand* sigmaw))
                     (funcall randomfunc (elt rect 2))))
          (funcall rounding-fn
                   (let ((*sigma-rand* sigmah))
                     (funcall randomfunc (elt rect 3))))))
;;; float aabb

(gen-make-aabb make-aabb2 vec4)

(defun aabb2~ (a b)
  (and
   (epsilon= (elt a 0) (elt b 0))
   (epsilon= (elt a 1) (elt b 1))
   (epsilon= (elt a 2) (elt b 2))
   (epsilon= (elt a 3) (elt b 3))))

(misc:definline aabb2-min-x (aabb)
  (elt aabb 0))

(misc:definline aabb2-max-x (aabb)
  (elt aabb 2))

(misc:definline aabb2-min-y (aabb)
  (elt aabb 1))

(misc:definline aabb2-max-y (aabb)
  (elt aabb 3))

(define-condition invalid-aabb-error (error)
  ((aabb
    :initarg :aabb
    :reader aabb))
  (:report (lambda (condition stream)
             (format stream "invalid aabb ~a" (aabb condition))))
  (:documentation "Error when aabb is invalid"))

(defun valid-aabb2-p (aabb)
  (and (>= (elt aabb 0) 0)
       (>= (elt aabb 1) 0)
       (>= (elt aabb 2) 0)
       (>= (elt aabb 3) 0)
       (> (elt aabb 2) (elt aabb 0))
       (> (elt aabb 3) (elt aabb 1))))

(defun expand-aabb2 (aabb coord)
  (let ((cp (copy-vec4 aabb)))
    (when (< (elt coord 0) (elt aabb 0))
      (setf (elt cp 0) (elt coord 0)))

    (when (> (elt coord 0) (elt aabb 2))
      (setf (elt cp 2) (elt coord 0)))

    (when (< (elt coord 1) (elt aabb 1))
      (setf (elt cp 1) (elt coord 1)))

    (when (> (elt coord 1) (elt aabb 3))
      (setf (elt cp 3) (elt coord 1)))
    cp))

(defun nexpand-aabb2 (aabb coord)
  (when (< (elt coord 0) (elt aabb 0))
    (setf (elt aabb 0) (elt coord 0)))

  (when (> (elt coord 0) (elt aabb 2))
    (setf (elt aabb 2) (elt coord 0)))

  (when (< (elt coord 1) (elt aabb 1))
    (setf (elt aabb 1) (elt coord 1)))

  (when (> (elt coord 1) (elt aabb 3))
    (setf (elt aabb 3) (elt coord 1)))
  aabb)

(defun union-aabb2 (aabb aabb2)
  (let ((cp (copy-vec4 aabb)))
    (setf cp (expand-aabb2 cp (subseq aabb2 0 2)))
    (setf cp (expand-aabb2 cp (list (elt aabb2 2) (elt aabb2 1))))
    (setf cp (expand-aabb2 cp (list (elt aabb2 2) (elt aabb2 3))))
    (setf cp (expand-aabb2 cp (list (elt aabb2 0) (elt aabb2 3))))
    cp))

(defun aabb2->rect2 (coords)
  "(upper-left-x upper-left-y bottom-right-x bottom-right-y) to
   (upper-left-x upper-left-y  w h)"
  (let ((x1 (elt coords 0))
        (y1 (elt coords 1))
        (x2 (elt coords 2))
        (y2 (elt coords 3)))
  (vec4 x1 y1 (- x2 x1) (- y2 y1))))

(defun rect2->aabb2 (coords)
  "(upper-left-x upper-left-y  w h) to
   (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  (let ((x1 (elt coords 0))
        (y1 (elt coords 1))
        (w (elt coords 2))
        (h (elt coords 3)))
  (vec4 x1 y1 (+ x1 w) (+ y1 h))))

(defun inside-aabb2-p (aabb x y)
  "t if x y is inside this bounding box
   aabb is: (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  (and
   (>= x (elt aabb 0))
   (<= x (elt aabb 2))
   (>= y (elt aabb 1))
   (<= y (elt aabb 3))))

(defun aabb2-intersect-p (aabb1 aabb2)
  (if
   (or
    (>= (elt aabb1 0) (elt aabb2 2))
    (<= (elt aabb1 2) (elt aabb2 0))
    (>= (elt aabb1 1) (elt aabb2 3))
    (<= (elt aabb1 3) (elt aabb2 1)))
   nil
   t))

(defun aabb2-inglobe-p (host guest)
  (and (inside-aabb2-p host (aabb2-min-x guest) (aabb2-min-y guest))
       (inside-aabb2-p host (aabb2-max-x guest) (aabb2-max-y guest))))

(defun approx-aabb2-intersect-p (aabb1 aabb2 enlarge)
  (if enlarge
   (or
    (not (epsilon= (elt aabb1 0) (elt aabb2 2)))
    (not (epsilon= (elt aabb1 2) (elt aabb2 0)))
    (not (epsilon= (elt aabb1 1) (elt aabb2 3)))
    (not (epsilon= (elt aabb1 3) (elt aabb2 1))))
   (not
    (or
     (epsilon= (elt aabb1 0) (elt aabb2 2))
     (epsilon= (elt aabb1 2) (elt aabb2 0))
     (epsilon= (elt aabb1 1) (elt aabb2 3))
     (epsilon= (elt aabb1 3) (elt aabb2 1))))))

(defun aabb2-null-p (aabb)
  (let ((rect (aabb2->rect2 aabb)))
    (and (= 0 (elt rect 2))
         (= 0 (elt rect 3)))))

(defun trasl-aabb2 (aabb &optional (dx (- (elt aabb 0))) (dy (- (elt aabb 1))))
  (vec4 (+ (elt aabb 0) dx)
        (+ (elt aabb 1) dy)
        (+ (elt aabb 2) dx)
        (+ (elt aabb 3) dy)))

(defun trasl-rect2 (rect &optional (dx (- (elt rect 0))) (dy (- (elt rect 1))))
  (vec4 (+ (elt rect 0) dx)
        (+ (elt rect 1) dy)
        (elt rect 2)
        (elt rect 3)))

(defun rotate-aabb2* (aabb angle)
  (let* ((vertices (list
                    (2d-vector-rotate (list (elt aabb 0) (elt aabb 1)) angle)
                    (2d-vector-rotate (list (elt aabb 2) (elt aabb 1)) angle)
                    (2d-vector-rotate (list (elt aabb 2) (elt aabb 3)) angle)
                    (2d-vector-rotate (list (elt aabb 0) (elt aabb 3)) angle)))
         (all-x (mapcar #'(lambda (v) (elt v 0)) vertices))
         (all-y (mapcar #'(lambda (v) (elt v 1)) vertices)))
    (vec4 (find-min all-x) (find-min all-y)
          (find-max all-x) (find-max all-y))))

(defun center-aabb2 (aabb)
  (let ((rect (aabb2->rect2 aabb)))
    (vec2 (+ (elt rect 0) (/ (elt rect 2) 2))
          (+ (elt rect 1) (/ (elt rect 3) 2)))))

(defun rotate-aabb2 (aabb angle &optional (pivot (list 0 0)))
  (let ((traslated (trasl-aabb2 aabb (- (elt pivot 0)) (- (elt pivot 1)))))
    (trasl-aabb2 (rotate-aabb2* traslated angle) (elt pivot 0) (elt pivot 1))))

(defun scale-aabb2 (aabb scale-x scale-y)
  (let ((center (center-aabb2 aabb)))
    (let* ((cx (elt center 0))
           (cy (elt center 1))
           (translated (trasl-aabb2 aabb (- cx) (- cy)))
           (a (* (aabb2-min-x translated) scale-x))
           (b (* (aabb2-min-y translated) scale-y))
           (c (* (aabb2-max-x translated) scale-x))
           (d (* (aabb2-max-y translated) scale-y)))
      (vec4 (+ a cx) (+ b cy) (+ c cx) (+ d cy)))))

(defparameter *sigma-rand* 0.001)

(defun aabb-safe-random (size)
   (if (< size 1)
       0.0
       (let ((randw (abs (first (random-gaussian-distribution *sigma-rand*)))))
         (if (< randw size)
             (if (= 0 randw) 1 randw)
             size))))

(defun random-sub-aabb (aabb &optional (sigmaw 1) (sigmah 1)
                        (randomfunc #'aabb-safe-random))
  (let ((trasl-aabb (trasl-aabb2 aabb)))
    (vec4 0.0 0.0
          (let ((*sigma-rand* sigmaw))
            (funcall randomfunc (elt trasl-aabb 2)))
          (let ((*sigma-rand* sigmah))
            (funcall randomfunc (elt trasl-aabb 3))))))

(defun random-sub-rect (rect &optional (sigmaw 1) (sigmah 1)
                        (randomfunc #'aabb-safe-random))
  (vec4 0.0 0.0
        (let ((*sigma-rand* sigmaw))
          (funcall randomfunc (elt rect 2)))
        (let ((*sigma-rand* sigmah))
          (funcall randomfunc (elt rect 3)))))

(defun line-eqn (a b &optional (thresh 1e-5))
  "Calculate a bidimensional line equation crossing vector a and b.
   Return a list containing m q and two flag indicating if the line is
   parallel to x or y respectively"
  (let ((dy (- (elt b 1) (elt a 1)))
        (dx (- (elt b 0) (elt a 0))))
    (cond
      ((<= 0 dy thresh) ;parallel to x
       (list 0 (elt b 1) t nil))
      ((<= 0 dx thresh) ; parallel to y
       (list 0 0 nil t))
      (t
       (list (/ dy dx) (- (elt a 1) (* (/ dy dx) (elt a 0))) nil nil)))))

(defmacro funcall-if-not-null (func val)
  (if (not (null func))
      `(funcall ,func ,val)
      val))

(defmacro displace-2d-vector ((v x y) &body body)
  `(let ((,x (elt ,v 0))
         (,y (elt ,v 1)))
     ,@body))

(defun 2d-vector-map (v &key (funcx nil) (funcy nil))
  "Return a list of x,y values of the vector transformed by funcx and funcy (if not nil) respectively"
  (list
   (if (not (null funcx))
       (funcall-if-not-null funcx (elt v 0))
       (funcall-if-not-null nil (elt v 0)))

   (if (not (null funcy))
       (funcall-if-not-null funcy (elt v 1))
       (funcall-if-not-null nil (elt v 1)))))

(defun 2d-vector-list-map (pairs &key (funcx nil) (funcy nil))
  "Remap pairs applying funcx and funcy (if not nil) to each component"
  (mapcar #'(lambda (v) (2d-vector-map v :funcx funcx :funcy funcy)) pairs))

(defun 2d-vector-list-scale (pairs &optional (ax 1) (ay 1))
  "Remap pairs scaling each components by ax and ay"
  (mapcar #'(lambda (v) (2d-vector-scale v ax ay)) pairs))

(defun 2d-vector-list-translate (pairs &optional (dx 0) (dy 0))
  "translate pairs by dx and dy"
  (mapcar #'(lambda (v) (2d-vector-map v
                                       :funcx #'(lambda (x) (+ x dx))
                                       :funcy #'(lambda (y) (+ y dy))))
          pairs))

(defun 2d-vector-list-rotate (pairs angle)
  (mapcar #'(lambda (v) (2d-vector-rotate v angle)) pairs))

(defgeneric 2d-vector-sum (a b))

(defmethod 2d-vector-sum ((a list) (b list))
  (mapcar #'(lambda (x y) (+ x y)) a b))

(defmethod 2d-vector-sum ((a vector) (b vector))
  (map 'vector #'(lambda (x y) (+ x y)) a b))

(defmethod 2d-vector-sum (a b)
  (map 'vector #'(lambda (x y) (+ x y)) a b))

(defgeneric 2d-vector-diff (a b))

(defmethod 2d-vector-diff ((a list) (b list))
  (mapcar #'(lambda (x y) (- x y)) a b))

(defmethod 2d-vector-diff ((a vector) (b vector))
  (map 'vector #'(lambda (x y) (- x y)) a b))

(defmethod 2d-vector-diff (a b)
  (map 'vector #'(lambda (x y) (- x y)) a b))

(defgeneric d2d-vector-diff (a b))

(defmethod d2d-vector-diff ((a list) (b list))
  (mapcar #'(lambda (x y) (d- (desired x) (desired y))) a b))

(defmethod d2d-vector-diff ((a vector) (b vector))
  (map 'vector #'(lambda (x y) (d- (desired x) (desired y))) a b))

(defun 2d-vector-dot-product (a b)
  (+ (* (elt a 0) (elt b 0)) (* (elt a 1) (elt b 1))))

(defun d2d-vector-dot-product (a b)
  (d+ (d* (desired (elt a 0)) (desired (elt b 0)))
          (d* (desired (elt a 1)) (desired (elt b 1)))))

(defun 2d-vector-cross-product (a b)
  (- (* (elt a 0) (elt b 1)) (* (elt a 1) (elt b 0))))

(defgeneric 2d-vector-scale (a amount-x &optional amount-y))

(defmethod 2d-vector-scale ((a list) amount-x &optional (amount-y amount-x))
  (list (* amount-x (elt a 0)) (* amount-y (elt a 1))))

(defmethod 2d-vector-scale ((a vector) amount-x &optional (amount-y amount-x))
  (vector (* amount-x (elt a 0)) (* amount-y (elt a 1))))

(defgeneric 2d-vector-translate (a amount-x &optional amount-y))

(defmethod 2d-vector-translate ((a list) amount-x &optional (amount-y amount-x))
  (list (+ amount-x (elt a 0)) (+ amount-y (elt a 1))))

(defmethod 2d-vector-translate ((a vector) amount-x &optional (amount-y amount-x))
  (vector (+ amount-x (elt a 0)) (+ amount-y (elt a 1))))

(defun 2d-vector-magn (a)
  (sqrt (+ (expt (elt a 0) 2) (expt (elt a 1) 2))))

(defgeneric 2d-vector-normalize (a))

(defmethod 2d-vector-normalize ((a list))
  (let ((mag (2d-vector-magn a)))
    (list (/ (elt a 0) mag) (/ (elt a 1) mag))))

(defmethod 2d-vector-normalize ((a vector))
  (let ((mag (2d-vector-magn a)))
    (vector (/ (elt a 0) mag) (/ (elt a 1) mag))))

(defun 2d-vector-angle (a b)
  (let* ((a-norm (2d-vector-normalize a))
         (b-norm (2d-vector-normalize b))
         (dot-product (2d-vector-dot-product a-norm b-norm))
         (angle (acos dot-product)))
    (if (< (2d-vector-cross-product a b) 0)
        (- angle)
        angle)))

(defgeneric 2d-vector-rotate (a angle)
  (:documentation "Angle in radians"))

(defmethod 2d-vector-rotate ((a list) angle)
  (list
   (- (* (elt a 0) (cos angle)) (* (elt a 1) (sin angle)))
   (+ (* (elt a 0) (sin angle)) (* (elt a 1) (cos angle)))))

(defmethod 2d-vector-rotate ((a vector) angle)
  (vector
   (- (* (elt a 0) (cos angle)) (* (elt a 1) (sin angle)))
   (+ (* (elt a 0) (sin angle)) (* (elt a 1) (cos angle)))))

(defun xy->pair (xs ys)
  "Convert (x1 x2 x3...) (y1 y2 y3...) to ((x1 y1) (x2 y2) (x3 y3) ...)"
  (mapcar #'(lambda (x y) (list x y)) xs ys))

(defun pair->interleaved-xy (x-y)
  "Convert ((x1 y1) (x2 y2) (x3 y3) ...) to (x1 y1 x2 y2 x3 y3 ...)"
  (reduce #'append x-y))

(defun xy->interleaved-xy (xs ys &key (modfunc-x nil) (modfunc-y nil))
  "Convert (x1 x2 x3...) (y1 y2 y3...) to ( (funcall modfunc-x x1) (funcall modfunc-y y1)...)"
  (pair->interleaved-xy (xy->pair (if (not (null modfunc-x))
                                      (mapcar modfunc-x xs)
                                      xs)
                                  (if (not (null modfunc-y))
                                      (mapcar modfunc-y ys)
                                      ys))))

(defun interleaved-xy->pair (xy)
  (macrolet ((get-from-list (when-clause list)
               `(loop
                   for i in ,list
                   for c = 0 then (1+ c)
                   when (,when-clause c)
                   collect i)))
    (let ((xs (get-from-list evenp xy))
          (ys (get-from-list oddp xy)))
      (xy->pair xs ys))))


(defun calc-quadrant (start end)
  (let* ((xend   (elt end   0))
         (xstart (elt start 0))
         (yend   (elt end   1))
         (ystart (elt start 1))
         (dy     (- yend ystart))
         (dx     (- xend xstart)))
     (cond
       ((and (>= dx 0)
             (>= dy 0))
        :ne)
       ((and (>=  dx 0)
             (minusp dy))
        :se)
       ((and (minusp dx)
             (>= dy 0))
        :nw)
       (t
        :sw))))

(defun calc-octant (start end)
  "Octants are:

     \2|1/
:nw  3\|/0 :ne
    ---+---
:sw  4/|\7 :se
     /5|6\
 "
  (let* ((quadrant (calc-quadrant start end))
         (line     (2d-utils:line-eqn start end))
         (m        (elt line 0)))
    (case quadrant
      (:ne
       (if (> m 1.0) 1 0))
      (:se
       (if (> m -1.0) 7 6))
      (:nw
       (if (< m -1.0) 2 3))
      (:sw
       (if (> m 1.0) 5 4)))))

(defun to-octant-0 (octant point)
  (let ((x (elt point 0))
        (y (elt point 1)))
    (cond
      ((= octant 0)
       point)
      ((= octant 1)
       (ivec2 y x))
      ((= octant 2)
       (ivec2 y (- x)))
      ((= octant 3)
       (ivec2 (- x) y))
      ((= octant 4)
       (ivec2 (- x) (- y)))
      ((= octant 5)
       (ivec2 (- y) (- x)))
      ((= octant 6)
       (ivec2 (- y) x))
      ((= octant 7)
       (ivec2 x (- y))))))

(defun from-octant-0 (octant point)
  (let ((x (elt point 0))
        (y (elt point 1)))
    (cond
      ((= octant 0)
       point)
      ((= octant 1)
       (ivec2 y x))
      ((= octant 2)
       (ivec2 (- y) x))
      ((= octant 3)
       (ivec2 (- x) y))
      ((= octant 4)
       (ivec2 (- x) (- y)))
      ((= octant 5)
       (ivec2 (- y) (- x)))
      ((= octant 6)
       (ivec2 y (- x)))
      ((= octant 7)
       (ivec2 x (- y))))))

(defun vertical-segment (start end)
  (assert (= (elt start 0) (elt end 0)))
  (let ((dy (- (elt end 1) (elt start 1))))
    (do ((res '())
         (y (elt start 1) (if (>= dy 0)
                              (1+ y)
                              (1- y))))
        ((if (>= dy 0)
             (> y (elt end 1))
             (< y (elt end 1)))
         (reverse res))
      (push (ivec2 (elt start 0) y) res))))

(defun segment-bresenham (start end)
  (if (= (elt start 0) (elt end 0))
      (vertical-segment start end)
      (let* ((octant       (calc-octant start end))
             (actual-start (to-octant-0 octant start))
             (actual-end   (to-octant-0 octant end))
             (xend         (elt actual-end   0))
             (xstart       (elt actual-start 0))
             (ystart       (elt actual-start 1))
             (dy           (- (elt actual-end 1) (elt actual-start 1)))
             (dx           (- (elt actual-end 0) (elt actual-start 0))))
        (do ((res '())
             (epsilon 0)
             (x xstart (1+ x))
             (y ystart))
            ((>= x (1+ xend)) (reverse res))
          (push (from-octant-0 octant (ivec2 x y)) res)
          (if (< (* 2 (+ epsilon dy))
                 dx)
              (incf epsilon dy)
              (progn
                (incf y)
                (incf epsilon (- dy dx))))))))

(defun antialiased-ipart (a)
  (floor a))

(defun antialiased-round (a)
  (antialiased-ipart (+ a 0.5)))

(defun antialiased-fpart (a)
  (if (< a 0)
      (1- (- a  (floor a)))
      (- a (floor a))))

(defun antialiased-rfpart (a)
  (- 1 (antialiased-fpart a)))

(defmacro with-translate-to-origin-octant-0 ((start end x0 y0 x1 y1 octant build-results-fn results)
                                             &body body)
  (alexandria:with-gensyms (origin swap-x-p swapped-start swapped-end actual-start actual-end)
      `(let* ((,results          '())
              (,swap-x-p     (> (elt start 0) (elt end 0)))
              (,origin       (ivec2 0 0))
              (,swapped-start   (if ,swap-x-p
                                    ,end
                                    ,start))
              (,swapped-end   (if ,swap-x-p
                                  (ivec2+ ,start (ivec2-negate ,end))
                                  (ivec2+ ,end   (ivec2-negate ,start))))
              (,octant       (calc-octant ,origin ,swapped-end))
              (,actual-start (to-octant-0 ,octant ,origin))
              (,actual-end   (to-octant-0 ,octant ,swapped-end))
              (,x0           (elt ,actual-start 0))
              (,y0           (elt ,actual-start 1))
              (,x1           (elt ,actual-end   0))
              (,y1           (elt ,actual-end   1)))
      (flet ((,build-results-fn (octant x y intensity)
               (push (list (ivec2+ ,swapped-start
                                   (from-octant-0 octant (ivec2 x y)))
                           (d intensity))
                     ,results)))
        ,@body))))

(defun segment-antialiased (start end)
  (with-translate-to-origin-octant-0 (start end x0 y0 x1 y1 octant build-results-fn results)
    (build-results-fn octant x0 y0 1.0)
    (let* ((dx       (- x1 x0))
           (dy       (- y1 y0))
           (gradient (if (= 0 dx) ;; never true when fallback is used
                             1
                             (/ dy dx)))
           (yend     (+ y0 (* gradient
                              (- (antialiased-round x0) x0))))
           (intery   (+ yend gradient))
           (xpxl1    (antialiased-round x0))
           (xpxl2    (antialiased-round x1)))
      (build-results-fn octant x1 y1 1.0)
      (loop for x from  xpxl1 below xpxl2 do
           (build-results-fn octant x (antialiased-ipart intery)       (antialiased-rfpart intery))
           (build-results-fn octant x (1+ (antialiased-ipart intery))  (antialiased-fpart intery))
           (setf intery (+ intery gradient)))
      results)))

(defun %vertical-segment-thick (start end width)
  (with-translate-to-origin-octant-0 (start end x0 y0 x1 y1 octant build-results-fn results)
      (let* ((segment            (segment-bresenham (ivec2 x0 y0) (ivec2 x1 y1)))
             (width/2            (floor (/ width 2))))
        (loop for point in segment do
             (displace-2d-vector (point x y)
               (loop for dx from (- width/2) to width/2 do
                    (build-results-fn octant (+ x dx) y 1.0))))
    results)))

(defun segment-thick (start end width antialiasp)
  (if (= (elt start 0) (elt end 0))
      (%vertical-segment-thick start end width)
      (with-translate-to-origin-octant-0 (start end x0 y0 x1 y1 octant build-results-fn results)
        (labels ((find-extremes (comp-fn)
                   #'(lambda (a b) (funcall comp-fn (elt a 0) (elt b 0))))
                 (draw-boundaries (segment)
                   (loop for point in segment do
                        (displace-2d-vector ((elt point 0) x y)
                          (let ((intensity (elt point 1)))
                            (build-results-fn octant x y intensity))))))
          (let* ((segment            (segment-bresenham (ivec2 x0 y0) (ivec2 x1 y1)))
                 (width/2            (floor (/ width 2)))
                 (min-x              (find-min-max (find-extremes #'<) segment))
                 (max-x              (find-min-max (find-extremes #'>) segment))
                 (lu                 (+ width/2 (elt min-x 1)))
                 (ru                 (+ width/2 (elt max-x 1)))
                 (ld                 (- (elt min-x 1) width/2))
                 (rd                 (- (elt max-x 1) width/2))
                 (segment-up-limit   (segment-antialiased (ivec2 (elt min-x 0) lu)
                                                          (ivec2 (elt max-x 0) ru)))
                 (segment-down-limit (segment-antialiased (ivec2 (elt min-x 0) ld)
                                                          (ivec2 (elt max-x 0) rd))))
            (loop for point in segment do
                 (displace-2d-vector (point x y)
                   (loop for dy from (- (1- width/2)) to width/2 do
                        (build-results-fn octant x (+ y dy) 1.0))))
            (when antialiasp
              (draw-boundaries segment-up-limit)
              (draw-boundaries segment-down-limit))))
        results)))

(defun segment (start end &key (antialiasp t) (width 1))
  (with-epsilon (1e-5)
    (cond
      ((> width 1)
       (segment-thick start end width antialiasp))
      ((or (not antialiasp)
           (epsilon= (elt start 0) (elt end 0))
           (epsilon= (elt start 1) (elt end 1))
           (epsilon= (d (abs (/ (- (elt start 1) (elt end 1))
                                (- (elt start 0) (elt end 0)))))
                     1.0))
       (mapcar #'(lambda (a) (list a 1.0)) (segment-bresenham start end)))
      (t
       (segment-antialiased start end)))))

(defun recursive-bezier (pairs &key (threshold 1))
  (labels ((midpoint (pb pe)
             (mapcar #'(lambda (x) (/ x 2)) (2d-vector-sum pb pe)))
           (eqvec-p (a b) (and (= (elt a 0) (elt b 0))
                               (= (elt a 1) (elt b 1)))))
    (let* ((p1 (elt pairs 0))
           (p2 (elt pairs 1))
           (p3 (elt pairs 2))
           (p4 (elt pairs 3))
           (p12 (midpoint p1 p2))
           (p23 (midpoint p2 p3))
           (p34 (midpoint p3 p4))
           (p12-23 (midpoint p12 p23))
           (p23-34 (midpoint p23 p34))
           (res (midpoint p12-23 p23-34)))
      (if (>= (2d-vector-magn (2d-vector-diff p1 res)) threshold)
          (remove-duplicates
           (append (list p1)
                   (recursive-bezier (list p1 p12 p12-23 res) :threshold threshold)
                   (list res)
                   (recursive-bezier (list res p23-34 p34 p4) :threshold threshold)
                   (list p4))
           :test #'eqvec-p)
          nil))))
