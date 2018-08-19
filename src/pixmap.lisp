;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

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

(in-package :pixmap)

(alexandria:define-constant +red-channel+   0 :test #'=)

(alexandria:define-constant +green-channel+ 1 :test #'=)

(alexandria:define-constant +blue-channel+  2 :test #'=)

(alexandria:define-constant +alpha-channel+ 3 :test #'=)

(alexandria:define-constant +targa-stream-element-type+ '(unsigned-byte 8) :test 'equalp)

(defclass pixmap (matrix)
  ((depth
    :initform 4
    :accessor depth
    :initarg :depth)
   (bits
    :initform (misc:make-array-frame 0)
    :accessor bits
    :initarg :bits)))

(defun make-pixmap-frame (w h &optional (depth 4) (bg (ubvec4 0 0 0 0)))
  (make-instance 'pixmap :depth depth :height h :width  w
                 :data (misc:make-array-frame (* h w) bg 'ubvec4 t)))

(defun make-pixmap (w h &optional (depth 4) (bg (ubvec4 0 0 0 0)))
  (make-instance 'pixmap :depth depth :height h :width  w
                 :data (misc:make-fresh-array (* h w) bg 'ubvec4 t)))

(defmethod marshal:class-persistant-slots ((object pixmap))
  (append '(depth) (call-next-method)))

(defmethod copy-matrix ((object pixmap))
  (make-instance 'pixmap :depth (depth object) :width (width object) :height (height object)
                 :data (alexandria:copy-array (data object))))

(defmethod clone ((object pixmap))
  (copy-matrix object))

(defmethod clone-into ((from pixmap) (to pixmap))
  (setf (depth  to) (depth from)
        (width  to) (width from)
        (height to) (height from)
        (data   to) (alexandria:copy-array (data from)))
  to)

(defmethod apply-kernel ((object pixmap) kernel &key (round-fn #'identity))
  (declare (matrix kernel))
  (declare (pixmap object))
  (declare (function round-fn))
  (let* ((res (make-pixmap-template object))
         (kernel-w (width kernel))
         (kernel-h (height kernel)))
    (nmap-matrix-xy object
                   #'(lambda (x y el)
                       (let ((sum (etypecase el
                                    (ubvec4 (make-fresh-ubvec4))
                                    (ivec4 (make-fresh-ivec4))
                                    (vec4 (make-fresh-vec4))
                                    (vector (misc:make-array-frame (length el) 0))
                                    (list (misc:make-fresh-list (length el) 0))
                                    (number 0))))
                         (loop
                            for r from (- y (floor (/ kernel-h 2))) below (+ kernel-h y)
                            for r1 from 0 below kernel-h do
                              (loop
                                 for c from (- x (floor (/ kernel-w 2))) below (+ kernel-w x)
                                 for c1 from 0 below kernel-w do
                                   (with-check-borders
                                       (c r 0 0 (1- (width object)) (1- (height object)))
                                     (setf sum (kernel-+ sum
                                                         (kernel-* (matrix-elt object r c)
                                                                   (matrix-elt kernel r1 c1))))
                                     (setf sum (kernel-+ sum
                                                         (kernel-* el
                                                                   (matrix-elt kernel r1 c1)))))))
                         (setf (matrix-elt res y x)
                               (round-all sum :rounding-function round-fn)))))
    res))

(defmethod papply-kernel ((object pixmap) kernel &key (round-fn #'identity))
  (declare (matrix kernel))
  (declare (pixmap object))
  (declare (function round-fn))
  (if (truecolorp object)
      (papply-kernel-ubvec4 object kernel :round-fn round-fn)
      (let* ((res (make-pixmap-template object))
             (kernel-w (width kernel))
             (kernel-h (height kernel)))
        (ploop-matrix (object x y)
          (let* ((el (matrix-elt object y x))
                 (sum (etypecase el
                        (ubvec4 (make-fresh-ubvec4))
                        (ivec4 (make-fresh-ivec4))
                        (vec4 (make-fresh-vec4))
                        (vector (misc:make-array-frame (length el) 0))
                        (list (misc:make-fresh-list (length el) 0))
                        (number 0))))
            (loop
               for r from (- y (floor (/ kernel-h 2))) below (+ kernel-h y)
               for r1 from 0 below kernel-h do
                 (loop
                    for c from (- x (floor (/ kernel-w 2))) below (+ kernel-w x)
                    for c1 from 0 below kernel-w do
                      (with-check-borders
                          (c r 0 0 (1- (width object)) (1- (height object)))
                        (setf sum (kernel-+ sum (kernel-* (matrix-elt object r c)
                                                          (matrix-elt kernel r1 c1))))
                        (setf sum (kernel-+ sum (kernel-* el
                                                          (matrix-elt kernel r1 c1)))))))
            (setf (matrix-elt res y x)
                  (round-all sum :rounding-function round-fn))))
        res)))

(defun ubvec4-kernel-+ (el delta)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ubvec4 el delta))
  (map 'ubvec4 #'(lambda (a b) (alexandria:clamp (f+ (the fixnum a) (the fixnum b)) 0 255))
       el delta))

(defun ubvec4-kernel-* (el mult)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ubvec4 el))
  (declare (desired-type mult))
  (map 'ubvec4 #'(lambda (a) (alexandria:clamp
                              (the fixnum (round (d* (desired (the fixnum a)) mult))) 0 255))
       el))

(defgeneric bits-pixel@ (object x y))

(defgeneric (setf bits-pixel@) (color object x y))

(defgeneric (setf alpha-bits@) (alpha-value object x y))

(defgeneric load-from-vector (object data))

(defgeneric papply-kernel-ubvec4 (object kernel &key round-fn))

(defgeneric truecolorp (object))

(defgeneric make-pixmap-template (object))

(defgeneric sync-data-to-bits (object))

(defgeneric sync-bits-to-data (object))

(defgeneric matrix->pixmap (object))

(defgeneric pixmap->ppm (object))

(defgeneric pixmap->tga-file (object))

(defgeneric draw-normalized-coord (object x y draw-function))

(defgeneric draw-float-normalized-coord (object x y draw-function))

(defgeneric draw-normalized-coord-custom-conversion (object x y function conversion-fn))

(defgeneric draw-sampled-coord (object x y draw-function))

(defgeneric ncopy-matrix-into-pixmap (object matrix &optional depth))

(defgeneric blit (src dest x-src y-src x-dst y-dst
                  &key w h function-blend))

(defgeneric pblit (src dest x-src y-src x-dst y-dst
                   &key w h function-blend))

(defgeneric pblit-w-repeat (src dest x-src y-src x-dst y-dst
                            &key
                              w h
                              behaivour-on-border-fn-x
                              behaivour-on-border-fn-y
                              discard-blit-fn
                              function-blend))

(defgeneric blit-unsafe (src dest x-src y-src x-dst y-dst
                         &key w h function-blend))

(defgeneric pblit-unsafe (src dest x-src y-src x-dst y-dst
                          &key w h function-blend))

(defgeneric tileize (object &key blend-replace-fn))

(defgeneric voronoize (object &key
                                size
                                tile-divisions
                                mean
                                sigma
                                max
                                average-size))

(defgeneric nrmse (object1 object2))

(defgeneric save-pixmap (object path))

(defgeneric to-grayscale (object))

(defgeneric dhash (object))

(defgeneric average-color (object aabb))

(defgeneric ppremultiply-alpha (object))

(defgeneric punremultiply-alpha (object))

(defun clear-to-color (pixmap &key (color (ubvec4 0 0 0 255)) (sync-p t))
  (let ((data (data pixmap)))
    (loop for i from 0 below (length data) do
         (setf (elt data i) color))
    (when sync-p
      (pixmap:sync-data-to-bits pixmap))
    pixmap))

(defmethod papply-kernel-ubvec4 ((object pixmap) kernel &key (round-fn #'identity))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap object))
  (declare (matrix kernel))
  (declare (function round-fn))
  (let* ((res (make-pixmap-template object))
         (kernel-w (width kernel))
         (kernel-h (height kernel)))
    (declare (pixmap res))
    (declare (fixnum kernel-h kernel-w))
    (ploop-matrix (object x y)
      (let* ((el (matrix-elt object y x))
             (sum (make-fresh-ubvec4)))
        (declare (ubvec4 el sum))
        (loop
           for r fixnum
           from (f- (the fixnum y) (the fixnum (truncate (/ kernel-h 2)))) below
             (f+ kernel-h y)
           for r1 fixnum from 0 below kernel-h do
             (loop
                for c fixnum
                from (f- (the fixnum x) (truncate (/ kernel-w 2))) below (f+ kernel-w x)
                for c1 fixnum from 0 below kernel-w do
                  (with-check-borders
                       (c r 0 0 (f- (the fixnum (width object)) 1)
                          (f- (the fixnum (height object)) 1))
                    ;; (with-check-borders
                    ;;    (c r 0 0 (1- (width object)) (1- (height object)))
                    (setf sum (ubvec4-kernel-+ sum (ubvec4-kernel-* (matrix-elt object r c)
                                                                    (matrix-elt kernel r1 c1))))
                    (setf sum (ubvec4-kernel-+ sum (ubvec4-kernel-* el
                                                                    (matrix-elt kernel r1 c1)))))))
        (setf (matrix-elt res y x)
              (round-all sum :rounding-function round-fn))))
    res))

(defmethod truecolorp ((object pixmap))
  (and (> (length (data object)) 0)
       (typep (elt (data object) 0) 'ubvec4)))

(defmethod make-pixmap-template ((object matrix))
  (make-instance 'pixmap
                 :depth (if (> (length (data object)) 0)
                            (if (arrayp (elt (data object) 0))
                                (length (elt (data object) 0))
                                1)
                            4)
                 :width (width object) :height (height object)
                 :data (misc:make-array-frame (* (width object) (height object))
                                              (if (> (length (data object)) 0)
                                                  (elt (data object) 0)
                                                  (ubvec4 0 0 0 0))
                                              (if (> (length (data object)) 0)
                                                  (element-type object)
                                                  'ubvec4)
                                              t)))

(defmethod sync-data-to-bits ((object pixmap))
  (with-accessors ((data data) (bits bits) (depth depth)) object
   (unwind-protect
        (progn
          (h-mirror-matrix object)
          (if (/= (length data) (* depth (length bits)))
              (progn
                (setf bits (misc:make-array-frame 0))
                (loop for pixel across data do
                     (loop for channel across pixel do
                          (vector-push-extend channel bits))))
              (loop for pixel-count from 0 below (length data) do
                   (loop for channel-count from 0 below depth do
                        (setf (elt bits (+ (* pixel-count depth) channel-count))
                              (elt (elt data pixel-count) channel-count))))))
     (h-mirror-matrix object)
     object)))

(defmethod sync-bits-to-data ((object pixmap))
  (with-accessors ((data data) (bits bits) (depth depth) (width width) (height height)) object
    (let ((pixel (make-fresh-ubvec4))
          (data-size (* width height)))
      (if (/= (length data) (* depth (length bits)))
          (progn
            (setf data (misc:make-array-frame data-size +ubvec4-zero+ 'ubvec4 t))
            (loop for i from 0 below (length bits) by depth do
                 (loop for channel-count from 0 below depth do
                      (setf (elt pixel channel-count) (elt bits (+ i channel-count))))
                 (setf (elt data (truncate (/ i depth))) (alexandria:copy-array pixel))))
          (loop for i from 0 below (length bits) by depth do
               (loop for channel-count from 0 below depth do
                    (setf (elt pixel channel-count)
                          (elt bits (+ i channel-count))))
               (setf (elt data (truncate (/ i depth))) (alexandria:copy-array pixel)))))
    (h-mirror-matrix object)))

(defun cristallize-bits (pixmap)
  "ensure the slot bits is a symple-array of fixnum"
  (with-accessors ((bits bits)) pixmap
    (let ((v (misc:make-array-frame (length (bits pixmap))
                                    0
                                    'fixnum
                                    t)))
    (loop for i from 0 below (length v) do
         (setf (elt v i) (elt bits i)))
    (setf (bits pixmap) v)
    pixmap)))

(defmethod voronoize ((object pixmap) &key
                                        (size (width object))
                                        (tile-divisions (floor (/ (width object) 10)))
                                        (mean 8)
                                        (sigma 2)
                                        (max 10)
                                        (average-size 5))
  (let ((worley (2d-worley-lut-indexed size tile-divisions 0 0 mean sigma max))
        (pixmap (make-pixmap-frame size size))
        (scale-x (d/ (desired (width object)) (desired size)))
        (scale-y (d/ (desired (height object)) (desired size))))
    (ploop-matrix (pixmap x y)
      (let* ((x-w (elt (elt (pixel@ worley x y) 0) 0))
             (y-w (elt (elt (pixel@ worley x y) 0) 1))
             (x-src (floor (d* x-w scale-x)))
             (y-src (floor (d* y-w scale-y)))
             (aabb-average-region (ivec4 (- x-src average-size) (- y-src average-size)
                                         (+ x-src average-size) (+ y-src average-size))))
        (setf (pixel@ pixmap x y)
              (vec4->byte-vector
               (mix-color
                §c000000ff
                (byte-vector->vec4 (average-color object aabb-average-region))
                (smoothstep-interpolate 0.0 1.0 (elt (pixel@ worley x y) 2)))))))
    (clone-into pixmap object)
    object))

(defmethod nrmse ((object1 pixmap) (object2 pixmap))
  (assert (and (= 4 (depth object1))
               (= 4 (depth object2))))
  (let* ((max-r (max (find-max (map 'list #'(lambda (a) (elt a 0)) (data object1)))
                     (find-max (map 'list #'(lambda (a) (elt a 0)) (data object2)))))
         (max-g (max (find-max (map 'list #'(lambda (a) (elt a 1)) (data object1)))
                     (find-max (map 'list #'(lambda (a) (elt a 1)) (data object2)))))
         (max-b (max (find-max (map 'list #'(lambda (a) (elt a 2)) (data object1)))
                     (find-max (map 'list #'(lambda (a) (elt a 2)) (data object2)))))
         (max-a (max (find-max (map 'list #'(lambda (a) (elt a 3)) (data object1)))
                     (find-max (map 'list #'(lambda (a) (elt a 3)) (data object2)))))
         (min-r (min (find-min (map 'list #'(lambda (a) (elt a 0)) (data object1)))
                     (find-min (map 'list #'(lambda (a) (elt a 0)) (data object2)))))
         (min-g (min (find-min (map 'list #'(lambda (a) (elt a 1)) (data object1)))
                     (find-min (map 'list #'(lambda (a) (elt a 1)) (data object2)))))
         (min-b (min (find-min (map 'list #'(lambda (a) (elt a 2)) (data object1)))
                     (find-min (map 'list #'(lambda (a) (elt a 2)) (data object2)))))
         (min-a (min (find-min (map 'list #'(lambda (a) (elt a 3)) (data object1)))
                     (find-min (map 'list #'(lambda (a) (elt a 3)) (data object2)))))
         (diff-r (if (= (- max-r min-r) 0) 1e-5 (desired (- max-r min-r))))
         (diff-g (if (= (- max-g min-g) 0) 1e-5 (desired (- max-g min-g))))
         (diff-b (if (= (- max-b min-b) 0) 1e-5 (desired (- max-b min-b))))
         (diff-a (if (= (- max-a min-a) 0) 1e-5 (desired (- max-a min-a))))
         (n (desired (* (width object1) (height object1))))
         (diffs (map 'vector #'(lambda (a b)
                                 (map 'vector #'(lambda (c d) (expt (- c d) 2))
                                      a b))
                     (data object1) (data object2))))
    (let ((res (vec4 0.0 0.0 0.0 0.0)))
      (loop for i across diffs do
           (setf (elt res 0) (desired (+ (elt res 0) (desired (elt i 0))))
                 (elt res 1) (desired (+ (elt res 1) (desired (elt i 1))))
                 (elt res 2) (desired (+ (elt res 2) (desired (elt i 2))))
                 (elt res 3) (desired (+ (elt res 3) (desired (elt i 3))))))
      (setf (elt res 0) (d- 1.0 (d/ (dsqrt (d/ (elt res 0) n)) diff-r))
            (elt res 1) (d- 1.0 (d/ (dsqrt (d/ (elt res 1) n)) diff-g))
            (elt res 2) (d- 1.0 (d/ (dsqrt (d/ (elt res 2) n)) diff-b))
            (elt res 3) (d- 1.0 (d/ (dsqrt (d/ (elt res 3) n)) diff-a)))
      res)))

(defmethod save-pixmap ((object pixmap) path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede :if-does-not-exist :create
                          :element-type +targa-stream-element-type+)
    (write-sequence (pixmap->tga-file object) stream))
  object)

(defmethod to-grayscale ((object pixmap))
  (assert (or (= 4 (the fixnum (depth object)))
              (= 3 (the fixnum (depth object)))))
  (let ((res (make-pixmap-frame (width object) (height object))))
    (setf (data res) (data
                      (map-matrix object
                                  #'(lambda (a)
                                      (let ((value (floor (d+ (d* 0.2126 (desired (elt a 0))) ; r
                                                              (d* 0.7152 (desired (elt a 1))) ; g
                                                              (d* 0.0722 (desired (elt a 2))))))) ; b
                                        (ubvec4 value value value 255))))))
    res))

(alexandria:define-constant +dhash-w+ 8 :test #'=)

(alexandria:define-constant +dhash-h+ 8 :test #'=)

(defmethod dhash ((object pixmap))
  (let ((scaled (to-grayscale object)))
    (ncopy-matrix-into-pixmap scaled (scale-matrix scaled
                                                   (d/ (d +dhash-w+) (d (width object)))
                                                   (d/ (d +dhash-h+) (d (height object)))))
    (let ((hash 0))
      (loop for i from 0 below 64 do
           (let ((pix (elt (elt (data scaled) i) 0))
                 (pix-before (elt (elt (data scaled)
                                       (mod (1- i) (* +dhash-w+ +dhash-h+))) 0)))
             (when (< pix pix-before)
               (setf (ldb (byte 1 i) hash) 1))))
      hash)))

(defun guess-correct-replace-fn (pixmap)
  (typecase (matrix-elt pixmap 0 0)
    (vec
     (blit-blend-replace-vec-fn))
    (vec4
     (blit-blend-replace-norm-fn))
    (single-float
     (blit-blend-replace-float-fn))
    (ubvec4
     (blit-blend-replace-fn))
    (ivec4
     (blit-blend-replace-ivec4-fn))))

(defmethod submatrix ((object pixmap) x y w h &optional (value 0))
  (let ((res (make-instance 'pixmap :depth (depth object) :height h :width w
                            :data (misc:make-array-frame (* w h) value t t)))
        (copy-fn (guess-correct-replace-fn object)))
    (blit object res x y 0 0 :h h :w w  :function-blend copy-fn)
    res))

(defmethod pixel@ ((object pixmap) x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap object))
  (declare (fixnum x y))
  (matrix-elt object y x))

(defmethod (setf pixel@) (color (object pixmap) x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap object))
  (declare (fixnum x y))
  (setf (matrix-elt object y x) color))

(declaim (inline %offset-bits))

(defun %offset-bits (w x y)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (fixnum w x y))
  (the fixnum (* 4 (+ (the fixnum (* w y)) x))))

(defmethod bits-pixel@ ((object pixmap) x y)
  "Color is an ubvec4"
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (fixnum x y))
  (with-accessors ((bits  bits)
                   (width width)) object
    (declare (fixnum width))
    (declare ((simple-array fixnum) bits))
    (let* ((offset (%offset-bits width x y)))
      (declare (fixnum offset))
      (ubvec4 (elt bits      offset)
              (elt bits (+ 1 offset))
              (elt bits (+ 2 offset))
              (elt bits (+ 3 offset))))))

(defmethod (setf bits-pixel@) (color (object pixmap) x y)
  "Color is an ubvec4"
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (fixnum x y))
  (declare (ubvec4 color))
  (with-accessors ((bits  bits)
                   (width width)) object
    (declare (fixnum width))
    (declare ((simple-array fixnum) bits))
    (let* ((offset (%offset-bits width x y)))
      (declare (fixnum offset))
      (setf (elt bits      offset)  (elt color 0)
            (elt bits (+ 1 offset)) (elt color 1)
            (elt bits (+ 2 offset)) (elt color 2)
            (elt bits (+ 3 offset)) (elt color 3)))
    object))

(defmethod (setf alpha-bits@) (alpha-value (object pixmap) x y)
  "value is an  unsigned byte (octect), please ensure  the slot 'bits'
of the pixmap is a symple-array of fixnum (see: cristallize-bits)"
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (fixnum x y))
  (declare ((unsigned-byte 8) alpha-value))
  (with-accessors ((bits  bits)
                   (width width)) object
    (declare (fixnum width))
    (declare ((simple-array fixnum) bits))
    (let* ((offset (%offset-bits width x y)))
      (declare (fixnum offset))
      (setf (elt bits (+ 3 offset)) alpha-value))
    object))

(defmethod draw-normalized-coord-custom-conversion ((object pixmap) x y function conversion-fn)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap object))
  (declare (desired-type x y))
  (declare (function function conversion-fn))
  (let ((x-abs (the fixnum (round (d* x (desired (the fixnum (width object)))))))
        (y-abs (the fixnum (round (d* y (desired (the fixnum (height object))))))))
    (declare (fixnum x-abs y-abs))
    (let ((pixel (funcall conversion-fn (funcall function x y))))
      (setf (matrix-elt-ubvec4 object y-abs x-abs) pixel))))

(defmethod draw-normalized-coord ((object pixmap) x y function)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap object))
  (declare (desired-type x y))
  (declare (function function))
  (let ((x-abs (the fixnum (round (d* x (desired (the fixnum (width object)))))))
        (y-abs (the fixnum (round (d* y (desired (the fixnum (height object))))))))
    (declare (fixnum x-abs y-abs))
    (let ((pixel (map 'ubvec4 #'float->byte (the vec4 (funcall function x y)))))
      (setf (matrix-elt-ubvec4 object y-abs x-abs) pixel))))

(defmethod draw-float-normalized-coord ((object pixmap) x y function)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap object))
  (declare (desired-type x y))
  (declare (function function))
  (let ((x-abs (the fixnum (round (d* x (desired (the fixnum (width object)))))))
        (y-abs (the fixnum (round (d* y (desired (the fixnum (height object))))))))
    (declare (fixnum x-abs y-abs))
    (let ((pixel (map 'vec4 #'identity (the vec4 (funcall function x y)))))
      ;;(misc:dbg "set @ ~a,~a ~a,~a" x y x-abs y-abs)
      (setf (matrix-elt-vec4 object y-abs x-abs) pixel))))

(defmethod draw-sampled-coord ((object pixmap) x y function)
  (let ((pixel (map 'vector #'float->byte (funcall function x y))))
    (setf (sample@ object x y) pixel)))

(defmethod ncopy-matrix-into-pixmap ((object pixmap) (matrix matrix)
                                    &optional (depth 4))
  (with-accessors ((data-pix data) (width-pix width)
                   (height-pix height) (depth-pix depth)) object
    (setf data-pix (data matrix)
          height-pix (height matrix)
          width-pix (width matrix)
          depth depth-pix)
    object))

(defun nscale-pixmap (pixmap scale-x scale-y)
  (ncopy-matrix-into-pixmap pixmap (scale-matrix pixmap scale-x scale-y)))

(defun nscale-pixmap-nearest (pixmap scale-x scale-y)
  (ncopy-matrix-into-pixmap pixmap (scale-matrix-nearest pixmap scale-x scale-y)))

(defun blit-blend-replace-float-fn ()
  #'(lambda (src dest x-src y-src x-dst y-dst)
      (declare (ignore dest x-dst y-dst))
      (matrix-elt src y-src x-src)))

(defun blit-blend-replace-vec-fn ()
  #'(lambda (src dest x-src y-src x-dst y-dst)
      (declare (ignore dest x-dst y-dst))
      (copy-vec (matrix-elt src y-src x-src))))

(defun blit-blend-replace-fn ()
  #'(lambda (src dest x-src y-src x-dst y-dst)
      (declare (ignore dest x-dst y-dst))
      (copy-ubvec4 (matrix-elt src y-src x-src))))

(defun %blit-blend-lerp (pix-src pix-dst)
  (let* ((pix-src   (ubvec4->vec4 pix-src))
         (pix-dst   (ubvec4->vec4 pix-dst))
         (alpha-src (elt pix-src 3))
         (alpha-dst (elt pix-dst 3))
         (alpha-out (dlerp alpha-dst alpha-src 1.0))
         (combine-fn #'(lambda (src dst)
                         (if (epsilon= alpha-out 0.0)
                             (float->byte 0.0)
                             (float->byte (d+ (d* src alpha-src)
                                              (d* (d* dst alpha-dst)
                                                  (d- 1.0 alpha-src)))))))
         (raw-color (map 'ubvec4 combine-fn pix-src pix-dst)))
    (setf (elt raw-color 3) (float->byte alpha-out))
    raw-color))

(defun blit-blend-lerp-fn ()
  #'(lambda (src dest x-src y-src x-dst y-dst)
      (let* ((pix-src   (pixel@ src x-src y-src))
             (pix-dst   (pixel@ dest x-dst y-dst)))
        (%blit-blend-lerp pix-src pix-dst))))

(defun blit-blend-replace-ivec4-fn ()
  #'(lambda (src dest x-src y-src x-dst y-dst)
      (declare (ignore dest x-dst y-dst))
      (copy-ivec4 (matrix-elt src y-src x-src))))

(defun blit-blend-replace-norm-fn ()
  #'(lambda (src dest x-src y-src x-dst y-dst)
      (declare (ignore dest x-dst y-dst))
      (copy-vec4 (matrix-elt src y-src x-src))))

(defun blit-blend-mult-fn ()
  #'(lambda (src dst x-src y-src x-dst y-dst)
      (map 'ubvec4 #'(lambda (a b) (floor (/ (* a b) 255)))
           (matrix-elt dst y-dst x-dst)
           (matrix-elt src y-src x-src))))

(defun blit-blend-sum-fn ()
  #'(lambda (src dst x-src y-src x-dst y-dst)
      (map 'ubvec4 #'(lambda (a b) (alexandria:clamp (+ a b) 0 255))
           (matrix-elt dst y-dst x-dst)
           (matrix-elt src y-src x-src))))

(defmethod blit ((src pixmap) (dest pixmap) x-src y-src x-dst y-dst
                 &key (w (width src)) (h (height src))
                   (function-blend (blit-blend-replace-fn)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap src dest))
  (declare (fixnum x-src y-src x-dst y-dst w h))
  (declare (function function-blend))
  (loop
     for y fixnum from y-src below (+ h y-src)
     for y-ct fixnum from 0 by 1 do
       (loop
          for x fixnum from x-src below (+ w x-src)
          for x-ct fixnum from 0 by 1 do
            (let ((x-dest (f+ x-dst x-ct))
                  (y-dest (f+ y-dst y-ct)))
              (with-check-matrix-borders (dest x-dest y-dest)
                (with-check-matrix-borders (src x y)
                  (setf (matrix-elt dest y-dest x-dest)
                        (funcall function-blend
                                 src
                                 dest
                                 x y x-dest y-dest))))))))

(defmethod pblit ((src pixmap) (dest pixmap) x-src y-src x-dst y-dst
                         &key (w (width src)) (h (height src))
                           (function-blend (blit-blend-replace-fn)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap src dest))
  (declare (fixnum x-src y-src x-dst y-dst w h))
  (declare (function function-blend))
  (ploop-submatrix (src x y x-src y-src (f+ x-src w) (f+ y-src h))
    (let ((x-dest (f+ x-dst (f- x x-src)))
          (y-dest (f+ y-dst (f- y y-src))))
      (with-check-matrix-borders (dest x-dest y-dest)
                (with-check-matrix-borders (src x y)
                  (setf (matrix-elt dest y-dest x-dest)
                        (funcall function-blend src dest x y x-dest y-dest)))))))

(defmethod pblit-w-repeat ((src pixmap) (dest pixmap) x-src y-src x-dst y-dst
                           &key
                             (w (width src)) (h (height src))
                             (behaivour-on-border-fn-x #'repeat-periodic-coord)
                             (behaivour-on-border-fn-y #'repeat-periodic-coord)
                             (discard-blit-fn #'(lambda (x-dst y-dst src dest)
                                                  (declare (ignore x-dst y-dst src dest))
                                                  nil))
                             (function-blend (blit-blend-replace-fn)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap src dest))
  (declare (fixnum x-src y-src x-dst y-dst w h))
  (declare (function function-blend behaivour-on-border-fn-x behaivour-on-border-fn-y
                     discard-blit-fn))
  (let ((w-dst (width dest))
        (h-dst (height dest)))
    (declare (fixnum w-dst h-dst))
    (ploop-submatrix (src x y x-src y-src (f+ x-src w) (f+ y-src h))
      (let ((x-dest (funcall behaivour-on-border-fn-x (f+ x-dst (f- x x-src)) w-dst))
            (y-dest (funcall behaivour-on-border-fn-y (f+ y-dst (f- y y-src)) h-dst)))
        (when (not (funcall discard-blit-fn x-dest y-dest src dest))
          (setf (matrix-elt dest y-dest x-dest)
                (funcall function-blend src dest x y x-dest y-dest)))))))

(defmethod pblit-unsafe ((src pixmap) (dest pixmap) x-src y-src x-dst y-dst
                         &key (w (width src)) (h (height src))
                           (function-blend (blit-blend-replace-fn)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap src dest))
  (declare (fixnum x-src y-src x-dst y-dst w h))
  (declare (function function-blend))
  (ploop-submatrix (src x y x-src y-src (f+ x-src w) (f+ y-src h))
    (let ((x-dest (f+ x-dst (f- x x-src)))
          (y-dest (f+ y-dst (f- y y-src))))
      (setf (matrix-elt dest y-dest x-dest)
            (funcall function-blend src dest x y x-dest y-dest)))))

(defmethod blit-unsafe ((src pixmap) (dest pixmap) x-src y-src x-dst y-dst
                        &key (w (width src)) (h (height src))
                          (function-blend (blit-blend-replace-fn)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (pixmap src dest))
  (declare (fixnum x-src y-src x-dst y-dst w h))
  (declare (function function-blend))
  (loop
     for y fixnum from y-src below (+ h y-src)
     for y-ct fixnum from 0 by 1 do
       (loop
          for x fixnum from x-src below (+ w x-src)
          for x-ct fixnum from 0 by 1 do
            (let ((x-dest (f+ x-dst x-ct))
                  (y-dest (f+ y-dst y-ct)))
              (setf (matrix-elt dest y-dest x-dest)
                        (funcall function-blend
                                 src
                                 dest
                                 x y x-dest y-dest))))))

(defmethod tileize  ((object pixmap)
                     &key (blend-replace-fn (guess-correct-replace-fn object)))
  (assert (= 4 (depth object)))

  ;; +----+----+
  ;; | nw | ne |
  ;; |    |    |
  ;; +----+----+
  ;; | sw | se |
  ;; |    |    |
  ;; +----+----+

  (let* ((w (width object))
         (h (height object))
         (w/2 (floor (/ w 2)))
         (h/2 (floor (/ h 2)))
         (nw (submatrix object 0 0 w/2 h/2))
         (ne (submatrix object w/2 0 w/2 h/2))
         (sw (submatrix object 0 h/2 w/2 h/2))
         (se (submatrix object w/2 h/2 w/2 h/2))
         (nw-dest (submatrix object 0 0 w/2 h/2))
         (ne-dest (submatrix object w/2 0 w/2 h/2))
         (sw-dest (submatrix object 0 h/2 w/2 h/2))
         (se-dest (submatrix object w/2 h/2 w/2 h/2)))
    (loop-matrix (nw x y)
        (blend-tileize nw
                (matrix-elt nw-dest y x)
                (matrix-elt se-dest y x)
                (matrix-elt nw y x)
                (matrix-elt se y x)
                x y))
    (loop-matrix (ne x y)
       (blend-tileize-rot ne
                      (matrix-elt ne-dest y x)
                      (matrix-elt sw-dest y x)
                      (matrix-elt ne y x)
                      (matrix-elt sw y x)
                      x y))
    (blit nw-dest object 0 0 0 0 :function-blend blend-replace-fn)
    (blit ne-dest object 0 0 w/2 0 :function-blend blend-replace-fn)
    (blit sw-dest object 0 0 0 h/2 :function-blend blend-replace-fn)
    (blit se-dest object 0 0 w/2 h/2 :function-blend blend-replace-fn)
    object))

(defun blend-tileize-rot (pixmap dest-1 dest-2 src-1 src-2 x y)
  (let* ((w (desired (width pixmap)))
         (h (desired (height pixmap)))
         (norm-x (d/ (desired x)
                     (desired w)))
         (norm-y (d/ (d- (dabs (d- (desired y) h)) 1.0)
                     (d- h 1.0))))
    (tileize-interpolate norm-x norm-y dest-1 dest-2 src-1 src-2)))

(defun blend-tileize (pixmap dest-1 dest-2 src-1 src-2 x y)
  (let* ((w (desired (width pixmap)))
         (h (desired (height pixmap)))
         (norm-x (d/ (d- (dabs (d- (desired x) w)) 1.0)
                     (d- w 1.0)))
         (norm-y (d/ (d- (dabs (d- (desired y) h)) 1.0)
                     (d- h 1.0))))
    (tileize-interpolate norm-x norm-y dest-1 dest-2 src-1 src-2)))

(defun tileize-interpolate (norm-x norm-y dest-1 dest-2 src-1 src-2)
  (let* ((weight (cond
                   ((and (d< norm-x 1e-5)
                         (d> norm-y 0.99999))
                    1.0)
                   ((and (d> norm-x 0.99999)
                         (d< norm-y 1e-5))
                    1.0)
                   (t
                    (d- 1.0 (d/ (d* norm-x norm-y)
                                (d+ (d* norm-x norm-y)
                                    (d* (d- 1.0 norm-x)
                                        (d- 1.0 norm-y))))))))
         (floatp (vec4p src-1))
         (alpha (dlerp weight (desired (elt src-2 3)) (desired (elt src-1 3))))
         (act-alpha (if floatp alpha (truncate alpha))))
    (setf (elt dest-1 3) act-alpha
          (elt dest-2 3) act-alpha)
    (when (/= (elt dest-1 3) 0)
      (loop for i from 0 below 3 do
           (let* ((value (d/ (dlerp weight
                                   (desired (* (elt src-2 i) (elt src-2 3)))
                                   (desired (* (elt src-1 i) (elt src-1 3))))
                             alpha))
                  (act-value (if floatp value (truncate value))))
             (setf (elt dest-1 i) act-value
                   (elt dest-2 i) act-value))))))

(defmethod average-color ((object pixmap) aabb)
  (let* ((x-min (elt aabb 0))
         (y-min (elt aabb 1))
         (x-max (elt aabb 2))
         (y-max (elt aabb 3))
         (w (- x-max x-min))
         (h (- y-max y-min))
         (num-el (* w h))
         (element (elt (data object) 0))
         (sum-fn (etypecase element
                   (ubvec4 #'ivec4+)
                   (ivec4 #'ivec4+)
                   (vec  #'vec+)
                   (vec4  #'vec4+)))
         (div-fn (etypecase element
                   (ubvec4 #'ivec4/)
                   (ivec4 #'ivec4/)
                   (vec  #'vec/)
                   (vec4  #'vec4/)))
         (average (etypecase element
                    (ubvec4 (make-fresh-ivec4))
                    (ivec4  (make-fresh-ivec4))
                    (vec    (alloc-vec))
                    (vec4   (make-fresh-vec4)))))
    (loop-submatrix (object x y x-min y-min x-max y-max)
       (setf average (funcall sum-fn average (matrix-elt object y x))))
    (setf average (funcall div-fn average num-el))
    (etypecase element
      (ubvec4 (ubvec4 (coerce (elt average 0) 'ubvec4-type)
                      (coerce (elt average 1) 'ubvec4-type)
                      (coerce (elt average 2) 'ubvec4-type)
                      (coerce (elt average 3) 'ubvec4-type)))
      (ivec4 average)
      (vec average)
      (vec4  average))))

(defmethod ppremultiply-alpha  ((object pixmap))
  (assert (truecolorp object))
  (ploop-matrix (object x y)
    (let* ((src   (pixel@ object x y))
           (alpha (elt src 3)))
      (setf (pixel@ object x y)
            (map 'ubvec4 #'(lambda (a) (floor (/ (* a alpha) 255))) src))
      (setf (elt (pixel@ object x y) 3) alpha))))

(defmethod punremultiply-alpha ((object pixmap))
  (assert (truecolorp object))
  (ploop-matrix (object x y)
    (let* ((src   (pixel@ object x y))
           (alpha (elt src 3)))
      (setf (pixel@ object x y)
            (map 'ubvec4 #'(lambda (a)
                             (if (/= alpha 0)
                                 (floor (* a (/ 255 alpha)))
                                 a))
                 src))
      (setf (elt (pixel@ object x y) 3) alpha))))

(defmacro with-premultiplied-alpha ((pixmap) &body body)
  `(progn
     (ppremultiply-alpha ,pixmap)
     ,@body
     (punremultiply-alpha ,pixmap)))

(defclass pixmap-file (pixmap)
  ((magic-number
    :initform ""
    :accessor magic-number
    :initarg :magic-number)
   (errors
    :initform nil
    :accessor errors
    :initarg :errors)))

(defun slurp-pixmap (type file)
  (let ((px (make-instance type)))
    (load px file)
    px))

(defun test-gradient ()
  (let ((px (gradient-image (slurp-pixmap 'pgm (fs:file-in-package "gradient-test.pgm"))
                            :round-fn #'(lambda (a) (alexandria:clamp (ceiling (abs a))
                                                                      0
                                                                      255)))))
    (with-open-file (stream
                     (fs:file-in-package "gradient-out.pgm")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (format stream "~a" (matrix->pgm px "gradient" 255)))))

(defgeneric load (object file))

(defgeneric load-from-stream (object stream))

(alexandria:define-constant +ppm-ignored-line+ "#.*\\n|^\\n" :test 'string=)

(alexandria:define-constant +ppm-magic-number+ "P3" :test 'string=)

(alexandria:define-constant +pgm-magic-number+ "P2" :test 'string=)

(alexandria:define-constant +pbm-magic-number+ "P1" :test 'string=)

(alexandria:define-constant +ppm-parser-token-num+ "[0-9]+" :test 'string=)

(defclass ppm-parsed-file (parser:parsed-file) ())

(defmethod initialize-instance :after ((object ppm-parsed-file) &key &allow-other-keys)
  (with-slots (parser:comment-line) object
    (setf parser:comment-line +ppm-ignored-line+)))

(parser:define-tokenizer-simple ppm-parsed-file
                                 "P3" ;; +ppm-magic-number+
                                 "P2" ;;+pgm-magic-number+
                                 "[0-9]+") ;;+ppm-parser-token-num+)

(defmethod parser:parse-comment-line ((object ppm-parsed-file))
  (parser:with-no-errors
    (multiple-value-bind (line length start)
        (buffered-input-file:get-line parser:*file*)
      (declare (ignore length))
      (if (parser:is-comment-line-p object line)
          (parser:parse-comment-line object)
          (progn
            (buffered-input-file:seek parser:*file* start)
            nil)))))

(parser:define-parser-skeleton pixmap ppm ppm-parsed-file
  (parser:*blank-space* '(#\space #\newline)))

(parser:defnocfun parse-magic-number ()
    (parser:with-valid-stream
      (let ((token (parser:next-token-simple parser:*file*)))
        (if (not (or
                  (string= token +ppm-magic-number+)
                  (string= token +pgm-magic-number+)))
            (progn
              (setf parser:*has-errors* t)
              (push (format nil "Error: magic number ~a unknown" token)
                    parser:*parsing-errors*)
              nil)
            token))))

(parser:defnocfun parse-anything ()
  (parser:with-no-errors
    (let ((token (parser:next-token-simple parser:*file*)))
      (or (parse-integer token :junk-allowed t) 0))))

(defun parse-anything* ()
  (parser:with-no-errors
    (let ((token (parser:next-token-simple parser:*file* :no-more-token-error nil)))
      (if token
          (or (parse-integer token :junk-allowed t) 0)
          nil))))

(alexandria:define-constant +ppm-max-row-length+ 70 :test '=)

(defclass ppm (pixmap-file)
  ((max-color
    :initform 0
    :accessor max-color
    :initarg :max-color)))

(defmethod initialize-instance :after ((object ppm) &key &allow-other-keys)
  (with-slots (depth magic-number) object
    (setf depth 3
          magic-number +ppm-magic-number+)))

(defmethod print-object ((object ppm) stream)
  (with-accessors ((magic-number magic-number)
                   (width width) (height height) (max-color max-color)
                   (bits bits) (depth depth)) object
    (format stream "~a~%~a~%~a~%~a~%" magic-number width height max-color)
    (let ((count-columns 0)
          (max-color-length (length (format nil "~a " (max-color object)))))
      (loop for y from 0 below (height object) do
           (loop for x from 0 below (width object) do
                (let ((pixel (matrix-elt object y x)))
                  (dotimes (var depth)
                    (let ((the-string (format nil "~a" (elt pixel var))))
                      (incf count-columns (1+ (length the-string)))
                      (if (>= (+ count-columns max-color-length)
                              +ppm-max-row-length+)
                          (progn
                            (setf count-columns 0)
                            (format stream "~a~%" the-string))
                          (format stream "~a " the-string))))))))))

(defmethod marshal:class-persistant-slots ((object ppm))
  (append '(max-color) (call-next-method)))

(defmethod matrix->pixmap ((object matrix))
  (make-instance 'pixmap
                 :depth (if (and (data object)
                                 (> (length (data object)) 0))
                            (if (numberp (elt (data object) 0))
                                1
                                (length (elt (data object) 0)))
                            4)
                 :height (height object) :width  (width object)
                 :data (data object)))

(defmethod pixmap->ppm ((object ppm))
  (with-accessors ((magic-number magic-number)
                   (width width) (height height) (max-color max-color)
                   (data data) (depth depth)) object
    (make-ppm* max-color depth data width height :magic magic-number)))

(defmethod load ((object ppm) (file string))
  (with-ppm-file (:filename file)
    (let ((parsed-magic-number (parse-magic-number)))
      (unless (string= parsed-magic-number (magic-number object))
        (setf parser:*has-errors* t)
        (push (format nil "Error: magic number ~a found ~a expected" parsed-magic-number
                      (magic-number object))
              parser:*parsing-errors*)))
    (setf (width object) (parse-anything)
          (height object) (parse-anything)
          (max-color object) (parse-anything))
    (parser:with-no-errors
      (parser:with-valid-stream
        (do ((pixel (parse-anything*)
                    (parse-anything*)))
            ((not pixel))
            (vector-push-extend pixel (bits object)))))
    (parser:with-no-errors
      (let ((right-length (* (depth object) (width object) (height object))))
        (cond
          ((< (length (bits object)) right-length)
           (setf (bits object) (concatenate 'vector
                                            (bits object)
                                            (misc:make-array-frame
                                             (- right-length (length (bits object))) 0))))
          ((> (length (bits object)) right-length)
           (setf (bits object) (subseq (bits object) 0 right-length))))))
    (setf (errors object) parser:*parsing-errors*))
  (sync-bits-to-data object)
  (h-mirror-matrix object)
  (sync-data-to-bits object))

(defun make-ppm* (max-color depth data w h &key (magic +ppm-magic-number+))
   "width and height specified as element, not pixel, size"
   (make-instance 'ppm
                 :max-color max-color :magic-number magic
                 :depth depth :data data :width w :height h))


(defun make-ppm (max-color depth data w h &key (magic +ppm-magic-number+))
  (make-instance 'ppm
                 :max-color max-color :magic-number magic
                 :depth depth :data data :width (* w depth) :height h))

(defclass pgm (ppm)
  ())

(defmethod initialize-instance :after ((object pgm) &key &allow-other-keys)
  (with-slots (depth magic-number) object
    (setf depth 1
          magic-number +pgm-magic-number+)))

(defmethod sync-bits-to-data ((object pgm))
  (with-accessors ((data data) (bits bits) (width width) (height height)) object
    (let ((data-size (* width height)))
      (if (/= (length data) (length bits))
          (progn
            (setf data (misc:make-array-frame data-size 0 '(unsigned-byte 8) t))
            (loop for i from 0 below (length bits) by 1 do
                 (setf (elt data i) (elt bits i))))
          (loop for i from 0 below (length bits) by 1 do
               (setf (elt data i) (elt bits i)))))
    (h-mirror-matrix object)))

(defmethod sync-data-to-bits ((object pgm))
  (with-accessors ((data data) (bits bits) (width width) (height height)) object
    (unwind-protect
         (progn
           (h-mirror-matrix object)
           (let ((bits-size (* width height)))
             (if (/= (length data) (length bits))
                 (progn
                   (setf bits (misc:make-array-frame bits-size 0 '(unsigned-byte 8) t))
                   (loop for i from 0 below (length data) by 1 do
                        (setf (elt bits i) (elt data i))))
                 (loop for i from 0 below (length data) by 1 do
                      (setf (elt bits i) (elt data i))))))
      (h-mirror-matrix object)
      object)))

(defmethod print-object ((object pgm) stream)
  (with-accessors ((magic-number magic-number)
                   (width width) (height height) (max-color max-color)
                   (data data) (depth depth)) object
    (format stream "~a~%~a~%~a~%~a~%" magic-number width height max-color)
    (when (and (data object)
               (> (length (data object)) 0))
      (let ((rows (mapcar #'(lambda (l) (misc:split-into-sublist l +ppm-max-row-length+))
                          (misc:split-into-sublist (data-as-list object) (width object)))))
        (format stream "~{~{~{~a ~}~%~}~}" rows)))))

(defmethod pixmap->ppm ((object pgm))
  (with-accessors ((magic-number magic-number)
                   (width width) (height height) (max-color max-color)
                   (data data) (depth depth)) object
    (let ((new-data (make-array 1 :adjustable t :fill-pointer 0)))
      (loop for i across data do
           (dotimes (ct 3)
             (vector-push-extend i new-data)))
      (make-ppm max-color 3 new-data
                (width object)
                (height object)
                :magic +ppm-magic-number+))))

(defgeneric rowbool->rowpixel (row))

(defgeneric rowgray->rowpgm (row))

(defgeneric rowcolor->rowppm (row))

(defmethod rowbool->rowpixel ((row vector))
  (map 'vector
       #'(lambda (b) (if b 1 0)) row))

(defmethod rowbool->rowpixel ((row list))
  (mapcar #'(lambda (b) (if b 1 0)) row))

(defmethod rowgray->rowpgm ((row vector))
  (map 'vector
       #'(lambda (b) (if b (if (listp b) (first b) b) 0)) row))

(defmethod rowgray->rowpgm ((row list))
  (mapcar #'(lambda (b) (if b (if (listp b) (first b) b) 0)) row))

(defmethod rowcolor->rowppm ((row vector))
  (reduce #'append
          (map 'vector
               #'(lambda (b)
                   (if b
                       (if (listp b)
                           b
                           (list b))
                       '(0 0 0)))
               row)))

(defmethod rowcolor->rowppm ((row list))
  (reduce #'append
          (mapcar #'(lambda (b)
                      (if b
                          (if (listp b)
                              b
                      (list b))
                          '(0 0 0)))
                  row)))

(defun matrix->pbm (matrix name &optional (magic "P1") (row-max-length +ppm-max-row-length+))
  (let ((data (data-as-list matrix)))
    (with-output-to-string (stream)
      (format stream "~a~%" magic)
      (format stream "#~a~%" name)
      (format stream "~a ~a~%" (width matrix) (height matrix))
      (let ((rows (mapcar #'(lambda (l) (misc:split-into-sublist l row-max-length))
                          (misc:split-into-sublist (rowbool->rowpixel data) (width matrix)))))
        (format stream "~{~{~{~a ~}~%~}~}" rows)))))

(defun matrix->pgm (matrix name max-gray-level &optional (magic "P2")
                    (row-max-length +ppm-max-row-length+))
  (let ((data (data-as-list matrix)))
    (with-output-to-string (stream)
      (format stream "~a~%" magic)
      (format stream "#~a~%" name)
      (format stream "~a ~a~%" (width matrix) (height matrix))
      (format stream "~a~%" (if (floatp max-gray-level)
                                (floor max-gray-level)
                                max-gray-level))
      (let ((rows (mapcar #'(lambda (l) (misc:split-into-sublist l row-max-length))
                          (misc:split-into-sublist (rowgray->rowpgm data)
                                                   (width matrix)))))
        (format stream "~{~{~{~a ~}~%~}~}" rows)))))

(defun matrix->pgm-less-mem (matrix name max-gray-level &optional (magic "P2"))
  (let ((data (data-as-list matrix)))
    (with-output-to-string (stream)
      (format stream "~a~%" magic)
      (format stream "#~a~%" name)
      (format stream "~a ~a~%" (width matrix) (height matrix))
      (format stream "~a~%" max-gray-level)
      (let ((rows (rowgray->rowpgm data))
            (i 0))
        (loop
           for px in rows
           do
             (let* ((breakp (if (or (>= (+ (length (format nil "~a " px))
                                           i)
                                        +ppm-max-row-length+))
                                t nil))
                    (the-string (format nil "~a~[~%~:; ~]" px
                                        (if breakp 0 1))))
               (if breakp
                   (setf i 5)
                   (incf i (length the-string)))
               (format stream "~a" the-string)))))))

(defun matrix->ppm (matrix &optional (max-color-level 255))
  (let ((ppm (make-ppm max-color-level 3
                       (data matrix)
                       (width matrix)
                       (height matrix))))
    (with-output-to-string (stream)
      (format stream "~a" ppm))))

(defun matrix->ppm* (matrix &optional (max-color-level 255))
  "width and height specified as element, not pixel, size"
  (let ((ppm (make-ppm* max-color-level 3
                       (data matrix)
                       (width matrix)
                       (height matrix))))
    (with-output-to-string (stream)
      (format stream "~a" ppm))))

(alexandria:define-constant +targa-img-rgba-rle+ 10 :test 'equalp)

(alexandria:define-constant +targa-img-rgba+ 2 :test 'equalp)

(alexandria:define-constant +targa-img-header-size+ 18 :test 'equalp)

(alexandria:define-constant +targa-img-scanline-topleft+ 2 :test 'equalp)

(alexandria:define-constant +targa-img-scanline-bottomleft+ 0 :test 'equalp)

(alexandria:define-constant +targa-img-signature+ "TRUEVISION-XFILE" :test 'equalp)

(misc:define-offset-size pixmap targa-img (id-len 0 1) (type 2 1) (spec 8 10)
                         (id +targa-img-header-size+) (colormap-spec 3 5))

(defclass tga (pixmap-file) ())

(defmethod initialize-instance :after ((object tga) &key (path nil) &allow-other-keys)
  (when path
    (load object path)))

(defgeneric rearrange-scanline-by-pixmap-origin (object origin))

(misc:define-parse-header-chunk (image-id-len +targa-img-id-len-offset+
                                              +targa-img-id-len-size+ tga nil))

(misc:define-parse-header-chunk (image-type +targa-img-type-offset+
                                            +targa-img-type-size+ tga nil))

(misc:define-parse-header-chunk (image-specs +targa-img-spec-offset+
                                            +targa-img-spec-size+ tga nil))

(misc:define-parse-header-chunk (colormap-specs +targa-img-colormap-spec-offset+
                                            +targa-img-colormap-spec-size+ tga nil))

(defun load-rearrange-raw-packet (bgra data)
  (loop for i from 0 below (length bgra) by 4 do
       (let ((b (elt bgra i))
             (g (elt bgra (+ i 1)))
             (r (elt bgra (+ i 2)))
             (a (elt bgra (+ i 3))))
         (vector-push-extend r data)
         (vector-push-extend g data)
         (vector-push-extend b data)
         (vector-push-extend a data))))

(defmethod load-from-stream ((object tga) (stream stream))
  (let ((type (first (parse-image-type object stream))))
    (if (or (= type +targa-img-rgba+)
            (= type +targa-img-rgba-rle+))
        (let* ((id-len (first (parse-image-id-len object stream)))
               (colormap-specs (parse-colormap-specs object stream))
               (colormap-len (misc:byte->int (subseq colormap-specs 2 4)))
               (img-specs (parse-image-specs object stream))
               ;;(x-origin  (misc:byte->int (subseq img-specs 0 2)))
               ;;(y-origin  (misc:byte->int (subseq img-specs 2 4)))
               (width     (misc:byte->int (subseq img-specs 4 6)))
               (height    (misc:byte->int (subseq img-specs 6 8)))
               (depth     (/ (misc:byte->int (subseq img-specs 8 9)) 8))
               (img-descr (first (subseq img-specs 9 10)))
               (alpha-size (boole boole-and img-descr #x0e))
               (scanline-origin (ash (boole boole-and img-descr #x30) -4))
               (scanline-offset (+ +targa-img-header-size+ id-len colormap-len))
               (scanline-size (* width height depth))
               (bgra (misc:make-array-frame (* width depth))))
          (if (/= 8 alpha-size)
              (push (format nil "Alpha bitsize should be 8 instead of ~x" alpha-size)
                    (errors object))
              (progn
                (setf (width object)  width
                      (height object) height
                      (depth object) depth)
                (file-position stream scanline-offset)
                (if (= type +targa-img-rgba+)
                    (progn
                      (loop for i from 0 below height do
                           (read-sequence bgra stream)
                           (load-rearrange-raw-packet bgra (bits object))))
                    (loop for i from 0 below (/ scanline-size depth) do
                         (let* ((packet-head (read-byte stream))
                                (packet-type (boole boole-and packet-head #x80))
                                (packet-count (boole boole-and packet-head #x7f)))
                           (if (= packet-type #x80) ;; rle packet
                               (let ((b (read-byte stream))
                                     (g (read-byte stream))
                                     (r (read-byte stream))
                                     (a (read-byte stream)))
                                 (loop for px from 0 to packet-count do
                                      (vector-push-extend r (bits object))
                                      (vector-push-extend g (bits object))
                                      (vector-push-extend b (bits object))
                                      (vector-push-extend a (bits object))))
                               (loop for px from 0 to packet-count do ;; raw packet
                                    (let ((b (read-byte stream))
                                          (g (read-byte stream))
                                          (r (read-byte stream))
                                          (a (read-byte stream)))
                                      (vector-push-extend r (bits object))
                                      (vector-push-extend g (bits object))
                                      (vector-push-extend b (bits object))
                                      (vector-push-extend a (bits object)))))
                           (incf i packet-count))))
                (rearrange-scanline-by-pixmap-origin object scanline-origin)
                (sync-bits-to-data object)
                object)))
        (push "Image type not supported: only rgba and compressed rgba allowed."
              (errors object)))))

(defmethod load ((object tga) (file string))
  (with-open-file (stream file :element-type +targa-stream-element-type+
                          :if-does-not-exist :error)
    (load-from-stream object stream)))

(defmethod load-from-vector ((object tga) (data vector))
  (flexi-streams:with-input-from-sequence (stream data)
    (load-from-stream object stream)))

(defmethod rearrange-scanline-by-pixmap-origin ((object tga) origin)
  (with-accessors ((height height) (bits bits) (width width)
                   (depth depth)) object
    (macrolet ((swap-bits (a b)
                 `(rotatef (elt bits ,a) (elt bits ,b))))
      (cond
        ((= origin +targa-img-scanline-topleft+)
         (let ((scanline-length (* depth width)))
           (loop for y from 0 below (floor (/ height 2)) do
                (loop for x from 0 below scanline-length do
                     (let ((sup (+ x (* y scanline-length)))
                           (inf (+ (* (- (1- height) y) scanline-length) x)))
                       (swap-bits sup inf))))))
        ((= origin +targa-img-scanline-bottomleft+)
         t)
        (t
         (push "scanline origin not-supported, top-left or bottom-left only are allowed"
               (errors object))
         nil)))))

(defun rle-encode-line (line depth &optional (res '()))
  (if (and line (> (length line) 0))
      (let* ((pix (elt line 0))
             (rearranged-pixel (vector (elt pix 2)   ; b
                                       (elt pix 1)   ; r
                                       (elt pix 0)   ; g
                                       2))
             (max-packet-count (do* ((ct 1 (1+ ct)))
                                    ((not (and (< ct (length line))
                                               (< ct 127)
                                               (equalp (elt line ct) pix)))
                                     ct))))

        (when (= depth 4)
          (setf (elt rearranged-pixel 3) (elt pix 3)))
        (rle-encode-line (subseq line max-packet-count)
                         depth
                         (push (list max-packet-count rearranged-pixel) res)))
      (reverse res)))

(defun targa-rle-scanline (line depth)
  (let ((encoded (rle-encode-line line depth)))
    (alexandria:flatten
     (mapcar #'(lambda (a)
                 (list (boole boole-ior #x80 (boole boole-and (1- (first a)) #x7f))
                       (second a)))
             encoded))))

(defmethod pixmap->tga-file ((object pixmap))
  (assert (= 4 (depth object)))
  (labels ((write-bytes (vector bytes)
             (loop for i in bytes do (vector-push-extend i vector))
             vector))
  (let ((results (misc:make-array-frame 0 +targa-stream-element-type+)))
    (vector-push-extend #x0 results) ; image id has zero length
    (vector-push-extend #x0 results) ; no colormap
    (vector-push-extend #x0a results) ; we use only truecolor image also we want them compressed
    (write-bytes results '(#x00 #x00 #x00 #x00 #x00)) ; color map specification
                                                      ; all zero for true color image
    (setf results (write-bytes results '(#x00 #x00))) ; x-origin
    (setf results (write-bytes results '(#x00 #x00))) ; y-origin
    (setf results (write-bytes results (misc:int16->bytes (width object))))
    (setf results (write-bytes results (misc:int16->bytes (height object))))
    (vector-push-extend #x20 results) ; 32 bpp plus alpha channel
    (vector-push-extend #x28 results) ; image origin top-left alpha channel, important
    (loop for i from 0 below (length (data object)) by (width object) do
         (let ((line (targa-rle-scanline (subseq (data object) i (+ i (width object)))
                                         (depth object))))
           (loop for p in line do
                (etypecase p
                    (number (vector-push-extend p results))
                    (vector (loop for j across p do (vector-push-extend j results)))
                    (list (loop for j in p do (vector-push-extend j results)))))))
    ;; footer
    (loop repeat 8 do
         (vector-push-extend #x0 results))
    (loop for i across +targa-img-signature+ do
         (vector-push-extend (char-code i) results))
    (vector-push-extend (char-code #\.) results)
    (vector-push-extend #x0 results)
    results)))

(defmethod pixmap->ppm ((object tga))
  (with-accessors ((width width) (height height)
                   (bits bits) (depth depth)) object
    (let ((ppm (make-instance 'ppm :max-color 255 :depth 3
                              :width width :height height
                              :bits (let ((new-bits (misc:make-array-frame 0)))
                                      (loop for i from 1 to (length bits) when (/= 0 (mod i 4)) do
                                           (vector-push-extend (elt bits (1- i)) new-bits))
                                      new-bits))))
    (sync-bits-to-data ppm)
    ppm)))

(defmethod matrix-line ((object pixmap) start end color &key (antialiasp t) (width 1))
  (assert (truecolorp object))
  (let ((points (2d-utils:segment start end :antialiasp antialiasp :width width)))
    (loop for p in points do
         (let* ((pos       (elt p 0))
                (intensity (elt p 1))
                (x         (elt pos 0))
                (y         (elt pos 1)))
           (with-check-matrix-borders (object x y)
             (let ((pix-src (ubvec4 (elt color 0)
                                    (elt color 1)
                                    (elt color 2)
                                    (float->byte (d intensity))))
                   (pix-dst (pixel@ object x y)))
               (setf (pixel@ object x y) (%blit-blend-lerp pix-src pix-dst)))))))
  object)
