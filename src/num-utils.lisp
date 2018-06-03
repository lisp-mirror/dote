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

(in-package :num-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun primes-list (primes &optional (pos 0))
    (declare (optimize (debug 0) (safety 0) (speed 3)))
    (declare ((simple-array fixnum) primes))
    (declare (fixnum pos))
    (let ((start (position-if #'(lambda (a)
                                  (declare (fixnum a))
                                  (> a 0))
                              primes :start (1+ pos))))
      (if start
          (progn
            (do ((pos-m (+ start (elt primes start)) (+ pos-m (elt primes start))))
                ((>= pos-m (length primes)))
              (declare (fixnum pos-m))
              (when (> (elt primes pos-m) 0)
                (setf (elt primes pos-m) -1)))
            (primes-list primes start))
          (subseq (remove-if #'(lambda (a)
                                 (declare (fixnum a))
                                 (< a 0))
                             primes)
                  1))))

  (defun make-primes-array (count)
    (let ((arr (misc:make-array-frame count 0 'fixnum t)))
      (loop
         for i across arr
         for a from 0 by 1 do
           (setf (elt arr a) (1+ a)))
      (primes-list arr)))

  (defparameter *array-primes* (make-primes-array 100000)))

(defun manhattam-distance (from to)
  (+ (abs (- (elt to 0) (elt from 0)))
     (abs (- (elt to 1) (elt from 1)))))

(defgeneric rad->deg (rad))

(defmethod rad->deg ((rad §d))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (d* (d* rad 360.0) 0.15915494309189535))

(defgeneric deg->rad (deg))

(defmethod deg->rad ((deg number))
  (/ (* deg +2pi+) 360))

(defmethod deg->rad ((deg §d))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  ;; 0.0027777778 ~= 1 /360
  (d* (d* deg +2pi+) 0.0027777778))

(defun spherical->cartesian (phi theta)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (desired-type phi theta))
  ;;    ^
  ;; z  |
  ;;    | theta (elevation)
  ;;    +-----> y
  ;;   /  phi azimut (ccw)
  ;;  / x
  ;;
  ;; z = r cos (theta)
  ;; x = r sin(theta) cos (phi)
  ;; y = r sin(theta) sin(phi)
  (sb-cga:vec (d* (dsin theta) (dcos phi))
              (d* (dsin theta) (dsin phi))
              (dcos theta)))

(defun cartesian->spherical (v)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (sb-cga:vec v))
  (let ((r (dsqrt (d+ (dexpt (elt v 0) 2.0)
                      (dexpt (elt v 1) 2.0)
                      (dexpt (elt v 2) 2.0)))))
    (values r
            (dacos (d/ (elt v 2) r))
            (datan (d/ (elt v 1) (elt v 0))))))

(defun find-min-max (function the-list)
  (restart-case
      (reduce #'(lambda (a b) (if (funcall function a b) a b)) the-list)
    (use-value (e) e)))

(defun find-min (the-list)
  (find-min-max #'< the-list))

(defun find-max (the-list)
  (find-min-max #'> the-list))

(defgeneric round-all (object &key rounding-function))

(defmethod round-all ((object list) &key (rounding-function #'round))
  (mapcar #'(lambda (n) (funcall rounding-function n)) object))

(defmethod round-all ((object number) &key (rounding-function #'round))
  (funcall rounding-function object))

(defmethod round-all ((object vector) &key (rounding-function #'round))
  (map (type-of object) #'(lambda (n) (funcall rounding-function n)) object))

(defun fract (n)
  (multiple-value-bind (int frac)
      (truncate n)
    (declare (ignore int))
    frac))

(defgeneric sign (n))

(defmethod sign ((n fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (f< n 0)
      -1
      1))

(defmethod sign ((n §d))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (d< n 0.0)
      -1.0
      1.0))

(alexandria:define-constant +fnv-prime-32+ 16777619 :test #'=)

(alexandria:define-constant +fnv-offset-basis-32+ 2166136261 :test #'=)

(defun fnv-hash-32 (octects)
  (let ((hash +fnv-offset-basis-32+))
    (loop for i across octects do
         (setf hash (boole boole-xor hash i))
         (setf hash (ldb (byte 32 0) (* hash +fnv-prime-32+))))
    hash))

(defun string-fnv-hash-32 (s)
  (fnv-hash-32 (map 'vector #'char-code (coerce s 'list))))

(alexandria:define-constant +fnv-prime-256+
    374144419156711147060143317175368453031918731002211 :test #'=)

(alexandria:define-constant +fnv-offset-basis-256+
    100029257958052580907070968620625704837092796014241193945225284501741471925557
  :test #'=)

(defun fnv-hash-256 (octects)
  (let ((hash +fnv-offset-basis-256+))
    (loop for i across octects do
         (setf hash (boole boole-xor hash i))
         (setf hash (ldb (byte 256 0) (* hash +fnv-prime-256+))))
    hash))

(defun string-fnv-hash-256 (s)
  (fnv-hash-256 (map 'vector #'char-code (coerce s 'list))))

(alexandria:define-constant +lcg-modulo-pow+ 64 :test #'=)

(alexandria:define-constant +lcg-good-bit-starts+ 32 :test #'=)

(alexandria:define-constant +lcg-good-bit-size+ 32 :test #'=)

(alexandria:define-constant +lcg-modulo+ 18446744073709551616 :test #'=)

(alexandria:define-constant +lcg-max+ 4294967295 :test #'=)

(alexandria:define-constant +lcg-a+ 3935559000370003845 :test #'=)

(alexandria:define-constant +lcg-c+ 2691343689449507681 :test #'=)

(defparameter *lcg-seed* 0)

(defun lcg-set-seed (&optional (seed (get-universal-time)))
  (setf *lcg-seed* seed))

(defun lcg-next ()
  (setf *lcg-seed*
        (ldb (byte +lcg-modulo-pow+ 0)
             (+ (* +lcg-a+ *lcg-seed*) +lcg-c+)))
  (ldb (byte +lcg-good-bit-size+ +lcg-good-bit-starts+) *lcg-seed*))

(defun lcg-next01 ()
    (/ (lcg-next) +lcg-max+))

(defgeneric lcg-next-upto (max))

(defmethod lcg-next-upto ((max float))
  (multiple-value-bind (integer-part remainder)
      (truncate max)
    (+ (* (lcg-next01) integer-part) (* (lcg-next01) remainder))))

(defmethod lcg-next-upto ((max integer))
  (mod (lcg-next) max))

(defmethod lcg-next-upto ((max ratio))
  (lcg-next-upto (float max)))

(defun lcg-next-in-range (from to)
  (+ (lcg-next-upto (- to from)) from))

(defun lcg-next-in-range* (range)
  "range is a cons cell (from . to)"
  (lcg-next-in-range (car range) (cdr range)))

(defmacro with-lcg-seed ((&optional (seed `(get-universal-time))) &body body)
  `(let ((*lcg-seed* ,seed))
     ,@body))

(defun get-random-float-sign ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (= (the integer (lcg-next-upto 2)) 0) 1.0 -1.0))

(defun random-gaussian-distribution (sigma)
  (labels ((ran ()
             (lcg-next-in-range -1.0 1.0))
           (displ (x1 x2)
             (+ (expt x1 2) (expt x2 2)))
           (pr (w)
             (sqrt (/ (* -2 (log w)) w))))
    (do* ((x1 (ran) (ran))
          (x2 (ran) (ran))
          (w1 (displ x1 x2) (displ x1 x2))
          (w (pr w1) (pr w1)))
         ((and (< w1 1.0) (> w1 0)) (list (* x1 w sigma) (* x2 w sigma))))))

(defun gaussian-probability (sigma mean)
  (+ (* (expt -1 (num:lcg-next-upto 2)) (first (random-gaussian-distribution sigma)))
     mean))

(defun random-select-by-frequency* (freq &key (sort nil) (normalize nil))
  (let ((actual-freq (copy-list freq)))
    (when normalize
      (let ((sum (reduce #'+ actual-freq :initial-value 0)))
        (setf actual-freq (mapcar #'(lambda (a) (/ a sum)) actual-freq))))
    (when sort
      (setf actual-freq (sort actual-freq #'<)))
    (let* ((dice-roll (lcg-next01))
           (pos (do* ((freqs actual-freq (rest freqs))
                      (sum-freq (first freqs) (+ sum-freq (first freqs)))
                      (pos 0 (1+ pos)))
                     ((>= sum-freq dice-roll) pos))))
      pos)))

(defun random-select-by-frequency (freq &key
                                           (key #'identity)
                                           (sort t)
                                           (sort-predicate #'<)
                                           (normalize t))
  (let ((sum (if normalize
                 (reduce #'+ freq :key key :initial-value 0)
                 1.0)))
    (when sort
      (setf freq (sort freq sort-predicate :key key)))
    (let* ((dice-roll (lcg-next01))
           (pos (do* ((pos 0 (1+ pos))
                      (sum-freq (/ (funcall key (elt freq pos)) sum)
                                (+ sum-freq (/ (funcall key (elt freq pos)) sum))))
                     ((>= sum-freq dice-roll) pos))))
      (elt freq pos))))

(defun tokuda-sequence (n)
  (do ((k 1 (1+ k))
       (h 1.0 (d+ (d* 2.25 h) 1.0))
       (res '()))
      ((not (< h n)) res)
    (push (ceiling h) res)))

(defgeneric shellsort (sequence predicate &key key)
  (:documentation "Note: makes a new sequence"))

(defmethod shellsort ((sequence list) predicate &key (key #'identity))
  (call-next-method (copy-list sequence)
                    predicate
                    :key key))

(defmethod shellsort ((sequence vector) predicate &key (key #'identity))
  (call-next-method (alexandria:copy-array sequence)
                    predicate
                    :key key))

(defmethod shellsort (sequence predicate &key (key #'identity))
  (loop for gap in (tokuda-sequence (length sequence)) do
       (loop for i from gap below (length sequence) by 1 do
            (let ((tmp (elt sequence i)))
              (do ((j   i (- j gap)))
                  ((not (and (>= j gap)
                              (not (funcall predicate
                                            (funcall key (elt sequence (- j gap)))
                                            (funcall key tmp)))))
                   (setf (elt sequence j) tmp))
                (let ((swp (elt sequence (- j gap))))
                  (setf (elt sequence j) swp))))))
  sequence)

(defun multisort (bag fns)
  (shellsort bag
             #'(lambda (a b)
                 (let ((partial (loop named outer for fn in fns do
                                     (cond
                                       ((< (funcall fn a b) 0)
                                        (return-from outer t))
                                       ((> (funcall fn a b) 0)
                                        (return-from outer nil))))))
                   partial))))

(defun multisort* (bag &rest fns)
  (multisort bag fns))

(defmacro gen-multisort-test (fn-< fn-> fn-access)
  (alexandria:with-gensyms (a b access-a access-b)
    `(lambda (,a ,b)
       (let ((,access-a (,fn-access ,a))
             (,access-b (,fn-access ,b)))
         (cond
           ((,fn-< ,access-a ,access-b)
            -1)
           ((,fn-> ,access-a ,access-b)
            1)
           (t 0))))))

(defun nearest-greatest-primes (num primes)
  (find-if #'(lambda (a) (< num a)) primes))

(defclass bag-picker ()
  ((bag
    :initarg :bag
    :initform #()
    :accessor bag)
   (prime
    :initarg :prime
    :initform 0
    :accessor prime)
   (next
    :initarg :next
    :initform 0
    :accessor next)
   (coeff1
    :initarg :coeff1
    :initform 0
    :accessor coeff1)
   (coeff2
    :initarg :coeff1
    :initform 0
    :accessor coeff2)
   (coeff3
    :initarg :coeff1
    :initform 0
    :accessor coeff3)))

(defmethod initialize-instance :after ((object bag-picker) &key &allow-other-keys)
  (with-accessors ((bag bag) (prime prime) (coeff1 coeff1)
                   (coeff2 coeff2) (coeff3 coeff3)) object
    (setf prime (nearest-greatest-primes (length bag) *array-primes*)
          coeff1 (1+ (lcg-next))
          coeff2 (1+ (lcg-next))
          coeff3 (1+ (lcg-next)))))

(defgeneric random-pick-from-set (object))

(defmethod random-pick-from-set ((object bag-picker))
  (declare (optimize (debug 0)))
  (with-accessors ((bag bag) (prime prime) (next next)
                   (coeff1 coeff1) (coeff2 coeff2) (coeff3 coeff3)) object
    (let ((skip (+ (* coeff1 (expt (length bag) 2))
                   (* coeff2 (length bag))
                   coeff3)))
      (let ((candidate (mod (+ next skip) prime)))
        (setf next candidate)
        (if (< candidate (length bag))
            next
            (random-pick-from-set object))))))

(defun rejection-sampling (box function howmany &optional (counts 0))
  (if (< counts howmany)
      (concatenate 'list
                   (let ((x (lcg-next-in-range (first box) (second box)))
                         (y (lcg-next-in-range (third box) (fourth box))))
                     (if (<= y (funcall function x))
                         (concatenate 'list
                                      (list x)
                                      (rejection-sampling box function howmany (1+ counts)))
                         (rejection-sampling box function howmany counts))))
      nil))

(defun bivariate-sampling (sigma-x sigma-y howmany)
  (loop repeat howmany collect
       (list (first (random-gaussian-distribution sigma-x))
             (first (random-gaussian-distribution sigma-y)))))

(defparameter *default-epsilon* 1e-7)

(defmacro with-epsilon ((epsilon) &body body)
  `(let ((*default-epsilon* ,epsilon))
     ,@body))

(defun add-epsilon-rel (v &optional (epsilon *default-epsilon*))
  (d+ v (d* epsilon v)))

(defun epsilon<= (a b &optional (epsilon *default-epsilon*))
  (or (d<= a b)
      (epsilon= a b epsilon)))

(defun epsilon>= (a b &optional (epsilon *default-epsilon*))
  (or (d>= a b)
      (epsilon= a b epsilon)))

(defun epsilon= (a b &optional (epsilon *default-epsilon*))
  (and (<= (- b epsilon) a (+ b epsilon))))

(defun normalize-value-in-range (v min max &key (clamp nil))
  "if  min < v <  max return a value remapped in range [0, 1]
Return a simple-float"
  (let ((res (/ (- v min) (- max min))))
    (if clamp
        (d (alexandria:clamp res 0.0 1.0))
        (d res))))

(defun smoothstep (x)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (d* (dexpt x (d 2)) (d- (d 3) (d* (d 2) x))))

(defun sinstep (x)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (dsin (d* x +pi/2+)))

(defun cosstep (x)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (d/ (d- 1.0 (dcos (d* x +pi+))) 2.0))

(defun sin-interpolate (a b step)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (d+ (d* a (dsin (d- step 1.0))) (d* b (dsin step))))

(defun elastic-step (x &optional (p 0.3) (s 0.0 s-provided-p))
  (let ((s (if s-provided-p
               s
               (* (dasin 1.0)
                  (d* p (d/ 1.0 (d* 2.0 +pi+)))))))
    (d- (d* (dexpt 2.0 (d* 10.0 (d- x 1.0)))
            (dsin (d/ (d* (d- (d- x 1.0) s)
                          (d* 2.0 +pi+))
                      p))))))

(defmacro gen-step (name interpolator-fun)
  (let ((fun-name         (alexandria:format-symbol t "~:@(~a-interpolate~)"     name))
        (fun-name-reverse (alexandria:format-symbol t "~:@(~a-interpolate-rev~)" name)))
    `(progn
       (defgeneric ,fun-name (a b weight))
       (defmethod  ,fun-name ((a number) (b number) (weight number))
         (declare (optimize (debug 3) (safety 0) (speed 3)))
         ,(alexandria:with-gensyms (w)
           `(let ((,w (alexandria:clamp (d/ (d- weight a) (d- b a)) (d 0)
                                        (d 1))))
              (,interpolator-fun ,w))))
       (defmethod ,fun-name ((a vector) (b vector) (par number))
         (map 'vector #'(lambda (v1 v2) (,fun-name v1 v2 par)) a b))
       (defmethod ,fun-name ((a list) (b list) (par number))
         (map 'list #'(lambda (v1 v2) (,fun-name v1 v2 par)) a b))
       ;; reverse
       (defgeneric ,fun-name-reverse (a b weight))
       (defmethod  ,fun-name-reverse ((a number) (b number) (weight number))
         (declare (optimize (debug 3) (safety 0) (speed 3)))
         ,(alexandria:with-gensyms (w delta)
            `(let* ((,delta (d- b a))
                    (,w     (alexandria:clamp (d- 1.0
                                                  (d/ (d- weight a)
                                                      ,delta))
                                              (d 0)
                                              (d 1))))
               (,interpolator-fun ,w))))
       (defmethod ,fun-name-reverse ((a vector) (b vector) (par number))
         (map 'vector #'(lambda (v1 v2) (,fun-name-reverse v1 v2 par)) a b))
       (defmethod ,fun-name-reverse ((a list) (b list) (par number))
         (map 'list #'(lambda (v1 v2) (,fun-name-reverse v1 v2 par)) a b)))))

(gen-step smoothstep smoothstep)

(gen-step sinstep sinstep)

(gen-step cosstep cosstep)

(gen-step elastic-step elastic-step)

(defgeneric bounce-step-interpolate (a b weight))

(defmethod bounce-step-interpolate ((a number) (b number) (weight number))
  (declare (optimize (debug 3) (safety 0) (speed 3)))
  (dabs (elastic-step-interpolate a b weight)))

(defmethod bounce-step-interpolate ((a vector) (b vector) (par number))
  (map 'vector #'(lambda (v1 v2) (bounce-step-interpolate v1 v2 par)) a b))

(defmethod bounce-step-interpolate ((a list) (b list) (par number))
  (map 'list #'(lambda (v1 v2) (bounce-step-interpolate v1 v2 par)) a b))

(defgeneric bounce-step-interpolate-rev (a b weight))

(defmethod bounce-step-interpolate-rev ((a number) (b number) (weight number))
  (declare (optimize (debug 3) (safety 0) (speed 3)))
    (dabs (elastic-step-interpolate-rev a b weight)))

(defmethod bounce-step-interpolate-rev ((a vector) (b vector) (par number))
  (map 'vector #'(lambda (v1 v2) (bounce-step-interpolate-rev v1 v2 par))
       a b))

(defmethod bounce-step-interpolate-rev ((a list) (b list) (par number))
  (map 'list #'(lambda (v1 v2) (bounce-step-interpolate-rev v1 v2 par))
       a b))

(defun clamp-0->max-less-one (val max)
  (alexandria:clamp val (d 0.0) (d (1- max))))

(defun clamp-0->max (val max)
  (alexandria:clamp val (d 0.0) (d max)))

(defun range-0to1 (val)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (single-float val))
  (d/ (d+ val 1.0) 2.0))

(defun bidimensional-gaussian (x y sigma-x sigma-y &optional (mu-x 0) (mu-y 0) (correlation 0))
  (declare (optimize (debug 3) (safety 0) (speed 3)))
  (let ((sigma-xy (d* sigma-x sigma-y))
        (correlation-sq (dexpt correlation (d 2))))
    (d* (d/ (d 1)
            (d* (d 2) constants:+pi+ sigma-xy (sqrt (d- (d 1) correlation-sq))))
        (exp (d* (d- (d/ (d 1) (d* (d 2) (d- (d 1)
                                                         correlation-sq))))
                 (d+ (d/ (expt (d- x mu-x) (d 2)) (expt sigma-x (d 2)))
                     (d/ (expt (d- y mu-y) (d 2)) (expt sigma-y (d 2)))
                     (d- (d/ (d* (d 2) correlation (d- x mu-x) (d- y mu-y)) sigma-xy))))))))

(defun gaussian-probability-distribution (x sigma &optional (mu 0))
  (declare (optimize (debug 3) (safety 0) (speed 3)))
  (d/ (d* (d/ (d 1)
              (d* sigma
                  (sqrt (d* (d 2) constants:+pi+))))
          (dexp (d- (d/ (dexpt (d- x mu)
                               (d 2))
                        (d* (d 2)
                            (dexpt sigma (d 2)))))))
      (d/ 1.0 sigma)))

(defun gaussian (x amplitude sigma mean)
  (d* amplitude (dexp (d- (d/ (expt (d- x mean) 2.0) (d* 2.0 (dexpt sigma 2.0)))))))

(defun enzyme-kinetics (max k x)
  (declare (optimize (debug 3) (safety 0) (speed 3)))
  (d/ (d* max x) (d+ k x)))

(defun gaussian-function (amplitude sigma mean)
  #'(lambda (x)
      (gaussian x amplitude sigma mean)))

(defun damped-impulse (x freq)
  (declare (optimize (debug 3) (safety 0) (speed 3)))
  (d/ (d/ (dsin (d* freq x)) x) freq))

(defun zig-zag-line (x m freq amp)
  (declare (optimize (debug 3) (safety 0) (speed 3)))
  (d* (d+ (d* m x) (dsin (d* freq x))) amp))

(defun almost-identity (x m n)
  (declare (optimize (debug 3) (safety 0) (speed 3)))
  (if (d> x m)
      x
      (let ((a (d- (d* 2.0 n) m))
            (b (d- (d* 2.0 m) (d* 3.0 n)))
            (c (d/ x m)))
        (d+ (d* (d+ (d* a c) b)
                (dexpt c 2.0))
            n))))

(defun impulse (x k)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (let ((h (d* k x)))
    (d* h (dexp (d- 1.0 h)))))

(defun exp-step (x k pow)
  (declare (optimize (debug 0) (safety 0) (speed 1)))
  (declare (desired-type x k pow))
  (dexp (d* (d- k) (dexpt x pow))))

(defun cubic-pulse (x center width)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (desired-type x center width))
  (let ((act-x (dabs (d- x center))))
    (if (d> act-x width)
        0.0
        (d- 1.0 (d* (dexpt act-x 2.0) (d- 3.0 (d* 2.0 act-x)))))))

(defun parabola (x k)
  (declare (optimize (debug 0) (safety 0) (speed 1)))
  (declare (desired-type x k))
  (dexpt (d* 4.0 x (d- 1.0 x)) k))

(defun ellipse3d (a b step)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (desired-type a b step))
  (misc:list->simple-array
   (loop for phi from 0.0 below +2pi+ by step collect
        (let ((x (d* a (dsin phi)))
              (y (d* b (dcos phi))))
          (sb-cga:vec x y 0.0)))
   +zero-vec+
  'sb-cga:vec))

(declaim (ftype (function ((simple-array single-float) &key (:pivot-position fixnum)))
                partition))

(defun partition (sequence &key (pivot-position (lcg-next-upto (length sequence))))
  (declare (optimize (debug 0) (safety 0) (space 0) (speed 3)))
  (declare (fixnum pivot-position))
  (declare ((simple-array single-float) sequence))
  (macrolet ((swap (a b)
               (alexandria:with-gensyms (tmp)
                 `(let ((,tmp ,b))
                    (setf ,b ,a
                          ,a ,tmp)))))
    (let ((1-less-sequence-length (1- (length sequence))))
      (declare (fixnum 1-less-sequence-length))
      (swap (aref sequence 1-less-sequence-length)
            (aref sequence pivot-position))
      (setf pivot-position 1-less-sequence-length)
      (let ((last-el (do* ((pivot (aref sequence pivot-position))
                           (i -1)
                           (j 0 (1+ j)))
                          ((not (< j 1-less-sequence-length)) (1+ i))
                       (declare (fixnum i j))
                       (when (d<= (aref sequence j) pivot)
                         (incf i)
                         (swap (aref sequence i)
                               (aref sequence j))))))
        (swap (aref sequence last-el)
              (aref sequence pivot-position))
        (values sequence last-el)))))

(declaim (ftype (function ((simple-array single-float) fixnum &optional fixnum fixnum))
                k-stats))

(defun k-stats (seq order &optional (from 0) (to (length seq)))
  (declare (optimize (debug 0) (safety 0) (space 0) (speed 3)))
  (declare (fixnum order from to))
  (declare ((simple-array single-float) seq))
  (if (= from to)
      (aref seq from)
      (multiple-value-bind (partition pivot-position)
          (partition seq)
        (declare (fixnum pivot-position))
        (declare ((simple-array single-float) partition))
        (let ((k (1+ pivot-position)))
          (cond
            ((= order pivot-position)
             (aref partition pivot-position))
            ((< order k)
             (k-stats (subseq partition from pivot-position) order))
            (t
             (k-stats (subseq partition (1+ pivot-position)) (- order k))))))))

(defun median (s)
  (d (k-stats s (floor (/ (length s) 2)))))

(defun unordered-pairs (set1 set2 &optional (test #'eq))
  (if (or (null set1)
          (null set2))
      nil
      (append (list (cons (first set1) (first set2)))
              (unordered-pairs (list (first set1)) (rest set2) test)
              (unordered-pairs (rest set1)          set2 test))))

(defun ordered-pairs (set1 set2 &optional (test #'eq))
  (let* ((unordered (unordered-pairs set1 set2 test))
         (res '()))
    (loop for i in unordered do
         (pushnew i res
                  :test #'(lambda (a b) (and (funcall test (car a) (cdr b))
                                             (funcall test (cdr a) (car b))))))
    res))

(defun ordered-pairs-no-twins (set1 set2 &optional (test #'eq))
  (remove-if #'(lambda (a) (funcall test (car a) (cdr a)))
             (ordered-pairs set1 set2 test)))
