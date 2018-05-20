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

(in-package :easter-egg)

(define-constant +complex-radius-space+ 2.0 :test #'=)

(define-constant +complex-min+         -2.0 :test #'=)

(define-constant +complex-max+          2.0 :test #'=)

(alexandria:define-constant +gradient+ (make-gradient (make-gradient-color 0.0  §c000000ff)
                                                      (make-gradient-color 0.33 §c3871daff)
                                                      (make-gradient-color 0.66 §c90e5c3ff)
                                                      (make-gradient-color 1.00 §cffffffff))
  :test #'gradient-equals)

(defun pix->mand (a)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (dlerp a +complex-max+ +complex-min+))

(defun mand->pix (a)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (normalize-value-in-range a +complex-min+ +complex-max+))

(defun in-cardiod-p (x y)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (declare (desired-type x y))
  (let ((p (sqrt (+ (expt (- x 1/4) 2)
                    (expt y 2)))))
    (< x (+ p (* -2
                 (expt p 2))
            1/4))))

(defun in-period-2-bulb-p (x y)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (declare (desired-type x y))
  (let ((lhs (+ (expt (+ x 1) 2)
                (expt y       2))))
    (< lhs 1/16)))


(definline terminatedp (v ct max-iter)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (declare (fixnum ct max-iter))
  (declare ((complex desired-type) v))
  (let* ((length (+ (expt (realpart v) 2) (expt (imagpart v) 2))))
    (or (> ct max-iter)
        (d> length (d* 2.0 +complex-radius-space+)))))

(definline mand-equation (z_i-1 c)
  (the (complex desired-type) (+ (expt z_i-1 2) c)))

(defun escapedp (c max-iter)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (declare (fixnum max-iter))
  (declare ((complex desired-type) c))
  (let* ((x (realpart c))
         (y (imagpart c)))
    (cond
      ((or (in-period-2-bulb-p x y)
           (in-cardiod-p x y))
       nil)
      (t
       (do* ((ct   0                 (1+ ct))
             (z    (complex 0.0 0.0) (mand-equation z c))
             (traj (list c)          (push z traj)))
            ((terminatedp z ct max-iter) (values ct (<= ct max-iter) traj))
         (declare (fixnum ct))
         (declare (list traj))
         (declare ((complex desired-type) z c)))))))

(defun mandelbrot-partial (pixmap x-upper y-upper x-bottom y-bottom
                           &key
                             (gradient      +gradient+)
                             (bg            (ubvec4 0 0 0 255))
                             (max-iteration 10000))
  (let* ((size (width pixmap)))
    (loop for x from x-upper below x-bottom by (/ 1 size) do
         ;#+debug-mode (misc:dbg "~a" x)
         (loop for y from y-upper below y-bottom by (/ 1 size) do
              (let ((c     (complex x y))
                    (pix-x (mand->pix x))
                    (pix-y (mand->pix y)))
                (multiple-value-bind (count escapedp)
                    (escapedp c max-iteration)
                  (if escapedp
                      (let* ((color (pick-color gradient (/ (rem count 256) 255.0))))
                        (parallel-setf (sample@ pixmap pix-x pix-y) (vec4->ubvec4 color)))
                      (parallel-setf (sample@ pixmap pix-x pix-y) bg)))))))
  t)

(defun make-mandelbrot (file-out size
                        &key
                          (slice-num     (os-utils:cpu-number))
                          (gradient      +gradient+)
                          (bg            (vec4 0.0 0.0 0.0 1.0))
                          (max-iteration 10000))
  (with-workers (workers-number :number slice-num)
    (with-file-matrix (pixmap size size)
      (let* ((lparallel:*kernel* (lparallel:make-kernel workers-number))
             (*parallel-setf-queue* (lparallel.queue:make-queue :initial-contents '(t)
                                                                :fixed-capacity 1))
             (actual-bg   (vec4->ubvec4 bg))
             (offset      (/ 1 slice-num))
             (offset-mand (d (* offset (- +complex-max+ +complex-min+))))
             (channel     (lparallel:make-channel)))
        (loop for i from 0 below slice-num do
             (let* ((x-upper   +complex-min+)
                    (x-bottom  +complex-max+)
                    (y-upper   (d+ +complex-min+ (d (* offset-mand i))))
                    (y-bottom  (d+ y-upper offset-mand)))
               #+debug-mode (misc:dbg "~a ~a ~a ~a" x-upper y-upper x-bottom y-bottom)
               (lparallel:submit-task channel
                                      #'(lambda ()
                                          (mandelbrot-partial pixmap
                                                              x-upper
                                                              y-upper
                                                              x-bottom
                                                              y-bottom
                                                              :gradient      gradient
                                                              :bg            actual-bg
                                                              :max-iteration max-iteration)))))
        (loop repeat slice-num do
             (lparallel:receive-result channel))
        (uiop:copy-file (file-path pixmap) file-out)
        t))))

(defun place-trajectory (pixmap trajectory)
  (with-mutex
      (loop for i in trajectory do
           (let* ((x          (realpart i))
                  (y          (imagpart i))
                  (pix-x      (mand->pix x))
                  (pix-y      (mand->pix y))
                  (old        (sample@ pixmap
                                       pix-x pix-y
                                       :interpolation nil
                                       :clamp         t))
                  (old-as-int (byte->int old))
                  (new-bytes  (int32->bytes (1+ old-as-int))))
             (setf (sample@ pixmap pix-x pix-y) new-bytes))))
  pixmap)

(defun buddhabrot-partial (pixmap x-upper y-upper x-bottom y-bottom
                           &key (max-iteration 1000) (sampling-count 100000))
  (loop for i from 0 below  sampling-count do
       #+debug-mode
       (when (= (rem i (/ sampling-count 1000)) 0)
         (misc:dbg "y ~a count ~a/~a ~a%" y-upper i sampling-count
                   (* 100.0 (/ i sampling-count))))
       (let ((c (complex (lcg-next-in-range x-upper x-bottom)
                         (lcg-next-in-range y-upper y-bottom))))
         (multiple-value-bind (count escapedp trajectory)
             (escapedp c max-iteration)
           (declare (ignore count))
           (when escapedp
             (place-trajectory pixmap trajectory)))))
  t)

(defgeneric find-traj-max-min (trajectories))

(defmethod find-traj-max-min ((trajectories file-matrix))
  (let ((max -1)
        (min  100000000000000))
    (loop for i from 0 below (* (width trajectories) (height trajectories)) do
         (let* ((value (ubvec4 0 0 0 0)))
           (read-sequence value (stream-handle trajectories))
           (let  ((value-as-int (byte->int value)))
             (when (> value-as-int max)
               (setf max value-as-int))
             (when (and (< value-as-int min)
                        (> value-as-int 0))
               (setf min value-as-int)))))
    (values max min)))

(defmethod find-traj-max-min ((trajectories pixmap))
  (let ((max -1)
        (min  100000000000000))
    (loop for value across (data trajectories) do
         (let  ((value-as-int (byte->int value)))
           (when (> value-as-int max)
             (setf max value-as-int))
           (when (and (< value-as-int min)
                      (> value-as-int 0))
             (setf min value-as-int))))
    (values max min)))

(defun make-buddhabrot-big (file-out file-in size
                            &key
                              (slice-num       (os-utils:cpu-number))
                              (gradient        +gradient+)
                              (scale-raw-value 1e3)
                              (sampling-count  60000000)
                              (max-iteration   20000))
  "file-in can be reused for incremental rendering"
  (with-workers (workers-number :number slice-num)
    (with-file-matrix (trajectories size size :file file-in)
      (let* ((lparallel:*kernel* (lparallel:make-kernel workers-number))
             (*parallel-setf-queue* (lparallel.queue:make-queue :initial-contents '(t)
                                                                :fixed-capacity 1))

             (offset      (/ 1 slice-num))
             (offset-mand (d (* offset (- +complex-max+ +complex-min+))))
             (channel     (lparallel:make-channel)))
        (loop for i from 0 below slice-num do
             (let* ((x-upper   +complex-min+)
                    (x-bottom  +complex-max+)
                    (y-upper   (d+ +complex-min+ (d (* offset-mand i))))
                    (y-bottom  (d+ y-upper offset-mand)))
               #+debug-mode (misc:dbg "~a ~a ~a ~a" x-upper y-upper x-bottom y-bottom)
               (lparallel:submit-task channel
                                      #'(lambda ()
                                          (buddhabrot-partial trajectories
                                                              x-upper
                                                              y-upper
                                                              x-bottom
                                                              y-bottom
                                                              :sampling-count sampling-count
                                                              :max-iteration  max-iteration)))))
        (loop repeat slice-num do
             (lparallel:receive-result channel)
             #+debug-mode (misc:dbg "thread terminated"))
        #+debug-mode (misc:dbg "finding max...")
        (multiple-value-bind (max min)
            (find-traj-max-min trajectories)
          #-debug-mode (declare (ignore min))
          #+debug-mode (misc:dbg "found ~a ~a." 1.0 (d (/ min max)))
          (with-file-matrix (pixmap size size)
            #+debug-mode (misc:dbg "converting.")
            (file-position (stream-handle trajectories) 0)
            (file-position (stream-handle pixmap)       0)
            (loop for i from 0 below (* (width trajectories) (height trajectories)) do
                 (let* ((value (ubvec4 0 0 0 0)))
                   (read-sequence value (stream-handle trajectories))
                   (let* ((value-as-int (byte->int value))
                          (norm-value   (d (* scale-raw-value
                                              (/ value-as-int max))))
                          (color        (vec4->ubvec4 (pick-color gradient norm-value))))
                     (write-sequence color (stream-handle pixmap)))))
            #+debug-mode (misc:dbg "copying.")
            (fs:copy-a-file (file-path pixmap) file-out :overwrite t))))))
  t)

(defun make-buddhabrot (&key
                          (size            (* 2 pixmap:+default-size-pixmap-library+))
                          (slice-num       (os-utils:cpu-number))
                          (gradient        +gradient+)
                          (scale-raw-value 20)
                          (sampling-count  3000000)
                          (max-iteration   20000))
  (let ((file-out (fs:temporary-filename)))
    (with-workers (workers-number :number slice-num)
      (let* ((trajectories          (make-pixmap-frame size size))
             (output                (make-pixmap-frame size size))
             (lparallel:*kernel*    (lparallel:make-kernel workers-number))
             (*workers-number*      slice-num)
             (*parallel-setf-queue* (lparallel.queue:make-queue :initial-contents '(t)
                                                                :fixed-capacity 1))

             (offset      (/ 1 slice-num))
             (offset-mand (d (* offset (- +complex-max+ +complex-min+))))
             (channel     (lparallel:make-channel)))
        (loop for i from 0 below slice-num do
             (let* ((x-upper   +complex-min+)
                    (x-bottom  +complex-max+)
                    (y-upper   (d+ +complex-min+ (d (* offset-mand i))))
                    (y-bottom  (d+ y-upper offset-mand)))
               #+debug-mode (misc:dbg "~a ~a ~a ~a" x-upper y-upper x-bottom y-bottom)
               (lparallel:submit-task channel
                                      #'(lambda ()
                                          (buddhabrot-partial trajectories
                                                              x-upper
                                                              y-upper
                                                              x-bottom
                                                              y-bottom
                                                              :sampling-count sampling-count
                                                              :max-iteration  max-iteration)))))
        (loop repeat slice-num do
             (lparallel:receive-result channel)
             #+debug-mode (misc:dbg "thread terminated"))
        #+debug-mode (misc:dbg "finding max...")
        (multiple-value-bind (max min)
            (find-traj-max-min trajectories)
          #-debug-mode (declare (ignore min))
          #+debug-mode (misc:dbg "found ~a ~a." 1.0 (d (/ min max)))
          #+debug-mode (misc:dbg "converting.")
          (loop for i from 0 below (* (width trajectories) (height trajectories)) do
               (with-accessors ((data-in data)) trajectories
                 (with-accessors ((data-out data)) output
                   (let* ((value (elt data-in i))
                          (value-as-int (byte->int value))
                          (norm-value   (d (* scale-raw-value
                                              (/ (sqrt value-as-int)
                                                 (sqrt max)))))
                          (color        (vec4->ubvec4 (pick-color gradient norm-value))))
                     (setf (elt data-out i) color)))))
          #+debug-mode (misc:dbg "saving. ~a" (width output))
          (setf (data output) (data (rotate-matrix output -90.0)))
          (save-pixmap output file-out))))
      file-out))
