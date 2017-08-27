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

(in-package :full-screen-masks)

(define-constant +grid-size+ 50 :test  #'=)

(define-constant +burn-texture+ "burn-mask.tga" :test #' string=)

(alexandria:define-constant +burn-gradient+ (color-utils:make-gradient
                                             (color-utils:make-gradient-color 0.0  §c000000ff)
                                             (color-utils:make-gradient-color 0.02 §cffffffff)
                                             (color-utils:make-gradient-color 0.05 §cffff00ff)
                                             (color-utils:make-gradient-color 0.66 §cff0000ff)
                                             (color-utils:make-gradient-color 1.0  §c00000000))
  :test #'color-utils:gradient-equals)

(defstruct cell
  (x         0)
  (y         0)
  (incr      .02)
  (intensity 0.0)
  (color     :white)
  (updatep   nil))

(defstruct grid
  (queue (make-queue (make-cell)))
  (cells))

(defun make-standard-grid (size)
  (let ((cells (matrix:make-matrix size size)))
    (matrix:nmap-matrix-xy cells
                           #'(lambda (c r el)
                               (declare (ignore el))
                               (setf (matrix:matrix-elt cells r c)
                                     (make-cell :x r :y c))))
    (make-grid :cells cells)))

(defun make-cell-updatable (cell)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (setf (cell-updatep cell) t))

(defun make-cell-visited* (cell)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (setf (cell-color cell) :black))

(defun make-cell-visited (grid x y)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (make-cell-visited* (matrix:matrix-elt (grid-cells grid) y x)))

(defun cell-visited-p* (cell)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (eq (cell-color cell) :black))

(defun cell-visited-p (grid x y)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (cell-visited-p* (matrix:matrix-elt (grid-cells grid) y x)))

(defun animate-cell (cell)
  (declare (cell cell))
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (incf (the single-float (cell-intensity cell))
        (the single-float (cell-incr cell))))

(defun format-color (color gradient) ;; 0.0 ->1.0
  (let ((color-float (color-utils:pick-color gradient color)))
    (map 'ubvec4 #'(lambda (x) (truncate (* 255 x))) color-float)))

(defun put-pixel (pixmap cell x y color)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (fixnum x y))
  (declare (ubvec4 color))
  (with-accessors ((bits pixmap:bits) (width pixmap:width)) pixmap
    (declare (fixnum width))
    (declare ((simple-array fixnum) bits))
    (let* ((offset (the fixnum (* 4 (+ (the fixnum (* width y)) x))))
           (alpha  (if (d> (cell-intensity cell) 0.01)
                       0.0
                       1.0)))
      (declare (fixnum offset))
      (setf (elt bits      offset)  (truncate (lerp alpha (elt color 0) (elt bits      offset)))
            (elt bits (+ 1 offset)) (truncate (lerp alpha (elt color 1) (elt bits (+ 1 offset))))
            (elt bits (+ 2 offset)) (truncate (lerp alpha (elt color 2) (elt bits (+ 2 offset))))
            (elt bits (+ 3 offset)) (elt color 3))
     pixmap)))

(defun draw-pixel (pixmap cell x y color)
  (put-pixel pixmap cell x y (format-color color +burn-gradient+)))

(defun gen-neighbour (x y)
  (vector (vector (1+ x) y)
          (vector x (1- y))
          (vector (1- x) y)
          (vector x (1+ y))))

(defun update-cell (grid current)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (let* ((neigh (gen-neighbour (cell-x current)
                               (cell-y current))))
    (declare ((simple-vector 4) neigh))
    (make-cell-updatable current)
    (loop for cell-coordinates across neigh do
         (let* ((x (elt (the (simple-vector 2) cell-coordinates) 0))
                (y (elt cell-coordinates 1))
                (cells (grid-cells grid))
                (queue (grid-queue grid))
                (cell (matrix:with-check-matrix-borders (cells x y)
                        (matrix:matrix-elt cells y x))))
       (when (and cell
                  (not (cell-visited-p* cell))
                  (not (cell-updatep cell)))
         (make-cell-visited* cell)
         (qpush queue cell))))))

(defun update (grid)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (let ((cells (grid-cells grid))
        (queue (grid-queue grid)))
    (when (not (qemptyp queue))
      (let* ((currents (loop repeat 20 collect (qpop queue))))
        (loop
           for current in currents
           when current do
             (update-cell grid current))))
    (loop
       for cell across (the (simple-vector *) (matrix:data cells))
       when (and (< (the single-float (cell-intensity cell)) 1.0)
                 (cell-updatep cell))
       do
       (animate-cell cell))))

(defun draw (renderer grid)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (loop
     for cell across (the simple-vector (matrix:data (grid-cells grid)))
     do
       (if (< (the single-float (cell-intensity cell)) 1.0)
           (draw-pixel renderer
                       cell
                       (cell-x         cell)
                       (cell-y         cell)
                       (cell-intensity cell))
           (draw-pixel renderer
                       cell
                       (cell-x         cell)
                       (cell-y         cell)
                       1.0))))

(defclass burn-mask (widget)
  ((grid
    :initform (make-standard-grid +grid-size+)
    :initarg  :grid
    :accessor grid)))

(defmethod initialize-instance :after ((object burn-mask) &key &allow-other-keys)
  (with-accessors ((width width) (height height)
                   (grid grid)) object
    (qpush (grid-queue grid)
           (matrix:matrix-elt (grid-cells grid)
                              (floor (/ +grid-size+ 2))
                              (floor (/ +grid-size+ 2))))
    (add-quad-for-widget object)
    (transform-vertices object (sb-cga:scale* (width object) (height object) 1.0))
    (prepare-for-rendering object)))

(defmethod calculate :before ((object burn-mask) dt)
  (with-accessors ((grid           grid)
                   (texture-object texture-object)) object
    (update grid)
    (draw texture-object grid)
    (update-for-rendering texture-object)))

(defmethod render ((object burn-mask) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)
                   (material-params material-params)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (use-program compiled-shaders :gui)
          (gl:active-texture :texture0)
          (texture:bind-texture texture-object)
          (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
          (uniform-matrix compiled-shaders :modelview-matrix 4
                          (vector (sb-cga:matrix* camera-vw-matrix
                                                  (elt view-matrix 0)
                                                  (elt model-matrix 0)))
                          nil)
          (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
          (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
          (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))

(defmethod removeable-from-world-p ((object burn-mask))
  (every #'(lambda (a) (d> (cell-intensity a) 0.90))
         (matrix:data (grid-cells (grid object)))))

(defun make-burn-mask ()
  (let ((instance (make-instance 'burn-mask
                                 :width    (d *window-w*)
                                 :height   (d *window-h*)))
        (texture  (texture:get-texture (res:get-resource-file +burn-texture+
                                                              +default-gui-resource+
                                                              :if-does-not-exists :error))))
    (texture:prepare-for-rendering texture)
    (pixmap:cristallize-bits texture)
    (setf (texture-object instance) texture)
    instance))
