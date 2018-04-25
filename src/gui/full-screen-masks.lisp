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

(in-package :full-screen-masks)

(define-constant +grid-size+      50              :test  #'=)

(define-constant +burn-texture+   "burn-mask.tga" :test #'string=)

(define-constant +burning-speed+  20              :test #'=)

(define-constant +burning-delay+ 120              :test #'=)

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

(defun update (grid &key (speed 200) (pick-random-p nil))
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (fixnum speed))
  (let* ((cells (grid-cells grid))
         (queue (grid-queue grid))
         (qsize (qsize queue)))
    (declare (fixnum qsize))
    (when (not (qemptyp queue))
      (let* ((currents (loop repeat (min qsize speed) collect
                            (qpop queue :random pick-random-p))))
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
    :accessor grid)
   (text
    :initform (make-instance 'flush-center-label
                             :x               (d (* 1/10 *window-w*))
                             :y               (d (- (* 3/4 *window-h*)
                                                    (/ (/ *window-h* 2) 2)))
                             :width           (d (* 8/10 *window-w*))
                             :height          (d (/ *window-h* 2))
                             :label           nil
                             :label-font-size (d (/ *window-h* 12)))
    :initarg  :text
    :accessor text)
   (burning-speed
    :initform +burning-speed+
    :initarg  :burning-speed
    :accessor burning-speed)
   (delay
     :initform +burning-delay+
     :initarg  :delay
     :accessor delay)
   (pick-random-p
     :initform t
     :initarg  :pick-random-p
     :reader   pick-random-p
     :writer   pick-random)))

(defmethod initialize-instance :after ((object burn-mask)
                                       &key
                                         (title "test")
                                         (color §cffff00ff)
                                         &allow-other-keys)
  (with-accessors ((width width) (height height)
                   (grid grid)
                   (text text)) object
    (qpush (grid-queue grid)
           (matrix:matrix-elt (grid-cells grid)
                              (floor (/ +grid-size+ 2))
                              (floor (/ +grid-size+ 2))))
    (add-quad-for-widget object)
    (transform-vertices object (sb-cga:scale* (width object) (height object) 1.0))
    (prepare-for-rendering object)
    ;;
    (when title
      (setf (label-font-color text) color)
      (setf (label text) title)
      (setf (y text) (d (- (/ *window-h* 2)
                           (/ (height text) 2)))))
    (mtree:add-child object text)))

(defmethod calculate :before ((object burn-mask) dt)
  (with-accessors ((grid           grid)
                   (texture-object texture-object)
                   (burning-speed burning-speed)
                   (delay delay)
                   (pick-random-p pick-random-p)
                   (text text)
                   (children children)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (fixnum delay))
    (if (<= delay 0)
        (progn
          (when (not (vector-empty-p children))
            (mtree:remove-child object text))
          (update grid :speed burning-speed :pick-random-p pick-random-p)
          (draw texture-object grid)
          (update-for-rendering texture-object))
        (decf delay))))

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
                   (material-params material-params)
                   (text text)
                   (children children)) object
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
          (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))
    (do-children-mesh (i object)
      (render i renderer))))

(defmethod removeable-from-world-p ((object burn-mask))
  (every #'(lambda (a) (d> (cell-intensity a) 0.90))
         (matrix:data (grid-cells (grid object)))))

(defun make-burn-mask (text color &optional (texture-file +burn-texture+))
  (let ((instance (make-instance 'burn-mask
                                 :width    (d *window-w*)
                                 :height   (d *window-h*)
                                 :title    text
                                 :color    color))
        (texture  (texture:get-texture (res:get-resource-file texture-file
                                                              +default-gui-resource+
                                                              :if-does-not-exists :error))))
    (texture:prepare-for-rendering texture)
    (pixmap:cristallize-bits texture)
    (setf (texture-object instance) texture)
    instance))

(defun turn-billboard-w ()
  (d *window-w*))

(defun turn-billboard-v-fn (time)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (desired-type time))
  (let* ((actual-time (clamp time 0.0001 0.9999))
         (v (d (dexpt (log (d/ actual-time
                             (d- 1.0 actual-time))
                          10)
                     2.0))))
    (d* 20.8 v)))

(defclass turn-billboard (widget inner-animation end-life-trigger)
  ((offset
    :initform 0.0
    :initarg :offset
    :accessor offset)
   (end-time
    :initform 2.5
    :initarg  :end-time
    :accessor end-time)))

(defmethod initialize-instance :after ((object turn-billboard) &key &allow-other-keys)
  (with-accessors ((width width)
                   (height height)
                   (state state)) object
    (add-quad-for-widget object)
    (transform-vertices object (sb-cga:scale* (width object) (height object) 1.0))
    (prepare-for-rendering object)
    (with-world (world state)
      (setf (end-of-life-callback object)
            #'(lambda () (game-event:with-send-action-terminated-check-type
                             (world action-scheduler:turn-billboard-show-action)
                           (widget:hide-and-remove-from-parent object nil)))))))

(defmethod calculate :after ((object turn-billboard) dt)
  (with-accessors ((offset               offset)
                   (animation-speed      animation-speed)
                   (start-time           start-time)
                   (end-time             end-time)
                   (el-time              el-time)
                   (pos                  pos)
                   (end-of-life-callback end-of-life-callback)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (desired-type dt el-time offset animation-speed start-time))
    (setf el-time (d+ el-time dt))
    (let ((p (normalize-value-in-range el-time start-time end-time)))
      (setf offset
            (d+ offset (d* (turn-billboard-v-fn p) animation-speed dt)))
      (setf (vec-x pos) offset)
      (with-maybe-trigger-end-of-life (object (removeable-from-world-p object))))))

(defmethod render ((object turn-billboard) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)
                   (material-params material-params)
                   (text text)
                   (children children)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (cl-gl-utils:with-depth-disabled
            (cl-gl-utils:with-blending
              (gl:blend-func :src-alpha :one-minus-src-alpha)
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
              (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))))

(defmethod removeable-from-world-p ((object turn-billboard))
  (d> (offset object)
      (d *window-w*)))

(defun make-turn-billboard-texture (texture-file)
  (let ((texture (tex:get-texture (res:get-resource-file texture-file
                                                         +turn-transition-billboard-dir+))))
    (setf (texture:border-color       texture) §c00000000)
    (setf (texture:s-wrap-mode        texture) :clamp-to-border)
    (setf (texture:t-wrap-mode        texture) :clamp-to-border)
    (setf (texture:interpolation-type texture) :linear)
    (prepare-for-rendering texture)
    texture))

(defun make-turn-billboard (texture-filename compiled-shaders)
  (make-instance 'turn-billboard
                 :animation-speed  70.0
                 :offset           (d- (turn-billboard-w))
                 :width            (turn-billboard-w)
                 :height           (d/ (d *window-h*) 3.0)
                 :y                (d/ (d *window-h*) 2.0)
                 :compiled-shaders compiled-shaders
                 :texture-object   (make-turn-billboard-texture texture-filename)))

(defun make-turn-billboard-ai (compiled-shaders)
  (make-turn-billboard +turn-billboard-ai-texture-name+ compiled-shaders))

(defun make-turn-billboard-human (compiled-shaders)
  (make-turn-billboard +turn-billboard-human-texture-name+ compiled-shaders))

(defun enqueue-turn-billboard-* (world type-fn)
  (action-scheduler:with-enqueue-action (world
                                         action-scheduler:turn-billboard-show-action)
    (mtree:add-child (world:gui world)
                     (funcall type-fn (compiled-shaders world)))))

(defun enqueue-turn-billboard-ai (world)
  (enqueue-turn-billboard-* world #'make-turn-billboard-ai))

(defun enqueue-turn-billboard-human (world)
  (enqueue-turn-billboard-* world #'make-turn-billboard-human))

(defclass fade-curtain (widget inner-animation end-life-trigger)
  ((end-time
    :initform 2.5
    :initarg  :end-time
    :accessor end-time)
   (fading-fn
    :initform #'(lambda (time) (smoothstep-interpolate 0.0 1.0 time))
    :initarg  :fading-fn
    :accessor fading-fn)
   (stopping-fn
    :initform #'(lambda (alpha) (d> alpha 0.9999))
    :initarg  :stopping-fn
    :accessor stopping-fn)
   (color
    :initform (vec4 1.0 0.0 1.0 1.0)
    :initarg  :color
    :accessor color)
   (shader
    :initform :fade
    :initarg  :shader
    :accessor shader)
   (alpha
    :initform 0.0
    :initarg  :alpha
    :accessor alpha)))

(defmethod initialize-instance :after ((object fade-curtain) &key &allow-other-keys)
  (with-accessors ((width width)
                   (height height)
                   (state state)) object
    (add-quad-for-widget object)
    (transform-vertices object (sb-cga:scale* (width object) (height object) 1.0))
    (prepare-for-rendering object)
    (setf (end-of-life-callback object)
          #'(lambda () (widget:hide-and-remove-from-parent object nil)))))

(defmethod calculate :after ((object fade-curtain) dt)
  (with-accessors ((animation-speed      animation-speed)
                   (el-time              el-time)
                   (alpha                alpha)
                   (fading-fn            fading-fn)
                   (end-of-life-callback end-of-life-callback)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (desired-type dt el-time animation-speed))
    (declare (function fading-fn))
    (setf el-time (d+ el-time (d* animation-speed dt)))
    (setf alpha  (funcall fading-fn el-time))
    (with-maybe-trigger-end-of-life (object (removeable-from-world-p object)))))

(defmethod render ((object fade-curtain) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)
                   (material-params material-params)
                   (color   color)
                   (alpha   alpha)
                   (el-time el-time)
                   (shader  shader)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (declare (desired-type alpha))
    (declare (vec4 color))
    (with-camera-view-matrix (camera-vw-matrix renderer)
      (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
        (cl-gl-utils:with-depth-disabled
          (cl-gl-utils:with-blending
            (gl:blend-equation :func-add)
            (gl:blend-func :src-alpha :one-minus-src-alpha)
            (use-program compiled-shaders shader)
            (gl:active-texture :texture0)
            (texture:bind-texture texture-object)
            (uniformi compiled-shaders  :texture-object +texture-unit-diffuse+)
            (uniformf compiled-shaders  :alpha (clamp alpha 0.0 1.0))
            (uniformf compiled-shaders  :time  el-time)
            (uniformfv compiled-shaders :fade-color color)
            (uniform-matrix compiled-shaders :modelview-matrix 4
                            (vector (sb-cga:matrix* camera-vw-matrix
                                                    (elt view-matrix 0)
                                                    (elt model-matrix 0)))
                            nil)
            (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
            (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
            (gl:draw-arrays :triangles 0 (* 3 (length triangles)))))))
    (do-children-mesh (c object)
      (render c renderer))))

(defmethod removeable-from-world-p ((object fade-curtain))
  (with-accessors ((alpha       alpha)
                   (stopping-fn stopping-fn)) object
    (funcall stopping-fn alpha)))

(defun make-fade-curtain (compiled-shaders
                          &key
                            (speed 5e-1)
                            (color (vec4 0.0 0.0 0.0 0.0))
                            (direction :in)
                            (texture-name +transparent-texture-name+))
  (make-instance 'fade-curtain
                 :fading-fn       (if (eq direction :in)
                                      #'(lambda (time)
                                          (smoothstep-interpolate 0.0 1.0 time))
                                      #'(lambda (time)
                                          (d- 1.0 (smoothstep-interpolate 0.0 1.0 time))))
                 :stopping-fn     (if (eq direction :in)
                                      #'(lambda (alpha) (d> alpha 0.9999))
                                      #'(lambda (alpha) (d< alpha 1e-5)))
                 :texture-object   (texture:get-texture texture-name)
                 :color            color
                 :animation-speed  speed
                 :width            (d *window-w*)
                 :height           (d *window-h*)
                 :x                0.0
                 :y                0.0
                 :shader           :fade
                 :compiled-shaders compiled-shaders))

(defun make-fade-out-flash (compiled-shaders
                          &key
                            (speed        20.0)
                            (color        (vec4 1.0 1.0 1.0 0.0))
                            (texture-name +white-texture-name+)
                            (width        (d *window-w*))
                            (height       (d *window-h*))
                            (x            0.0)
                            (y            0.0))
  (make-instance 'fade-curtain
                 :fading-fn        #'(lambda (time)  (num:impulse time 4.5e-1))
                 :stopping-fn      #'(lambda (alpha) (d< alpha 1e-3))
                 :texture-object   (texture:get-texture texture-name)
                 :color            color
                 :animation-speed  speed
                 :width            width
                 :height           height
                 :x                x
                 :y                y
                 :shader           :fade-flash
                 :compiled-shaders compiled-shaders))

(defun make-lava-fx (compiled-shaders
                     &key
                       (speed        5.0e-1)
                       (color        (vec4 1.0 0.34 0.0 1.0))
                       (texture-name +logo-mask-texture-name+)
                       (width        (d *window-w*))
                       (height       (d *window-h*))
                       (x            0.0)
                       (y            0.0))
  (make-instance 'fade-curtain
                 :fading-fn        #'(lambda (time)  time)
                 :stopping-fn
                 #-debug-mode #'(lambda (alpha) (declare (ignore alpha)) nil)
                 #+debug-mode (let ((time 100.0))
                                #'(lambda (alpha)
                                    (declare (ignore alpha))
                                    (decf time 0.1)
                                    (< time 0.0)))
                 :texture-object   (texture:get-texture texture-name)
                 :color            color
                 :animation-speed  speed
                 :width            width
                 :height           height
                 :x                x
                 :y                y
                 :shader           :fade-lava
                 :compiled-shaders compiled-shaders))

(defclass spark-fx (fade-curtain)
  ((actual-el-time
    :initform 0.0
    :initarg  :actual-el-time
    :accessor actual-el-time)
   (delay-ct
    :initform 100
    :initarg  :delay-ct
    :accessor delay-ct)
   (delay
     :initform 100
     :initarg  :delay
     :accessor delay)
   (delay-range
     :initform (cons 300 400)
     :initarg  :delay-range
     :accessor delay-range)
   (animate
     :initform nil
     :initarg  :animate
     :reader   animatep
     :writer   (setf animate))))

(defmethod calculate :after ((object spark-fx) dt)
  (with-accessors ((el-time                 el-time)
                   (fading-fn               fading-fn)
                   (alpha                   alpha)
                   (animation-speed         animation-speed)
                   (actual-el-time          actual-el-time)
                   (delay                   delay)
                   (animate                 animate)
                   (delay-ct                delay-ct)
                   (delay-range             delay-range)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (desired-type dt el-time actual-el-time animation-speed))
    (declare (fixnum delay delay-ct))
    (declare (function fading-fn))
    (setf actual-el-time (d+ actual-el-time (d* animation-speed dt)))
    (when (and (animatep object)
               (>= el-time 1.0))
      (setf animate nil)
      ;; note that alpha  is (ab)used as a flag to  stop the rendering
      ;; of this effect: check the shader!
      (setf alpha 0.0)
      (setf delay (lcg-next-in-range* delay-range))
      (setf delay-ct delay)
      (setf el-time 0.0))
    (if (not (animatep object))
        (if (< delay-ct 0)
            (progn
              (setf animate t)
              (setf alpha  0.6)
              (setf actual-el-time 0.0))
            (progn
              (setf alpha 0.0)
              (setf el-time 0.0)
              (decf delay-ct)))
        (progn
          (setf alpha 0.6)
          (setf el-time actual-el-time)))
    (with-maybe-trigger-end-of-life (object (removeable-from-world-p object)))))

(defun make-spark-fx (compiled-shaders
                      &key
                        (fading-fn   #'(lambda (time)  time))
                        (stopping-fn #'(lambda (alpha) (declare (ignore alpha)) nil))
                        (speed        5.0e-1)
                        (color        (vec4 .7 .7 .5 1.0))
                        (delay-range  (cons 300 400))
                        (texture-name +logo-mask-texture-name+)
                        (width        (d *window-w*))
                        (height       (d *window-h*))
                        (x            0.0)
                        (y            0.0))
  (make-instance 'spark-fx
                 :fading-fn        fading-fn
                 :stopping-fn      stopping-fn
                 :texture-object   (texture:get-texture texture-name)
                 :color            color
                 :delay-range      delay-range
                 :animation-speed  speed
                 :width            width
                 :height           height
                 :x                x
                 :y                y
                 :shader           :spark
                 :compiled-shaders compiled-shaders))

(defun make-logo-spark (compiled-shaders)
  (make-spark-fx compiled-shaders
                 :fading-fn   #'(lambda (time)  time)
                 :stopping-fn #'(lambda (alpha) (declare (ignore alpha)) nil)
                 :speed        25.0e-1
                 :color        (vec4 0.7 0.7 0.5 1.0)
                 :delay-range  (cons 300 400)
                 :texture-name +logo-texture-name+
                 :width        (d *logo-w*)
                 :height       (d *logo-h*)
                 :x            0.0
                 :y            0.0))

(defun make-logo-lava (compiled-shaders)
  (make-lava-fx compiled-shaders
                :speed        10.0e-1
                :color        (vec4 1.0 0.34 0.0 1.0)
                :texture-name +logo-mask-texture-name+
                :width        (d *logo-w*)
                :height       (d *logo-h*)
                :x            (d (- (/ *window-w* 2)
                                    (/ *logo-w* 2)))
                :y            (d (- *window-h*
                                    *logo-h*))))
(defun make-logo (compiled-shaders)
  (let ((lava (make-logo-lava  compiled-shaders))
        (logo (make-logo-spark compiled-shaders)))
    (mtree:add-child lava logo)))
