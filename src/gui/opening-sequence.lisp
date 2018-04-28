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

(in-package :opening-sequence)

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
                 :stopping-fn      #'(lambda (alpha) (declare (ignore alpha)) nil)
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

(defclass opening-text (signalling-light) ())

(defclass opening-text (widget inner-animation end-life-trigger)
  ((end-time
    :initform 1.2
    :initarg  :end-time
    :accessor end-time)))

(defmethod initialize-instance :after ((object opening-text) &key &allow-other-keys)
  (with-accessors ((width width)
                   (height height)
                   (state state)) object
    (add-quad-for-widget object)
    (transform-vertices object (sb-cga:scale* (width object) (height object) 1.0))
    (prepare-for-rendering object)
    (setf (end-of-life-callback object)
          #'(lambda () (widget:hide-and-remove-from-parent object nil)))))

(defmethod calculate :after ((object opening-text) dt)
  (with-accessors ((animation-speed      animation-speed)
                   (el-time              el-time)
                   (end-of-life-callback end-of-life-callback)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (desired-type dt el-time animation-speed))
    (setf el-time (d+ el-time (d* animation-speed dt)))
    (with-maybe-trigger-end-of-life (object (removeable-from-world-p object)))))

(defmethod render ((object opening-text) renderer)
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
              (gl:draw-arrays :triangles 0 (* 3 (length triangles)))))))))

(defmethod removeable-from-world-p ((object opening-text))
  (d> (el-time  object)
      (end-time object)))

(defun op-button-h ()
  (d* 5.0 (h1-font-size *reference-sizes*)))

(defun op-button-w ()
  (d* 2.0 (op-button-h)))

(defun make-opening-button (x y label callback)
  (let* ((h (op-button-h))
         (w (op-button-w)))
    (make-instance 'button
                   :use-label-global-style nil
                   :label-font-size        14.00
                   :x                      x
                   :y                      y
                   :width                  w
                   :height                 h
                   :label                  label
                   :callback               callback)))

(defun make-quit-cb (world)
  #'(lambda (b e)
      (declare (ignore b e))
      (with-accessors ((main-state main-state)) world
        (sdl2.kit:close-window (game-state:fetch-render-window main-state)))))

(defun make-load-cb (gui)
  #'(lambda (w e)
      (declare (ignore e))
      (tg:gc)
      (let ((window  (load-save-window:make-window (compiled-shaders w) :load)))
        (mtree:add-child gui window))))

(defclass scrolling-opening (signalling-light inner-animation end-life-trigger)
  ((stop-scrolling
    :initform nil
    :initarg  :stop-scrolling
    :accessor stop-scrolling)
   (opening-text-params
    :initform (list (cons  3.0 (texture:get-texture +text-1-texture-name+))
                    (cons 16.0 (texture:get-texture +text-2-texture-name+)))
    :initarg  :opening-text-params
    :accessor opening-text-params
    :documentation "alist '(time-to-start . texture-to-show)")
   (world
    :initform nil
    :initarg  :world
    :accessor world)
   (b-new-game
    :initform (make-opening-button (d (- (/ *window-w* 2.0) (/ (op-button-w) 2)))
                                   (d (+ (* 2 (op-button-h))
                                         (* *window-h* 1/8)))
                                   (_ "New Game")
                                   nil)

    :initarg  :b-new-game
    :accessor b-new-game)
   (b-load-game
    :initform (make-opening-button (d (- (/ *window-w* 2.0) (/ (op-button-w) 2)))
                                   (d (+ (op-button-h)
                                         (* *window-h* 1/8)))
                                   (_ "Load Game")
                                   nil) ;; callback is setted in 'on-end-fn'

    :initarg  :b-load-game
    :accessor b-load-game)
   (b-quit-game
    :initform (make-opening-button (d (- (/ *window-w* 2.0) (/ (op-button-w) 2)))
                                   (d (* *window-h* 1/8))
                                   (_ "Quit Game")
                                   nil) ;; callback is setted in 'on-end-fn'

    :initarg  :b-quit-game
    :accessor b-quit-game)))

(defun on-end-fn (scrolling-opening-instance)
  #'(lambda ()
      (with-accessors ((world            world)
                       (compiled-shaders compiled-shaders)
                       (b-new-game       b-new-game)
                       (b-load-game      b-load-game)
                       (b-quit-game      b-quit-game)) scrolling-opening-instance
        (with-accessors ((gui world:gui)) world
          (setf (compiled-shaders b-new-game) compiled-shaders)
          (setf (compiled-shaders b-load-game) compiled-shaders)
          (setf (compiled-shaders b-quit-game) compiled-shaders)
          (setf (callback         b-quit-game) (make-quit-cb world))
          (setf (callback         b-load-game) (make-load-cb gui))
          ;; order of adding is important: the first is added the first is drawn
          (mtree:add-child gui (make-logo compiled-shaders))
          (mtree:add-child gui b-quit-game)
          (mtree:add-child gui b-load-game)
          (mtree:add-child gui b-new-game)
          (mtree:add-child gui (full-screen-masks:make-fade-out-flash compiled-shaders
                                                                      :speed 10.0))))))

(defmethod initialize-instance :after ((object scrolling-opening) &key &allow-other-keys)
  (with-accessors ((width            width)
                   (height           height)
                   (y                y)
                   (state            state)
                   (compiled-shaders compiled-shaders)) object
    (setf (end-of-life-callback object) (on-end-fn object))))

(defgeneric maybe-apply-opening-text (scrolling-opening time))

(defmethod maybe-apply-opening-text ((object scrolling-opening) time)
  (flet ((time-test (a b)
           (let ((*default-epsilon* 1e-1))
             (epsilon= a (car b)))))
    (with-accessors ((compiled-shaders compiled-shaders)
                     (opening-text-params opening-text-params)
                     (text-textures       text-textures)
                     (world               world)) object
      (with-accessors ((gui world:gui)) world
        (when-let* ((pos  (position time
                                    opening-text-params
                                    :test #'time-test))
                    (tex  (cdr (elt opening-text-params pos)))
                    (text (make-instance 'opening-text
                                         :x                0.0
                                         :y                0.0
                                         :width            (d *window-w*)
                                         :height           (d *window-h*)
                                         :compiled-shaders compiled-shaders
                                         :texture-object   tex)))
          (setf opening-text-params (delete@ opening-text-params pos))
          (mtree:add-child gui text))))))

(defmethod calculate :after ((object scrolling-opening) dt)
  (with-accessors ((animation-speed      animation-speed)
                   (el-time              el-time)
                   (alpha                alpha)
                   (fading-fn            fading-fn)
                   (end-of-life-callback end-of-life-callback)
                   (stop-scrolling       stop-scrolling)
                   (y                    y)
                   (height               height)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (desired-type dt el-time animation-speed))
    (declare (function fading-fn))
    (when (not stop-scrolling)
      (maybe-apply-opening-text object el-time)
      (setf el-time (d+ el-time dt))
      (setf (pos object) (sb-cga:vec+ (pos object)
                                      (sb-cga:vec 0.0
                                                  (d* animation-speed dt)
                                                  0.0)))
      (with-maybe-trigger-end-of-life (object (d> y 0.0))
        (setf stop-scrolling t)))))

(defmethod render :after ((object scrolling-opening) renderer)
  (do-children-mesh (c object)
    (render c renderer)))

(defun make-opening (world)
  (let* ((w                (d *window-w*))
         (h                (d (* 3.0 *window-h*)))
         (compiled-shaders (world:compiled-shaders world))
         (opening          (make-instance 'scrolling-opening
                                          :world            world
                                          :animation-speed  #+fast-opening 3000.0
                                                            #-fast-opening 30.0
                                          :compiled-shaders compiled-shaders
                                          :x                0.0
                                          :y                (d (- *window-h*))
                                          :width            w
                                          :height           h
                                          :texture-name     +bg-start-texture-name+
                                          :button-status    t))
         (fading           (full-screen-masks:make-fade-curtain compiled-shaders
                                                                :width            w
                                                                :height           h
                                                                :direction        :out
                                                                :speed            0.25)))
    (mtree:add-child opening fading)
    opening)) ; useless as add-child return the parent
