;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along      with      this      program.       If      not,      see
;; <http://www.gnu.org/licenses/>.

(in-package :credits)

(define-constant +credit-prefix+ "^[0-9]+" :test #'string=)

(define-constant +all-credits-bg+ '("bg-1.tga"  "bg-2.tga" "bg-3.tga" "bg-4.tga") :test #'equalp)

(define-constant +all-credits-bg-speeds+ '(0.05 0.0 0.15 0.25)                    :test #'equalp)

(defclass credit-item (widget inner-animation end-life-trigger)
  ((end-time
    :initform 1.2
    :initarg  :end-time
    :accessor end-time)
   (pixel-size
    :initform 50.0
    :initarg  :pixel-size
    :accessor pixel-size)))

(defmethod initialize-instance :after ((object credit-item) &key &allow-other-keys)
  (add-quad-for-widget object)
  (transform-vertices object (sb-cga:scale* (width object) (height object) 1.0))
  (prepare-for-rendering object)
  (setf (end-of-life-callback object)
        #'(lambda ()
            (widget:hide-and-remove-from-parent object nil)
            (game-event:send-action-terminated-event))))

(defmethod calculate :after ((object credit-item) dt)
  (with-accessors ((animation-speed      animation-speed)
                   (end-time             end-time)
                   (el-time              el-time)
                   (end-of-life-callback end-of-life-callback)
                   (pixel-size         pixel-size)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (desired-type dt el-time animation-speed))
    (let* ((scaling-thrs-scaling 0.3)
           (scaling-thrs (d* end-time scaling-thrs-scaling)))
      (cond
        ((d< el-time scaling-thrs)
         (setf pixel-size (dlerp (d- 1.0 (smoothstep-interpolate 0.0 scaling-thrs el-time))
                                 200.0
                                 0.0)))
        ((d> el-time (d* end-time (d- 1.0 scaling-thrs)))
         (setf pixel-size (dlerp (smoothstep-interpolate (d- 1.0 scaling-thrs) end-time el-time)
                                 150.0
                                 0.0)))))
    (setf el-time (d+ el-time (d* animation-speed dt)))
    (with-maybe-trigger-end-of-life (object (removeable-from-world-p object)))))

(defmethod render ((object credit-item) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo               vbo)
                   (vao               vao)
                   (texture-object    texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix      model-matrix)
                   (view-matrix       view-matrix)
                   (compiled-shaders  compiled-shaders)
                   (triangles         triangles)
                   (material-params   material-params)
                   (pixel-size      pixel-size)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
            (cl-gl-utils:with-blending
              (gl:blend-func :src-alpha :one-minus-src-alpha)
              (use-program compiled-shaders :credit-item)
              (gl:active-texture :texture0)
              (texture:bind-texture texture-object)
              (uniformf compiled-shaders :pixel-size  pixel-size)
              (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
              (uniform-matrix compiled-shaders :modelview-matrix 4
                              (vector (sb-cga:matrix* camera-vw-matrix
                                                      (elt view-matrix 0)
                                                      (elt model-matrix 0)))
                              nil)
              (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
              (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
              (gl:draw-arrays :triangles 0 (* 3 (length triangles)))))))))

(defmethod removeable-from-world-p ((object credit-item))
  (d> (el-time  object)
      (end-time object)))

(defun all-credit-items ()
  (flet ((extract (a)
           (cl-ppcre:scan-to-strings +credit-prefix+
                                     (fs:strip-dirs-from-path a))))
    (let ((all (remove-if-not #'extract
                              (mapcar #'fs:pathname->namestring
                                      (res:get-resource-files +default-credit-items+)))))
    (shellsort all
               #'(lambda (a b)
                     ;; checking for null prefix is useless but not harmful
                     (let ((na (safe-parse-number (extract a)))
                           (nb (safe-parse-number (extract b))))
                       (cond
                         ((null na)
                          nil)
                         ((null nb)
                          t)
                         (t
                          (< na nb)))))))))

(defun make-credit-item (world texture-name
                         &key
                           (w (d (/ *window-w* 2)))
                           (h (d (/ *window-h* 3))))
  (texture:with-prepared-texture (texture-credit texture-name)
    (let* ((compiled-shaders (world:compiled-shaders world))
           (text             (make-instance 'credit-item
                                            :animation-speed  0.2
                                            :texture-object   texture-credit
                                            :compiled-shaders compiled-shaders
                                            :width            w
                                            :height           h
                                            :x                (d- (d (/ *window-w* 2.0))
                                                                  (d/ w 2.0))
                                            :y                (d- (d (/ *window-h* 2.0))
                                                                  (d/ h 2.0)))))
      text)))

(defun make-credits-texts (world)
  (let* ((all-items (all-credit-items)))
    (map 'nil #'(lambda (item)
                  (action-scheduler:with-enqueue-action (world)
                    (let ((text (make-credit-item world item)))
                      (mtree:add-child (world:gui world) text))))
         all-items)
    (action-scheduler:with-enqueue-action-and-send-remove-after
        (world action-scheduler:game-action)
      (remove-entity-if (world:gui world)
                        #'(lambda (a) (typep a 'scrolling-bg))))))

(defclass scrolling-bg (widget inner-animation)
  ((dx
    :initform 0.0
    :accessor dx)
   (dy
    :initform 0.0
    :accessor dy)
   (traj-x-fn
    :initform #'(lambda (dt) (d* 0.1 dt))
    :initarg  :traj-x-fn
    :accessor traj-x-fn)))

(defmethod initialize-instance :after ((object scrolling-bg) &key &allow-other-keys)
  (add-quad-for-widget object)
  (transform-vertices object (sb-cga:scale* (width object) (height object) 1.0))
  (prepare-for-rendering object))

(defmethod calculate :after ((object scrolling-bg) dt)
  (with-accessors ((animation-speed      animation-speed)
                   (end-time             end-time)
                   (el-time              el-time)
                   (end-of-life-callback end-of-life-callback)
                   (pixel-size         pixel-size)
                   (dx                 dx)
                   (traj-x-fn          traj-x-fn)) object
    (declare (optimize (debug 0) (speed 3) (safety 0)))
    (declare (desired-type dt el-time animation-speed))
    (declare (function traj-x-fn))
    (setf el-time (d+ el-time (d* animation-speed dt)))
    (setf dx (d+ dx (funcall traj-x-fn dt)))))

(defmethod render ((object scrolling-bg) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo               vbo)
                   (vao               vao)
                   (texture-object    texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix      model-matrix)
                   (view-matrix       view-matrix)
                   (compiled-shaders  compiled-shaders)
                   (triangles         triangles)
                   (material-params   material-params)
                   (pixel-size        pixel-size)
                   (dx                dx)
                   (dy                dy)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (shown object)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (use-program compiled-shaders :scrolling-bg)
          (gl:active-texture :texture0)
          (texture:bind-texture texture-object)
          (uniformf compiled-shaders :dx dx)
          (uniformf compiled-shaders :dy dy)
          (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
          (uniform-matrix compiled-shaders :modelview-matrix 4
                          (vector (sb-cga:matrix* camera-vw-matrix
                                                  (elt view-matrix 0)
                                                  (elt model-matrix 0)))
                          nil)
          (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
          (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
          (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))

(defun make-scrolling-bg (texture-name speed compiled-shaders)
  (let ((texture-name (res:get-resource-file texture-name +default-credit-items+)))
    (texture:with-prepared-texture (texture-bg texture-name)
      (let ((bg (make-instance 'scrolling-bg
                               :compiled-shaders compiled-shaders
                               :traj-x-fn        #'(lambda (dt) (d* speed dt))
                               :texture-object   texture-bg
                               :width            (d *window-w*)
                               :height           (d *window-h*)
                               :x                0.0
                               :y                0.0)))
        bg))))

(defun make-credits-bg (world)
  (loop
     for texture in +all-credits-bg+
     for speed   in +all-credits-bg-speeds+ do
       (let ((bg (make-scrolling-bg texture speed (world:compiled-shaders world))))
         (mtree:add-child (world:gui world) bg))))

(defun make-credits (world)
  (make-credits-bg    world)
  (make-credits-texts world))
