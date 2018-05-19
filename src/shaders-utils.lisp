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

(in-package :shaders-utils)

(alexandria:define-constant +attribute-position-location+        0 :test #'=)

(alexandria:define-constant +attribute-normal-location+          4 :test #'=)

(alexandria:define-constant +attribute-tangent-location+         8 :test #'=)

(alexandria:define-constant +attribute-texture-location+        12 :test #'=)

;; for instanced rendering
(alexandria:define-constant +attribute-pick-pos-location+        1 :test #'=)

;; for terrain decals (roads etc...)
(alexandria:define-constant +attribute-texture-decals-location+ 13 :test #'=)

(alexandria:define-constant +attribute-pick-weight-location+     1 :test #'=)

(alexandria:define-constant +texture-unit-diffuse+               0 :test #'=)

(alexandria:define-constant +texture-unit-normalmap+             1 :test #'=)

(alexandria:define-constant +texture-unit-projector+             4 :test #'=)

(alexandria:define-constant +texture-unit-clouds-1+              1 :test #'=)

(alexandria:define-constant +texture-unit-clouds-2+              2 :test #'=)

(alexandria:define-constant +texture-unit-clouds-3+              3 :test #'=)

(alexandria:define-constant +feedback-new-position+  :new-position :test #'eq)

(alexandria:define-constant +feedback-new-velocity+  :new-velocity :test #'eq)

(alexandria:define-constant +feedback-out-mixed+     :mixed        :test #'eq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-shader-source (name)
    (let ((actual-file (fs:preprocess (res:get-resource-file name +shaders-resource+)
                                      +shaders-resource+)))
      (prog1
          (filesystem-utils:slurp-file actual-file)
        (fs:delete-file-if-exists actual-file)))))

;; (defvar *shader-dict*
;;   '((:solid
;;      (:uniforms ((x "x") (y "y")))
;;      (:shaders
;;       :vertex-shader "..."
;;       :fragment-shader "..."))))

(defun compile-and-check-shader (shader source)
  (gl:shader-source shader source)
  (gl:compile-shader shader)
  (unless (gl:get-shader shader :compile-status)
    (gl:get-shader-info-log shader)))

(defmacro with-compiling ((program compiled-shaders) &body body)
  `(let (,compiled-shaders)
     (loop
        for type in shaders by #'cddr
        for text in (cdr shaders) by #'cddr
        do (let ((shader (gl:create-shader type)))
             (alexandria:when-let ((log (compile-and-check-shader shader text)))
                 (format *error-output* "Compile Log for ~A:~%~A~%shader ~a" type log text))
             (push shader ,compiled-shaders)))
     (let ((,program (gl:create-program)))
       (if (= 0 program)
           (progn
             (loop for shader in ,compiled-shaders
                do (gl:delete-shader shader))
             (error "Error creating program"))
           (progn ,@body)))))

(defun compile-and-link-program (&rest shaders)
  "(compile-and-link-program :vertex-shader STRING :fragment-shader STRING ...)"
  (with-compiling (program compiled-shaders)
    (loop for shader in compiled-shaders
       do (gl:attach-shader program shader))
    (gl:link-program program)
    (let ((log (gl:get-program-info-log program)))
      (unless (string= "" log)
        (format *error-output* "Link Log:~%~A~%" log)))
    (loop for shader in compiled-shaders
       do (gl:detach-shader program shader)
         (gl:delete-shader shader))
    program))

(defun compile-and-link-feedback-particles-program (&rest shaders)
  "(compile-and-link-program :vertex-shader STRING :fragment-shader STRING ...)"
  (with-compiling (program compiled-shaders)
    (loop for shader in compiled-shaders
       do (gl:attach-shader program shader))
    ;; set varyings output variable name
    (cffi:with-foreign-object (string-array :pointer 2)
      (setf (cffi:mem-aref string-array :pointer 0)
            (cffi:foreign-string-alloc (symbol-to-uniform +feedback-new-position+)))
      (setf (cffi:mem-aref string-array :pointer 1)
            (cffi:foreign-string-alloc (symbol-to-uniform +feedback-new-velocity+)))
      (%gl:transform-feedback-varyings-ext program 2 string-array :interleaved-attribs)
      (cffi:foreign-string-free (cffi:mem-aref string-array :pointer 0))
      (cffi:foreign-string-free (cffi:mem-aref string-array :pointer 1)))
    (gl:link-program program)
    (let ((log (gl:get-program-info-log program)))
      (unless (string= "" log)
        (format *error-output* "Link Log:~%~A~%" log)))
    (loop for shader in compiled-shaders do
         (gl:detach-shader program shader)
         (gl:delete-shader shader))
    program))

(defun compile-and-link-feedback-lerp-program (&rest shaders)
  "(compile-and-link-program :vertex-shader STRING :fragment-shader STRING ...)"
  (with-compiling (program compiled-shaders)
    (loop for shader in compiled-shaders
       do (gl:attach-shader program shader))
    ;; set varyings output variable name
    (cffi:with-foreign-object (string-array :pointer 1)
      (setf (cffi:mem-aref string-array :pointer 0)
            (cffi:foreign-string-alloc (symbol-to-uniform +feedback-out-mixed+)))
      (%gl:transform-feedback-varyings-ext program 1 string-array :interleaved-attribs)
      (cffi:foreign-string-free (cffi:mem-aref string-array :pointer 0)))
    (gl:link-program program)
    (let ((log (gl:get-program-info-log program)))
      (unless (string= "" log)
        (format *error-output* "Link Log:~%~A~%" log)))
    (loop for shader in compiled-shaders do
         (gl:detach-shader program shader)
         (gl:delete-shader shader))
    program))

(defclass program ()
  ((name
    :initform nil
    :initarg :name)
   (id
    :initform nil)
   (uniforms
    :initform (make-hash-table :test 'equal))))

(defclass shader-dictionary ()
  ((programs
    :initform (make-hash-table))
   (active-program
    :initform nil)))

(defgeneric preprocess-program-entry (type entry program))

(defgeneric postprocess-program-entry (type entry program))

(defmethod preprocess-program-entry (type entry program))

(defmethod postprocess-program-entry (type entry program))

(defmethod interfaces:destroy ((object program))
  (with-slots (id) object
    (gl:delete-program id)))

(defmethod interfaces:destroy ((object shader-dictionary))
  (with-slots (programs) object
    (loop for program being the hash-values in programs do
         (interfaces:destroy program))))

(defmethod preprocess-program-entry ((type (eql :shaders)) entry program)
  (let ((p (apply #'compile-and-link-program entry)))
    (gl:use-program p)
    (with-slots (id) program
      (setf id p))))

(defmethod preprocess-program-entry ((type (eql :feedback-particles-shaders)) entry program)
  (let ((p (apply #'compile-and-link-feedback-particles-program entry)))
    (gl:use-program p)
    (with-slots (id) program
      (setf id p))))

(defmethod preprocess-program-entry ((type (eql :feedback-lerp-shaders)) entry program)
  (let ((p (apply #'compile-and-link-feedback-lerp-program entry)))
    (gl:use-program p)
    (with-slots (id) program
      (setf id p))))

(defun symbol-to-uniform (symbol)
  (substitute #\_ #\- (string-downcase (symbol-name symbol))))

(defmethod postprocess-program-entry ((type (eql :uniforms)) entry program)
  (with-slots (id uniforms) program
    (loop for uniform in entry
          as symbol = (if (symbolp uniform) uniform (car uniform))
          as name = (if (or (symbolp uniform)
                            (not (cadr uniform)))
                        (symbol-to-uniform uniform)
                        (cadr uniform))
          as loc = (gl:get-uniform-location id name)
          do (setf (gethash symbol uniforms) loc))))

(defun find-program (dictionary name)
  (with-slots (programs) dictionary
    (gethash name programs)))

(defun find-uniform (dictionary program name)
  (with-slots (uniforms) (find-program dictionary program)
    (gethash name uniforms)))

(defun compile-shader-dictionary (dictionary)
  "Input is a well-formatted list of shaders.  Returns a new
SHADER-DICTIONARY object.  This must be called with a valid, active
GL-CONTEXT.  The result is only valid while that GL-CONTEXT is
valid."
  (let ((sd (make-instance 'shader-dictionary)))
    (with-slots (programs) sd
      (loop
         for program-spec in dictionary
         as name = (car program-spec)
         as program = (make-instance 'program :name name)
         do (setf (gethash name programs) program)
           (loop for entry in (cdr program-spec)
              do
                #+debug-mode (misc:dbg "preprocess  ~a" (car entry))
                (preprocess-program-entry
                 (car entry) (cdr entry) program))
           (loop for entry in (cdr program-spec)
              do (postprocess-program-entry
                  (car entry) (cdr entry) program))))
    sd))

(defun use-program (dict program)
  "Set program named `PROGRAM` in `DICT` as the active program."
  (with-slots (active-program) dict
    (let ((p (find-program dict program)))
      (with-slots (id) p
        (setf active-program p)
        (gl:use-program id)))))

(defmacro with-uniform-location ((var name) dict &body body)
  `(with-slots (active-program) ,dict
     (with-slots (uniforms) active-program
       (let ((,var (gethash ,name uniforms)))
         ,@body))))

(declaim (inline uniformi uniformf uniformfv uniform-matrix))
(defun uniformi (dict name x &optional y z w)
  "Set the value for uniform with name `NAME` in the
active program (set by sdk2.kit:use-program)."
  (with-uniform-location (u name) dict
    (cond
      (w (%gl:uniform-4i u x y z w))
      (z (%gl:uniform-3i u x y z))
      (y (%gl:uniform-2i u x y))
      (x (%gl:uniform-1i u x)))))

(defun uniformf (dict name x &optional y z w)
  "Set the value for uniform with name `NAME` in the
active program (set by sdk2.kit:use-program)."
  (with-uniform-location (u name) dict
    (cond
      (w (%gl:uniform-4f u x y z w))
      (z (%gl:uniform-3f u x y z))
      (y (%gl:uniform-2f u x y))
      (x (%gl:uniform-1f u x)))))

(defun uniformfv (dict name a)
  (with-uniform-location (u name) dict
    (gl:uniformfv u a)))

(defun uniform-matrix (dict name dim matrices &optional (transpose t))
  (with-uniform-location (u name) dict
    (gl:uniform-matrix u dim matrices transpose)))

(defparameter *shaders-library*
  `((:terrain
     (:uniforms
      ;; fragment
      :color-border
      :height-texture-thrs
      :pick-color
      :fog-density
      :time
      :texture-terrain-level-1
      :texture-terrain-level-2
      :texture-terrain-level-3
      :texture-terrain-rock-level-1
      :texture-terrain-rock-level-2
      :texture-muddy-soil-decal
      :texture-roads-decal
      :texture-building-decal
      :scale-building-text-coord
      :scale-road-text-coord
      :scale-soil-text-coord
      :decals-weights
      :light-pos
      :ia
      :id
      :is
      :ka
      :kd
      :ks
      :shine
      ;; vertex
      :terrain-size
      :clip-plane
      :modelview-matrix
      :proj-matrix)
     (:shaders :vertex-shader   ,(get-shader-source "ads.vert")
               :vertex-shader   ,(get-shader-source "terrain.vert")
               :fragment-shader ,(get-shader-source "terrain.frag")))
    (:water
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :time
                :wave-ampl
                :wave-freq
                :modelview-matrix
                :proj-matrix
                :proj-texture-matrix
                :texture-object)
     (:shaders :vertex-shader   ,(get-shader-source "ads.vert")
               :vertex-shader   ,(get-shader-source "water.vert")
               :fragment-shader ,(get-shader-source "water.frag")))
    (:water-no-texture
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :time
                :wave-ampl
                :wave-freq
                :modelview-matrix
                :proj-matrix
                :proj-texture-matrix)
     (:shaders :vertex-shader   ,(get-shader-source "ads.vert")
               :vertex-shader   ,(get-shader-source "water-no-texture.vert")
               :fragment-shader ,(get-shader-source "water-no-texture.frag")))
    (:tree
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :time
                :fog-density
                :model-matrix
                :modelview-matrix
                :proj-matrix
                :texture-object)
     (:shaders :vertex-shader   ,(get-shader-source "ads.vert")
               :vertex-shader   ,(get-shader-source "tree.vert")
               :fragment-shader ,(get-shader-source "tree.frag")))
    (:mesh-bump
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :time
                :fog-density
                :model-matrix
                :model-matrix
                :modelview-matrix
                :proj-matrix
                :texture-object
                :normal-map)
     (:shaders :vertex-shader   ,(get-shader-source "bump.vert")
               :vertex-shader   ,(get-shader-source "mesh-bump.vert")
               :fragment-shader ,(get-shader-source "mesh-bump.frag")))
    (:mesh-bump-inst
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :time
                :fog-density
                :model-matrix
                :model-matrix
                :modelview-matrix
                :proj-matrix
                :texture-object
                :normal-map)
     (:shaders :vertex-shader   ,(get-shader-source "bump.vert")
               :vertex-shader   ,(get-shader-source "mesh-bump-inst.vert")
               :fragment-shader ,(get-shader-source "mesh-bump.frag")))
    (:building-floor-bump
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :fog-density
                :pick-color
                :modelview-matrix
                :proj-matrix
                :texture-object
                :normal-map
                :scale-text-coord)
     (:shaders :vertex-shader   ,(get-shader-source "bump.vert")
               :vertex-shader   ,(get-shader-source "building-floor-bump.vert")
               :fragment-shader ,(get-shader-source "building-floor-bump.frag")))
    (:building-floor-ads
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :fog-density
                :pick-color
                :modelview-matrix
                :proj-matrix
                :texture-object
                :scale-text-coord)
     (:shaders :vertex-shader   ,(get-shader-source "ads.vert")
               :vertex-shader   ,(get-shader-source "building-floor-ads.vert")
               :fragment-shader ,(get-shader-source "building-floor-ads.frag")))
    (:mesh-ads
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :modelview-matrix
                :model-matrix
                :time
                :fog-density
                :proj-matrix
                :texture-object
                :scale-text-coord)
     (:shaders :vertex-shader   ,(get-shader-source "ads.vert")
               :vertex-shader   ,(get-shader-source "mesh-ads.vert")
               :fragment-shader ,(get-shader-source "mesh-ads.frag")))
    (:mesh-ads-inst
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :modelview-matrix
                :model-matrix
                :time
                :fog-density
                :proj-matrix
                :texture-object
                :scale-text-coord)
     (:shaders :vertex-shader   ,(get-shader-source "ads.vert")
               :vertex-shader   ,(get-shader-source "mesh-ads-inst.vert")
               :fragment-shader ,(get-shader-source "mesh-ads.frag")))
    (:mesh-debug
     (:uniforms :out-color
                :modelview-matrix
                :proj-matrix)
     (:shaders :vertex-shader   ,(get-shader-source "mesh-debug.vert")
               :fragment-shader ,(get-shader-source "mesh-debug.frag")))
    (:md2-ads
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :modelview-matrix
                :model-matrix
                :keyframe-interpolation
                :time
                :fog-density
                :proj-matrix
                :texture-object
                :scale-text-coord)
     (:shaders :vertex-shader   ,(get-shader-source "ads.vert")
               :vertex-shader   ,(get-shader-source "md2-ads.vert")
               :fragment-shader ,(get-shader-source "mesh-ads.frag")))
    (:md2-bump
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :time
                :keyframe-interpolation
                :fog-density
                :model-matrix
                :model-matrix
                :modelview-matrix
                :proj-matrix
                :texture-object
                :normal-map)
     (:shaders :vertex-shader   ,(get-shader-source "bump.vert")
               :vertex-shader   ,(get-shader-source "md2-bump.vert")
               :fragment-shader ,(get-shader-source "mesh-bump.frag")))
    (:wall-decorated
     (:uniforms :light-pos
                :ia
                :id
                :is
                :ka
                :kd
                :ks
                :shine
                :fog-density
                :modelview-matrix
                :model-matrix
                :time
                :proj-matrix
                :proj-texture-matrix
                :texture-object
                :texture-projector)
     (:shaders :vertex-shader   ,(get-shader-source "ads.vert")
               :vertex-shader   ,(get-shader-source "wall-decorated.vert")
               :fragment-shader ,(get-shader-source "wall-decorated.frag")))
    (:skydome
     (:uniforms :modelview-matrix
                :proj-matrix
                :proj-texture-matrix
                :texture-object
                :texture-clouds-1
                :texture-clouds-2
                :texture-clouds-3
                :texture-smoke
                :traslation-clouds-speed
                :weather-type
                :sky-color
                :ia
                :ka)
     (:shaders :vertex-shader   ,(get-shader-source "skydome.vert")
               :fragment-shader ,(get-shader-source "skydome.frag")))
    (:gui
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :ia)
     (:shaders :vertex-shader   ,(get-shader-source "gui.vert")
               :fragment-shader ,(get-shader-source "gui.frag")))
    (:gui-splash-progress
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :progress)
     (:shaders :vertex-shader   ,(get-shader-source "gui.vert")
               :fragment-shader ,(get-shader-source "splash-progress-gauge.frag")))
    (:gui-fonts
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :mult-color)
     (:shaders :vertex-shader   ,(get-shader-source "gui.vert")
               :fragment-shader ,(get-shader-source "gui-fonts.frag")))
    (:gui-naked-button
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :texture-overlay
                :ia)
     (:shaders :vertex-shader   ,(get-shader-source "gui.vert")
               :fragment-shader ,(get-shader-source "gui-naked-button.frag")))
    (:gui-animated-icon
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :texture-window-width
                :frame-idx)
     (:shaders :vertex-shader   ,(get-shader-source "animated-icon.vert")
               :fragment-shader ,(get-shader-source "animated-icon.frag")))
    (:fade
     (:uniforms :modelview-matrix
                :proj-matrix
                :fade-color
                :texture-object
                :alpha
                :time)
     (:shaders :vertex-shader   ,(get-shader-source "gui.vert")
               :fragment-shader ,(get-shader-source "fade.frag")))
    (:fade-flash
     (:uniforms :modelview-matrix
                :proj-matrix
                :fade-color
                :texture-object
                :alpha
                :time)
     (:shaders :vertex-shader   ,(get-shader-source "gui.vert")
               :fragment-shader ,(get-shader-source "fade-flash.frag")))
    (:fade-lava
     (:uniforms :modelview-matrix
                :proj-matrix
                :fade-color
                :texture-object
                :alpha
                :time)
     (:shaders :vertex-shader   ,(get-shader-source "gui.vert")
               :fragment-shader ,(get-shader-source "fade-lava.frag")))
    (:spark
     (:uniforms :modelview-matrix
                :proj-matrix
                :fade-color
                :texture-object
                :alpha
                :time)
     (:shaders :vertex-shader   ,(get-shader-source "gui.vert")
               :fragment-shader ,(get-shader-source "spark.frag")))
    (:tree-impostor
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :time)
     (:shaders :vertex-shader   ,(get-shader-source "tree-impostor.vert")
               :fragment-shader ,(get-shader-source "tree-impostor.frag")))

    (:tooltip
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :post-scaling
                :vert-displacement-speed
                :duration
                :mult-color
                :gravity
                :time)
      (:shaders :vertex-shader   ,(get-shader-source "tooltip.vert")
                :fragment-shader ,(get-shader-source "tooltip.frag")))
    (:animated-billboard
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :post-scaling
                :vert-displacement-speed
                :duration
                :texture-window-width
                :frame-idx
                :gravity
                :time)
     (:shaders :vertex-shader   ,(get-shader-source "animated-billboard.vert")
               :fragment-shader ,(get-shader-source "animated-billboard.frag")))
    (:status-orb
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :post-scaling
                :texture-window-width
                :frame-idx)
     (:shaders :vertex-shader   ,(get-shader-source "status-orb.vert")
               :fragment-shader ,(get-shader-source "status-orb.frag")))
    (:closing-curtain
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :alpha)
     (:shaders :vertex-shader   ,(get-shader-source "gui.vert")
               :fragment-shader ,(get-shader-source "closing-curtain.frag")))
    (:particles-blood
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :time)
      (:shaders :vertex-shader   ,(get-shader-source "generic-particle.vert")
                :fragment-shader ,(get-shader-source "particle-blood.frag")))
    (:particles-fire-dart
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :time)
      (:shaders :vertex-shader   ,(get-shader-source "generic-particle.vert")
                :fragment-shader ,(get-shader-source "particle-fire-dart.frag")))
    (:particles-spell-decals
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :time)
      (:shaders :vertex-shader   ,(get-shader-source "spell-decal.vert")
                :fragment-shader ,(get-shader-source "spell-decal.frag")))
    (:particles-aerial-explosion
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :time)
      (:shaders :vertex-shader   ,(get-shader-source "generic-particle.vert")
                :fragment-shader ,(get-shader-source "aerial-explosion.frag")))
    (:particles-up-spark
     (:uniforms :modelview-matrix
                :proj-matrix
                :texture-object
                :time)
      (:shaders :vertex-shader   ,(get-shader-source "generic-particle-cyl.vert")
                :fragment-shader ,(get-shader-source "aerial-explosion.frag")))
    ;;;;; transform feedback
    (:array-lerp
     (:uniforms :w)
     (:feedback-lerp-shaders :vertex-shader
                             ,(get-shader-source "lerp-feedback.vert")))
    (:blood-integrator
     (:uniforms :dt
                :noise-scale
                :gravity
                :min-y)
     (:feedback-particles-shaders :vertex-shader
                                  ,(get-shader-source "particle-blood-feedback.vert")))
    (:fire-dart-integrator
     (:uniforms :dt
                :gravity)
     (:feedback-particles-shaders :vertex-shader
                                  ,(get-shader-source "particle-fire-dart-feedback.vert")))))

(defun compile-library ()
  (let ((*error-output* (make-string-output-stream)))
    (handler-case
        (compile-shader-dictionary *shaders-library*)
      (error ()
        (progn
          (misc:dbg "error compiling shaders ~a" (get-output-stream-string *error-output*))
          nil)))))
