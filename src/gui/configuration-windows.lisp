;; Dawn of the era: a tactical game.
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

(in-package :configuration-windows)

(defclass conf-char-field (char-field) ())

(defmethod on-key-pressed ((object conf-char-field) event)
  (with-parent-widget (window) object
    (let ((all-labels (list (input-forward window)
                            (input-cw window)
                            (input-upward window)
                            (input-go-to-active window)
                            (input-left window)
                            (input-right window)
                            (input-back window)
                            (input-ccw window)
                            (input-downward window)
                            (input-reset-camera window))))
      (setf all-labels (remove object all-labels :test #'identificable:test-id=))
      (if (and (focus object)
               (gui-printable-p (char-event event))
               (not (find (char-event event) all-labels :test #'string= :key #'label)))
          (call-next-method)
          nil))))

(defun keyboard-window-w ()
  (let ((frame (d/ (d *window-w*) 2.0)))
    (adjust-window-w frame)))

(defun keyboard-window-h ()
  (let ((frame (d/ (d *window-w*) 7.0)))
    (adjust-window-h frame)))

(defun standard-text-entry-w ()
  (input-text-w *reference-sizes*))

(defun standard-text-entry-h ()
  (input-text-h *reference-sizes*))

(defun standard-label-entry-w ()
  (input-text-w *reference-sizes*))

(defun standard-label-entry-h ()
  (input-text-h *reference-sizes*))

(defun keyboard-text-entry-size ()
  (d/ (icon-size)
      2.0))

(defun make-keyboard-text-entry (x y default)
  (make-instance 'conf-char-field
                 :width  (keyboard-text-entry-size)
                 :height (keyboard-text-entry-size)
                 :x      x
                 :y      y
                 :label  default))

(defun make-standard-text-entry (x y default
                                 &key
                                   (width  (standard-text-entry-w))
                                   (height (standard-text-entry-h))
                                   (type   'text-field))
  (make-instance type
                 :width  width
                 :height height
                 :x      x
                 :y      y
                 :label  default))

(defun icon-size ()
  (d/ (keyboard-window-w) 8.5))

(defun make-icon-config (x y texture-name)
  (make-instance 'signalling-light
                 :width         (icon-size)
                 :height        (icon-size)
                 :x             x
                 :y             y
                 :texture-name  texture-name
                 :button-status t))

(defun char-field-stacked-x (index)
  (let((actual-index (d index)))
    (d+ (d* 1.5
            (keyboard-text-entry-size))
        (d* (d* 0.5 actual-index)
            (keyboard-text-entry-size))
        (d* actual-index
            (d+ (keyboard-text-entry-size)
                (icon-size))))))

(defun char-field-stacked-y ()
  (d+ (icon-size)
      (keyboard-text-entry-size)))

(defun char-field-inbetween-x (index)
  (let((actual-index (d index)))
    (d+ (d* actual-index (keyboard-text-entry-size))
        (d* actual-index (icon-size)))))

(defun char-field-inbetween-y ()
  (d* 1.25 (keyboard-text-entry-size)))

(defun keyboard-button-w ()
  (d/ (main-window-button-w) 2.0))

(defun keyboard-button-h ()
  (d/ (main-window-button-w) 5.0))

(defun save-keybindings-configuration-cb (w e)
  (declare (ignore e))
  (with-parent-widget (window) w
    (gconf:set-forward                (label (input-forward window)))
    (gconf:set-rotate-camera-cw       (label (input-cw window)))
    (gconf:set-upward                 (label (input-upward window)))
    (gconf:set-go-to-active-character (label (input-go-to-active window)))
    (gconf:set-next-character         (label (input-next window)))
    (gconf:set-left                   (label (input-left window)))
    (gconf:set-right                  (label (input-right window)))
    (gconf:set-back                   (label (input-back window)))
    (gconf:set-rotate-camera-ccw      (label (input-ccw window)))
    (gconf:set-downward               (label (input-downward window)))
    (gconf:set-reset-camera           (label (input-reset-camera window)))
    (gconf:set-prev-character         (label (input-prev window)))
    (gconf:dump)))

(defun sync-keybinding-conf->gui (window)
  (setf (label (input-forward window))       (gconf:config-forward))
  (setf (label (input-cw window))            (gconf:config-rotate-camera-cw))
  (setf (label (input-upward window))        (gconf:config-upward))
  (setf (label (input-go-to-active window))  (gconf:config-go-to-active-character))
  (setf (label (input-next window))          (gconf:config-next-character))
  (setf (label (input-left window))          (gconf:config-left))
  (setf (label (input-right window))         (gconf:config-right))
  (setf (label (input-back window))          (gconf:config-back))
  (setf (label (input-ccw window))           (gconf:config-rotate-camera-ccw))
  (setf (label (input-downward window))      (gconf:config-downward))
  (setf (label (input-reset-camera window))  (gconf:config-reset-camera))
  (setf (label (input-prev window))          (gconf:config-prev-character)))

(defun reset-keybindings-configuration-cb (w e)
  (declare (ignore e))
  (with-parent-widget (window) w
    (gconf:reset-keybindings)
    (sync-keybinding-conf->gui window)))

(defclass keyboard-window (window)
  ((s-move
    :initform (make-icon-config (keyboard-text-entry-size)
                                (keyboard-text-entry-size)
                                +config-move-texture-name+)
    :initarg  :s-move
    :accessor s-move)
   (s-rotation
    :initform (make-icon-config  (d+ (d* 2.5 (keyboard-text-entry-size))
                                     (icon-size))
                                 (keyboard-text-entry-size)
                                 +config-rotation-texture-name+)
    :initarg  :s-rotation
    :accessor s-rotation)
   (s-elevation
    :initform (make-icon-config (d+ (d* 4.0 (keyboard-text-entry-size))
                                    (d* 2.0 (icon-size)))
                                (keyboard-text-entry-size)
                                +config-camera-elevation-texture-name+)
    :initarg  :s-elevation
    :accessor s-elevation)
   (s-camera-reset
    :initform (make-icon-config (d+ (d* 5.5 (keyboard-text-entry-size))
                                    (d* 3.0 (icon-size)))
                                (keyboard-text-entry-size)
                                +config-look-at-texture-name+)
    :initarg  :s-camera-reset
    :accessor s-camera-reset)
   (s-change-character
    :initform (make-icon-config (d+ (d* 7.0 (keyboard-text-entry-size))
                                    (d* 4.0 (icon-size)))
                                (keyboard-text-entry-size)
                                +config-change-selected-character-texture-name+)
    :initarg  :s-change-character
    :accessor s-change-character)
   (input-forward
    :initform (make-keyboard-text-entry (char-field-stacked-x 0)
                                        0.0
                                        (gconf:config-forward))
    :initarg :input-forward
    :accessor input-forward)
   (input-cw
    :initform (make-keyboard-text-entry (char-field-stacked-x 1)
                                        0.0
                                        (gconf:config-rotate-camera-cw))
    :initarg :input-cw
    :accessor input-cw)
   (input-upward
    :initform (make-keyboard-text-entry (char-field-stacked-x 2)
                                        0.0
                                        (gconf:config-upward))
    :initarg :input-upward
    :accessor input-upward)
   (input-go-to-active
    :initform (make-keyboard-text-entry (char-field-stacked-x 3)
                                        0.0
                                        (gconf:config-go-to-active-character))
    :initarg :input-go-to-active
    :accessor input-go-to-active)
   (input-next
    :initform (make-keyboard-text-entry (char-field-stacked-x 4)
                                        0.0
                                        (gconf:config-next-character))
    :initarg :input-next
    :accessor input-next)
   (input-left
    :initform (make-keyboard-text-entry (char-field-inbetween-x 0)
                                        (char-field-inbetween-y)
                                        (gconf:config-left))
    :initarg :input-left
    :accessor input-left)
   (input-right
    :initform (make-keyboard-text-entry (char-field-inbetween-x 1)
                                        (char-field-inbetween-y)
                                        (gconf:config-right))
    :initarg :input-right
    :accessor input-right)
   (input-back
    :initform (make-keyboard-text-entry (char-field-stacked-x 0)
                                        (char-field-stacked-y)
                                        (gconf:config-back))
    :initarg :input-back
    :accessor input-back)
   (input-ccw
    :initform (make-keyboard-text-entry (char-field-stacked-x 1)
                                        (char-field-stacked-y)
                                        (gconf:config-rotate-camera-ccw))
    :initarg :input-ccw
    :accessor input-ccw)
   (input-downward
    :initform (make-keyboard-text-entry (char-field-stacked-x 2)
                                        (char-field-stacked-y)
                                        (gconf:config-downward))
    :initarg :input-downward
    :accessor input-downward)
   (input-reset-camera
    :initform (make-keyboard-text-entry (char-field-stacked-x 3)
                                        (char-field-stacked-y)
                                        (gconf:config-reset-camera))
    :initarg :input-reset-camera
    :accessor input-reset-camera)
   (input-prev
    :initform (make-keyboard-text-entry (char-field-stacked-x 4)
                                        (char-field-stacked-y)
                                        (gconf:config-prev-character))
    :initarg :input-prev
    :accessor input-prev)
   (b-save
    :initform (make-instance 'button
                             :width    (keyboard-button-w)
                             :height   (keyboard-button-h)
                             :x        0.0
                             :y        (d- (keyboard-window-h)
                                           (keyboard-button-h))
                             :callback #'save-keybindings-configuration-cb
                             :label    (_ "Save"))
    :initarg :b-save
    :accessor b-save)
   (b-reset
    :initform (make-instance 'button
                             :width    (keyboard-button-w)
                             :height   (keyboard-button-h)
                             :x        (d+ (keyboard-button-w)
                                           (spacing *reference-sizes*))
                             :y        (d- (keyboard-window-h)
                                           (keyboard-button-h))
                             :callback #'reset-keybindings-configuration-cb
                             :label (_ "Reset"))
    :initarg :b-reset
    :accessor b-reset)))

(defmethod initialize-instance :after ((object keyboard-window) &key &allow-other-keys)
  (with-accessors ((s-move             s-move)
                   (s-rotation         s-rotation)
                   (s-elevation        s-elevation)
                   (s-camera-reset     s-camera-reset)
                   (s-change-character s-change-character)
                   (input-forward      input-forward)
                   (input-cw           input-cw)
                   (input-upward       input-upward)
                   (input-go-to-active input-go-to-active)
                   (input-next         input-next)
                   (input-left         input-left)
                   (input-right        input-right)
                   (input-back         input-back)
                   (input-ccw          input-ccw)
                   (input-downward     input-downward)
                   (input-reset-camera input-reset-camera)
                   (input-prev         input-prev)
                   (b-save             b-save)
                   (b-reset            b-reset)) object
  (add-child object s-move)
  (add-child object s-rotation)
  (add-child object s-elevation)
  (add-child object s-camera-reset)
  (add-child object s-change-character)
  (add-child object input-forward)
  (add-child object input-cw)
  (add-child object input-upward)
  (add-child object input-go-to-active)
  (add-child object input-next)
  (add-child object input-left)
  (add-child object input-right)
  (add-child object input-back)
  (add-child object input-ccw)
  (add-child object input-downward)
  (add-child object input-reset-camera)
  (add-child object input-prev)
  (add-child object b-save)
  (add-child object b-reset)))

(defun make-keyboard-window (compiled-shaders)
  (let ((w (make-instance 'keyboard-window
                          :x      (main-window-w)
                          :y      (d (- *window-h* (keyboard-window-h)))
                          :width  (keyboard-window-w)
                          :height (keyboard-window-h)
                          :label  (_ "Keyboard Configuration"))))
    (add-window-button-cb-hide-remove w)
    (setf (compiled-shaders w) compiled-shaders)
    w))

(defun appearance-window-w ()
  (let ((frame (d* 0.33 (d *window-w*))))
    (adjust-window-w frame)))

(defun appearance-window-h ()
  (let ((frame (d* 0.025 (d *window-h*))))
    (adjust-window-h frame)))

(defun appearance-button-w ()
  (appearance-window-w))

(defun appearance-button-h ()
  (d* 0.5 (appearance-window-h)))

(defun update-config-smooth-movements-cb (button event)
  (declare (ignore event))
  (gconf:set-smooth-movements (button-state button))
  (gconf:dump))

(defclass appearance-window (window)
  ((checkb-smooth-movement
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (appearance-button-h)
                             :width              (appearance-button-w)
                             :x                  0.0
                             :y                  0.0
                             :label              (_ "Character smooth movements")
                             :callback           #'update-config-smooth-movements-cb
                             :initial-state      (gconf:config-smooth-movements)
                             :color              :green)
    :initarg  :checkb-smooth-movement
    :accessor checkb-smooth-movement)))

(defmethod initialize-instance :after ((object appearance-window) &key &allow-other-keys)
  (with-accessors ((checkb-smooth-movement checkb-smooth-movement)) object
    (add-child object checkb-smooth-movement)))

(defun make-appearance-window (compiled-shaders)
  (let ((w (make-instance 'appearance-window
                          :x      (main-window-w)
                          :y      (d (- *window-h* (appearance-window-h)))
                          :width  (appearance-window-w)
                          :height (appearance-window-h)
                          :label  (_ "Appearance Configuration"))))
    (add-window-button-cb-hide-remove w)
    (setf (compiled-shaders w) compiled-shaders)
    w))

(defun gui-window-w ()
  (let ((frame (d* 1.5 (main-window-w))))
    (adjust-window-w frame)))

(defun gui-window-h ()
  (let ((frame (d* 0.5 (main-window-h))))
    (adjust-window-w frame)))

(defun gui-button-w ()
  (d* (gui-window-w) 0.25))

(defun gui-button-h ()
  (d* 0.4 (gui-button-w)))

(defun gui-scaling-scaling-camera-label-h ()
  (d* 0.4 (gui-button-h)))

(defun gui-scaling-camera-text-entry-h ()
  (d* 0.4 (gui-button-h)))

(defclass conf-camera-scaling-field (text-field) ())

(defmethod on-key-pressed :after ((object conf-camera-scaling-field) event)
  (let ((candidate (conditions:with-default-on-error (nil)
                     (parse-number->desired (label object)))))
    (if (focus object)
        (when candidate
          (gconf:set-camera-fp-scaling-movement candidate)
          (gconf:dump))
        nil)))

(defmacro with-config-checkbutton ((button config-fn) &body body)
  `(progn
     ,@body
     (,config-fn (button-state ,button))
     (gconf:dump)))

(defun update-config-fullscreen-cb (button event)
  (declare (ignore event))
  (with-config-checkbutton (button gconf:set-fullscreen)))

(defun update-config-inhibit-bgm-cb (button event)
  (declare (ignore event))
  (with-accessors ((button-state button-state)) button
    (let ((old-config (gconf:config-inhibit-bgm)))
      (if (button-state button)
          (sound:stop-music)
          (when old-config
            (sound:play-music sound:+bg-battle-1+))))
    (with-config-checkbutton (button gconf:set-inhibit-bgm))))

(defun update-config-clip-trees-cb (button event)
  (declare (ignore event))
  (with-config-checkbutton (button gconf:set-tree-clip)
    (if (button-state button)
        (mesh:set-tree-clip)
        (mesh:unset-tree-clip))))

(defclass gui-window (window)
  ((l-scale-fp-speed
    :initform (make-instance 'widget:simple-label
                             :label     (_ "Speed for \"look around\" camera.")
                             :font-size (standard-font-size *reference-sizes*)
                             :height    (gui-scaling-scaling-camera-label-h)
                             :width     (gui-window-w)
                             :x         0.0
                             :y         0.0)

    :initarg  :l-scale-fp-speed
    :accessor l-scale-fp-speed)
   (input-camera-fp-scaling-movement
    :initform (make-standard-text-entry 0.0
                                        (add-epsilon-rel (gui-scaling-scaling-camera-label-h)
                                                         (spacing-rel *reference-sizes*))
                                        (to-s (gconf:config-camera-fp-scaling-movement))
                                        :height (gui-scaling-camera-text-entry-h)
                                        :type   'conf-camera-scaling-field)
    :initarg  :input-camera-fp-scaling-movement
    :accessor input-camera-fp-scaling-movement)
   (s-sound-volume
    :initform (make-instance 'scale-value
                             :height          (gui-scaling-camera-text-entry-h)
                             :width           (gui-window-w)
                             :x               0.0
                             :y
                             (add-epsilon-rel (d* 2.0
                                                  (gui-scaling-scaling-camera-label-h))
                                              (spacing-rel *reference-sizes*))
                             :label-font-size (h4-font-size *reference-sizes*)
                             :val             (sound:get-volume)
                             :justified       t)
    :initarg  :s-sound-volume
    :accessor s-sound-volume)
   (checkb-inhibit-bgm
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (gui-scaling-camera-text-entry-h)
                             :width              (gui-window-w)
                             :x                  0.0
                             :y
                             (add-epsilon-rel (d* 3.0
                                                  (gui-scaling-scaling-camera-label-h))
                                              (spacing-rel *reference-sizes*))
                             :label              (_ "Inhibit background music")
                             :callback           #'update-config-inhibit-bgm-cb
                             :initial-state      (gconf:config-inhibit-bgm)
                             :color              :green)
    :initarg  :checkb-inhibit-bgm
    :accessor checkb-inhibit-bgm)
   (checkb-fullscreen
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (gui-scaling-camera-text-entry-h)
                             :width              (gui-window-w)
                             :x                  0.0
                             :y
                             (add-epsilon-rel (d* 4.0
                                                  (gui-scaling-scaling-camera-label-h))
                                              (spacing-rel *reference-sizes*))
                             :label              (_ "Fullscreen (requires restart)")
                             :callback           #'update-config-fullscreen-cb
                             :initial-state      (gconf:config-fullscreen)
                             :color              :green)
    :initarg  :checkb-fullscreen
    :accessor checkb-fullscreen)
   (checkb-clip-trees
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (gui-scaling-camera-text-entry-h)
                             :width              (gui-window-w)
                             :x                  0.0
                             :y
                             (add-epsilon-rel (d* 5.0
                                                  (gui-scaling-scaling-camera-label-h))
                                              (spacing-rel *reference-sizes*))
                             :label              (_ "Clip trees")
                             :callback           #'update-config-clip-trees-cb
                             :initial-state      (gconf:config-tree-clip)
                             :color              :green)
    :initarg  :checkb-clip-trees
    :accessor checkb-clip-trees)))

(defmacro with-set-volume-and-dump-config (&body body)
  `(progn
     ,@body
      (gconf:set-sound-volume (sound:get-volume))
      (gconf:dump)
      (sound:get-volume)))

(defun decrease-volume-cb (val)
  (declare (ignore val))
  (with-set-volume-and-dump-config
    (sound:decrease-volume)))

(defun increase-volume-cb (val)
  (declare (ignore val))
  (with-set-volume-and-dump-config
    (sound:increase-volume)))

(defmethod initialize-instance :after ((object gui-window) &key &allow-other-keys)
  (with-accessors ((l-scale-fp-speed                 l-scale-fp-speed)
                   (input-camera-fp-scaling-movement input-camera-fp-scaling-movement)
                   (s-sound-volume                   s-sound-volume)
                   (checkb-inhibit-bgm               checkb-inhibit-bgm)
                   (checkb-fullscreen                checkb-fullscreen)
                   (checkb-clip-trees                checkb-clip-trees)) object
    (setf (callback-plus  s-sound-volume) (make-simple-scale-cb #'increase-volume-cb))
    (setf (callback-minus s-sound-volume) (make-simple-scale-cb #'decrease-volume-cb))
    (setf (val->string-fn s-sound-volume)
          #'(lambda (a)
              (format nil (_ "Music volume: ~a") a)))
    (sync-value-w-label s-sound-volume)
    (add-children* object
                   l-scale-fp-speed
                   input-camera-fp-scaling-movement
                   s-sound-volume
                   checkb-inhibit-bgm
                   checkb-fullscreen
                   checkb-clip-trees)))

(defun make-gui-window (compiled-shaders)
  (let ((w (make-instance 'gui-window
                          :x      (main-window-w)
                          :y      (d (- *window-h* (gui-window-h)))
                          :width  (gui-window-w)
                          :height (gui-window-h)
                          :label  (_ "GUI Configuration"))))
    (add-window-button-cb-hide-remove w)
    (setf (compiled-shaders w) compiled-shaders)
    w))

(defun debug-window-w ()
  (let ((frame (d* 0.33 (d *window-w*))))
    (adjust-window-w frame)))

(defun debug-window-h ()
  (let ((frame (d* 0.025 (d *window-h*))))
    (adjust-window-h frame)))

(defun debug-button-w ()
  (debug-window-w))

(defun debug-button-h ()
  (d* 0.5 (debug-window-h)))

(defun update-config-planner-cb (button event)
  (declare (ignore event))
  (gconf:set-inhibit-planner (button-state button))
  (gconf:dump))

(defclass debug-window (window)
  ((checkb-inhibit-planner
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (debug-button-h)
                             :width              (debug-button-w)
                             :x                  0.0
                             :y                  0.0
                             :label              (_ "Inhibit AI planner")
                             :callback           #'update-config-planner-cb
                             :initial-state      (gconf:config-inhibit-planner)
                             :color              :green)
    :initarg  :checkb-inhibit-planner
    :accessor checkb-inhibit-planner)))

(defmethod initialize-instance :after ((object debug-window) &key &allow-other-keys)
  (with-accessors ((checkb-inhibit-planner checkb-inhibit-planner)) object
    (add-child object checkb-inhibit-planner)))

(defun make-debug-window (compiled-shaders)
  (let ((w (make-instance 'debug-window
                          :x      (main-window-w)
                          :y      (d (- *window-h* (debug-window-h)))
                          :width  (debug-window-w)
                          :height (debug-window-h)
                          :label  (_ "Debug Configuration"))))
    (add-window-button-cb-hide-remove w)
    (setf (compiled-shaders w) compiled-shaders)
    w))

(defun ai-window-w ()
  (let ((frame (d* 0.33 (d *window-w*))))
    (adjust-window-w frame)))

(defun ai-window-h ()
  (let ((frame (d* 0.025 (d *window-h*))))
    (adjust-window-h frame)))

(defun ai-button-w ()
  (ai-window-w))

(defun ai-button-h ()
  (d* 0.6 (ai-window-h)))

(defun update-config-ai-training-cb (button event)
  (declare (ignore event))
  (gconf:set-train-ai (button-state button))
  (gconf:dump))

(defclass ai-window (window)
  ((checkb-train-ai
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (ai-button-h)
                             :width              (ai-button-w)
                             :x                  0.0
                             :y                  0.0
                             :label              (_ "Train computer opponent")
                             :callback           #'update-config-ai-training-cb
                             :initial-state      (gconf:config-train-ai)
                             :color              :green)
    :initarg  :checkb-train-ai
    :accessor checkb-train-ai)))

(defmethod initialize-instance :after ((object ai-window) &key &allow-other-keys)
  (with-accessors ((checkb-train-ai checkb-train-ai)) object
    (add-child object checkb-train-ai)))

(defun make-ai-window (compiled-shaders)
  (let ((w (make-instance 'ai-window
                          :x      (main-window-w)
                          :y      (d (- *window-h* (ai-window-h)))
                          :width  (ai-window-w)
                          :height (ai-window-h)
                          :label  (_ "AI Configuration"))))
    (add-window-button-cb-hide-remove w)
    (setf (compiled-shaders w) compiled-shaders)
    w))

(defun main-window-button-h ()
  (small-square-button-size *reference-sizes*))

(defun main-window-w ()
  (adjust-window-w (d/ (d *window-w*) 4.0)))

(defun main-window-h ()
  (adjust-window-h (d* 5.0
                       (main-window-button-h))))

(defun main-window-button-w ()
  (d- (main-window-w)
      (d* 2.0
          (left-frame-offset *reference-sizes*)
          (main-window-w))))

(defun %open-conf-window (widget make-fn)
  (with-root-widget (root widget)
    (add-child root
               (funcall make-fn (compiled-shaders widget)))))

(defun open-keyboard-window-cb (widget event)
  (declare (ignore event))
  (%open-conf-window widget #'make-keyboard-window))

(defun open-appearance-window-cb (widget event)
  (declare (ignore event))
  (%open-conf-window widget #'make-appearance-window))

(defun open-gui-window-cb (widget event)
  (declare (ignore event))
  (%open-conf-window widget #'make-gui-window))

(defun open-debug-window-cb (widget event)
  (declare (ignore event))
  (%open-conf-window widget #'make-debug-window))

(defun open-ai-window-cb (widget event)
  (declare (ignore event))
  (%open-conf-window widget #'make-ai-window))

(defclass main-window (window)
  ((b-keyboard
    :initform (make-instance 'button
                             :width    (main-window-button-w)
                             :height   (main-window-button-h)
                             :x        0.0
                             :y        0.0
                             :callback #'open-keyboard-window-cb
                             :label (_ "Keyboard"))
    :initarg  b-keyboard
    :accessor b-keyboard)
   (b-appearance
    :initform (make-instance 'button
                             :width    (main-window-button-w)
                             :height   (main-window-button-h)
                             :x        0.0
                             :y        (d+ (main-window-button-h)
                                           (spacing *reference-sizes*))
                             :callback #'open-appearance-window-cb
                             :label (_ "Appearance"))
    :initarg :b-appearance
    :accessor b-appearance)
   (b-gui
    :initform (make-instance 'button
                             :width    (main-window-button-w)
                             :height   (main-window-button-h)
                             :x        0.0
                             :y        (d+ (d* 2.0 (main-window-button-h))
                                           (spacing *reference-sizes*))
                             :callback #'open-gui-window-cb
                             :label (_ "GUI"))
    :initarg  b-gui
    :accessor b-gui)
   (b-ai
    :initform (make-instance 'button
                             :width    (main-window-button-w)
                             :height   (main-window-button-h)
                             :x        0.0
                             :y        (d+ (d* 3.0 (main-window-button-h))
                                           (spacing *reference-sizes*))
                             :callback #'open-ai-window-cb
                             :label (_ "Computer opponent"))
    :initarg  b-ai
    :accessor b-ai)

   #+debug-mode
   (b-debug
    :initform (make-instance 'button
                             :width    (main-window-button-w)
                             :height   (main-window-button-h)
                             :x        0.0
                             :y        (d+ (d* 4.0 (main-window-button-h))
                                           (spacing *reference-sizes*))
                             :callback #'open-debug-window-cb
                             :label (_ "Debug"))
    :initarg :b-debug
    :accessor b-debug)))

(defmethod initialize-instance :after ((object main-window) &key &allow-other-keys)
  (with-accessors ((b-keyboard   b-keyboard)
                   (b-gui        b-gui)
                   (b-ai         b-ai)
                   (b-appearance b-appearance)) object
    (add-child object b-keyboard)
    (add-child object b-gui)
    (add-child object b-ai)
    (add-child object b-appearance)
    #+debug-mode (add-child object (b-debug object))))

(defun make-main-window (compiled-shaders)
  (let ((w (make-instance 'main-window
                          :x      0.0
                          :y      (d (- *window-h* (main-window-h)))
                          :width  (main-window-w)
                          :height (main-window-h)
                          :label  (_ "Game Configuration"))))
    (add-window-button-cb-hide-remove w)
    (setf (compiled-shaders w) compiled-shaders)
    w))
