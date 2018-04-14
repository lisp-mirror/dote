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

(in-package :load-save-window)

(define-constant +saving-game-screenshot-size+        128.0             :test #'=)

(defun load-save-button-w ()
  (square-button-size *reference-sizes*))

(defun load-save-button-h ()
  (d/ (load-save-button-w) 2.0))

(defun load-save-window-w ()
  (let ((frame (d+ (load-save-button-w)
                   (preview-size)
                   (d* 4.0 (spacing *reference-sizes*)))))
    (adjust-window-w frame)))

(defun load-save-window-h ()
  (let ((frame (d+ (d* (load-save-button-h) 5.0))))
    (adjust-window-h frame)))

(defun preview-size ()
  (d* (square-button-size *reference-sizes*) 2.0))

(defun make-preview-config (x y texture-name)
  (make-instance 'signalling-light
                 :width         (preview-size)
                 :height        (preview-size)
                 :x             x
                 :y             y
                 :texture-name  texture-name
                 :button-status t))

(defun update-window-appereance (window file)
  (with-accessors ((s-preview  s-preview)
                   (res-action res-action)) window
    (let ((new-preview (pixmap:slurp-pixmap 'pixmap:tga file))
          (texture     (texture:get-texture +load-save-preview-texture-name+)))
      (setf (pixmap:data texture) (pixmap:data new-preview))
      (pixmap:sync-data-to-bits texture)
      (update-for-rendering texture))))

(defun update-window-for-load (button slot)
  (with-parent-widget (win) button
    (let ((file (res:get-resource-file +save-game-screenshot-name+
                                       slot
                                       :if-does-not-exists nil)))

      (if (not file)
          (append-error-box-to-window  win (_ "No game is saved here"))
          (progn
            (setf (res-action win) slot)
            (update-window-appereance win file))))))

(defun update-window-for-save (button slot)
  (with-parent-widget (win) button
    (let ((file (res:get-resource-file +save-game-screenshot-name+
                                       slot
                                       :if-does-not-exists nil)))
      (setf (res-action win) slot)
      (when file
        (update-window-appereance win file)))))

(defun %update-cb-for-load (w e slot)
  (declare (ignore e))
  (update-window-for-load w slot))

(defun %update-cb-for-save (w e slot)
  (declare (ignore e))
  (update-window-for-save w slot))

(defun update-cb-for-load-clsr (slot)
  #'(lambda (w e)
      (%update-cb-for-load w e slot)))

(defun update-cb-for-save-clsr (slot)
  #'(lambda (w e)
      (%update-cb-for-save w e slot)))

(defun load-game-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (with-accessors ((state      state)
                     (res-action res-action)) win
      (when res-action
        (let ((render-window (game-state:fetch-render-window state)))
          (saved-game:load-game render-window res-action))))))

(defun write-screenshot-for-saving (world filename)
  (let* ((pixmap (cl-gl-utils:with-render-to-pixmap (*window-w* *window-h*)
                   (interfaces:render world world))))
    (pixmap:ncopy-matrix-into-pixmap pixmap
                                     (matrix:scale-matrix pixmap
                                                          (d (/ +saving-game-screenshot-size+
                                                                *window-w*))
                                                          (d (/  +saving-game-screenshot-size+
                                                                 *window-h*))))
    (pixmap:save-pixmap pixmap filename)))

(defun save-game-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (with-accessors ((state      state)
                     (res-action res-action)) win
      (game-state:with-world (world state)
        (when res-action
          (saved-game:save-game res-action state)
          (let* ((success-message (make-message-box (format nil
                                                            (_ "Game saved in ~a")
                                                            (join-with-srings res-action "/"))
                                                    (_ "Success")
                                                    :info
                                                    (cons (_ "OK")
                                                          #'hide-and-remove-parent-cb))))
            (setf (compiled-shaders success-message) (compiled-shaders w))
            ;; save a screenshot
            (let ((image-file (res:get-resource-file +save-game-screenshot-name+
                                                     res-action
                                                     :if-does-not-exists :create)))
              (write-screenshot-for-saving world image-file))
            (add-child win success-message)))))))

(defclass load-save-window (window)
  ((res-action
    :initform nil
    :initarg  :res-action
    :accessor res-action)
   (s-preview
    :initform (make-preview-config (d+ (load-save-button-w)
                                       (spacing *reference-sizes*))
                                   0.0
                                   +load-save-preview-texture-name+)
    :initarg  :s-preview
    :accessor s-preview)
   (b-1
    :initform (make-instance 'button
                             :width    (load-save-button-w)
                             :height   (load-save-button-h)
                             :x        0.0
                             :y        0.0
                             :callback (update-cb-for-load-clsr +save-game-dir-1+)
                             :label    (_ "Slot 1"))
    :initarg :b-1
    :accessor b-1)
    (b-2
    :initform (make-instance 'button
                             :width    (load-save-button-w)
                             :height   (load-save-button-h)
                             :x        0.0
                             :y        (d+ (load-save-button-h)
                                           (spacing *reference-sizes*))
                             :callback (update-cb-for-load-clsr +save-game-dir-2+)
                             :label    (_ "Slot 2"))
    :initarg :b-2
    :accessor b-2)
   (b-3
    :initform (make-instance 'button
                             :width    (load-save-button-w)
                             :height   (load-save-button-h)
                             :x        0.0
                             :y        (d+ (d* 2.0 (load-save-button-h))
                                           (spacing *reference-sizes*))
                             :callback (update-cb-for-load-clsr +save-game-dir-3+)
                             :label    (_ "Slot 3"))
    :initarg :b-3
    :accessor b-3)
   (b-action
    :initform (make-instance 'button
                             :width    (load-save-button-w)
                             :height   (load-save-button-h)
                             :x        0.0
                             :y        (d+ (d* 4.0 (load-save-button-h))
                                           (spacing *reference-sizes*))
                             :callback #'load-game-cb
                             :label    (_ "Load"))
    :initarg :b-action
    :accessor b-action)))

(defmethod initialize-instance :after ((object load-save-window)
                                       &key (action :load)  &allow-other-keys)
  (with-accessors ((s-preview s-preview)
                   (b-1       b-1)
                   (b-2       b-2)
                   (b-3       b-3)
                   (b-action  b-action)) object
    (add-child object s-preview)
    (add-child object b-1)
    (add-child object b-2)
    (add-child object b-3)
    (add-child object b-action)
    (when (not (eq action :load))
      (setf (label object)   (_ "Save game"))
      (setf (label b-action) (_ "Save"))
      (setf (callback b-action) #'save-game-cb)
      (setf (callback b-1) (update-cb-for-save-clsr +save-game-dir-1+))
      (setf (callback b-2) (update-cb-for-save-clsr +save-game-dir-2+))
      (setf (callback b-3) (update-cb-for-save-clsr +save-game-dir-3+)))))

(defun make-window (compiled-shaders action)
  (let ((w (make-instance 'load-save-window
                          :action action
                          :x      50.0
                          :y      (d (- *window-h* (load-save-window-h)))
                          :width  (load-save-window-w)
                          :height (load-save-window-h)
                          :label  (_ "Load game"))))
    (add-window-button-cb-hide-remove w)
    (setf (compiled-shaders w) compiled-shaders)
    w))