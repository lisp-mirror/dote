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
  (d* 1.5 (square-button-size *reference-sizes*)))

(defun load-save-button-h ()
  (d/ (load-save-button-w) 2.0))

(defun load-save-window-w ()
  (let ((frame (d+ (load-save-button-w)
                   (preview-size)
                   (d* 4.0 (spacing *reference-sizes*)))))
    (adjust-window-w frame)))

(defun load-save-window-h ()
  (let ((frame (max (d* (load-save-button-h) 5.0)
                    (preview-size))))
    (adjust-window-h frame)))

(defun preview-size ()
  (d* (square-button-size *reference-sizes*) 3.0))

(defun notes-entry-w ()
  (preview-size))

(defun notes-entry-h ()
  (d* 0.05 (load-save-window-h)))

(defun make-preview-config (x y texture-name)
  (make-instance 'signalling-light
                 :width         (preview-size)
                 :height        (preview-size)
                 :x             x
                 :y             y
                 :texture-name  texture-name
                 :button-status t))

(defun update-window-appereance (window slot file)
  (with-accessors ((s-preview   s-preview)
                   (res-action  res-action)
                   (text-mtime  text-mtime)
                   (input-notes input-notes)) window
    (let ((texture (texture:get-texture +load-save-preview-texture-name+)))
      (if file
          (let* ((new-preview   (pixmap:slurp-pixmap 'pixmap:tga file))
                 (mtime         (fs:get-stat-mtime file))
                 (decoded-mtime (multiple-value-list (decode-universal-time mtime)))
                 (notes         (slurp-notes-file slot)))
            (setf (label input-notes) notes)
            (setf (label text-mtime)
                  (format nil
                          (_ "saved at: ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d")
                          (misc:time-year-of    decoded-mtime)
                          (misc:time-month-of   decoded-mtime)
                          (misc:time-date-of    decoded-mtime)
                          (misc:time-hour-of    decoded-mtime)
                          (misc:time-minutes-of decoded-mtime)))
            (setf (pixmap:data texture) (pixmap:data new-preview))
            (pixmap:sync-data-to-bits texture))
          (let ((aabb (ivec4:ivec4 0 0
                                   (1- (pixmap:width texture))
                                   (1- (pixmap:height texture)))))
            (pixmap:clear-to-color texture
                                   :color  (pixmap:average-color texture aabb)
                                   :sync-p nil)
            (matrix:matrix-line-norm texture
                                     (vec2:vec2 0.0 0.0)
                                     (vec2:vec2 1.0 1.0)
                                     (ubvec4:ubvec4 0 0 0 255))
            (matrix:matrix-line-norm texture
                                     (vec2:vec2 1.0 0.0)
                                     (vec2:vec2 0.0 1.0)
                                     (ubvec4:ubvec4 0 0 0 255))
            (pixmap:sync-data-to-bits texture)))
      (update-for-rendering texture))))

(defun slurp-notes-file (slot)
  (when-let* ((file  (res:get-resource-file +save-game-notes-name+
                                            slot
                                            :if-does-not-exists nil))
              (notes (fs:slurp-file file)))
    notes))

(defun update-window-for-load (button slot)
  (with-parent-widget (win) button
    (let ((file (res:get-resource-file +save-game-screenshot-name+
                                       slot
                                       :if-does-not-exists nil)))
      (if (not file)
          (append-error-box-to-window  win (_ "No game is saved here"))
          (progn
            (setf (res-action win) slot)
            (update-window-appereance win slot file))))))

(defun update-window-for-save (button slot)
  (with-parent-widget (win) button
    (let ((screenshot-file (res:get-resource-file +save-game-screenshot-name+
                                                  slot
                                                  :if-does-not-exists nil)))
      (setf (res-action win) slot)
      (update-window-appereance win slot screenshot-file))))

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
  (with-parent-widget (win) w
    (with-accessors ((state          state)
                     (res-action     res-action)
                     (players-only-p players-only-p)) win
      (when res-action
        (let* ((render-window (game-state:fetch-render-window state))
               (file-valid-p  (if players-only-p
                                  (progn
                                    (saved-game:load-players render-window res-action)
                                    (widget:hide-and-remove-parent-cb w e))
                                  (saved-game:load-game render-window res-action))))
          ;; select a player
          (when file-valid-p
            (game-state:with-world (world state)
              (with-accessors ((player-entities game-state:player-entities)) state
                (let* ((selected     (first-elt player-entities)))
                  (world:bind-entity-to-world world selected)
                  (keyboard-world-navigation:slide-to-active-player world))))))))))

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

(defun save-a-screenshot (win world action)
  (let ((image-file (res:get-resource-file +save-game-screenshot-name+
                                           action
                                           :if-does-not-exists :create)))
    (setf (shown win) nil) ; without the window ;)
    (gl:clear :color-buffer :depth-buffer)
    (interfaces:render world world)
    (world:render-gui world)
    (write-screenshot-for-saving world image-file)
    (setf (shown win) t)))

(defun save-notes (action notes)
  (let ((notes-file (res:get-resource-file +save-game-notes-name+
                                           action
                                           :if-does-not-exists :create)))
    (with-open-file (stream notes-file :direction :output :if-exists :supersede)
      (format stream "~a" notes))))

(defun save-game-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (with-accessors ((state       state)
                     (res-action  res-action)
                     (input-notes input-notes)) win
      (game-state:with-world (world state)
        (when res-action
          (saved-game:save-game res-action state)
          (let* ((success-message (make-message-box (format nil
                                                            (_ "Game saved in ~a")
                                                            (join-with-strings res-action "/"))
                                                    (_ "Success")
                                                    :info
                                                    (cons (_ "OK")
                                                          #'hide-and-remove-parent-cb))))
            (setf (compiled-shaders success-message) (compiled-shaders w))
            (save-a-screenshot win world res-action)
            (save-notes res-action (label input-notes))
            (add-child (world:gui world) success-message)))))))

(defclass load-save-window (window)
  ((players-only-p
    :initform nil
    :initarg  :players-only-p
    :accessor players-only-p)
   (res-action
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
                             :use-label-global-style nil
                             :label-font-size        (h5-font-size *reference-sizes*)
                             :width                  (load-save-button-w)
                             :height                 (load-save-button-h)
                             :x                      0.0
                             :y                      0.0
                             :callback               (update-cb-for-load-clsr +save-game-dir-1+)
                             :label                  (_ "Slot 1"))
    :initarg :b-1
    :accessor b-1)
    (b-2
     :initform (make-instance 'button
                              :use-label-global-style nil
                              :label-font-size        (h5-font-size *reference-sizes*)
                              :width                  (load-save-button-w)
                              :height                 (load-save-button-h)
                              :x                      0.0
                              :y                      (d+ (load-save-button-h)
                                                          (spacing *reference-sizes*))
                              :callback               (update-cb-for-load-clsr +save-game-dir-2+)
                              :label                  (_ "Slot 2"))
    :initarg :b-2
    :accessor b-2)
   (b-3
    :initform (make-instance 'button
                             :use-label-global-style nil
                             :label-font-size        (h5-font-size *reference-sizes*)
                             :width                  (load-save-button-w)
                             :height                 (load-save-button-h)
                             :x                      0.0
                             :y                      (d+ (d* 2.0 (load-save-button-h))
                                                        (spacing *reference-sizes*))
                             :callback               (update-cb-for-load-clsr +save-game-dir-3+)
                             :label                  (_ "Slot 3"))
    :initarg :b-3
    :accessor b-3)
   (b-action
    :initform (make-instance 'button
                             :use-label-global-style nil
                             :label-font-size        (h5-font-size *reference-sizes*)
                             :width                  (load-save-button-w)
                             :height                 (load-save-button-h)
                             :x                      0.0
                             :y                      (d- (load-save-window-h)
                                                         (load-save-button-h))
                             :callback               #'load-game-cb
                             :label                  (_ "Load"))
    :initarg :b-action
    :accessor b-action)
   (input-notes
    :initform (make-instance 'text-field
                             :width  (notes-entry-w)
                             :height (notes-entry-h)
                             :x      (d+ (load-save-button-w)
                                         (spacing *reference-sizes*))
                             :y      (add-epsilon-rel (preview-size)
                                                      0.01)
                             :label "")
    :initarg :input-notes
    :accessor input-notes)
   (text-mtime
    :initform (make-instance 'simple-label
                             :label     ""
                             :font-size (h1-font-size *reference-sizes*)
                             :width     (d- (load-save-window-w)
                                            (load-save-button-w))
                             :height    (load-save-button-h)
                             :x         (load-save-button-w)
                             :y         (d- (load-save-window-h)
                                            (load-save-button-h)))
    :initarg :text-mtime
    :accessor text-mtime)))

(defmethod initialize-instance :after ((object load-save-window)
                                       &key (action :load)  &allow-other-keys)
  (with-accessors ((s-preview s-preview)
                   (b-1         b-1)
                   (b-2         b-2)
                   (b-3         b-3)
                   (b-action    b-action)
                   (input-notes input-notes)
                   (text-mtime  text-mtime)) object
    (add-children* object
                   s-preview
                   b-1
                   b-2
                   b-3
                   b-action
                   input-notes
                   text-mtime)
    (when (not (eq action :load))
      (setf (label b-action) (_ "Save"))
      (setf (callback b-action) #'save-game-cb)
      (setf (callback b-1) (update-cb-for-save-clsr +save-game-dir-1+))
      (setf (callback b-2) (update-cb-for-save-clsr +save-game-dir-2+))
      (setf (callback b-3) (update-cb-for-save-clsr +save-game-dir-3+)))))

(defun make-window (compiled-shaders action
                    &key
                      (title        (_ "Load game"))
                      (players-only nil)
                      (x            50.0)
                      (y            (d (- *window-h* (load-save-window-h)))))
  (let ((w (make-instance 'load-save-window
                          :players-only-p players-only
                          :action         action
                          :x              x
                          :y              y
                          :width          (load-save-window-w)
                          :height         (load-save-window-h)
                          :label          title)))
    (add-window-button-cb-hide-remove w)
    (setf (compiled-shaders w) compiled-shaders)
    w))
