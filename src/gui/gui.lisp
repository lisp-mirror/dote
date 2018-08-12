;; Dawn of the era: a tactical game.
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

(in-package :gui)

(defparameter *fonts-db* (make-hash-table :test 'eql))

(alexandria:define-constant +ascii-size+                128 :test #'=)

(alexandria:define-constant +font-file-extension+    ".tga" :test #'string=)

(alexandria:define-constant +default-font+        "default" :test #'string=)

(alexandria:define-constant +default-font-handle+         0 :test #'=)

(alexandria:define-constant +tooltip-font+        "tooltip" :test #'string=)

(alexandria:define-constant +tooltip-font-handle+         1 :test #'=)

(alexandria:define-constant +char-code-backspace+         8 :test #'=)

(defun char-code-backspace-p (char)
  (= (char->code char) +char-code-backspace+))

(defun join-lines-for-static-text (lines)
  (join-with-strings lines +gui-static-text-delim+))

(defun join-lines-for-static-text* (&rest lines)
  (join-with-strings lines +gui-static-text-delim+))

(defun get-font (handle)
  (gethash handle *fonts-db*))

(defun get-char-mesh (handle char)
  (gethash char (gethash handle *fonts-db*)))

;; fonts picking
(defun char->texture (char &key (min-code-value 32) (max-code-value 126) (min-s 0.0) (max-s 1.0))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (fixnum min-code-value max-code-value))
  (let* ((code       (d  (alexandria:clamp (char-code char) min-code-value max-code-value)))
         (delta-code (d  (f- (1+ max-code-value) min-code-value)))
         (incs       (d/ (d- max-s min-s) delta-code))
         (s          (d* incs (d- (d code) (d min-code-value)))))
    (declare (desired-type code s))
    ;;  - c +-------+ d
    ;;  |   |\      |     c has coordinates (s, 1.0)
    ;;  |   | \ T2  |
    ;;h |   |  \    |
    ;;  |   |   \   |
    ;;  |   |    \  |      ^ t
    ;;  |   | T1  \ |      |
    ;;  |   |      \|      |    s
    ;;  -   +-------+      +---->
    ;;      a        b
    (values (vec2 s           0.0) (vec2 (d+ s incs) 0.0)
            (vec2 (d+ s incs) 1.0) (vec2 s           1.0))))

(definline gui-printable-p (char &key
                               (min-code-value 32)
                               (max-code-value 126))
  (<= min-code-value (char->code char) max-code-value))

(defun char->quad-texture (char &key
                                  (min-code-value 32)
                                  (max-code-value 126)
                                  (min-s 0.0)
                                  (max-s 1.0))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  ;;  - c +-------+ d
  ;;  |   |\      |     c has coordinates (sc, tc)
  ;;  |   | \ T2  |
  ;;h |   |  \    |
  ;;  |   |   \   |
  ;;  |   |    \  |      ^ y
  ;;  |   | T1  \ |      |
  ;;  |   |      \|      |    x
  ;;  -   +-------+      +---->
  ;;      a        b
  (multiple-value-bind (a b d c)
      (char->texture char
                     :min-code-value min-code-value
                     :max-code-value max-code-value
                     :min-s min-s
                     :max-s max-s)
    (let* ((res  (make-array-frame 6 (vec2:vec2 0.0 0.0) 'vec2:vec2 t)))
      (declare ((simple-array vec2:vec2 (6)) res))
      (setf (elt res 0) a
            (elt res 1) b
            (elt res 2) c
            (elt res 3) b
            (elt res 4) d
            (elt res 5) c)
      res)))

(defun load-font (font-file handle shaders-dictionary)
  (let ((font-texture (get-texture (res:get-resource-file (strcat font-file
                                                                  +font-file-extension+)
                                                          constants:+fonts-resource+
                                                          :if-does-not-exists :error)))
        (map          (make-hash-table :test 'eql)))
    (setf (texture:interpolation-type font-texture) :nearest)
    (setf (texture:s-wrap-mode  font-texture) :clamp-to-border)
    (setf (texture:t-wrap-mode  font-texture) :clamp-to-border)
    (setf (texture:border-color font-texture) §c00000000)
    (prepare-for-rendering font-texture)
    (loop for i from 0 below +ascii-size+ do
         (let ((texture-coords (char->quad-texture (code-char i)))
               (mesh  (make-instance 'triangle-mesh)))
           (setf (interfaces:compiled-shaders mesh) shaders-dictionary)
           (setf (texture-object mesh) font-texture)
           (quad-w-explicit-texture-coords mesh
                                           1.0 1.0
                                           texture-coords
                                           +zero-vec+ nil t)
           (remove-orphaned-vertices mesh)
           (prepare-for-rendering mesh)
           (setf (gethash (code-char i) map) mesh)))
    (setf (gethash handle *fonts-db*) map)))

(defun clean-font-db ()
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (maphash #'(lambda (k2 v2)
                            (declare (ignore k2))
                            (interfaces:destroy v2))
                        v))
           *fonts-db*)
  (maphash #'(lambda (k v) (declare (ignore k)) (setf v nil)) *fonts-db*)
  (setf *fonts-db* nil)
  (setf *fonts-db* (make-hash-table :test 'eql)))

(defmacro gen-simple-tex-setup (function-name texture-handle parameter file
                               resource-name
                               &rest texture-setup)
  (let ((fn-name (alexandria:format-symbol t "~:@(setup-tex-~a~)" function-name)))
    `(progn
       (defparameter ,parameter (or (res:get-resource-file ,file ,resource-name
                                                           :if-does-not-exists nil)
                                    (res:get-shared-resource-filename ,file ,resource-name)))
       (alexandria:define-constant ,texture-handle
           ,(string-trim '(#\+) (symbol-name texture-handle))
         :test #'string=)
       (defun ,fn-name ()
         (multiple-value-bind (bg error)
             (texture:get-texture ,parameter)
           (if (not error)
               (progn
                 ,@texture-setup
                 (texture:prepare-for-rendering bg)
                 (setf (texture:filename bg) ,texture-handle)
                 bg)
               (values nil error)))))))

(defmacro gen-texture-bulk (&rest file)
  `(progn
     ,@(loop for f in file collect
            (let* ((file-name (if (consp f)
                                  (symbol-name (first f))
                                  (symbol-name f)))
                   (res-name  (if (consp f)
                                  (second f)
                                  '+default-gui-resource+))
                   (base      (basename file-name)))
              `(gen-simple-tex-setup ,(alexandria:symbolicate base)
                                    ,(alexandria:symbolicate (wrap-with (strcat base
                                                                                "-TEXTURE-NAME")
                                                                        "+"))
                                    ,(alexandria:symbolicate (wrap-with (strcat "TEX-"
                                                                                base)
                                                                        "*"))
                                    ,(string-downcase file-name)
                                    ,res-name
                                    ,@(if (consp f)
                                          (subseq f 2)
                                          nil))))))

(gen-texture-bulk (game-over.tga
                   +default-gui-resource+)
                  (victory.tga
                   +default-gui-resource+)
                  (white.tga
                   +default-gui-resource+)
                  (elaborating-plan-spinner.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                  (portrait-unknown.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                  (preview-unknown.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear)
                   (setf (s-wrap-mode  bg) :clamp-to-border)
                   (setf (t-wrap-mode  bg) :clamp-to-border)
                   (setf (border-color bg) §c00000000))
                  (load-save-preview.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear)
                   (setf (s-wrap-mode  bg) :clamp-to-border)
                   (setf (t-wrap-mode  bg) :clamp-to-border)
                   (setf (border-color bg) §c00000000))
                  (berserk.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                  (coma.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                  (terror.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                  (poison.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                  (immune-berserk.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                  (immune-coma.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                  (immune-terror.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                  (immune-poison.tga
                   +default-gui-resource+
                   (setf (interpolation-type bg) :linear))
                   window-top-bar.tga
                   window.tga
                   button.tga
                   button-pressed.tga
                   message-16-error.tga
                   message-16-help.tga
                   message-16-info.tga
                   message-16-ok.tga
                   message-16-warning.tga
                   text-field.tga
                   text-field-focused.tga
                   text-field-overlay.tga
                   check-button.tga
                   square-button.tga
                   square-button-pressed.tga
                   inventory-slot.tga
                   inventory-slot-selected.tga
                   transparent.tga
                   silhouette.tga
                   (drop-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (bag.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (add-to-bag.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (up-arrow-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (down-arrow-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (wear-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (spell-book-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (use-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (use-item-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (plus-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (minus-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (up-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (down-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (left-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (right-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (load-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (save-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (option-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (quit-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (next-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (previous-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (rotate-char-cw-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (rotate-char-ccw-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (next-turn-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (move-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (attack-short-range-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (attack-long-range-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (attack-long-range-imprecise-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (activation-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (magic-staff-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (open-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (close-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (zoom-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (unzoom-overlay.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (chest-opened.tga
                    +default-gui-resource+
                    (setf (interpolation-type bg) :linear)
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (chest-closed.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (chest-closed-locked.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (config-camera-elevation.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (config-change-selected-character.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (config-look-at.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (config-move.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000))
                   (config-rotation.tga
                    +default-gui-resource+
                    (setf (s-wrap-mode  bg) :clamp-to-border)
                    (setf (t-wrap-mode  bg) :clamp-to-border)
                    (setf (border-color bg) §c00000000)))

(gen-simple-tex-setup frame
                     +frame-texture-name+
                     *gui-frame*
                     "basic-frame.tga"
                     +default-gui-resource+)

(gen-simple-tex-setup button-cancel
                     +button-cancel-texture-name+
                     *gui-button-cancel*
                     "cancel-overlay.tga"
                     +default-gui-resource+
                     (setf (s-wrap-mode  bg) :clamp-to-border)
                     (setf (t-wrap-mode  bg) :clamp-to-border)
                     (setf (border-color bg) §c00000000))

(gen-simple-tex-setup button-ok
                     +button-ok-texture-name+
                     *gui-button-ok*
                     "ok-overlay.tga"
                     +default-gui-resource+
                     (setf (s-wrap-mode  bg) :clamp-to-border)
                     (setf (t-wrap-mode  bg) :clamp-to-border)
                     (setf (border-color bg) §c00000000))

(gen-simple-tex-setup window-close-button
                     +window-close-button-texture-name+
                     *gui-window-close-button*
                     "window-button.tga"
                     +default-gui-resource+)

(gen-simple-tex-setup window-close-button-pressed
                     +window-close-button-pressed-texture-name+
                     *gui-window-close-button-pressed*
                     "window-button-pressed.tga"
                     +default-gui-resource+)

(gen-simple-tex-setup blue-h-bar
                     +blue-h-bar+
                     *gui-blue-h-bar*
                     "blue-bar.tga"
                     +default-gui-resource+)

(gen-simple-tex-setup red-h-bar
                     +red-h-bar+
                     *gui-red-h-bar*
                     "red-bar.tga"
                     +default-gui-resource+)

(gen-simple-tex-setup green-h-bar
                     +green-h-bar+
                     *gui-green-h-bar*
                     "green-bar.tga"
                     +default-gui-resource+)

(gen-simple-tex-setup check-button-checked-green
                     +check-button-checked-green+
                     *gui-check-button-checked-green*
                     "check-button-checked-green.tga"
                     +default-gui-resource+)

(gen-simple-tex-setup check-button-overlay
                     +check-button-overlay+
                     *gui-check-button-overlay*
                     "check-button-overlay.tga"
                     +default-gui-resource+)
;; opening

(gen-texture-bulk (logo.tga
                   +default-gui-opening+
                   (setf (interpolation-type bg) :linear)
                   (setf (s-wrap-mode  bg) :clamp-to-border)
                   (setf (t-wrap-mode  bg) :clamp-to-border)
                   (setf (border-color bg) §c00000000))
                  (logo-mask.tga
                   +default-gui-opening+
                   (setf (interpolation-type bg) :nearest)
                   (setf (s-wrap-mode  bg) :repeat)
                   (setf (t-wrap-mode  bg) :repeat))
                  (bg-start.tga
                   +default-gui-opening+
                   (setf (interpolation-type bg) :linear)
                   (setf (s-wrap-mode  bg) :clamp-to-border)
                   (setf (t-wrap-mode  bg) :clamp-to-border)
                   (setf (border-color bg) §c00000000))
                  (text-1.tga
                    +default-gui-opening+
                   (setf (interpolation-type bg) :linear)
                   (setf (s-wrap-mode  bg) :clamp-to-border)
                   (setf (t-wrap-mode  bg) :clamp-to-border)
                   (setf (border-color bg) §c00000000))
                  (text-2.tga
                   +default-gui-opening+
                   (setf (interpolation-type bg) :linear)
                   (setf (s-wrap-mode  bg) :clamp-to-border)
                   (setf (t-wrap-mode  bg) :clamp-to-border)
                   (setf (border-color bg) §c00000000)))

(defun setup-gui (compiled-shaders)
  (load-font +default-font+ +default-font-handle+ compiled-shaders)
  (load-font +tooltip-font+ +tooltip-font-handle+ compiled-shaders)
  (setup-tex-game-over)
  (setup-tex-victory)
  (setup-tex-white)
  (setup-tex-frame)
  (setup-tex-window-top-bar)
  (setup-tex-window-close-button)
  (setup-tex-window-close-button-pressed)
  (setup-tex-window)
  (setup-tex-button-cancel)
  (setup-tex-button-ok)
  (setup-tex-button)
  (setup-tex-button-pressed)
  (setup-tex-message-16-error)
  (setup-tex-message-16-help)
  (setup-tex-message-16-info)
  (setup-tex-message-16-ok)
  (setup-tex-message-16-warning)
  (setup-tex-square-button)
  (setup-tex-square-button-pressed)
  (setup-tex-inventory-slot)
  (setup-tex-inventory-slot-selected)
  (setup-tex-transparent)
  (setup-tex-silhouette)
  (setup-tex-bag)
  (setup-tex-add-to-bag)
  (setup-tex-blue-h-bar)
  (setup-tex-red-h-bar)
  (setup-tex-green-h-bar)
  (setup-tex-drop-overlay)
  (setup-tex-check-button-overlay)
  (setup-tex-check-button-checked-green)
  (setup-tex-check-button)
  (setup-tex-text-field)
  (setup-tex-text-field-focused)
  (setup-tex-text-field-overlay)
  (setup-tex-save-overlay)
  (setup-tex-up-arrow-overlay)
  (setup-tex-down-arrow-overlay)
  (setup-tex-wear-overlay)
  (setup-tex-spell-book-overlay)
  (setup-tex-use-overlay)
  (setup-tex-use-item-overlay)
  (setup-tex-plus-overlay)
  (setup-tex-minus-overlay)
  (setup-tex-up-overlay)
  (setup-tex-down-overlay)
  (setup-tex-left-overlay)
  (setup-tex-right-overlay)
  (setup-tex-load-overlay)
  (setup-tex-quit-overlay)
  (setup-tex-next-overlay)
  (setup-tex-previous-overlay)
  (setup-tex-rotate-char-cw-overlay)
  (setup-tex-rotate-char-ccw-overlay)
  (setup-tex-next-turn-overlay)
  (setup-tex-move-overlay)
  (setup-tex-option-overlay)
  (setup-tex-elaborating-plan-spinner)
  (setup-tex-portrait-unknown)
  (setup-tex-preview-unknown)
  (setup-tex-load-save-preview)
  (setup-tex-berserk)
  (setup-tex-coma)
  (setup-tex-terror)
  (setup-tex-poison)
  (setup-tex-immune-berserk)
  (setup-tex-immune-coma)
  (setup-tex-immune-terror)
  (setup-tex-immune-poison)
  (setup-tex-activation-overlay)
  (setup-tex-attack-short-range-overlay)
  (setup-tex-attack-long-range-overlay)
  (setup-tex-attack-long-range-imprecise-overlay)
  (setup-tex-magic-staff-overlay)
  (setup-tex-open-overlay)
  (setup-tex-close-overlay)
  (setup-tex-zoom-overlay)
  (setup-tex-unzoom-overlay)
  (setup-tex-chest-closed)
  (setup-tex-chest-closed-locked)
  (setup-tex-chest-opened)
  (setup-tex-config-camera-elevation)
  (setup-tex-config-change-selected-character)
  (setup-tex-config-look-at)
  (setup-tex-config-move)
  (setup-tex-config-rotation)
  (setup-tex-logo)
  (setup-tex-logo-mask)
  (setup-tex-bg-start)
  (setup-tex-text-1)
  (setup-tex-text-2))
