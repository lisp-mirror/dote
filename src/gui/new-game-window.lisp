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

(in-package :new-game-window)

(defclass map-button (naked-button) ())

(defun make-new-game-window-square-button (overlay x y callback)
  (make-instance 'naked-button
                 :x               x
                 :y               y
                 :width           (%square-button-size)
                 :height          (%square-button-size)
                 :texture-object  (get-texture +square-button-texture-name+)
                 :texture-pressed (get-texture +square-button-pressed-texture-name+)
                 :texture-overlay (get-texture overlay)
                 :callback        callback))

(defun %square-button-size ()
  (small-square-button-size *reference-sizes*))

(defun new-game-window-w ()
  (adjust-window-w  (d/ (d *window-w*) 2.0)))

(defun new-game-window-h ()
  (adjust-window-h  (d/ (d *window-h*) 2.0)))

(defun new-game-win-button-list-h ()
  (d* (new-game-win-button-list-w)
      .33))

(defun new-game-win-button-list-w ()
  (d* 1.5
      (square-button-size *reference-sizes*)))

(defun new-game-win-button-h ()
  (square-button-size *reference-sizes*))

(defun new-game-win-button-w ()
  (small-square-button-size *reference-sizes*))

(defun new-game-win-button-size ()
  (small-square-button-size *reference-sizes*))

(defun new-game-win-ok/close-y ()
  (d- (new-game-window-h)
      (%square-button-size)))

(defun make-preview-config (x y texture-name)
  (make-instance 'signalling-light
                 :width         50.0
                 :height        50.0
                 :x             x
                 :y             y
                 :texture-name  texture-name
                 :button-status t))


(defclass new-game-window (window)
  ((buttons-start
    :initform 0
    :initarg  :buttons-start
    :accessor buttons-start)
   (buttons-slice-size
    :initform 5
    :initarg  :buttons-slice-size
    :accessor buttons-slice-size)
   (map-file
    :initform nil
    :initarg  :map-file
    :accessor map-file)
   (b-scroll-up
    :initform (make-new-game-window-square-button +up-overlay-texture-name+
                                                  0.0
                                                  (d- (new-game-window-h)
                                                      (%square-button-size))
                                                  #'scroll-up-cb)
    :initarg  :b-scroll-up
    :accessor b-scroll-up)
   (b-scroll-down
    :initform (make-new-game-window-square-button +down-overlay-texture-name+
                                                  (%square-button-size)
                                                  (d- (new-game-window-h)
                                                      (%square-button-size))
                                                  #'scroll-down-cb)
    :initarg  :b-scroll-down
    :accessor b-scroll-down)
   (b-ok
    :initform (make-instance 'naked-button
                             :x                   (d* 2.0
                                                      (%square-button-size))
                             :y                   (d- (new-game-window-h)
                                                      (new-game-win-button-size))
                             :width               (new-game-win-button-size)
                             :height              (new-game-win-button-size)
                             :texture-object      (get-texture +square-button-texture-name+)
                             :texture-pressed     (get-texture +square-button-pressed-texture-name+)
                             :texture-overlay     (get-texture +button-ok-texture-name+)
                             :callback            #'load-game-cb)
    :initarg  :b-ok
    :accessor b-ok)
   (text-notes
    :initform (make-instance 'widget:static-text
                             :height          (d* (new-game-window-h) 0.9)
                             :width           (d- (new-game-window-w)
                                                  (new-game-win-button-list-w))
                             :x               (new-game-win-button-list-w)
                             :y               0.0
                             :label-font-size (h3-font-size *reference-sizes*)
                             :label           "notes"
                             :justified       t)
    :initarg  :text-notes
    :accessor text-notes)))

(defmethod initialize-instance :after ((object new-game-window) &key &allow-other-keys)
  (with-accessors ((b-ok          b-ok)
                   (b-scroll-down b-scroll-down)
                   (b-scroll-up   b-scroll-up)
                   (input-path    input-path)
                   (text-notes    text-notes)) object
    (regenerate-map-buttons object)
    (add-children* object
                   text-notes
                   b-ok
                   b-scroll-up
                   b-scroll-down)))

(defgeneric regenerate-map-buttons (object))

(defun remove-map-buttons (chooser)
  (setf (children chooser)
        (remove-if #'(lambda (c) (typep c 'map-button))
                   (children chooser))))

(defun all-maps-filenames ()
  (labels ((strip (a)
             (strip-dirs-from-path (pathname->namestring a)))
           (eq-filename (a b)
             (string= (strip a)
                      (strip b))))
    (shellsort (mapcar #'(lambda (a) (strip a))
                       (res:get-resource-files-merge +maps-resource+
                                                     #'eq-filename))
               #'string<)))

(defun make-list-button-callback (new-game-window)
  #'(lambda (w e)
      (declare (ignore e))
      (with-accessors ((buttons-start      buttons-start)
                       (buttons-slice-size buttons-slice-size)
                       (map-file           map-file)
                       (text-notes         text-notes)) new-game-window
        (let ((file (res:get-resource-file (label w)
                                           +maps-resource+))
              (level-config:*wants-complete-parsing* nil))
          (load file :verbose nil :print nil)
          (if (and level-config:*level-name*
                   level-config:*level-notes*)
              (progn
                (setf map-file (label w))
                (setf (label text-notes)
                      (text-utils:join-with-strings* (text-utils:strcat
                                                      +gui-static-text-delim+
                                                      +gui-static-text-delim+)
                                                     level-config:*level-name*
                                                     level-config:*level-notes*))
                (level-config:clean-global-vars))
              (setf (label text-notes) (_ "File corrupted")))))))

(defmethod regenerate-map-buttons ((object new-game-window))
  (with-accessors ((buttons-start      buttons-start)
                   (buttons-slice-size buttons-slice-size)
                   (map-file           map-file)
                   (text-notes         text-notes)) object
    (remove-map-buttons object)
    (let ((all-maps (all-maps-filenames)))
      (loop
         for label in (safe-subseq all-maps
                                   buttons-start
                                   (+ buttons-start buttons-slice-size))
         for y from 0.0 by (new-game-win-button-list-h) do
           (let ((button (make-instance 'button
                                        :use-label-global-style nil
                                        :label-font-size        (h4-font-size *reference-sizes*)
                                        :height                 (new-game-win-button-list-h)
                                        :width                  (new-game-win-button-list-w)
                                        :x                      0.0
                                        :y                      y
                                        :compiled-shaders       (compiled-shaders object)
                                        :label                  label)))
             (setf (callback button) (make-list-button-callback object))
             (add-child object button))))))

(defun scroll (win shift-fn)
  (with-accessors ((buttons-start      buttons-start)
                   (buttons-slice-size buttons-slice-size)) win
    (setf buttons-start      (funcall shift-fn buttons-start))
    (regenerate-map-buttons win)))

(defun scroll-down-cb (button event)
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((buttons-start      buttons-start)
                     (buttons-slice-size buttons-slice-size)) win
      (let ((all-maps (all-maps-filenames)))
        (when (< (+ buttons-start buttons-slice-size)
                 (length all-maps))
          (scroll win #'(lambda (a) (1+ a))))))))

(defun scroll-up-cb (button event)
  (declare (ignore event))
  (declare (ignore event))
  (with-parent-widget (win) button
    (with-accessors ((buttons-start buttons-start)) win
      (when (> buttons-start 0)
        (scroll win #'(lambda (a) (- a 1)))))))

(defun load-game-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (with-accessors ((state    state)
                     (map-file map-file)) win
      (when  map-file
        (let ((render-window (game-state:fetch-render-window state)))

          (saved-game::prepare-for-map-loading render-window)
          (saved-game:load-map render-window map-file)
          (saved-game:init-new-map render-window))))))

(defun make-window (compiled-shaders)
  (let ((win (make-instance 'new-game-window
                            :x                (d- (d/ (d *window-w*) 2.0)
                                                  (d/ (new-game-window-w) 2.0))
                            :y                (d- (d/ (d *window-h*) 2.0)
                                                  (d/ (new-game-window-h) 2.0))
                            :width            (new-game-window-w)
                            :height           (new-game-window-h)
                            :label            (_ "Choose a map"))))
    (setf (compiled-shaders win) compiled-shaders)
    win))
