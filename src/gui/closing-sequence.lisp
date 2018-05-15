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

(in-package :closing-sequence)

(defclass closing-curtain (signalling-light) ())

(defmethod render :after ((object closing-curtain) renderer)
  (do-children-mesh (c object)
    (render c renderer)))

(defun restart-game (widget)
  (with-accessors ((state state)) widget
    (let ((render-window (game-state:fetch-render-window state)))
      (sdl2.kit:close-window render-window)
      (main-window:main))))

(defmethod on-mouse-pressed ((object closing-curtain) event)
  (declare (ignore event))
  (restart-game object)
  t)

(defmethod on-key-pressed ((object closing-curtain) event)
  (declare (ignore event))
  (restart-game object)
  t)

(defun make-closing (world texture-name)
  (let* ((w                (d *window-w*))
         (h                (d *window-h*))
         (compiled-shaders (world:compiled-shaders world))
         (bg               (make-instance 'closing-curtain
                                          :compiled-shaders compiled-shaders
                                          :x                0.0
                                          :y                0.0
                                          :width            w
                                          :height           h
                                          :texture-name     texture-name))
         (fading           (full-screen-masks:make-fade-curtain compiled-shaders
                                                                :width            w
                                                                :height           h
                                                                :direction        :out
                                                                :speed            0.25)))
    (mtree:add-child bg fading)
    bg)) ; useless as add-child return the parent

(defun start-closing-sequence (world texture-name)
  (mtree:add-child (world:gui world)
                   (make-closing world texture-name)))

(defun start-game-over-sequence (world)
  (mtree:add-child (world:gui world)
                   (make-closing world +game-over-texture-name+)))

(defun start-victory-sequence (world)
  (mtree:add-child (world:gui world)
                   (make-closing world +victory-texture-name+)))
