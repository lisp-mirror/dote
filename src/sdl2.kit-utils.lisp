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

(in-package :sdl2.kit-utils)

(defun fetch-window (window-id)
  (window-from-id window-id))

(defun fetch-window-id (w)
  (sdl-window-id w))

(defun clean-restart-gl-context (window)
  (with-slots (sdl2.kit:gl-context) window
    (sdl2:gl-delete-context (slot-value window 'sdl2.kit:gl-context))
    (setf (slot-value window 'sdl2.kit:gl-context)
          (sdl2:gl-create-context (sdl2.kit:sdl-window window)))))

(defun move-mouse-to-center-screen (window)
  (multiple-value-bind (w h)
      (sdl2:get-window-size (sdl-window window))
    (sdl2:warp-mouse-in-window (sdl-window window) (/ w 2) (/ h 2))))
