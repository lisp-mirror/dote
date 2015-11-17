(in-package :sdl2.kit-utils)

(defun fetch-window (window-id)
  (window-from-id window-id))

(defun fetch-window-id (w)
  (sdl-window-id w))
