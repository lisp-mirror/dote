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

(in-package :keyboard-world-navigation)

(define-constant +slide-velocity-scaling+ 3.0 :test #'=)

(defun slide-camera (world displacement)
  (let* ((camera (camera world)))
    (setf (mode camera)  :drag)
    (camera:drag-camera camera displacement)))

(defun slide-forward (world)
  (slide-camera world (vec 0.0
                           0.0
                           (d* +terrain-chunk-tile-size+
                               +slide-velocity-scaling+))))

(defun slide-back (world)
  (slide-camera world (vec 0.0
                           0.0
                           (d- (d* +terrain-chunk-tile-size+
                                   +slide-velocity-scaling+)))))

(defun slide-left (world)
  (slide-camera world (vec (d* +terrain-chunk-tile-size+
                               +slide-velocity-scaling+)
                           0.0
                           0.0)))

(defun slide-right (world)
  (slide-camera world (vec (d- (d* +terrain-chunk-tile-size+
                                   +slide-velocity-scaling+))
                           0.0
                           0.0)))

(defun slide-upward (world)
  (incf (elt (entity:pos (world:camera world)) 1)
        (d* +terrain-chunk-tile-size+
            +slide-velocity-scaling+)))

(defun slide-downward (world)
  (incf (elt (entity:pos (world:camera world)) 1)
        (d- (d* +terrain-chunk-tile-size+
                +slide-velocity-scaling+))))

(defun slide-to-active-player (world)
  (world:point-camera-to-entity world
                                (widget:bound-player (toolbar world))))

(defun rotate-90-around-player (world orientation)
  (with-accessors ((selected-pc selected-pc)
                   (camera camera)) world
    (setf (camera:target camera) (entity:pos selected-pc))
    (camera:look-at* camera)
    (ecase orientation
      (:cw
       (setf (orbit-interpolator camera)
             (camera:gen-orbit-interpolator-cw camera
                                               (entity:pos selected-pc)
                                               camera:+angular-speed-rotate-command+
                                               +pi/2+)))
      (:ccw
       (setf (orbit-interpolator camera)
             (camera:gen-orbit-interpolator-ccw camera
                                                (entity:pos selected-pc)
                                                camera:+angular-speed-rotate-command+
                                                +pi/2+))))
    (setf (mode (world:camera world)) :orbit)))

(defun rotate-90-around-player-cw (world)
  (rotate-90-around-player world :cw))

(defun rotate-90-around-player-ccw (world)
  (rotate-90-around-player world :ccw))

(defun reset-camera (world)
  (with-accessors ((selected-pc selected-pc)
                   (camera camera)
                   (main-state main-state)) world
    (multiple-value-bind (x-mouse y-mouse)
        (sdl2:mouse-state)
      (let* ((pos-terrain-matrix (world:pick-pointer-position world world x-mouse y-mouse))
             (x-world            (map-utils:coord-map->chunk (ivec2:ivec2-x pos-terrain-matrix)))
             (z-world            (map-utils:coord-map->chunk (ivec2:ivec2-y pos-terrain-matrix)))
             (y-world            (game-state:approx-terrain-height@pos main-state x-world z-world)))
        (camera:reset-camera-view camera (vec x-world
                                              y-world
                                              z-world))))))
(defun select-a-new-player (world player)
  (with-accessors ((selected-pc selected-pc)
                   (camera camera)
                   (main-state main-state)) world
    (reset-camera world)
    (setf (widget:bound-player (toolbar world)) player)
    (slide-to-active-player world)))

(defun select-next-player (world)
  (with-accessors ((selected-pc selected-pc)
                   (main-state main-state)) world
    (game-state:select-next-pc main-state)
    (select-a-new-player world (game-state:selected-pc main-state))))

(defun select-previous-player (world)
  (with-accessors ((selected-pc selected-pc)
                   (main-state main-state)) world
    (game-state:select-prev-pc main-state)
    (select-a-new-player world (game-state:selected-pc main-state))))

(define-constant +start-name-screenshot+
    (text-utils:strcat +program-name+ "-screenshot-1.tga")
  :test #'string=)

(defun save-screenshot (world filename &key (append-home t))
  (let* ((home (if append-home
                   (fs:home-dir :add-separator-ends t)
                   ""))
         (path (text-utils:strcat home filename)))
    (cl-gl-utils:with-render-to-file (path *window-w* *window-h*)
      #+debug-mode (misc:dbg "save screenshot ~a" path)
      (interfaces:render world world))))

(defun make-screenshot (world)
  (let* ((home              (fs:home-dir :add-separator-ends t))
         (regexp-filename   (text-utils:strcat +program-name+ "-screenshot-([0-9]+)\\.tga"))
         (template-filename (text-utils:strcat +program-name+ "-screenshot-~a.tga"))
         (files-existent    (remove-if-not #'(lambda (a) (cl-ppcre:scan regexp-filename a))
                                           (mapcar #'fs:pathname->namestring
                                                   (fs:directory-files home)))))
    (if (not files-existent)
        (save-screenshot world +start-name-screenshot+)
        (let ((all-nums (mapcar #'(lambda (n)
                                    (multiple-value-bind (all registers)
                                        (cl-ppcre:scan-to-strings regexp-filename n)
                                      (declare (ignore all))
                                      (parse-integer (first-elt registers)
                                                     :junk-allowed nil)))
                                files-existent)))
          (setf all-nums (shellsort all-nums #'>))
          (save-screenshot world (format nil
                                         template-filename
                                         (1+ (first-elt all-nums))))))))
