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
