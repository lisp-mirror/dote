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

(in-package :mesh)

(defclass door-mesh-shell (triangle-mesh-shell)
  ((openp
    :initform nil
    :initarg  :openp
    :accessor openp)))

(defmethod rendering-needed-p ((object door-mesh-shell) renderer)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-camera (camera renderer)
    (let* ((center     (aabb-center (aabb object)))
	   (pos-camera (pos camera))
	   (a      (vec2 (elt center 0)     (elt center 2)))
	   (b      (vec2 (elt pos-camera 0) (elt pos-camera 2))))
      (declare (vec center pos-camera))
      (and (d< (vec2-length (vec2- a b))
	       (d* 2.0 +quad-tree-leaf-size+))
	   (world:cone-aabb-intersects-p renderer object)))))

(defmethod on-game-event ((object door-mesh-shell) (event game-event:end-turn))
  (misc:dbg " end turn ~a(~a) ~a" (type-of object) (id object) (type-of event))
  nil)

(defmethod on-game-event ((object door-mesh-shell) (event game-event:open-door-event))
    (if (= (id object) (game-event:id-destination event))
	(let ((pos (mesh:calculate-cost-position object)))
	  ;; TODO here enemies could spot you
	  (set-minimum-cost-map-layer@ (state object) (elt pos 0) (elt pos 1))
	  (setf (renderp object) nil)
	  (setf (openp   object) t)
	  ;; update the labyrinth
	  (update-for-rendering (parent-labyrinth object))
	  t))
    nil)

(defmethod on-game-event ((object door-mesh-shell) (event game-event:close-door-event))
  (if (= (id object) (game-event:id-destination event))
      (let ((pos (mesh:calculate-cost-position object)))
	(set-invalicable-cost-map-layer@ (state object) (elt pos 0) (elt pos 1))
	(setf (renderp object) t)
	(setf (openp   object) nil)
	(update-for-rendering (parent-labyrinth object))
	t))
  nil)
