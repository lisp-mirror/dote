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

(defclass trap-mesh-shell (triangle-mesh-shell)
  ((faction
    :initform +pc-type+
    :initarg  :faction)))

(gen-type-p trap-mesh-shell)

(defmethod my-faction ((object trap-mesh-shell))
  (slot-value object 'faction ))

(defmethod (setf my-faction) (value (object trap-mesh-shell))
  (setf (slot-value object 'faction) value))

(defun %faction-eq (trap faction-id)
  (declare (trap-mesh-shell trap))
  (eq (my-faction trap)
      faction-id))

(defmethod faction-player-p ((object trap-mesh-shell) &optional (id-entity (id object)))
  (declare (ignore id-entity))
  (%faction-eq object +pc-type+))

(defmethod faction-ai-p ((object trap-mesh-shell) &optional (id-entity (id object)))
  (declare (ignore id-entity))
  (%faction-eq object +npc-type+))

(defmethod rendering-needed-p ((object trap-mesh-shell) renderer)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-accessors ((state state)) object
    (with-camera (camera renderer)
      (let* ((center     (aabb-center (aabb object)))
	     (pos-camera (pos camera))
	     (a      (vec2 (elt center 0)     (elt center 2)))
	     (b      (vec2 (elt pos-camera 0) (elt pos-camera 2))))
	(declare (vec center pos-camera))
	(and (%faction-eq object (game-state:faction-turn state))
	     (d< (vec2-length (vec2- a b))
		 (d* 2.0 +quad-tree-leaf-size+))
	     (world:cone-aabb-intersects-p renderer object))))))

(defmethod game-event:on-game-event ((object trap-mesh-shell)
				     (event game-event:deactivate-trap-event))
  (game-event:check-event-targeted-to-me (object event)
    (game-state:with-world (world (state object))
      (let ((player (find-entity-by-id (state object) (game-event:id-origin event))))
	(when player
	  (world:post-entity-message world
				     player
				     (_ "Trap deactivated!")
				     t
				     (cons (_ "Ok")
					   #'(lambda (a b)
					       (declare (ignore a b))
					       (world:remove-all-windows world)
					       (remove-entity-by-id world (id object))
					       (pop-trap-entity (state object) object)))))))))

(defmethod on-game-event ((object trap-mesh-shell) (event game-event:end-turn))
  (misc:dbg " end turn ~a(~a) ~a" (type-of object) (id object) (type-of event))
  nil)
