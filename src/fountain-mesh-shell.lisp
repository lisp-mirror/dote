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

(defclass fountain-mesh-shell (triangle-mesh-shell) ())

(defmethod game-event:on-game-event ((object fountain-mesh-shell) (event game-event:end-turn))
  ;;(misc:dbg " end turn ~a(~a) ~a" (type-of object) (id object) (type-of event))
  nil)

(defmethod game-event:on-game-event ((object fountain-mesh-shell)
				     (event game-event:other-interaction-event))
  (game-event:check-event-targeted-to-me (object event)
    (let ((player (find-entity-by-id (state object) (game-event:id-origin event))))
      (when player
	(battle-utils:defend-from-fountain-interaction object player)))))

(defmethod rendering-needed-p ((object fountain-mesh-shell) renderer)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (world:cone-aabb-intersects-p renderer object))

(defmethod apply-damage :after ((object fountain-mesh-shell) damage
				&key &allow-other-keys)
  (with-accessors ((state state)
		   (pos pos)
		   (dir dir)
		   (aabb aabb)
		   (texture-object texture-object)
		   (compiled-shaders compiled-shaders)) object
    (when (entity-dead-p object)
      (let ((debris (particles:make-debris (aabb-center aabb)
					   +y-axe+
					   (particles:debris-particles-number damage)
					   texture-object
					   compiled-shaders)))
	(game-state:with-world (world state)
	  (world:push-entity world debris))))))
