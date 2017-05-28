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

(defclass container-mesh-shell (triangle-mesh-shell) ())

(defmethod game-event:on-game-event ((object container-mesh-shell) (event game-event:end-turn))
  ;;(misc:dbg " end turn ~a(~a) ~a" (type-of object) (id object) (type-of event))
  nil)

(defmethod game-event:on-game-event ((object container-mesh-shell)
                                     (event game-event:lock-object-event))
  (game-event:check-event-targeted-to-me (object event)
    (let ((keycode (random-container:generate-keycode)))
      (n-setf-path-value (character:basic-interaction-params (ghost object))
                         (list basic-interaction-parameters:+can-be-opened+)
                         keycode))))

(defmethod game-event:on-game-event ((object container-mesh-shell)
                                     (event game-event:unlock-object-event))
  (game-event:check-event-targeted-to-me (object event)
    (game-state:with-world (world (state object))
      (let ((player (find-entity-by-id (state object) (game-event:id-origin event))))
        (when player
          (let* ((container-keycode (interactive-entity:object-keycode (ghost object)))
                 (key               (find-if #'(lambda (a)
                                                 (and (interactive-entity:keyp           a)
                                                      (interactive-entity:object-keycode a)
                                                      (string= (interactive-entity:object-keycode a)
                                                               container-keycode)))
                                             (character:inventory (ghost player)))))
            (cond
              (key
               (character:remove-from-inventory (ghost player) key)
               (n-setf-path-value (character:basic-interaction-params (ghost object))
                                  (list basic-interaction-parameters:+can-be-opened+)
                                  t)
               (world:post-entity-message world
                                          player
                                          (_ "Container opened!")
                                          t
                                          (cons (_ "Ok")
                                                #'(lambda (a b)
                                                    (declare (ignore a b))
                                                    (world:remove-all-windows world)))))
              ((game-event:force-unlock-p event)
               (n-setf-path-value (character:basic-interaction-params (ghost object))
                                  (list basic-interaction-parameters:+can-be-opened+)
                                  t)
               (world:post-entity-message world
                                          player
                                          (_ "Container opened!")
                                          t
                                          (cons (_ "Ok")
                                                #'(lambda (a b)
                                                    (declare (ignore a b))
                                                    (world:remove-all-windows world)
                                                    (battle-utils:defend-from-container-trap object
                                                        player)))))
              (t
               (world:post-entity-message world
                                          player
                                          (_ "No fitting key found")
                                          t
                                          (cons (_ "Ok")
                                                #'(lambda (a b)
                                                    (declare (ignore a b))
                                                    (world:remove-all-windows world))))))))))))

(defmethod rendering-needed-p ((object container-mesh-shell) renderer)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (world:cone-aabb-intersects-p renderer object))
