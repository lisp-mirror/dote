;; dawn of the Era: a tactical game.
;; Copyright (C) 2015  cage

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along      with      this      program.       If      not,      see
;; <http://www.gnu.org/licenses/>.

(in-package :md2-mesh)

;;;; planning

(defgeneric actuate-plan (object strategy action))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+interrupt-action+)))
  ;; "does nothing"
  )

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+find-hiding-place-action+)))
  ;; TODO
  )

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+idle-action+)))
  (with-accessors ((state state)) object
    (with-accessors ((ai-entities-action-order game-state:ai-entities-action-order)) state
      ;; awake other AI player
      (and ai-entities-action-order
           (pop ai-entities-action-order)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action   (eql ai-utils:+hide-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (when-let ((hiding-tile (ai-utils:go-find-hiding-place object)))
        ;; clear memoized results coming from the planner
        ;; (test precondition is-there-hiding-place-p)
        (ai-utils:go-find-hiding-place-clear-cache)
        (misc:dbg "hiding place is ~a" hiding-tile)
        (action-scheduler:with-enqueue-action (world action-scheduler:tactical-plane-action)
          (let ((entity-pos (calculate-cost-position object)))
            (multiple-value-bind (path total-cost costs)
                (blackboard:path-with-concerning-tiles blackboard
                                                       entity-pos
                                                       hiding-tile
                                                       :cut-off-first-tile nil)
              (declare (ignore costs))
              (let ((path-struct (game-state:make-movement-path path total-cost)))
                (setf (game-state:selected-path state) path-struct)
                (let* ((tiles          (game-state:tiles (game-state:selected-path state)))
                       (cost           (game-state:cost  (game-state:selected-path state)))
                       (movement-event (make-instance 'game-event:move-entity-along-path-event
                                                      :path           tiles
                                                      :cost           cost
                                                      :id-destination (id object))))
                  (game-event:propagate-move-entity-along-path-event movement-event))))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action   (eql ai-utils:+flee-action+)))
    (with-slots-for-reasoning (object state ghost blackboard)
      (game-state:with-world (world state)
        (action-scheduler:with-enqueue-action (world action-scheduler:tactical-plane-action)
          (let ((new-pos (validate-player-path object
                                               (list
                                                (ai-utils:go-next-flee-position blackboard
                                                                                object)))))
            (when new-pos
              ;;(misc:dbg "~a go to ~a dir ~a" (id object) (game-state:tiles new-pos) (dir object))
              (setf (game-state:selected-path state) new-pos)
              (let* ((tiles          (game-state:tiles (game-state:selected-path state)))
                     (cost           (game-state:cost  (game-state:selected-path state)))
                     (movement-event (make-instance 'game-event:move-entity-along-path-event
                                                    :path           tiles
                                                    :cost           cost
                                                    :id-destination (id object))))
                (game-event:propagate-move-entity-along-path-event movement-event))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action   (eql ai-utils:+launch-heal-spell-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-launch-heal-spell object)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action   (eql ai-utils:+launch-teleport-spell-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-launch-teleport-spell object)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action   (eql ai-utils:+launch-wall-break-spell-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-launch-wall-breaking-spell object)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action   (eql ai-utils:+place-trap-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-place-trap object)))))

;;; explore

(defmethod actuate-plan ((object md2-mesh)
                         (strategy (eql +explore-strategy+))
                         (action   (eql ai-utils:+move-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action (world action-scheduler:tactical-plane-action)
        (let ((new-pos (validate-player-path object
                                             (next-move-position object +explore-strategy+))))
          (when new-pos
            ;;(misc:dbg "~a go to ~a dir ~a" (id object) (game-state:tiles new-pos) (dir object))
            (setf (game-state:selected-path state) new-pos)
            (let* ((tiles          (game-state:tiles (game-state:selected-path state)))
                   (cost           (game-state:cost  (game-state:selected-path state)))
                   (movement-event (make-instance 'game-event:move-entity-along-path-event
                                                  :path           tiles
                                                  :cost           cost
                                                  :id-destination (id object))))
              (game-event:propagate-move-entity-along-path-event movement-event))))))))

;;;;; strategy
(defun actuate-strategy (mesh)
  (with-slots-for-reasoning (mesh state ghost blackboard)
    (game-state:with-world (world state)
      (with-accessors ((ai-entities-action-order game-state:ai-entities-action-order)) state
        (when (and (eq  (my-faction mesh) game-state:+npc-type+)
                   (eq  (my-faction mesh) (game-state:faction-turn state))
                   (world:actions-queue-empty-p world) ;; ensure one action at time
                   ai-entities-action-order            ;; if nil all ai players made a move
                   (= (id mesh)
                      (id (first-elt ai-entities-action-order))) ;; ensure it's my turn
                   ghost)
          (elaborate-current-tactical-plan ghost blackboard mesh nil)
          (let ((action (pop-action-plan ghost)))
            (actuate-plan mesh
                          (blackboard:strategy-decision blackboard)
                          action)))))))
