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

(define-constant +w-memory-target-id+        :target-id           :test #'eq)

(define-constant +w-memory-path-struct+      :path-struct         :test #'eq)

(define-constant +channel-planner-timeout+   0.008                :test #'=)

(defparameter    *planner-channel*           nil)

(defun put-in-working-memory (entity key value)
  (with-accessors ((ghost entity:ghost)) entity
      (with-accessors ((planner-working-memory planner-working-memory)) ghost
        (push value planner-working-memory)
        (push key   planner-working-memory)))
  entity)

(defun get-from-working-mem (entity key)
  (with-accessors ((ghost entity:ghost)) entity
    (with-accessors ((planner-working-memory planner-working-memory)) ghost
      (getf planner-working-memory key nil))))

(defun %clean-plan (ghost)
  #+debug-ai (misc:dbg "clear planner cache")
  (blackboard:reachable-p-w/concening-tiles-fn-clear-cache)
  (blackboard:reachable-p-w/concening-tiles-unlimited-cost-fn-clear-cache)
  (erase-working-memory ghost)
  (ai-utils:go-find-hiding-place-clear-cache)
  (goap:invalidate-tests-cache))

;;;; planning

(defgeneric actuate-plan (object strategy action))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+plan-stopper+)))
  (with-accessors ((ghost ghost)) object
    (%clean-plan ghost)))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+interrupt-action+)))
  (with-accessors ((ghost ghost)) object
    (%clean-plan ghost)))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+find-hiding-place-action+)))
  ;; does nothing
  )

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+idle-action+)))
  (with-accessors ((state state) (ghost ghost)) object
    (with-accessors ((ai-entities-action-order game-state:ai-entities-action-order)) state
      (erase-working-memory ghost)
      (goap:invalidate-tests-cache)
      ;; awake other AI player
      (and ai-entities-action-order
           (pop ai-entities-action-order)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+hide-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (let ((all-opponents-can-see-me (blackboard:all-other-factions-can-see-entity state
                                                                                    object)))
        (when-let ((hiding-tile (ai-utils:go-find-hiding-place object
                                                               all-opponents-can-see-me)))
          ;; clear memoized results coming from the planner
          ;; (test precondition is-there-hiding-place-p)
          ;;(ai-utils:go-find-hiding-place-clear-cache)
          (dbg "hiding tile chosen ~a" hiding-tile)
          (let ((entity-pos (calculate-cost-position object)))
            (multiple-value-bind (path total-cost costs)
                (blackboard:path-with-concerning-tiles blackboard
                                                       entity-pos
                                                       hiding-tile
                                                       :cut-off-first-tile nil)
              (declare (ignore costs))
              (let ((path-struct (game-state:make-movement-path path total-cost)))
                (%do-simple-move object path-struct state world)))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+flee-action+)))
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
                         (action (eql ai-utils:+launch-heal-spell-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-launch-heal-spell object)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-heal-spell-friend-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-launch-heal-spell-friend object)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-teleport-spell-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-launch-teleport-spell object)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-wall-break-spell-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-launch-wall-breaking-spell object)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+place-trap-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-place-trap object)))))

;;;; explore

(defmethod actuate-plan ((object md2-mesh)
                         (strategy (eql +explore-strategy+))
                         (action   (eql ai-utils:+move-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action (world action-scheduler:tactical-plane-action)
        (let ((new-pos (validate-player-path object
                                             (next-move-position object +explore-strategy+))))
          (when new-pos
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
                         (action (eql ai-utils:+go-near-weak-friend-atk-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (let* ((weak-friend   (ai-utils:friend-who-needs-help blackboard
                                                            object
                                                            :exclude-me t))
             (place-near    (first-elt (ai-utils:places-near-weak-friend blackboard
                                                                         object
                                                                         :weak-friend
                                                                         weak-friend)))
             (cost-to-reach (ai-utils:cost-to-reach-w/o-concerning-place blackboard
                                                                         object
                                                                         place-near))
             (place-pos     (ai-utils:protect-place-pos place-near)))
        (when (> cost-to-reach 0.0)
          (multiple-value-bind (path total-cost costs)
              (blackboard:path-near-goal-w/o-concerning-tiles blackboard
                                                              object
                                                              place-pos
                                                              :cut-off-first-tile nil)
            (declare (ignore costs))
            (let* ((path-struct (game-state:make-movement-path path total-cost)))
              (%do-simple-move object path-struct state world))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-near-weak-friend-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (let* ((weak-friend   (ai-utils:friend-who-needs-help blackboard
                                                            object
                                                            :exclude-me t))
             (place-near    (first-elt (ai-utils:places-near-weak-friend blackboard
                                                                         object
                                                                         :weak-friend
                                                                         weak-friend)))
             (cost-to-reach (ai-utils:cost-to-reach-w/o-concerning-place blackboard
                                                                         object
                                                                         place-near))
             (place-pos     (ai-utils:protect-place-pos place-near)))
        (when (> cost-to-reach 0.0)
          (multiple-value-bind (path total-cost costs)
              (blackboard:path-near-goal-w/o-concerning-tiles blackboard
                                                              object
                                                              place-pos
                                                              :cut-off-first-tile nil)
            (declare (ignore costs))
            (let* ((path-struct (game-state:make-movement-path path total-cost)))
              (%do-simple-move object path-struct state world))))))))


(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+protect-attack-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (%rotate-until-someone-visible state object t)
        (let* ((defender-id (ai-utils:attackable-opponents-id blackboard object)))
          (battle-utils:attack-w-current-weapon object
                                                (find-entity-by-id state defender-id)))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+protect-attack-spell-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (ai-utils:go-launch-attack-spell object)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+protect-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        (%rotate-until-someone-visible state object t)
        (when (ai-utils:reward-possible-p state)
          (ai-utils:go-reward-heal-spell object))))))

;;;; attack

(defun %rotate-until-someone-visible (state entity
                                      &optional
                                        (decrement-movement-point nil) (max 4))
  "rotate a until someone is visible or give up after 4 attempts
Note: all attackable position will be updated as well"
  (game-state:with-world (world state)
    (with-accessors ((blackboard blackboard:blackboard)) state
      (when (and (> max 0)
                 (not (able-to-see-mesh:other-faction-visible-players entity)))
        (let ((event (make-instance 'game-event:rotate-entity-ccw-event
                                    :id-destination            (id entity)
                                    :decrement-movement-points decrement-movement-point)))
          (action-scheduler:with-enqueue-action-and-send-remove-after
              (world action-scheduler:tactical-plane-action)
            (game-event:propagate-rotate-entity-ccw-event event)
            (blackboard:update-all-attacking-pos blackboard)
            (%rotate-until-someone-visible state entity
                                           decrement-movement-point
                                           (1- max))))))))

(defun %rotate-until-visible (state a b &optional (decrement-movement-point nil) (max 4))
  "rotate a until b is visible or give up after 4 attempts
Note: all attackable position will be updated as well"
  (game-state:with-world (world state)
    (with-accessors ((blackboard blackboard:blackboard)) state
      (when (and (> max 0)
                 (not (able-to-see-mesh:other-visible-p a b)))
        (let ((event (make-instance 'game-event:rotate-entity-ccw-event
                                    :id-destination            (id a)
                                    :decrement-movement-points decrement-movement-point)))
          (action-scheduler:with-enqueue-action-and-send-remove-after
              (world action-scheduler:tactical-plane-action)
            (game-event:propagate-rotate-entity-ccw-event event)
            (blackboard:update-all-attacking-pos blackboard)
            (%rotate-until-visible state a b decrement-movement-point (1- max))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+attack-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (let* ((id-defender (get-from-working-mem object +w-memory-target-id+))
           (defender    (find-entity-by-id state id-defender)))
      #+debug-ai (misc:dbg "atk id ~a" id-defender)
      (%rotate-until-visible state object defender)
      (battle-utils:attack-w-current-weapon object
                                            (find-entity-by-id state id-defender)))))

(defun %do-simple-move (mesh path-struct state world)
  (action-scheduler:with-enqueue-action (world action-scheduler:tactical-plane-action)
    (setf (game-state:selected-path state) path-struct)
    (let* ((tiles          (game-state:tiles (game-state:selected-path state)))
           (cost           (game-state:cost  (game-state:selected-path state)))
           (movement-event (make-instance 'game-event:move-entity-along-path-event
                                          :path           tiles
                                          :cost           cost
                                          :id-destination (id mesh))))
      (game-event:propagate-move-entity-along-path-event movement-event))))

(defun need-to-move-to-attack-p (entity)
  (get-from-working-mem entity +w-memory-path-struct+))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-to-attack-pos-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (when (need-to-move-to-attack-p object)
        (let ((path-struct (get-from-working-mem object +w-memory-path-struct+)))
          (%do-simple-move object path-struct state world))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-near-to-attack-pos-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (let ((reachable-fn (blackboard:reachable-p-w/concening-tiles-unlimited-cost-fn
                           blackboard)))
        (multiple-value-bind (path total-cost costs target-id)
          (blackboard:best-path-near-attack-goal-w-current-weapon blackboard
                                                                  object
                                                                  :cut-off-first-tile nil
                                                                  :reachable-fn-p
                                                                  reachable-fn)
          (declare (ignore costs))
          (let* ((path-struct (game-state:make-movement-path path total-cost))
                 (defender    (find-entity-by-id state target-id)))
            (%do-simple-move object path-struct state world)
            (%rotate-until-visible state object defender)))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+find-attack-pos-action+)))
  (flet ((put-in-memory (target-id path-struct)
           (put-in-working-memory object +w-memory-path-struct+ path-struct)
           (put-in-working-memory object +w-memory-target-id+   target-id)))
    (with-slots-for-reasoning (object state ghost blackboard)
      (let* ((target-next (blackboard:entity-in-valid-attackable-pos-p object)))
        (if target-next ;; if non nil we are in an attack position
            (put-in-memory (id target-next) nil)
            (multiple-value-bind (path total-cost costs target-id-move)
                (blackboard:best-path-to-reach-enemy-w-current-weapon blackboard
                                                                      object
                                                                      :cut-off-first-tile nil)
              (declare (ignore costs))
              (let* ((path-struct (game-state:make-movement-path path total-cost)))
                (put-in-memory target-id-move path-struct))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+load-weapon-action+)))
  ;; TODO
  )

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+attack-spell-action+)))
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (action-scheduler:with-enqueue-action-and-send-remove-after
          (world action-scheduler:tactical-plane-action)
        ;(%rotate-until-someone-visible state object t)
        (ai-utils:go-launch-attack-spell object)))))

;;;; strategy
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
          (if (null *planner-channel*)
              (progn
                (widget:activate-planner-icon world)
                (setf *planner-channel* (lparallel:make-channel))
                (lparallel:submit-task *planner-channel*
                                       'elaborate-current-tactical-plan
                                       ghost blackboard mesh nil))
              (if (lparallel:try-receive-result *planner-channel*
                                                :timeout +channel-planner-timeout+)
                  (progn
                    (widget:deactivate-planner-icon world)
                    (setf *planner-channel* nil)
                    (let ((action (pop-action-plan ghost)))
                      #+debug-ai (misc:dbg "popped action ~a" action)
                      (actuate-plan mesh
                                    (blackboard:strategy-decision blackboard)
                                    action))))))))))
;                  (misc:dbg "plan not ready."))))))))
