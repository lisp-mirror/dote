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

;;;; planning

(defgeneric actuate-plan (object strategy action))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+interrupt-action+)))
   ;; TODO erase working memory
  ;; clean all goap cached function
  )

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
      ;; awake other AI player
      (and ai-entities-action-order
           (pop ai-entities-action-order)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+hide-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (when-let ((hiding-tile (ai-utils:go-find-hiding-place object)))
        ;; clear memoized results coming from the planner
        ;; (test precondition is-there-hiding-place-p)
        (ai-utils:go-find-hiding-place-clear-cache)
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

;;;; attack

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+attack-action+)))
  (labels ((rotate-until-visible (a b)
           (when (not (able-to-see-mesh:other-visible-p a b))
             (let ((event (make-instance 'game-event:rotate-entity-ccw-event
                                         :id-destination (id a)
                                         :decrement-movement-points nil)))
               (game-event:propagate-rotate-entity-ccw-event event)
               (rotate-until-visible a b)))))
    (with-slots-for-reasoning (object state ghost blackboard)
      (let* ((id-defender (get-from-working-mem object +w-memory-target-id+))
             (defender    (find-entity-by-id state id-defender)))
        (misc:dbg "atk id ~a" id-defender)
        (rotate-until-visible object defender)
        (battle-utils:attack-w-current-weapon object
                                              (find-entity-by-id state id-defender))))))

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

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-to-attack-pos-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (let ((path-struct (get-from-working-mem object +w-memory-path-struct+)))
        (%do-simple-move object path-struct state world)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+find-attack-pos-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (multiple-value-bind (path total-cost costs target-id)
        (blackboard:best-path-to-reach-enemy-w-current-weapon blackboard
                                                              object
                                                              :cut-off-first-tile nil)
      (declare (ignore costs))
      (let ((path-struct (game-state:make-movement-path path total-cost)))
        (put-in-working-memory object +w-memory-path-struct+ path-struct)
        (put-in-working-memory object +w-memory-target-id+   target-id)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+load-weapon-action+)))
  ;; TODO
  )

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
          (elaborate-current-tactical-plan ghost blackboard mesh nil)
          (let ((action (pop-action-plan ghost)))
              (actuate-plan mesh
                            (blackboard:strategy-decision blackboard)
                            action)))))))
