;; dawn of the Era: a tactical game.
;; Copyright (C) 2017  cage

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

(define-constant +w-memory-target-id+             :target-id           :test #'eq)

(define-constant +w-memory-path-struct+           :path-struct         :test #'eq)

(define-constant +w-memory-action-did-costs+      :action-did-cost     :test #'eq)

(define-constant +channel-planner-timeout+   0.008                     :test #'=)

(defparameter    *planner-channel*           nil)

(defgeneric put-in-working-memory (object key value))

(defmethod put-in-working-memory ((object entity) key value)
  (put-in-working-memory (ghost object) key value))

(defmethod put-in-working-memory ((object player-character) key value)
  (with-accessors ((planner-working-memory planner-working-memory)) object
    (push value planner-working-memory)
    (push key   planner-working-memory))
  object)

(defgeneric get-from-working-memory (object key))

(defmethod get-from-working-memory ((object entity) key)
  (get-from-working-memory (ghost object) key))

(defmethod get-from-working-memory ((object player-character) key)
  (with-accessors ((planner-working-memory planner-working-memory)) object
    (getf planner-working-memory key nil)))

(defun set-cost-occurred (entity)
  (put-in-working-memory entity +w-memory-action-did-costs+ t))

(defun cost-occurred-p (entity)
  (get-from-working-memory entity +w-memory-action-did-costs+))

(defun force-idle-plan-if-no-cost (entity)
  (when (not (cost-occurred-p entity))
    (setf (character:force-idle-plan (ghost entity)) t)))

(let ((id 0))
  (defun blacklist-clear-id ()
    (setf id 0))
  (defun blacklist-increment-id ()
    (incf id))
  (defun blacklist-action-with-parent (entity planner action-name)
    (flet ((format-cond-name (s)
             (format-keyword (text-utils:strcat (symbol-name s)
                                                "-"
                                                (format nil "~a" id)))))
      (blacklist-increment-id)
      (with-accessors ((ghost ghost)) entity
        (with-accessors ((planners planners)) ghost
          #+debug-mode (assert (> (length (original-current-plan ghost)) 2))
          (let* ((original-plan (original-current-plan ghost))
                 (last-action   (goap:find-action planner action-name))
                 (parent-action (goap:find-action planner (elt original-plan
                                                               (- (length original-plan) 3))))
                 (sink-action   (goap:fetch-sink-action planner))
                 (link          (goap:find-link-between-action parent-action
                                                               last-action))
                 (link-name     (goap:condition-name link)))
            #+debug-mode (assert link)
            (let* ((sink-effect-name (format-cond-name link-name))
                   (sink-condition   (goap:make-action-condition sink-effect-name t)))
              (goap:add-action-effect          sink-action sink-condition)
              (goap:add-action-precondition    sink-action link)
              (goap:add-action-precondition    last-action sink-condition)
              (goap:remove-action-precondition last-action link-name)
              entity)))))))

(defun blacklist (entity strategy-decision action-name)
  (with-accessors ((ghost ghost)) entity
    (with-accessors ((planners planners)) ghost
      #+debug-mode (assert (> (length (original-current-plan ghost)) 1))
      (let* ((planner            (find-plan ghost strategy-decision))
             (original-plan      (original-current-plan ghost)))
        (if (= (length original-plan) 2) ;; action with-no-parents, just increase cost
            (let ((last-action (goap:find-action planner action-name)))
              (goap:maximize-action-cost last-action)
              entity)
            (blacklist-action-with-parent entity planner action-name))))))

(defun blacklist-action-if-no-cost (entity strategy-decision action)
  #+debug-ai (dbg "blacklisting ~a, ~a? ~a"
                  entity
                  action
                  (not (cost-occurred-p entity)))
  (when (not (cost-occurred-p entity))
    #+debug-ai (misc:dbg "blacklisting ~s" action)
    (blacklist entity strategy-decision action)))

    ;;(character:push-in-blacklisted-plan-goals (ghost entity) action)))

(defmacro with-force-idle-plane-if-no-cost ((entity) &body body)
  `(prog1
       (progn ,@body)
     (force-idle-plan-if-no-cost ,entity)))

(defmacro with-blacklist-action-if-no-cost ((entity strategy-decision action)
                                                    &body body)
  `(prog1
       (progn ,@body)
     (blacklist-action-if-no-cost ,entity ,strategy-decision ,action)))

(defmacro with-maybe-set-cost-occurred ((entity saved-mp saved-sp) &body body)
  (with-gensyms (ghost)
    `(with-accessors ((,ghost ghost)) ,entity
       (progn ,@body)
       #+debug-ai (misc:dbg "cost? saved ~a ~a, ~a ~a"
                            ,saved-mp (current-movement-points ,ghost)
                            ,saved-sp (current-magic-points   ,ghost))
       (when (or (> ,saved-mp (current-movement-points ,ghost))
                 (> ,saved-sp (current-magic-points    ,ghost)))
         (set-cost-occurred ,entity)))))

(defun %clean-plan (ghost)
  #+debug-ai (misc:dbg "clear planner cache")
  (blackboard:reachable-p-w/concening-tiles-fn-clear-cache)
  (blackboard:reachable-p-w/concening-tiles-unlimited-cost-fn-clear-cache)
  (erase-working-memory ghost)
  (ai-utils:go-find-hiding-place-clear-cache)
  (ai-utils:useful-reachable-fountain-clear-cache)
  (goap:invalidate-tests-cache))

(defun %clean-plan-and-blacklist (ghost)
  (%clean-plan ghost)
  ;; clear blacklisted actions for planner
  #+debug-ai (misc:dbg "clear planner cache and blacklist")
  (clear-blacklist ghost))

(defmacro with-maybe-blacklist ((entity strategy-decision action
                                        &key (ignore-points-difference nil))
                                &body body)
  (with-gensyms (saved-mp saved-sp ghost state world)
    `(with-accessors ((,state state)
                      (,ghost ghost)) ,entity
       (game-state:with-world (,world ,state)
         (let ((,saved-mp (current-movement-points ,ghost))
               (,saved-sp (current-magic-points    ,ghost)))
           (declare (ignorable ,saved-mp ,saved-sp))
           ;; (clear-blacklist-if-cost-occurred ,entity)
           ,@body
           ,(when (not ignore-points-difference)
              `(action-scheduler:with-enqueue-action-and-send-remove-after
                   (,world action-scheduler:tactical-plane-action)
                 (with-maybe-set-cost-occurred (,entity ,saved-mp ,saved-sp))))
           (when (action-terminal-p ,ghost ,action)
             #+debug-ai
             (misc:dbg "~a is terminal in ~a!" ,action (original-current-plan (ghost ,entity)))
             (action-scheduler:with-enqueue-action-and-send-remove-after
                 (,world action-scheduler:tactical-plane-action)
               (with-blacklist-action-if-no-cost (,entity ,strategy-decision ,action)))))))))

;;;; planning

(defmacro defact-plan (arg &rest body)
  (let* ((function-name (alexandria:format-symbol t "~:@(actuate-plan~)")))
    `(defmethod ,function-name (,@arg)
       (clear-blacklist-if-cost-occurred object)
       ,@body)))

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
    (%clean-plan-and-blacklist ghost)))

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
      (%clean-plan-and-blacklist ghost)
      ;; awake other AI player
      (and ai-entities-action-order
           (pop ai-entities-action-order)))))

;; general

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+rotate-action+)))
  (with-maybe-blacklist (object strategy action :ignore-points-difference t)
    (with-accessors ((state state)) object
      (%rotate-until-someone-visible state object :decrement-movement-points t))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+hide-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (let ((all-opponents-can-see-me (blackboard:all-other-factions-can-see-entity state
                                                                                    object)))
        (when-let ((hiding-tile (ai-utils:go-find-hiding-place object
                                                               all-opponents-can-see-me)))
          #+debug-ai (dbg "hiding tile chosen ~a" hiding-tile)
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
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)) object
      (with-accessors ((blackboard game-state:blackboard)) state
        (game-state:with-world (world state)
          (multiple-value-bind (path cost)
              (ai-utils:next-flee-position blackboard object)
            (let* ((path-struct (game-state:make-movement-path path cost)))
              (%do-simple-move object path-struct state world))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-heal-spell-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)) object
      (game-state:with-world (world state)
        (action-scheduler:with-enqueue-action-and-send-remove-after
            (world action-scheduler:tactical-plane-action)
          (ai-utils:go-launch-heal-spell object))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-heal-spell-friend-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)) object
      (game-state:with-world (world state)
        (action-scheduler:with-enqueue-action-and-send-remove-after
            (world action-scheduler:tactical-plane-action)
          (ai-utils:go-launch-heal-spell-friend object))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-teleport-spell-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)) object
      (game-state:with-world (world state)
        (action-scheduler:with-enqueue-action-and-send-remove-after
            (world action-scheduler:tactical-plane-action)
          (ai-utils:go-launch-teleport-spell object))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-wall-break-spell-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)) object
      (game-state:with-world (world state)
        (let ((nearest-wall (ai-utils:find-nearest-visible-wall object)))
          (assert nearest-wall)
          (%rotate-until-visible state object nearest-wall))
        (action-scheduler:with-enqueue-action-and-send-remove-after
            (world action-scheduler:tactical-plane-action)
          (ai-utils:go-launch-wall-breaking-spell object))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+place-trap-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)) object
      (game-state:with-world (world state)
        (action-scheduler:with-enqueue-action-and-send-remove-after
            (world action-scheduler:tactical-plane-action)
          (ai-utils:go-place-trap object))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+use-fountain+)))
  (with-accessors ((state state)) object
    (let ((nearest-fountain (ai-utils:useful-reachable-fountain object)))
      #+debug-mode (assert nearest-fountain)
      (%rotate-until-visible state object nearest-fountain)
      (game-event:send-activate-switch-event object nearest-fountain))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-to-fountain-action+)))
  ;; note: no  need to  blacklist the  context precondition  of action
  ;; ":find-fountain"  ensure  the  fountain is  valid  and  reachable
  ;; goap:lint-planner   will  take   care   of   that  checking   for
  ;; goap:is-there-useful-reachable-fountain-p                      if
  ;; ai-utils:+use-fountain+ is present in the planner.
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (multiple-value-bind (fountain path-to-reach-fountain cost-to-reach-fountain)
          (ai-utils:useful-reachable-fountain object)
        #+debug-mode (assert fountain)
        (when (not (epsilon= cost-to-reach-fountain 0.0)) ;; we are just next to fountain
          (let ((path-struct (game-state:make-movement-path path-to-reach-fountain
                                                            cost-to-reach-fountain)))
            (%do-simple-move object path-struct state world)))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+find-fountain-action+)))
  ;; does nothing
  )

;;;; explore

(defmethod actuate-plan ((object md2-mesh)
                         (strategy (eql +explore-strategy+))
                         (action   (eql ai-utils:+move-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)) object
      (game-state:with-world (world state)
        (multiple-value-bind (path cost)
            (next-move-position object +explore-strategy+)
          (let* ((path-struct (game-state:make-movement-path path cost)))
            (%do-simple-move object path-struct state world)))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-near-weak-friend-atk-action+)))
  (with-maybe-blacklist (object strategy action)
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
                (%do-simple-move object path-struct state world)))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-near-weak-friend-action+)))
  (with-maybe-blacklist (object strategy action)
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
                (%do-simple-move object path-struct state world)))))))))


(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+protect-attack-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-slots-for-reasoning (object state ghost blackboard)
      (game-state:with-world (world state)
        (%rotate-until-someone-visible state object :decrement-movement-points t)
        (action-scheduler:with-enqueue-action-and-send-remove-after
            (world action-scheduler:tactical-plane-action)
          (let* ((defender-id (ai-utils:attackable-opponents-id blackboard object)))
            (battle-utils:attack-w-current-weapon object
                                                  (find-entity-by-id state defender-id))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+protect-attack-spell-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)) object
      (game-state:with-world (world state)
        (action-scheduler:with-enqueue-action-and-send-remove-after
            (world action-scheduler:tactical-plane-action)
          (ai-utils:go-launch-attack-spell object))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+protect-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)
                     (ghost ghost)) object
      (game-state:with-world (world state)
        (%rotate-until-someone-visible state object :decrement-movement-points t)
        (action-scheduler:with-enqueue-action-and-send-remove-after
            (world action-scheduler:tactical-plane-action)
          (when (ai-utils:reward-possible-p state)
            (ai-utils:go-reward-heal-spell object)))))))

;;;; attack

(defmacro %gen-rotate-until-visible (name state entity target-entity
                                     decrement-movement-points
                                     set-cost-occurred-p
                                     max
                                     count)
  (with-gensyms (world blackboard event)
    `(game-state:with-world (,world ,state)
       (assert (<= ,count ,max))
       (with-accessors ((,blackboard blackboard:blackboard)) ,state
         (cond
           ((= ,count ,max)
            count)
           (,(if target-entity
                 `(absee-mesh:other-visible-p ,entity ,target-entity)
                 `(absee-mesh:other-faction-visible-players ,entity))
            (misc:dbg "rotation end with see in ~a times" count)
             (when (and (> count 0)
                        ,set-cost-occurred-p)
               (misc:dbg "setting cost!")
                 (set-cost-occurred ,entity))
               ,count)
           (t
            (let ((,event (make-instance 'game-event:rotate-entity-ccw-event
                                         :id-destination            (id ,entity)
                                         :decrement-movement-points ,decrement-movement-points)))
              (action-scheduler:with-enqueue-action-and-send-remove-after
                  (,world action-scheduler:tactical-plane-action)
                (game-event:propagate-rotate-entity-ccw-event ,event)
                (blackboard:update-all-attacking-pos ,blackboard)
                ,(let ((function-recursive-call (list name state entity
                                                      :decrement-movement-points
                                                      decrement-movement-points
                                                      :set-cost-occurred-p
                                                      set-cost-occurred-p
                                                      :max
                                                      max
                                                      :count
                                                      `(1+ ,count))))
                   (if target-entity
                       (fresh-list-insert@ function-recursive-call target-entity 3)
                       function-recursive-call))))))))))

(defun %rotate-until-someone-visible (state entity &key
                                                     (decrement-movement-points nil)
                                                     (set-cost-occurred-p t)
                                                     (max 4)
                                                     (count 0))
  "rotate entity until someone is visible  or give up after 4 attempts
return the number  of rotation occurred
Note:  all attackable position will be updated as well"
  (%gen-rotate-until-visible %rotate-until-someone-visible
                             state
                             entity
                             nil
                             decrement-movement-points
                             set-cost-occurred-p
                             max
                             count))

(defun %rotate-until-visible (state entity target-entity &key
                                                           (decrement-movement-points nil)
                                                           (max 4)
                                                           (set-cost-occurred-p t)
                                                           (count 0))
  "rotate entity  until target-entity  is visible or  give up  after 4
attempts Note: all attackable position will be updated as well"
  (%gen-rotate-until-visible %rotate-until-visible
                             state
                             entity
                             target-entity
                             decrement-movement-points
                             set-cost-occurred-p
                             max
                             count))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+attack-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-slots-for-reasoning (object state ghost blackboard)
      (let* ((id-defender (get-from-working-memory object +w-memory-target-id+))
             (defender    (find-entity-by-id state id-defender)))
        #+debug-ai (misc:dbg "atk id ~a" id-defender)
        (%rotate-until-visible state object defender)
        (battle-utils:attack-w-current-weapon object
                                              (find-entity-by-id state
                                                                 id-defender))))))

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
  (get-from-working-memory entity +w-memory-path-struct+))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-to-attack-pos-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-slots-for-reasoning (object state ghost blackboard)
      (game-state:with-world (world state)
        (when (need-to-move-to-attack-p object)
          (let ((path-struct (get-from-working-memory object +w-memory-path-struct+)))
            #+debug-ai (misc:dbg "path-struct go ~a" (game-state:tiles path-struct))
            (%do-simple-move object path-struct state world)))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-near-to-attack-pos-action+)))
  (with-maybe-blacklist (object strategy action)
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
              (%rotate-until-visible state object defender))))))))

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
  (with-maybe-blacklist (object strategy action)
    (with-accessors ((state state)) object
      (game-state:with-world (world state)
        (action-scheduler:with-enqueue-action-and-send-remove-after
            (world action-scheduler:tactical-plane-action)
          ;;(%rotate-until-someone-visible state object t)
          (ai-utils:go-launch-attack-spell object))))))

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
;;                  (misc:dbg "plan not ready."))))))))
