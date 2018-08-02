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

(defparameter    *planner-channel*           nil)

(defparameter    *update-infos-channel*      nil)

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

(defun set-memory-ignore-interrupt-from (npc ignored-entity-id)
  (put-in-working-memory npc +w-memory-ignore-interrupt-id+ ignored-entity-id))

(defun get-memory-ignore-interrupt-from (npc)
  (get-from-working-memory npc +w-memory-ignore-interrupt-id+))

(defun set-memory-seen-before-door (npc)
  (put-in-working-memory npc
                         +w-memory-opponents-before-open-door+
                         (ai-utils:all-visible-opponents-id npc)))

(defun get-memory-seen-before-door (npc)
  (get-from-working-memory npc +w-memory-opponents-before-open-door+))

(defun set-memory-entity-goal-vanished (npc)
  (put-in-working-memory npc
                         +w-memory-opponents-entity-vanished+
                         t))

(defun get-memory-entity-goal-vanished (npc)
  (get-from-working-memory npc +w-memory-opponents-entity-vanished+))

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
  #+(and debug-mode debug-ai) (dbg "blacklisting ~a, ~a? ~a"
                                   entity
                                   action
                                   (not (cost-occurred-p entity)))
  (when (not (cost-occurred-p entity))
    #+(and debug-mode debug-ai) (misc:dbg "blacklisting ~s" action)
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
       #+(and debug-mode debug-ai) (misc:dbg "cost? saved ~a ~a, ~a ~a"
                                             ,saved-mp (current-movement-points ,ghost)
                                             ,saved-sp (current-spell-points    ,ghost))
       (when (or (> ,saved-mp (current-movement-points ,ghost))
                 (> ,saved-sp (current-spell-points    ,ghost)))
         (set-cost-occurred ,entity)))))

(defun %clean-plan (entity &optional (excluded '()))
  (with-accessors ((ghost ghost)
                   (state state)) entity
    (with-accessors ((blackboard blackboard:blackboard)) state
      #+(and debug-mode debug-ai) (misc:dbg "clear planner cache")
      (erase-working-memory ghost)
      (ai-logger:clean-log ghost      ai-logger:+ai-log-clean-end-plan+)
      (ai-logger:clean-log blackboard ai-logger:+ai-log-clean-end-plan+)
      (world:clear-all-memoized-function-cache* excluded)
      (let ((all-visible-pcs (visible-players-in-state-from-faction state
                                                                    game-state:+npc-type+
                                                                    :alive-only t)))
        ;; note:   even   if   we    use   alive-only   equal   to   t
        ;; game-state:loop-player-entities  will   skip  dead  players
        ;; anyway
        (game-state:loop-player-entities state
                                         #'(lambda (a)
                                             (when (find-if #'(lambda (ent) (= (id ent) (id a)))
                                                            all-visible-pcs)
                                               (blackboard:log-pc-entity-presence blackboard a))))
        (game-state:loop-ai-entities state
                                     #'(lambda (a)
                                         (blackboard:log-ai-entity-presence blackboard a)))))))

(defun %clean-plan-and-blacklist (entity &rest excluded)
  (with-accessors ((ghost ghost)) entity
    (%clean-plan entity excluded)
    ;; clear blacklisted actions for planner
    #+(and debug-mode debug-ai) (misc:dbg "clear planner cache and blacklist")
    (clear-blacklist ghost)))

(defmacro with-maybe-blacklist ((entity strategy-decision action
                                        &key (ignore-points-difference nil))
                                &body body)
  (with-gensyms (saved-mp saved-sp ghost state world)
    `(with-accessors ((,state state)
                      (,ghost ghost)) ,entity
       (game-state:with-world (,world ,state)
         (let ((,saved-mp (current-movement-points ,ghost))
               (,saved-sp (current-spell-points    ,ghost)))
           (declare (ignorable ,saved-mp ,saved-sp))
           ;; (clear-blacklist-if-cost-occurred ,entity)
           ,@body
           ,(when (not ignore-points-difference)
              `(action-scheduler:with-enqueue-action-and-send-remove-after
                   (,world action-scheduler:tactical-plane-action)
                 (with-maybe-set-cost-occurred (,entity ,saved-mp ,saved-sp))))
           (when (action-terminal-p ,ghost ,action)
             #+(and debug-mode debug-ai)
             (misc:dbg "~a is terminal in ~a!" ,action (original-current-plan (ghost ,entity)))
             (action-scheduler:with-enqueue-action-and-send-remove-after
                 (,world action-scheduler:tactical-plane-action)
               (with-blacklist-action-if-no-cost (,entity ,strategy-decision ,action)))))))))

;; used as parallel thread
(defun update-all-blackboard-infos (entity)
  (with-accessors ((state state)) entity
    (with-accessors ((blackboard game-state:blackboard)) state
      #+ (and debug-mode debug-ai)
      (game-state:with-world (world state)
        (world::render-influence-map world))
      (blackboard::%update-all-infos blackboard)
      entity)))

;; used as parallel thread
;; yes it is the same as above...
(defun update-blackboard-infos (entity)
  (with-accessors ((state state)) entity
    (with-accessors ((blackboard game-state:blackboard)) state
      #+ (and debug-mode debug-ai)
      (game-state:with-world (world state)
        (world::render-influence-map world))
      (blackboard::%update-all-infos blackboard)
      entity)))

;;;; planning

(defmacro defact-plan (arg &rest body)
  (let* ((function-name (alexandria:format-symbol t "~:@(actuate-plan~)")))
    `(defmethod ,function-name (,@arg)
       (clear-blacklist-if-cost-occurred object)
       ,@body)))

(defgeneric actuate-plan (object strategy action))

(defun spawn-update-infos-task (mesh update-fn)
  (with-accessors ((state state)) mesh
    (game-state:with-world (world state)
      (widget:activate-planner-icon world)
      (setf *update-infos-channel* (lparallel:make-channel))
      (lparallel:submit-task *update-infos-channel* update-fn mesh))))

(defun get-presence-log-data (blackboard key)
  (ai-logger:ai-log-data (ai-logger:get-log blackboard key)))

(defun get-ai-presence-log-data (blackboard)
  (ai-logger:ai-log-data (ai-logger:get-log blackboard
                                            ai-logger:+ai-log-ai-entity-presence+)))

(defun get-pc-presence-log-data (blackboard)
  (ai-logger:ai-log-data (ai-logger:get-log blackboard
                                            ai-logger:+ai-log-pc-entity-presence+)))

(defmacro with-spawn-if-presence-diff ((entity update-fn) &body body)
  (with-gensyms (state blackboard old-pc-log old-ai-log
                       new-pc-log new-ai-log no-diff-pres-p)
    `(with-accessors ((,state state)) ,entity
       (with-accessors ((,blackboard game-state:blackboard)) ,state
         (let ((,old-pc-log (get-pc-presence-log-data ,blackboard))
               (,old-ai-log (get-ai-presence-log-data ,blackboard)))
           ,@body
           (let* ((,new-pc-log     (get-pc-presence-log-data ,blackboard))
                  (,new-ai-log     (get-ai-presence-log-data ,blackboard))
                  (,no-diff-pres-p (and (= (length ,old-pc-log) (length ,new-pc-log))
                                        (= (length ,old-ai-log) (length ,new-ai-log))
                                        (null (set-difference ,old-pc-log ,new-pc-log
                                                              :test
                                                              #'ai-logger:equal-presence-p))
                                        (null (set-difference ,old-ai-log ,new-ai-log
                                                              :test
                                                              #'ai-logger:equal-presence-p)))))
             #+(and nil debug-mode debug-ai)
             (misc:dbg (text-utils:strcat "diff ~a old-pc ~a new-pc ~a  old-ai ~a "
                                          "new-ai ~a diff-pc ~a diff-ai ~a")
                        ,no-diff-pres-p
                        ,old-pc-log ,new-pc-log
                        ,old-ai-log ,new-ai-log
                        (set-difference ,old-pc-log ,new-pc-log
                                        :test #'ai-logger:equal-presence-p)
                        (set-difference ,old-ai-log ,new-ai-log
                                        :test #'ai-logger:equal-presence-p))
             (when (not ,no-diff-pres-p)
               (spawn-update-infos-task ,entity ,update-fn))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+plan-stopper+)))
  (with-spawn-if-presence-diff (object 'update-blackboard-infos)
    (%clean-plan object (list :ai-tree))))


(defun %get-interrupt-entity (state id)
  (and id
       (numberp id)
       (find-entity-by-id state id)))

(defun ignore-interrupt-p (npc)
  (with-accessors ((state state)
                   (ghost ghost)) npc
    (let* ((interrupting-id        (get-from-working-memory npc
                                                            +w-memory-interrupting-by-id+))
           (interrupting-entity    (%get-interrupt-entity state interrupting-id))
           (saved-interrupt-id     (get-from-working-memory npc
                                                            +w-memory-saved-interrupting-id+))
           (saved-interrupt-entity (%get-interrupt-entity state saved-interrupt-id)))
      (cond
        ((null interrupting-entity)
         nil)
        ((door-mesh-shell-p interrupting-entity) ;; opening a door
         (let* ((all-visible-now    (ai-utils:all-visible-opponents-id npc))
                (all-visible-before (get-memory-seen-before-door npc)))
           (if (visible-more-p all-visible-before
                               all-visible-now)
               nil
               (progn
                 #+(and debug-mode debug-ai)
                 (misc:dbg "interrupt: ignoring opening door")
                 t))))
        ((trap-mesh-shell-p interrupting-entity)
         #+(and debug-mode debug-ai)
         (misc:dbg "interrupt: ignoring after trap")
         t)
        ((and (get-memory-ignore-interrupt-from npc)
              (= (get-memory-ignore-interrupt-from npc)
                 interrupting-id))
         #+(and debug-mode debug-ai)
         (misc:dbg "interrupt: ignoring as the same character is met")
         t)
        ((null saved-interrupt-entity)
         nil)
        ((/= (id interrupting-entity)
             (id saved-interrupt-entity))
         nil)
        (t
         #+(and debug-mode debug-ai)
         (misc:dbg "interrupt: ignored")
         t)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+interrupt-action+)))
  (with-accessors ((state state)
                   (ghost ghost)) object
    (let ((interrupting-id (get-from-working-memory object +w-memory-interrupting-by-id+)))
      (with-spawn-if-presence-diff (object 'update-all-blackboard-infos)
        (%clean-plan-and-blacklist object))
      (put-in-working-memory object +w-memory-saved-interrupting-id+ interrupting-id))))

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
      (%clean-plan-and-blacklist object :ai-tree)
      ;; awake other AI player
      (and ai-entities-action-order
           (pop ai-entities-action-order)))))

;; general

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+rotate-action+)))
  (with-maybe-blacklist (object strategy action :ignore-points-difference t)
    (with-accessors ((state state)
                     (ghost ghost)) object
      (ai-utils:if-difficult-level>medium (state)
          (let ((help-rotating-needed-p (< (character:current-movement-points ghost)
                                           (d* 4.0 +rotate-entity-cost-cost+))))
            (when help-rotating-needed-p
              (setf (character:current-movement-points ghost)
                    (d* 4.0 +rotate-entity-cost-cost+)))
            (%rotate-until-someone-visible state object :decrement-movement-points t)
            (when help-rotating-needed-p
              (setf (character:current-movement-points ghost) 0.0)))
        (%rotate-until-someone-visible state object :decrement-movement-points t)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+hide-action+)))
  (with-slots-for-reasoning (object state ghost blackboard)
    (game-state:with-world (world state)
      (let ((all-opponents-can-see-me (blackboard:all-other-factions-can-see-entity state
                                                                                    object)))
        (when-let ((hiding-tile (ai-utils:go-find-hiding-place object
                                                               all-opponents-can-see-me)))
          #+(and debug-mode debug-ai) (dbg "hiding tile chosen ~a" hiding-tile)
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
        #- debug-mode (declare (ignore fountain))
        #+ debug-mode (assert fountain)
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
                         strategy
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
          (let* ((defender-id (ai-utils:faction-attackable-opponents-id blackboard object)))
            (battle-utils:attack-w-current-weapon object
                                                  (find-entity-by-id state defender-id))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-near-to-enemy-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-slots-for-reasoning (object state ghost blackboard)
      (game-state:with-world (world state)
        (multiple-value-bind (path total-cost costs id-target)
            (blackboard:best-path-near-enemy-pos-w-current-weapon blackboard
                                                                  object
                                                                  :cut-off-first-tile nil)
          (declare (ignore costs))
          #+(and debug-mode debug-ai) (assert path)
          #+(and debug-mode debug-ai)
          (misc:dbg "interrupt from ~a will be ignored"
                    (find-entity-by-id state id-target))
          (set-memory-ignore-interrupt-from object id-target)

          (when path
            (let* ((path-struct (game-state:make-movement-path path total-cost)))
              (%do-simple-move object path-struct state world))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-near-to-enemy-insecure-action+)))
  (with-maybe-blacklist (object strategy action)
    (with-slots-for-reasoning (object state ghost blackboard)
      (game-state:with-world (world state)
        (multiple-value-bind (path total-cost costs id-target)
            (blackboard:insecure-path-near-enemy-pos-w-current-weapon blackboard
                                                                      object
                                                                      :cut-off-first-tile nil)
          (declare (ignore costs))
          #+(and debug-mode debug-ai) (assert path)
          #+(and debug-mode debug-ai)
          (misc:dbg "interrupt from ~a will be ignored"
                    (find-entity-by-id state id-target))
          (set-memory-ignore-interrupt-from object id-target)
          (when path
            (let* ((path-struct (game-state:make-movement-path path total-cost)))
              (%do-simple-move object path-struct state world))))))))

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
            #+(and debug-mode debug-ai)
             (misc:dbg "rotation end with seen in ~a times" count)
             (when (and (> count 0)
                        ,set-cost-occurred-p)
               #+(and debug-mode debug-ai) (misc:dbg "setting cost!")
               (set-cost-occurred ,entity))
               ,count)
           (t
            (let ((,event (make-instance 'game-event:rotate-entity-ccw-event
                                         :id-destination            (id ,entity)
                                         :decrement-movement-points ,decrement-movement-points)))
              (action-scheduler:with-enqueue-action-and-send-remove-after
                  (,world action-scheduler:tactical-plane-action)
                #+(and debug-mode debug-ai)
                (misc:dbg "can't see an enemy, rotating (~a)" count)
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
    (when (not (get-memory-entity-goal-vanished object))
      (with-slots-for-reasoning (object state ghost blackboard)
        (let* ((id-defender (get-from-working-memory object +w-memory-target-id+))
               (defender    (find-entity-by-id state id-defender)))
          #+(and debug-mode debug-ai) (misc:dbg "atk id ~a" id-defender)
          (%rotate-until-visible state object defender)
          (battle-utils:attack-w-current-weapon object
                                                (find-entity-by-id state
                                                                   id-defender)))))))

(defun %do-simple-move (mesh path-struct state world)
  (action-scheduler:with-enqueue-action (world action-scheduler:tactical-plane-action)
    (setf (game-state:selected-path state) path-struct)
    (let* ((tiles          (game-state:tiles (game-state:selected-path state)))
           (cost           (game-state:cost  (game-state:selected-path state)))
           (movement-event (make-instance 'game-event:move-entity-along-path-event
                                          :path           tiles
                                          :cost           cost
                                          :id-destination (id mesh))))
      #+ (and debug-mode debug-ai) (misc:dbg "go to ~a" tiles)
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
            #+(and debug-mode debug-ai)
            (misc:dbg "path-struct go ~a" (game-state:tiles path-struct))
            (%do-simple-move object path-struct state world)))))))

(defun common-go-for-any-spell (entity strategy action target-finder-fn)
  (with-maybe-blacklist (entity strategy action)
    (with-slots-for-reasoning (entity state ghost blackboard)
      (game-state:with-world (world state)
        (multiple-value-bind (target-entity spell position)
            (funcall target-finder-fn entity)
          (multiple-value-bind (path total-cost costs)
              (blackboard:path-with-concerning-tiles blackboard
                                                     (calculate-cost-position entity)
                                                     position
                                                     :cut-off-first-tile  nil
                                                     :allow-path-length-1 nil)
            (declare (ignore costs))
            #+(and debug-mode debug-ai) (misc:dbg "path spell go ~a" path)
            (put-in-working-memory entity +w-memory-target-id+ (id target-entity))
            (put-in-working-memory entity +w-memory-spell+     spell)
            (when path ; if no move is needed path values nil
              (let* ((path-struct (game-state:make-movement-path path total-cost)))
                (%do-simple-move entity path-struct state world)))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-to-attack-spell-pos-action+)))
    (common-go-for-any-spell object strategy action
                           #'ai-utils:reachable-attackable-opponents-attack-spell))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-to-damage-spell-pos-action+)))
    (common-go-for-any-spell object strategy action
                           #'ai-utils:reachable-attackable-opponents-damage-spell))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+go-to-heal-spell-pos-action+)))
  (common-go-for-any-spell object strategy action
                           #'ai-utils:reachable-healing-friend-heal-spell))

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

(defun %common-find-attack-pos (npc path-building-fn reachable-fn)
  (flet ((put-path-in-memory (target-id path-struct)
           (put-in-working-memory npc +w-memory-path-struct+ path-struct)
           (put-in-working-memory npc +w-memory-target-id+   target-id)))
    (with-slots-for-reasoning (npc state ghost blackboard)
      (let* ((target-next  (blackboard:entity-in-valid-attackable-pos-p npc)))
        (if target-next ;; if non nil we are in an attack position
            (put-path-in-memory (id target-next) nil)
            (multiple-value-bind (path total-cost costs target-id-move)
                (funcall path-building-fn blackboard
                         npc
                         :cut-off-first-tile nil
                         :reachable-fn-p     reachable-fn)
              (declare (ignore costs))
              (let ((entity-attacked (find-entity-by-id state target-id-move)))
                ;; entity-attacked is nil the attack position is valid no more
                ;; this could happens if attacked PC moved from there
                ;; or the game has been reloaded :(
                (when (null entity-attacked)
                  (set-memory-entity-goal-vanished npc)
                  (blackboard:remove-all-attacks-with-id blackboard target-id-move))
                #+ (and debug-mode debug-ai)
                (misc:dbg "path attack ~a ~a" path total-cost)
                #+(and debug-mode debug-ai)
                (misc:dbg "interrupt from ~a will be ignored"
                          (find-entity-by-id state target-id-move))
                (set-memory-ignore-interrupt-from npc target-id-move)
                (let* ((path-struct (game-state:make-movement-path path total-cost)))
                  (put-path-in-memory target-id-move path-struct)))))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+find-attack-pos-action+)))
  (with-accessors ((state state)) object
    (with-accessors ((blackboard game-state:blackboard)) state
      (%common-find-attack-pos object
                               #'blackboard:best-path-to-reach-attack-pos-w-current-weapon
                               (blackboard:reachable-p-w/concening-tiles-fn blackboard)))))

(defun insecure-path-to-reach-attack-pos (blackboard
                                          player
                                          &key
                                          (cut-off-first-tile nil)
                                          (reachable-fn-p     nil))
  (blackboard:insecure-path-to-reach-attack-pos-w-current-weapon blackboard
                                                                 player
                                                                 :cut-off-first-tile
                                                                 cut-off-first-tile
                                                                 :reachable-fn-p
                                                                 reachable-fn-p))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+find-attack-pos-insecure-action+)))
  (with-accessors ((state state)) object
    (with-accessors ((blackboard game-state:blackboard)) state
      (%common-find-attack-pos object
                               #'insecure-path-to-reach-attack-pos
                               (blackboard:reachable-p-w/o-concening-tiles-fn blackboard)))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+load-weapon-action+)))
  ;; TODO
  )

(defun common-launch-spell (launcher strategy action spell-launch-fn)
  (with-maybe-blacklist (launcher strategy action)
    (with-accessors ((state state)) launcher
      (game-state:with-world (world state)
        (let* ((target-id (get-from-working-memory launcher +w-memory-target-id+))
               (spell     (get-from-working-memory launcher +w-memory-spell+))
               (target    (find-entity-by-id state target-id)))
          #+(and debug-mode debug-ai) (misc:dbg "common launch spell: ~a" spell)
          (%rotate-until-visible state launcher target :decrement-movement-points nil)
          (action-scheduler:with-enqueue-action-and-send-remove-after
              (world action-scheduler:tactical-plane-action)
            (funcall spell-launch-fn launcher target spell)))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-attack-spell-action+)))
  (common-launch-spell object strategy action #'ai-utils:go-launch-attack-spell*))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-damage-spell-action+)))
  (common-launch-spell object strategy action #'ai-utils:go-launch-spell*))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+launch-heal-spell-action+)))
  (common-launch-spell object strategy action #'ai-utils:go-launch-spell*))

(defun common-prepare-launch-spell-curr-pos (entity strategy action
                                             spell-finder-fn
                                             target-finder-fn)
  (with-maybe-blacklist (entity strategy action)
    (with-slots-for-reasoning (entity state ghost blackboard)
      (let ((available-spells (funcall spell-finder-fn entity)))
        #+debug-mode (assert available-spells)
        (multiple-value-bind (target-entity spell)
            (funcall target-finder-fn available-spells entity)
          #+debug-mode (assert target-entity)
          #+(and debug-mode debug-ai) (misc:dbg "path spell stay ~a" spell)
          (put-in-working-memory entity +w-memory-target-id+ (id target-entity))
          (put-in-working-memory entity +w-memory-spell+     spell)
          (%rotate-until-visible state entity target-entity :decrement-movement-points nil))))))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+attack-spell-curr-pos-action+)))
  (common-prepare-launch-spell-curr-pos object strategy action
                                        #'ai-utils:available-attack-spells
                                        #'ai-utils:attackable-opponents-attack-spell))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+damage-spell-curr-pos-action+)))
  (common-prepare-launch-spell-curr-pos object strategy action
                                        #'ai-utils:available-damage-spells
                                        #'ai-utils:attackable-opponents-damage-spell))

(defmethod actuate-plan ((object md2-mesh)
                         strategy
                         (action (eql ai-utils:+heal-spell-curr-pos-action+)))
  (common-prepare-launch-spell-curr-pos object strategy action
                                        #'ai-utils:available-heal-spells
                                        #'ai-utils:healable-friend-heal-spell))

;;;; strategy
(defun actuate-strategy (mesh)
  (with-slots-for-reasoning (mesh state ghost blackboard)
    (game-state:with-world (world state)
      (with-accessors ((ai-entities-action-order game-state:ai-entities-action-order)) state
        (when (and (eq  (my-faction    mesh) game-state:+npc-type+)
                   (eq  (my-faction    mesh) (game-state:faction-turn state))
                   (not (entity-dead-p mesh))
                   (world:actions-queue-empty-p world) ; ensure one action at time
                   ghost)
          (if (null ai-entities-action-order)       ; if nil all ai players made a move
              (game-state:increase-game-turn state) ; turn is terminated for AI
              (when   (and (= (id mesh)
                              (id (first-elt ai-entities-action-order)))) ;; ensure it's my turn
                (flet ((spawn-planner-task ()
                         #+ (and debug-mode debug-ai) (misc:dbg "spawn planner for ~a" mesh)
                         (world:toolbar-selected-action world)
                         (widget:activate-planner-icon  world)
                         (setf *planner-channel* (lparallel:make-channel))
                         (lparallel:submit-task *planner-channel*
                                                'elaborate-current-tactical-plan
                                                ghost blackboard mesh nil))
                       (terminate-planner-task ()
                         (widget:deactivate-planner-icon world)
                         (setf *planner-channel* nil)
                         (let ((action (pop-action-plan ghost)))
                           #+(and debug-mode debug-ai) (misc:dbg "popped action ~a" action)
                           (let ((strategy (blackboard:strategy-decision blackboard)))
                             (actuate-plan mesh strategy action))))
                       (check-ending (channel)
                         (lparallel:try-receive-result channel
                                                       :timeout +channel-planner-timeout+)))
                  (if (not *planner-channel*) ;; no planning
                      (if (not *update-infos-channel*) ; no  updating
                          (spawn-planner-task)
                          (when (check-ending *update-infos-channel*)
                            (setf *update-infos-channel* nil)))
                      (when (check-ending *planner-channel*)
                        (terminate-planner-task)))))))))))
