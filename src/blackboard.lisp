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

(in-package :blackboard)

(define-constant +goal-attack-occupied-value+             100.0 :test #'=)

(define-constant +explored-tile-value+                    100.0 :test #'=)

(define-constant +unexplored-tile-value+                    0.0 :test #'=)

(define-constant +concerning-tile-value+ +explored-tile-value+ :test #'=)

(define-constant +attack-nongoal-tile-value+                5.0 :test #'=)

(define-constant +length-line-of-sight-inf+              1000.0 :test #'=)

(define-constant +goal-tile-value+                          0.0 :test #'=)

(define-constant +concerning-tiles-cost->graph-scaling+     3.0 :test #'=)

(define-constant +min-concerning-invalicable-threshold+    75.0 :test #'=)

(define-constant +max-concerning-invalicable-threshold+    99.0 :test #'=)

(define-constant +concerning-tiles-invalicables-inc-step+  10.0 :test #'=)

(define-constant +concerning-tiles-min-value+               0.0 :test #'=)

(define-constant +landmark-numbers+                         4  :test #'=)

(defmacro with-conc-path-total-cost ((total-cost) path-calculating-exp &body body)
  "Use with 'path-with-concerning-tiles' only"
  (with-gensyms (path costs)
    `(multiple-value-bind (,path ,total-cost ,costs)
         ,path-calculating-exp
       (declare (ignore ,path ,costs))
       ,@body)))

(defun reachablep (atk-pos def-pos mp)
  (<= (map-manhattam-distance atk-pos def-pos)
      mp))

(defun reachable-constantly-t (atk-pos def-pos mp)
  (declare (ignore atk-pos def-pos mp))
  t)

(defparameter *reachable-p-fn* #'reachablep)

(definline cost-limit-concerning-tiles ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (d/ +invalicable-element-cost+
      10.0))

(defclass entity-taker ()
  ((entity-id
    :initform nil
    :initarg :entity-id
    :accessor entity-id)))

(defmethod marshal:class-persistant-slots ((object entity-taker))
  '(entity-id))

(defclass def-target (entity-taker)
  ((goal-pos
    :initform '()
    :initarg  :goal-pos
    :accessor goal-pos)
   (age
    :initform 1
    :initarg  :age
    :accessor age)))

(defmethod marshal:class-persistant-slots ((object def-target))
  (append '(goal-pos
            age)
          (call-next-method)))

(defgeneric max-attackers (object))

(defmethod max-attackers ((object def-target))
  (length (goal-pos object)))

(defmethod print-object ((object def-target) stream)
  (format stream "~a[~a] age ~a" (entity-id object) (goal-pos object) (age object)))

(defun make-defender-target (id age &optional (goal-pos '()))
  (make-instance 'def-target :entity-id id :goal-pos goal-pos :age age))

(defclass attacker (entity-taker)
  ((target-id
    :initform nil
    :initarg  :target-id
    :accessor target-id)
   (target-pos
    :initform nil
    :initarg  :target-pos
    :accessor target-pos)))

(defmethod print-object ((object attacker) stream)
  (format stream "~a -> ~a@~a" (entity-id object) (target-id object) (target-pos object)))

(defmethod marshal:class-persistant-slots ((object attacker))
  (append '(target-id
            target-pos)
          (call-next-method)))

(defun make-attacker-instance (target-pos defender-id attacker-id)
  (make-instance 'attacker
                 :target-pos target-pos
                 :target-id  defender-id
                 :entity-id  attacker-id))

(defclass blackboard (ai-logger:ai-logger)
  ((main-state
    :initform nil
    :initarg  :main-state
    :accessor main-state
    :type     game-state)
   (number-pc@start-turn
    :initform (list 0 0)
    :initarg  :number-pc@start-turn
    :accessor number-pc@start-turn
    :type     list)
   (number-npc@start-turn
    :initform (list 0 0)
    :initarg  :number-npc@start-turn
    :accessor number-npc@start-turn
    :type     list)
   (exhausted-fountains-ids
    :initform '()
    :initarg  :exhausted-fountains-ids
    :accessor exhausted-fountains-ids)
   (visited-tiles
    :initform nil
    :initarg  :visited-tiles
    :accessor visited-tiles
    :type matrix
    :documentation "matrix element @ (x, y) is non nil if the tile has been visited,
                   false otherwise")
   (per-turn-visited-tiles
    :initform '()
    :initarg  :per-turn-visited-tiles
    :accessor per-turn-visited-tiles
    :type list
    :documentation "an alist (entity . matrix-of-visited-tiles), see slot visited-tiles")
   (unexplored-layer
    :initform nil
    :initarg  :unexplored-layer
    :accessor unexplored-layer
    :type     dijkstra-layer)
   (concerning-tiles
    :initform nil
    :initarg  :concerning-tiles
    :accessor concerning-tiles
    :type     matrix)
   (concerning-tiles-invalicables
    :initform nil
    :initarg  :concerning-tiles-invalicables
    :accessor concerning-tiles-invalicables
    :type     matrix
    :documentation "the concerning tiles near a NPC (for unexplored-layer only)")
   (concerning-tiles-facing
    :initform nil
    :initarg  :concerning-tiles-facing
    :accessor concerning-tiles-facing
    :type     matrix
    :documentation "the concerning tiles in front o a NPC")
   (concerning-tiles-invalicable-threshold
    :initform +min-concerning-invalicable-threshold+
    :initarg  :concerning-tiles-invalicable-threshold
    :accessor concerning-tiles-invalicable-threshold)
   (attack-enemy-melee-positions
    :initform '()
    :initarg  :attack-enemy-melee-positions
    :accessor attack-enemy-melee-positions
    :documentation   "This  holds   the   positions   AI  with   melee
    weapon (except pole  weapon) should reach to attack  the enemy (it
    is a list of def-target instances)")
   (attack-enemy-pole-positions
    :initform '()
    :initarg  :attack-enemy-pole-positions
    :accessor attack-enemy-pole-positions
    :documentation  "This  holds the  positions  AI  with pole  weapon
    should  reach to  attack the  enemy (it  is a  list of  def-target
    instances)")
   (attack-enemy-bow-positions
    :initform '()
    :initarg  :attack-enemy-bow-positions
    :accessor attack-enemy-bow-positions
    :documentation "This holds the positions AI with bow weapon should
    reach to attack the enemy (it is a list of def-target instances)")
   (attack-enemy-crossbow-positions
    :initform '()
    :initarg  :attack-enemy-crossbow-positions
    :accessor attack-enemy-crossbow-positions
    :documentation "This  holds the positions AI  with crossbow weapon
    should  reach to  attack the  enemy (it  is a  list of  def-target
    instances)")
   (main-influence-map
    :initform nil
    :initarg  :main-influence-map
    :accessor main-influence-map
    :type     matrix
    :documentation "This  is the influence map. Influence is based on
    character:calculate-influence")
   (landmarks-dists
    :initform (make-array-frame +landmark-numbers+ 'matrix t)
    :initarg  :landmarks-dists
    :accessor landmarks-dists
    :documentation "A vector of matrix,  each matrix holds the minimum
    path from all the tiles to a chosen landmarks (for path of the PCs
    i.e. no concerning tiles and doors")
   (landmarks-w/concenring-no-doors-dists
    :initform (make-array-frame +landmark-numbers+ 'matrix t)
    :initarg  :landmarks-w/concenring-no-doors-dists
    :accessor landmarks-w/concenring-no-doors-dists
    :documentation "A vector of matrix,  each matrix holds the minimum
    path from  all the  tiles to  a chosen  landmarks (for  paths with
    concerning tiles and no doors, usually for NPC)")
   (landmarks-w/o-concenring-no-doors-dists
    :initform (make-array-frame +landmark-numbers+ 'matrix t)
    :initarg  :landmarks-w/o-concenring-no-doors-dists
    :accessor landmarks-w/o-concenring-no-doors-dists
    :documentation "A vector of matrix,  each matrix holds the minimum
    path from  all the  tiles to  a chosen  landmarks (for  paths *without*
    concerning tiles and without doors, usually for NPC)")
   (use-enemy-fov-when-exploring
    :initform nil
    :initarg  :use-enemy-fov-when-exploring-p
    :reader   use-enemy-fov-when-exploring-p
    :writer   (setf use-enemy-fov-when-exploring))
   (use-enemy-fov-when-attacking
    :initform nil
    :initarg  :use-enemy-fov-when-attacking-p
    :reader   use-enemy-fov-when-attacking-p
    :writer   (setf use-enemy-fov-when-attacking))
   (attack-enemy-melee-layer
    :initform nil
    :initarg  :attack-enemy-melee-layer
    :accessor attack-enemy-melee-layer
    :type     dijkstra-layer
    :documentation "just for debugging")
   (attack-enemy-pole-layer
    :initform nil
    :initarg  :attack-enemy-pole-layer
    :accessor attack-enemy-pole-layer
    :type     dijkstra-layer
    :documentation "just for debugging")
   (attack-enemy-bow-layer
    :initform nil
    :initarg  :attack-enemy-bow-layer
    :accessor attack-enemy-bow-layer
    :type     dijkstra-layer
    :documentation "just for debugging")
   (attack-enemy-crossbow-layer
    :initform nil
    :initarg  :attack-enemy-crossbow-layer
    :accessor attack-enemy-crossbow-layer
    :type     dijkstra-layer
    :documentation "just for debugging")))

(defun init-logs (blackboard)
  (with-accessors ((logs ai-logger:logs)) blackboard
    (let ((pc-presence-log (ai-logger:make-ai-log :clean-trigger
                                                  ai-logger:+ai-log-clean-end-plan+))
          (ai-presence-log (ai-logger:make-ai-log :clean-trigger
                                                  ai-logger:+ai-log-clean-end-plan+)))
      (setf (gethash ai-logger:+ai-log-pc-entity-presence+ logs) pc-presence-log)
      (setf (gethash ai-logger:+ai-log-ai-entity-presence+ logs) ai-presence-log)))
  blackboard)

(defmethod initialize-instance :after ((object blackboard) &key &allow-other-keys)
  (with-accessors ((visited-tiles                 visited-tiles)
                   (concerning-tiles              concerning-tiles)
                   (concerning-tiles-facing       concerning-tiles-facing)
                   (concerning-tiles-invalicables concerning-tiles-invalicables)
                   (unexplored-layer              unexplored-layer)
                   (attack-enemy-melee-layer      attack-enemy-melee-layer)
                   (attack-enemy-pole-layer       attack-enemy-pole-layer)
                   (attack-enemy-bow-layer        attack-enemy-bow-layer)
                   (attack-enemy-crossbow-layer   attack-enemy-crossbow-layer)
                   (use-enemy-fov-when-exploring  use-enemy-fov-when-exploring)
                   (use-enemy-fov-when-attacking  use-enemy-fov-when-attacking)
                   (main-influence-map            main-influence-map)
                   (main-state                    main-state)) object
    (init-logs object)
    (let ((wmap (width  (map-state main-state)))
          (hmap (height (map-state main-state))))
      (setf visited-tiles                 (make-matrix wmap hmap nil))
      (setf concerning-tiles              (make-matrix wmap hmap +concerning-tiles-min-value+))
      (setf concerning-tiles-facing       (make-matrix wmap hmap +concerning-tiles-min-value+))
      (setf concerning-tiles-invalicables (make-matrix wmap hmap +concerning-tiles-min-value+))
      (setf unexplored-layer              (inmap:make-dijkstra-layer wmap hmap
                                                                     +unexplored-tile-value+))
      (setf attack-enemy-melee-layer
            (inmap:make-dijkstra-layer wmap hmap +attack-nongoal-tile-value+))
      (setf attack-enemy-pole-layer
            (inmap:make-dijkstra-layer wmap hmap +attack-nongoal-tile-value+))
      (setf attack-enemy-bow-layer
            (inmap:make-dijkstra-layer wmap hmap +attack-nongoal-tile-value+))
      (setf attack-enemy-crossbow-layer
            (inmap:make-dijkstra-layer wmap hmap +attack-nongoal-tile-value+))
      (reset-per-turn-visited-tiles object)
      (%update-influence-map object)
      ;; setting smartness based on level
      (when (>= (level-difficult main-state) +difficult-medium+)
        (setf use-enemy-fov-when-exploring nil
              use-enemy-fov-when-attacking t)))))

(defun %update-influence-map (blackboard)
  "just for debugging"
  (with-accessors ((main-influence-map main-influence-map)) blackboard
    (setf main-influence-map (strategic-ai:faction-influence-map blackboard +pc-type+))
    ;; (let ((matrix:*truncate-matrix-value-when-printing* t))
    ;;   (misc:dbg "inf dump~%~a~%presence pc ~a npc ~a dbg ~a ~a avg dist ~a ~%vuln ~a~%vis ~a"
    ;;             main-influence-map
    ;;             (strategic-ai:faction-map-under-control main-influence-map +pc-type+)
    ;;             (strategic-ai:faction-map-under-control main-influence-map +npc-type+)
    ;;             (strategic-ai:dmg-points-ratio    blackboard +pc-type+)
    ;;             (strategic-ai:dmg-points-ratio     blackboard +npc-type+)
    ;;             (strategic-ai:average-cost-faction blackboard +npc-type+)
    ;;             (strategic-ai:entities-vulnerables blackboard +pc-type+)
    ;;             (strategic-ai:visible-opponents    blackboard +pc-type+)))
    blackboard))

(defmethod make-influence-map ((object blackboard) &key (matrix nil))
  (with-accessors ((main-influence-map main-influence-map)
                   (main-state         main-state))        object
    (make-influence-map main-state :matrix matrix)))

(defun update-concerning-zones-around-entity (entity)
  (with-accessors ((ghost ghost)
                   (dir dir)
                   (id id)
                   (state state)) entity
    (with-player-cost-pos (entity x y)
      (let* ((difficult        (game-state:level-difficult state))
             (danger-zone-size (max 2 (blackboard:calc-danger-zone-size difficult)))
             (concerning-fn    (concerning-tile-default-mapping-clsr x y
                                                                     danger-zone-size)))
        (game-state:set-concerning-tile-fn state
                                           x
                                           y
                                           :danger-zone-size   danger-zone-size
                                           :concerning-tile-fn concerning-fn)))))

(defun add-tail-concerning-zone (attacker defender)
  (with-accessors ((ghost ghost)
                   (dir dir)
                   (id id)
                   (state state)) defender
    (with-accessors ((blackboard blackboard:blackboard)) state
      (with-accessors ((concerning-tiles blackboard:concerning-tiles)) blackboard
        (let* ((pos-entity-goal  (mesh:calculate-cost-position defender))
               (pos-entity-start (mesh:calculate-cost-position attacker))
               (difficult        (game-state:level-difficult state))
               (tail             (2d-utils:segment pos-entity-start pos-entity-goal
                                                   :antialiasp nil
                                                   :width      1))
               (cost             (d* (blackboard:calc-concerning-tiles-cost-scaling
                                      difficult)
                                     +open-terrain-cost+)))
          (when (> difficult 3)
            (loop for point in tail do
                 (let ((coord (elt point 0)))
                   (2d-utils:displace-2d-vector (coord x y)
                     (incf (matrix:matrix-elt concerning-tiles y x)
                           cost))))))))))

(defun decrease-concerning (game-state concerning-map)
  (let* ((level           (level-difficult game-state))
         (decrease-rate   (d  (/ 1 level)))
         (decrease-amount (d/ +concerning-tile-value+ 3.0)))
    (ploop-matrix (concerning-map x y)
      (let ((old-value (matrix-elt concerning-map y x)))
        (when (and old-value
                   (d> old-value +concerning-tiles-min-value+))
          (let ((new-value (d- old-value
                               (d* decrease-rate decrease-amount))))
            (if (d> new-value +concerning-tiles-min-value+)
                (setf (matrix-elt concerning-map y x) new-value)
                (setf (matrix-elt concerning-map y x) +concerning-tiles-min-value+))))))))

(defun decrease-concerning-invalicables (game-state map)
  (with-accessors ((blackboard blackboard)) game-state
    (decrease-concerning game-state map)
    (update-concerning-invalicable blackboard)))

(defun calc-def-target-age (state)
  (truncate (* (level-difficult state) 1.5)))

(defun remove-attack-starved (blackboard positions)
  (with-accessors ((main-state main-state)) blackboard
    (let* ((all-visibles-ids (all-player-id-visible-from-ai main-state))
           (age-updated      (mapcar #'(lambda (a) ;; decrease age of non visible pos
                                         (if (not (find (entity-id a) all-visibles-ids
                                                        :test #'=))
                                             ;; not visible
                                             (setf (age a) (1- (age a)))
                                             ;; visible
                                             (progn
                                               (setf (age a)
                                                     (calc-def-target-age main-state))
                                               (setf (goal-pos a) nil)))
                                         a)
                                     positions))
           (not-starved      (remove-if #'(lambda (a) (<= (age a) 0)) ;; remove starved
                                        age-updated)))
      ;; remove death
      (remove-if #'(lambda (a)
                     (let ((ent (game-state:find-entity-by-id main-state (entity-id a))))
                       (entity:entity-dead-p ent)))
                 not-starved))))

(defun someone-ai-carry-weapon-fn (blackboard probe-fn)
  (with-accessors ((main-state main-state)) blackboard
    (loop-ai-entities main-state
         #'(lambda (a) (when (funcall probe-fn a)
                         (return-from someone-ai-carry-weapon-fn t)))))
  nil)

(defun update-all-attacking-pos (blackboard)
  (flet ((carry-test (fn)
           (someone-ai-carry-weapon-fn blackboard #'(lambda (a) (funcall fn (ghost a))))))
    (when (carry-test #'character:weapon-type-pole-p)
      (update-pole-attackable-pos     blackboard))
    (when (carry-test #'(lambda (g) (or (character:weapon-type-impact-p g)
                                        (character:weapon-type-edge-p g))))
      (update-melee-attackable-pos    blackboard))
    (when (carry-test #'character:weapon-type-bow-p)
      (update-bow-attackable-pos      blackboard))
    (when (carry-test #'character:weapon-type-crossbow-p)
      (update-crossbow-attackable-pos blackboard))))

(defun %update-all-layers (blackboard)
  #- inhibit-update-unexplored-layer
  (update-unexplored-layer blackboard)
  #+ (and debug-mode debug-ai debug-blackboard-layers)
  (progn
    (%update-influence-map        blackboard)
    (update-attack-melee-layer    blackboard)
    (update-attack-pole-layer     blackboard)
    (update-attack-bow-layer      blackboard)
    (update-attack-crossbow-layer blackboard)))

(defun make-single-turn-visited-tiles-matrix (w h)
  (make-matrix w h nil))

(defun reset-single-turn-visited-tiles-matrix (matrix)
  (ploop-matrix (matrix x y)
    (setf (matrix-elt matrix y x) nil)))

(defun reset-per-turn-visited-tiles (blackboard)
  (with-accessors ((tiles per-turn-visited-tiles)
                   (main-state       main-state)) blackboard
    (let ((wmap (width  (map-state main-state)))
          (hmap (height (map-state main-state))))
      (setf tiles (map-ai-entities main-state
                                   #'(lambda (a)
                                       (cons a
                                             (make-single-turn-visited-tiles-matrix wmap
                                                                                    hmap))))))))

(defun get-matrix-turn-visited-tiles (blackboard entity)
  (cdr (assoc entity (per-turn-visited-tiles blackboard) :test #'eq)))

(defun get-value-turn-visited-tiles (blackboard entity x y)
  (when-let ((tiles (get-matrix-turn-visited-tiles blackboard entity)))
    (matrix-elt tiles y x)))

(defun %update-landmarks (blackboard all-dists movement-costs other-costs-layer)
  (with-accessors ((main-state main-state)) blackboard
    (graph:with-pushed-cost-layer (movement-costs other-costs-layer)
      (let ((w-mat            (width (map-state main-state)))
            (h-mat            (width (map-state main-state)))
            (landmarks-so-far '()))
        (lparallel:pdotimes (i +landmark-numbers+)
          (do ((found nil))
              (found)
            (when-let* ((landmark-x          (lcg-next-upto w-mat))

                        (landmark-y          (lcg-next-upto h-mat))
                        (landmark-node       (ivec2 landmark-x landmark-y))
                        (landmark-not-exists (not (find landmark-node
                                                        landmarks-so-far
                                                        :test #'ivec2=)))
                        (landmark-id         (graph:node->node-id movement-costs landmark-node))
                        (costs               (graph:all-minimum-path-costs movement-costs
                                                                           landmark-id)))
              (setf found t)
              (setf (elt all-dists i) costs))))))))

(defun update-pc-landmarks (blackboard)
  (with-accessors ((main-state main-state)) blackboard
    (%update-landmarks blackboard
                       (landmarks-dists   blackboard)
                       (movement-costs-pc main-state)
                       nil)))

(defun update-w-concerning-landmarks (blackboard &key (suppress-doors-p t))
  (with-accessors ((main-state main-state)) blackboard
    (let* ((concerning-tiles  (concerning-tiles->costs-matrix blackboard))
           (suppress-door-mat (and suppress-doors-p
                                   (list (suppress-closed-door-cost-mat blackboard))))
           (others-costs     (append (list concerning-tiles) suppress-door-mat)))
      (%update-landmarks blackboard
                         (landmarks-w/concenring-no-doors-dists blackboard)
                         (movement-costs                        main-state)
                         others-costs))))

(defun update-w/o-concerning-landmarks (blackboard &key (suppress-doors-p t))
  (with-accessors ((main-state main-state)) blackboard
    (let* ((suppress-door-mat (and suppress-doors-p
                                   (suppress-closed-door-cost-mat blackboard)))
           (others-costs      (list suppress-door-mat)))
      (%update-landmarks blackboard
                         (landmarks-w/o-concenring-no-doors-dists blackboard)
                         (movement-costs                          main-state)
                         others-costs))))

(defun %update-all-infos (blackboard)
  (%update-all-layers       blackboard)
  (update-all-attacking-pos blackboard))

(defmethod game-event:on-game-event ((object blackboard) (event game-event:end-turn))
  (with-accessors ((concerning-tiles              concerning-tiles)
                   (concerning-tiles-facing       concerning-tiles-facing)
                   (concerning-tiles-invalicables concerning-tiles-invalicables)
                   (main-state                    main-state))                   object
    (decrease-concerning              (main-state object) concerning-tiles)
    (decrease-concerning              (main-state object) concerning-tiles-facing)
    (reset-default-concerning-invalicable-range   object)
    (decrease-concerning-invalicables (main-state object) concerning-tiles-invalicables)
    (with-world (world main-state)
      (action-scheduler:with-enqueued-bg-process (world action-scheduler:bg-process-action)
        (widget:activate-planner-icon world)
        (%update-all-infos            object)
        (reset-per-turn-visited-tiles object)
        (if (faction-turn-ai-p world)
            (update-pc-landmarks object)
            (progn
              (update-w-concerning-landmarks   object)
              (update-w/o-concerning-landmarks object)))
        #+ (and debug-mode debug-ai)
        (progn
          (dbg "pole  ~a"      (attack-enemy-pole-positions     object))
          (dbg "melee ~a"      (attack-enemy-melee-positions    object))
          (dbg "bow   ~a"      (attack-enemy-bow-positions      object))
          (dbg "crossbow   ~a" (attack-enemy-crossbow-positions object))
          (dbg "def-pos ~a"    (fetch-defender-positions        object))
          (dbg "atk-pos(melee) ~a"
               (fetch-attacker-positions object
                                         :filter-fn
                                         (filter-attack-pos-by-weapon
                                          #'character:weapon-type-minimum-range-p))))
        (widget:deactivate-planner-icon world)))
    nil))

(defmethod game-event:on-game-event ((object blackboard)
                                     (event game-event:fountain-exhausted-event))
  (with-accessors ((exhausted-fountains-ids exhausted-fountains-ids)) object
    (pushnew (game-event:id-origin event) exhausted-fountains-ids :test #'=)
    ;; values nil to pass the event to other registered entities, if any.
    nil))

(defmethod main-state ((object blackboard))
  (slot-value object 'main-state))

(defmethod (setf main-state) (new-state (object blackboard))
  (with-slots (main-state) object
    (setf main-state new-state)))

(defgeneric concerning-tiles->costs-matrix (object))

(defgeneric strategy-decision (object))

(defgeneric update-unexplored-layer (object))

(defgeneric update-concerning-tiles-player (object player &key all-visibles-from-ai))

(defgeneric update-concerning-invalicable (object))

(defgeneric reduce-concerning-invalicable-range (object))

(defgeneric reset-default-concerning-invalicable-range (object))

(defgeneric next-unexplored-position (object player &key scale-factor-cost-concern))

(defgeneric next-actual-unexplored-position (object player &key scale-factor-cost-concern))

(defgeneric update-attack-melee-layer (object))

(defgeneric update-melee-attackable-pos-player (object player &key all-visibles-from-ai))

(defgeneric update-melee-attackable-pos (object))

(defgeneric update-attack-pole-layer (object))

(defgeneric update-pole-attackable-pos-player (object player &key all-visibles-from-ai))

(defgeneric update-pole-attackable-pos (object))

(defgeneric update-attack-bow-layer (object))

(defgeneric update-bow-attackable-pos-player (object player &key all-visibles-from-ai))

(defgeneric update-attack-crossbow-layer (object))

(defgeneric update-crossbow-attackable-pos-player (object player &key all-visibles-from-ai))

(defgeneric update-bow-attackable-pos (object))

(defgeneric update-crossbow-attackable-pos (object))

(defgeneric fountain-exhausted-p (object entity))

(defgeneric log-entity-presence (object entity key-log))

(defgeneric log-ai-entity-presence (object entity))

(defgeneric log-pc-entity-presence (object entity))

(defun calc-concerning-tiles-cost-scaling (difficult-level)
  (dlerp (smoothstep-interpolate 2.0 5.0 (d difficult-level))
         10.0
         15.0))

(defun increase-concerning-tile-facing-player (blackboard player concerning-matrix)
  "increase concerning of player's facing tiles"
    (with-accessors ((concerning-tiles concerning-tiles)
                     (main-state main-state)) blackboard
      (with-accessors ((map-state map-state)) main-state
        (labels ((set-tile (facing-pos &optional (value +concerning-tile-value+))
                   (with-check-matrix-borders (map-state (elt facing-pos 0) (elt facing-pos 1))
                     (set-concerning-tile-custom-map-fn main-state
                                                        player
                                                        concerning-matrix
                                                        (elt facing-pos 0) (elt facing-pos 1)
                                                        :danger-zone-size      1
                                                        :concerning-tile-value value
                                                        :map-element-fn
                                                        #'(lambda (x y old new)
                                                            (declare (ignore x y old))
                                                            (d new))))))
          (let* ((dir        (dir player))
                 (facing-pos (map-utils:facing-pos (pos player) dir))
                 (ghost      (ghost player)))
            (cond
              ((character:weapon-type-pole-p ghost)
               (let ((actual-facing-pos (ivec2+ facing-pos (ivec2 (truncate (elt dir 0))
                                                                  (truncate (elt dir 2))))))
                 (set-tile actual-facing-pos +concerning-tile-value+)))
              ((character:weapon-type-long-range ghost)
               (let ((width (calc-danger-zone-size (level-difficult main-state)))
                     (displ (ivec2 (truncate (elt dir 0))
                                   (truncate (elt dir 2)))))
                 (loop for i from 0 below width do
                      (let ((value (dlerp (smoothstep-interpolate (d 0) (d (1- width)) (d i))
                                          (d* (d/ (d width)
                                                  2.0)
                                              +concerning-tile-value+)
                                          0.0)))
                        (set-tile (ivec2+ facing-pos (ivec2* displ i)) value)))))
              (t ;; melee
               (set-tile facing-pos +concerning-tile-value+))))))))

(defmethod concerning-tiles->costs-matrix ((object blackboard))
  (with-accessors ((concerning-tiles concerning-tiles)
                   (concerning-tiles-facing concerning-tiles-facing)
                   (main-state main-state)) object
    (let ((res (clone concerning-tiles)))
      (setf res
            (matrix:map-matrix res
                               #'(lambda (a)
                                   (let* ((min-value           0.0)
                                          (min-value-normalize +concerning-tiles-min-value+)
                                          (max-value           (cost-limit-concerning-tiles))
                                          (max-normalize      +concerning-tile-value+))
                                     (if (not (epsilon= min-value a))
                                         (d* (normalize-value-in-range a
                                                                       min-value-normalize
                                                                       max-normalize)
                                             max-value)
                                         min-value)))))
      ;; commented out: the concerning tiles value would leak across not visible tiles
      ;; (setf res (matrix:pgaussian-blur-separated res #'identity 1))
      (ploop-matrix (res x y)
         (setf (matrix-elt res y x)
               (d+ (matrix-elt res y x)
                   (matrix-elt concerning-tiles-facing y x))))
      res)))

(defmethod strategy-decision ((object blackboard))
  (with-accessors ((main-state main-state)) object
    (with-world (world main-state)
      (let* ((fact     (strategic-ai:register-ai-tree-data-cached world +npc-type+))
             (decision (id3:take-decision (strategic-ai:current-decision-tree)
                                          +ai-fact-header+
                                          fact)))
        #+(and debug-mode debug-ai)
        (misc:dbg "fact is ~a strategy is ~a" fact decision)
        decision))))

(defmethod set-tile-visited ((object blackboard) entity-visiting x y &key (update-infos nil))
  (call-next-method object
                    entity-visiting
                    (map-utils:coord-chunk->matrix x)
                    (map-utils:coord-chunk->matrix y)
                    :update-infos update-infos))

(defmethod set-tile-visited ((object blackboard) entity-visiting (x fixnum) (y fixnum)
                              &key (update-infos nil))
  (with-accessors ((visited-tiles visited-tiles)
                   (main-state main-state)
                   (concerning-tiles concerning-tiles)) object
    (with-accessors ((ghost ghost)) entity-visiting
    ;; mark visited global table
    (setf (matrix-elt visited-tiles y x) t)
    (when entity-visiting
      (let ((pos (calculate-cost-position entity-visiting)))
        ;; mark visited for the entity single table
        (when-let ((per-turn-tiles (get-matrix-turn-visited-tiles object entity-visiting)))
          (setf (matrix-elt per-turn-tiles y x) t))
        ;; update character's log of movement
        (character:append-to-last-pos-occupied ghost pos))
      (misc:dbg "log pos ~a" (character:last-pos-occupied ghost)))
    (when update-infos
      (%update-all-infos object)))))

(defun calc-danger-zone-size (difficult-level)
  "The size of the concerning zone when when some concerning event occurs"
  (ceiling (dlerp (smoothstep-interpolate 2.0 5.0 difficult-level)
                  4.0
                  8.0)))

(defun set-concerning-tile-custom-map-fn (main-state player concerning-tiles x y
                                          &key
                                            (danger-zone-size
                                             (let* ((level (level-difficult main-state)))
                                               (calc-danger-zone-size level)))
                                            (concerning-tile-value +concerning-tile-value+)
                                            (map-element-fn
                                             #'(lambda (x y old new)
                                                 (declare (ignore x y))
                                                 (d+ old new))))
  (let* ((dangerous-tiles-pos (gen-neighbour-position-in-box x
                                                             y
                                                             danger-zone-size
                                                             danger-zone-size))
         (player-pos          (calculate-cost-position player)))
    (loop for point across dangerous-tiles-pos do
         (2d-utils:displace-2d-vector (point x y)
           (with-check-matrix-borders (concerning-tiles x y)
             ;; add value if the position  is visible (ray only) or is
             ;; occupied by the character itself
             (when (or (ivec2= point player-pos)
                       (able-to-see-mesh:placeholder-visible-ray-p player x y))
               (setf (matrix-elt concerning-tiles y x)
                     (funcall map-element-fn
                              x y
                              (matrix-elt concerning-tiles y x)
                              concerning-tile-value))))))))

;;;; unused
(defmethod set-concerning-tile ((object blackboard) x y
                                &key
                                  (danger-zone-size
                                   (let* ((main-state (main-state object))
                                          (level (level-difficult main-state)))
                                     (calc-danger-zone-size level)))
                                  (concerning-tile-value +concerning-tile-value+))
  (with-accessors ((concerning-tiles concerning-tiles)) object
    (let* ((dangerous-tiles-pos (gen-neighbour-position-in-box x
                                                               y
                                                               danger-zone-size
                                                               danger-zone-size)))
      (loop for point across dangerous-tiles-pos do
           (2d-utils:displace-2d-vector (point x y)
             (with-check-matrix-borders (concerning-tiles x y)
               (incf (matrix-elt concerning-tiles y x) concerning-tile-value))))))
  object)

(defmethod set-concerning-tile-fn ((object blackboard) x y
                                   &key
                                     (danger-zone-size (let* ((main-state (main-state object))
                                                              (level (level-difficult main-state)))
                                                         (calc-danger-zone-size level)))
                                     (concerning-tile-fn
                                      (concerning-tile-default-mapping-clsr x y
                                                                            danger-zone-size)))
  (with-accessors ((concerning-tiles concerning-tiles)) object
    (let* ((dangerous-tiles-pos (gen-neighbour-position-in-box x
                                                               y
                                                               danger-zone-size
                                                               danger-zone-size)))
      (loop for point across dangerous-tiles-pos do
           (2d-utils:displace-2d-vector (point x y)
             (with-check-matrix-borders (concerning-tiles x y)
               (setf (matrix-elt concerning-tiles y x)
                     (funcall concerning-tile-fn
                              x y
                              (matrix-elt concerning-tiles y x)
                              0.0))))))) ; ignored
  object)


(defun calc-enemy-danger-zone-size (player)
  (with-accessors ((main-state state)
                   (ghost ghost)) player
    (let* ((level          (level-difficult main-state))
           (weapon-type    (character:weapon-type (ghost player)))
           (range          (case weapon-type
                             ((:edge :impact)
                              (+ 1 level +weapon-melee-range+))
                             (:pole
                              (+ 1 level +weapon-pole-range+))
                             (:bow
                              (* 2 +weapon-bow-range+))
                             (:crossbow
                              (* 2 +weapon-crossbow-range+))
                             (otherwise
                              (* 2 +weapon-melee-range+))))
           (movement-weight (if (> level 1)
                                (/ level 25)
                                0)))
      (floor (+ range
                (* movement-weight
                   (character:actual-movement-points ghost)))))))

(defun reset-layer (djk-map value)
  (with-accessors ((layer layer)) djk-map
    ;; reset layer
    (ploop-matrix (layer x y)
       (setf (matrix-elt layer y x) value))))

(defun reset-explored-layer (unexplored-layer)
  (reset-layer unexplored-layer +unexplored-tile-value+))

(defun reset-attack-layer (unexplored-layer)
  (reset-layer unexplored-layer +attack-nongoal-tile-value+))

(defun all-*-id-visible-from-faction (game-state faction)
  "Note: loop-*-entities will skip death characters"
  (let ((all-visibles '())
        (loop-fn (faction->loop-faction-fn faction)))
    (funcall loop-fn game-state
             #'(lambda (v)
                 (let ((visibles (able-to-see-mesh:other-faction-visible-players v)))
                   (loop for visible in visibles do
                        (pushnew visible all-visibles :key #'id :test #'=)))))
    (map 'list #'id all-visibles)))


(defun all-ai-id-visible-from-player (game-state)
  "Note: loop-player-entities will skip death characters"
  (all-*-id-visible-from-faction game-state +pc-type+))

(defun all-player-id-visible-from-ai (game-state)
  "Note: loop-ai-entities will skip death characters"
  (let ((all-visibles '()))
    (loop-ai-entities game-state
                     #'(lambda (v)
                         (let ((visibles (able-to-see-mesh:other-faction-visible-players v)))
                           (loop for visible in visibles do
                                (pushnew visible all-visibles :key #'id :test #'=)))))
    (map 'list #'id all-visibles)))

(defun all-other-factions-can-see-entity (game-state entity)
  "Note: map-fn will skip death characters"
  (let ((all-able-to-see '())
        (map-fn (opposite-faction-map-fn entity)))
    (funcall map-fn game-state
             #'(lambda (v)
                 (when (able-to-see-mesh:other-visible-p v entity)
                   (pushnew v all-able-to-see :key #'id :test #'=))))
    all-able-to-see))

(defmacro gen-all-*-visibles-by-entity (name loop-fn test-visibility-fn)
  (with-gensyms (all-able-to-see game-state entity v)
    (let ((fn-name (format-fn-symbol t "all-~a-visibles-by-entity" name)))
      `(defun ,fn-name (,game-state ,entity)
         (let ((,all-able-to-see '()))
           (,loop-fn ,game-state
                     #'(lambda (,v)
                         (when (,test-visibility-fn ,v ,entity)
                           (pushnew ,v ,all-able-to-see :key #'id :test #'=))))
           ,all-able-to-see)))))

(gen-all-*-visibles-by-entity ai loop-ai-entities able-to-see-mesh:other-visible-p)

(gen-all-*-visibles-by-entity player loop-player-entities able-to-see-mesh:other-visible-p)

(gen-all-*-visibles-by-entity ai-ray loop-ai-entities able-to-see-mesh:other-visible-ray-p)

(gen-all-*-visibles-by-entity player-ray loop-player-entities able-to-see-mesh:other-visible-ray-p)

(defun all-visibles-by-entity (game-state entity)
  (lcat (all-ai-visibles-by-entity game-state entity)
        (all-player-visibles-by-entity game-state entity)))

(defun all-visibles-ray-by-entity (game-state entity)
  (lcat (all-ai-visibles-by-entity game-state entity)
        (all-player-visibles-by-entity game-state entity)))

(defun nsuperimpose-layer (from destination &key (fn #'(lambda (a b)
                                                         (declare (ignore b))
                                                         a)))
  (ploop-matrix (destination x y)
     (setf (matrix-elt destination y x)
           (funcall fn (matrix-elt from y x) (matrix-elt destination y x)))))

(defun matrix-update-values (layer positions visible-positions new-value)
  (loop for point across positions do
       (displace-2d-vector (point x y)
         (with-check-matrix-borders (layer x y)
           (when (find point visible-positions :test #'ivec2=)
             (setf (matrix-elt layer y x) new-value))))))

(defun layer-skip-filter-fn (game-state
                             &key
                               (ignore-visibility-player-test  nil)
                               (ignore-become-invalicable-test nil))
  (with-accessors ((blackboard blackboard)) game-state
    (let ((all-visibles-entities (all-player-id-visible-from-ai game-state))
          (max-mp                (max-ai-movement-points game-state)))
    #'(lambda (el-type pos)
        (displace-2d-vector (pos x y)
          (let ((cost (get-cost game-state x y)))
            (and (not (door@pos-p game-state x y))
                 (or (inmap:skippablep el-type pos)
                     (and (not ignore-become-invalicable-test)
                          (concerning-became-explore-invalicable-tile-p blackboard x y))
                     (and (not ignore-visibility-player-test)
                          (find (entity-id-in-pos game-state x y)
                               all-visibles-entities
                               :test #'=))
                     (> cost max-mp)))))))))

(defun update-conc-invalicable-skippable-fn (game-state)
  (layer-skip-filter-fn game-state
                        :ignore-visibility-player-test  t
                        :ignore-become-invalicable-test t))

(defmethod update-unexplored-layer ((object blackboard))
  (with-accessors ((visited-tiles                 visited-tiles)
                   (main-state                    main-state)
                   (concerning-tiles              concerning-tiles)
                   (concerning-tiles-invalicables concerning-tiles-invalicables)
                   (unexplored-layer              unexplored-layer)) object
    (with-accessors ((layer layer)) unexplored-layer
      (reset-explored-layer unexplored-layer)
      (let ((all-visibles (all-player-id-visible-from-ai main-state)))
        ;; calculate concerning tiles tiles occupied by enemies
        (loop-player-entities main-state
                             #'(lambda (player)
                                 (update-concerning-tiles-player object
                                                                 player
                                                                 :all-visibles-from-ai
                                                                 all-visibles)))
        ;; add-concerning tiles
        (nsuperimpose-layer concerning-tiles layer)
        ;; mark already visited tiles
        (ploop-matrix (layer x y)
           (when (matrix-elt visited-tiles y x)
             (setf (matrix-elt layer y x) +explored-tile-value+)))
        ;; update invalicable human player pos
        (update-concerning-invalicable object)
        ;; smooth the map here
        (inmap:smooth-dijkstra-layer unexplored-layer
                                     main-state
                                     :skippable-predicate
                                     (layer-skip-filter-fn main-state))))))

(defun %2d-ray-stopper (player x y use-fov)
  "x  and y  in  cost-map  space (integer  coordinates),  values t  if
   somenthing prevented the ray to reach the target position"
  (with-accessors ((state state)) player
    (let* ((visiblep (if use-fov
                         (able-to-see-mesh:placeholder-visible-p     player x y)
                         (able-to-see-mesh:placeholder-visible-ray-p player x y))))
      (or
       ;; not sure could be removed, doing anyway
       ;; (not (map-element-empty-p (element-mapstate@ state x y)))
       (not visiblep)))))

(defun concerning-tile-default-mapping-clsr (x-center y-center size)
  #'(lambda (x y old new)
      (declare (ignore new))
      (let* ((max  (map-manhattam-distance (ivec2 x-center y-center)
                                           (ivec2 (f+ x-center size)
                                                  (f+ y-center size))))
             (min  0)
             (dist (map-manhattam-distance (ivec2 x-center y-center)
                                           (ivec2 x y)))
             (normalized-param (normalize-value-in-range dist min max))
             (t+1              (d+ 1.0 normalized-param))
             (scale-factor     (dexpt (d* t+1 (d- 2.0 t+1)) 4.0)))
        (min (max old
                  (d* scale-factor
                      +concerning-tile-value+))
             +concerning-tile-value+))))

(defmethod update-concerning-tiles-player ((object blackboard) (player md2-mesh:md2-mesh)
                                           &key
                                             (all-visibles-from-ai
                                              (all-player-id-visible-from-ai (state player))))
  "note: player is human side"
  (with-accessors ((visited-tiles                  visited-tiles)
                   (concerning-tiles               concerning-tiles)
                   (concerning-tiles-facing        concerning-tiles-facing)
                   (main-state                     main-state)
                   (use-enemy-fov-when-exploring-p use-enemy-fov-when-exploring-p)) object
    (let ((increase-facing-p nil))
      (labels ((update-single-tile (concerning-matrix x y concerning-fn)
                 (setf increase-facing-p t)
                 (setf (matrix-elt concerning-matrix y x)
                       (funcall concerning-fn
                                x y
                                (matrix-elt concerning-matrix y x)
                                0.0))) ; ignored
               (update-single-pass (dangerous-tiles-pos concerning-fn)
                 (loop for point across dangerous-tiles-pos do
                      (displace-2d-vector (point x y)
                        (with-check-matrix-borders (concerning-tiles x y)
                          (when (not (%2d-ray-stopper player x y
                                                      use-enemy-fov-when-exploring-p))
                            (update-single-tile concerning-tiles x y concerning-fn)))))))
        (cond
          ((faction-player-p main-state (id player))
           (when (find (id player) all-visibles-from-ai :test #'=)
             (let* ((position            (pos-entity-chunk->cost-pos (pos player)))
                    (x-player            (elt position 0))
                    (y-player            (elt position 1))
                    (danger-zone-size    (calc-enemy-danger-zone-size player))
                    (dangerous-tiles-pos (gen-neighbour-position-in-box x-player
                                                                        y-player
                                                                        danger-zone-size
                                                                        danger-zone-size
                                                                        :add-center t))
                    (concerning-fn (concerning-tile-default-mapping-clsr x-player
                                                                         y-player
                                                                         danger-zone-size)))
               (let* ((all-visibles     (if use-enemy-fov-when-exploring-p
                                            (all-visibles-by-entity main-state player)
                                            (all-visibles-ray-by-entity main-state player)))
                      (all-visibles-ids (mapcar #'identificable:id all-visibles)))
                 (update-single-pass dangerous-tiles-pos concerning-fn)
                 ;; 1st pass
                 (map nil #'(lambda (a)
                              (let ((pos (calculate-cost-position a)))
                                (when (find pos dangerous-tiles-pos :test #'ivec2=)
                                  (displace-2d-vector (pos x y)
                                    (update-single-tile concerning-tiles x y concerning-fn)))))
                      all-visibles)
                 ;; 2nd pass
                 ;; we are going to ignore in %2d-ray-stopper all characters (for ray test)
                 (let ((absee-mesh:*ray-test-id-entities-ignored* all-visibles-ids))
                   (update-single-pass dangerous-tiles-pos concerning-fn)))
               ;; add the player (human) position
               (setf (matrix-elt concerning-tiles y-player x-player)
                     +concerning-tile-value+)
               ;; increase the  tile the player  is facing based  on the
               ;; weapon they are carrying
               (when increase-facing-p
                 (increase-concerning-tile-facing-player object
                                                         player
                                                         concerning-tiles-facing))))))))))

(defun concerning-tile-invalicable-mapping-clsr (x-center y-center)
  #'(lambda (x y)
      (let* ((dist (map-manhattam-distance (ivec2 x-center y-center)
                                           (ivec2 x y))))
        (d/ +concerning-tile-value+ (d (1+ dist))))))

(defmethod update-concerning-invalicable ((object blackboard))
  (with-accessors ((main-state                    main-state)
                   (concerning-tiles              concerning-tiles)
                   (concerning-tiles-invalicables concerning-tiles-invalicables)) object
    (ploop-matrix (concerning-tiles-invalicables x y)
      (when (null (matrix-elt concerning-tiles-invalicables y x))
        (setf (matrix-elt concerning-tiles-invalicables y x) +concerning-tiles-min-value+))
      (when (concerning-became-explore-invalicable-tile-p object x y)
        (setf (matrix-elt concerning-tiles-invalicables y x)
              +concerning-tile-value+)))
    ;; smooth
    (let ((map (wrap-matrix-as-dijkstra-map concerning-tiles-invalicables)))
      (smooth-dijkstra-layer map
                             main-state
                             :skippable-predicate
                             (update-conc-invalicable-skippable-fn main-state))
      (setf concerning-tiles-invalicables (layer map)))
    object))

(defmethod reduce-concerning-invalicable-range ((object blackboard))
  (with-accessors ((concerning-tiles-invalicable-threshold
                    concerning-tiles-invalicable-threshold)) object
    (setf concerning-tiles-invalicable-threshold
          (min +max-concerning-invalicable-threshold+
               (incf concerning-tiles-invalicable-threshold
                     +concerning-tiles-invalicables-inc-step+)))))

(defmethod reset-default-concerning-invalicable-range ((object blackboard))
  "reset to default value"
  (with-accessors ((concerning-tiles-invalicable-threshold
                    concerning-tiles-invalicable-threshold)) object
    (setf concerning-tiles-invalicable-threshold
          +min-concerning-invalicable-threshold+ )))

(defun concerning-became-explore-invalicable-tile-p (blackboard x y)
  (with-accessors ((concerning-tiles concerning-tiles)
                   (concerning-tiles-invalicable-threshold
                    concerning-tiles-invalicable-threshold)) blackboard
    (d> (matrix-elt concerning-tiles y x)
        concerning-tiles-invalicable-threshold)))

(defun concerning-became-explore-invalicable-tile-p* (blackboard coord)
  (concerning-became-explore-invalicable-tile-p blackboard
                                                (ivec2-x coord)
                                                (ivec2-y coord)))

(defmethod next-actual-unexplored-position ((object blackboard) (player md2-mesh:md2-mesh)
                                     &key (scale-factor-cost-concern 1.0))
  (with-accessors ((unexplored-layer              unexplored-layer)
                   (concerning-tiles-invalicables concerning-tiles-invalicables)) object
    (let ((pos (calculate-cost-position player)))
      (if (concerning-became-explore-invalicable-tile-p* object pos)
          (progn
            #+debug-mode
            (let ((next-pos (inmap:next-dijkstra-position concerning-tiles-invalicables
                                                          player
                                                          scale-factor-cost-concern)))
              (misc:dbg "using inner @ ~a ~a" pos next-pos))
            (inmap:next-dijkstra-position concerning-tiles-invalicables
                                                        player
                                                        scale-factor-cost-concern))
          (inmap:next-dijkstra-position unexplored-layer player
                                        scale-factor-cost-concern)))))

(defun %2d-ray-stopper-melee-fn (player x y)
  "x and y in cost-map space (integer coordinates)

note: if a character of human faction is blocking visibility this form
values nil, i. e. the ray is not blocked"
  (with-accessors ((state state)) player
    (multiple-value-bind (visiblep entity-hitted)
        (able-to-see-mesh:placeholder-visible-ray-p player x y)
      (let* ((emptyp                 (map-element-empty-p (element-mapstate@ state x y)))
             (blocked-by-character-p (and (not visiblep)
                                          entity-hitted
                                          (faction-player-p state (id entity-hitted)))))
        (cond
          (blocked-by-character-p
           nil)
          ((not visiblep)
           t)
          ((not emptyp)
           t)
          (t
           nil))))))

(defun is-visible-player-pov-p (goal-tile-pos player use-enemy-fov-when-attacking-p)
  (if use-enemy-fov-when-attacking-p
      (displace-2d-vector (goal-tile-pos x y)
        ;; check if  visible or blocked
        ;; by  ray (even  if not  in the
        ;; visibility cone  (for example
        ;; behind the attacked entity).
        (or (able-to-see-mesh:placeholder-visible-p player x y)
            (not (able-to-see-mesh:placeholder-visible-ray-p player x y))))
      (displace-2d-vector (goal-tile-pos x y)
        (able-to-see-mesh:placeholder-visible-ray-p player x y))))

;; TODO use is-visible-player-pov-p
(defun remove-from-player-pov (goal-tiles-pos player use-enemy-fov-when-attacking-p)
  "remove tiles from the target's point of view (player is from human side)"
  (if use-enemy-fov-when-attacking-p
      (remove-if #'(lambda (a)
                     (displace-2d-vector (a x y)
                       ;; remove if  visible or blocked
                       ;; by  ray (even  if not  in the
                       ;; visibility cone  (for example
                       ;; behind the attacked entity).
                       (or (able-to-see-mesh:placeholder-visible-p player x y)
                           (not (able-to-see-mesh:placeholder-visible-ray-p player
                                                                            x
                                                                            y)))))
                 goal-tiles-pos)
      (remove-if-not #'(lambda (a)
                         (displace-2d-vector (a x y)
                           (able-to-see-mesh:placeholder-visible-ray-p player
                                                                       x
                                                                       y)))
                     goal-tiles-pos)))

(defun %attack-layer-player-goal-pos (blackboard
                                      layer
                                      player
                                      all-visibles-from-ai
                                      goal-tiles-generator-fn)
  (with-accessors ((main-state main-state)
                   (use-enemy-fov-when-attacking-p use-enemy-fov-when-attacking-p)) blackboard
    (cond
      ((faction-player-p main-state (id player))
       (when (find (id player) all-visibles-from-ai :test #'=)
         (let* ((position (pos-entity-chunk->cost-pos (pos player)))
                (x-player (elt position 0))
                (y-player (elt position 1))
                (goal-tiles-pos (funcall goal-tiles-generator-fn
                                         x-player
                                         y-player)))
           (setf goal-tiles-pos (remove-if #'(lambda (a)
                                               (or (not (element@-inside-p layer
                                                                           (elt a 0)
                                                                           (elt a 1)))
                                                   (skippablep (el-type-in-pos main-state
                                                                               (elt a 0)
                                                                               (elt a 1))
                                                               nil)))
                                           goal-tiles-pos))
           ;; remove tiles from the target's point of view
           (setf goal-tiles-pos (remove-from-player-pov goal-tiles-pos
                                                        player
                                                        use-enemy-fov-when-attacking-p))
           (loop
              for point in (map 'list #'identity goal-tiles-pos)
              when (2d-tile-visible-around-ai-p main-state ; skip tiles not visible by AI
                                                (sequence->ivec2 point)
                                                #'%2d-ray-stopper-melee-fn)
              collect point)))))))

(defun %update-attack-pos (blackboard update-fn)
  (with-accessors ((main-state main-state)) blackboard
    (let ((all-visibles (all-player-id-visible-from-ai main-state)))
      (loop-player-entities main-state  #'(lambda (player)
                                           (funcall update-fn
                                                    blackboard
                                                    player
                                                    :all-visibles-from-ai
                                                    all-visibles))))))

(defun %update-attack-layer (def-positions map)
  (reset-attack-layer map)
    (let ((layer (inmap:layer map)))
      (loop for i in def-positions do
           (loop for p in (goal-pos i) do
                (displace-2d-vector (p x y)
                  (setf (matrix-elt layer y x) +goal-tile-value+))))))

(defmacro with-update-attackable-layer ((blackboard pos layer))
  `(with-accessors ((,pos ,pos)
                    (,layer ,layer)) ,blackboard
     (%update-attack-layer ,pos ,layer)))

(defun push-target-position (bag player target-pos)
  (let* ((id-player (id player))
         (game-state (state player))
         (blackboard (blackboard game-state)))
    (labels ((%push ()
               (let ((target-found (find-if #'(lambda (a) (= (entity-id a) id-player)) bag)))
                 (if target-found
                     (pushnew target-pos (goal-pos target-found) :test #'ivec2=)
                     (push (make-defender-target id-player
                                                 (calc-def-target-age game-state)
                                                 (list target-pos))
                           bag)))
               bag)
             (%find-maybe-remove-pos (target-pos bag)
               "if nil it is safe to add the position"
               (let ((target-with-pos (find-if #'(lambda (a) (find target-pos a :test #'ivec2=))
                                               bag
                                               :key #'goal-pos))
                     (removep         (die-utils:pass-d2 1))) ; roll a die
                 (if target-with-pos ; another target has the same slot
                     (if removep
                         (progn
                           (setf (goal-pos target-with-pos)  ; remove the old slot
                                 (remove target-pos (goal-pos target-with-pos) :test #'ivec2=))
                           nil)
                         t)
                     nil)))
             (%find-pos (blackboard target-pos)
               (with-accessors ((attack-enemy-pole-positions     attack-enemy-pole-positions)
                                (attack-enemy-melee-positions    attack-enemy-melee-positions)
                                (attack-enemy-bow-positions      attack-enemy-bow-positions)
                                (attack-enemy-crossbow-positions attack-enemy-crossbow-positions))
                   blackboard
                 (or (%find-maybe-remove-pos target-pos attack-enemy-pole-positions)
                     (%find-maybe-remove-pos target-pos attack-enemy-melee-positions)
                     (%find-maybe-remove-pos target-pos attack-enemy-bow-positions)
                     (%find-maybe-remove-pos target-pos attack-enemy-crossbow-positions)))))
      (if (not (%find-pos blackboard target-pos))
          (%push)
          bag))))

(defun %calc-new-goal-attack-position-bag (player new-valid-positions old-position-bag)
  (loop for position in new-valid-positions do
       (setf old-position-bag (push-target-position old-position-bag player position)))
  old-position-bag)

(defun melee-weapon-goal-generator-fn ()
  #'(lambda (x y)
      (mapcar #'ivec2:sequence->ivec2
              (gen-4-neighbour-counterclockwise x y
                                                :add-center nil))))

(defun %calc-melee-attack-position-player (blackboard player all-visibles-from-ai)
  (with-accessors ((attack-enemy-melee-layer     attack-enemy-melee-layer)
                   (attack-enemy-melee-positions attack-enemy-melee-positions)) blackboard
    (with-accessors ((layer layer)) attack-enemy-melee-layer
      (let* ((goal-generator-fn (melee-weapon-goal-generator-fn))
             (goal-tiles-pos    (%attack-layer-player-goal-pos blackboard
                                                               layer
                                                               player
                                                               all-visibles-from-ai
                                                               goal-generator-fn)))
        (loop
           for point in goal-tiles-pos
           when (and (valid-coordinates-p layer point)
                     (not (goal-canceled-for-neighbour-p all-visibles-from-ai
                                                         player
                                                         point)))
           collect point)))))

(defmethod update-melee-attackable-pos-player ((object blackboard) (player md2-mesh:md2-mesh)
                                               &key
                                                 (all-visibles-from-ai
                                                  (all-player-id-visible-from-ai (state player))))
    (with-accessors ((attack-enemy-melee-positions attack-enemy-melee-positions)) object
      (let ((valid-positions (%calc-melee-attack-position-player object
                                                                 player
                                                                 all-visibles-from-ai)))
        (setf attack-enemy-melee-positions
              (%calc-new-goal-attack-position-bag player
                                                  valid-positions
                                                  attack-enemy-melee-positions)))))

(defmethod update-attack-melee-layer ((object blackboard))
  (with-update-attackable-layer (object attack-enemy-melee-positions
                                        attack-enemy-melee-layer))
  object)

(defmethod update-melee-attackable-pos ((object blackboard))
  (with-accessors ((attack-enemy-melee-positions attack-enemy-melee-positions)) object
    (setf attack-enemy-melee-positions
          (remove-attack-starved object attack-enemy-melee-positions))
    (%update-attack-pos object #'update-melee-attackable-pos-player)))

(defun increase-attack-goal-fading-weight (allied player goal-pos increase-threshold-dist)
  (let* ((allied-pos      (pos-entity-chunk->cost-pos (pos allied)))
         (dist            (d (map-utils:map-manhattam-distance allied-pos goal-pos)))
         (res             (dlerp (- 1.0 (smoothstep-interpolate 1.0 increase-threshold-dist dist))
                                 0.0
                                 1.0)))
    (if (= (id allied) (id player))
        0.0
        res)))

(defun calc-goal-canceled-for-neighbour-threshold (player)
  (with-accessors ((state state)) player
    (let ((level (level-difficult state)))
      (cond
        ((<= 0 level 1)
         3.0)
        ((<= 2 level 3)
         1.0)
        (t
         0.8)))))

(defun calc-goal-canceled-for-neighbour-dist-threshold (player)
  (with-accessors ((state state)) player
    (let ((level (level-difficult state)))
      (cond
        ((<= 0 level 1)
         2.0)
        ((<= 2 level 3)
         3.0)
        (t
         4.0)))))

(defun goal-canceled-for-neighbour-p (all-visibles-from-ai player goal-pos)
  (with-accessors ((state state)) player
    (let* ((threshold (calc-goal-canceled-for-neighbour-dist-threshold player))
           (sum-neighbour (loop for allied-id in all-visibles-from-ai sum
                               (let ((allied (find-entity-by-id state allied-id)))
                                 (if allied
                                     (increase-attack-goal-fading-weight allied
                                                                         player
                                                                         goal-pos
                                                                         threshold)
                                     0.0)))))
      (> sum-neighbour (calc-goal-canceled-for-neighbour-threshold player)))))

(defun pole-weapon-goal-generator-fn (player)
  (let ((level (level-difficult (state player))))
    #'(lambda (x y)
        (let ((size +weapon-pole-range+))
          (remove-if-not #'(lambda (a)
                             (<= (manhattam-distance a (ivec2 x y))
                                 size))
                         (cond
                           ((< level 2)
                            (gen-4-neighbour-counterclockwise x y :add-center nil))
                           ((<= 2 level +difficult-medium+)
                            (if (die-utils:pass-d2-periodic 2 10)
                                (gen-4-neighbour-counterclockwise x y :add-center nil)
                                (gen-cross-near x y size :add-center nil)))
                           (t
                            (gen-cross-near x y size :add-center nil))))))))

(defun %calc-pole-attack-position-player (blackboard player all-visibles-from-ai)
  (with-accessors ((attack-enemy-pole-layer attack-enemy-pole-layer)
                   (attack-enemy-pole-positions attack-enemy-pole-positions)) blackboard
    (with-accessors ((layer layer)) attack-enemy-pole-layer
      (let* ((goal-generator-fn (pole-weapon-goal-generator-fn player))
             (goal-tiles-pos    (%attack-layer-player-goal-pos blackboard
                                                               layer
                                                               player
                                                               all-visibles-from-ai
                                                               goal-generator-fn)))
        (loop
           for point in goal-tiles-pos
           when (and (valid-coordinates-p layer point)
                     (not (goal-canceled-for-neighbour-p all-visibles-from-ai
                                                         player
                                                         point)))
           collect point)))))

(defmethod update-pole-attackable-pos-player ((object blackboard) (player md2-mesh:md2-mesh)
                                              &key
                                                (all-visibles-from-ai
                                                 (all-player-id-visible-from-ai (state player))))
  (with-accessors ((attack-enemy-pole-positions attack-enemy-pole-positions)) object
    ;;     all valid-tiles
    (let ((valid-positions (%calc-pole-attack-position-player object
                                                              player
                                                              all-visibles-from-ai)))
        (setf attack-enemy-pole-positions
              (%calc-new-goal-attack-position-bag player ; add the new def-target objects
                                                  valid-positions
                                                  attack-enemy-pole-positions)))))

(defun long-range-weapon-goal-generator-fn (player range)
  (let ((level (level-difficult (state player))))
    #'(lambda (x y)
        (let ((size (* 2 range)))
          (cond
            ((< level 2)
             (gen-4-neighbour-counterclockwise x y :add-center nil))
            ((<= 2 level 3)
             (if (die-utils:pass-d2 2)
                 (gen-4-neighbour-counterclockwise x y :add-center nil)
                 (gen-ring-box-position x y size size)))
            (t
             (gen-ring-box-position x y size size)))))))

(defun %calc-attack-long-range-position-player (blackboard player djk-map
                                                all-visibles-from-ai range)
  (with-accessors ((layer layer)) djk-map
    (let* ((goal-generator-fn (long-range-weapon-goal-generator-fn player range))
           (goal-tiles-pos    (%attack-layer-player-goal-pos blackboard
                                                             layer
                                                             player
                                                             all-visibles-from-ai
                                                             goal-generator-fn)))
      (loop
         for point in goal-tiles-pos
         when (and (valid-coordinates-p layer point)
                   (not (goal-canceled-for-neighbour-p all-visibles-from-ai
                                                       player
                                                       point)))
         collect point))))

(defun %update-attack-long-range-position-player (blackboard player djk-map
                                                  target-positions-bag
                                                  all-visibles-from-ai range)
  (let ((valid-positions (%calc-attack-long-range-position-player blackboard
                                                                  player
                                                                  djk-map
                                                                  all-visibles-from-ai
                                                                  range)))
    (setf target-positions-bag
          (%calc-new-goal-attack-position-bag player
                                              valid-positions
                                              target-positions-bag))
    target-positions-bag))

(defun %update-attack-long-range-layer-player (blackboard player djk-map
                                               all-visibles-from-ai range)
  (let ((valid-positions (%calc-attack-long-range-position-player blackboard
                                                                 player
                                                                 djk-map
                                                                 all-visibles-from-ai
                                                                 range)))
    (with-accessors ((layer layer)) djk-map
      (loop
         for position in valid-positions do
           (displace-2d-vector (position x y)
             (setf (matrix-elt layer y x)
                   +goal-tile-value+))))))

(defun bow-range-for-attack-goal ()
  (floor (d/ (d +weapon-bow-range+) 2.0)))

(defmethod update-bow-attackable-pos-player ((object blackboard) (player md2-mesh:md2-mesh)
                                             &key
                                               (all-visibles-from-ai
                                                (all-player-id-visible-from-ai (state player))))
  (with-accessors ((attack-enemy-bow-layer     attack-enemy-bow-layer)
                   (attack-enemy-bow-positions attack-enemy-bow-positions)) object
    (setf attack-enemy-bow-positions
          (%update-attack-long-range-position-player object
                                                     player
                                                     attack-enemy-bow-layer
                                                     attack-enemy-bow-positions
                                                     all-visibles-from-ai
                                                     (bow-range-for-attack-goal)))))

(defun crossbow-range-for-attack-goal ()
  (floor (d/ (d +weapon-crossbow-range+) 2.0)))

(defmethod update-crossbow-attackable-pos-player ((object blackboard) (player md2-mesh:md2-mesh)
                                                  &key
                                                    (all-visibles-from-ai
                                                     (all-player-id-visible-from-ai (state player))))
  (with-accessors ((attack-enemy-crossbow-layer attack-enemy-crossbow-layer)
                   (attack-enemy-crossbow-positions attack-enemy-crossbow-positions)) object
    (setf attack-enemy-crossbow-positions
          (%update-attack-long-range-position-player object
                                                     player
                                                     attack-enemy-crossbow-layer
                                                     attack-enemy-crossbow-positions
                                                     all-visibles-from-ai
                                                     (crossbow-range-for-attack-goal)))))

(defmethod update-bow-attackable-pos ((object blackboard))
  (with-accessors ((attack-enemy-bow-positions attack-enemy-bow-positions)) object
    (setf attack-enemy-bow-positions
          (remove-attack-starved object attack-enemy-bow-positions))
    (%update-attack-pos object #'update-bow-attackable-pos-player)))

(defmethod update-crossbow-attackable-pos ((object blackboard))
  (with-accessors ((attack-enemy-crossbow-positions attack-enemy-crossbow-positions)) object
    (setf attack-enemy-crossbow-positions
          (remove-attack-starved object attack-enemy-crossbow-positions))
    (%update-attack-pos object #'update-crossbow-attackable-pos-player)))

(defmethod update-attack-pole-layer ((object blackboard))
  (with-update-attackable-layer (object attack-enemy-pole-positions
                                        attack-enemy-pole-layer))
  object)

(defmethod update-pole-attackable-pos ((object blackboard))
  (with-accessors ((attack-enemy-pole-positions attack-enemy-pole-positions)) object
    (setf attack-enemy-pole-positions
          (remove-attack-starved object attack-enemy-pole-positions))
    (%update-attack-pos object #'update-pole-attackable-pos-player)))

(defmethod update-attack-bow-layer ((object blackboard))
  (with-update-attackable-layer (object attack-enemy-bow-positions
                                        attack-enemy-bow-layer))
  object)

(defmethod update-attack-crossbow-layer ((object blackboard))
  (with-update-attackable-layer (object attack-enemy-crossbow-positions
                                        attack-enemy-crossbow-layer))
  object)

(defun 2d-tile-visible-p (game-state tile-position ray-stopper-fn)
  (loop-ai-entities game-state #'(lambda (entity)
                                  (displace-2d-vector (tile-position x y)
                                    (when (not (funcall ray-stopper-fn entity x y))
                                      (return-from 2d-tile-visible-p t)))))
  nil)

(defun 2d-tile-visible-around-ai-p (game-state tile-position ray-stopper-fn)
  (with-accessors ((blackboard blackboard)) game-state
    (with-accessors ((layer concerning-tiles-invalicables)) blackboard
      (loop-ai-entities game-state
           #'(lambda (entity)
               (displace-2d-vector (tile-position x y)
                 ;; visible if hitted
                 (when (not (funcall ray-stopper-fn entity x y))
                   (return-from 2d-tile-visible-around-ai-p t)))))
      nil)))

(defun disgregard-all-plans (game-state)
  (loop-ai-entities game-state #'(lambda (v)
                                  (character:disgregard-tactical-plan (ghost v)))))

(defmethod calc-ai-entities-action-order ((object blackboard))
  (flet ((sort-entities (entities)
           (num-utils:multisort* entities
                                (ai-utils:multisort-ai-visibile-asc-clsr object)
                                #'ai-utils:multisort-combined-power-asc)))
    (with-accessors ((main-state main-state)) object
      (with-accessors ((ai-entities-action-order ai-entities-action-order)
                       (ai-entities              ai-entities))              main-state
        (let* ((ai-entities-sorted    (sort-entities ai-entities))
               (all-ai-entities-order (remove-if #'entity-dead-p
                                                 (copy-list ai-entities-sorted))))
          (setf ai-entities              ai-entities-sorted)
          (setf ai-entities-action-order all-ai-entities-order))))))

(defmethod fountain-exhausted-p ((object blackboard) (entity mesh:fountain-mesh-shell))
  (with-accessors ((exhausted-fountains-ids exhausted-fountains-ids)) object
    (find (id entity) exhausted-fountains-ids :test #'=)))

(defmethod log-entity-presence ((object blackboard) (entity entity) key-log)
  (let ((struct   (ai-logger:make-entity-pres :id  (id                      entity)
                                    :pos (calculate-cost-position entity)
                                    :dir (dir                     entity))))
    (pushnew struct
             (ai-logger:ai-log-data (ai-logger:get-log object key-log))
             :test #'ai-logger:equal-presence-p)
    object))

(defmethod log-ai-entity-presence ((object blackboard) (entity entity))
  (log-entity-presence object entity ai-logger:+ai-log-ai-entity-presence+))

(defmethod log-pc-entity-presence ((object blackboard) (entity entity))
  (log-entity-presence object entity ai-logger:+ai-log-pc-entity-presence+))

(defun remove-entity-from-attack-pos (positions entity)
  (remove (id entity) positions :test #'= :key #'entity-id))

(defmethod remove-entity-from-all-attack-pos ((object blackboard) entity)
  (with-accessors ((attack-enemy-melee-positions    attack-enemy-melee-positions)
                   (attack-enemy-pole-positions     attack-enemy-pole-positions)
                   (attack-enemy-bow-positions      attack-enemy-bow-positions)
                   (attack-enemy-crossbow-positions attack-enemy-crossbow-positions)) object
    (setf attack-enemy-melee-positions
          (remove-entity-from-attack-pos attack-enemy-melee-positions entity))
    (setf attack-enemy-pole-positions
          (remove-entity-from-attack-pos attack-enemy-pole-positions entity))
    (setf attack-enemy-bow-positions
          (remove-entity-from-attack-pos attack-enemy-bow-positions entity))
    (setf attack-enemy-crossbow-positions
          (remove-entity-from-attack-pos attack-enemy-crossbow-positions entity))
    object))

(defmacro with-tiles-*-spell-around (fn state entity range)
  (with-gensyms (x y)
    `(displace-2d-vector ((calculate-cost-position ,entity) ,x ,y)
       (,fn (map-state ,state) ,x ,y ,range ,range))))

(defun tiles-attack-spell-around-human (state entity range)
  (with-tiles-*-spell-around gen-valid-ring-box-position state entity range))

(defun tiles-attack-spell-around-ai (state entity range)
  (with-tiles-*-spell-around gen-valid-neighbour-position-in-box state entity range))

(defun tiles-heal-spell-around-ai (state entity range)
  (with-tiles-*-spell-around gen-valid-neighbour-position-in-box state entity range))

(defun tiles-damage-spell-around-ai (state entity range)
  (with-tiles-*-spell-around gen-valid-neighbour-position-in-box state entity range))

(defun remove-tile-empty-or-w/ignored-entities (state &rest ignored-entities)
  #'(lambda (a)
      (displace-2d-vector (a x y)
        (if (not (map-element-empty-p (element-mapstate@ state x y)))
            (not (find (entity-id-in-pos state x y)
                       (map 'list #'id ignored-entities)))
            nil))))

(defstruct spell-pos
  (tile)
  (concerning)
  (cost)
  (enemy-visible-tricky-p) ; is the enemy visible from this position but not the converse?
  (enemy-able-to-see-p))   ; is the enemy able to see this position?

(defun goal-pos->spell-struct-fn (blackboard concerning-tiles attacker-pos attacker
                                  dodged-entity)
  (declare (ignore attacker))
  #'(lambda (a)
      (with-conc-path-total-cost (cost)
          (path-with-concerning-tiles blackboard
                                      attacker-pos
                                      a
                                      :cut-off-first-tile nil
                                      :allow-path-length-1 t)
        (displace-2d-vector (a x y)
          (make-spell-pos :tile       a
                          :concerning (matrix:matrix-elt concerning-tiles y x)

                          :cost  cost
                          :enemy-visible-tricky-p
                          (and dodged-entity
                               (not (absee-mesh:placeholder-visible-p dodged-entity x y))
                               (absee-mesh:placeholder-able-to-see-p x y dodged-entity))
                          :enemy-able-to-see-p
                          (and dodged-entity
                               (absee-mesh:placeholder-visible-p dodged-entity x y)))))))

(defun multisort-spell-pos-tricky-vis< (a b)
  (if (spell-pos-enemy-visible-tricky-p a)
      (if (spell-pos-enemy-visible-tricky-p b)
          nil
          t)
      nil))

(defun multisort-spell-pos-tricky-vis> (a b)
  (if (spell-pos-enemy-visible-tricky-p a)
      (if (spell-pos-enemy-visible-tricky-p b)
          nil
          t)
      nil))

(defun calculate-single-spell-goal-pos (attacker defender pivot-entity
                                        dodged-entity
                                        spell range
                                        gen-pos-fn)
  "Calculate best position to launch spell from attacker to defender around pivot"
  (with-accessors ((state          state)
                   (attacker-ghost ghost)) attacker
    (with-accessors ((map-state map-state)
                     (blackboard blackboard)) state
      (when-let* ((concerning-smoothed (concerning-tiles->costs-matrix blackboard))
                  (defender-pos        (calculate-cost-position defender))
                  (attacker-pos        (calculate-cost-position attacker))
                  (pivot-pos           (calculate-cost-position pivot-entity))
                  (spell-range         (truncate (spell:range spell)))
                  (goal-tiles-pos      (funcall gen-pos-fn state pivot-entity range))
                  (reach-fn      (reachable-p-w/concening-tiles-fn blackboard
                                                                   :allow-path-length-1 t
                                                                   :cut-off-first-tile  nil)))
        ;; remove tiles not empty and not occupied by attacker
        (setf goal-tiles-pos (remove-if (remove-tile-empty-or-w/ignored-entities state attacker)
                                        goal-tiles-pos))
        ;; remove out of range
        (setf goal-tiles-pos (remove-if-not #'(lambda (a)
                                                (< (manhattam-distance a defender-pos)
                                                   spell-range))
                                            goal-tiles-pos))
        ;; (when (find spell-type (list :attack :damage) :test #'eq)
        ;;   ;; remove tiles from the target's point of view
        ;;   (setf goal-tiles-pos (remove-from-player-pov goal-tiles-pos
        ;;                                                defender
        ;;                                                use-enemy-fov-when-attacking-p)))
        ;; remove non reachables
        (setf goal-tiles-pos (ai-utils:remove-non-reachables-pos reach-fn
                                                                 attacker-pos
                                                                 attacker-ghost
                                                                 goal-tiles-pos
                                                                 :key #'identity))
        ;; ;; remove is enemy is not visible from tiles position
        (setf goal-tiles-pos
               (remove-if-not #'(lambda (a)
                                  (2d-utils:displace-2d-vector (a x y)
                                    (able-to-see-mesh:placeholder-visible-p defender
                                                                            x y)))
                              goal-tiles-pos))
        (let ((goal-structs (map 'list
                                 (goal-pos->spell-struct-fn blackboard
                                                            concerning-smoothed
                                                            attacker-pos
                                                            attacker
                                                            dodged-entity)
                                 goal-tiles-pos)))
          (if-difficult-level>medium (state)
            ;; sort by "not visible by  enemy", concerning (the less the
            ;; better)  and  by  cost  to   reach  the  tile  (again  in
            ;; increasing order).
            (setf goal-structs
                  (num:multisort* goal-structs
                                  (num:gen-multisort-test multisort-spell-pos-tricky-vis<
                                                          multisort-spell-pos-tricky-vis>
                                                          identity)
                                  (num:gen-multisort-test <
                                                          >
                                                          spell-pos-concerning)
                                  (num:gen-multisort-test <
                                                          >
                                                          spell-pos-cost)))
            nil)
          ;; retun the positions and the structs as well
          (values (map 'list #'spell-pos-tile goal-structs)
                  goal-structs))))))

(defmacro with-*-spell-goal-pos-skeleton (attacker defender
                                          with-spell-macro
                                          cache-fn-symbol
                                          nocache-fn-symbol
                                          search-cache-fn-symbol
                                          insert-cache-fn-symbol)
  (with-gensyms (found new-res range best-spell)
    `(labels ((make-cache-key (attacker defender spell &optional (res nil))
                (make-%atk-spell-around-me-cache-value :attacker attacker
                                                       :defender defender
                                                       :spell    spell
                                                       :res      res))
              (get-from-cache (attacker defender spell)
                (,search-cache-fn-symbol (make-cache-key attacker defender spell)))
              (put-in-cache (attacker defender spell res)
                (,insert-cache-fn-symbol (make-cache-key attacker defender spell res)))
              (extract-res (cache-value)
                (%atk-spell-around-me-cache-value-res cache-value)))
       ;; note we need  to calculate the spell here too,  This to get the
       ;; correct cache key.  Of course we are assuming the same spell is
       ;; going to be chosen by the procedure without cache.
       (,with-spell-macro (,range ,best-spell ,attacker ,defender)
         (let ((,found (get-from-cache ,attacker ,defender ,best-spell)))
           (if ,found
               (extract-res ,found)
               (let ((,new-res (,nocache-fn-symbol ,attacker ,defender)))
                 (put-in-cache ,attacker ,defender ,best-spell ,new-res)
                 (,cache-fn-symbol ,attacker ,defender))))))))

(defmacro with-attack-spell-goal-pos-skeleton (attacker defender
                                               cache-fn-symbol
                                               nocache-fn-symbol
                                               search-cache-fn-symbol
                                               insert-cache-fn-symbol)
  `(with-*-spell-goal-pos-skeleton
       ,attacker
     ,defender
     with-best-attack-spell
     ,cache-fn-symbol
     ,nocache-fn-symbol
     ,search-cache-fn-symbol
     ,insert-cache-fn-symbol))

(defmacro with-damage-spell-goal-pos-skeleton (attacker defender
                                               cache-fn-symbol
                                               nocache-fn-symbol
                                               search-cache-fn-symbol
                                               insert-cache-fn-symbol)
  `(with-*-spell-goal-pos-skeleton
       ,attacker
     ,defender
     with-best-damage-spell
     ,cache-fn-symbol
     ,nocache-fn-symbol
     ,search-cache-fn-symbol
     ,insert-cache-fn-symbol))

(defmacro with-healing-spell-goal-pos-skeleton (attacker defender
                                               cache-fn-symbol
                                               nocache-fn-symbol
                                               search-cache-fn-symbol
                                               insert-cache-fn-symbol)
  `(with-*-spell-goal-pos-skeleton
       ,attacker
     ,defender
     with-best-heal-spell
     ,cache-fn-symbol
     ,nocache-fn-symbol
     ,search-cache-fn-symbol
     ,insert-cache-fn-symbol))

(defmacro with-best-*-spell ((range best-spell attacker defender
                                    filter-spell-db-fn
                                    find-spell-fn) &body body)
  (with-gensyms (all-spells state)
    `(with-accessors ((,state state)) ,attacker
       (when-let* ((,range      (level-difficult ,state))
                   (,all-spells (,filter-spell-db-fn ,attacker))
                   (,best-spell ,(if (functionp find-spell-fn)
                                     `(funcall ,find-spell-fn
                                               ,state
                                               ,all-spells
                                               ,attacker
                                               ,defender
                                               :safe-zone-dist ,range)
                                     `(,find-spell-fn ,state
                                                      ,all-spells
                                                      ,attacker
                                                      ,defender
                                                      :safe-zone-dist ,range))))
         ,@body))))

(defmacro with-best-attack-spell ((range best-spell attacker defender) &body body)
  `(with-best-*-spell (,range ,best-spell ,attacker ,defender
                              ai-utils:available-attack-spells
                              ai-utils:find-best-attack-spell)
     ,@body))

(defun find-best-heal-spell-wrapper (state spell-db launcher target
                                     &key (safe-zone-dist 0.0))
  (declare (ignore launcher safe-zone-dist))
  (ai-utils:find-best-heal-spell state spell-db target))

(defmacro with-best-heal-spell ((range best-spell launcher target) &body body)
  `(with-best-*-spell (,range ,best-spell ,launcher ,target
                              ai-utils:available-heal-spells
                              find-best-heal-spell-wrapper)
     ,@body))

(defmacro with-best-damage-spell ((range best-spell launcher target) &body body)
  `(with-best-*-spell (,range ,best-spell ,launcher ,target
                              ai-utils:available-damage-spells
                              ai-utils:find-best-damage-spell)
     ,@body))

(defstruct %atk-spell-around-me-cache-value
  (attacker)
  (defender)
  (spell)
  (res))

(defun %atk-spell-around-me-equal (a b)
  (and (= (id (%atk-spell-around-me-cache-value-attacker a))
          (id (%atk-spell-around-me-cache-value-attacker b)))
       (= (id (%atk-spell-around-me-cache-value-defender a))
          (id (%atk-spell-around-me-cache-value-defender b)))
       (eq (spell:identifier (%atk-spell-around-me-cache-value-spell a))
           (spell:identifier (%atk-spell-around-me-cache-value-spell b)))))

(defstruct (%atk-spell-around-friend-cache-value
             (:include %atk-spell-around-me-cache-value))
  (friend))

(defun %atk-spell-around-friend-equal (a b)
  (and (= (id (%atk-spell-around-friend-cache-value-attacker a))
          (id (%atk-spell-around-friend-cache-value-attacker b)))
       (= (id (%atk-spell-around-friend-cache-value-defender a))
          (id (%atk-spell-around-friend-cache-value-defender b)))
       (= (id (%atk-spell-around-friend-cache-value-friend a))
          (id (%atk-spell-around-friend-cache-value-friend b)))
       (eq (spell:identifier (%atk-spell-around-friend-cache-value-spell a))
           (spell:identifier (%atk-spell-around-friend-cache-value-spell b)))))

(defun best-attack-spell-goal-pos-nocache (attacker defender)
  "Calculate the 'ideal' attack spell positions
Note: assuming the range  of the spell is not less  ore equal than the
effective range.  I think this is safe to assume. ;)"
  (with-accessors ((state state)) attacker
    (with-accessors ((blackboard blackboard)
                     (map-state map-state)) state
      (with-accessors ((use-enemy-fov-when-attacking-p use-enemy-fov-when-attacking-p)) blackboard
        (when-let* ((spells                 (ai-utils:available-attack-spells attacker))
                    (best-attack-spell      (ai-utils:find-best-attack-spell  state
                                                                              spells
                                                                              attacker
                                                                              defender))
                    (all-enemy-visibles-ids (all-player-id-visible-from-ai state)))
          (when (find (id defender)
                      all-enemy-visibles-ids
                      :test #'=)
            (calculate-single-spell-goal-pos attacker
                                             defender
                                             defender
                                             defender
                                             best-attack-spell
                                             (spell:range best-attack-spell)
                                             #'tiles-attack-spell-around-human)))))))

(defcached-list best-attack-spell-goal-pos ((attacker defender)
                                                 :equal-fn #'%atk-spell-around-me-equal)
  (with-attack-spell-goal-pos-skeleton attacker defender
                                       best-attack-spell-goal-pos
                                       best-attack-spell-goal-pos-nocache
                                       best-attack-spell-goal-pos-search-cache
                                       best-attack-spell-goal-pos-insert-cache))

(defun attack-spell-goal-pos-around-friend-nocache (attacker defender friend)
  (with-accessors ((state state)) attacker
    (with-accessors ((blackboard blackboard)
                     (map-state map-state)) state
      (with-accessors ((use-enemy-fov-when-attacking-p use-enemy-fov-when-attacking-p)) blackboard
        (with-best-attack-spell (range best-attack-spell attacker defender)
          (when-let* ((all-enemy-visibles-ids (all-player-id-visible-from-ai state)))
            (when (find (id defender)
                        all-enemy-visibles-ids
                        :test #'=)
              (calculate-single-spell-goal-pos attacker
                                               defender
                                               friend
                                               defender
                                               best-attack-spell
                                               range
                                               #'tiles-attack-spell-around-ai))))))))

(defcached-list attack-spell-goal-pos-around-friend ((attacker defender friend)
                                                 :equal-fn #'%atk-spell-around-friend-equal)
  (labels ((make-cache-key (attacker defender friend spell &optional (res nil))
             (make-%atk-spell-around-friend-cache-value :attacker attacker
                                                        :defender defender
                                                        :spell    spell
                                                        :friend   friend
                                                        :res      res))
           (get-from-cache (attacker defender friend spell)
             (attack-spell-goal-pos-around-friend-search-cache (make-cache-key attacker
                                                                               defender
                                                                               friend
                                                                               spell)))
           (put-in-cache (attacker defender friend spell res)
             (attack-spell-goal-pos-around-friend-insert-cache (make-cache-key attacker
                                                                               defender
                                                                               friend
                                                                               spell
                                                                               res)))
           (extract-res (cache-value)
             (%atk-spell-around-friend-cache-value-res cache-value)))
    ;; note we need  to calculate the spell here too,  This to get the
    ;; correct cache key.  Of course we are assuming the same spell is
    ;; going to be chosen by the procedure without cache.
    (with-best-attack-spell (range best-spell attacker defender)
      (let ((found (get-from-cache attacker defender friend best-spell)))
        (if found
            (extract-res found)
            (let ((new-res (attack-spell-goal-pos-around-friend-nocache attacker defender friend)))
              (put-in-cache attacker defender friend best-spell new-res)
            (attack-spell-goal-pos-around-friend attacker defender friend)))))))

(defun attack-spell-goal-pos-around-me-nocache (attacker defender)
  (attack-spell-goal-pos-around-friend attacker defender attacker))

(defcached-list attack-spell-goal-pos-around-me ((attacker defender)
                                                 :equal-fn #'%atk-spell-around-me-equal)
  (with-attack-spell-goal-pos-skeleton attacker defender
                                       attack-spell-goal-pos-around-me
                                       attack-spell-goal-pos-around-me-nocache
                                       attack-spell-goal-pos-around-me-search-cache
                                       attack-spell-goal-pos-around-me-insert-cache))

(defun heal-spell-goal-pos-around-friend-nocache (launcher target)
  (with-accessors ((state state)) launcher
    (with-accessors ((blackboard blackboard)
                     (map-state map-state)) state
      (with-accessors ((use-enemy-fov-when-attacking-p use-enemy-fov-when-attacking-p)) blackboard
        (with-best-heal-spell (range best-heal-spell launcher target)
          (calculate-single-spell-goal-pos launcher
                                           target
                                           target
                                           nil
                                           best-heal-spell
                                           range
                                           #'tiles-heal-spell-around-ai))))))

(defcached-list heal-spell-goal-pos-around-friend ((launcher target friend)
                                                 :equal-fn #'%atk-spell-around-friend-equal)
  (labels ((make-cache-key (launcher target friend spell &optional (res nil))
             (make-%atk-spell-around-friend-cache-value :attacker launcher
                                                        :defender target
                                                        :spell    spell
                                                        :friend   friend
                                                        :res      res))
           (get-from-cache (launcher target friend spell)
             (heal-spell-goal-pos-around-friend-search-cache (make-cache-key launcher
                                                                               target
                                                                               friend
                                                                               spell)))
           (put-in-cache (launcher target friend spell res)
             (heal-spell-goal-pos-around-friend-insert-cache (make-cache-key launcher
                                                                               target
                                                                               friend
                                                                               spell
                                                                               res)))
           (extract-res (cache-value)
             (%atk-spell-around-friend-cache-value-res cache-value)))
    ;; note we need  to calculate the spell here too,  This to get the
    ;; correct cache key.  Of course we are assuming the same spell is
    ;; going to be chosen by the procedure without cache.
    (with-best-heal-spell (range best-spell launcher target)
      (let ((found (get-from-cache launcher target friend best-spell)))
        (if found
            (extract-res found)
            (let ((new-res (heal-spell-goal-pos-around-friend-nocache launcher target)))
              (put-in-cache launcher target friend best-spell new-res)
              (heal-spell-goal-pos-around-friend launcher target friend)))))))

(defun heal-spell-goal-pos-around-me-nocache (launcher target)
  (heal-spell-goal-pos-around-friend launcher target launcher))

(defcached-list heal-spell-goal-pos-around-me ((launcher target)
                                                 :equal-fn #'%atk-spell-around-me-equal)
  (with-healing-spell-goal-pos-skeleton launcher target
                                        heal-spell-goal-pos-around-me
                                        heal-spell-goal-pos-around-me-nocache
                                        heal-spell-goal-pos-around-me-search-cache
                                        heal-spell-goal-pos-around-me-insert-cache))

(defun damage-spell-goal-pos-around-friend-nocache (launcher target)
  (with-accessors ((state state)) launcher
    (with-accessors ((blackboard blackboard)
                     (map-state map-state)) state
      (with-accessors ((use-enemy-fov-when-attacking-p use-enemy-fov-when-attacking-p)) blackboard
        (with-best-damage-spell (range best-damage-spell launcher target)
          (calculate-single-spell-goal-pos launcher
                                           target
                                           target
                                           target
                                           best-damage-spell
                                           range
                                           #'tiles-damage-spell-around-ai))))))

(defcached-list damage-spell-goal-pos-around-friend ((launcher target friend)
                                                 :equal-fn #'%atk-spell-around-friend-equal)
  (labels ((make-cache-key (launcher target friend spell &optional (res nil))
             (make-%atk-spell-around-friend-cache-value :attacker launcher
                                                        :defender target
                                                        :spell    spell
                                                        :friend   friend
                                                        :res      res))
           (get-from-cache (launcher target friend spell)
             (damage-spell-goal-pos-around-friend-search-cache (make-cache-key launcher
                                                                               target
                                                                               friend
                                                                               spell)))
           (put-in-cache (launcher target friend spell res)
             (damage-spell-goal-pos-around-friend-insert-cache (make-cache-key launcher
                                                                               target
                                                                               friend
                                                                               spell
                                                                               res)))
           (extract-res (cache-value)
             (%atk-spell-around-friend-cache-value-res cache-value)))
    ;; note we need  to calculate the spell here too,  This to get the
    ;; correct cache key.  Of course we are assuming the same spell is
    ;; going to be chosen by the procedure without cache.
    (with-best-damage-spell (range best-spell launcher target)
      (let ((found (get-from-cache launcher target friend best-spell)))
        (if found
            (extract-res found)
            (let ((new-res (damage-spell-goal-pos-around-friend-nocache launcher target)))
              (put-in-cache launcher target friend best-spell new-res)
              (damage-spell-goal-pos-around-friend launcher target friend)))))))

(defun damage-spell-goal-pos-around-me-nocache (launcher target)
  (damage-spell-goal-pos-around-friend launcher target launcher))

(defcached-list damage-spell-goal-pos-around-me ((launcher target)
                                                 :equal-fn #'%atk-spell-around-me-equal)
  (with-damage-spell-goal-pos-skeleton launcher target
                                        damage-spell-goal-pos-around-me
                                        damage-spell-goal-pos-around-me-nocache
                                        damage-spell-goal-pos-around-me-search-cache
                                        damage-spell-goal-pos-around-me-insert-cache))


(defun invalidate-blackboard-cache ()
  (attack-spell-goal-pos-around-friend-clear-cache)
  (attack-spell-goal-pos-around-me-clear-cache)
  (heal-spell-goal-pos-around-friend-clear-cache)
  (heal-spell-goal-pos-around-me-clear-cache)
  (damage-spell-goal-pos-around-friend-clear-cache)
  (damage-spell-goal-pos-around-me-clear-cache)
  (reachable-p-w/concening-tiles-fn-clear-cache)
  (reachable-p-w/concening-tiles-unlimited-cost-fn-clear-cache))
