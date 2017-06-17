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

(define-constant +goal-attack-occupied-value+      100.0 :test #'=)

(define-constant +explored-tile-value+             100.0 :test #'=)

(define-constant +unexplored-tile-value+             0.0 :test #'=)

(define-constant +concerning-tile-value+           100.0 :test #'=)

(define-constant +attack-nongoal-tile-value+         5.0 :test #'=)

(define-constant +length-line-of-sight-inf+       1000.0 :test #'=)

(define-constant +goal-tile-value+                   0.0 :test #'=)

(defclass entity-taker ()
  ((entity-id
    :initform nil
    :initarg :entity-id
    :accessor entity-id)))

(defclass def-target (entity-taker)
  ((goal-pos
    :initform '()
    :initarg  :goal-pos
    :accessor goal-pos)))

(defgeneric max-attackers (object))

(defmethod max-attackers ((object def-target))
  (length (goal-pos object)))

(defmethod print-object ((object def-target) stream)
  (format stream "~a[~a]" (entity-id object) (max-attackers object)))

(defun make-defender-target (id &optional (goal-pos '()))
  (make-instance 'def-target :entity-id id :goal-pos goal-pos))

(defclass attacker (entity-taker)
  ((target-id
    :initform nil
    :initarg  :target-id
    :accessor target-id)
   (target-pos
    :initform nil
    :initarg  :target-pos
    :accessor target-pos)))

(defclass blackboard ()
  ((main-state
    :initform nil
    :initarg  :main-state
    :accessor main-state
    :type     game-state)
   (visited-tiles
    :initform nil
    :initarg  :visited-tiles
    :accessor visited-tiles
    :type matrix)
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
   (attack-enemy-melee-layer
    :initform nil
    :initarg  :attack-enemy-melee-layer
    :accessor attack-enemy-melee-layer
    :type     dijkstra-layer
    :documentation "This holds the position the player with melee weapon (except pole weapon) should reach to attack the enemy")
   (attack-enemy-pole-layer
    :initform nil
    :initarg  :attack-enemy-pole-layer
    :accessor attack-enemy-pole-layer
    :type     dijkstra-layer
    :documentation "This holds the position the player with pole weapon should reach to attack the enemy")
   (attack-enemy-bow-layer
    :initform nil
    :initarg  :attack-enemy-bow-layer
    :accessor attack-enemy-bow-layer
    :type     dijkstra-layer
    :documentation "This holds the position the player with bow weapon should reach to attack the enemy")
   (attack-enemy-crossbow-layer
    :initform nil
    :initarg  :attack-enemy-crossbow-layer
    :accessor attack-enemy-crossbow-layer
    :type     dijkstra-layer
    :documentation "This holds the position the player with crossbow weapon should reach to attack the enemy")
   (attack-enemy-melee-positions
    :initform '()
    :initarg  :attack-enemy-melee-positions
    :accessor attack-enemy-melee-positions
    :documentation "This holds the position the positions with melee weapon (except pole weapon) should reach to attack the enemy")
   (attack-enemy-pole-positions
    :initform '()
    :initarg  :attack-enemy-pole-positions
    :accessor attack-enemy-pole-positions
    :documentation "This holds the position the positions with pole weapon should reach to attack the enemy")
   (attack-enemy-bow-positions
    :initform '()
    :initarg  :attack-enemy-bow-positions
    :accessor attack-enemy-bow-positions
    :documentation "This holds the position the positions with bow weapon should reach to attack the enemy")
   (attack-enemy-crossbow-positions
    :initform '()
    :initarg  :attack-enemy-crossbow-positions
    :accessor attack-enemy-crossbow-positions
    :documentation "This holds the position the positions with crossbow weapon should reach to attack the enemy")
   (use-enemy-fov-when-exploring
    :initform nil
    :initarg  :use-enemy-fov-when-exploring-p
    :reader   use-enemy-fov-when-exploring-p
    :writer   (setf use-enemy-fov-when-exploring))
   (use-enemy-fov-when-attacking
    :initform nil
    :initarg  :use-enemy-fov-when-attacking-p
    :reader   use-enemy-fov-when-attacking-p
    :writer   (setf use-enemy-fov-when-attacking))))

(defmethod initialize-instance :after ((object blackboard) &key &allow-other-keys)
  (with-accessors ((visited-tiles    visited-tiles)
                   (concerning-tiles concerning-tiles)
                   (unexplored-layer unexplored-layer)
                   (attack-enemy-melee-layer     attack-enemy-melee-layer)
                   (attack-enemy-pole-layer      attack-enemy-pole-layer)
                   (attack-enemy-bow-layer       attack-enemy-bow-layer)
                   (attack-enemy-crossbow-layer  attack-enemy-crossbow-layer)
                   (use-enemy-fov-when-exploring use-enemy-fov-when-exploring)
                   (use-enemy-fov-when-attacking use-enemy-fov-when-attacking)
                   (main-state       main-state)) object
    (let ((wmap (width  (map-state main-state)))
          (hmap (height (map-state main-state))))
      (setf visited-tiles    (make-matrix wmap hmap nil))
      (setf concerning-tiles (make-matrix wmap hmap 0.0))
      (setf unexplored-layer (inmap:make-dijkstra-layer wmap hmap +unexplored-tile-value+))
      (setf attack-enemy-melee-layer
            (inmap:make-dijkstra-layer wmap hmap +attack-nongoal-tile-value+))
      (setf attack-enemy-pole-layer
            (inmap:make-dijkstra-layer wmap hmap +attack-nongoal-tile-value+))
      (setf attack-enemy-bow-layer
            (inmap:make-dijkstra-layer wmap hmap +attack-nongoal-tile-value+))
      (setf attack-enemy-crossbow-layer
            (inmap:make-dijkstra-layer wmap hmap +attack-nongoal-tile-value+))
      ;; setting smartness based on level
      (when (>= (level-difficult main-state) 3)
        (setf use-enemy-fov-when-exploring nil
              use-enemy-fov-when-attacking t)))))

(defun decrease-concerning (game-state concerning-map)
  (let* ((level           (level-difficult game-state))
         (decrease-rate   (d  (/ 1 level)))
         (decrease-amount (d/ +concerning-tile-value+ 3.0)))
    (loop-matrix (concerning-map x y)
       (when (d> (matrix-elt concerning-map y x) 0.0)
         (let ((new-value (d- (matrix-elt concerning-map y x)
                              (d* decrease-rate decrease-amount))))
           (if (d> new-value 0.0)
               (setf (matrix-elt concerning-map y x) new-value)
               (setf (matrix-elt concerning-map y x) 0.0)))))))

(defun %update-all-layers (blackboard)
  (setf (attack-enemy-melee-positions    blackboard) nil
        (attack-enemy-pole-positions     blackboard) nil
        (attack-enemy-bow-positions      blackboard) nil
        (attack-enemy-crossbow-positions blackboard) nil)
  (update-unexplored-layer               blackboard)
  (update-attack-melee-layer             blackboard)
  (update-attack-pole-layer              blackboard)
  (update-attack-bow-layer               blackboard)
  (update-attack-crossbow-layer          blackboard))

(defmethod game-event:on-game-event ((object blackboard) (event game-event:end-turn))
  (with-accessors ((concerning-tiles concerning-tiles)) object
    (decrease-concerning (main-state object) concerning-tiles)
    (%update-all-layers object)
    ;; ;; test
    ;; (let ((pix (inmap:dijkstra-layer->pixmap (attack-enemy-pole-layer object))))
    ;;   (pixmap:save-pixmap pix (fs:file-in-package "pole.tga")))
    nil))

(defmethod main-state ((object blackboard))
  (slot-value object 'main-state))

(defmethod (setf main-state) (new-state (object blackboard))
  (with-slots (main-state) object
    (setf main-state new-state)))

(defgeneric strategy-decision (object))

(defgeneric update-unexplored-layer (object))

(defgeneric update-unexplored-layer-player (object player &key all-visibles-from-ai))

(defgeneric next-unexplored-position (object player))

(defgeneric update-attack-melee-layer (object))

(defgeneric update-attack-melee-layer-player (object player &key all-visibles-from-ai))

(defgeneric update-attack-pole-layer (object))

(defgeneric update-attack-bow-layer (object))

(defgeneric update-attack-crossbow-layer (object))

(defgeneric update-attack-pole-layer-player (object player &key all-visibles-from-ai))

(defgeneric update-attack-bow-layer-player (object player &key all-visibles-from-ai))

(defgeneric update-attack-crossbow-layer-player (object player &key all-visibles-from-ai))

;; TODO use decision tree
(defmethod strategy-decision ((object blackboard))
  (declare (ignore object))
  +explore-strategy+)

(defmethod set-tile-visited ((object blackboard) x y)
  (call-next-method object
                    (map-utils:coord-chunk->matrix x)
                    (map-utils:coord-chunk->matrix y)))

(defmethod set-tile-visited ((object blackboard) (x fixnum) (y fixnum))
  (with-accessors ((visited-tiles visited-tiles)
                   (main-state main-state)
                   (unexplored-layer unexplored-layer)) object
    ;; mark visited
    (setf (matrix-elt visited-tiles y x) t)
    (%update-all-layers object)))

(defun calc-danger-zone-size (difficult-level)
  "The size of the concerning zone when when some concerning event occurs"
  (ceiling (dlerp (smoothstep-interpolate 2.0 5.0 difficult-level)
                  4.0
                  8.0)))

(defmethod set-concerning-tile ((object blackboard) x y
                                &key
                                  (danger-zone-size (let* ((main-state (main-state object))
                                                           (level (level-difficult main-state)))
                                                      (calc-danger-zone-size level))))
  (with-accessors ((concerning-tiles concerning-tiles)) object
    (let* ((dangerous-tiles-pos (gen-neighbour-position-in-box x
                                                               y
                                                               danger-zone-size
                                                               danger-zone-size)))
      (loop for point across dangerous-tiles-pos do
           (displace-2d-vector (point x y)
             (with-check-matrix-borders (concerning-tiles x y)
               (incf (matrix-elt concerning-tiles y x) +concerning-tile-value+)))))))

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
    (loop-matrix (layer x y)
       (setf (matrix-elt layer y x) value))))

(defun reset-explored-layer (unexplored-layer)
  (reset-layer unexplored-layer +unexplored-tile-value+))

(defun reset-attack-layer (unexplored-layer)
  (reset-layer unexplored-layer +attack-nongoal-tile-value+))

(defun all-player-id-visible-from-ai (game-state)
  (let ((all-visibles '()))
    (map-ai-entities game-state
                     #'(lambda (k v)
                         (declare (ignore k))
                         (let ((visibles (able-to-see-mesh:other-faction-visible-players v)))
                           (loop for visible in visibles do
                                (pushnew visible all-visibles :key #'id :test #'=)))))
    (map 'list #'id all-visibles)))

(defun nsuperimpose-layer (from destination &key (fn #'(lambda (a b)
                                                         (declare (ignore b))
                                                         a)))
  (loop-matrix (destination x y)
     (setf (matrix-elt destination y x)
           (funcall fn (matrix-elt from y x) (matrix-elt destination y x)))))

(defun matrix-update-values (layer positions visible-positions new-value)
  (loop for point across positions do
       (displace-2d-vector (point x y)
         (with-check-matrix-borders (layer x y)
           (when (find point visible-positions :test #'ivec2=)
             (setf (matrix-elt layer y x) new-value))))))

(defmethod update-unexplored-layer ((object blackboard))
  (with-accessors ((visited-tiles visited-tiles)
                   (main-state main-state)
                   (concerning-tiles concerning-tiles)
                   (unexplored-layer unexplored-layer)) object
    (with-accessors ((layer layer)) unexplored-layer
      (reset-explored-layer unexplored-layer)
      (let ((all-visibles (all-player-id-visible-from-ai main-state)))
        ;; calculate concerning tiles tiles occupied by enemies
        (map-player-entities main-state
                             #'(lambda (k player)
                                 (declare (ignore k))
                                 (update-unexplored-layer-player object
                                                                 player
                                                                 :all-visibles-from-ai
                                                                 all-visibles)))
        ;; add-concerning tiles
        (nsuperimpose-layer concerning-tiles layer)
        ;; mark already visited tiles
        (loop-matrix (layer x y)
           (when (matrix-elt visited-tiles y x)
             (setf (matrix-elt layer y x) +explored-tile-value+)))
        ;; smooth the map here
        (let* ((max-mp (max-ai-movement-points main-state))
               (skip-fn #'(lambda (el-type pos)
                           (let ((cost (get-cost main-state (elt pos 0) (elt pos 1))))
                             (or (inmap:skippablep el-type pos)
                                 (find (entity-id-in-pos main-state (elt pos 0) (elt pos 1))
                                       all-visibles
                                       :test #'=)
                                 (> cost max-mp))))))
          (inmap:smooth-dijkstra-layer (blackboard:unexplored-layer object)
                                       main-state
                                       :skippable-predicate skip-fn))))))

(defun %2d-ray-stopper (player x y use-fov)
  "x  and y  in  cost-map  space (integer  coordinates),  values t  if
   somenthing prevented the ray to reach the target position"
  (with-accessors ((state state)) player
    (let* ((visiblep        (if use-fov
                                (able-to-see-mesh:placeholder-visible-p     player x y)
                                (able-to-see-mesh:placeholder-visible-ray-p player x y))))
      (or (not (map-element-empty-p (element-mapstate@ state x y)))
          (not visiblep)))))

(defmethod update-unexplored-layer-player ((object blackboard) (player md2-mesh:md2-mesh)
                                           &key
                                             (all-visibles-from-ai
                                              (all-player-id-visible-from-ai (state player))))
  (with-accessors ((unexplored-layer               unexplored-layer)
                   (visited-tiles                  visited-tiles)
                   (concerning-tiles               concerning-tiles)
                   (main-state                     main-state)
                   (use-enemy-fov-when-exploring-p use-enemy-fov-when-exploring-p)) object
    (with-accessors ((layer layer)) unexplored-layer
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
                                                                      danger-zone-size)))
             (loop for point across dangerous-tiles-pos do
                  (displace-2d-vector (point x y)
                    (with-check-matrix-borders (layer x y)
                      (when (not (%2d-ray-stopper player x y
                                                  use-enemy-fov-when-exploring-p))
                        (setf (matrix-elt concerning-tiles y x)
                              +concerning-tile-value+))))))))))))

(defmethod next-unexplored-position ((object blackboard) (player md2-mesh:md2-mesh))
  (with-accessors ((unexplored-layer unexplored-layer)) object
    (with-accessors ((layer layer)) unexplored-layer
      (let* ((position (pos-entity-chunk->cost-pos (pos player)))
             (x-player (elt position 0))
             (y-player (elt position 1))
             (neighbour-elements (remove-if-not
                                  #'(lambda (a)
                                      (displace-2d-vector (a x y)
                                        (valid-index-p layer y x)))
                                  (gen-4-neighbour-counterclockwise x-player
                                                                    y-player
                                                                    :add-center nil)))
             (compare-fn         #'(lambda (a b)
                                     (let ((cost-a (matrix-elt layer
                                                               (elt a 1)
                                                               (elt a 0)))
                                           (cost-b (matrix-elt layer
                                                               (elt b 1)
                                                               (elt b 0))))
                                       (cond
                                         ((null cost-a)
                                          nil)
                                         ((null cost-b)
                                          t)
                                         (t
                                          (< cost-a cost-b))))))
             (min                (find-min-max compare-fn neighbour-elements)))
        (list (sequence->ivec2 min))))))

(defun %2d-ray-stopper-melee-fn (player x y)
  "x and y in cost-map space (integer coordinates)"
  (with-accessors ((state state)) player
    (multiple-value-bind (visiblep entity-hitted)
        (able-to-see-mesh:placeholder-visible-p player x y)
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
           (if use-enemy-fov-when-attacking-p
               (setf goal-tiles-pos
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
                                goal-tiles-pos))
               (setf goal-tiles-pos
                     (remove-if-not #'(lambda (a)
                                        (displace-2d-vector (a x y)
                                          (able-to-see-mesh:placeholder-visible-ray-p player
                                                                                      x
                                                                                      y)))
                                    goal-tiles-pos)))
           (loop
              for point in (map 'list #'identity goal-tiles-pos)
              when (2d-tile-visible-p main-state
                                      (sequence->ivec2 point)
                                      #'%2d-ray-stopper-melee-fn) ; skip tiles not visible by AI
              collect point)))))))

(defun %update-attack-layer (blackboard djk-map update-fn)
  (with-accessors ((main-state               main-state)
                   (concerning-tiles         concerning-tiles)
                   (attack-enemy-melee-layer attack-enemy-melee-layer)
                   (unexplored-layer         unexplored-layer)) blackboard
    (with-accessors ((layer layer)) djk-map
      (reset-attack-layer djk-map)
      (let ((all-visibles (all-player-id-visible-from-ai main-state)))
        ;; add-concerning tiles
        (nsuperimpose-layer concerning-tiles layer :fn #'max)
        (map-player-entities main-state  #'(lambda (k player)
                                             (declare (ignore k))
                                             (funcall update-fn
                                                      blackboard
                                                      player
                                                      :all-visibles-from-ai
                                                      all-visibles)))
        (inmap:smooth-dijkstra-layer djk-map main-state)))))

(defun push-target-position (bag id-player target-pos)
  (let ((target-found (find-if #'(lambda (a) (= (entity-id a) id-player)) bag)))
    (if target-found
        (pushnew target-pos (goal-pos target-found) :test #'ivec2=)
        (push (make-defender-target id-player (list target-pos)) bag))
    bag))

(defmethod update-attack-melee-layer-player ((object blackboard) (player md2-mesh:md2-mesh)
                                             &key
                                               (all-visibles-from-ai
                                                (all-player-id-visible-from-ai (state player))))
  (with-accessors ((attack-enemy-melee-layer     attack-enemy-melee-layer)
                   (attack-enemy-melee-positions attack-enemy-melee-positions)) object
    (with-accessors ((layer layer)) attack-enemy-melee-layer
      (let* ((goal-generator-fn #'(lambda (x y)
                                    (gen-4-neighbour-counterclockwise x y :add-center nil)))
             (goal-tiles-pos    (%attack-layer-player-goal-pos object
                                                               layer
                                                               player
                                                               all-visibles-from-ai
                                                               goal-generator-fn)))
        (loop
           for point in goal-tiles-pos do
             (displace-2d-vector (point x y)
               (with-check-matrix-borders (layer x y)
                 (when (not (goal-canceled-for-neighbour-p all-visibles-from-ai
                                                           player
                                                           point))
                   ;; update position as well
                   (setf attack-enemy-melee-positions
                         (push-target-position attack-enemy-melee-positions
                                               (id player)
                                               point))
                   (setf (matrix-elt layer y x)
                         +goal-tile-value+)))))))))

(defmethod update-attack-melee-layer ((object blackboard))
  (with-accessors ((main-state main-state)
                   (concerning-tiles concerning-tiles)
                   (attack-enemy-melee-layer attack-enemy-melee-layer)
                   (unexplored-layer unexplored-layer)) object
    (%update-attack-layer object attack-enemy-melee-layer #'update-attack-melee-layer-player)))

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
         5.0)))))

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

(defun %pole-weapon-goal-generator-fn (player)
  (let ((level (level-difficult (state player))))
    #'(lambda (x y)
        (let ((size (* 2 +weapon-pole-range+)))
          (remove-if-not #'(lambda (a)
                             (<= (manhattam-distance a (ivec2 x y))
                                 (/ size 2)))
                         (cond
                           ((< level 2)
                            (gen-4-neighbour-counterclockwise x y :add-center nil))
                           ((<= 2 level 3)
                            (if (die-utils:pass-d2 2)
                                (gen-4-neighbour-counterclockwise x y :add-center nil)
                                (gen-ring-box-position x y size size)))
                           (t
                            (gen-ring-box-position x y size size))))))))

(defmethod update-attack-pole-layer-player ((object blackboard) (player md2-mesh:md2-mesh)
                                            &key
                                              (all-visibles-from-ai
                                               (all-player-id-visible-from-ai (state player))))
  (with-accessors ((attack-enemy-pole-layer attack-enemy-pole-layer)
                   (attack-enemy-pole-positions attack-enemy-pole-positions)) object
    (with-accessors ((layer layer)) attack-enemy-pole-layer
      (let* ((goal-generator-fn (%pole-weapon-goal-generator-fn player))
             (goal-tiles-pos    (%attack-layer-player-goal-pos object
                                                               layer
                                                               player
                                                               all-visibles-from-ai
                                                               goal-generator-fn)))
        (loop
           for point in goal-tiles-pos do
             (displace-2d-vector (point x y)
               (with-check-matrix-borders (layer x y)
                 (when (not (goal-canceled-for-neighbour-p all-visibles-from-ai
                                                           player
                                                           point))
                   ;; update position as well
                   (setf attack-enemy-pole-positions
                         (push-target-position attack-enemy-pole-positions
                                               (id player)
                                               point))
                   (setf (matrix-elt layer y x)
                         +goal-tile-value+)))))))))

(defun %long-range-weapon-goal-generator-fn (player range)
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

(defun %update-attack-long-range-layer-player (blackboard player djk-map
                                               target-positions-bag
                                               all-visibles-from-ai range)
  (with-accessors ((layer layer)) djk-map
    (let* ((goal-generator-fn (%long-range-weapon-goal-generator-fn player range))
           (goal-tiles-pos    (%attack-layer-player-goal-pos blackboard
                                                             layer
                                                             player
                                                             all-visibles-from-ai
                                                             goal-generator-fn)))
      (loop
         for point in goal-tiles-pos do
           (displace-2d-vector (point x y)
             (with-check-matrix-borders (layer x y)
               (when (not (goal-canceled-for-neighbour-p all-visibles-from-ai
                                                         player
                                                         point))
                 ;; update position as well
                   (setf target-positions-bag
                         (push-target-position target-positions-bag
                                               (id player)
                                               point))
                 (setf (matrix-elt layer y x)
                       +goal-tile-value+))))))))

(defmethod update-attack-bow-layer-player ((object blackboard) (player md2-mesh:md2-mesh)
                                             &key
                                               (all-visibles-from-ai
                                                (all-player-id-visible-from-ai (state player))))
  (with-accessors ((attack-enemy-bow-layer attack-enemy-bow-layer)
                   (attack-enemy-bow-positions attack-enemy-bow-positions)) object
      (%update-attack-long-range-layer-player object
                                              player
                                              attack-enemy-bow-layer
                                              attack-enemy-bow-positions
                                              all-visibles-from-ai
                                              (floor (d/ (d +weapon-bow-range+) 2.0)))))

(defmethod update-attack-crossbow-layer-player ((object blackboard) (player md2-mesh:md2-mesh)
                                             &key
                                               (all-visibles-from-ai
                                                (all-player-id-visible-from-ai (state player))))
  (with-accessors ((attack-enemy-crossbow-layer attack-enemy-crossbow-layer)
                   (attack-enemy-crossbow-positions attack-enemy-crossbow-positions)) object
      (%update-attack-long-range-layer-player object
                                              player
                                              attack-enemy-crossbow-layer
                                              attack-enemy-crossbow-positions
                                              all-visibles-from-ai
                                              (floor (d/ (d +weapon-crossbow-range+) 2.0)))))

(defmethod update-attack-pole-layer ((object blackboard))
  (with-accessors ((main-state main-state)
                   (concerning-tiles concerning-tiles)
                   (attack-enemy-pole-layer attack-enemy-pole-layer)
                   (unexplored-layer unexplored-layer)) object
    (%update-attack-layer object attack-enemy-pole-layer #'update-attack-pole-layer-player)))

(defmethod update-attack-bow-layer ((object blackboard))
  (with-accessors ((main-state main-state)
                   (concerning-tiles concerning-tiles)
                   (attack-enemy-bow-layer attack-enemy-bow-layer)
                   (unexplored-layer unexplored-layer)) object
    (%update-attack-layer object attack-enemy-bow-layer #'update-attack-bow-layer-player)))

(defmethod update-attack-crossbow-layer ((object blackboard))
  (with-accessors ((main-state main-state)
                   (concerning-tiles concerning-tiles)
                   (attack-enemy-crossbow-layer attack-enemy-crossbow-layer)
                   (unexplored-layer unexplored-layer)) object
    (%update-attack-layer object
                          attack-enemy-crossbow-layer
                          #'update-attack-crossbow-layer-player)))

(defun 2d-tile-visible-p (game-state tile-position ray-stopper-fn)
  (map-ai-entities game-state #'(lambda (k entity)
                                  (declare (ignore k))
                                  (displace-2d-vector (tile-position x y)
                                    (when (not (funcall ray-stopper-fn entity x y))
                                      (return-from 2d-tile-visible-p t)))))
  nil)

(defun disgregard-all-plans (game-state)
  (map-ai-entities game-state #'(lambda (k v)
                                  (declare (ignore k))
                                  (character:disgregard-tactical-plan (ghost v)))))
