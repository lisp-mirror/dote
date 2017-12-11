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

(in-package :ai-utils)

(define-constant +inc-point-attackable+                   10.0 :test #'=)

(define-constant +scaling-target-damage-best-attack-spell+ 1.2 :test #'=)

(define-constant +reward-possible-hi-level+               33.0 :test #'=)

(define-constant +reward-possible-low-level+              10.0 :test #'=)

(define-constant +attack-least-powerful-hi-level-chance+  70.0 :test #'=)

(define-constant +attack-least-powerful-low-level-chance+ 50.0 :test #'=)

(defun gen-neigh-costs (entity cost-map-fn)
  (with-accessors ((state entity:state)) entity
    (with-accessors ((map-state game-state:map-state)) state
      (let* ((pos        (mesh:calculate-cost-position entity))
             (x          (elt pos 0))
             (y          (elt pos 1))
             (neigh      (matrix:gen-valid-4-neighbour-counterclockwise map-state
                                                                        x y
                                                                        :add-center nil))
             (neigh-cost (mapcar cost-map-fn neigh)))
    neigh-cost))))

(defun gen-neigh (entity)
  (with-accessors ((state entity:state)) entity
    (with-accessors ((map-state game-state:map-state)) state
      (let* ((pos   (mesh:calculate-cost-position entity))
             (x     (elt pos 0))
             (y     (elt pos 1)))
        (matrix:gen-valid-4-neighbour-counterclockwise map-state
                                                       x y
                                                       :add-center nil)))))

(defun health-under-threshold-p (entity threshold)
  (let ((ghost (entity:ghost entity)))
    (< (character:current-damage-points ghost)
       (* threshold (character:actual-damage-points ghost)))))

(defun too-low-health-p (entity)
  (health-under-threshold-p entity 0.5))

(defun near-to-death-health-p (entity)
  (health-under-threshold-p entity 0.1))

(defun friend-who-needs-help (strategy-expert entity &key (exclude-me nil))
  (loop-ai-entities (interfaces:main-state strategy-expert)
                   #'(lambda (v)
                       (when (and (too-low-health-p v)
                                  (or (not exclude-me)
                                      (/= (identificable:id v) (identificable:id entity))))
                           (return-from friend-who-needs-help v))))
  nil)

(defun find-best-spell-most-powerful (spell-db)
  (first-elt (shellsort spell-db (spell:sort-spells-by-level-fn t))))

(defun find-best-heal-spell-clever-predicate (entity)
  #'(lambda (s)
      (let ((heal-fx (plist-path-value
                      (basic-interaction-params s)
                      '(:healing-effects :heal-damage-points))))
        (>= (points heal-fx)
            (character:actual-damage-points (ghost entity))))))

(defun find-best-attack-spell-clever-predicate (launcher-entity target-entity)
  (let ((dist (map-utils:map-manhattam-distance (mesh:calculate-cost-position launcher-entity)
                                                (mesh:calculate-cost-position target-entity))))
    #'(lambda (s)
        (let ((damage-spell          (spell:damage-inflicted s))
              (effective-range-spell (spell:effective-range s))
              (damage-points-target  (character:current-damage-points (ghost target-entity))))
          (and (< effective-range-spell dist)
               (>= damage-spell
                   (d* +scaling-target-damage-best-attack-spell+
                       damage-points-target)))))))

(defun find-best-attack-spell-harmless-to-launcher-clrs (launcher-entity target-entity)
  (let ((dist (map-utils:map-manhattam-distance (mesh:calculate-cost-position launcher-entity)
                                                (mesh:calculate-cost-position target-entity))))
    #'(lambda (s)
        (let ((effective-range-spell (spell:effective-range s)))
          (< effective-range-spell dist)))))

(defun find-best-spell-clever (spell-db predicate &key (use-best-if-not-found t))
  (let* ((sorted-spell-db (shellsort spell-db (spell:sort-spells-by-level-fn nil)))
         (spell (find-if predicate
                         sorted-spell-db)))
    (or spell
        (and use-best-if-not-found
             (last-elt sorted-spell-db)))))

(defun find-best-heal-spell-clever (spell-db entity)
  (find-best-spell-clever spell-db (find-best-heal-spell-clever-predicate entity)
                          :use-best-if-not-found t))

(defun find-best-attack-spell-clever (spell-db launcher-entity target-entity)
  (or (find-best-spell-clever spell-db
                              (find-best-attack-spell-clever-predicate launcher-entity
                                                                       target-entity)
                              :use-best-if-not-found nil)
      (find-best-spell-clever spell-db
                              (find-best-attack-spell-harmless-to-launcher-clrs launcher-entity
                                                                                target-entity)
                              :use-best-if-not-found nil)))

(defmacro if-difficult-level>medium ((game-state) if-more if-less)
  `(if (> (level-difficult ,game-state)
          +difficult-medium+)
       ,if-more
       ,if-less))

(defun find-best-heal-spell (state spell-db entity)
  (if-difficult-level>medium (state)
    (find-best-heal-spell-clever   spell-db entity)
    (find-best-spell-most-powerful spell-db)))

(defun find-best-attack-spell (state spell-db launcher-entity target-entity)
  (if-difficult-level>medium (state)
    (find-best-attack-spell-clever spell-db launcher-entity target-entity)
    (find-best-spell-most-powerful spell-db)))

(defun reward-possible-p (state)
  (if-difficult-level>medium (state)
    (dice:pass-d100.0 +reward-possible-hi-level+)
    (dice:pass-d100.0 +reward-possible-low-level+)))

(defun available-heal-spells (entity)
  (character:castable-spells-list-by-tag (entity:ghost entity) spell:+spell-tag-heal+))

(defun available-attack-spells (entity)
  (character:castable-attack-spells-list (entity:ghost entity)))

(defun reachable-help-needed-friend-heal-spell-p (available-spells launcher-entity)
  (with-slots-for-reasoning (launcher-entity state ghost blackboard)
    (when-let ((friend-to-help (friend-who-needs-help blackboard launcher-entity))
               (spell          (find-best-heal-spell state available-spells launcher-entity)))
      (battle-utils:range-spell-valid-p launcher-entity friend-to-help spell))))

(defun target-reachable-attack-spell (entity)
  (with-accessors ((state state)) entity
    (with-accessors ((blackboard blackboard:blackboard)) state
      (when-let ((all-visibles (visible-opponents-sorted blackboard entity))
                 (chance       (if-difficult-level>medium (state)
                                 +attack-least-powerful-hi-level-chance+
                                 +attack-least-powerful-low-level-chance+)))
        (if (dice:pass-d100.0 chance)
            (least-powerful-visible-opponents blackboard entity)
            (random-elt all-visibles))))))

(defun attackable-opponents-attack-spell (available-spells launcher-entity)
   (with-slots-for-reasoning (launcher-entity state ghost blackboard)
    (when-let* ((target-entity (target-reachable-attack-spell launcher-entity))
                (spell         (find-best-attack-spell state
                                                       available-spells
                                                       launcher-entity
                                                       target-entity)))
      (if (battle-utils:range-spell-valid-p launcher-entity target-entity spell)
          (values target-entity spell)
          (values nil nil)))))

(defun at-least-n-teleport-chain-p (ghost spell n)
  (< (* n (spell:cost spell))
     (character:current-spell-points ghost)))

(defun attackable-position-exists-path (strategy-expert entity reachable-fn)
  "note:  path  can be  made  by  a single  tile,  the  one where  the
character is. In this case its cost is 0.0"
  (if (blackboard:entity-in-valid-attackable-pos-p entity)
      (values (calculate-cost-position entity) 0.0)
      (blackboard:best-path-to-reach-enemy-w-current-weapon strategy-expert entity
                                                            :reachable-fn-p
                                                            reachable-fn)))
(defun combined-power-compare-clsr (&optional (desc t))
  #'(lambda (a b)
      (let* ((ghost-a (entity:ghost a))
             (ghost-b (entity:ghost b))
             (power-a (character:combined-power ghost-a))
             (power-b (character:combined-power ghost-b)))
        (if desc
            (>= power-a power-b)
            (<  power-a power-b)))))

(defun sort-by-manhattam-dist-clsr (entity-pivot-pos &optional (comp-fn #'<))
  #'(lambda (a b)
      (let ((d1 (map-utils:map-manhattam-distance entity-pivot-pos a))
            (d2 (map-utils:map-manhattam-distance entity-pivot-pos b)))
        (funcall comp-fn d1 d2))))

(defun find-nearest-visible-wall (entity)
  (flet ((invisible-wall-p-fn (a)
           (absee-mesh:other-visible-p entity
                                       a
                                       :exclude-if-labyrinth-entity nil)))
  (with-player-cost-pos (entity x y)
    (with-slots-for-reasoning (entity state ghost blackboard)
      (let* ((difficult         (level-difficult state))
             (box-size          (* difficult 2))
             (all-walls         (map 'list
                                     #'(lambda (a)
                                         (find-entity-by-id state (entity-id (car a))))
                                     (neighborhood-by-type state
                                                           y ; row
                                                           x ; column
                                                           +wall-type+
                                                           :h-offset box-size
                                                           :w-offset box-size)))
             (all-visibles-wall (remove-if-not #'invisible-wall-p-fn all-walls)))
        ;; just plain dist, lame :(
        (when all-visibles-wall
          (let ((nearest (first-elt (shellsort all-visibles-wall
                                               (sort-by-manhattam-dist-clsr entity)))))
            (if nearest
                (with-player-cost-pos (nearest x y)
                  (values nearest (ivec2:ivec2 x y)))
                nil))))))))

(defstruct protect-place
  (pos          nil :type ivec2:ivec2)
  (points       0.0 :type desired-type)
  (attack-pos-p nil))

(defun linear-decay-manahattam-dist (size a b &key (dist-scaling 1.0))
  (d- (d size)
      (d* dist-scaling
          (d (map-utils:map-manhattam-distance a b)))))

(defun find-pos-in-place (places pos)
  (find-if #'(lambda (a)
               (ivec2:ivec2= (protect-place-pos a)
                             pos))
           places))

(defun places-near-weak-friend (strategy-expert entity
                                &key (weak-friend (friend-who-needs-help strategy-expert
                                                                         entity
                                                                         :exclude-me t)))
  (when weak-friend
    (with-accessors ((ghost ghost)) entity
      (with-accessors ((main-state main-state)) strategy-expert
        (with-accessors ((map-state map-state)) main-state
          (when-let* ((pos-weak    (entity:calculate-cost-position weak-friend))
                      (pos-entity  (entity:calculate-cost-position entity))
                      (x-weak      (elt pos-weak 0))
                      (y-weak      (elt pos-weak 1))
                      (size        (if-difficult-level>medium (main-state)
                                     (truncate (/ +weapon-bow-range+ 3))
                                     (truncate (/ +weapon-bow-range+ 4))))
                      (neighs      (matrix:gen-valid-neighbour-position-in-box map-state
                                                                               x-weak
                                                                               y-weak
                                                                               size size
                                                                               :add-center nil)))
            (setf neighs (seq->list (remove-if-not #'(lambda (p)
                                                       (empty@pos-p main-state
                                                                    (elt p 0)
                                                                    (elt p 1)))
                                                   neighs)))
            ;; add entity position
            (push pos-entity neighs)
            ;; points decrease with manhattam-distance
            (let* ((places   (map 'list
                                  #'(lambda (p)
                                      (let ((max (d* (d size) 0.2)))
                                        (make-protect-place :pos    p
                                                            :points
                                                            (linear-decay-manahattam-dist max
                                                                                          pos-weak
                                                                                          p))))
                                  neighs))
                   (reach-fn (blackboard:reachable-p-w/o-concening-tiles-fn strategy-expert))
                   (tactic   (character:weapon-case (entity)
                               :pole     (blackboard:attack-enemy-pole-positions strategy-expert)
                               :melee    (blackboard:attack-enemy-melee-positions strategy-expert)
                               :bow      nil
                               :crossbow nil)))
              ;; increase points (and set flag) if is a valid attack place
              (loop for tc in tactic do
                   (loop for p in (blackboard:goal-pos tc) do
                        (when-let ((increase (find-pos-in-place places p)))
                          (incf (protect-place-points increase) +inc-point-attackable+)
                          (setf (protect-place-attack-pos-p increase) t))))
              ;; the same as above for current-position
              (when (attackable-opponents-id strategy-expert entity)
                (let ((place-entity (find-pos-in-place places pos-entity)))
                  (incf (protect-place-points       place-entity) (d* +inc-point-attackable+ 2.0))
                  (setf (protect-place-attack-pos-p place-entity) t)))
              ;; remove non reachable places
              (remove-if-not #'(lambda (p)
                                 (funcall reach-fn
                                          pos-entity
                                          (protect-place-pos p)
                                          (character:current-movement-points ghost)))
                             places)
              ;; increase points of visible (by opponents)
              (loop
                 for p in places
                 when (absee-mesh:tile-placeholder-visible-by-faction-p main-state
                                                                        (protect-place-pos p)
                                                                        +pc-type+)
                 do
                   (incf (protect-place-points p) (/ +inc-point-attackable+ 4.0)))
              ;; increase points if near opponents
              (let ((ids-ai (blackboard::all-player-id-visible-from-ai main-state)))
                (loop for id in ids-ai do
                     (loop for p in places do
                          (incf (protect-place-points p)
                                (max 0.0
                                     (linear-decay-manahattam-dist (d* (d size) 1.5)
                                                                   pos-entity
                                                                   (protect-place-pos p)
                                                                   :dist-scaling 4.5))))))
              ;; decrease by cost to reach
              (loop for p in places do
                   (let ((cost (blackboard:cost-w/o-concening-tiles entity
                                                                    (protect-place-pos p))))
                     (when cost ;; the cost to reach the tile where the player is values null
                       (decf (protect-place-points p) (d* cost 0.2)))))
              ;; sort by points (descending order)
              (shellsort places #'(lambda (a b) (> (protect-place-points a)
                                                   (protect-place-points b)))))))))))

(define-constant +max-places-near-weak-friend-low+ 5)

(define-constant +max-places-near-weak-friend-hi+  3)

(defun good-places-to-protect (strategy-expert entity)
  (with-accessors ((main-state main-state)) strategy-expert
    (with-accessors ((map-state map-state)) main-state
      (when-let* ((weak-friend    (ai-utils:friend-who-needs-help strategy-expert
                                                                  entity
                                                                  :exclude-me t))
                  (pos-near       (places-near-weak-friend strategy-expert
                                                           entity
                                                           :weak-friend weak-friend))
                  (cutoff         (if-difficult-level>medium (main-state)
                                    +max-places-near-weak-friend-hi+
                                    +max-places-near-weak-friend-low+))
                  (good-places    (subseq pos-near 0 (min (length pos-near) cutoff))))
        good-places))))

(defun near-weak-friend-p (strategy-expert entity)
  (when-let* ((pos            (mesh:calculate-cost-position entity))
              (good-places    (good-places-to-protect strategy-expert entity))
              (good-positions (map 'list
                                   #'ai-utils:protect-place-pos
                                   good-places)))
    (find pos good-positions :test #'ivec2:ivec2=)))

(defun all-visibles-opponents (strategy-expert entity &key (alive-only t))
  "the visible opponents of AI, if such exist."
  (with-accessors ((main-state main-state)) strategy-expert
    (absee-mesh:visible-players-in-state-from-faction main-state
                                                      (my-faction entity)
                                                      :alive-only alive-only)))

(defun visible-opponents-sorted (strategy-expert entity &key (alive-only t))
  "the visible opponents of AI in attack range, if such exists,
sorted from the most powerful to the least one.
see: character:combined-power"
  (with-accessors ((main-state main-state)) strategy-expert
    (when-let* ((all     (all-visibles-opponents strategy-expert
                                                 entity
                                                 :alive-only alive-only))
                (sorted  (shellsort all (combined-power-compare-clsr t))))
      sorted)))

(defun most-powerful-visible-opponents (strategy-expert entity &key (alive-only t))
  "the most powerful visible opponents of AI in attack range, if such exists.
see: character:combined-power"
  (when-let ((all (visible-opponents-sorted strategy-expert entity :alive-only alive-only)))
    (first-elt all)))

(defun least-powerful-visible-opponents (strategy-expert entity &key (alive-only t))
  "the least powerful visible opponents of AI in attack range, if such exists.
see: character:combined-power"
  (when-let ((all (visible-opponents-sorted strategy-expert entity :alive-only alive-only)))
    (last-elt all)))

(defun attackable-opponents-id (strategy-expert entity &key (alive-only t))
  "the first visible opponents of AI in attack range, if such exists.
TODO: refactorting, use all-visibles-opponents function above"
  (with-accessors ((main-state main-state)) strategy-expert
    (when-let ((weapon-type    (character:weapon-type (entity:ghost entity)))
               (visibles-pcs   (absee-mesh:visible-players-in-state-from-faction
                                main-state
                                (my-faction entity)
                                :alive-only alive-only))
               (pos            (mesh:calculate-cost-position entity)))
      (loop for defender in visibles-pcs do
           (when (battle-utils:range-weapon-valid-p pos defender weapon-type)
             (return-from attackable-opponents-id (id defender))))))
  nil)

(defun cost-to-reach-w/o-concerning-place (strategy-expert entity place-near)
  (if (ivec2:ivec2= (calculate-cost-position entity)
                    (protect-place-pos place-near))
      0.0
      (let ((res (multiple-value-list ;(path total-cost costs)
                  (blackboard:path-near-goal-w/o-concerning-tiles strategy-expert
                                                                  entity
                                                                  (protect-place-pos place-near)
                                                                  :cut-off-first-tile
                                                                  nil))))
        (second res))))

(defun attack-when-near-pos-long-range-p (strategy-expert entity &key (alive-only t))
  "note: places-near contains only reachable tiles so
path-near-goal-w/o-concerning-tiles always returns a non nil value"
  (with-accessors ((main-state main-state)) strategy-expert
    (with-accessors ((map-state map-state)) main-state
      (when-let* ((weak-friend    (ai-utils:friend-who-needs-help strategy-expert
                                                                  entity
                                                                  :exclude-me t))
                  (pos            (mesh:calculate-cost-position entity))
                  (weapon-type    (character:weapon-type-long-range (entity:ghost entity)))
                  (visibles-pcs   (absee-mesh:visible-players-in-state-from-faction
                                   main-state
                                   (my-faction entity)
                                   :alive-only alive-only))
                  (places-near    (places-near-weak-friend strategy-expert
                                                           entity
                                                           :weak-friend weak-friend))
                  (place-near     (first-elt places-near))
                  (pos-near       (protect-place-pos place-near))
                  (cost-to-reach  (cost-to-reach-w/o-concerning-place strategy-expert
                                                                      entity
                                                                      place-near))
                  (attack-cost    (battle-utils:cost-attack-w-current-weapon entity)))
        (loop for defender in visibles-pcs do
             (when (and (battle-utils:range-weapon-valid-p pos-near defender weapon-type)
                        (<= (+ cost-to-reach attack-cost)
                            (character:current-movement-points (entity:ghost entity))))
               (return-from attack-when-near-pos-long-range-p t)))
        nil))))

(defun attack-when-near-pos-short-range-p (strategy-expert entity)
  "note: places-near contains only reachable tiles so
path-near-goal-w/o-concerning-tiles always returns a non nil value,
also check for pole weapon"
  (with-accessors ((main-state main-state)) strategy-expert
    (with-accessors ((map-state map-state)) main-state
      (when-let* ((weak-friend    (ai-utils:friend-who-needs-help strategy-expert
                                                                  entity
                                                                  :exclude-me t))
                  (pos            (mesh:calculate-cost-position entity))
                  (places-near    (places-near-weak-friend strategy-expert
                                                           entity
                                                           :weak-friend weak-friend))
                  (place-near     (first-elt places-near))
                  (cost-to-reach  (cost-to-reach-w/o-concerning-place strategy-expert
                                                                      entity
                                                                      place-near))
                  (attack-cost    (battle-utils:cost-attack-w-current-weapon entity)))
        (and (protect-place-attack-pos-p place-near)
             (<= (+ cost-to-reach attack-cost)
                 (character:current-movement-points (entity:ghost entity))))))))

(defun attack-when-near-pos-p (strategy-expert entity)
  "note: places-near contains only reachable tiles so
path-near-goal-w/o-concerning-tiles always returns a non nil value"
  (character:weapon-case (entity)
    :pole     (attack-when-near-pos-short-range-p strategy-expert entity)
    :melee    (attack-when-near-pos-short-range-p strategy-expert entity)
    :bow      (attack-when-near-pos-long-range-p  strategy-expert entity)
    :crossbow (attack-when-near-pos-long-range-p  strategy-expert entity)))

(defun find-wall-breaking-spells (entity)
  (character:castable-spells-list-by-tag (entity:ghost entity)
                                         spell:+spell-tag-remove-wall+))

(defstruct fountain-place
  (entity)
  (pos)
  (path)
  (cost))

(defun map-state-element->entity (game-state map-element)
  (find-entity-by-id game-state (entity-id map-element)))

(defun %get-path-go-near (blackboard from to)
  (if (= (manhattam-distance from to) 1) ; just next
      (values nil 0.0)
      (multiple-value-bind (path total-cost costs)
          (blackboard:path-with-concerning-tiles blackboard from to)
        (declare (ignore total-cost))
        (values (all-but-last-elt path)
                (reduce #'(lambda (a b) (d+ (d a) (d b)))
                        (all-but-last-elt costs))))))

(defun useful-reachable-fountain-nocache (entity &key (include-first-path-tile nil))
  "Return values: entity nearest fountain, path to reach it and total cost."
  (with-slots-for-reasoning (entity state ghost blackboard)
    (let* ((difficult         (level-difficult state))
           (box-size          (* difficult 3))
           (player-position   (mesh:calculate-cost-position entity))
           (player-x          (elt player-position 0))
           (player-y          (elt player-position 1))
           (all-fountains     (remove-if #'(lambda (a) ; remove exhausted
                                             (let ((entity (map-state-element->entity state
                                                                                      (car a))))
                                               (blackboard:fountain-exhausted-p blackboard
                                                                                entity)))
                                         (neighborhood-by-type state
                                                               player-y
                                                               player-x
                                                               +magic-furniture-type+
                                                               :h-offset box-size
                                                               :w-offset box-size)))
           (all-fountain-places (map 'list ; note: not reachable will became a null element
                                     #'(lambda (a)
                                         (let* ((entity     (map-state-element->entity state
                                                                                       (car a)))
                                                (entity-pos (cdr a)))
                                           (multiple-value-bind (path cost)
                                               (%get-path-go-near blackboard
                                                                  player-position
                                                                  entity-pos)
                                             (when (<= (+ cost +activate-switch-cost+)
                                                       (character:current-movement-points ghost))
                                               (make-fountain-place :entity entity
                                                                    :pos    entity-pos
                                                                    :path   path
                                                                    :cost   cost)))))
                                     all-fountains)))
      ;; remove not reachable pos
      (setf all-fountain-places (remove-if-null all-fountain-places))
      ;; sort
      (setf all-fountain-places
            (shellsort all-fountain-places
                       (let ((actual-sort (sort-by-manhattam-dist-clsr player-position)))
                         #'(lambda (a b)
                             (let ((p1 (fountain-place-pos a))
                                   (p2 (fountain-place-pos b)))
                               (funcall actual-sort p1 p2))))))

      (if all-fountain-places
          (let* ((nearest      (first-elt all-fountain-places))
                 (nearest-path (fountain-place-path nearest))
                 (path         (if include-first-path-tile
                                   (let ((p (make-fresh-array (1+ (length nearest-path)))))
                                     (setf (elt p 0) player-position)
                                     (loop for i from 1 below (length p) do
                                          (setf (elt p i) (elt nearest-path (1- i))))
                                     p)
                                   nearest-path)))
            (values (fountain-place-entity nearest)
                    path
                    (fountain-place-cost   nearest)))
          (values nil #() nil)))))

(defcached useful-reachable-fountain ((entity &key (include-first-path-tile t))
                                                :test eq)
  (declare (optimize (speed 0) (safety 0) (debug 0)))
  (if (gethash entity cache)
      (let ((res (gethash entity cache)))
        (values (elt res 0)
                (elt res 1)
                (elt res 2)))
      (let ((res (multiple-value-list
                  (useful-reachable-fountain-nocache entity
                                                     :include-first-path-tile
                                                     include-first-path-tile))))
        (setf (gethash entity cache) res)
        (useful-reachable-fountain entity :include-first-path-tile include-first-path-tile))))

;;; actions

(defun go-launch-wall-breaking-spell (entity)
  (with-slots-for-reasoning (entity state ghost blackboard)
    (with-world (world state)
      (when-let ((available-spells (find-wall-breaking-spells entity))
                 (nearest-wall     (find-nearest-visible-wall entity)))
        (let* ((spells (shellsort available-spells (spell:sort-spells-by-level-fn nil)))
               (spell  (first-elt spells)))
          (when spell
            (setf (character:spell-loaded ghost) spell)
            (battle-utils:attack-launch-spell world
                                              entity
                                              nearest-wall
                                              :assume-visible nil)))))))

(defun go-launch-attack-spell (entity)
  (with-slots-for-reasoning (entity state ghost blackboard)
    (with-world (world state)
      (multiple-value-bind (target-entity spell)
          (attackable-opponents-attack-spell (ai-utils:available-attack-spells entity)
                                             entity)
        (when target-entity
          (setf (character:spell-loaded ghost) spell)
          (battle-utils:attack-launch-spell world
                                            entity
                                            target-entity
                                            :assume-visible nil))))))

(defstruct hiding-place
  (pos)
  (cost)
  (average-cost-opponents))

(defcached go-find-hiding-place ((entity opponents-can-see-entity) :test eq)
  (declare (optimize (speed 0) (safety 0) (debug 3)))
  (flet ((get-path-cost (blackboard from to)
           (multiple-value-bind (path total-cost costs)
               (blackboard:path-with-concerning-tiles blackboard from to)
             (declare (ignore path costs))
             total-cost))
         (put-in-cache (a)
           (setf (gethash entity cache) a)
           (go-find-hiding-place entity opponents-can-see-entity)))
    (or (gethash entity cache)
        (with-slots-for-reasoning (entity state ghost blackboard)
          (let* ((difficult         (level-difficult state))
                 (box-size          (* difficult 5))
                 (internal-box-size (* difficult 2))
                 (player-position   (mesh:calculate-cost-position entity))
                 (player-x          (elt player-position 0))
                 (player-y          (elt player-position 1))
                 (all-hiding-pos
                  (absee-mesh:tiles-placeholder-visibility-in-ring-by-faction state
                                                                              player-x
                                                                              player-y
                                                                              box-size
                                                                              internal-box-size
                                                                              +pc-type+
                                                                              opponents-can-see-entity))
                 (all-hiding-places (loop for hiding-pos in all-hiding-pos collect
                                         (let ((cost (get-path-cost  blackboard
                                                                     player-position
                                                                     hiding-pos)))
                                           (make-hiding-place :pos  hiding-pos
                                                              :cost cost)))))
            ;; remove not reachable pos
            (setf all-hiding-places
                  (remove-if #'(lambda (h)
                                 (let ((cost (get-path-cost blackboard
                                                            player-position
                                                            (hiding-place-pos h))))
                                   (> cost (character:current-movement-points ghost))))
                             all-hiding-places))
            (when all-hiding-places
              ;; calculate average
              (loop for hiding-place in all-hiding-places do
                   (let ((sum 0)
                         (ct  0))
                     (loop-player-entities state
                          #'(lambda (player-ent)
                              (let ((from     (calculate-cost-position player-ent))
                                    (hiding-place (hiding-place-pos hiding-place)))
                                (incf sum
                                      (get-path-cost blackboard
                                                     from
                                                     hiding-place))
                                (incf ct 1))))
                     (setf (hiding-place-average-cost-opponents hiding-place)
                           (d (/ sum ct)))))
              ;; sort
              (setf all-hiding-places (shellsort all-hiding-places
                                                 #'(lambda (a b)
                                                     (d> (hiding-place-average-cost-opponents a)
                                                         (hiding-place-average-cost-opponents b)))))
              ;(dbg "all hiding places~%~a" all-hiding-places)
              (if-difficult-level>medium (state)
                (let* ((difficult-scaling (* difficult 2))
                       (max-lenght (max 1 (lcg-next-upto (ceiling (/ (length all-hiding-places)
                                                                     difficult-scaling))))))
                  (put-in-cache (hiding-place-pos (random-elt (subseq all-hiding-places
                                                                      0
                                                                      max-lenght)))))
                (put-in-cache (hiding-place-pos (random-elt all-hiding-places))))))))))

(defun go-launch-teleport-spell (entity)
  (with-slots-for-reasoning (entity state ghost blackboard)
    (with-world (world state)
      (when-let* ((available-spells
                   (character:castable-spells-list-by-tag ghost
                                                          spell:+spell-tag-teleport+))
                  (spells         (shellsort available-spells
                                             (spell:sort-spells-by-level-fn t)))
                  (min-cost-spell (last-elt  spells))
                  (max-cost-spell (first-elt spells))
                  (cost-pos       (mesh:calculate-cost-position entity))
                  (spell          (if-difficult-level>medium (state)
                                    (if (and (at-least-n-teleport-chain-p ghost
                                                                          min-cost-spell
                                                                          +min-chain-teleport+)
                                             (mesh:inside-room-p entity))
                                        min-cost-spell
                                        max-cost-spell)
                                    max-cost-spell)))
        (setf (character:spell-loaded ghost) spell)
        (battle-utils:launch-spell world entity entity)))))

(defun %go-launch-heal-spell (entity exclude-me)
  (with-slots-for-reasoning (entity state ghost blackboard)
    (with-world (world state)
      (when-let ((available-spells (available-heal-spells entity))
                 (friend-to-help   (friend-who-needs-help blackboard
                                                          entity
                                                          :exclude-me exclude-me)))
        (let* ((spell (find-best-heal-spell state available-spells entity)))
          (when spell
            (setf (character:spell-loaded ghost) spell)
            (battle-utils:launch-spell world entity friend-to-help)))))))

(defun %go-launch-spell (spell launcher target)
  (with-slots-for-reasoning (launcher state ghost blackboard)
    (with-world (world state)
      (setf (character:spell-loaded ghost) spell)
      (battle-utils:launch-spell world launcher target))))

(defun go-launch-heal-spell-friend (entity)
  (%go-launch-heal-spell entity t))

(defun go-launch-heal-spell (entity)
  (%go-launch-heal-spell entity nil))

(defun go-reward-heal-spell (entity)
  (when-let* ((spells (spell:spells-list-by-tag spell:+spell-tag-heal-reward+))
              (spell  (first-elt spells)))
    (incf (character:current-spell-points (ghost entity))
          (spell:cost spell))
    (%go-launch-spell spell entity entity)))

(defun next-flee-position (strategy-expert entity)
  "return the flee position next to entity and its cost, if any"
  (let ((dijkstra-map (blackboard::build-flee-layer-player strategy-expert entity)))
    (inmap:next-dijkstra-position dijkstra-map entity 1.0)))

(defun go-place-trap (entity)
  (with-accessors ((ghost entity:ghost)) entity
    (let ((trap (character:find-item-in-inventory-if ghost #'interactive-entity:trapp)))
      (md2-mesh:place-trap entity trap))))
