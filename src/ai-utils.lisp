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

(define-constant +planner-file-extension+         ".lisp"                :test #'string=)

(define-constant +plan-stopper+                   :end                   :test #'eq)

(define-constant +idle-action+                    :idle                  :test #'eq)

(define-constant +interrupt-action+               :interrupt             :test #'eq)

(define-constant +move-action+                    :move                  :test #'eq)

(define-constant +faint-action+                   :faint                 :test #'eq)

(define-constant +go-to-attack-pos-action+        :go-to-attack-pos      :test #'eq)

(define-constant +interrupted-action+             :interrupt             :test #'eq)

(define-constant +launch-heal-spell-action+       :launch-heal-spell     :test #'eq)

(define-constant +launch-teleport-spell-action+   :launch-teleport-spell :test #'eq)

(define-constant +launch-wall-break-spell-action+ :launch-spell-wall     :test #'eq)

(define-constant +hide-action+                    :hide                  :test #'eq)

(define-constant +flee-action+                    :flee                  :test #'eq)

(define-constant +find-hiding-place-action+       :find-hiding-place     :test #'eq)

(define-constant +place-trap-action+              :place-trap            :test #'eq)

(define-constant +attack-action+                  :attack                :test #'eq)

(define-constant +go-to-attack-pos-action+        :go-to-attack-pos      :test #'eq)

(define-constant +find-attack-pos-action+         :find-attack-position  :test #'eq)

(define-constant +load-weapon-action+             :load-weapon           :test #'eq)

(define-constant +go-near-to-attack-pos-action+   :go-near-to-attack-pos :test #'eq)

(define-constant +min-chain-teleport+             3                      :test #'=)

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

(defun too-low-health-p (entity)
  (let ((ghost (entity:ghost entity)))
    (< (character:current-damage-points ghost)
       (* 0.5 (character:actual-damage-points ghost)))))

(defun friend-who-needs-help (strategy-expert)
  (map-ai-entities (interfaces:main-state strategy-expert)
                   #'(lambda (v)
                       (when (too-low-health-p v)
                         (return-from friend-who-needs-help v))))
  nil)

(defun find-best-heal-spell-most-powerful (spell-db)
  (first-elt (shellsort spell-db (spell:sort-spells-by-level t))))

(defun find-best-heal-spell-clever (spell-db entity)
  (let* ((sorted-spell-db (shellsort spell-db (spell:sort-spells-by-level nil)))
         (spell (find-if #'(lambda (s)
                             (let ((heal-fx (plist-path-value
                                             (basic-interaction-params s)
                                             '(:healing-effects :heal-damage-points))))
                               (>= (points heal-fx)
                                   (character:actual-damage-points (ghost entity)))))
                         sorted-spell-db)))
    (or spell
        (last-elt sorted-spell-db))))

(defmacro if-difficult-level>medium ((game-state) if-more if-less)
  `(if (> (level-difficult ,game-state)
          +difficult-medium+)
       ,if-more
       ,if-less))

(defun find-best-heal-spell (state spell-db entity)
  (if-difficult-level>medium (state)
    (find-best-heal-spell-clever         spell-db entity)
    (find-best-heal-spell-most-powerful  spell-db)))

(defun available-heal-spells (entity)
  (character:castable-spells-list-by-tag (entity:ghost entity) spell:+spell-tag-heal+))

(defun reachable-help-needed-friend-spell-p (available-spells launcher-entity)
  (with-slots-for-reasoning (launcher-entity state ghost blackboard)
    (when-let ((friend-to-help   (friend-who-needs-help blackboard))
               (spell (find-best-heal-spell state available-spells launcher-entity)))
      (battle-utils:range-spell-valid-p launcher-entity friend-to-help spell))))

(defun at-least-n-teleport-chain-p (ghost spell n)
  (< (* n (spell:cost spell))
     (character:current-magic-points ghost)))

(defun attackable-position-exists-path (strategy-expert entity reachable-fn)
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

(defun sort-by-manhattam-dist-clsr (entity-pivot &optional (comp-fn #'<))
  #'(lambda (a b)
      (let ((d1 (map-utils:map-manhattam-distance entity-pivot a))
            (d2 (map-utils:map-manhattam-distance entity-pivot b)))
        (funcall comp-fn d1 d2))))

(defun find-nearest-wall (entity)
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

(defun find-wall-breaking-spells (entity)
  (character:castable-spells-list-by-tag (entity:ghost entity)
                                         spell:+spell-tag-remove-wall+))

(defun go-launch-wall-breaking-spell (entity)
  (with-slots-for-reasoning (entity state ghost blackboard)
    (with-world (world state)
      (when-let ((available-spells (find-wall-breaking-spells entity))
                 (nearest-wall     (find-nearest-wall     entity)))
        (let* ((spells (shellsort available-spells (spell:sort-spells-by-level nil)))
               (spell  (first-elt spells)))
          (when spell
            (setf (character:spell-loaded ghost) spell)
            (battle-utils:attack-launch-spell world
                                              entity
                                              nearest-wall
                                              :assume-visible t)))))))

(defstruct hiding-place
  (pos)
  (cost)
  (average-cost-opponents))

(defcached go-find-hiding-place ((entity) :test eq)
  (declare (optimize (speed 0) (safety 0) (debug 3)))
  (flet ((get-path-cost (blackboard from to)
           (multiple-value-bind (path total-cost costs)
               (blackboard:path-with-concerning-tiles blackboard from to)
             (declare (ignore path costs))
             total-cost))
         (put-in-cache (a)
           (setf (gethash entity cache) a)
           (go-find-hiding-place entity)))
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
                                                                              +pc-type+))
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
                     (map-ai-entities state
                                      #'(lambda (player-ent)
                                          (let ((from (mesh:calculate-cost-position player-ent)))
                                            (incf sum
                                                  (get-path-cost blackboard
                                                                 from
                                                                 (hiding-place-pos hiding-place)))
                                            (incf ct 1))))
                     (setf (hiding-place-average-cost-opponents hiding-place)
                           (d (/ sum ct)))))
              ;; sort
              (setf all-hiding-places (shellsort all-hiding-places
                                                 #'(lambda (a b)
                                                     (d> (hiding-place-average-cost-opponents a)
                                                         (hiding-place-average-cost-opponents b)))))
              ;(misc:dbg "~{~a~%~}" all-hiding-places)
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
                                             (spell:sort-spells-by-level t)))
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

(defun go-launch-heal-spell (entity)
  (with-slots-for-reasoning (entity state ghost blackboard)
    (with-world (world state)
      (when-let ((available-spells (available-heal-spells entity))
                 (friend-to-help   (friend-who-needs-help blackboard)))
        (let* ((spell (find-best-heal-spell state available-spells entity)))
          (when spell
            (setf (character:spell-loaded ghost) spell)
            (battle-utils:launch-spell world entity friend-to-help)))))))

(defun go-next-flee-position (strategy-expert entity)
  "return the flee position next to entity and its cost"
  (with-accessors ((state entity:state)
                   (ghost entity:ghost)) entity
    (flet ((find-cost (a)
             (game-state:get-cost state (elt a 0) (elt a 1)))
           (find-cost-matrix (matrix a)
             (matrix:matrix-elt matrix (elt a 1) (elt a 0))))
      (when-let* ((concerning-matrix  (blackboard:concerning-tiles->costs-matrix strategy-expert))
                  (neigh              (gen-neigh       entity))
                  (neigh-cost         (gen-neigh-costs entity #'find-cost))
                  (neigh-conc-cost    (gen-neigh-costs entity
                                                       #'(lambda (a)
                                                           (find-cost-matrix concerning-matrix
                                                                             a))))
                  (min-concerning-pos (position (num:find-min neigh-conc-cost)
                                                neigh-conc-cost
                                                :test #'num:epsilon=)))
        (values (elt neigh      min-concerning-pos)
                (elt neigh-cost min-concerning-pos))))))

(defun go-place-trap (entity)
  (with-accessors ((ghost entity:ghost)) entity
    (let ((trap (character:find-item-in-inventory-if ghost #'interactive-entity:trapp)))
      (md2-mesh:place-trap entity trap))))
