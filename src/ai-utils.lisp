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

(define-constant +planner-file-extension+       ".lisp"                :test #'string=)

(define-constant +idle-action+                  :idle                  :test #'eq)

(define-constant +interrupt-action+             :interrupt             :test #'eq)

(define-constant +move-action+                  :move                  :test #'eq)

(define-constant +faint-action+                 :faint                 :test #'eq)

(define-constant +go-to-attack-pos-action+      :go-to-attack-pos      :test #'eq)

(define-constant +interrupted-action+           :interrupt             :test #'eq)

(define-constant +launch-heal-spell-action+     :launch-heal-spell     :test #'eq)

(define-constant +launch-teleport-spell-action+ :launch-teleport-spell :test #'eq)

(define-constant +min-chain-teleport+           3                      :test #'=)

(defun too-low-health-p (entity)
  (let ((ghost (entity:ghost entity)))
    (< (character:current-damage-points ghost)
       (* 0.5 (character:actual-damage-points ghost)))))

(defun friend-who-needs-help (strategy-expert)
  (game-state:map-ai-entities (interfaces:main-state strategy-expert)
                              #'(lambda (k v)
                                  (declare (ignore k))
                                  (when (too-low-health-p v)
                                    (return-from friend-who-needs-help v))))
  nil)

(defun find-best-heal-spell (spell-db entity)
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
  `(if (> (game-state:level-difficult ,game-state)
          +difficult-medium+)
       ,if-more
       ,if-less))

(defun go-launch-heal-spell (entity)
  (with-slots-for-reasoning (entity state ghost blackboard)
    (game-state:with-world (world state)
      (when-let ((available-spells
                  (character:castable-spells-list-by-tag ghost
                                                         spell:+spell-tag-heal+))
                 (friend-to-help   (friend-who-needs-help blackboard)))
        (let* ((spell (if-difficult-level>medium (state)
                        (find-best-heal-spell available-spells entity)
                        (first-elt (shellsort available-spells
                                              (spell:sort-spells-by-level t))))))
          (when spell
            (setf (character:spell-loaded ghost) spell)
            (battle-utils:launch-spell world entity friend-to-help)))))))

(defun at-least-n-teleport-chain-p (ghost spell n)
  (< (* n (spell:cost spell))
     (character:current-magic-points ghost)))

(defun go-launch-teleport-spell (entity)
  (with-slots-for-reasoning (entity state ghost blackboard)
    (game-state:with-world (world state)
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

(defun combined-power-compare-clsr (&optional (desc t))
  #'(lambda (a b)
      (let* ((ghost-a (entity:ghost a))
             (ghost-b (entity:ghost b))
             (power-a (character:combined-power ghost-a))
             (power-b (character:combined-power ghost-b)))
        (if desc
            (>= power-a power-b)
            (<  power-a power-b)))))

(defstruct hiding-place
  (pos)
  (cost)
  (average-cost-opponents))

(defun find-hiding-place-box (entity)
  (flet ((get-path-cost (blackboard from to)
           (multiple-value-bind (path total-cost costs)
               (blackboard:path-with-concerning-tiles blackboard from to)
             (declare (ignore path costs))
             total-cost)))
    (with-slots-for-reasoning (entity state ghost blackboard)
      (let* ((difficult         (game-state:level-difficult state))
             (box-size          (* difficult 5))
             (player-position   (mesh:calculate-cost-position entity))
             (player-x          (elt player-position 0))
             (player-y          (elt player-position 1))
             (all-hiding-pos
              (absee-mesh:tiles-placeholder-visibility-in-box-by-faction state
                                                                         player-x
                                                                         player-y
                                                                         box-size
                                                                         game-state:+pc-type+))
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
                 (game-state:map-ai-entities state
                                             #'(lambda (k player-ent)
                                                 (declare (ignore k))
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
          (misc:dbg "~{~a~%~}" all-hiding-places)
          (if-difficult-level>medium (state)
            (let* ((difficult-scaling (* difficult 2))
                   (max-lenght (max 1 (lcg-next-upto (ceiling (/ (length all-hiding-pos)
                                                                 difficult-scaling))))))
              (hiding-place-pos (random-elt (subseq all-hiding-places 0 max-lenght))))
            (hiding-place-pos (random-elt all-hiding-places))))))))
