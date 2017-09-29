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

(define-constant +pole-key-tactics+     :pole     :test #'eq)

(define-constant +melee-key-tactics+    :melee    :test #'eq)

(define-constant +bow-key-tactics+      :bow      :test #'eq)

(define-constant +crossbow-key-tactics+ :crossbow :test #'eq)

;; atk := (pos . mp)
(defun atk-mp (atk)
  (cdr atk))

(defun atk-pos (atk)
  (car atk))

(defun atk-mp-o (atk out)
  (cdro atk out))

(defun atk-pos-o (atk out)
  (caro atk out))

(defun def-pos (def)
  (car def))

(defun def-goal-pos (def)
  (second def))

(defun def-pos-o (def out)
  (caro def out))

(defun def-goal-pos-o (def out)
  (secondo def out))

(defun def-mp-o (def out)
  (thirdo def out))

(defun def-mp (def)
  (third def))

(defun attack-equals-p (a b)
  (and (ivec2= (def-goal-pos a) (def-goal-pos b))
       (ivec2= (def-pos a)      (def-pos b))
       (=      (def-mp a)       (def-mp  b))))

;; atk := (pos . mp)
;; def := (def-slot-pos atk-pos mp)

(defun make-def-o (def-pos atk-pos atk-mp out)
  (project (def-pos atk-pos atk-mp)
    (== out `(,def-pos ,atk-pos ,atk-mp))))

(defun tactic-attacker-pos (tactic)
  (elt tactic 1))

(defun tactic-defender-slot-pos (tactic)
  (elt tactic 0))

(defun tactic-valid-p (tactic)
  (= (length tactic) 3))

(defun suppress-closed-door-cost-mat (blackboard)
  (with-accessors ((main-state main-state)
                   (concerning-tiles concerning-tiles)) blackboard
    (with-accessors ((costs-from-map costs-from-map)) main-state
      (let ((res (make-matrix (width  concerning-tiles)
                              (height concerning-tiles)
                              0.0)))
        (nmap-matrix-xy res
                        #'(lambda (x y cost)
                            (declare (ignore cost))
                            ;; check for a closed door
                            (when (and (door@pos-p main-state x y)
                                       (epsilon= (matrix-elt costs-from-map y x)
                                                 +invalicable-element-cost+))
                              (setf (matrix-elt res y x)
                                    (d- +open-terrain-cost+ +invalicable-element-cost+)))))
        res))))

;; Best...well...
(defun build-best-path-to-attack (blackboard ai-player weapon-tactic
                                  &key
                                    (cut-off-first-tile t)
                                    (reachable-fn-p (reachable-p-w/concening-tiles-fn blackboard)))
  "returns four values:
- the path (taking into account concerning tiles);
- the cost of each move (taking  into account terrain and other objects;
  like building or  players)
- the total cost of  the path (again only with map cost (i.e. no concerning tiles);
- the id of the target entity."
  (when-let* ((all-tactics        (build-all-attack-tactics blackboard
                                                            reachable-fn-p))
              (all-weapon-tactics (getf all-tactics weapon-tactic))
              (my-tactic   (find-if #'(lambda (a) (= (id ai-player) (entity-id a)))
                                    all-weapon-tactics)))
    ;;(dbg "all ~a melee ~a my ~a" all-tactics all-weapon-tactics  my-tactic)
    (let ((start-pos          (mesh:calculate-cost-position ai-player))
          (end-pos            (target-pos my-tactic)))
      ;;(dbg "from ~a -> ~a" start-pos end-pos)
      (multiple-value-bind (result-path cumulative-cost costs-terrain)
          (path-with-concerning-tiles blackboard
                                      start-pos
                                      end-pos
                                      :cut-off-first-tile cut-off-first-tile)
        (values result-path cumulative-cost costs-terrain (target-id my-tactic))))))

(defun best-path-near-attack-goal-w-current-weapon (blackboard ai-player
                                                    &key
                                                      (cut-off-first-tile t)
                                                      (reachable-fn-p #'reachablep))
  (multiple-value-bind (path cumulative-cost costs target-id)
      (best-path-to-reach-enemy-w-current-weapon blackboard ai-player
                                                 :cut-off-first-tile cut-off-first-tile
                                                 :reachable-fn-p     reachable-fn-p)
    (let ((max (do ((accum 0.0)
                    (idx 0 (1+ idx)))
                   ((or (>= idx (length costs))
                        (>= accum
                           (character:current-movement-points (ghost ai-player))))
                    idx)
                 (incf accum (elt costs idx)))))
      (values (subseq path 0 max)
              cumulative-cost
              costs
              target-id))))

(defun best-path-to-reach-enemy-w-current-weapon (blackboard ai-player
                                                  &key
                                                    (cut-off-first-tile t)
                                                    (reachable-fn-p
                                                     (reachable-p-w/concening-tiles-fn blackboard)))
  (with-accessors ((ghost ghost)) ai-player
    (let ((weapon-type (character:weapon-type ghost)))
      (when weapon-type
        (cond
          ((character:weapon-type-pole-p ghost)
           (build-best-path-to-attack blackboard ai-player +pole-key-tactics+
                                      :cut-off-first-tile cut-off-first-tile
                                      :reachable-fn-p reachable-fn-p))
          ((character:weapon-type-bow-p ghost)
           (build-best-path-to-attack blackboard ai-player +bow-key-tactics+
                                      :cut-off-first-tile cut-off-first-tile
                                      :reachable-fn-p reachable-fn-p))
          ((character:weapon-type-crossbow-p ghost)
           (build-best-path-to-attack blackboard ai-player +crossbow-key-tactics+
                                      :cut-off-first-tile cut-off-first-tile
                                      :reachable-fn-p reachable-fn-p))
          (t ;; any other melee weapon
           (build-best-path-to-attack blackboard ai-player +melee-key-tactics+
                                      :cut-off-first-tile cut-off-first-tile
                                      :reachable-fn-p reachable-fn-p)))))))

(defun best-path-w-current-weapon-reachable-p (blackboard ai-player)
  (multiple-value-bind (path cumulative-cost costs)
      (best-path-to-reach-enemy-w-current-weapon blackboard ai-player)
    (declare (ignore costs))
    (values (and path
                 (<= cumulative-cost (character:current-movement-points (ghost ai-player))))
            cumulative-cost)))

(defun path-with-concerning-tiles (blackboard ai-position
                                   defender-position
                                   &key (cut-off-first-tile t))
  "return three values: the path (taking into account concerning tiles),
the  total cost of (taking  into account terrain and other objects,
like building  or players *but NOT  the doors*) and the cost of each move of
the path, again only with map cost (i.e. no concerning tiles)

if cut-off-first-tile is  not nil the first element  of the calculated
path is removed
"
  (with-accessors ((main-state main-state)
                   (concerning-tiles concerning-tiles)) blackboard
    (with-accessors ((main-state main-state)) blackboard
      (with-accessors ((movement-costs movement-costs)) main-state
        (let* ((suppress-door-math (suppress-closed-door-cost-mat blackboard))
               (concerning-tiles   (concerning-tiles->costs-matrix blackboard))
               (others-costs       (list concerning-tiles suppress-door-math))
               (path-w/concering   (game-state:build-movement-path main-state
                                                                   ai-position
                                                                   defender-position
                                                                   :other-costs-layer
                                                                   others-costs)))
          (if (and path-w/concering
                   (> (length path-w/concering) 1))
              (let* ((costs-terrain   (loop for i across (subseq path-w/concering 1) collect
                                           (dmax +open-terrain-cost+
                                                 (d+ (d (matrix-elt movement-costs
                                                                    (elt i 1)
                                                                    (elt i 0)))
                                                     (matrix-elt suppress-door-math
                                                                 (elt i 1)
                                                                 (elt i 0))))))
                     (cumulative-cost (reduce #'(lambda (a b) (d+ (d a) (d b)))
                                              costs-terrain))
                     (result-path     (if cut-off-first-tile
                                          (subseq path-w/concering 1)
                                          path-w/concering)))
                ;; (dbg "path ~a ~a ~a"
                ;;           (subseq path-w/concering 1)
                ;;           cumulative-cost costs-terrain)
                (values result-path cumulative-cost costs-terrain))
              (values nil nil nil)))))))

(defstruct reachable-cache-value
  (ai-pos)
  (def-pos)
  (mp)
  (res))

(defun %reach-cache-value-eq (a b)
  (and (ivec2=   (reachable-cache-value-ai-pos  a)
                 (reachable-cache-value-ai-pos  b))
       (ivec2=   (reachable-cache-value-def-pos a)
                 (reachable-cache-value-def-pos b))
       (epsilon= (reachable-cache-value-mp a)
                 (reachable-cache-value-mp b))))

(defmacro with-reachable-cache-fns ((put-cache-fn-name get-cache-fn-name
                                                       defcached-put-fn defcached-get-fn)
                                    &body body)
  `(flet ((,put-cache-fn-name (ai-position defender-position ai-movement-points res)
            (,defcached-put-fn (make-reachable-cache-value :ai-pos  ai-position
                                                           :def-pos defender-position
                                                           :mp      ai-movement-points
                                                           :res     res)))
          (,get-cache-fn-name (ai-position defender-position ai-movement-points)
            (,defcached-get-fn (make-reachable-cache-value :ai-pos  ai-position
                                                           :def-pos defender-position
                                                           :mp      ai-movement-points))))
     ,@body))

(defcached-list reachable-p-w/concening-tiles-fn ((blackboard)
                                                       :equal-fn #'%reach-cache-value-eq)
  #'(lambda (ai-position defender-position ai-movement-points)
      (with-reachable-cache-fns (put-in-cache get-from-cache
                                              reachable-p-w/concening-tiles-fn-insert-cache
                                              reachable-p-w/concening-tiles-fn-search-cache)
        (let ((cached (get-from-cache ai-position defender-position ai-movement-points)))
          (if cached
              (reachable-cache-value-res cached)
              (multiple-value-bind (path cumulative-cost costs)
                  (path-with-concerning-tiles blackboard ai-position defender-position)
                (declare (ignore costs))
                (let ((res (and path (<= cumulative-cost ai-movement-points))))
                  (put-in-cache ai-position defender-position ai-movement-points res)
                  (reachable-cache-value-res (get-from-cache ai-position
                                                             defender-position
                                                             ai-movement-points)))))))))

(defcached-list reachable-p-w/concening-tiles-unlimited-cost-fn
    ((blackboard) :equal-fn #'%reach-cache-value-eq)
  "pretends all players have unlimited movement points"
  #'(lambda (ai-position defender-position ai-movement-points)
      (with-reachable-cache-fns (put-in-cache get-from-cache
                                 reachable-p-w/concening-tiles-unlimited-cost-fn-insert-cache
                                 reachable-p-w/concening-tiles-unlimited-cost-fn-search-cache)
        (let ((cached (get-from-cache ai-position defender-position ai-movement-points)))
          (if cached
              (reachable-cache-value-res cached)
              (multiple-value-bind (path cumulative-cost costs)
                  (path-with-concerning-tiles blackboard ai-position defender-position)
                (declare (ignore cumulative-cost costs))
                (let ((res (if path
                               t
                               nil)))
                  (put-in-cache ai-position defender-position ai-movement-points res)
                  (reachable-cache-value-res (get-from-cache ai-position
                                                             defender-position
                                                             ai-movement-points)))))))))


(defun reachableo (def)
  (fresh (atk-pos def-pos mp)
    (def-pos-o def  def-pos)
    (def-goal-pos-o def atk-pos)
    (def-mp-o       def mp)
    (project (atk-pos def-pos mp)
      (let ((res (funcall *reachable-p-fn* atk-pos def-pos mp)))
        (== res t)))))

(defun attach-attacker@-o (defenders attacker list-length pos out &optional (ct 0))
  (conda
   ((project (ct list-length)    ;; after position
      (== (>= ct list-length) t)
      (== out '())))
   ((project  (ct  pos)            ; position  of  the element  where
      (== (= ct pos) t)            ; attacker must be attached to
      (fresh (c d cdd tmp new-defender)
        (conso c d defenders)
        (cdro  c cdd)
        (conda
            ((nullo cdd)
             (fresh (def-pos atk-pos atk-mp)
               (def-pos-o c def-pos)
               (atk-mp-o  attacker atk-mp)
               (atk-pos-o attacker atk-pos)
               (make-def-o def-pos atk-pos atk-mp new-defender)
               (reachableo new-defender)))
            (else
             (== c new-defender)))
        (attach-attacker@-o d attacker list-length pos tmp (1+ ct))
        (conso new-defender tmp out))))
   (else
    (project (ct pos)
      (fresh (c d tmp)
        (conso c d defenders)
        (attach-attacker@-o d attacker list-length pos tmp (1+ ct))
        (conso  c tmp out))))))

(defun attach-attacker-o (defender attacker list-length ct out)
  (conda
   ((project (ct list-length)
      (== (>= ct list-length) t)
      (== out '())))
   (else
    (project (ct)
      (fresh (tmp tmp2)
        (attach-attacker@-o defender attacker list-length ct tmp)
        (attach-attacker-o  defender attacker list-length (1+ ct)  tmp2)
        (conso tmp tmp2 out))))))

;; defenders (list (def1 def2 def3 ...) ...)
(defun substo-all (defenders defender-number attacker out)
  (conde
   ((nullo defenders)
    (== out '()))
   (else
    (fresh (car-def cdr-def tmp tmp2)
      (conso  car-def cdr-def defenders)
      (attach-attacker-o car-def attacker defender-number 0 tmp)
      (substo-all cdr-def   defender-number attacker tmp2)
      (appendo tmp tmp2 out)))))

(defun %make-attack-tactics (defenders defender-number attackers out)
  (conde
   ((nullo attackers)
    (== out '()))
   (else
    (fresh (car-atk cdr-atk tmp res)
      (conso car-atk cdr-atk attackers)
      (substo-all defenders defender-number car-atk tmp)
      (%make-attack-tactics tmp defender-number cdr-atk res)
      (appendo tmp res out)))))

(defun make-attack-tactics (defenders attackers)
  (car (run* (q)
         (%make-attack-tactics (list defenders) (length defenders) attackers q))))

(defun attacker-class->attacker (game-state atk)
  (let* ((entity (entity:find-entity-by-id game-state (blackboard:entity-id atk)))
         (mp     (character:current-movement-points    (entity:ghost entity)))
         (pos    (mesh:calculate-cost-position entity)))
    (cons pos mp)))

(defun defender-class->defender (def)
  (let ((positions (blackboard:goal-pos def)))
    (mapcar #'(lambda (a) (list a)) positions)))

(defgeneric fetch-defender-positions (object))

(defgeneric fetch-attacker-positions (object))

(defmethod fetch-defender-positions ((object blackboard:blackboard))
  (with-accessors ((main-state                      main-state)
                   (attack-enemy-pole-positions     attack-enemy-pole-positions)
                   (attack-enemy-melee-positions    attack-enemy-melee-positions)
                   (attack-enemy-bow-positions      attack-enemy-bow-positions)
                   (attack-enemy-crossbow-positions attack-enemy-crossbow-positions))
      object
    (flet ((%fetch (l)
             (loop for i in l append (defender-class->defender i))))
      (list
       +pole-key-tactics+     (%fetch attack-enemy-pole-positions)
       +melee-key-tactics+    (%fetch attack-enemy-melee-positions)
       +bow-key-tactics+      (%fetch attack-enemy-bow-positions)
       +crossbow-key-tactics+ (%fetch attack-enemy-crossbow-positions)))))

(defmethod fetch-attacker-positions ((object blackboard))
  (with-accessors ((main-state main-state)) object
    (let ((res '()))
      (flet ((fetch (entity)
               (let* ((ghost  (ghost entity))
                      (status (character:status ghost)))
                 (when (or (not status)
                           (and (not (member status
                                             (list interactive-entity:+status-terror+
                                                   interactive-entity:+status-berserk+
                                                   interactive-entity:+status-faint+)))))
                   (push (cons (mesh:calculate-cost-position entity)
                               (truncate (character:current-movement-points ghost)))
                         res)))))
        (map-ai-entities main-state #'(lambda (v) (fetch v)))
      res))))

(defun find-defender-id-by-goal-position (blackboard position)
  (with-accessors ((attack-enemy-pole-positions     attack-enemy-pole-positions)
                   (attack-enemy-melee-positions    attack-enemy-melee-positions)
                   (attack-enemy-bow-positions      attack-enemy-bow-positions)
                   (attack-enemy-crossbow-positions attack-enemy-crossbow-positions))
      blackboard
    (flet ((find-id (defenders)
             (loop for defender in defenders do
                  (when (find position (goal-pos defender) :test #'ivec2:ivec2=)
                    (return-from find-id (entity-id defender))))))
      (or (find-id attack-enemy-pole-positions)
          (find-id attack-enemy-melee-positions)
          (find-id attack-enemy-bow-positions)
          (find-id attack-enemy-crossbow-positions)))))

(defun attack-tactic->id-entities (blackboard attack-tactic)
  "values are id-attacker id-defender if any"
  (with-accessors ((main-state main-state)) blackboard
    (let* ((attacker-position (tactic-attacker-pos      attack-tactic))
           (defender-position (tactic-defender-slot-pos attack-tactic)))
      (values (game-state:find-ai-id-by-position main-state attacker-position)
              (find-defender-id-by-goal-position blackboard defender-position)))))

(defun build-single-attack-tactics (blackboard position-attakers position-defenders)
  (let ((all (blackboard:make-attack-tactics position-defenders position-attakers)))
    (setf all (remove-if #'(lambda (plan) (every #'(lambda (a) (null (cdr a))) plan))
                         all))
    (when all
      ;; TODO sort  tactics, the idea  is to attack the  less powerful
      ;; enemy with the maximum number of attacker see: character:combined-power
      (setf all (elt all 0))
      (setf all (remove-if-not #'tactic-valid-p all)))
    (mapcar #'(lambda (plan)
                (multiple-value-bind (attacker-id defender-id)
                    (attack-tactic->id-entities blackboard plan)
                    (make-instance 'attacker
                                   :target-pos (tactic-defender-slot-pos plan)
                                   :target-id  defender-id
                                   :entity-id  attacker-id)))
            all)))

(defun build-all-attack-tactics (blackboard reachable-fn-p)
  (let* ((*reachable-p-fn* reachable-fn-p)
         (all-defender-pos (fetch-defender-positions blackboard))
         (all-attacker-pos (fetch-attacker-positions blackboard)))
    (when (> (length all-attacker-pos)
             (length all-defender-pos))
      (setf all-attacker-pos (subseq all-attacker-pos 0 (length all-defender-pos))))
    (list
     +pole-key-tactics+     (build-single-attack-tactics blackboard
                                                         all-attacker-pos
                                                         (getf all-defender-pos
                                                               +pole-key-tactics+))
     +melee-key-tactics+    (build-single-attack-tactics blackboard
                                                         all-attacker-pos
                                                         (getf all-defender-pos
                                                               +melee-key-tactics+))
     +bow-key-tactics+      (build-single-attack-tactics blackboard
                                                         all-attacker-pos
                                                         (getf all-defender-pos
                                                               +bow-key-tactics+))
     +crossbow-key-tactics+ (build-single-attack-tactics blackboard
                                                         all-attacker-pos
                                                         (getf all-defender-pos
                                                               +crossbow-key-tactics+)))))
