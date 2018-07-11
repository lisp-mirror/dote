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

;; atk := (pos . mp)

(defun make-atk (pos mp)
  (cons pos mp))

(defun atk-mp (atk)
  (cdr atk))

(defun atk-pos (atk)
  (car atk))

;; def := (def-slot-pos atk-pos mp)

(defun def-pos (def)
  (car def))

(defun def-goal-pos (def)
  (second def))

(defun def-mp (def)
  (third def))

(defun def-free-goal-p (def)
  (null (def-goal-pos def)))

(defun attack-equals-p (a b)
  (and (ivec2= (def-goal-pos a) (def-goal-pos b))
       (ivec2= (def-pos a)      (def-pos b))
       (=      (def-mp a)       (def-mp  b))))

;; atk := (pos . mp)
;; def := (def-slot-pos atk-pos mp)

(defun make-def (def-pos atk-pos atk-mp)
  (list def-pos atk-pos atk-mp))

(defun tactic-attacker-pos (tactic)
  (elt tactic 1))

(defun tactic-defender-slot-pos (tactic)
  (elt tactic 0))

(defun tactic-valid-p (tactic)
  (= (length tactic) 3))

(defmacro with-position-valid-slot ((tactics candidate) &body additional-constrains)
  (with-gensyms (atk-pos atk-mp)
    `(let ((,atk-pos (atk-pos ,candidate))
           (,atk-mp   (atk-mp  ,candidate)))
       (position-if #'(lambda (a)
                        (and ,@(if (null additional-constrains)
                                   (list t)
                                   additional-constrains)
                             (funcall *reachable-p-fn* ,atk-pos (def-pos a) ,atk-mp)))
                    ,tactics))))

(defun position-valid-in-free-slot (tactics candidate)
  (with-position-valid-slot (tactics candidate)
    (def-free-goal-p a)))

(defun position-valid-in-occupied-slot (tactics candidate)
  (with-position-valid-slot (tactics candidate)
    (not (def-free-goal-p a))))

;; this is suboptimal at best...
(defun %build-single-attack-tactics (tactics position-attackers candidate
                                     &optional (substituted-by-idx nil))
  (labels ((make-new-tactics (old-tactics attacker position)
             (let* ((old-tactic  (elt old-tactics position))
                    (new-tactic  (make-def (def-pos old-tactic)
                                           (atk-pos attacker)
                                           (atk-mp  attacker))))
               (fresh-list-subst@ old-tactics
                                  new-tactic
                                  position)))
           (discard-and-restart (tactics position-attackers)
             (%build-single-attack-tactics tactics
                                           (rest  position-attackers)
                                           (first position-attackers)
                                           nil)))
    (macrolet ((swap-and-restart (tactics candidate position-occupied)
                 (with-gensyms (old-tactic new-tactics new-candidate)
                   `(let* ((,old-tactic    (elt ,tactics ,position-occupied))
                           (,new-tactics   (make-new-tactics ,tactics
                                                             ,candidate
                                                             ,position-occupied))
                           (,new-candidate (make-atk (def-goal-pos ,old-tactic)
                                                     (def-mp       ,old-tactic))))
                      (%build-single-attack-tactics ,new-tactics
                                                    position-attackers
                                                    ,new-candidate
                                                    ,position-occupied)))))
      (if (null candidate)
          tactics
          (let ((position-free (position-valid-in-free-slot tactics candidate)))
            (if position-free
                (%build-single-attack-tactics (make-new-tactics tactics
                                                                candidate
                                                                position-free)
                                              (rest  position-attackers)
                                              (first position-attackers)
                                              nil)
                (let ((position-occupied (position-valid-in-occupied-slot tactics candidate)))
                  (if position-occupied
                      (if substituted-by-idx
                          (if (= position-occupied substituted-by-idx) ; trying to swap
                                                                       ; againg with the
                                                                       ; old pos: not allowed
                              (discard-and-restart tactics position-attackers)
                              (swap-and-restart tactics candidate position-occupied))
                          (swap-and-restart tactics candidate position-occupied))
                      ;; valid position not found, discard
                      (discard-and-restart tactics position-attackers)))))))))

(defun build-single-attack-tactics (blackboard position-attackers position-defenders)
  (let ((all (%build-single-attack-tactics position-defenders
                                           (rest  position-attackers)
                                           (first position-attackers)
                                           nil)))
    (when all
      (tactics-as-list->tactics-as-attacker-instance blackboard all))))

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

(defun tactic-exists-p (blackboard ai-player weapon-tactic reachable-fn-p)
  (when-let* ((all-tactics        (build-all-attack-tactics blackboard
                                                            reachable-fn-p))
              (all-weapon-tactics (getf all-tactics weapon-tactic))
              (my-tactic          (find-if #'(lambda (a) (= (id ai-player) (entity-id a)))
                                    all-weapon-tactics)))
    t))

(defun build-path-to-attack (blackboard ai-player weapon-tactic
                             &key
                               (cut-off-first-tile t)
                               (reachable-fn-p (reachable-p-w/concening-tiles-fn blackboard))
                               (building-path-fn #'path-with-concerning-tiles))
  "returns four values:
- the path (taking into account concerning tiles);
- the cost of each move (taking  into account terrain and other objects;
  like building or  players)
- the total cost of  the path (again only with map cost (i.e. no concerning tiles);
- the id of the target entity."
  (let* ((all-tactics        (build-all-attack-tactics blackboard
                                                       reachable-fn-p))
         (all-weapon-tactics (getf all-tactics weapon-tactic))
         (my-tactic   (find-if #'(lambda (a) (= (id ai-player) (entity-id a)))
                               all-weapon-tactics)))
    (when (and all-tactics all-weapon-tactics my-tactic)
      (let ((start-pos          (mesh:calculate-cost-position ai-player))
            (end-pos            (target-pos my-tactic)))
        ;;(dbg "from ~a -> ~a" start-pos end-pos)
        (multiple-value-bind (result-path cumulative-cost costs-terrain)
            (funcall building-path-fn
                     blackboard
                     start-pos
                     end-pos
                     :cut-off-first-tile cut-off-first-tile)
          (values result-path cumulative-cost costs-terrain (target-id my-tactic)))))))


;; Best...well...
(defun build-best-path-to-attack (blackboard ai-player weapon-tactic
                                  &key
                                    (cut-off-first-tile t)
                                    (reachable-fn-p (reachable-p-w/concening-tiles-fn blackboard)))
  "takes into account concerning-tiles for build paths.
returns four values:
- the path (taking into account concerning tiles);
- the cost of each move (taking  into account terrain and other objects;
  like building or  players)
- the total cost of  the path (again only with map cost (i.e. no concerning tiles);
- the id of the target entity."
  (build-path-to-attack blackboard ai-player weapon-tactic
                        :cut-off-first-tile cut-off-first-tile
                        :reachable-fn-p     reachable-fn-p
                        :building-path-fn   #'path-with-concerning-tiles))

(defun build-insecure-path-to-attack (blackboard ai-player weapon-tactic
                                      &key
                                        (cut-off-first-tile t)
                                        (reachable-fn-p (reachable-p-w/concening-tiles-fn blackboard)))
  "takes into account concerning-tiles for build paths.
returns four values:
- the path (taking into account concerning tiles);
- the cost of each move (taking  into account terrain and other objects;
  like building or  players)
- the total cost of  the path (again only with map cost (i.e. no concerning tiles);
- the id of the target entity."
  (build-path-to-attack blackboard ai-player weapon-tactic
                        :cut-off-first-tile cut-off-first-tile
                        :reachable-fn-p     reachable-fn-p
                        :building-path-fn   #'path-w/o-concerning-tiles))

(defun index-last-reachable-tile (entity path-costs)
  (with-accessors ((ghost ghost)) entity
    (do ((accum 0.0)
         (idx 0 (1+ idx)))
        ((or (>= idx (length path-costs))
             (>= accum
                 (character:current-movement-points ghost)))
         idx)
      (incf accum (elt path-costs idx)))))

(defun best-path-near-attack-goal-w-current-weapon (blackboard ai-player
                                                    &key
                                                      (cut-off-first-tile t)
                                                      (reachable-fn-p #'reachablep))
  (multiple-value-bind (path cumulative-cost costs target-id)
      (best-path-to-reach-attack-pos-w-current-weapon blackboard ai-player
                                                 :cut-off-first-tile cut-off-first-tile
                                                 :reachable-fn-p     reachable-fn-p)
    (let ((max (index-last-reachable-tile ai-player costs)))
      (values (subseq path 0 max)
              cumulative-cost
              costs
              target-id))))

(defun path-near-enemy-pos-w-current-weapon (blackboard ai-player path-builder-fn
                                             &key (cut-off-first-tile t))
  (let ((target (ai-utils:faction-best-visible-target ai-player)))
    (multiple-value-bind (path cumulative-cost costs)
        (funcall path-builder-fn
                 blackboard
                 (calculate-cost-position ai-player)
                 (calculate-cost-position target)
                 :cut-off-first-tile  cut-off-first-tile
                 :allow-path-length-1 nil)
      (when path
        (let ((max (index-last-reachable-tile ai-player costs)))
          (values (subseq path 0 max)
                  cumulative-cost
                  costs
                  (id target)))))))

(defun best-path-near-enemy-pos-w-current-weapon (blackboard ai-player
                                                  &key (cut-off-first-tile t))
  (path-near-enemy-pos-w-current-weapon blackboard ai-player
                                        #'path-with-concerning-tiles
                                        :cut-off-first-tile cut-off-first-tile))

(defun insecure-path-near-enemy-pos-w-current-weapon (blackboard ai-player
                                                      &key (cut-off-first-tile t))
  (path-near-enemy-pos-w-current-weapon blackboard ai-player
                                        #'path-w/o-concerning-tiles
                                        :cut-off-first-tile cut-off-first-tile))

(defun ghost->weapon-tactics (ai-player)
  (with-accessors ((ghost ghost)) ai-player
    (let ((weapon-type (character:weapon-type ghost)))
      (when weapon-type
        (cond
          ((character:weapon-type-pole-p ghost)
           +pole-key-tactics+)
          ((character:weapon-type-bow-p ghost)
           +bow-key-tactics+)
          ((character:weapon-type-crossbow-p ghost)
           +crossbow-key-tactics+)
          (t ;; any other melee weapon
           +melee-key-tactics+))))))

(defun path-to-reach-attack-pos-w-current-weapon (blackboard ai-player
                                                  &key
                                                    (cut-off-first-tile t)
                                                    (reachable-fn-p
                                                     (reachable-p-w/concening-tiles-fn blackboard))
                                                    (path-builder-fn #'build-best-path-to-attack))
  (with-accessors ((ghost ghost)) ai-player
    (let ((weapon-type (character:weapon-type ghost)))
      (when weapon-type
        (cond
          ((character:weapon-type-pole-p ghost)
           (funcall path-builder-fn blackboard ai-player +pole-key-tactics+
                    :cut-off-first-tile cut-off-first-tile
                    :reachable-fn-p reachable-fn-p))
          ((character:weapon-type-bow-p ghost)
           (funcall path-builder-fn blackboard ai-player +bow-key-tactics+
                    :cut-off-first-tile cut-off-first-tile
                    :reachable-fn-p reachable-fn-p))
          ((character:weapon-type-crossbow-p ghost)
           (funcall path-builder-fn blackboard ai-player +crossbow-key-tactics+
                    :cut-off-first-tile cut-off-first-tile
                    :reachable-fn-p reachable-fn-p))
          (t ;; any other melee weapon
           (funcall path-builder-fn blackboard ai-player +melee-key-tactics+
                    :cut-off-first-tile cut-off-first-tile
                    :reachable-fn-p reachable-fn-p)))))))

(defun best-path-to-reach-attack-pos-w-current-weapon (blackboard ai-player
                                                  &key
                                                    (cut-off-first-tile t)
                                                    (reachable-fn-p
                                                     (reachable-p-w/concening-tiles-fn blackboard)))
  (path-to-reach-attack-pos-w-current-weapon blackboard
                                             ai-player
                                             :cut-off-first-tile cut-off-first-tile
                                             :reachable-fn-p     reachable-fn-p
                                             :path-builder-fn    #'build-best-path-to-attack))

(defun insecure-path-to-reach-attack-pos-w-current-weapon (blackboard ai-player
                                                           &key
                                                             (cut-off-first-tile t)
                                                             (reachable-fn-p
                                                              (reachable-p-w/concening-tiles-fn blackboard)))
  (path-to-reach-attack-pos-w-current-weapon blackboard
                                             ai-player
                                             :cut-off-first-tile cut-off-first-tile
                                             :reachable-fn-p     reachable-fn-p
                                             :path-builder-fn    #'build-insecure-path-to-attack))


(defun best-path-w-current-weapon-reachable-p (blackboard ai-player)
  (multiple-value-bind (path cumulative-cost costs)
      (best-path-to-reach-attack-pos-w-current-weapon blackboard ai-player)
    (declare (ignore costs))
    (values (and path
                 (<= cumulative-cost (character:current-movement-points (ghost ai-player))))
            cumulative-cost)))

(defun calc-path-tiles-no-doors (blackboard ai-position
                                 defender-position
                                 heuristic
                                 &key
                                   (cut-off-first-tile t)
                                   (other-costs        '())
                                   (allow-path-length-1 nil))
  "return three values: the path (taking into account other-costs),
the  total cost of (taking  into account terrain and other objects,
like building  or players *but NOT  the doors*) and the cost of each move of
the path, again only with map cost (i.e. no other-costs).

If cut-off-first-tile is  not nil the first element  of the calculated
path is removed
"
  (with-accessors ((main-state main-state)) blackboard
    (with-accessors ((main-state main-state)) blackboard
      (with-accessors ((movement-costs movement-costs)) main-state
        (let* ((suppress-door-mat  (suppress-closed-door-cost-mat blackboard))
               (actual-other-costs (append other-costs (list suppress-door-mat)))
               (path-w/concering   (game-state:build-movement-path main-state
                                                                   ai-position
                                                                   defender-position
                                                                   :other-costs-layer
                                                                   actual-other-costs
                                                                   :heuristic-cost-function
                                                                   heuristic)))
          (if path-w/concering
              (cond
                ((and allow-path-length-1
                      (= (length path-w/concering) 1))
                  (values path-w/concering 0.0))
                ((> (length path-w/concering) 1)
                 (let* ((costs-terrain (loop for i across (subseq path-w/concering 1) collect
                                            (dmax +open-terrain-cost+
                                                  (d+ (d (matrix-elt movement-costs
                                                                     (elt i 1)
                                                                     (elt i 0)))
                                                      (matrix-elt suppress-door-mat
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
                   (values result-path cumulative-cost costs-terrain)))
                (t
                 (values nil nil nil)))
              (values nil nil nil)))))))

(defstruct %path-cache
  (from)
  (to)
  (cut-off-first-p)
  (allow-length-1-p)
  (path)
  (cumulative-cost)
  (costs-terrain))

(defun %path-eq (a b)
  (and (ivec2= (%path-cache-from a)
               (%path-cache-from b))
       (ivec2= (%path-cache-to a)
               (%path-cache-to b))
       (eq     (%path-cache-cut-off-first-p a)
               (%path-cache-cut-off-first-p b))
       (eq     (%path-cache-allow-length-1-p a)
               (%path-cache-allow-length-1-p b))))

(defmacro with-cache-path-skel ((search-cache-fn insert-cache-fn) &body body)
  `(labels ((make-cache-key (from to cut-first allow-1
                                  &optional
                                  (path nil) (cumulative-cost 0.0)
                                  (costs-terrain nil))
              (make-%path-cache :from             from
                                :to               to
                                :cut-off-first-p  cut-first
                                :allow-length-1-p allow-1
                                :path             path
                                :cumulative-cost  cumulative-cost
                                :costs-terrain    costs-terrain))
             (get-from-cache (from to cut-first allow-1)
               (,search-cache-fn (make-cache-key from to
                                                 cut-first
                                                 allow-1)))
             (put-in-cache (from to cut-first allow-1
                                 path cumulative-cost costs-terrain)
               (,insert-cache-fn (make-cache-key from
                                                 to
                                                 cut-first allow-1
                                                 path
                                                 cumulative-cost
                                                 costs-terrain))))
     ,@body))

(defcached-list path-with-concerning-tiles ((blackboard ai-position
                                                       defender-position
                                                       &key
                                                       (cut-off-first-tile t)
                                                       (allow-path-length-1 nil))
                                            :equal-fn #'%path-eq)
    "return three values: the path (taking into account concerning tiles),
the  total cost of (taking  into account terrain and other objects,
like building  or players *but NOT  the doors*) and the cost of each move of
the path, again only with map cost (i.e. no concerning tiles).

If cut-off-first-tile is  not nil the first element  of the calculated
path is removed
"
    (with-cache-path-skel (path-with-concerning-tiles-search-cache
                           path-with-concerning-tiles-insert-cache)
      (let ((found (get-from-cache ai-position
                                   defender-position
                                   cut-off-first-tile
                                   allow-path-length-1)))
        (if found
            (values (%path-cache-path            found)
                    (%path-cache-cumulative-cost found)
                    (%path-cache-costs-terrain   found))
            (let* ((concerning-tiles (concerning-tiles->costs-matrix blackboard))
                   (others-costs     (list concerning-tiles))
                   (heuristic        (heuristic-manhattam)))
              (multiple-value-bind (path cumulative-cost terrain-costs)
                  (calc-path-tiles-no-doors blackboard ai-position defender-position
                                            heuristic
                                            :cut-off-first-tile  cut-off-first-tile
                                            :other-costs         others-costs
                                            :allow-path-length-1 allow-path-length-1)
                (put-in-cache ai-position
                              defender-position
                              cut-off-first-tile
                              allow-path-length-1
                              path
                              cumulative-cost
                              terrain-costs)
                (path-with-concerning-tiles blackboard
                                            ai-position
                                            defender-position
                                            :cut-off-first-tile cut-off-first-tile
                                            :allow-path-length-1 allow-path-length-1)))))))

(defcached-list path-w/o-concerning-tiles ((blackboard ai-position
                                                       defender-position
                                                       &key
                                                       (cut-off-first-tile t)
                                                       (allow-path-length-1 nil))
                                           :equal-fn #'%path-eq)
  "see calc-path-tiles-no-doors"
  (with-cache-path-skel (path-w/o-concerning-tiles-search-cache
                         path-w/o-concerning-tiles-insert-cache)
      (let ((found (get-from-cache ai-position
                                   defender-position
                                   cut-off-first-tile
                                   allow-path-length-1)))
        (if found
            (values (%path-cache-path            found)
                    (%path-cache-cumulative-cost found)
                    (%path-cache-costs-terrain   found))
            (let* ((heuristic (heuristic-manhattam)))
              (multiple-value-bind (path cumulative-cost terrain-costs)
                  (calc-path-tiles-no-doors blackboard ai-position defender-position
                                            heuristic
                                            :cut-off-first-tile cut-off-first-tile
                                            :other-costs        '()
                                            :allow-path-length-1 allow-path-length-1)
                (put-in-cache ai-position
                              defender-position
                              cut-off-first-tile
                              allow-path-length-1
                              path
                              cumulative-cost
                              terrain-costs)
                (path-w/o-concerning-tiles blackboard
                                           ai-position
                                           defender-position
                                           :cut-off-first-tile cut-off-first-tile
                                           :allow-path-length-1 allow-path-length-1)))))))

(defun pos-longest-reachable-path (entity costs)
  (do ((accum 0.0)
       (idx 0 (1+ idx)))
      ((or (>= idx (length costs))
           (>= accum
               (character:current-movement-points (ghost entity))))
       (1+ idx))
    (incf accum (elt costs idx))))

(defun path-near-goal-w/o-concerning-tiles (blackboard entity goal-pos
                                            &key
                                              (cut-off-first-tile t))
  "note: doors are ignored"
  (multiple-value-bind (path cumulative-cost costs)
      (path-w/o-concerning-tiles blackboard
                                 (calculate-cost-position entity)
                                 goal-pos
                                 :cut-off-first-tile cut-off-first-tile)
    (let ((max (pos-longest-reachable-path entity costs)))
      (values (subseq path 0 max)
              cumulative-cost
              costs))))

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

(defcached-list reachable-p-w/concening-tiles-fn ((blackboard
                                                   &key
                                                   (cut-off-first-tile t)
                                                   (allow-path-length-1 nil))
                                                  :equal-fn #'%reach-cache-value-eq)
  #'(lambda (ai-position defender-position ai-movement-points)
      (with-reachable-cache-fns (put-in-cache get-from-cache
                                              reachable-p-w/concening-tiles-fn-insert-cache
                                              reachable-p-w/concening-tiles-fn-search-cache)
        (let ((cached (get-from-cache ai-position defender-position ai-movement-points)))
          (if cached
              (reachable-cache-value-res cached)
              (multiple-value-bind (path cumulative-cost costs)
                  (path-with-concerning-tiles blackboard
                                              ai-position
                                              defender-position
                                              :cut-off-first-tile  cut-off-first-tile
                                              :allow-path-length-1 allow-path-length-1)
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

(defun cost-w/o-concening-tiles (entity goal-pos)
  (multiple-value-bind (path cumulative-cost costs)
      (path-w/o-concerning-tiles (blackboard (state entity))
                                 (calculate-cost-position entity)
                                 goal-pos)
    (declare (ignore costs))
    (and path cumulative-cost)))

(defun reachable-p-w/o-concening-tiles-fn (blackboard)
  #'(lambda (ai-position defender-position ai-movement-points)
      (multiple-value-bind (path cumulative-cost costs)
          (path-w/o-concerning-tiles blackboard ai-position defender-position)
        (declare (ignore costs))
        (and path (<= cumulative-cost ai-movement-points)))))

(defun attacker-class->attacker (game-state atk)
  (let* ((entity (entity:find-entity-by-id game-state (blackboard:entity-id atk)))
         (mp     (character:current-movement-points    (entity:ghost entity)))
         (pos    (mesh:calculate-cost-position entity)))
    (cons pos mp)))

(defun defender-class->defender (def)
  (let ((positions (blackboard:goal-pos def)))
    (mapcar #'(lambda (a) (list a)) positions)))

(defgeneric fetch-defender-positions (object))

(defgeneric fetch-attacker-positions (object &key filter-fn))

(defun min-dist (blackboard pos &key (dist-fn #'map-utils:map-manhattam-distance))
  (let ((min 1e6))
    (loop-ai-entities (main-state blackboard)
       #'(lambda (a)
           (when-let* ((pos-player  (calculate-cost-position a))
                       (dist        (funcall dist-fn pos-player pos))
                       (visible-a-p (absee-mesh:placeholder-visible-p a
                                                                      (seq-x pos)
                                                                      (seq-y pos))))
             (when (< dist min)
               (setf min dist)))))
    min))

(defun sort-attack-position-pred (blackboard)
  (with-accessors ((main-state main-state)) blackboard
    #'(lambda (a b)
        (let ((cost-a (get-cost main-state (seq-x a) (seq-y a)))
              (cost-b (get-cost main-state (seq-x b) (seq-y b)))
              (dist-a (min-dist blackboard a))
              (dist-b (min-dist blackboard b)))
          (if (epsilon= dist-a dist-b)
              (< cost-a cost-b)
              (< dist-a dist-b))))))

(defun sort-attack-position (blackboard positions weapon-tactic)
  (declare (ignore weapon-tactic))
  (if positions
      (let ((sorted (shellsort positions (sort-attack-position-pred blackboard) :key #'first)))
        sorted)
      positions)); nil

(defmethod fetch-defender-positions ((object blackboard:blackboard))
  (with-accessors ((main-state                      main-state)
                   (attack-enemy-pole-positions     attack-enemy-pole-positions)
                   (attack-enemy-melee-positions    attack-enemy-melee-positions)
                   (attack-enemy-bow-positions      attack-enemy-bow-positions)
                   (attack-enemy-crossbow-positions attack-enemy-crossbow-positions))
      object
    (flet ((%fetch (l weapon-tactic)
             (let ((all-pos (loop for i in l append (defender-class->defender i))))
               (sort-attack-position object all-pos weapon-tactic))))
      (list
       +pole-key-tactics+     (%fetch attack-enemy-pole-positions     +pole-key-tactics+)
       +melee-key-tactics+    (%fetch attack-enemy-melee-positions    +melee-key-tactics+)
       +bow-key-tactics+      (%fetch attack-enemy-bow-positions      +bow-key-tactics+)
       +crossbow-key-tactics+ (%fetch attack-enemy-crossbow-positions +crossbow-key-tactics+)))))

(defmethod fetch-attacker-positions ((object blackboard)
                                     &key (filter-fn #'(lambda (a)
                                                         (declare (ignore a))
                                                         t)))
  (with-accessors ((main-state main-state)) object
    (let ((res '()))
      (flet ((fetch (entity)
               (let* ((ghost  (ghost entity))
                      (status (character:status ghost)))
                 (when (or (not status)
                           (and (not (character:status-terror-p ghost))
                                (not (character:status-faint-p  ghost))))
                   (push (cons (mesh:calculate-cost-position entity)
                               (truncate (character:current-movement-points ghost)))
                         res)))))
        (loop-ai-entities main-state
             #'(lambda (ent)
                 (when (and (character:pclass-of-useful-in-attack-tactic-p ent)
                            (funcall filter-fn ent))
                   (fetch ent))))
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

(defun tactics-as-list->tactics-as-attacker-instance (blackboard tactics)
  (setf tactics (remove-if-not #'tactic-valid-p tactics))
  (mapcar #'(lambda (plan)
              (multiple-value-bind (attacker-id defender-id)
                  (attack-tactic->id-entities blackboard plan)
                (make-attacker-instance (tactic-defender-slot-pos plan)
                                        defender-id
                                        attacker-id)))
          tactics))

(defun filter-attack-pos-by-weapon (fn)
  #'(lambda (ent)
      (funcall fn (ghost ent))))

(defun build-all-attack-tactics (blackboard reachable-fn-p)
  (flet ((%fetch-attack (fn)
           (fetch-attacker-positions blackboard
                                     :filter-fn (filter-attack-pos-by-weapon fn))))
    (let* ((*reachable-p-fn* reachable-fn-p)
           (all-defender-pos (fetch-defender-positions blackboard)))
      (list
       +pole-key-tactics+     (build-single-attack-tactics blackboard
                                                           (%fetch-attack
                                                            #'character:weapon-type-pole-p)
                                                           (getf all-defender-pos
                                                                 +pole-key-tactics+))
       +melee-key-tactics+    (build-single-attack-tactics blackboard
                                                           (%fetch-attack
                                                            #'character:weapon-type-minimum-range-p)
                                                           (getf all-defender-pos
                                                                 +melee-key-tactics+))
       +bow-key-tactics+      (build-single-attack-tactics blackboard
                                                           (%fetch-attack
                                                            #'character:weapon-type-bow-p)
                                                           (getf all-defender-pos
                                                                 +bow-key-tactics+))
       +crossbow-key-tactics+ (build-single-attack-tactics blackboard
                                                           (%fetch-attack
                                                            #'character:weapon-type-crossbow-p)
                                                           (getf all-defender-pos
                                                                 +crossbow-key-tactics+))))))

(defun entity-in-valid-attackable-pos-p (entity &key (use-ray-test-only t))
  "is the entity in a valid attacking position with the current weapon?"
  (with-accessors ((state state)) entity
    (with-accessors ((map-state map-state)) state
      (let* ((entity-pos  (calculate-cost-position entity))
             (map-fn      (game-state:opposite-faction-map-fn entity))
             (goals-fn    (character:weapon-case (entity)
                            :pole
                            (pole-weapon-goal-generator-fn entity)
                            :melee
                            (melee-weapon-goal-generator-fn)
                            :bow
                            (long-range-weapon-goal-generator-fn entity
                                                                 (bow-range-for-attack-goal))
                            :crossbow
                            (long-range-weapon-goal-generator-fn entity
                                                                 (crossbow-range-for-attack-goal))))
             (goals-raw   (funcall map-fn
                                   state
                                   #'(lambda (enemy)
                                       (let ((enemy-pos (calculate-cost-position enemy)))
                                         (displace-2d-vector (enemy-pos x y)
                                           (funcall goals-fn x y))))))
             (goals      (remove-if-not #'(lambda (c) (valid-coordinates-p map-state c))
                                        (reduce #'(lambda (a b)
                                                    (if (listp b)
                                                        (lcat a b)
                                                        (lcat a (seq->list b))))
                                                goals-raw
                                                :initial-value '())))
             (test-vis-fn (if use-ray-test-only
                              #'(lambda (o)
                                  (absee-mesh:other-visible-ray-p entity o
                                                                  :exclude-if-labyrinth-entity t))
                              #'(lambda (o)
                                  (absee-mesh:other-visible-p entity o
                                                              :exclude-if-labyrinth-entity t)))))
        (dolist (pos goals)
          (when (ivec2= pos entity-pos) ; entity is in attackable position...
            (funcall map-fn             ; but can it see an enemy?
                     state
                     #'(lambda (v)
                         (when (funcall test-vis-fn v) ; yes; return
                           (return-from entity-in-valid-attackable-pos-p v))))))
        nil)))) ; not a valid attack position
