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

(in-package :game-state)

(alexandria:define-constant +start-day+                 6          :test #'=)

(alexandria:define-constant +end-day+                  20          :test #'=)

(alexandria:define-constant +start-night+              21          :test #'=)

(alexandria:define-constant +end-night+                 5          :test #'=)

(alexandria:define-constant +zenith-night+             23          :test #'=)

(alexandria:define-constant +zenith-day+               13          :test #'=)

(alexandria:define-constant +yellow-light-color+        §cffff99ff :test #'vec4~)

(alexandria:define-constant +white-light-color+         §cffffffff :test #'vec4~)

(alexandria:define-constant +blueish-light-color+       §c7ba8e4ff :test #'vec4~)

(alexandria:define-constant +density-no-fog+            0.0        :test #'=)

(alexandria:define-constant +density-fog+               0.006      :test #'=)

(alexandria:define-constant +pc-a*-cross-weight+        4.05       :test #'=)

(alexandria:define-constant  +default-a*-cost-scaling+  1.001      :test #'=)

(defun hour->light-color (h)
  (cond
    ((or (<= 0 h +start-day+)
         (<= +start-night+ h +zenith-night+))
     +blueish-light-color+)
    ((or (<= 7 h 8)
         (<= 18 h +end-day+))
     nil)
    ((or (<= 9 h 10)
         (= h 17))
     +yellow-light-color+)
    ((<= 11 h 16)
     +white-light-color+)
    (t
     (error "No valid hour..."))))

(defun hour->celestial-body-position-latitude (h)
  (if (<= +start-day+ h +end-day+) ;; day
      (d+ 30.0 (d* 8.57 (d- (d h) (d +start-day+))))
      (if (<= +start-night+ h +zenith-night+) ;; night
          (d* 26.7 (d- (d h) (d +start-night+)))
          (d+ 100.0 (d* 13.33 (d h))))))

(defun hour->celestial-body-position-longitude (h)
  (if (<= +start-day+ h +zenith-day+) ;; day
      (d+ (d +end-day+) (d* 4.28 (d- (d h) (d +start-day+))))
      (if (<= 14 h +end-day+)
          (d- 153.0 (d* 7.0 (d h)))
          (if (<= +start-night+ h +zenith-night+) ;; night
              (d* 13.330 (d- (d h) (d +start-night+)))
              (d- 45.0 (d+ 10.38 (d* 3.46 (d h))))))))

(defun hour->celestial-body-position (h)
  (let* ((longitude (sb-cga:rotate-around (3d-utils:vec-negate +x-axe+)
                                   (deg->rad (hour->celestial-body-position-longitude h))))
         (latitude  (sb-cga:rotate-around +y-axe+
                                          (deg->rad (hour->celestial-body-position-latitude h))))
         (transform (sb-cga:matrix* latitude longitude)))
    (sb-cga:vec* (sb-cga:transform-direction +z-axe+ transform)
          +maximum-map-size+)))

(defmacro gen-type (&rest names)
  `(progn
     ,@(loop for name in names collect
            `(alexandria:define-constant
                 ,(alexandria:format-symbol t "~:@(+~a-type+~)" name)
                 ,(alexandria:make-keyword name)
               :test #'eq))))

(gen-type ceiling
          floor
          empty
          unknown
          door-n
          door-s
          door-w
          door-e
          wall
          tree
          furniture
          magic-furniture
          container
          pillar
          chair
          table
          walkable
          wall-decoration
          npc
          pc
          trap)

(defun invalid-entity-id-map-state ()
  (- +start-id-counter+ 1))

(defclass map-state-element (identificable)
  ((entity-id
    :initform (invalid-entity-id-map-state)
    :initarg  :entity-id
    :accessor entity-id)
   (el-type
    :initform +empty-type+
    :initarg  :el-type
    :accessor el-type)
   (occlude
    :initform nil
    :initarg  :occlude
    :reader   occludep
    :writer   (setf occlude))
   (old-state-element
    :initform nil
    :initarg  :old-state-element
    :accessor old-state-element
    :documentation "a pointer to the nest element (as in a linked list)")))

(defmethod initialize-instance :after ((object map-state-element) &key &allow-other-keys)
  (when (not (eq (old-state-element object) :stopper))
    (let ((saved (make-instance 'map-state-element :old-state-element :stopper)))
      (clone-into object saved)
      (setf (old-state-element object) saved))))

(defmethod marshal:class-persistant-slots ((object map-state-element))
  '(entity-id
    el-type
    occlude
    old-state-element))

(defmethod clone-into ((from map-state-element) (to map-state-element))
  (setf (entity-id to)         (entity-id from)
        (el-type   to)         (el-type   from)
        (occlude   to)         (occludep  from)))

(defmethod clone ((object map-state-element))
  (with-simple-clone (object 'map-state-element)))

(defgeneric map-element-empty-p (object))

(defgeneric map-element-occupied-by-character-p (object))

(defgeneric map-element-occupied-by-invalicable-p (object &key include-door))

(defgeneric neighborhood-by-type (object row column type &key w-offset h-offset))

(defgeneric get-neighborhood (object row column predicate &key w-offset h-offset))

(defgeneric insert-state-chain-after (object idx entity type occlusion-value &optional ct))

(defmethod insert-state-chain-after ((object map-state-element)
                                      idx entity type occlusion-value
                                      &optional (ct 0))
  (with-accessors ((old-state-element old-state-element)) object
    (if (= ct idx)
        (let ((new-element (make-instance 'map-state-element
                                          :old-state-element old-state-element
                                          :occlude           occlusion-value
                                          :el-type           type
                                          :entity-id         (id entity))))
          (setf old-state-element new-element)
          object)
        (insert-state-chain-after object idx entity type occlusion-value (1+ ct)))))


(defmacro or-types (probe &body types)
  `(or
    ,@(loop for type in types collect
           `(eq (el-type ,probe) ,type))))

(defmethod map-element-empty-p ((object map-state-element))
  (or-types object +empty-type+ +floor-type+ +walkable-type+))

(defmethod map-element-occupied-by-character-p ((object map-state-element))
  "t if occupied by either player or ai (npc) character"
  (or (eq (el-type object)
          +pc-type+)
      (eq (el-type object)
          +npc-type+)))

(defmethod map-element-occupied-by-invalicable-p ((object map-state-element)
                                                  &key (include-door t))
  (cond
    ((or-types object
       +wall-type+
       +tree-type+
       +furniture-type+
       +magic-furniture-type+
       +container-type+
       +pillar-type+
       +chair-type+
       +table-type+)
     :occupied)
    ((and include-door
          (or-types object
            +door-n-type+
            +door-s-type+
            +door-w-type+
            +door-e-type+))
     :occupied)
    (t nil)))

(defclass movement-path ()
  ((tiles
    :initform nil
    :accessor tiles
    :initarg  :tiles)
   (cost
    :initform nil
    :accessor cost
    :initarg  :cost)))

(defun make-movement-path (tiles cost)
  (make-instance 'movement-path :tiles tiles :cost cost))

(defclass game-state ()
  ((game-map-file
    :accessor game-map-file
    :initarg  :game-map-file
    :initform nil)
   (game-hour
    :accessor game-hour
    :initarg :game-hour
    :initform 0)
   (game-minutes
    :accessor game-minutes
    :initarg :game-minutes
    :initform 0)
   (game-turn
    :accessor game-turn
    :initarg :game-turn
    :initform 0)
   (ai-entities-action-order
    :accessor ai-entities-action-order
    :initarg  :ai-entities-action-order
    :initform '()
    :documentation "Note: keep always ordered: see blackboard:calc-ai-entities-action-order")
   (current-time
    :accessor current-time
    :initarg :current-time
    :initform (num:desired 0.0))
   (celestial-body-position
    :accessor celestial-body-position
    :initarg :celestial-body-position
    :initform (sb-cga:vec 0.0 100.0 0.0))
   (sky-bg-color
    :accessor sky-bg-color
    :initarg :sky-bg-color
    :initform §c000000ff)
   (light-color
    :accessor light-color
    :initarg  :light-color
    :initform (vec4->vec §cffffffff)
    :documentation "The diffuse and specular color (phong shading) for sun or moon light")
   (fog-density
    :accessor fog-density
    :initarg  :fog-density
    :initform +density-no-fog+)
   (movement-costs
    :accessor movement-costs
    :initarg  :movement-costs
    :initform nil
    :type graph:tile-multilayers-graph
    :documentation  "note  this  is  a multilayer  graph  and  hold  a
    reference  (i.e.  sum  the  values from)  to  costs-from-maps  and
    costs-from-players, see load-level:load-level.")
   (movement-costs-pc
    :accessor movement-costs-pc
    :initarg  :movement-costs-pc
    :initform nil
    :type graph:tile-multilayers-graph
    :documentation  "note  this  is  a multilayer  graph  and  hold  a
    reference  (i.e.  sum  the  values from)  to  costs-from-maps  and
    costs-from-players, see load-level:load-level.")
   (costs-from-map
    :accessor costs-from-map
    :initarg  :costs-from-map
    :initform nil
    :type matrix:matrix)
   (costs-from-players
    :accessor costs-from-players
    :initarg  :costs-from-players
    :initform nil
    :type matrix:matrix)
   (costs-from-pc
    :accessor costs-from-pc
    :initarg  :costs-from-pc
    :initform nil
    :type matrix:matrix
    :documentation  "Used only for build movement path of PC")
   (map-state
    :accessor map-state
    :initarg  :map-state
    :initform nil)
   (all-entities
    :accessor all-entities
    :initarg  :all-entities
    :initform (rb-tree:make-root-rb-node nil rb-tree:+rb-red+))
   (level-difficult
    :accessor level-difficult
    :initarg  :level-difficult
    :initform (1+ +difficult-medium+))
   (level-name
    :accessor level-name
    :initarg  :level-name
    :initform "test")
   (level-name-color
    :accessor level-name-color
    :initarg  :level-name-color
    :initform (vec4 1.0 0.0 1.0 1.0))
   (map-cache-dir
    :accessor map-cache-dir
    :initarg  :map-cache-dir
    :initform nil)
   (window-id
    :initform nil
    :initarg  :window-id
    :accessor window-id)
   (labyrinth-entities
    :initform (make-hash-table :test 'equal)
    :initarg  :labyrinth-entities
    :accessor labyrinth-entities)
   (player-entities
    :initform '()
    :initarg  :player-entities
    :accessor player-entities
    :type     :list)
   (ai-entities
    :initform ()
    :initarg  :ai-entities
    :accessor ai-entities
    :type     :list
    :documentation "Note: keep always ordered: see blackboard:calc-ai-entities-action-order")
   (selected-pc
    :initform nil
    :initarg  :selected-pc
    :reader selected-pc)
   (index-selected-pc
    :initform 0
    :initarg  :index-selected-pc
    :accessor index-selected-pc
    :type fixnum
    :documentation "index of an element of slot selected pc")
   (selected-path
    :initform nil
    :initarg  :selected-path
    :accessor selected-path)
   (blackboard
    :initform nil
    :initarg  :blackboard
    :accessor blackboard
    :type blackboard:blackboard)))

(defgeneric select-next-pc (object))

(defgeneric select-prev-pc (object))

(defgeneric fetch-render-window (object))

(defgeneric fetch-world (object))

(defgeneric setup-game-hour (object hour))

(defgeneric prepare-map-state (object map))

(defgeneric el-type-in-pos (object x y))

(defgeneric entity-id-in-pos (object x y))

(defgeneric entity-in-pos (object x y))

(defgeneric entity-ai-in-pos (object x y))

(defgeneric entity-player-in-pos (object x y))

(defgeneric build-movement-path (object start end
                                 &key other-costs-layer heuristic-cost-function))

(defgeneric build-movement-path-pc (object start end
                                    &key other-costs-layer heuristic-cost-function))

(defgeneric terrain-aabb-2d (object))

(defgeneric terrain-aabb-2d (object))

(defgeneric push-entity (object entity))

(defgeneric push-trap-entity (object entity))

(defgeneric pop-trap-entity (object entity))

(defgeneric pop-map-state-entity (object entity))

(defgeneric push-labyrinth-entity (object labyrinth))

(defgeneric find-labyrinth-by-id (object labyrinth-id))

(defgeneric map-level (object))

(defgeneric add-to-player-entities (object id))

(defgeneric add-to-ai-entities (object id))

(defgeneric fetch-from-player-entities (object id-entity))

(defgeneric fetch-from-ai-entities (object entity))

(defgeneric map-player-entities (object function))

(defgeneric loop-player-entities (object function))

(defgeneric map-ai-entities (object function))

(defgeneric loop-ai-entities (object function))

(defgeneric all-player-id-by-faction (object faction))

(defgeneric terrain-height@pos (object x z))

(defgeneric place-player-on-map (object player faction &key position force-position-p))

(defgeneric update-rendering-needed-ai (object))

(defgeneric update-all-visibility-state (object))

(defgeneric set-invalicable-cost-player-layer@ (object x y))

(defgeneric set-invalicable-cost-pc-layer@ (object x y))

(defgeneric set-invalicable-cost-map-layer@ (object x y))

(defgeneric set-minimum-cost-map-layer@ (object x y))

(defgeneric set-minimum-cost-player-layer@ (object x y))

(defgeneric set-minmum-cost-pc-layer@ (object x y))

(defgeneric set-map-state-type (object x y type))

(defgeneric set-map-state-id (object x y id))

(defgeneric set-map-state-occlusion (object x y occlusion-value))

(defgeneric setup-map-state-entity (object entity type occlusion-value))

(defgeneric move-map-state-entity (object entity from))

(defgeneric clean-map-state-entity (object coord))

(defgeneric approx-terrain-height@pos (object x z))

(defgeneric path-same-ends-p (object start end))

(defgeneric turn-on-fog (object))

(defgeneric turn-off-fog (object))

(defgeneric entity-next-p (object me other))

(defgeneric faction-turn  (object))

(defgeneric faction-turn-human-p (object))

(defgeneric faction-turn-ai-p    (object))

(defgeneric set-tile-visited (object entity-visiting x y &key update-infos))

(defgeneric set-concerning-tile (object x y &key danger-zone-size concerning-tile-value))

(defgeneric set-concerning-tile-fn (object x y &key danger-zone-size concerning-tile-fn))

(defgeneric max-ai-movement-points (object))

(defgeneric position-inside-room-p (object position))

(defgeneric calc-ai-entities-action-order (object))

(defgeneric door-in-next-path-tile-p (object path idx-pos-maybe-door))

(defgeneric invalicable-in-next-path-tile-p (object entity path idx))

(defgeneric remove-entity-from-all-attack-pos (object entity))

(defgeneric clean-characters-logs (object trigger))

(defgeneric reset-costs-from-pc (object))

(defgeneric get-costs-from-pc@ (object x y))

(defgeneric fetch-all-entity-in-map-by-type (object type))

(defgeneric fetch-all-traps (object))

(defgeneric fetch-all-containers (object))

(defgeneric fetch-all-doors (object))

(defmethod (setf selected-pc) (entity (object game-state))
  "set index-selected-pc as well"
  (with-accessors ((index-selected-pc index-selected-pc)
                   (player-entities   player-entities)
                   (ai-entities       ai-entities)) object
    (declare (list player-entities ai-entities))
    (let ((pos-found #- debug-mode
                     (position entity player-entities :test #'test-id=)
                     #+debug-mode
                     (or (position entity player-entities :test #'test-id=)
                         (position entity ai-entities     :test #'test-id=))))
      #+debug-mode (assert pos-found)
      (setf index-selected-pc pos-found)
      (setf (slot-value object 'selected-pc) entity))))

(defmethod select-next-pc ((object game-state))
  (with-accessors ((index-selected-pc index-selected-pc)
                   (player-entities   player-entities)
                   (selected-pc selected-pc)) object
    (setf index-selected-pc (rem (1+ index-selected-pc)
                                 (length player-entities)))
    (setf selected-pc (elt player-entities index-selected-pc))
    object))

(defmethod select-prev-pc ((object game-state))
  (with-accessors ((index-selected-pc index-selected-pc)
                   (player-entities   player-entities)
                   (selected-pc selected-pc)) object
    (setf index-selected-pc (mod (1- index-selected-pc)
                                 (length player-entities)))
    (setf selected-pc (elt player-entities index-selected-pc))
    object))

(defmethod fetch-render-window ((object game-state))
  (and (window-id object)
       (sdl2.kit-utils:fetch-window (window-id object))))

(defmacro with-world ((world game-state) &body body)
  `(let ((,world (fetch-world ,game-state)))
     ,@body))

(defmethod fetch-world ((object game-state))
  (let ((w (fetch-render-window object)))
    (and w
         (main-window:world w))))

(defmethod  setup-game-hour ((object game-state) hour)
  (with-accessors ((game-hour game-hour)
                   (sky-bg-color sky-bg-color)
                   (celestial-body-position celestial-body-position)
                   (light-color light-color)) object
    (setf game-hour               hour)
    (setf sky-bg-color            (pixmap:gen-bg-sky-colors hour))
    (setf celestial-body-position (hour->celestial-body-position hour))
    (setf light-color             (vec4->vec (or (hour->light-color game-hour)
                                                 sky-bg-color)))))

(defmethod get-cost ((object game-state) x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (get-cost-insecure object x y))

(defmethod get-cost-insecure ((object game-state) x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-accessors ((movement-costs movement-costs)) object
    (graph:traverse-cost movement-costs #() (vector x y))))

(defmacro gen-map-state-reader (&rest names)
  `(progn
     ,@(loop for name in names collect
            (let ((fn-name (alexandria:format-symbol t "~:@(~a-in-pos~)" name))
                  (fn      (alexandria:format-symbol t "~:@(~a~)" name)))
              `(progn
                 (defgeneric ,fn-name (object x y))
                 (defmethod  ,fn-name ((object game-state) (x fixnum) (y fixnum))
                   (declare (optimize (speed 0) (safety 3) (debug 3)))
                   (,fn (matrix-elt (map-state object) y x))))))))

(gen-map-state-reader el-type entity-id occludep)

(defmethod entity-in-pos ((object game-state) (x fixnum) (y fixnum))
  (let ((id (entity-id (matrix-elt (map-state object) y x))))
    (find-entity-by-id object id)))

(defun entity-of-faction-in-pos (game-state x y faction-fn)
  (let ((id (entity-id (matrix-elt (map-state game-state) y x))))
    (if (funcall faction-fn game-state id)
        (entity-in-pos game-state x y)
        nil)))

(defmethod entity-ai-in-pos ((object game-state) (x fixnum) (y fixnum))
  (entity-of-faction-in-pos object x y #'faction-ai-p))

(defmethod entity-player-in-pos ((object game-state) (x fixnum) (y fixnum))
  (entity-of-faction-in-pos object x y #'faction-player-p))

(misc:defalias entity-ai@pos-p #'entity-ai-in-pos)

(misc:defalias entity-player@pos-p #'entity-player-in-pos)

(defgeneric pawn@pos-p (object x y))

(defgeneric element-mapstate@ (object x y))

(defgeneric door@pos-p (object x y))

(defgeneric empty@pos-p (object x y))

(defgeneric trap@pos-p (object x y))

(defgeneric container@pos-p (object x y))

(defgeneric magic-forniture@pos-p (object x y))

(defmethod pawn@pos-p ((object game-state) x y)
  (let ((entity (entity-in-pos object x y)))
    (pawnp entity)))

(defmethod element-mapstate@ ((object game-state) (x fixnum) (y fixnum))
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (matrix-elt (map-state object) y x))

(defmethod door@pos-p ((object game-state) (x fixnum) (y fixnum))
  (door@pos-p (map-state object) x y))

(defmethod door@pos-p ((object matrix) (x fixnum) (y fixnum))
  (let ((el (matrix-elt object y x)))
    (or-types el
      +door-n-type+
      +door-s-type+
      +door-w-type+
      +door-e-type+)))

(defmacro def-*@pos-p-gamestate (&rest names)
  `(progn
     ,@(loop for n in names collect
            `(defmethod ,n ((object game-state) (x fixnum) (y fixnum))
               (,n (map-state object) x y)))))

(def-*@pos-p-gamestate empty@pos-p trap@pos-p container@pos-p magic-furniture@pos-p)

(defmethod empty@pos-p ((object matrix) (x fixnum) (y fixnum))
  (map-element-empty-p (matrix-elt object y x)))

(defun %eq-type (matrix x y type)
  (eq (el-type (matrix-elt matrix y x))
      type))

(defmacro def-*@pos-p-matrix (&rest names-types)
  `(progn
     ,@(loop for nt in names-types collect
            `(defmethod ,(car nt) ((object matrix) (x fixnum) (y fixnum))
                 (%eq-type object x y ,(cdr nt))))))

(def-*@pos-p-matrix
    (trap@pos-p          .  +trap-type+)
    (container@pos-p     .  +container-type+)
  (magic-furniture@pos-p .  +magic-furniture-type+))

(defmethod prepare-map-state ((object game-state) (map random-terrain))
  (with-accessors ((map-state map-state)) object
     (setf map-state
          (gen-matrix-frame (truncate (* (width  (matrix map)) +terrain-chunk-size-scale+))
                            (truncate (* (height (matrix map)) +terrain-chunk-size-scale+))))
     (loop for i from 0 below (length (data map-state)) do
          (setf (elt (data map-state) i) (make-instance 'map-state-element)))))

(defun heuristic-manhattam (&optional (cost-scaling +default-a*-cost-scaling+))
  #'(lambda (object a b start-node)
      (declare (ignore object start-node))
      (let* ((a-x   (d (elt a 0)))
             (a-y   (d (elt a 1)))
             (b-x   (d (elt b 0)))
             (b-y   (d (elt b 1)))
             (cost  (d+ (dabs (d- b-x a-x))
                        (dabs (d- b-y a-y)))))
        (d* cost cost-scaling))))

(defun heuristic-alt (landmarks)
  #'(lambda (graph goal-node next-node start-node)
      (declare (ignore graph start-node))
      (loop for mat across landmarks maximize
           (let ((min-cost-next (matrix-elt mat
                                            (2d-utils:seq-y next-node)
                                            (2d-utils:seq-x next-node)))
                 (min-cost-goal (matrix-elt mat
                                            (2d-utils:seq-y goal-node)
                                            (2d-utils:seq-x goal-node))))
             (dabs (d- (d min-cost-next) (d min-cost-goal)))))))

(defun heuristic-alt-pc (game-state)
  (with-accessors ((blackboard blackboard)) game-state
    (with-accessors ((landmarks-dists blackboard:landmarks-dists)) blackboard
      (heuristic-alt landmarks-dists))))

(defun %build-movement-path (graph start end other-costs-layer
                             &key (heuristic-cost-function (heuristic-manhattam)))
  (graph:with-pushed-cost-layer (graph other-costs-layer)
    (let ((tree (graph:astar-search graph
                                    (graph:node->node-id graph start)
                                    (graph:node->node-id graph end)
                                    :heuristic-cost-function heuristic-cost-function)))
      (multiple-value-bind (raw-path cost)
          (graph:graph->path tree (graph:node->node-id graph end))
        (let ((path (map 'vector
                         #'(lambda (id) (graph:node-id->node graph id))
                         raw-path)))
          (values path cost))))))

(defmethod build-movement-path ((object game-state) start end
                                &key
                                  (other-costs-layer '())
                                  (heuristic-cost-function (heuristic-manhattam)))
  (with-accessors ((movement-costs movement-costs)) object
    (%build-movement-path movement-costs start end other-costs-layer
                          :heuristic-cost-function heuristic-cost-function)))

(defmethod build-movement-path-pc ((object game-state) start end
                                   &key
                                     (other-costs-layer '())
                                     (heuristic-cost-function (heuristic-alt-pc object)))
  (with-accessors ((movement-costs-pc movement-costs-pc)) object
    (%build-movement-path movement-costs-pc start end other-costs-layer
                         :heuristic-cost-function heuristic-cost-function)))

(defmethod build-movement-path ((object graph:tile-multilayers-graph) start end
                                &key
                                  (other-costs-layer '())
                                  (heuristic-cost-function (heuristic-manhattam)))
    (graph:with-pushed-cost-layer (object other-costs-layer)
      (let ((tree (graph:astar-search object
                                      (graph:node->node-id object start)
                                      (graph:node->node-id object end)
                                      :heuristic-cost-function heuristic-cost-function)))
        (multiple-value-bind (raw-path cost)
            (graph:graph->path tree (graph:node->node-id object end))
          (let ((path (map 'vector
                           #'(lambda (id) (graph:node-id->node object id))
                           raw-path)))
            (values path cost))))))

(defmethod terrain-aabb-2d ((object game-state))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-accessors ((map-state map-state)) object
    (vec4 0.0
          0.0
          (d* (d (the fixnum (width  map-state))) +terrain-chunk-tile-size+)
          (d* (d (the fixnum (height map-state))) +terrain-chunk-tile-size+))))

(defmethod push-entity ((object game-state) entity)
  (with-accessors ((all-entities all-entities)) object
    (setf all-entities (bs-tree:insert all-entities
                                       entity
                                       :equal     #'=
                                       :compare   #'<
                                       :key-datum #'id
                                       :key       #'id))))

(defmethod push-trap-entity ((object game-state) entity)
  (let* ((pos (map-utils:pos->game-state-pos entity)))
    (if (empty@pos-p object (ivec2-x pos) (ivec2-y pos))
        (progn
          (push-entity object entity)
          (setup-map-state-entity object entity +trap-type+ nil))
        (let ((map-state-element (element-mapstate@ object
                                                    (ivec2-x pos)
                                                    (ivec2-y pos))))
          ;; if not empty,  "slide" the trap under  the entity already
          ;; placed on the tile
          (push-entity object entity)
          (insert-state-chain-after map-state-element
                                    0
                                    entity
                                    +trap-type+
                                    nil))))) ;; nil as traps does not occlude the view

(defmethod pop-trap-entity ((object game-state) entity)
    (let* ((pos (map-utils:pos->game-state-pos entity))
           (map-state-element (element-mapstate@ object
                                               (elt pos 0)
                                               (elt pos 1))))
      (setf (old-state-element map-state-element)
            (old-state-element (old-state-element map-state-element)))))

(defmethod pop-map-state-entity ((object game-state) entity)
  (let* ((pos (map-utils:pos->game-state-pos entity))
         (map-state-element (element-mapstate@ object
                                               (elt pos 0)
                                               (elt pos 1))))
    (setf (matrix-elt (map-state object) (elt pos 1) (elt pos 0))
          (old-state-element map-state-element))))

(defmethod push-labyrinth-entity ((object game-state) labyrinth)
  (setf (gethash (id labyrinth) (labyrinth-entities object)) labyrinth))

(defmethod find-labyrinth-by-id ((object game-state) labyrinth-id)
  (gethash labyrinth-id (labyrinth-entities object)))

(defmethod find-entity-by-id ((object game-state) id)
  (with-accessors ((all-entities all-entities)) object
    (bs-tree:data (bs-tree:search all-entities
                                  id
                                  :equal     #'=
                                  :compare   #'<
                                  :key       #'id
                                  :key-datum #'identity))))

(defmethod remove-entity-by-id ((object game-state) id)
  (with-accessors ((all-entities all-entities)) object
    (setf all-entities (rb-tree:remove-node all-entities
                                            id
                                            :equal     #'=
                                            :compare   #'<
                                            :key       #'id
                                            :key-datum #'identity))))

(defmethod map-level ((object game-state))
  (truncate (/ (+ (level-difficult object)
                  (1+ (* 8 (num:smoothstep-interpolate (d +minimum-map-size+)
                                                       (d +maximum-map-size+)
                                                       (d (width (map-state object)))))))
               2)))

(defmethod add-to-player-entities ((object game-state) entity)
  (with-accessors ((player-entities player-entities)) object
    (push entity player-entities)))

(defmethod add-to-ai-entities ((object game-state) entity)
  (with-accessors ((ai-entities ai-entities)) object
    (push entity ai-entities)))

(defmethod fetch-from-player-entities ((object game-state) id-entity)
  (with-accessors ((player-entities player-entities)) object
    (find id-entity player-entities :test #'= :key #'id)))

(defmethod fetch-from-ai-entities ((object game-state) id-entity)
  (with-accessors ((ai-entities ai-entities)) object
    (find id-entity ai-entities :test #'= :key #'id)))

(defmacro map-entities (entities fn clause)
  (let ((legal-clause '(:do :collect)))
    (when (not (find clause legal-clause :test #'eq))
      (error (format nil "clause must be one of ~s" legal-clause)))
    (alexandria:with-gensyms (entity)
      `(loop for ,entity in ,entities when (not (entity:entity-dead-p ,entity)) ,clause
            (funcall ,fn ,entity)))))

(defmethod map-player-entities ((object game-state) function)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (function function))
  (with-accessors ((player-entities player-entities)) object
    #+ keep-dead-player (mapcar function player-entities)
    #- keep-dead-player (map-entities player-entities function :collect)))

(defmethod loop-player-entities ((object game-state) function)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (function function))
  (with-accessors ((player-entities player-entities)) object
    #+ keep-dead-player (mapcar function player-entities)
    #- keep-dead-player (map-entities player-entities function :do)))

(defmethod map-ai-entities ((object game-state) function)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (function function))
  (with-accessors ((ai-entities ai-entities)) object
    #+ keep-dead-player (mapcar function ai-entities)
    #- keep-dead-player (map-entities ai-entities function :collect)))

(defmethod loop-ai-entities ((object game-state) function)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (function function))
  (with-accessors ((ai-entities ai-entities)) object
    #+ keep-dead-player (mapcar function ai-entities)
    #- keep-dead-player (map-entities ai-entities function :do)))

(defun faction->map-faction-fn (faction)
   (if (eq faction +npc-type+)
       #'map-ai-entities
       #'map-player-entities))

(defun faction->loop-faction-fn (faction)
   (if (eq faction +npc-type+)
       #'loop-ai-entities
       #'loop-player-entities))

(defgeneric faction->opposite-faction (object))

(defmethod faction->opposite-faction ((object entity))
  (faction->opposite-faction (my-faction object)))

(defmethod faction->opposite-faction ((object symbol))
  (if (eq object +npc-type+)
       +pc-type+
       +npc-type+))

(defgeneric opposite-faction-map-fn (object))

(defmethod opposite-faction-map-fn ((object symbol))
  (let* ((opposite-faction (faction->opposite-faction object)))
    (faction->map-faction-fn opposite-faction)))

(defmethod opposite-faction-map-fn ((object entity))
  (opposite-faction-map-fn (my-faction object)))

(defmethod all-player-id-by-faction ((object game-state) faction)
  (let ((res '())
        (fn  (faction->map-faction-fn faction)))
    (funcall fn object
             #'(lambda (v)
                 (push (id v) res)))
    res))

(defun find-faction-entity-id-by-position (game-state probe faction)
  (flet ((fn (entity)
           (let ((pos (mesh:calculate-cost-position entity)))
             (when (ivec2:ivec2= pos probe)
               (return-from find-faction-entity-id-by-position (id entity))))))
    (ecase faction
        (:player
         (loop-player-entities game-state #'fn))
        (:ai
         (loop-ai-entities game-state #'fn)))
    nil))

(defun find-player-id-by-position (game-state probe)
  (find-faction-entity-id-by-position game-state probe :player))

(defun find-ai-id-by-position (game-state probe)
  (find-faction-entity-id-by-position game-state probe :ai))

(defmethod faction-player-p ((object game-state) &optional (id-entity nil))
  (assert (numberp id-entity))
  (fetch-from-player-entities object id-entity))

(defmethod faction-player-p ((object symbol) &optional id-entity)
  (declare (ignore id-entity))
  (eq object game-state:+pc-type+))

(defmethod faction-ai-p ((object game-state) &optional (id-entity nil))
  (assert (numberp id-entity))
  (fetch-from-ai-entities object id-entity))

(defmethod faction-ai-p ((object symbol) &optional id-entity)
  (declare (ignore id-entity))
  (eq object game-state:+npc-type+))

(defmethod approx-terrain-height@pos ((object game-state) x z)
  "x and z in world space"
  (let ((world-ref (fetch-world object)))
    (world:pick-height-terrain world-ref x z)))

(defmethod place-player-on-map ((object game-state) player faction
                                &key (position #(0 0)) (force-position-p nil))

  (with-accessors ((map-state map-state)) object
    (let ((stop nil)
          (player-coordinates (copy-ivec2 position)))
      (labels ((%place-player (pos)
                 (when (not stop)
                   (let* ((next-tiles (misc:shuffle (gen-neighbour-position (elt pos 0)
                                                                            (elt pos 1))))
                          (empty-tiles (remove-if-not #'(lambda (pos)
                                                          (let ((x (elt pos 0))
                                                                (y (elt pos 1)))
                                                            (with-check-matrix-borders (map-state x y)
                                                              (map-element-empty-p (matrix:matrix-elt map-state y x)))))
                                                      next-tiles)))
                     (if empty-tiles
                         (progn
                           (setf stop t)
                           (setf player-coordinates (elt empty-tiles 0)))
                         (loop for i in next-tiles do
                              (let ((x (elt i 0))
                                    (y (elt i 1)))
                                (with-check-matrix-borders (map-state x y)
                                  (%place-player i)))))))))
        (when (not force-position-p)
          (%place-player position))
        (setf (entity:pos player)
              (sb-cga:vec (map-utils:coord-map->chunk (d (elt player-coordinates 0)))
                          (num:d+ 1.5 ; hardcoded :(  to be removed soon
                                  (approx-terrain-height@pos object
                                                             (map-utils:coord-map->chunk
                                                              (d (elt player-coordinates 0)))
                                                             (map-utils:coord-map->chunk
                                                              (d (elt player-coordinates 1)))))
                          (map-utils:coord-map->chunk (d (elt player-coordinates 1)))))
        (set-invalicable-cost-player-layer@ object
                                            (elt player-coordinates 0)
                                            (elt player-coordinates 1))
        (if (eq faction +pc-type+)
            (progn
              (md2:attach-orb player)
              (add-to-player-entities object player))
            (let ((pos-entity (mesh:calculate-cost-position player)))
              (add-to-ai-entities     object player)
              (set-tile-visited object player (ivec2-x pos-entity) (ivec2-y pos-entity))))
        ;; update visibility, keep the last here as to works the hook to an update-visibility
        ;; event make a lookup in (ai|player)-entities.
        ;; see: md2-mesh:on-game-event ((object md2-mesh) (event update-visibility))
        (absee-mesh:update-visibles-state player)))))

(defmethod update-rendering-needed-ai ((object game-state))
  "set rendering needed for all the visibles AI's pawn"
  (loop-player-entities object
       #'(lambda (a) (absee-mesh:update-visibility-cone a)))
  (let ((all-ids-ai-visibles (blackboard:all-ai-id-visible-from-player object)))
    (loop for id in all-ids-ai-visibles do
         (let ((opponent-entity (find-entity-by-id object id)))
           (setf (mesh:renderp opponent-entity) t))))
  object)

(defmethod update-all-visibility-state ((object game-state))
  (flet ((update-fn (a)
           (absee-mesh:update-visibles-state a)))
    (loop-player-entities object #'update-fn)
    (loop-ai-entities     object #'update-fn)))

(defmacro with-set-cost-matrix@ (state mat x y value)
  `(with-accessors ((,mat ,mat)) ,state
     (setf (matrix-elt ,mat ,y ,x) ,value)))

(defmethod set-invalicable-cost-player-layer@ ((object game-state) x y)
  (with-set-cost-matrix@ object costs-from-players x y +invalicable-element-cost+))

(defmethod set-invalicable-cost-map-layer@ ((object game-state) x y)
  (with-set-cost-matrix@ object costs-from-map x y +invalicable-element-cost+))

(defmethod set-invalicable-cost-pc-layer@ ((object game-state) x y)
  (with-set-cost-matrix@ object costs-from-pc x y +invalicable-element-cost+))

(defmethod set-minimum-cost-map-layer@ ((object game-state) x y)
  (with-set-cost-matrix@ object costs-from-map x y +open-terrain-cost+))

(defmethod set-minimum-cost-player-layer@ ((object game-state) x y)
  (with-set-cost-matrix@ object costs-from-players x y +minimum-player-layer-cost+))

(defmethod set-minimum-cost-pc-layer@ ((object game-state) x y)
  (with-set-cost-matrix@ object costs-from-pc x y +open-terrain-cost+))

(defmethod set-map-state-type ((object game-state) x y type)
  (setf (el-type (matrix:matrix-elt (map-state object) y x)) type))

(defmethod set-map-state-id ((object game-state) x y id)
  (setf (entity-id (matrix:matrix-elt (map-state object) y x)) id))

(defmethod set-map-state-occlusion ((object game-state) x y occlusion-value)
  (setf (occlude (matrix:matrix-elt (map-state object) y x)) occlusion-value))

(defmethod setup-map-state-entity ((object game-state) entity type occlusion-value)
  (with-accessors ((pos pos) (id id)) entity
    (let ((x-matrix (map-utils:coord-chunk->matrix (elt pos 0)))
          (y-matrix (map-utils:coord-chunk->matrix (elt pos 2))))
      (set-map-state-type      object x-matrix y-matrix type)
      (set-map-state-id        object x-matrix y-matrix id)
      (set-map-state-occlusion object x-matrix y-matrix occlusion-value))))

(defmethod move-map-state-entity ((object game-state) entity from)
  (let* ((old-x            (elt from 0))
         (old-y            (elt from 1))
         (old-type         (el-type-in-pos  object old-x old-y))
         (old-occlusion    (occludep-in-pos object old-x old-y))
         (target-state-pos (mesh:calculate-cost-position entity))
         (target-x         (elt target-state-pos 0))
         (target-y         (elt target-state-pos 1)))
    ;; set tile coming from to the saved
    (setf (matrix:matrix-elt (map-state object) old-y old-x)
          (clone (old-state-element (matrix:matrix-elt (map-state object) old-y old-x))))
    ;; set saved tile going with the value from the old
    (setf (old-state-element (matrix:matrix-elt (map-state object) target-y target-x))
          (clone (matrix:matrix-elt (map-state object) target-y target-x)))
    ;; finally set the value of the tile with the entity
    (setup-map-state-entity  object entity old-type old-occlusion)))

(defmethod clean-map-state-entity ((object game-state) coord)
  "Coord is #(x y), #(z x) actually"
  (with-accessors ((map-state map-state)) object
    (let* ((x    (ivec2-x coord))
           (y    (ivec2-y coord))
           (tile (make-instance 'map-state-element :occlude nil)))
      (setf (matrix-elt map-state y x) tile))))

(defmethod get-neighborhood ((object game-state) row column predicate
                             &key
                               (w-offset 1) (h-offset 1)
                               (neigh-fn #'gen-neighbour-position-in-box))
  "Return (map-element . position-of-entity)"
  (with-accessors ((map-state map-state)) object
    (let ((pos (remove-if-not
                #'(lambda (a) (element@-inside-p map-state (elt a 0) (elt a 1)))
                (funcall neigh-fn column row w-offset h-offset :add-center nil)))
          (results (misc:make-fresh-array 0 nil 'map-state-element nil)))
      (loop for i across pos do
           (let ((element (matrix-elt map-state (elt i 1) (elt i 0))))
             (when (funcall predicate element i)
               (vector-push-extend (cons element i) results))))
      results)))

(defmethod neighborhood-by-type ((object game-state) row column type
                                 &key
                                   (w-offset 1) (h-offset 1))
  (get-neighborhood object row column
                    #'(lambda (el pos)
                        (declare (ignore pos))
                        (or (null type)
                            (eq (el-type el) type)))
                    :w-offset w-offset :h-offset h-offset))

(defmethod path-same-ends-p ((object game-state) start end)
  (with-accessors ((selected-path selected-path)) object
    (when selected-path
      (let ((tiles (tiles selected-path)))
        (ivec2:ivec2= (alexandria:first-elt tiles) start)
        (ivec2:ivec2= (alexandria:last-elt  tiles) end)))))

(defmethod turn-on-fog ((object game-state))
  (setf (fog-density object) +density-fog+))

(defmethod turn-off-fog ((object game-state))
  (setf (fog-density object) +density-no-fog+))

(defmethod entity-next-p ((object game-state) (me entity) (other entity))
  (let* ((position-matrix (map-utils:pos-entity-chunk->cost-pos (pos me))))
    (not (misc:vector-empty-p  (get-neighborhood object
                                                 (elt position-matrix 1)
                                                 (elt position-matrix 0)
                                                 #'(lambda (e p)
                                                     (declare (ignore p))
                                                     (= (entity-id e) (id other))))))))

(defmethod faction-turn ((object game-state))
  (with-accessors ((game-turn game-turn)) object
    (if (= (rem game-turn 2) 0)
        +pc-type+
        +npc-type+)))

(defmethod faction-turn-human-p ((object game-state))
  (eq (faction-turn object) +pc-type+))

(defmethod faction-turn-ai-p ((object game-state))
  (not (faction-turn-human-p object)))

(defmethod set-tile-visited ((object game-state) entity-visiting x y &key (update-infos nil))
  (with-accessors ((blackboard blackboard)) object
    (set-tile-visited blackboard entity-visiting x y :update-infos update-infos)))

(defmethod set-concerning-tile ((object game-state) x y
                                &key
                                  (danger-zone-size (let* ((level (level-difficult object)))
                                                      (blackboard:calc-danger-zone-size level)))
                                  (concerning-tile-value nil))
  (assert concerning-tile-value)
  (with-accessors ((blackboard blackboard)) object
    (set-concerning-tile blackboard  x y
                         :danger-zone-size danger-zone-size
                         :concerning-tile-value concerning-tile-value)))

(defmethod set-concerning-tile-fn ((object game-state) x y
                                &key
                                  (danger-zone-size (let* ((level (level-difficult object)))
                                                      (blackboard:calc-danger-zone-size level)))
                                  (concerning-tile-fn
                                   (blackboard:concerning-tile-default-mapping-clsr x y
                                                                                    danger-zone-size)))
  (with-accessors ((blackboard blackboard)) object
    (set-concerning-tile-fn blackboard  x y
                            :danger-zone-size danger-zone-size
                            :concerning-tile-fn concerning-tile-fn)))

(defun max-movement-points (game-state iterator-fn)
  (let ((all-mp (funcall iterator-fn
                         game-state
                         #'(lambda (v)
                             (character:actual-movement-points (ghost v))))))
    (find-max all-mp)))

(defmethod max-ai-movement-points ((object game-state))
  "Note: map-ai-entities will skip death characters"
  (max-movement-points object #'map-ai-entities))

(defmethod position-inside-room-p ((object game-state) position)
  (with-accessors ((map-state map-state)) object
    (flet ((node-equals    (a b) (ivec2:ivec2= a b))
           (key-fn         (a)   (identity a))
           (gen-first-near (g visited-node)
             (let ((all (map 'list
                             #'ivec2:sequence->ivec2
                             (matrix:gen-4-neighbour-counterclockwise (elt visited-node 0)
                                                                      (elt visited-node 1)
                                                                      :add-center nil))))
               (remove-if #'(lambda (pos)
                              (game-state:map-element-occupied-by-invalicable-p
                               (matrix:matrix-elt g
                                                  (elt pos 1)
                                                  (elt pos 0))
                               :include-door t))
                          all)))
           (escapedp (node) (if (matrix:pos@border-p map-state node)
                                (return-from position-inside-room-p nil)
                                node)))
      (let ((element (element-mapstate@ object (elt position 0) (elt position 1))))
        (if (eq (game-state:el-type element) +floor-type+)
            (graph:gen-bfs-visit-block (map-state position
                                                  node-equals
                                                  key-fn escapedp
                                                  gen-first-near
                                                  res)
              t)
            nil)))))

(defmethod calc-ai-entities-action-order ((object game-state))
  (calc-ai-entities-action-order (blackboard object)))

(defmethod door-in-next-path-tile-p ((object game-state) path idx-pos-maybe-door)
  (if (< (length path) (1+ idx-pos-maybe-door)) ;; the path is too short
      nil
      (let ((pos-maybe-door (elt path idx-pos-maybe-door)))
        (2d-utils:displace-2d-vector (pos-maybe-door x y)
          (if (door@pos-p       object x y)
              (entity-id-in-pos object x y)
              nil)))))

(defmethod invalicable-in-next-path-tile-p ((object game-state) entity path idx)
  (if (< (length path) (1+ idx)) ;; the path is too short
      nil
      (let ((pos (elt path idx)))
        (2d-utils:displace-2d-vector (pos x y)
          (let ((cost (get-cost object x y)))
            (< (character:current-movement-points (ghost entity))
               cost))))))

(defmethod skydome-bottom-color ((object game-state))
  (pixmap:skydome-bottom-color (game-hour object)))

(defmethod remove-entity-from-all-attack-pos ((object game-state) entity)
  (remove-entity-from-all-attack-pos (blackboard object) entity))

(defmethod clean-characters-logs ((object game-state) trigger)
  (loop-ai-entities object #'(lambda (a)
                               (ai-logger:clean-log (ghost a) trigger))))

(defmethod reset-costs-from-pc ((object game-state))
  (with-accessors ((costs-from-pc   costs-from-pc)
                   (player-entities player-entities)
                   (ai-entities     ai-entities)) object
    (ploop-matrix (costs-from-pc x y)
      (setf (matrix-elt costs-from-pc y x) +minimum-player-layer-cost+))
    (flet ((set-cost (e)
             (let ((pos (calculate-cost-position e)))
               (set-invalicable-cost-pc-layer@ object (ivec2:ivec2-x pos) (ivec2:ivec2-y pos)))))
      (map nil #'(lambda (a) (set-cost a)) player-entities)
      (map nil #'(lambda (a)
                   ;; in this  context it means: "is  visible?"  note:
                   ;; faint character  *are* always visibles  but just
                   ;; because this test has been choosen!
                   (when (mesh:renderp a)
                     (set-cost a)))
           ai-entities)))
  object)

(defmethod get-costs-from-pc@ ((object game-state) x y)
  (with-accessors ((costs-from-pc costs-from-pc)) object
    (matrix-elt costs-from-pc y x)))

(defmethod fetch-all-entity-in-map-by-type ((object game-state) (type symbol))
  (with-accessors ((map-state map-state)) object
    (let ((all-ids (filter-matrix-data map-state #'(lambda (a)
                                                     (let ((el-type (el-type a)))
                                                       (not (eq type el-type)))))))
      (map 'list #'(lambda (a) (find-entity-by-id object (entity-id a))) all-ids))))

(defmethod fetch-all-traps ((object game-state))
  (fetch-all-entity-in-map-by-type object +trap-type+))

(defmethod fetch-all-containers ((object game-state))
  (fetch-all-entity-in-map-by-type object +container-type+))

(defmethod fetch-all-magic-furnitures ((object game-state))
  (fetch-all-entity-in-map-by-type object +magic-furniture-type+))

(defmethod fetch-all-doors ((object game-state))
  (with-accessors ((map-state map-state)) object
    (let* ((all-ids (filter-matrix-data map-state
                                        #'(lambda (a)
                                            (let ((type (el-type a)))
                                              (and (not (eq type +door-n-type+))
                                                   (not (eq type +door-s-type+))
                                                   (not (eq type +door-w-type+))
                                                   (not (eq type +door-e-type+)))))))
           (res     (map 'list #'(lambda (a)
                                   (find-entity-by-id object (entity-id a)))
                         all-ids)))
      res)))

(defun increase-game-turn (state)
  (let ((end-event   (make-instance 'game-event:end-turn
                                    :end-turn-count   (game-turn    state)
                                    :end-turn-faction (faction-turn state)))
        (start-event (make-instance 'game-event:start-turn)))
    (game-event:propagate-end-turn   end-event)
    (game-event:propagate-start-turn start-event)))
