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

(alexandria:define-constant +start-day+     6                :test #'=)

(alexandria:define-constant +end-day+      20                :test #'=)

(alexandria:define-constant +start-night+  21                :test #'=)

(alexandria:define-constant +end-night+     5                :test #'=)

(alexandria:define-constant +zenith-night+ 23                :test #'=)

(alexandria:define-constant +zenith-day+   13                :test #'=)

(alexandria:define-constant +yellow-light-color+  §cffff99ff :test #'vec4~)

(alexandria:define-constant +white-light-color+   §cffffffff :test #'vec4~)

(alexandria:define-constant +blueish-light-color+ §c7ba8e4ff :test #'vec4~)

(alexandria:define-constant +density-no-fog+      0.0        :test #'=)

(alexandria:define-constant +density-fog+         0.006      :test #'=)

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
    :accessor old-state-element)))

(defmethod initialize-instance :after ((object map-state-element) &key &allow-other-keys)
  (when (not (eq (old-state-element object) :stopper))
    (let ((saved (make-instance 'map-state-element :old-state-element :stopper)))
      (clone-into object saved)
      (setf (old-state-element object) saved))))

(defmethod clone-into ((from map-state-element) (to map-state-element))
  (setf (entity-id to)         (entity-id from)
        (el-type   to)         (el-type   from)
        (occlude   to)         (occludep  from)))

(defmethod clone ((object map-state-element))
  (with-simple-clone (object 'map-state-element)))

(defgeneric map-element-empty-p (object))

(defgeneric map-element-occupied-by-character-p (object))

(defgeneric neighborhood-by-type (object row column type &key w-offset h-offset))

(defgeneric get-neighborhood (object row column predicate &key w-offset h-offset))

(defgeneric insert-state-chain-after (object idx entity type occlusion-value &optional ct))

(defmethod map-element-empty-p ((object map-state-element))
  (or (eq (el-type object)
          +empty-type+)
      (eq (el-type object)
          +floor-type+)))

(defmethod map-element-occupied-by-character-p ((object map-state-element))
  "t if occupied by either player or ai (npc) character"
  (or (eq (el-type object)
          +pc-type+)
      (eq (el-type object)
          +npc-type+)))

(defmethod insert-state-chain-after ((object map-state-element)
                          idx entity type occlusion-value
                          &optional (ct 0))
  (with-accessors ((old-state-element old-state-element)) object
    (if (= ct idx)
        (let ((new (make-instance 'map-state-element
                                  :entity-id         (id entity)
                                  :el-type           type
                                  :occlude           occlusion-value
                                  :old-state-element :stopper)))
          (setf (old-state-element new) (old-state-element object))
          (setf (old-state-element object) new))
        (insert-state-chain-after (old-state-element object)
                                  idx entity type occlusion-value (1+ ct)))))

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
  ((game-hour
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
    :type graph:tile-multilayers-graph)
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
    :initform 4)
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
    :initform (make-hash-table :test 'equal)
    :initarg  :player-entities
    :accessor player-entities
    :type     :md2-mesh)
   (ai-entities
    :initform (make-hash-table :test 'equal)
    :initarg  :ai-entities
    :accessor ai-entities
    :type     :md2-mesh)
   (selected-pc
    :initform nil
    :initarg  :selected-pc
    :accessor selected-pc)
   (selected-path
    :initform nil
    :initarg  :selected-path
    :accessor selected-path)
   (blackboard
    :initform nil
    :initarg  :blackboard
    :accessor blackboard
    :type blackboard:blackboard)))

(defgeneric fetch-render-window (object))

(defgeneric fetch-world (object))

(defgeneric setup-game-hour (object hour))

(defgeneric prepare-map-state (object map))

(defgeneric el-type-in-pos (object x y))

(defgeneric entity-id-in-pos (object x y))

(defgeneric build-movement-path (object start end &key other-costs-layer))

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

(defgeneric map-ai-entities (object function))

(defgeneric terrain-height@pos (object x z))

(defgeneric place-player-on-map (object player faction &optional position))

(defgeneric set-invalicable-cost-player-layer@ (object x y))

(defgeneric set-invalicable-cost-map-layer@ (object x y))

(defgeneric set-minimum-cost-map-layer@ (object x y))

(defgeneric set-minimum-cost-player-layer@ (object x y))

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

(defgeneric make-influence-map (object))

(defgeneric set-tile-visited (object x y))

(defgeneric set-concerning-tile (object x y &key danger-zone-size))

(defgeneric max-ai-movement-points (object))

(defmethod fetch-render-window ((object game-state))
  (and (window-id object)
       (sdl2.kit-utils:fetch-window (window-id object))))

(defmacro with-world ((world object) &body body)
  `(let ((,world (fetch-world ,object)))
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

(defgeneric element-mapstate@ (object x y))

(defgeneric door@pos-p (object x y))

(defmethod element-mapstate@ ((object game-state) (x fixnum) (y fixnum))
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (matrix-elt (map-state object) y x))

(defmethod door@pos-p ((object game-state) (x fixnum) (y fixnum))
  (or (eq (el-type-in-pos object x y) +door-n-type+)
      (eq (el-type-in-pos object x y) +door-s-type+)
      (eq (el-type-in-pos object x y) +door-w-type+)
      (eq (el-type-in-pos object x y) +door-e-type+)))

(defmethod prepare-map-state ((object game-state) (map random-terrain))
  (with-accessors ((map-state map-state)) object
     (setf map-state
          (gen-matrix-frame (truncate (* (width  (matrix map)) +terrain-chunk-size-scale+))
                            (truncate (* (height (matrix map)) +terrain-chunk-size-scale+))))
     (loop for i from 0 below (length (data map-state)) do
          (setf (elt (data map-state) i) (make-instance 'map-state-element)))))

(defun heuristic-manhattam ()
  #'(lambda (object a b start-node)
      (declare (ignore object))
      (let* ((a-x   (d (elt a 0)))
             (a-y   (d (elt a 1)))
             (b-x   (d (elt b 0)))
             (b-y   (d (elt b 1)))
             (s-x   (d (elt start-node 0)))
             (s-y   (d (elt start-node 1)))
             (cost  (d+ (dabs (d- b-x a-x))
                        (dabs (d- b-y a-y))))
             (dx1   (d- b-x a-x))
             (dy1   (d- b-y a-y))
             (dx2   (d- s-x a-x))
             (dy2   (d- s-y a-y))
             (cross (abs (d- (d* dx1 dy2) (d* dx2 dy1)))))
        (d+ cost (d* cross 0.05)))))

(defmethod build-movement-path ((object game-state) start end &key (other-costs-layer '()))
  (with-accessors ((movement-costs movement-costs)) object
    (graph:with-pushed-cost-layer (movement-costs other-costs-layer)
      (let ((tree (graph:astar-search movement-costs
                                      (graph:node->node-id movement-costs start)
                                      (graph:node->node-id movement-costs end)
                                      :heuristic-cost-function (heuristic-manhattam))))
        (multiple-value-bind (raw-path cost)
            (graph:graph->path tree (graph:node->node-id movement-costs end))
          (let ((path (map 'vector
                           #'(lambda (id) (graph:node-id->node movement-costs id))
                           raw-path)))
            (values path cost)))))))

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
  (push-entity object entity)
  (let* ((pos (map-utils:pos->game-state-pos entity))
         (map-state-element (element-mapstate@ object
                                               (elt pos 0)
                                               (elt pos 1))))
    (insert-state-chain-after map-state-element
                              0
                              entity
                              +trap-type+
                              nil))) ;; nil as traps does not occlude the view

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
                  (1+ (* 8 (num:smoothstep-interpolate (d +minimium-map-size+)
                                                       (d +maximum-map-size+)
                                                       (d (width (map-state object)))))))
               2)))

(defmethod add-to-player-entities ((object game-state) entity)
  (with-accessors ((player-entities player-entities)) object
    ;; test
    (character:reset-movement-points (ghost entity))
    (setf (gethash (id entity) player-entities) entity)))

(defmethod add-to-ai-entities ((object game-state) entity)
  (with-accessors ((ai-entities ai-entities)) object
    (setf (gethash (id entity) ai-entities) entity)))

(defmethod fetch-from-player-entities ((object game-state) id-entity)
  (with-accessors ((player-entities player-entities)) object
    (gethash id-entity player-entities)))

(defmethod fetch-from-ai-entities ((object game-state) id-entity)
  (with-accessors ((ai-entities ai-entities)) object
    (gethash id-entity ai-entities)))

(defmethod map-player-entities ((object game-state) function)
  (with-accessors ((player-entities player-entities)) object
    (maphash function player-entities)))

(defmethod map-ai-entities ((object game-state) function)
  (with-accessors ((ai-entities ai-entities)) object
    (maphash function ai-entities)))

(defmethod faction-player-p ((object game-state) &optional (id-entity nil))
  (assert (numberp id-entity))
  (fetch-from-player-entities object id-entity))

(defmethod faction-ai-p ((object game-state) &optional (id-entity nil))
  (assert (numberp id-entity))
  (fetch-from-ai-entities object id-entity))

(defmethod approx-terrain-height@pos ((object game-state) x z)
  "x and z in world space"
  (let ((world-ref (fetch-world object)))
    (world::pick-height-terrain world-ref x z)))

(defmethod place-player-on-map ((object game-state) player faction &optional (pos #(0 0)))
  (with-accessors ((map-state map-state)) object
    (let ((stop nil)
          (player-coordinates nil))
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
        (%place-player pos)
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
            (add-to-player-entities object player)
            (let ((pos-entity (mesh:calculate-cost-position player)))
              (add-to-ai-entities     object player)
              (set-tile-visited object (elt pos-entity 0) (elt pos-entity 1))))))))

(defmethod set-invalicable-cost-player-layer@ ((object game-state) x y)
  (with-accessors ((costs-from-players costs-from-players)) object
    (setf (matrix-elt costs-from-players y x) +invalicable-element-cost+)))

(defmethod set-invalicable-cost-map-layer@ ((object game-state) x y)
  (with-accessors ((costs-from-map costs-from-map)) object
    (setf (matrix-elt costs-from-map y x) +invalicable-element-cost+)))

(defmethod set-minimum-cost-map-layer@ ((object game-state) x y)
  (with-accessors ((costs-from-map costs-from-map)) object
    (setf (matrix-elt costs-from-map y x) +open-terrain-cost+)))

(defmethod set-minimum-cost-player-layer@ ((object game-state) x y)
  (with-accessors ((costs-from-players costs-from-players)) object
    (setf (matrix-elt costs-from-players y x) +minimum-player-layer-cost+)))

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
    (let* ((x    (elt coord 0))
           (y    (elt coord 1))
           (tile (make-instance 'map-state-element :occlude nil)))
      (setf (matrix-elt map-state y x) tile))))

(defmethod get-neighborhood ((object game-state) row column predicate
                             &key (w-offset 2) (h-offset 2))
  (with-accessors ((map-state map-state)) object
    (let ((pos (remove-if-not
                #'(lambda (a) (element@-inside-p map-state (elt a 0) (elt a 1)))
                (gen-neighbour-position-in-box column row w-offset h-offset :add-center nil)))
          (results (misc:make-fresh-array 0 nil 'map-state-element nil)))
      (loop for i across pos do
           (let ((element (matrix-elt map-state (elt i 1) (elt i 0))))
             (when (funcall predicate element i)
               (vector-push-extend (cons element i) results))))
      results)))

(defmethod neighborhood-by-type ((object game-state) row column type
                                 &key
                                   (w-offset 2) (h-offset 2))
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

(defmethod make-influence-map ((object game-state))
  (with-accessors ((player-entities player-entities)
                   (map-state map-state)
                   (ai-entities ai-entities)) object
    (let* ((im (make-matrix (width map-state) (height map-state) 0.0)))
      (inmap:apply-influence im player-entities)
      (inmap:apply-influence im ai-entities)
      im)))

(defmethod set-tile-visited ((object game-state) x y)
  (with-accessors ((blackboard blackboard)) object
    (set-tile-visited blackboard  x y)))

(defmethod set-concerning-tile ((object game-state) x y
                                &key (danger-zone-size (let* ((level (level-difficult object)))
                                                         (blackboard:calc-danger-zone-size level))))
  (with-accessors ((blackboard blackboard)) object
    (set-concerning-tile blackboard  x y :danger-zone-size danger-zone-size)))

(defun max-movement-points (game-state iterator-fn)
  (let ((all-mp '()))
    (funcall iterator-fn game-state #'(lambda (k v)
                                        (declare (ignore k))
                                        (push (character:actual-movement-points (ghost v)) all-mp)))
    (find-max all-mp)))

(defmethod max-ai-movement-points ((object game-state))
  (max-movement-points object #'map-ai-entities))
