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

(in-package :world)

(alexandria:define-constant +camera-point-to-entity-dir-scale+ (d* +terrain-chunk-tile-size+
                                                        8.0)
  :test #'=)

(defun make-skydome (bg cloud-1 cloud-2 cloud-3 smoke)
  (let ((mesh (gen-skydome 100.0)))
    ;; background
    (setf (texture:interpolation-type bg) :nearest)
    (texture:prepare-for-rendering    bg)
    (setf (texture-object mesh)       bg)
    (setf (texture:s-wrap-mode        bg) :clamp-to-edge)
    (setf (texture:t-wrap-mode        bg) :clamp-to-edge)
    ;; cloud layer 1
    (setf (texture::interpolation-type  cloud-1) :linear)
    (texture:prepare-for-rendering      cloud-1)
    (setf (elt (texture-clouds mesh) 0) cloud-1)
    ;; cloud layer 2
    (setf (texture::interpolation-type  cloud-2) :linear)
    (texture:prepare-for-rendering      cloud-2)
    (setf (elt (texture-clouds mesh) 1) cloud-2)
    ;; cloud layer 3
    (setf (texture::interpolation-type  cloud-3) :linear)
    (texture:prepare-for-rendering      cloud-3)
    (setf (elt (texture-clouds mesh) 2) cloud-3)
    ;; smoke
    (setf (texture:interpolation-type smoke) :linear)
    (setf (texture:s-wrap-mode        smoke) :clamp-to-border)
    (setf (texture:t-wrap-mode        smoke) :clamp-to-border)
    (setf (texture:border-color       smoke) §c00000000)
    (texture:prepare-for-rendering    smoke)
    (setf (texture-projector mesh)    smoke)
    (prepare-for-rendering mesh)
    mesh))

(defclass doors ()
  ((door-n
    :accessor door-n
    :initarg :door-n
    :initform nil)
   (door-s
    :accessor door-s
    :initarg :door-s
    :initform nil)
   (door-e
    :accessor door-e
    :initarg :door-e
    :initform nil)
   (door-w
    :accessor door-w
    :initarg :door-w
    :initform nil)))

(defmethod destroy ((object doors))
  (destroy (door-n object))
  (destroy (door-s object))
  (destroy (door-e object))
  (destroy (door-w object))
  (setf    (door-n object) nil
           (door-s object) nil
           (door-e object) nil
           (door-w object) nil))

(defclass chairs ()
  ((chair-n
    :accessor chair-n
    :initarg :chair-n
    :initform nil)
   (chair-s
    :accessor chair-s
    :initarg :chair-s
    :initform nil)
   (chair-e
    :accessor chair-e
    :initarg :chair-e
    :initform nil)
   (chair-w
    :accessor chair-w
    :initarg :chair-w
    :initform nil)))

(defmethod destroy ((object chairs))
  (destroy (chair-n object))
  (destroy (chair-s object))
  (destroy (chair-e object))
  (destroy (chair-w object))
  (setf    (chair-n object) nil
           (chair-s object) nil
           (chair-e object) nil
           (chair-w object) nil))

(defclass world (transformable renderizable)
  ((camera
    :accessor camera
    :initarg :camera
    :initform (make-instance 'camera :pos (vec 0.0 0.0 1.0)))
   (skydome
    :accessor skydome
    :initarg :skydome
    :initform nil)
   (entities
    :accessor entities
    :initarg :entities
    :initform (make-instance 'quad-tree:quad-tree
                             :aabb (vec4:vec4 0.0 0.0
                                              (num:d +map-max-size+)
                                              (num:d +map-max-size+))))
   (main-state
    :initarg :main-state
    :initform nil
    :allocation :class)
   (actions-queue
    :initarg :actions-queue
    :initform (make-instance 'action-scheduler:action-scheduler)
    :accessor actions-queue)
   (cached-aabb
    :initarg :cached-aabb
    :initform nil
    :reader   cached-aabb)
   (frame-window
    :accessor frame-window
    :initarg :frame-window
    :initform nil)
   (trees-bag
    :accessor trees-bag
    :initarg  :trees-bag
    :initform nil)
   (walls-bag
    :accessor walls-bag
    :initarg  :walls-bag
    :initform nil)
   (floor-bag
    :accessor floor-bag
    :initarg  :floor-bag
    :initform nil)
   (doors-bag
    :accessor doors-bag
    :initarg  :doors-bag
    :initform (make-instance 'doors))
   (chairs-bag
    :accessor chairs-bag
    :initarg  :chairs-bag
    :initform (make-instance 'chairs))
   (furnitures-bag
    :accessor furnitures-bag
    :initarg  :furnitures-bag
    :initform nil)
   (containers-bag
    :accessor containers-bag
    :initarg  :containers-bag
    :initform nil)
   (magic-furnitures-bag
    :accessor magic-furnitures-bag
    :initarg  :magic-furnitures-bag
    :initform nil)
   (windows-bag
    :accessor windows-bag
    :initarg  :windows-bag
    :initform nil)
   (pillars-bag
    :accessor pillars-bag
    :initarg  :pillars-bag
    :initform nil)
   (tables-bag
    :accessor tables-bag
    :initarg  :tables-bag
    :initform nil)
   (wall-decorations-bag
    :accessor wall-decorations-bag
    :initarg  :wall-decorations-bag
    :initform nil)
   (walkable-bag
    :accessor walkable-bag
    :initarg  :walkable-bag
    :initform nil)
   (traps-bag
    :accessor traps-bag
    :initarg  :traps-bag
    :initform nil)
   (toolbar
    :accessor toolbar
    :initarg  :toolbar
    :initform (make-instance 'widget:main-toolbar
                             :x 0.0
                             :y 0.0
                             :width  (num:d *window-w*)
                             :height (num:d *window-h*)))
   (gui
    :accessor gui
    :initarg  :gui
    :initform (make-instance 'widget:widget
                             :x 0.0 :y 0.0
                             :width  *window-w*
                             :height *window-h*
                             :label nil))
   (influence-map-type
     :accessor influence-map-type
     :initarg  :influence-map-type
     :initform :unexplored-layer
     :documentation "just for debugging")))

(defmethod initialize-instance :after ((object world) &key &allow-other-keys)
  (with-accessors ((toolbar       toolbar)
                   (actions-queue actions-queue)) object
    (game-event:register-for-refresh-toolbar-event object)
    (game-event:register-for-update-highlight-path object)
    (game-event:register-for-start-turn            object)
    (game-event:register-for-end-turn              object)
    ;; registering root action-scheduler
    (game-event:register-for-end-turn               actions-queue)
    (game-event:register-for-game-action-terminated actions-queue)
    (setf (widget:bound-world toolbar) object)))

(defmethod actions-queue-empty-p ((object world))
 (actions-queue-empty-p (actions-queue object)))

(defgeneric render-influence-map (object))

(defun type-influence->pixmap (world)
  (with-accessors ((main-state main-state)
                   (influence-map-type influence-map-type)) world
    (with-accessors ((blackboard blackboard)) main-state
      (let ((layer (ecase influence-map-type
                     (:concerning-invalicables
                      (inmap::layer->pixmap
                       (blackboard::concerning-tiles-invalicables blackboard)))
                     (:concerning-facing
                      (inmap::layer->pixmap
                       (blackboard:concerning-tiles-facing blackboard)))
                     (:smoothed-concerning-layer
                      (inmap::layer->pixmap
                       (blackboard:concerning-tiles->costs-matrix blackboard)))
                     (:concerning-layer
                      (inmap::layer->pixmap
                       (blackboard:concerning-tiles blackboard)))
                     (:unexplored-layer
                      (inmap:dijkstra-layer->pixmap
                       (blackboard:unexplored-layer blackboard)))
                     (:attack-enemy-melee-layer
                      (inmap:dijkstra-layer->pixmap
                       (blackboard:attack-enemy-melee-layer blackboard)))
                     (:attack-enemy-pole-layer
                      (inmap:dijkstra-layer->pixmap
                       (blackboard:attack-enemy-pole-layer blackboard)))
                     (:attack-enemy-bow-layer
                      (inmap:dijkstra-layer->pixmap
                       (blackboard:attack-enemy-bow-layer blackboard)))
                     (:attack-enemy-crossbow-layer
                      (inmap:dijkstra-layer->pixmap
                       (blackboard:attack-enemy-crossbow-layer blackboard))))))
        layer))))

(defmethod render-influence-map ((object world))
  (with-accessors ((main-state main-state)
                   (toolbar    toolbar))   object
    (with-accessors ((blackboard blackboard)) main-state
      (let ((texture-map (type-influence->pixmap object)))
        (matrix:v-mirror-matrix texture-map)
        (matrix:h-mirror-matrix texture-map)
        (widget:sync-influence-map toolbar texture-map)))))

(defmethod game-event:on-game-event ((object world)
                                     (event game-event:refresh-toolbar-event))
  (with-accessors ((toolbar toolbar)) object
    (widget:sync-with-player toolbar
                             :reset-health-animation
                             (game-event:reset-health-status-animation-p event)))
  #+ (and debug-mode debug-ai) (render-influence-map object))

(defmethod game-event:on-game-event ((object world)
                                     (event game-event:update-highlight-path))
  (highlight-path-costs-space object object (game-event:tile-pos event)))

(defmethod remove-entity-if ((object world) predicate)
  (with-accessors ((main-state main-state)) object
    (let* ((ids '())
           (fn  #'(lambda (a)
                    (if (funcall predicate a)
                        (progn
                          (push (id a) ids)
                          t)
                        nil))))
      (remove-entity-if (entities object) fn)
      (loop for id in ids do
           (game-state:remove-entity-by-id main-state id))
      object)))

(defmethod faction-turn ((object world))
  (faction-turn (main-state object)))

(defmethod faction-turn-human-p ((object world))
  (eq (faction-turn object) +pc-type+))

(defmethod faction-turn-ai-p ((object world))
  (not (faction-turn-human-p object)))

(defun rebuild-all-ai-planners (world)
  (loop-ai-entities (main-state world)
                   #'(lambda (a)
                       (character:setup-planners (ghost a)))))

(defun add-start-turn-billboard (world)
  (if (faction-turn-human-p world)
      (full-screen-masks:enqueue-turn-billboard-human world)
      (full-screen-masks:enqueue-turn-billboard-ai    world)))

(defmethod game-event:on-game-event ((object world) (event game-event:start-turn))
  (with-accessors ((main-state main-state)) object
    ;(tg:gc :full t)
    (flet ((%process-postponed-messages (entity)
             (mesh:process-postponed-messages entity)))
      (loop-player-entities main-state #'%process-postponed-messages)
      (loop-ai-entities     main-state #'%process-postponed-messages))
    (calc-ai-entities-action-order main-state)
    (add-start-turn-billboard object)
    nil))

(defmethod game-event:on-game-event ((object world) (event game-event:end-turn))
  (with-accessors ((main-state main-state)) object
    ;(tg:gc :full t)
    ;;(misc:dbg " end turn ~a ~a" (type-of object) (type-of event))
    (clear-all-memoized-function-cache)
    (md2-mesh:blacklist-clear-id)
    (rebuild-all-ai-planners        object)
    (remove-all-tooltips            object)
    (remove-all-windows             object)
    (remove-all-removeable          object)
    (remove-all-removeable-from-gui object)
    (clean-characters-logs          object character:+planner-log-clean-end-turn+)
    ;;(remove-entity-if (gui object) #'(lambda (a) (typep a 'widget:message-window)))
    (incf (game-turn (main-state object))) ;; new turn starts here!
    nil))

(defmethod remove-entity-by-id ((object world) id)
  (remove-entity-by-id (main-state object) id) ;; remove from game state (logic)
  (remove-entity-by-id (entities object)   id)) ;; remove from rendering

(defmethod enqueue-action ((object world) (new-action game-action))
  (with-accessors ((actions-queue actions-queue)) object
    (enqueue-action actions-queue new-action)))

(defmethod apply-damage ((object world) damage
                         &key (duration 10.0) &allow-other-keys)
  (with-accessors ((camera camera)) object
    (setf (fading-away-fn camera) (tremor:standard-tremor-fn duration :power damage))))

(defgeneric (setf main-state) (new-state object))

(defgeneric get-window-size (object))

(defgeneric frustum-bounding-sphere-intersects-p (object ent))

(defgeneric frustum-aabb-intersects-p (object ent))

(defgeneric cone-aabb-intersects-p (object ent))

(defgeneric initialize-skydome (object &key bg clouds-1 clouds-2 clouds-3 smoke weather-type))

(defgeneric render-gui (object))

(defgeneric highlight-tile-screenspace (object renderer x y))

(defgeneric highlight-path-costs-space (object renderer path))

(defgeneric pick-player-entity (object renderer x y &key bind))

(defgeneric pick-any-entity (object renderer x y))

(defgeneric pick-height-terrain (object x z))

(defgeneric iterate-quad-tree (object function probe))

(defgeneric iterate-quad-tree-anyway (object function))

(defgeneric all-furniture-bags-not-empty-p  (object))

(defgeneric all-furnitures-but-pillars-not-empty-p (object))

(defgeneric push-interactive-entity (object entity type occlusion-value
                                     &key
                                      add-to-world
                                      add-to-gamestate))

(defgeneric push-terrain-chunk (object entity aabb))

(defgeneric setup-map-state-tile (object x y type id occlusion-value))

(defgeneric set-window-accepts-input (object value))

(defgeneric move-entity (object entity from &key update-costs))

(defgeneric toolbar-selected-action (object))

(defgeneric (setf toolbar-selected-action) (val object))

(defgeneric post-entity-message (object entity text suppress-default-action &rest actions))

(defgeneric point-camera-to-entity (object entity))

(defgeneric add-ai-opponent (object type gender))

(defgeneric world-aabb (object))

(defgeneric remove-all-tooltips (object))

(defgeneric remove-all-windows (object))

(defgeneric remove-all-removeable (object))

(defgeneric remove-all-removeable-from-gui (object))

(defgeneric activate-all-tooltips (object))

(defgeneric clean-characters-logs (object trigger))

(defmethod iterate-quad-tree ((object world) function probe)
  (quad-tree:iterate-nodes-intersect (entities object)
                                    function
                                    probe))

(defmethod iterate-quad-tree ((object world) function probe)
  (quad-tree:iterate-nodes-intersect (entities object)
                                    function
                                    probe))

(defmacro walk-quad-tree ((object &optional (aabb `(aabb-2d (camera ,object)))) &body body)
  `(iterate-quad-tree ,object
                      #'(lambda (entities)
                          (loop named entity-loop for entity across (quad-tree:data entities) do
                               ,@body))
                      ,aabb))

(defmethod iterate-quad-tree-anyway ((object world) function)
  (quad-tree:iterate-nodes (entities object) function))

(defmacro walk-quad-tree-anyway ((object) &body body)
  `(iterate-quad-tree-anyway ,object
                      #'(lambda (entities)
                          (loop named entity-loop for entity across (quad-tree:data entities) do
                               ,@body))))

(defmethod main-state ((object world))
  (slot-value object 'main-state))

(defmethod destroy ((object world))
  (destroy (skydome object))
  (setf    (skydome object) nil)
  (map nil #'destroy (trees-bag object))
  (setf (trees-bag object) nil)
  (destroy (walls-bag object))
  (setf (walls-bag object) nil)
  (destroy (floor-bag object))
  (setf (floor-bag object) nil)
  (destroy (doors-bag object))
  (setf (doors-bag object) nil)
  (map nil #'destroy (furnitures-bag object))
  (setf (furnitures-bag object) nil)
  (map nil #'destroy (containers-bag object))
  (setf (containers-bag object) nil)
  (map nil #'destroy (magic-furnitures-bag object))
  (setf (magic-furnitures-bag object) nil)
  (map nil #'destroy (pillars-bag object))
  (setf (pillars-bag object) nil)
  (destroy (chairs-bag object))
  (setf (chairs-bag object) nil)
  (map nil #'destroy (tables-bag object))
  (setf (tables-bag object) nil)
  (map nil #'destroy (wall-decorations-bag object))
  (setf (wall-decorations-bag object) nil)
  (map nil #'destroy (walkable-bag object))
  (setf (walkable-bag object) nil)
  (destroy (windows-bag object))
  (setf (windows-bag object) nil)
  (destroy (gui object))
  (setf (gui object) nil)
  (quad-tree:iterate-nodes (entities object)
                           #'(lambda (a) (map 'nil #'destroy (quad-tree:data a))))
  (quad-tree:iterate-nodes (entities object)
                           #'(lambda (a) (setf (quad-tree:data a) nil)))
  (setf (entities object) nil))

(defmethod (setf main-state) (new-state (object world))
  (with-slots (main-state skydome camera) object
    (setf main-state new-state)
    (setf (entity:state skydome) new-state)
    (setf (entity:state camera) new-state)))

(defmacro gen-accessors-matrix (name)
  (let ((fn-name (list (alexandria:format-symbol t "~:@(setf~)")
                       (alexandria:format-symbol t "~:@(~a~)" name)))
        (get-name (alexandria:format-symbol t "~:@(~a~)" name)))
    `(defmethod ,fn-name (new-value (object world))
       (with-slots (,name) object
         (setf (elt ,name 0) new-value))
       (setf (,get-name (camera object)) new-value))))

(gen-accessors-matrix model-matrix)

(gen-accessors-matrix view-matrix)

(gen-accessors-matrix projection-matrix)

(defmethod build-projection-matrix ((object world) near far fov ratio)
  (build-projection-matrix (camera object) near far fov ratio))

(defmethod calculate ((object world) dt)
  (incf (current-time (main-state object)) dt)
  (setf (widget:label (widget:text-fps (elt (mtree-utils:children (gui object)) 0)))
        (format nil "~,2f" (main-window:fps (frame-window object))))
  (calculate (gui object) dt)
  (calculate (camera object)  dt)
  (calculate (skydome object) dt)
  (calculate-frustum (camera object))
  (calculate-aabb    (camera object))
  (calculate-cone    (camera object))
  (walk-quad-tree (object)
    (calculate entity dt)))

(defmethod render ((object world) (renderer world))
  (render (camera  object) object)
  (render (skydome object) object)
  (walk-quad-tree (object)
    (when (and (not (use-blending-p entity)))
      (render entity renderer)))
  (walk-quad-tree (object)
    (when (use-blending-p entity)
      (render entity renderer))))

(defmethod render-for-reflection ((object world) (renderer world))
  (render-for-reflection (camera  object) object)
  (render-for-reflection (skydome object) object)
  (walk-quad-tree (object)
    (render-for-reflection entity renderer)))

(defmethod pick-pointer-position ((object world) renderer x y)
  (walk-quad-tree (object)
    (multiple-value-bind (picked cost-matrix-position matrix-position raw-position)
        (pick-pointer-position entity renderer x y)
      (when picked
        (return-from pick-pointer-position
          (values cost-matrix-position matrix-position raw-position)))))
  nil)

(defmethod pick-height-terrain ((object world) x z)
  "x z in world space"
  (walk-quad-tree (object)
    (when (and (typep entity 'terrain-chunk:terrain-chunk)
               (insidep (aabb entity) (vec x +zero-height+ z)))
      (return-from pick-height-terrain (terrain-chunk:approx-terrain-height entity x z))))
  0.0)

(defmethod highlight-tile-screenspace ((object world) renderer x y)
  "Coordinates in screen space"
    (walk-quad-tree (object)
      (multiple-value-bind (picked cost-matrix-position matrix-position raw-position)
          (pick-pointer-position entity renderer x y)
        (declare (ignore raw-position))
        (when picked
          (turn-off-highligthed-tiles object)
          (set-tile-highlight entity
                              (elt matrix-position 1) ; row
                              (elt matrix-position 0) ; column
                              :clear-highligthed-set nil
                              :add-to-highligthed-set t)
          (return-from highlight-tile-screenspace cost-matrix-position))))
    nil)

(defmethod highlight-path-costs-space ((object world) renderer path)
  "Path contains coordinates from cost matrix"
  (declare (ignore renderer))
  (turn-off-highligthed-tiles object)
  (map nil
       #'(lambda (coord)
           (block walking
             (walk-quad-tree-anyway (object)
               (when (pickable-mesh-p entity)
                 (let ((tile-coord (cost-coord->lookup-tile entity
                                                            (elt coord 1)
                                                            (elt coord 0))))
                   (handler-bind ((null-tile-element
                                   #'(lambda(e)
                                       (declare (ignore e))
                                       (invoke-restart 'pickable-mesh::use-value nil)))
                                  (out-of-bonds-tile-element
                                   #'(lambda(e)
                                       (declare (ignore e))
                                       (invoke-restart 'pickable-mesh::use-value nil))))
                     (when (set-tile-highlight entity (elt tile-coord 1) (elt tile-coord 0)
                                               :add-to-highligthed-set t
                                               :clear-highligthed-set nil)
                       (return-from walking))))))))
       path))

(defmethod pick-player-entity ((object world) renderer x y &key (bind nil))
  "Coordinates in screen space"
  (with-accessors ((main-state main-state)
                   (toolbar toolbar)) object
    (walk-quad-tree-anyway (object)
        (multiple-value-bind (picked cost-matrix-position matrix-position raw-position)
            (pick-pointer-position entity renderer x y)
          (declare (ignore raw-position matrix-position))
          (when picked
            (let* ((x-cost    (elt cost-matrix-position 0)) ; column
                   (y-cost    (elt cost-matrix-position 1)) ; row
                   (entity-id (entity-id-in-pos main-state x-cost y-cost)))
              ;; sync with toolbar...maybe better in main-window ?
              (when (valid-id-p entity-id)
                (let ((entity (or (fetch-from-player-entities main-state entity-id)
                                  (fetch-from-ai-entities main-state entity-id)))) ;; testing only
                  (when entity
                    (when bind
                      (setf (selected-pc main-state) entity)
                      (setf (widget:bound-player toolbar) entity)
                      (game-event:send-refresh-toolbar-event :reset-health-status-animation nil))
                    (return-from pick-player-entity entity)))))))))
  nil)

(defmethod pick-any-entity ((object world) renderer x y)
  "Coordinates in screen space"
  (with-accessors ((main-state main-state)
                   (toolbar toolbar)) object
    (walk-quad-tree-anyway (object)
        (multiple-value-bind (picked cost-matrix-position matrix-position raw-position)
            (pick-pointer-position entity renderer x y)
          (declare (ignore raw-position matrix-position))
          (when picked
            (let* ((x-cost    (elt cost-matrix-position 0)) ; column
                   (y-cost    (elt cost-matrix-position 1)) ; row
                   (entity-id (entity-id-in-pos main-state x-cost y-cost)))
              ;; sync with toolbar...maybe better in main-window ?
              (when (valid-id-p entity-id)
                (let ((entity (find-entity-by-id main-state entity-id)))
                  (when entity
                    (return-from pick-any-entity entity)))))))))
  nil)

(defmethod selected-pc ((object world))
  (when (main-state object)
    (selected-pc (main-state object))))

(defmethod turn-off-highligthed-tiles ((object world))
  (walk-quad-tree (object)
    (when (pickable-mesh-p entity)
      (turn-off-highligthed-tiles entity)))
  object)

(defmethod main-light-pos ((object world))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (celestial-body-position (main-state object)))

(defmethod main-light-pos-eye-space ((object world))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (transform-point (main-light-pos object)
                   (elt (the +transform-matrix-cointainer+
                             (view-matrix (camera  object))) 0)))

(defmethod main-light-color ((object world))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (light-color (main-state object)))

(defmethod elapsed-time ((object world))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (current-time (main-state object)))

(defmethod push-entity ((object world) entity)
  (bubbleup-modelmatrix entity)
  (quad-tree:push-down (entities object) entity))

(defmethod (setf compiled-shaders) (new-value (object world))
  (with-slots (compiled-shaders) object
    (setf compiled-shaders  new-value)
    (setf (compiled-shaders (skydome object)) new-value)))

(defmethod get-camera-pos ((object world))
  (pos (camera object)))

(defmethod frustum-bounding-sphere-intersects-p ((object world) ent)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (let* ((ent-sphere (bounding-sphere ent))
         (ent-center (sphere-center   ent-sphere))
         (camera-sphere (frustum-sphere (camera object)))
         (camera-sphere-center (sphere-center camera-sphere))
         (camera-sphere-radius (sphere-radius camera-sphere)))
    (declare (num:desired-type camera-sphere-radius))
    (num:d< (vec-length (vec- ent-center camera-sphere-center))
            camera-sphere-radius)))

(defmethod cone-aabb-intersects-p ((object world) ent)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (declare (world object))
  (with-accessors ((camera camera)) object
    ;;      e          h
    ;;       +--------+
    ;;      /|       /|
    ;;     / |      / |
    ;; f  +--------+ g|
    ;;    |  |     |  |
    ;;    |a +-----|--+ d
    ;;    | /      | /
    ;;    |/       |/
    ;;  b +--------+ c
    (let* ((aabb  (aabb ent))
           (min-x (min-x aabb))
           (min-y (min-y aabb))
           (min-z (min-z aabb))
           (max-x (max-x aabb))
           (max-y (max-y aabb))
           (max-z (max-z aabb))
           (a     (vec min-x min-y min-z))
           (b     (vec min-x min-y max-z))
           (c     (vec max-x min-y max-z))
           (d     (vec max-x min-y min-z))
           (e     (vec min-x max-y min-z))
           (f     (vec min-x max-y max-z))
           (g     (vec max-x max-y max-z))
           (h     (vec max-x max-y min-z))
           (cone  (frustum-cone camera)))
      (or (point-in-cone-p cone a)
          (point-in-cone-p cone b)
          (point-in-cone-p cone c)
          (point-in-cone-p cone d)
          (point-in-cone-p cone e)
          (point-in-cone-p cone f)
          (point-in-cone-p cone g)
          (point-in-cone-p cone h)))))

(defmethod frustum-aabb-intersects-p ((object world) ent)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  ;;      e          h
  ;;       +--------+
  ;;      /|       /|
  ;;     / |      / |
  ;; f  +--------+ g|
  ;;    |  |     |  |
  ;;    |a +-----|--+ d
  ;;    | /      | /
  ;;    |/       |/
  ;;  b +--------+ c
  (let* ((aabb  (aabb ent))
         (min-x (min-x aabb))
         (min-y (min-y aabb))
         (min-z (min-z aabb))
         (max-x (max-x aabb))
         (max-y (max-y aabb))
         (max-z (max-z aabb))
         (a     (vec min-x min-y min-z))
         (b     (vec min-x min-y max-z))
         (c     (vec max-x min-y max-z))
         (d     (vec max-x min-y min-z))
         (e     (vec min-x max-y min-z))
         (f     (vec min-x max-y max-z))
         (g     (vec max-x max-y max-z))
         (h     (vec max-x max-y min-z))
         (planes (frustum-planes (camera object))))
    (declare ((simple-array vec4:vec4 (6)) planes))
    ;; if a plane with normal on  the opposite side af all this aabb's
    ;; vertices exists, there is no intersection.
    (loop for plane across planes do
         (let ((totally-outside (and (not (plane-point-same-side-p plane a))
                                     (not (plane-point-same-side-p plane b))
                                     (not (plane-point-same-side-p plane c))
                                     (not (plane-point-same-side-p plane d))
                                     (not (plane-point-same-side-p plane e))
                                     (not (plane-point-same-side-p plane f))
                                     (not (plane-point-same-side-p plane g))
                                     (not (plane-point-same-side-p plane h)))))
           (when totally-outside
             (return-from frustum-aabb-intersects-p nil))))
    t))

(defmethod initialize-skydome ((object world)
                               &key
                                 (bg       (texture:get-texture texture:+skydome-bg+))
                                 (clouds-1 (texture:get-texture texture:+cloud-1+))
                                 (clouds-2 (texture:get-texture texture:+cloud-2+))
                                 (clouds-3 (texture:get-texture texture:+cloud-3+))
                                 (smoke    (texture:get-texture texture:+smoke-tray+))
                                 (weather-type :foggy-night-clear-day))
  (setf (skydome object) (make-skydome bg clouds-1 clouds-2 clouds-3 smoke)
        (weather-type    (skydome object)) weather-type))

(defmethod get-window-size ((object world))
  (sdl2:get-window-size (frame-window object)))

(defmethod render-gui ((object world))
  (render (gui object) object))

(defmethod all-furniture-bags-not-empty-p ((object world))
  (and (furnitures-bag       object)
       (containers-bag       object)
       (magic-furnitures-bag object)
       (pillars-bag          object)))

(defmethod all-furnitures-but-pillars-not-empty-p ((object world))
  (and (furnitures-bag       object)
       (containers-bag       object)
       (magic-furnitures-bag object)))

(defmethod push-interactive-entity ((object world) entity type occlusion-value
                                    &key
                                      (add-to-world t)
                                      (add-to-gamestate t))
  "Use for any interactive entity (i.e. has a character)"
  (when add-to-world
    (push-entity                  object  entity))  ; add to quadtree
  (when add-to-gamestate
    ;; add to entity tree of game-state
    (push-entity (main-state      object) entity)
    ;; add to game-state's matrix
    (world:setup-map-state-entity object  entity type occlusion-value)))

(defmethod push-trap-entity ((object world) entity)
  (push-entity object entity)  ; add to quadtree
  (game-state:push-trap-entity (main-state object) entity))

(defmethod set-map-state-type ((object world) x y type)
  (game-state:set-map-state-type (main-state object) x y type))

(defmethod set-map-state-id ((object world) x y id)
  (game-state:set-map-state-id (main-state object) x y id))

(defmethod set-map-state-occlusion ((object world) x y occlusion-value)
  (game-state:set-map-state-occlusion (main-state object) x y occlusion-value))

(defmethod setup-map-state-entity ((object world) entity type occlusion-value)
  (game-state:setup-map-state-entity (main-state object) entity type occlusion-value))

(defmethod move-map-state-entity ((object world) entity from)
  (game-state:move-map-state-entity (main-state object) entity from))

(defmethod move-entity ((object world) entity from &key (update-costs t))
  (with-accessors ((entities entities)
                   (main-state main-state)) object
    ;; update quadtree
    ;; remove entity from quad tree and game state
    (remove-entity-by-id entities (id entity))
    ;; re-add to quad-tre (updating leaf it belong, if needed)
    (push-entity object entity)
    ;; re-add to game-state
    (push-entity main-state entity)
    (when update-costs
      ;; set minimum cost for leaved tile
      (game-state:set-minimum-cost-player-layer@ main-state (elt from 0) (elt from 1))
      ;; set maximum cost for entered tile
      (let ((cost-pos-target (mesh:calculate-cost-position entity)))
        (game-state:set-invalicable-cost-player-layer@ main-state
                                                       (elt cost-pos-target 0)
                                                       (elt cost-pos-target 1)))
      (game-state:move-map-state-entity object entity from))))

(defmethod toolbar-selected-action ((object world))
  (widget:selected-action (toolbar object)))

(defmethod (setf toolbar-selected-action) (val (object world))
  (setf (widget:selected-action (toolbar object)) val))

(defmethod reset-toolbar-selected-action ((object world))
  (widget:reset-toolbar-selected-action (toolbar object)))

(defmethod setup-map-state-tile ((object world) x y type id occlusion-value)
  (world:set-map-state-type      object x y type)
  (world:set-map-state-id        object x y id)
  (world:set-map-state-occlusion object x y occlusion-value))

(defmethod place-player-on-map ((object world) player faction &optional (pos #(0 0)))
  ;; events
  (game-event:register-for-end-turn                           player)
  ;; movement
  (game-event:register-for-move-entity-along-path-event       player)
  (game-event:register-for-move-entity-along-path-end-event   player)
  (game-event:register-for-move-entity-entered-in-tile-event  player)
  (game-event:register-for-rotate-entity-cw-event             player)
  (game-event:register-for-rotate-entity-ccw-event            player)
  ;; health
  ;;poison
  (game-event:register-for-cause-poisoning-event              player)
  (game-event:register-for-cure-poisoning-event               player)
  (game-event:register-for-immune-poisoning-event             player)
  (game-event:register-for-cancel-immune-poisoning-event      player)
  ;; terror
  (game-event:register-for-cause-terror-event                 player)
  (game-event:register-for-cure-terror-event                  player)
  (game-event:register-for-cancel-terror-event                player)
  (game-event:register-for-immune-terror-event                player)
  (game-event:register-for-cancel-immune-terror-event         player)
  ;; berserk
  (game-event:register-for-cause-berserk-event                player)
  (game-event:register-for-cure-berserk-event                 player)
  (game-event:register-for-cancel-berserk-event               player)
  (game-event:register-for-immune-berserk-event               player)
  (game-event:register-for-cancel-immune-berserk-event        player)
  ;; faint
  (game-event:register-for-cause-faint-event                  player)
  (game-event:register-for-cure-faint-event                   player)
  (game-event:register-for-cancel-faint-event                 player)
  (game-event:register-for-immune-faint-event                 player)
  (game-event:register-for-cancel-immune-faint-event          player)
  ;; DMG points
  (game-event:register-for-heal-damage-event                  player)
  ;; modifier
  (game-event:register-for-modifier-object-event              player)
  ;; wearing
  (game-event:register-for-wear-object-event                  player)
  (game-event:register-for-unwear-object-event                player)
  ;; visibility
  (game-event:register-for-update-visibility                  player)
  ;; attack
  (game-event:register-for-attack-melee-event                 player)
  (game-event:register-for-attack-long-range-event            player)
  ;; attack spell
  (game-event:register-for-attack-spell-event                 player)
  (game-event:register-for-end-attack-spell-event             player)
  (game-event:register-for-end-defend-from-attack-spell-event player)
  ;; spell
  (game-event:register-for-spell-event                        player)
  (game-event:register-for-end-spell-event                    player)
  (game-event:register-for-end-defend-from-spell-event        player)
  ;; traps
  (game-event:register-for-trap-triggered-event               player)
  ;; plan, AI etc.
  (game-event:register-for-game-interrupt-terminated-event    player)
  ;; events registration ends here
  (game-state:place-player-on-map (main-state object)         player faction pos)
  (push-interactive-entity object                             player faction :occlude))

(defmethod push-labyrinth-entity ((object world) labyrinth)
  (game-state:push-labyrinth-entity (main-state object) labyrinth)
  (push-interactive-entity object labyrinth
                           +wall-type+ ; ignored as :add-to-gamestate is nil
                           :occlude    ; ignored as :add-to-gamestate is nil
                           :add-to-gamestate nil
                           :add-to-world     t))

(defmethod find-labyrinth-by-id ((object world) labyrinth-id)
  (game-state:find-labyrinth-by-id (main-state object) labyrinth-id))

(defmethod post-entity-message ((object world) entity text
                                suppress-default-action
                                &rest actions)
  "note: if entity is from AI does nothing"
  (with-accessors ((gui gui)
                   (compiled-shaders compiled-shaders)) object
    (with-accessors ((ghost ghost)
                     (state state)) entity
      (when (faction-player-p state (id entity))
        (with-accessors ((portrait portrait)) ghost
          (let* ((image          (or (texture:handle portrait) :info))
                 (default-action (if (not suppress-default-action)
                                     (list (cons (_ "OK")
                                                 #'widget:hide-and-remove-parent-cb))
                                     nil))
                 (all-actions    (concatenate 'list
                                              default-action
                                              actions))
                 (message-box (widget:make-message-box* text
                                                        (_ "Message")
                                                        image
                                                        all-actions)))
            (setf (compiled-shaders message-box) compiled-shaders)
            (mtree:add-child gui message-box)))))))

(defun point-to-entity-and-hide-cb (world entity)
  #'(lambda (w e)
      (widget:hide-and-remove-parent-cb w e)
      (world:point-camera-to-entity world entity)))

(defmethod point-camera-to-entity ((object world) entity))

(defmethod point-camera-to-entity ((object world) (entity entity))
  (with-accessors ((camera camera)) object
    (with-accessors ((pos pos)) entity
      (multiple-value-bind (res scaling)
          (vector-plane-intersection (pos camera)
                                     (dir camera)
                                     (vector 0.0 1.0 0.0 +zero-height+))
        (declare (ignore res))
        (setf (mode camera) :drag)
        (let ((destination (vec+ (aabb-center (aabb entity))
                                 (vec* (dir camera) (d- scaling)))))
          (setf (elt destination 1) (elt (pos camera) 1))
          (drag-camera-to camera destination))))))

(defun previews-path (type gender)
  (let ((re (text-utils:strcat
             (ecase type
               (:warrior
                +model-preview-warrior-re+)
               (:archer
                +model-preview-archer-re+)
               (:wizard
                +model-preview-wizard-re+)
               (:healer
                +model-preview-healer-re+)
               (:ranger
                +model-preview-ranger-re+))
             (ecase gender
               (:male
                "-male")
               (:female
                "-female"))
             +model-preview-ext-re+)))
    (mapcar #'(lambda (a)
                (res:strip-off-resource-path +ai-player-models-resource+ a))
            (fs:search-matching-file (res:get-resource-file "" +ai-player-models-resource+)
                                     :name re))))

(defun calc-capital (default level-difficult)
  (truncate (max (* (* 3/4 default)
                    (/ level-difficult 2))
                 default)))

(defmethod add-ai-opponent ((object world) type gender)
  (with-accessors ((main-state main-state)) object
    (let* ((*standard-capital-characteristic* (calc-capital *standard-capital-characteristic*
                                                            (level-difficult (main-state object))))
           (preview-paths  (previews-path type gender))
           (ghost          (ecase type
                            (:warrior
                             (make-warrior :human))
                            (:archer
                             (make-archer  :human))
                            (:ranger
                             (make-ranger  :human))
                            (:wizard
                             (make-wizard  :human))
                            (:healer
                             (make-healer  :human)))))
      ;; copy some new points to current
      (setf (current-damage-points   ghost) (damage-points   ghost))
      (setf (current-movement-points ghost) (movement-points ghost))
      (setf (current-spell-points    ghost) (spell-points    ghost))
      ;; setup model
      (let* ((dir (text-utils:strcat (fs:path-first-element (first preview-paths))
                                     fs:*directory-sep*))
             (model (md2:load-md2-player ghost
                                         dir
                                         (compiled-shaders object)
                                         +ai-player-models-resource+))
             (portrait-texture (texture:gen-name-and-inject-in-database
                                (texture:clone (texture:get-texture gui:+portrait-unknown-texture-name+)))))
        (pixmap:sync-data-to-bits portrait-texture)
        (texture:prepare-for-rendering portrait-texture)
        (setf (character:model-origin-dir ghost) dir)
        (setf (portrait (entity:ghost model)) portrait-texture)
        (setf (renderp  model) nil)
        (world:place-player-on-map object model game-state:+npc-type+ #(0 0))
        ;; initialize visited tiles per turn for new AI's pawn
        (let ((position (calculate-cost-position model)))
          (2d-utils:displace-2d-vector (position x y)
            (blackboard:reset-per-turn-visited-tiles (blackboard (state model)))
            (set-tile-visited (state model) model x y)))
        model))))

(defmethod world-aabb ((object world))
  (if (cached-aabb object)
      (cached-aabb object)
      (let ((res (make-instance 'aabb)))
        (walk-quad-tree-anyway (object)
          #+debug-mode
          (when (terrain-chunk:terrain-chunk-p entity) ; just to be sure
            (assert (triangles entity)))
          (when (every #'(lambda (a) (d>= a 0.0)) (aabb-p1 (aabb entity)))
            (expand res (aabb-p1 (aabb entity))))
          (when (every #'(lambda (a) (d>= a 0.0)) (aabb-p2 (aabb entity)))
            (expand res (aabb-p2 (aabb entity)))))
        (setf (slot-value object 'cached-aabb) res)
        res)))

(defmethod remove-all-tooltips ((object world))
  (remove-entity-if (entities object) #'(lambda (a) (typep a 'billboard:tooltip))))

(defmethod remove-all-windows ((object world))
  (remove-entity-if (gui object) #'(lambda (a) (typep a 'widget:window))))

(defmethod remove-all-removeable ((object world))
  (remove-entity-if (entities object)
                    #'(lambda (a)
                        (removeable-from-world-p a))))

(defmethod clean-characters-logs ((object world) trigger)
  (clean-characters-logs (main-state object) trigger))

(defmethod remove-all-removeable-from-gui ((object world))
  (remove-entity-if (gui object)
                    #'(lambda (a)
                        (removeable-from-world-p a))))

(defmethod activate-all-tooltips ((object world))
  (walk-quad-tree-anyway (object)
    (when (typep entity 'billboard:tooltip)
      (setf (renderp    entity) t
            (calculatep entity) t))))

(defmethod widget:activate-planner-icon ((object world))
  (widget:activate-planner-icon (toolbar object)))

(defmethod widget:deactivate-planner-icon ((object world))
  (widget:deactivate-planner-icon (toolbar object)))

(defun apply-tremor (world power duration)
  (interfaces:apply-damage world power :duration duration))

(defun apply-tremor-0 (world)
  (apply-tremor world
                tremor:+explosion-level-0-shake-power+
                tremor:+explosion-level-0-shake-duration+))

(defun apply-tremor-1 (world)
  (apply-tremor world
                tremor:+explosion-level-1-shake-power+
                tremor:+explosion-level-1-shake-duration+))

(defun apply-tremor-2 (world)
  (apply-tremor world
                tremor:+explosion-level-2-shake-power+
                tremor:+explosion-level-2-shake-duration+))

(defun apply-tremor-3 (world)
  (apply-tremor world
                tremor:+explosion-level-3-shake-power+
                tremor:+explosion-level-3-shake-duration+))

(defun clear-all-memoized-function-cache ()
  #+debug-ai (misc:dbg "clear planner cache")
  (blackboard:invalidate-blackboard-cache)
  (ai-utils:invalidate-ai-utils-cache)
  (goap:invalidate-tests-cache))
