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

(in-package :arrows)

(alexandria:define-constant +arrow-speed+        0.0005      :test #'=)

(alexandria:define-constant +arrow-model-name+   "model.obj" :test #'string=)

(defclass arrow ()
  ((type-current-action-scheduler
    :initform 'action-scheduler:attack-long-range-action
    :initarg  :type-current-action-scheduler
    :accessor type-current-action-scheduler)
   (trajectory
    :initform nil
    :initarg  :trajectory
    :accessor trajectory)
   (launcher-entity
    :initform nil
    :initarg  :launcher-entity
    :accessor launcher-entity)
   (attack-event-fn
    :initform nil
    :initarg  :attack-event-fn
    :accessor attack-event-fn)
   (hitted
    :initform nil
    :initarg  :hitted
    :reader   hittedp
    :writer   (setf hitted))))

(defun arrowp (a)
  (typep a 'arrow))

(defmethod clone-into :after ((from arrow) (to arrow))
  (setf (trajectory      to) (clone (trajectory from))
        (launcher-entity to) (launcher-entity   from)
        (attack-event-fn to) (attack-event-fn   from)
        (hitted          to) (hittedp           from))
  to)

(defmethod clone ((object arrow))
  (with-simple-clone (object 'arrow)))

(defmethod copy-flat-into :after ((from arrow) (to arrow))
  (setf (trajectory      to) (trajectory        from)
        (launcher-entity to) (launcher-entity   from)
        (attack-event-fn to) (attack-event-fn   from)
        (hitted          to) (hittedp           from))
  to)

(defmethod copy-flat ((object arrow))
  (with-simple-copy-flat (object 'arrow)))

(defclass arrow-mesh (triangle-mesh arrow) ())

(defmethod clone-into :after ((from arrow-mesh) (to arrow-mesh))
  to)

(defmethod clone ((object arrow-mesh))
  (with-simple-clone (object 'arrow-mesh)))

(defmethod copy-flat-into :after ((from arrow-mesh) (to arrow-mesh))
  to)

(defmethod copy-flat ((object arrow-mesh))
  (with-simple-copy-flat (object 'arrow-mesh)))

(defun hittable-by-arrow-p (entity)
  (and
   (not (terrain-chunk:terrain-chunk-p             entity))
   (not (building-floor-mesh:building-floor-mesh-p entity))
   (not (particles:particles-cluster-p             entity))
   (not (water-mesh-p                              entity))
   (not (arrowp                                    entity))
   (renderp                                        entity)))

(defun %overlap-labyrints-p (aabb-arrow vec-object)
  (when (not (vector-empty-p vec-object))
    (let ((state (state (elt vec-object 0))))
      (loop for a across vec-object do
           (let* ((map-pos      (map-utils:pos->game-state-pos a))
                  (element-type (el-type-in-pos state (elt map-pos 0) (elt map-pos 1))))
             (when (and (not (eq game-state:+empty-type+
                                 element-type))
                        (overlapp (actual-aabb-for-bullets a)
                                  aabb-arrow))
               (return-from %overlap-labyrints-p a))))))
  nil)

(defun %quad-aabb-overlap-p-fn (node arrow)
  (loop for entity across (quad-tree:data node) do
       (when (and (hittable-by-arrow-p entity)
                  (not (= (id arrow) (id entity)))
                  (overlapp (actual-aabb-for-bullets entity)
                            (aabb arrow)))
         (if (labyrinth-mesh-p entity)
             (let* ((walls    (children (wall-instanced    entity)))
                    (windows  (children (window-instanced  entity)))
                    (pillars  (children (pillar-instanced  entity)))
                    (doors-n  (remove-if #'openp (children (door-n-instanced entity))))
                    (doors-s  (remove-if #'openp (children (door-s-instanced entity))))
                    (doors-e  (remove-if #'openp (children (door-e-instanced entity))))
                    (doors-w  (remove-if #'openp (children (door-w-instanced entity))))
                    (tables   (children (table-instanced   entity)))
                    (chairs-n (children (chair-n-instanced entity)))
                    (chairs-s (children (chair-s-instanced entity)))
                    (chairs-e (children (chair-e-instanced entity)))
                    (chairs-w (children (chair-w-instanced entity)))
                    (res (or (%overlap-labyrints-p (aabb arrow) walls)
                             (%overlap-labyrints-p (aabb arrow) windows)
                             (%overlap-labyrints-p (aabb arrow) pillars)
                             (%overlap-labyrints-p (aabb arrow) doors-n)
                             (%overlap-labyrints-p (aabb arrow) doors-s)
                             (%overlap-labyrints-p (aabb arrow) doors-e)
                             (%overlap-labyrints-p (aabb arrow) doors-w)
                             (%overlap-labyrints-p (aabb arrow) tables)
                             (%overlap-labyrints-p (aabb arrow) chairs-n)
                             (%overlap-labyrints-p (aabb arrow) chairs-s)
                             (%overlap-labyrints-p (aabb arrow) chairs-e)
                             (%overlap-labyrints-p (aabb arrow) chairs-w))))
               (when res
                  (return-from %quad-aabb-overlap-p-fn res)))
             (return-from %quad-aabb-overlap-p-fn entity))))
  nil)

(defun arrow-collision-p (arrow)
  (with-accessors ((trajectory trajectory)
                   (pos pos)
                   (state state)) arrow
    (with-world (world state)
      (quad-tree:iterate-nodes-intersect (world:entities world)
                                         #'(lambda (node)
                                             (let ((collided (%quad-aabb-overlap-p-fn node arrow)))
                                               (when collided
                                                 (return-from arrow-collision-p collided))))
                                         (aabb-2d arrow)))))

(defmethod calculate :after ((object arrow-mesh) dt)
  (with-accessors ((trajectory trajectory)
                   (pos pos)
                   (state state)
                   (hittedp hittedp)
                   (launcher-entity launcher-entity)
                   (attack-event-fn attack-event-fn)) object
    (when (not hittedp)
      (with-world (world state)
        (let ((camera (world:camera world)))
          (if (or (not (3d-utils:insidep (world:world-aabb world)
                                         pos))
                  (d<= (elt pos 1)
                       (approx-terrain-height@pos state
                                                  (elt pos 0)
                                                  (elt pos 2)))) ;; gone out of the map
              (game-event:with-send-action-and-interrupt-terminated-check-type
                  (world (type-current-action-scheduler object))
                ;; remove from world
                (remove-entity-by-id world (id object))
                (setf (hitted object) nil)
                (setf (camera:followed-entity camera) nil)
                (setf (camera:mode camera) :fp))
              (let ((intersected-entity (arrow-collision-p object)))
                (if (and intersected-entity
                         (not (= (id intersected-entity)
                                 (id launcher-entity))))
                    (game-event:with-send-action-terminated-check-type
                        (world (type-current-action-scheduler object))
                      ;; note the player receiving the attack will unset interrupt plan
                      ;; send attack event
                      (funcall attack-event-fn launcher-entity intersected-entity)
                      ;; remove from world
                      (remove-entity-by-id world (id object))
                      (setf (hitted object) t)
                      (setf (camera:followed-entity camera) nil)
                      (setf (camera:mode camera) :fp))
                    (progn
                      (incf (displacement trajectory) +arrow-speed+)
                      (setf pos (ray-ends trajectory pos))
                      ;; update quadtree
                      (world:move-entity world object nil :update-costs nil))))))))))

(defclass arrow-attack-spell-mesh (triangle-mesh arrow)
  ((aabb-size
    :initarg  :aabb-size
    :initform (d/ +terrain-chunk-tile-size+ 4.0)
    :accessor aabb-size)))

(defmethod initialize-instance :after ((object arrow-attack-spell-mesh) &key &allow-other-keys)
  (setf (use-blending-p object) t)
  (setf (type-current-action-scheduler object) 'action-scheduler:attack-launch-spell-action))

(defmethod aabb ((object arrow-attack-spell-mesh))
  (with-slots (aabb bounding-sphere) object
    (with-accessors ((model-matrix model-matrix)
                     (aabb-size aabb-size)) object
      (declare ((simple-vector 1) model-matrix))
      (let* ((p1  (transform-point (vec     aabb-size   (d- aabb-size)   aabb-size)
                                   (elt model-matrix 0)))
             (p2  (transform-point (vec (d- aabb-size)  aabb-size       (d- aabb-size))
                                   (elt model-matrix 0)))
             (res (make-instance 'aabb)))
        (expand res p1)
        (expand res p2)
        (setf bounding-sphere (aabb->bounding-sphere res)
              aabb            res)
        res))))

(defmethod aabb-2d ((object arrow-attack-spell-mesh))
  (flatten-to-aabb2-xz-positive (aabb object)))

(defmethod render ((object arrow-attack-spell-mesh) renderer)
  (do-children-mesh (c object)
    (render c renderer)))

(defmethod removeable-from-world-p ((object arrow-attack-spell-mesh))
  (and (not (vector-empty-p (mtree:children object)))
       (removeable-from-world-p (elt (mtree:children object) 0))))

;; (defmethod render :after ((object arrow-attack-spell-mesh) renderer)
;;   (render-debug object renderer))

(defmethod calculate :after ((object arrow-attack-spell-mesh) dt)
  (with-accessors ((trajectory trajectory)
                   (pos pos)
                   (state state)
                   (hittedp hittedp)
                   (launcher-entity launcher-entity)
                   (attack-event-fn attack-event-fn)) object
    (when (not hittedp)
      (with-world (world state)
        (let ((camera (world:camera world)))
          (if (or (not (3d-utils:insidep (world:world-aabb world)
                                         pos))
                  (d<= (elt pos 1)
                       (approx-terrain-height@pos state
                                                  (elt pos 0)
                                                  (elt pos 2))))
              (game-event:with-send-action-terminated
                ;; remove from world
                (remove-entity-by-id world (id object))
                (setf (hitted object) nil)
                (setf (camera:followed-entity camera) nil)
                (setf (camera:mode camera) :fp))
              (let ((intersected-entity (arrow-collision-p object)))
                (if (and intersected-entity
                         (not (= (id intersected-entity)
                                 (id launcher-entity))))
                    (game-event:with-send-action-terminated
                      ;; send attack event
                      (funcall attack-event-fn launcher-entity intersected-entity)
                      (particles::set-respawn (elt (mtree:children object) 0) nil)
                      (setf (renderp (elt (mtree:children object) 0)) nil)
                      (setf (hitted object) t)
                      (setf (camera:followed-entity camera) nil)
                      (setf (camera:mode camera) :fp))
                    (progn
                      (incf (displacement trajectory) +arrow-speed+)
                      (setf pos (ray-ends trajectory pos))
                      ;; update quadtree
                      (world:move-entity world object nil :update-costs nil))))))))))

(defstruct arrow-db-entry id mesh)

(defparameter *arrows-factory-db* '())

(defun clean-db ()
  (map 'nil #'(lambda (a) (destroy (arrow-db-entry-mesh a))) *arrows-factory-db*)
  (setf *arrows-factory-db* nil))

(defun %get-arrow (id)
  (let ((res (find-if #'(lambda (a) (string= (arrow-db-entry-id a) id)) *arrows-factory-db*)))
    (and res (arrow-db-entry-mesh res))))

(defun get-arrow (name)
  (let* ((path (text-utils:strcat name fs:*directory-sep* +arrow-model-name+))
         (id (res:get-resource-file path +arrows-resource+)))
    (or (%get-arrow id)
        (let ((new-arrow (make-instance 'arrow-mesh))
              (raw-mesh  (obj-mesh:load id)))
          (clone-into raw-mesh new-arrow)
          (prepare-for-rendering new-arrow)
          (push (make-arrow-db-entry :id id :mesh new-arrow) *arrows-factory-db*)
          new-arrow))))

(defun launch-ray (attacker defender imprecision-increase)
  (flet ((decrease-atk (standard)
           (dmax 0.0 (d- standard (d* imprecision-increase standard)))))
    (let* ((ghost-atk     (entity:ghost attacker))
           (weapon        (character:worn-weapon ghost-atk))
           (weapon-type   (if weapon
                              (cond
                                ((interactive-entity:bowp weapon)
                                 :bow)
                                ((interactive-entity:crossbowp weapon)
                                 :crossbow)
                                (t
                                 :spell))
                              :spell))
           (attack-chance (if (eq weapon-type :spell)
                              (character:actual-attack-spell-chance ghost-atk)
                              (decrease-atk (character:actual-range-attack-chance ghost-atk))))
           (ray-dir       (normalize (vec- (aabb-center (actual-aabb-for-bullets defender))
                                           (aabb-center (aabb attacker)))))
           (ray           (make-instance 'ray
                                         :ray-direction ray-dir
                                         :displacement +arrow-speed+)))
      (if (d< (dabs (secure-dacos (dot-product (dir attacker)
                                               ray-dir)))
              +visibility-cone-half-hangle+)
          (progn
            (when (not (die-utils:pass-d100.0 attack-chance))
              (setf (ray-direction ray)
                    (transform-direction (ray-direction ray)
                                         (rotate-around +y-axe+
                                                        (d* (dexpt -1.0
                                                                   (d (lcg-next-in-range 1 3)))
                                                            (if (eq weapon-type :bow)
                                                                (lcg-next-in-range 0.05 1.0)
                                                                (lcg-next-in-range 0.01 0.5)))))))
            ray)
          nil))))

(defun %common-launch-projectile (world attacker defender mesh attack-event-fn
                                  imprecision-increase
                                  &key
                                    (position        (aabb-center (aabb attacker)))
                                    (camera-follow-p t))
  (let ((ray (launch-ray attacker defender imprecision-increase)))
    (if ray
        (progn
          (setf (renderp mesh) t)
          (setf (hitted mesh) nil)
          (setf (attack-event-fn mesh) attack-event-fn)
          (setf (pos mesh) position)
          (setf (trajectory mesh) ray)
          (setf (dir mesh) (normalize (vec-negate (ray-direction (trajectory mesh)))))
          (setf (launcher-entity mesh) attacker)
          (setf (compiled-shaders mesh) (compiled-shaders world))
          (when camera-follow-p
            (setf (camera:followed-entity (world:camera world)) mesh)
            (setf (camera:mode (world:camera world)) :follow))
          ray)
        (progn
          (game-event:send-action-terminated-event)
          nil))))

(defun launch-arrow (name world attacker defender imprecision-increase)
  (let* ((mesh     (get-arrow name))
         (successp (%common-launch-projectile world
                                              attacker
                                              defender
                                              mesh
                                              #'battle-utils:send-attack-long-range-event
                                              imprecision-increase
                                              ;; test was ':camera-follow-p t'
                                              :camera-follow-p nil)))
    (when successp
      (world:push-entity world mesh))))

(defun send-attack-spell-events-fn (spell)
  #'(lambda (attacker defender)
      (let* ((state            (state attacker))
             (range            (spell:effective-range           spell))
             (pos-defender     (map-utils:pos->game-state-pos   defender))
             (accetable-type   (list +empty-type+
                                     +unknown-type+))
             (neighborhood     (get-neighborhood state
                                                 (elt pos-defender 1)
                                                 (elt pos-defender 0)
                                                 #'(lambda (el pos)
                                                     (declare (ignore pos))
                                                     (not (find (el-type el)
                                                                accetable-type
                                                                :test #'eq)))
                                                 :w-offset range
                                                 :h-offset range)))
        (loop for map-element across neighborhood do
             (let* ((id-entity (entity-id (car map-element)))
                    (entity    (find-entity-by-id state id-entity))
                    (pos       (map-utils:pos->game-state-pos entity))
                    (dist      (map-utils:map-manhattam-distance pos-defender pos)))
               (when (<= dist range)
                 (battle-utils:send-attack-spell-event attacker entity))))
        (battle-utils:send-attack-spell-event attacker defender))))

(defun launch-attack-spell (spell world attacker defender
                            &key (invisiblep nil))
  (let* ((mesh     (make-instance 'arrow-attack-spell-mesh
                                  :aabb-size (funcall (spell:effective-aabb-size spell) spell)))
         (successp (%common-launch-projectile world
                                              attacker
                                              defender
                                              mesh
                                              (send-attack-spell-events-fn spell)
                                              0.0
                                              :camera-follow-p t)))
    ;; testing aabb
    ;; (prepare-for-rendering mesh)
    ;; (setf (render-aabb mesh) t)
    ;;end testing
    (when successp
      (when (not invisiblep)
        (let* ((shaders       (compiled-shaders world))
               (bullet        (funcall (spell:arrow spell)
                                       +zero-vec+
                                       +entity-forward-direction+
                                       shaders))
               (target-effect (funcall (spell:visual-effect-target spell)
                                       +zero-vec+
                                       shaders)))
          (mtree:add-child mesh bullet)
          (with-enqueue-action
              (world action-scheduler:particle-effect-action)
            (setf (pos target-effect) (pos mesh))
            (world:push-entity world target-effect))
          (with-enqueue-action-and-send-remove-after
              (world action-scheduler:end-attack-spell-action)
            (game-event:send-end-attack-spell-event attacker)
            (game-event:send-end-defend-from-attack-spell-event defender))))
      (world:push-entity world mesh))))

(defun launch-attack-spell-trap (spell world attacker defender)
  (let* ((shaders       (compiled-shaders world))
         (target-effect (funcall (spell:visual-effect-target spell)
                                 +zero-vec+
                                 shaders)))
    (setf (pos    target-effect) (pos defender))
    (end-of-life-remove-from-action-scheduler target-effect
                                              action-scheduler::particle-effect-action)
    (with-enqueue-action ;; make a sub-scheduler
        (world action-scheduler:action-scheduler))
    (with-enqueue-action
        (world action-scheduler:particle-effect-action)
      (world:push-entity world target-effect))
    (with-enqueue-action-and-send-remove-after
        (world action-scheduler:send-spell-fx-action)
      (battle-utils:send-attack-spell-event attacker defender :ignore-visible t))
    (with-enqueue-action-and-send-remove-after
        (world action-scheduler:end-attack-spell-action)
      (game-event:send-end-defend-from-attack-spell-event defender))))

(defun send-spell-events-fn (spell attacker defender)
  #'(lambda ()
      (let* ((state            (state attacker))
             (range            (spell:effective-range           spell))
             (pos-defender     (map-utils:pos->game-state-pos   defender))
             (neighborhood-pc  (neighborhood-by-type state
                                                     (elt pos-defender 1)
                                                     (elt pos-defender 0)
                                                     +pc-type+
                                                     :h-offset range
                                                     :w-offset range))
             (neighborhood-npc (neighborhood-by-type state
                                                     (elt pos-defender 1)
                                                     (elt pos-defender 0)
                                                     +npc-type+
                                                     :h-offset range
                                                     :w-offset range))
             (neighborhood     (concatenate 'vector neighborhood-npc neighborhood-pc)))
        (loop for map-element across neighborhood do
             (let* ((id-entity (entity-id (car map-element)))
                    (entity    (find-entity-by-id state id-entity))
                    (pos       (map-utils:pos->game-state-pos entity))
                    (dist      (map-utils:map-manhattam-distance pos-defender pos)))
               (when (<= dist range)
                 (battle-utils:send-spell-event attacker entity))))
        (battle-utils:send-spell-event attacker defender))))

(defun %enqueue-tooltip (entity message)
  (billboard:enqueue-tooltip entity
                             message
                             :color     billboard:+damage-color+
                             :font-type gui:+tooltip-font-handle+
                             :activep   t))

(defun launch-spell (spell world attacker defender)
  (let* ((range        (spell:range spell))
         (pos-attacker (pos attacker))
         (x-attacker   (map-utils:coord-chunk->matrix (elt pos-attacker 0)))
         (z-attacker   (map-utils:coord-chunk->matrix (elt pos-attacker 2)))
         (pos-defender (pos defender))
         (x-defender   (map-utils:coord-chunk->matrix (elt pos-defender 0)))
         (z-defender   (map-utils:coord-chunk->matrix (elt pos-defender 2)))
         (dist         (map-utils:map-manhattam-distance (ivec2:ivec2 x-attacker z-attacker)
                                                         (ivec2:ivec2 x-defender z-defender))))
    (if (<= dist range)
        ;;; TEST
        (if (or t (die-utils:pass-d100.0 (character:actual-spell-chance (ghost attacker))))
            (let* ((shaders (compiled-shaders world))
                   (target-effect (funcall (spell:visual-effect-target spell)
                                           (copy-vec (aabb-center (aabb defender)))
                                           shaders)))
              (end-of-life-remove-from-action-scheduler target-effect
                                                        action-scheduler:particle-effect-action)
              (with-enqueue-action
                   (world action-scheduler:particle-effect-action)
                (world:push-entity world target-effect))
              (with-enqueue-action-and-send-remove-after
                  (world action-scheduler:send-spell-fx-action)
                (funcall (send-spell-events-fn spell attacker defender)))
              (with-enqueue-action-and-send-remove-after
                  (world action-scheduler:end-spell-action)
                (game-event:send-end-spell-event attacker))
                (game-event:send-end-defend-from-spell-event defender))
            (%enqueue-tooltip attacker (_ "fail")))
        (%enqueue-tooltip attacker (_ "too far")))))
