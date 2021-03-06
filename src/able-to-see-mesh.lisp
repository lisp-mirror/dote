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

(in-package :able-to-see-mesh)

(defparameter *visibility-target-placeholder* nil)

(defclass visibility-placeholder (tree-mesh-shell able-to-see-mesh) ())

(defparameter *ray-test-id-entities-ignored* nil
  "a list of id that the ray will ignore" )

(defun clean-visibility-target-placeholder ()
  (setf *visibility-target-placeholder* nil))

(defun setup-placeholder (world shaders)
  (let ((tree-mesh (trees:gen-tree (res:get-resource-file +mesh-placeholder-file+
                                                          constants:+trees-resource+
                                                          :if-does-not-exists :error)
                                   :flatten t)))
    (setf (compiled-shaders tree-mesh) shaders)
    (setf *visibility-target-placeholder* (mesh:fill-shell-from-mesh tree-mesh
                                                                     'visibility-placeholder))
    (setf (renderp *visibility-target-placeholder*) nil)
    (setf (entity:pos *visibility-target-placeholder*)
          (vec (map-utils:coord-map->chunk 0.0)
               (- +zero-height+)
               (map-utils:coord-map->chunk 0.0)))
    (world:push-entity world *visibility-target-placeholder*)))

(defmacro with-placeholder@ ((x y z) &body body)
  "coordinates in terrain (float) space"
  `(progn
     (setf (entity:pos *visibility-target-placeholder*) (vec ,x ,y ,z))
     (world:move-entity (game-state:fetch-world (state *visibility-target-placeholder*))
                        *visibility-target-placeholder* nil :update-costs nil)
     ,@body))

(defmacro %placeholder-visible-p-wrapper ((player x y) &body body)
  "convert x y (integer coordinate (e. g. cost space) in 3d coordinates and
move the placeholder there"
  (with-gensyms (x-terrain-space z-terrain-space h-terrain-space state)
    `(with-accessors ((,state state)) ,player
       (let* ((,x-terrain-space (map-utils:coord-map->chunk ,x))
              (,z-terrain-space (map-utils:coord-map->chunk ,y))
              (,h-terrain-space (approx-terrain-height@pos ,state
                                                           ,x-terrain-space
                                                           ,z-terrain-space)))
         (with-placeholder@ (,x-terrain-space ,h-terrain-space ,z-terrain-space)
           ,@body)))))

(defun reset-placeholder-position ()
  "move in an invisible place the placeholder"
  (setf (entity:pos *visibility-target-placeholder*)
        (vec (map-utils:coord-map->chunk 0.0)
             (- +zero-height+)
             (map-utils:coord-map->chunk 0.0)))
  (world:move-entity (game-state:fetch-world (state *visibility-target-placeholder*))
                     *visibility-target-placeholder*
                     nil
                     :update-costs nil))

(defun placeholder-visible-p (player x y &key (me-visible-by-myself-p t))
  "x y z logical map space (integer)"
  (with-accessors ((state state)) player
    ;; if the position is occupied switch to normal visibility
    (if (not (map-element-empty-p (element-mapstate@ state x y)))
        (other-visible-p player (find-entity-by-id state
                                                   (entity-id-in-pos state x y))
                         :me-visible-by-myself-p me-visible-by-myself-p)
        (%placeholder-visible-p-wrapper (player x y)
          (multiple-value-bind (ray entity)
              (other-visible-p player *visibility-target-placeholder*
                               :me-visible-by-myself-p me-visible-by-myself-p)
            ;; reset placeholder's posittion
            ;; TODO: use reset-placeholder-position
            (setf (entity:pos *visibility-target-placeholder*)
                  (vec (map-utils:coord-map->chunk 0.0)
                       (- +zero-height+)
                       (map-utils:coord-map->chunk 0.0)))
            (world:move-entity (game-state:fetch-world (state *visibility-target-placeholder*))
                               *visibility-target-placeholder* nil :update-costs nil)
            (values ray entity))))))

(defun tiles-placeholder-visible-in-box (matrix player x-center y-center size
                                         &key (me-visible-by-myself-p t))
  "x-center y-center size logical map space (integer)"
  (let ((tiles (matrix:gen-valid-neighbour-position-in-box matrix
                                                           x-center
                                                           y-center
                                                           size size
                                                           :add-center t)))
    (loop
       for tile across tiles
       when
         (placeholder-visible-p player (elt tile 0) (elt tile 1)
                                :me-visible-by-myself-p me-visible-by-myself-p)
       collect tile)))

(defmacro with-invisible-ids ((ids) &body body)
  `(let ((*ray-test-id-entities-ignored* ,ids))
     ,@body))

(defun tile-placeholder-visible-by-faction-p (main-state pos-tile faction
                                              &key (me-visible-by-myself-p t))
  (let ((map-fn (faction->map-faction-fn faction)))
    (funcall map-fn main-state
             #'(lambda (entity)
                 (when (placeholder-visible-p entity (elt pos-tile 0) (elt pos-tile 1)
                                              :me-visible-by-myself-p me-visible-by-myself-p)
                   (return-from tile-placeholder-visible-by-faction-p t))))
    nil))

(defun tiles-placeholder-visibility-in-ring-by-faction (game-state x-center y-center
                                                        ext-size
                                                        int-size
                                                        faction
                                                        entities-checks-even-if-invisible
                                                        &key
                                                          (remove-non-empty-tiles t)
                                                          (discard-non-visible-opponents t))
  " TODO: write this docstring in proper english. :))

x-center  y-center ext-size  logical map  space (integer).   Note: all
entities of both the factions will be ignored in visibility test.

Here 'opponents' means character of faction opposite of 'faction' parameter.

If remove-non-empty-tiles is non nil  occupied tiles (but not occupied
by *visible* character of faction 'faction') will be discarded.

*visible* means visible from character of opponents.

If discard-non-visible-opponents is not nil *invisible* opponents will
be ignored.

*invisible* means not visible from character of opponents.

Faction is  usual  human  player character

Returns two values: *invisibles* and *visibles* tiles"
  (flet ((remove-from-unvisible-tiles (visibles-of-faction x y)
           (if  (empty@pos-p game-state x y)
                nil                         ;; empty tile, maintain
                (let ((entity@pos (game-state:entity-in-pos game-state x y)))
                  (if (pawnp entity@pos)
                      (if (eq faction (my-faction entity@pos)) ; yes, check faction
                          (if (find entity@pos
                                    visibles-of-faction
                                    :test #'identificable:test-id=)
                              t    ; same faction of 'faction' and visible, remove
                              nil)
                          t)       ; opposite faction, remove
                      t))))) ;non interactive entity (wall, tree etc.) remove
    (let* ((map-state  (map-state game-state))
           (tiles (misc:seq->list
                   (matrix:gen-valid-fat-ring-positions map-state
                                                        x-center
                                                        y-center
                                                        ext-size ext-size
                                                        int-size int-size)))
           (visibles         '())
           (map-fn           (faction->map-faction-fn faction))
           ;; opposite faction is AI usually
           (opposite-faction (faction->opposite-faction faction))
           (friends-id       (game-state:all-player-id-by-faction game-state faction))
           ;; friend of opposite-faction (friend of AI usually)
           (friends-opposite-faction-id (game-state:all-player-id-by-faction game-state
                                                                             opposite-faction))
           ;; friend of faction (friend of human's character usually)
           (visibles-faction-entities   (visible-players-in-state-from-faction game-state
                                                                               opposite-faction)))
      ;; remove non empty tile if required
      (when remove-non-empty-tiles
        (setf tiles
              (remove-if #'(lambda (p)
                             (remove-from-unvisible-tiles visibles-faction-entities
                                                          (elt p 0) (elt p 1)))
                         tiles)))
      (funcall map-fn game-state
               #'(lambda (v)
                   ;; ignore  player of both factions
                   (with-invisible-ids ((concatenate 'list
                                                     friends-opposite-faction-id
                                                     friends-id))
                     ;; do not  test for visibility  of a tile  if the
                     ;; opponent is not visible  from opponents of 'v'
                     ;; (usually v is an opponent of the AI)
                     (when (or (not discard-non-visible-opponents)
                               (find v
                                     entities-checks-even-if-invisible
                                     :test #'identificable:test-id=)
                               (find v
                                     visibles-faction-entities
                                     :test #'identificable:test-id=))
                       (loop
                          for tile in tiles
                          when (placeholder-visible-p v (elt tile 0) (elt tile 1))
                          do
                            (pushnew tile visibles :test #'ivec2:ivec2=))))))
      (values (set-difference tiles visibles :test #'ivec2:ivec2=)
              visibles))))

(defun placeholder-visible-ray-p (player x y &key (me-visible-by-myself-p t))
  "x y logical map space (integer)"
  (with-accessors ((state state)) player
    ;; if the position is occupied switch to normal visibility test
    (if (not (map-element-empty-p (element-mapstate@ state x y)))
        (other-visible-ray-p player (find-entity-by-id state
                                                       (entity-id-in-pos state x y))
                             :me-visible-by-myself-p me-visible-by-myself-p)
        (%placeholder-visible-p-wrapper (player x y)
          (multiple-value-bind (ray entity)
              (other-visible-ray-p player *visibility-target-placeholder*
                                   :me-visible-by-myself-p me-visible-by-myself-p)
            (setf (entity:pos *visibility-target-placeholder*)
                  (vec (map-utils:coord-map->chunk 0.0)
                       (- +zero-height+)
                       (map-utils:coord-map->chunk 0.0)))
            (values ray entity))))))

(defun placeholder-able-to-see-p (x-placeholder y-placeholder target
                              &key
                                (me-visible-by-myself-p t)
                                (placeholder-direction  nil))
  "can a placeholder see the entity target?
If direction is nil the placeholder turn around and test for visibility until sweep 360°"
  (labels ((visibility-test ()
             (multiple-value-bind (ray entity)
                 (other-visible-p *visibility-target-placeholder*
                                  target
                                  :me-visible-by-myself-p me-visible-by-myself-p)
               (reset-placeholder-position)
               (values ray entity)))
           (set-dir-and-test (dir)
             (%placeholder-visible-p-wrapper (target x-placeholder y-placeholder)
               (setf (dir *visibility-target-placeholder*) dir)
               (update-visibility-cone *visibility-target-placeholder*)
               (visibility-test))))
      (if placeholder-direction
          (set-dir-and-test placeholder-direction)
          (or (set-dir-and-test +x-axe+)
              (set-dir-and-test (vec-negate +x-axe+))
              (set-dir-and-test +z-axe+)
              (set-dir-and-test (vec-negate +z-axe+))))))

(defclass able-to-see-mesh (triangle-mesh)
  ((visibility-cone
    :initform (make-instance 'cone)
    :initarg  :visibility-cone
    :accessor visibility-cone)))

(defgeneric other-visible-p (object target
                             &key
                               exclude-if-labyrinth-entity cone
                               me-visible-by-myself-p))

(defgeneric update-visibility-cone (object &key rebuild-modelmatrix))

(defgeneric visible-players (object &key predicate alive-only))

(defgeneric other-faction-visible-players (object &key alive-only))

(defgeneric visible-players-in-state-from-faction (object faction &key alive-only))

(defgeneric other-visible-cone-p (object target &key
                                                  cone
                                                  me-visible-by-myself-p))

(defgeneric other-visible-ray-p (object target &key
                                                 exclude-if-labyrinth-entity
                                                 me-visible-by-myself-p))

(defgeneric update-visibles-state (object))

(defgeneric labyrinth-element-hitted-by-ray (object target))

(defgeneric nonlabyrinth-element-hitted-by-ray (object target))

(defmethod update-visibility-cone ((object able-to-see-mesh) &key (rebuild-modelmatrix t))
  (with-accessors ((visibility-cone visibility-cone)
                   (pos  pos)
                   (dir dir)) object
    (setf (cone-apex   visibility-cone) (aabb-center (actual-aabb-for-visibility object)))
    (setf (cone-height visibility-cone) (vec* dir (vec-length (cone-height visibility-cone))))
    (when rebuild-modelmatrix
      (bubbleup-modelmatrix object))
    object))

(defmethod visible-players ((object able-to-see-mesh)
                            &key (predicate #'identity) (alive-only t))
  "A list containing all visible pc satisfing predicate"
  (with-accessors ((dir dir)
                   (pos pos)
                   (id id)
                   (state state)) object
    ;;(misc:dbg "~a" (visibility-cone object))
    (let ((others (if (faction-player-p state id)
                      (game-state:ai-entities     state)
                      (game-state:player-entities state)))
          (mines  (if (faction-player-p state id)
                      (game-state:player-entities state)
                      (game-state:ai-entities     state))))
      (let ((all (concatenate 'list
                              (loop for ent in others
                                 when (and (other-visible-p object ent)
                                           (funcall predicate ent))
                                 collect ent)
                              (loop for ent in mines
                                 when (funcall predicate ent)
                                 collect ent))))
        (if alive-only
            (remove-if #'entity:entity-dead-p all)
            all)))))

(defmethod other-faction-visible-players ((object able-to-see-mesh) &key (alive-only t))
  (visible-players object
                   :predicate #'(lambda (a)
                                  (not (eq (my-faction object)
                                           (my-faction a))))
                   :alive-only alive-only))

(defmethod visible-players-in-state-from-faction ((object game-state) faction
                                                  &key (alive-only t))
  "all visible opponents seen by faction (list of entities)"
  (let ((opposite-faction (faction->opposite-faction faction))
        (map-fn           (faction->map-faction-fn faction))
        (res              '()))

    (funcall map-fn
             object
             #'(lambda (ent)
                 (let ((visibles (visible-players ent
                                                  :predicate #'(lambda (a)
                                                                 (eq (my-faction a)
                                                                     opposite-faction))
                                                  :alive-only alive-only)))
                   (loop for visible in visibles do
                        (pushnew visible res :test #'test-id=)))))
    res))

(defun all-visibles-opponents (entity &key (alive-only t))
  "the visible opponents of AI entity, if exist."
  (with-accessors ((state state)) entity
    (let ((opposite-faction (faction->opposite-faction entity)))
      (absee-mesh:visible-players entity
                                  :alive-only alive-only
                                  :predicate  #'(lambda (a)
                                                  (eq (my-faction a)
                                                      opposite-faction))))))

(defmacro with-test-me-visible ((a b should-be-visible-p) &body body)
  `(if (and ,should-be-visible-p
            (= (id ,a) (id ,b)))
       (values (make-instance 'ray
                              :displacement 0.0
                              :ray-direction (dir ,a))
               ,a)
       (progn ,@body)))

(defmethod other-visible-p ((object able-to-see-mesh) (target triangle-mesh)
                            &key
                              (exclude-if-labyrinth-entity t)
                              (cone (visibility-cone object))
                              (me-visible-by-myself-p t))
  "is target visible from object?
note: by default non labyrinth elements are ignored"
  (with-test-me-visible (object target me-visible-by-myself-p)
    (let ((in-cone-p (other-visible-cone-p object target
                                           :cone cone
                                           :me-visible-by-myself-p me-visible-by-myself-p)))
      (when in-cone-p
        ;;(misc:dbg "visible for cone ~a" target)
        ;;(return-from other-visible-p t)
        (multiple-value-bind (ray-hitted entity-hitted)
            (other-visible-ray-p object
                                 target
                                 :exclude-if-labyrinth-entity exclude-if-labyrinth-entity
                                 :me-visible-by-myself-p me-visible-by-myself-p)
          (if ray-hitted
              (values ray-hitted entity-hitted)
              (values nil        entity-hitted)))))))

(defmacro ensure-not-dead ((entity) &body body)
  `(and (not (entity-dead-p ,entity))
        ,@body))

(defmethod other-visible-cone-p ((object able-to-see-mesh) (target vector)
                                 &key
                                   (cone (visibility-cone object))
                                   (me-visible-by-myself-p t))
  "is target visible from object (cone test)? Trget is a 3d vector."
  (with-accessors ((pos pos)) object
    (if (and me-visible-by-myself-p
             (vec~ pos target))
        (values (make-instance 'ray
                               :displacement 0.0
                               :ray-direction (dir object))
                object)
        (ensure-not-dead (object)
          (point-in-cone-p cone target)))))

;; TODO use the method specialized in: mesh, vector
(defmethod other-visible-cone-p ((object able-to-see-mesh) (target triangle-mesh)
                                 &key
                                   (cone (visibility-cone object))
                                   (me-visible-by-myself-p t))
  "is target visible from object (cone test)?"
  (with-test-me-visible (object target me-visible-by-myself-p)
    (let ((center (aabb-center (actual-aabb-for-visibility target))))
      (ensure-not-dead (object)
        (point-in-cone-p cone center)))))

(defmethod other-visible-ray-p ((object able-to-see-mesh) (target triangle-mesh)
                                &key
                                  (exclude-if-labyrinth-entity t)
                                  (me-visible-by-myself-p t))
  "is target visible from object (ray test)?

note:  if  exclude-if-labyrinth-entity  is  non  nil  target  will  be
invisible if part of a labyrinth.
Also note that that ray is long as much as the height of the visibility cone of this mesh"
  (with-test-me-visible (object target me-visible-by-myself-p)
    (multiple-value-bind (ray-nonlab-hitted entity-nonlab-hitted)
        (nonlabyrinth-element-hitted-by-ray object target)
      (multiple-value-bind (ray-lab-hitted entity-lab-hitted)
          (labyrinth-element-hitted-by-ray object target)
        (if exclude-if-labyrinth-entity
            (cond
              ((null ray-nonlab-hitted)
               (values nil entity-nonlab-hitted))
                ((null ray-lab-hitted)
                 (values ray-nonlab-hitted entity-nonlab-hitted))
                (t ;hitted both labyrinth and player
                 (if (d< (displacement ray-lab-hitted)
                         (displacement ray-nonlab-hitted))
                     (values nil               entity-nonlab-hitted)
                     (values ray-nonlab-hitted entity-nonlab-hitted))))
              (cond
                ((and ray-lab-hitted
                      ray-nonlab-hitted)
                 (if (d< (displacement ray-lab-hitted)
                         (displacement ray-nonlab-hitted))
                     (values nil               entity-nonlab-hitted)
                     (values ray-nonlab-hitted entity-nonlab-hitted)))
                (ray-nonlab-hitted
                 (values ray-nonlab-hitted entity-nonlab-hitted))
                (ray-lab-hitted
                 (values ray-lab-hitted entity-lab-hitted))
                (t ;; not visible
                 (values nil nil))))))))

(defmethod update-visibles-state ((object able-to-see-mesh))
  "update visibility, note that the  update-visibility event is sent.
   See: md2-mesh:on-game-event ((object md2-mesh) (event update-visibility))"
  (absee-mesh:update-visibility-cone       object)
  (game-event:send-update-visibility-event object nil))

(defmethod nonlabyrinth-element-hitted-by-ray ((object able-to-see-mesh) (target triangle-mesh))
  (with-accessors ((dir dir)
                   (pos pos)
                   (state state)
                   (visibility-cone visibility-cone)) object
    ;; launch a ray
    (let* ((actual-aabb-target (actual-aabb-for-visibility target))
           (actual-aabb-object (actual-aabb-for-visibility object))
           (center-target (aabb-center actual-aabb-target))
           (center-object (aabb-center actual-aabb-object))
           (ray           (make-instance 'ray
                                         :ray-direction
                                         (normalize (vec- center-target
                                                          center-object))))
           (world-ref     (game-state:fetch-world (state object)))
           (quad-tree     (world:entities world-ref)))
      (loop
         for dt from 0.0
         below (vec-length (cone-height visibility-cone))
         by +visibility-ray-displ-incr+ do
           (incf (displacement ray) dt)
           (let* ((ends (ray-ends ray pos))
                  (entities (quad-tree:query-data-path-to-point quad-tree
                                                                (vec2 (elt ends 0)
                                                                      (elt ends 2)))))
             (loop for d across entities
                ;; check if this entity should be ignored
                when (not (find (id d) *ray-test-id-entities-ignored* :test #'=))
                do
                  (cond
                    ((terrain-chunk:terrain-chunk-p d)
                     (let* ((x-chunk (elt ends 0))
                            (z-chunk (elt ends 2))
                            (y   (game-state:approx-terrain-height@pos state
                                                                       x-chunk
                                                                       z-chunk)))
                       (when (and y (< (elt ends 1) y))
                         (return-from nonlabyrinth-element-hitted-by-ray nil))))
                    ((labyrinth-mesh-p d)
                     ;;does nothing, continue to the next iteration
                     )
                    ((arrows:arrowp d)
                     ;;does nothing, continue to the next iteration
                     )
                    ((trap-mesh-shell-p d)
                     ;;does nothing, continue to the next iteration
                     )
                    ((tree-mesh-shell-p d)
                     ;;(misc:dbg "tree trunk ~%~a ~a -> ~a" (tree-trunk-aabb d)
                     ;;        ends (insidep (tree-trunk-aabb d) ends))
                     (when (and (insidep (aabb d) ends)
                                (insidep (actual-aabb-for-visibility d) ends))
                       (if (= (id d) (id target))
                           (return-from nonlabyrinth-element-hitted-by-ray (values ray d))
                           (return-from nonlabyrinth-element-hitted-by-ray nil))))
                    (t
                     (when (ensure-not-dead (d)
                             (insidep (actual-aabb-for-visibility d)
                                      (ray-ends ray pos))) ;; O_O
                       (if (= (id d) (id target))
                           (return-from nonlabyrinth-element-hitted-by-ray (values ray d))
                           (when (not (= (id d) (id object)))
                             ;; (misc:dbg "hitted?? ~a" d)
                             (return-from
                              nonlabyrinth-element-hitted-by-ray (values nil d)))))))))))))

(defun %blocked-by-ray-p (ray-ends vec-object)
  (loop for a across vec-object do
       (when (insidep (actual-aabb-for-visibility a) ray-ends)
         (return-from %blocked-by-ray-p a)))
  nil)

(defmethod labyrinth-element-hitted-by-ray ((object able-to-see-mesh) (target triangle-mesh))
  (with-accessors ((dir dir)
                   (pos pos)
                   (state state)
                   (visibility-cone visibility-cone)) object
    ;; launch a ray
    (let* ((ray (make-instance 'ray
                               :ray-direction (normalize (vec- (aabb-center (aabb target))
                                                               (aabb-center (aabb object))))))
           (all-labyrinths (loop for l being the hash-value in
                                (game-state:labyrinth-entities (state object))
                              collect l)))
      (loop
         for dt from 0.0
         below (vec-length (cone-height visibility-cone))
         by +visibility-ray-displ-incr+ do
           (incf (displacement ray) dt)
           (let* ((ends (ray-ends ray pos)))
             (loop for lab in all-labyrinths do
                  (when (insidep (aabb lab) ends)
                    (let ((walls    (children (wall-instanced    lab)))
                          (windows  (children (window-instanced  lab)))
                          (pillars  (children (pillar-instanced  lab)))
                          (doors-n  (remove-if #'openp (children (door-n-instanced lab))))
                          (doors-s  (remove-if #'openp (children (door-s-instanced lab))))
                          (doors-e  (remove-if #'openp (children (door-e-instanced lab))))
                          (doors-w  (remove-if #'openp (children (door-w-instanced lab))))
                          (tables   (children (table-instanced   lab)))
                          (chairs-n (children (chair-n-instanced lab)))
                          (chairs-s (children (chair-s-instanced lab)))
                          (chairs-e (children (chair-e-instanced lab)))
                          (chairs-w (children (chair-w-instanced lab))))
                      (let ((res (or (%blocked-by-ray-p ends walls)
                                     (%blocked-by-ray-p ends windows)
                                     (%blocked-by-ray-p ends pillars)
                                     (%blocked-by-ray-p ends doors-n)
                                     (%blocked-by-ray-p ends doors-s)
                                     (%blocked-by-ray-p ends doors-e)
                                     (%blocked-by-ray-p ends doors-w)
                                     (%blocked-by-ray-p ends tables)
                                     (%blocked-by-ray-p ends chairs-n)
                                     (%blocked-by-ray-p ends chairs-s)
                                     (%blocked-by-ray-p ends chairs-e)
                                     (%blocked-by-ray-p ends chairs-w))))
                        (when res
                          (return-from labyrinth-element-hitted-by-ray (values ray res))))))))))))

(defun calc-end-line-sight (player)
  (with-accessors ((pos pos)
                   (dir dir)
                   (state state)
                   (visibility-cone able-to-see-mesh:visibility-cone)) player
    (let* ((line-sight     (cone-height visibility-cone)))
      (ivec2:ivec2-length (map-utils:pos-entity-chunk->cost-pos line-sight)))))

(defun calc-angle-sight (player)
  (with-accessors ((visibility-cone able-to-see-mesh:visibility-cone)) player
    (d* 2.0 (half-angle visibility-cone))))

(defun visible-more-p (ids-visible-a ids-visible-b)
  (or (< (length ids-visible-a)
         (length ids-visible-b))
      (set-difference ids-visible-b
                      ids-visible-a
                      :test #'=)))
