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

(define-constant +explored-tile-value+             100.0 :test #'=)

(define-constant +unexplored-tile-value+             0.0 :test #'=)

(define-constant +concerning-tile-value+           100.0 :test #'=)

(define-constant +attack-nongoal-tile-value+        5.0 :test #'=)

(define-constant +length-line-of-sight-inf+       1000.0 :test #'=)

(define-constant +goal-tile-value+                   0.0 :test #'=)

(defclass blackboard ()
  ((main-state
    :initform nil
    :initarg  :main-state
    :accessor main-state
    :type     game-state)
   (visited-tiles
    :initform nil
    :initarg  :visited-tiles
    :accessor visited-tiles
    :type matrix)
   (unexplored-layer
    :initform nil
    :initarg  :unexplored-layer
    :accessor unexplored-layer
    :type     dijkstra-layer)
   (concerning-tiles
    :initform nil
    :initarg  :concerning-tiles
    :accessor concerning-tiles
    :type     matrix)
   (attack-enemy-melee-layer
    :initform nil
    :initarg  :attack-enemy-melee-layer
    :accessor attack-enemy-melee-layer
    :type     dijkstra-layer
    :documentation "This holds the position the player with melee weapon (except pole weapon) should reach to attack the enemy")
   (use-enemy-fov-when-exploring
    :initform nil
    :initarg  :use-enemy-fov-when-exploring-p
    :reader   use-enemy-fov-when-exploring-p
    :writer   (setf use-enemy-fov-when-exploring))))

(defmethod initialize-instance :after ((object blackboard) &key &allow-other-keys)
  (with-accessors ((visited-tiles    visited-tiles)
                   (concerning-tiles concerning-tiles)
                   (unexplored-layer unexplored-layer)
                   (attack-enemy-melee-layer attack-enemy-melee-layer)
                   (main-state       main-state)) object
    (let ((wmap (width  (map-state main-state)))
          (hmap (height (map-state main-state))))
      (setf visited-tiles    (make-matrix wmap hmap nil))
      (setf concerning-tiles (make-matrix wmap hmap 0.0))
      (setf unexplored-layer (inmap:make-dijkstra-layer wmap hmap +unexplored-tile-value+))
      (setf attack-enemy-melee-layer
            (inmap:make-dijkstra-layer wmap hmap +attack-nongoal-tile-value+)))))

(defun decrease-concerning (game-state concerning-map)
  (let* ((level           (level-difficult game-state))
         (decrease-rate   (d  (/ 1 level)))
         (decrease-amount (d/ +concerning-tile-value+ 3.0)))
    (loop-matrix (concerning-map x y)
       (when (d> (matrix-elt concerning-map y x) 0.0)
         (let ((new-value (d- (matrix-elt concerning-map y x)
                              (d* decrease-rate decrease-amount))))
           (if (d> new-value 0.0)
               (setf (matrix-elt concerning-map y x) new-value)
               (setf (matrix-elt concerning-map y x) 0.0)))))))

(defmethod game-event:on-game-event ((object blackboard) (event game-event:end-turn))
  (with-accessors ((concerning-tiles concerning-tiles)) object
    (decrease-concerning (main-state object) concerning-tiles)
    (update-attack-melee-layer object)
    ;; debug ;;;;;;
    ;; (let ((pixmap (inmap:dijkstra-layer->pixmap
    ;;                (attack-enemy-melee-layer object))))
    ;;   (pixmap:save-pixmap pixmap (fs:file-in-package "exploring.tga")))
    nil))

(defmethod main-state ((object blackboard))
  (slot-value object 'main-state))

(defmethod (setf main-state) (new-state (object blackboard))
  (with-slots (main-state) object
    (setf main-state new-state)))

(defgeneric update-unexplored-layer (object))

(defgeneric update-unexplored-layer-player (object player &key all-visibles-from-ai))

(defgeneric next-unexplored-position (object player))

(defgeneric update-attack-melee-layer (object))

(defgeneric update-attack-melee-layer-player (object player &key all-visibles-from-ai))

(defmethod set-tile-visited ((object blackboard) x y)
  (call-next-method object
                    (map-utils:coord-chunk->matrix x)
                    (map-utils:coord-chunk->matrix y)))

(defmethod set-tile-visited ((object blackboard) (x fixnum) (y fixnum))
  (with-accessors ((visited-tiles visited-tiles)
                   (main-state main-state)
                   (unexplored-layer unexplored-layer)) object
    ;; mark visited
    (setf (matrix-elt visited-tiles y x) t)
    (update-unexplored-layer object)))

(defun calc-danger-zone-size (difficult-level)
  (ceiling (dlerp (smoothstep-interpolate 2.0 3.0 difficult-level)
                  1.0
                  3.0)))

(defmethod set-concerning-tile ((object blackboard) x y)
  (with-accessors ((concerning-tiles concerning-tiles)
                   (main-state main-state)) object
    (let* ((danger-zone-size    (calc-danger-zone-size (level-difficult main-state)))
           (dangerous-tiles-pos (gen-neighbour-position-in-box x
                                                               y
                                                               danger-zone-size
                                                               danger-zone-size)))
      (loop for point across dangerous-tiles-pos do
           (displace-2d-vector (point x y)
             (with-check-matrix-borders (concerning-tiles x y)
               (incf (matrix-elt concerning-tiles y x) +concerning-tile-value+)))))))

(defun calc-enemy-danger-zone-size (player)
  (with-accessors ((main-state state)
                   (ghost ghost)) player
    (let* ((level          (level-difficult main-state))
           (weapon-type    (character:weapon-type (ghost player)))
           (range          (case weapon-type
                             ((:edge :impact)
                              (+ 1 level +weapon-melee-range+))
                             (:pole
                              (+ 1 level +weapon-pole-range+))
                             (:bow
                              +weapon-bow-range+)
                             (:crossbow
                              +weapon-crossbow-range+)
                             (otherwise
                              +weapon-melee-range+)))
           (movement-weight (if (> level 1)
                                (/ level 25)
                                0)))
      (floor (+ range
                (* movement-weight
                   (character:actual-movement-points ghost)))))))

(defun reset-layer (djk-map value)
  (with-accessors ((layer layer)) djk-map
    ;; reset layer
    (loop-matrix (layer x y)
       (setf (matrix-elt layer y x) value))))

(defun reset-explored-layer (unexplored-layer)
  (reset-layer unexplored-layer +unexplored-tile-value+))

(defun reset-attack-layer (unexplored-layer)
  (reset-layer unexplored-layer +attack-nongoal-tile-value+))

(defun all-player-id-visible-from-ai (game-state)
  (let ((all-visibles '()))
    (map-ai-entities game-state
                     #'(lambda (k v)
                         (declare (ignore k))
                         (let ((visibles (able-to-see-mesh:other-faction-visible-players v)))
                           (loop for visible in visibles do
                                (pushnew visible all-visibles :key #'id :test #'=)))))
    (map 'list #'id all-visibles)))

(defun nsuperimpose-layer (from destination &key (fn #'(lambda (a b)
                                                         (declare (ignore a))
                                                         b)))
  (loop-matrix (destination x y)
     (setf (matrix-elt destination y x)
           (funcall fn (matrix-elt from y x) (matrix-elt destination y x)))))

(defun matrix-update-values (layer positions visible-positions new-value)
  (loop for point across positions do
       (displace-2d-vector (point x y)
         (with-check-matrix-borders (layer x y)
           (when (find point visible-positions :test #'ivec2=)
             (setf (matrix-elt layer y x) new-value))))))

(defmethod update-unexplored-layer ((object blackboard))
  (with-accessors ((visited-tiles visited-tiles)
                   (main-state main-state)
                   (concerning-tiles concerning-tiles)
                   (unexplored-layer unexplored-layer)) object
    (with-accessors ((layer layer)) unexplored-layer
      (reset-explored-layer unexplored-layer)
      (let ((all-visibles (all-player-id-visible-from-ai main-state)))
        ;; calculate concerning tiles tiles occupied by enemies
        (map-player-entities main-state
                             #'(lambda (k player)
                                 (declare (ignore k))
                                 (update-unexplored-layer-player object
                                                                 player
                                                                 :all-visibles-from-ai
                                                                 all-visibles)))
        ;; add-concerning tiles
        (nsuperimpose-layer concerning-tiles layer)
        ;; (loop-matrix (layer x y)
        ;;    (setf (matrix-elt layer            y x)
        ;;          (matrix-elt concerning-tiles y x)))
        ;; mark already visited tiles
        (loop-matrix (layer x y)
           (when (matrix-elt visited-tiles y x)
             (setf (matrix-elt layer y x) +explored-tile-value+)))
        ;; smooth the map here
        (let ((skip-fn #'(lambda (el-type pos)
                           (let ((chunk-x (map-utils:coord-map->chunk (elt pos 0)))
                                 (chunk-z (map-utils:coord-map->chunk (elt pos 1))))
                             (or (inmap:skippablep el-type pos)
                                 (find (entity-id-in-pos main-state (elt pos 0) (elt pos 1))
                                       all-visibles
                                       :test #'=)
                                 (d< (approx-terrain-height@pos main-state chunk-x chunk-z)
                                     +zero-height+))))))
          (inmap:smooth-dijkstra-layer (blackboard:unexplored-layer object)
                                       main-state
                                       :skippable-predicate skip-fn))))))

(defmethod update-unexplored-layer-player ((object blackboard) (player md2-mesh:md2-mesh)
                                           &key
                                             (all-visibles-from-ai
                                              (all-player-id-visible-from-ai (main-state player))))
  (with-accessors ((unexplored-layer unexplored-layer)
                   (visited-tiles    visited-tiles)
                   (concerning-tiles concerning-tiles)
                   (main-state       main-state)) object
    (with-accessors ((layer layer)) unexplored-layer
      (cond
        ((faction-player-p main-state (id player))
         (when (find (id player) all-visibles-from-ai :test #'=)
           (let* ((max-angle           (if (use-enemy-fov-when-exploring-p object)
                                           (calc-angle-sight player)
                                           +2pi+))
                  (max-line-sight      (if (use-enemy-fov-when-exploring-p object)
                                           +length-line-of-sight-inf+
                                           (calc-end-line-sight player)))
                  (visible-tiles       (2d-ray-casting player
                                                       :max-angle-sight max-angle
                                                       :end-line-sight  max-line-sight))
                  (position            (pos-entity-chunk->cost-pos (pos player)))
                  (x-player            (elt position 0))
                  (y-player            (elt position 1))
                  (danger-zone-size    (calc-enemy-danger-zone-size player))
                  (dangerous-tiles-pos (gen-neighbour-position-in-box x-player
                                                                      y-player
                                                                      danger-zone-size
                                                                      danger-zone-size)))
             (loop for point across dangerous-tiles-pos do
                  (displace-2d-vector (point x y)
                                      (with-check-matrix-borders (layer x y)
                                        (when (find point visible-tiles :test #'ivec2=)
                                          (setf (matrix-elt concerning-tiles y x)
                                                +concerning-tile-value+))))))))))))

(defmethod next-unexplored-position ((object blackboard) (player md2-mesh:md2-mesh))
  (with-accessors ((unexplored-layer unexplored-layer)) object
    (with-accessors ((layer layer)) unexplored-layer
      (let* ((position (pos-entity-chunk->cost-pos (pos player)))
             (x-player (elt position 0))
             (y-player (elt position 1))
             (neighbour-elements (remove-if-not
                                  #'(lambda (a)
                                      (displace-2d-vector (a x y)
                                        (valid-index-p layer y x)))
                                  (gen-4-neighbour-counterclockwise x-player
                                                                    y-player
                                                                    :add-center nil)))
             (compare-fn         #'(lambda (a b)
                                     (let ((cost-a (matrix-elt layer
                                                               (elt a 1)
                                                               (elt a 0)))
                                           (cost-b (matrix-elt layer
                                                               (elt b 1)
                                                               (elt b 0))))
                                       (cond
                                         ((null cost-a)
                                          nil)
                                         ((null cost-b)
                                          t)
                                         (t
                                          (< cost-a cost-b))))))
             (min                (find-min-max compare-fn neighbour-elements)))
        (list (sequence->ivec2 min))))))

(defun %2d-ray-stopper-melee-fn (player x y)
  "x and y in cost-map space (integer coordinates)"
  (with-accessors ((state state)) player
    (let* ((center         (aabb-center (mesh:aabb player)))
           (elevation      (elt center 1)))
      (cond
        ((d> (approx-terrain-height@pos state
                                        (coord-map->chunk x)
                                        (coord-map->chunk y))
             elevation)
         t)
        ((and (door@pos-p state x y)
              (mesh:openp (find-entity-by-id state (entity-id-in-pos state x y))))
         nil)
        ((and (occludep-in-pos state x y)
              (not (map-element-occupied-by-character-p (element-mapstate@ state x y))))
         ;; the ray does not take into account characters
         t)
        (t  ;; otherwise
         nil)))))

(defmethod update-attack-melee-layer-player ((object blackboard) (player md2-mesh:md2-mesh)
                                             &key
                                               (all-visibles-from-ai
                                                (all-player-id-visible-from-ai (main-state player))))
  (with-accessors ((main-state main-state)
                   (attack-enemy-melee-layer attack-enemy-melee-layer)) object
    (with-accessors ((layer layer)) attack-enemy-melee-layer
      (cond
        ((faction-player-p main-state (id player))
         (when (find (id player) all-visibles-from-ai :test #'=)
           (let* ((position (pos-entity-chunk->cost-pos (pos player)))
                  (x-player (elt position 0))
                  (y-player (elt position 1))
                  (goal-tiles-pos (gen-4-neighbour-counterclockwise x-player
                                                                    y-player
                                                                    :add-center nil)))
             (setf goal-tiles-pos (remove-if #'(lambda (a)
                                                 (or (not (element@-inside-p layer
                                                                             (elt a 0)
                                                                             (elt a 1)))
                                                     (skippablep (el-type-in-pos main-state
                                                                                 (elt a 0)
                                                                                 (elt a 1))
                                                                 nil)))
                                             goal-tiles-pos))

             (loop
                for point in goal-tiles-pos
                when (2d-tile-visible-p main-state
                                        (sequence->ivec2 point)
                                        #'%2d-ray-stopper-melee-fn)
                do
                  (displace-2d-vector (point x y)
                    (with-check-matrix-borders (layer x y)
                      (setf (matrix-elt layer y x)
                            +goal-tile-value+)))))))))))

(defmethod update-attack-melee-layer ((object blackboard))
  (with-accessors ((main-state main-state)
                   (concerning-tiles concerning-tiles)
                   (attack-enemy-melee-layer attack-enemy-melee-layer)
                   (unexplored-layer unexplored-layer)) object
    (with-accessors ((layer layer)) attack-enemy-melee-layer
      (reset-attack-layer attack-enemy-melee-layer)
      (let ((all-visibles (all-player-id-visible-from-ai main-state)))
        ;; add-concerning tiles
        ;(nsuperimpose-layer concerning-tiles layer :fn #'max)
        (map-player-entities main-state  #'(lambda (k player)
                                             (declare (ignore k))
                                             (update-attack-melee-layer-player object
                                                                               player
                                                                               :all-visibles-from-ai
                                                                               all-visibles)))
        (inmap:smooth-dijkstra-layer attack-enemy-melee-layer main-state)))))

(defun calc-end-line-sight (player)
  (with-accessors ((pos pos)
                   (dir dir)
                   (state state)
                   (visibility-cone able-to-see-mesh:visibility-cone)) player
    (let* ((line-sight     (cone-height visibility-cone)))
      (ivec2-length (pos-entity-chunk->cost-pos line-sight)))))

(defun calc-angle-sight (player)
  (with-accessors ((visibility-cone able-to-see-mesh:visibility-cone)) player
    (d* 2.0 (half-angle visibility-cone))))

(defun %2d-ray-stopper-fn (player x y)
  "x and y in cost-map space (integer coordinates)"
  (with-accessors ((state state)) player
    (let* ((center         (aabb-center (mesh:aabb player)))
           (elevation      (elt center 1)))
      (or (d> (approx-terrain-height@pos state
                                         (coord-map->chunk x)
                                         (coord-map->chunk y))
              elevation)
          (not (map-element-empty-p (element-mapstate@ state x y)))))))

(defun 2d-ray-casting (player
                       &key
                         (ray-stopper-fn  #'%2d-ray-stopper-fn)
                         (angle-sweep-inc 0.01)
                         (end-line-sight  (calc-end-line-sight player))
                         (max-angle-sight (calc-angle-sight player)))
  (with-accessors ((pos pos)
                   (dir dir)
                   (state state)
                   (visibility-cone able-to-see-mesh:visibility-cone)) player
    (let* ((player-pos     (pos->game-state-pos player))
           (start-vec      (2d-vector-rotate (vec2 (elt dir 0) (elt dir 2))
                                             (d- (half-angle visibility-cone))))
           (saw-so-far     '()))
      (loop
         for angle from 0.0 below max-angle-sight by angle-sweep-inc do
           (let* ((rotated-dir (2d-vector-rotate start-vec angle))
                  (end         (vec2*   (vec2 (elt rotated-dir 0)
                                              (elt rotated-dir 1))
                                        (d end-line-sight)))
                  (line        (segment (ivec2 0 0)
                                        (ivec2 (round (elt end 0))
                                               (round (elt end 1))))))
             (when line
               ;; translate
               (setf line (map 'list #'(lambda (a)
                                         (ivec2+ a player-pos))
                               line))
               (pushnew (first line) saw-so-far :test #'ivec2=)
               ;; launch ray
               (loop named draw-loop for point in (rest line) do
                    (displace-2d-vector (point x y)
                      (with-check-matrix-borders ((map-state state) x y)
                        (if (funcall ray-stopper-fn player x y)
                            (return-from draw-loop nil)
                            (pushnew point saw-so-far :test #'ivec2=))))))))
      saw-so-far)))

(defun 2d-tile-visible-player-p (player tile-position
                                 &key
                                   (ray-stopper-fn  #'%2d-ray-stopper-fn)
                                   (end-line-sight  (calc-end-line-sight player)))
  (with-accessors ((pos pos)
                   (dir dir)
                   (state state)
                   (visibility-cone able-to-see-mesh:visibility-cone)) player
    (let* ((player-pos             (pos->game-state-pos player))
           (player-pos-3d          (sb-cga:vec (d (elt player-pos 0)) 0.0 (d (elt player-pos 1))))
           (dir-to-tile            (ivec2- tile-position player-pos))
           (tile-pos-terrain-space (sb-cga:vec (coord-map->chunk (elt tile-position 0))
                                               (elt pos 1)
                                               (coord-map->chunk (elt tile-position 1)))))
      (when (point-in-cone-p visibility-cone tile-pos-terrain-space)
        (let* ((ray (make-instance 'ray
                                   :ray-direction
                                   (sb-cga:normalize (sb-cga:vec (d (elt dir-to-tile 0))
                                                                 0.0
                                                                 (d (elt dir-to-tile 1))))
                                   :displacement 1e-5)))
          (do ()
              ((let ((ends (3d-utils:ray-ends ray player-pos-3d)))
                 (if (element@-inside-p (map-state state)
                                        (round (elt ends 0))
                                        (round (elt ends 2)))
                     (or (funcall ray-stopper-fn player
                                  (round (elt ends 0))
                                  (round (elt ends 2)))
                         (>= (sb-cga:vec-length (sb-cga:vec- ends player-pos-3d))
                             end-line-sight))
                     t)))
            (incf (displacement ray) .1))
          (let* ((ends (ray-ends ray player-pos-3d))
                 (line-discrete (segment player-pos
                                         (ivec2 (round (elt ends 0))
                                                (round (elt ends 2))))))
            (loop for point in line-discrete do
                 (when (ivec2= point tile-position)
                   (return-from 2d-tile-visible-player-p t)))
            nil))))))

(defun 2d-tile-visible-p (game-state tile-position ray-stopper-fn)
  (map-ai-entities game-state #'(lambda (k v)
                                  (declare (ignore k))
                                  (when (2d-tile-visible-player-p v
                                                                  tile-position
                                                                  :ray-stopper-fn ray-stopper-fn)
                                    (return-from 2d-tile-visible-p t))))
  nil)
