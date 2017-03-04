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

(define-constant +length-line-of-sight-inf+       1000.0 :test #'=)

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
   (risky-exploring
    :initform t
    :initarg  :risky-exploring-p
    :reader   risky-exploring-p
    :writer   (setf risky-exploring))))

(defmethod initialize-instance :after ((object blackboard) &key &allow-other-keys)
  (with-accessors ((visited-tiles    visited-tiles)
                   (concerning-tiles concerning-tiles)
                   (unexplored-layer unexplored-layer)
                   (main-state       main-state)) object
    (let ((wmap (width  (map-state main-state)))
          (hmap (height (map-state main-state))))
      (setf visited-tiles    (make-matrix wmap hmap nil))
      (setf concerning-tiles (make-matrix wmap hmap 0.0))
      (setf unexplored-layer (inmap:make-dijkstra-layer wmap hmap +unexplored-tile-value+)))))

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
    (decrease-concerning (main-state object) concerning-tiles)))

(defmethod main-state ((object blackboard))
  (slot-value object 'main-state))

(defmethod (setf main-state) (new-state (object blackboard))
  (with-slots (main-state) object
    (setf main-state new-state)))

(defgeneric update-unexplored-layer (object))

(defgeneric update-unexplored-layer-player (object player &key all-visibles-from-ai))

(defgeneric next-unexplored-position (object player))

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
    ;; ;; debug ;;;;;;
    ;; (let ((pixmap (inmap:dijkstra-layer->pixmap
    ;;                (unexplored-layer object))))
    ;;   ;; (texture (first (texture:get-texture texture:+influence-map+))))
    ;;   ;; (setf pixmap (matrix:scale-matrix-nearest pixmap 8.0 8.0))
    ;;   ;; (setf (pixmap:data texture) (pixmap:data pixmap))
    ;;   ;; (pixmap:sync-data-to-bits texture)
    ;;   ;; (update-for-rendering texture))))))
    ;;   (setf (pixel@ pixmap x y) (ubvec4:ubvec4 0 255 0 255))
    ;;   (pixmap:save-pixmap pixmap (fs:file-in-package "exploring.tga")))))

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
                              (1+ +weapon-melee-range+))
                             (:pole
                              (1+ +weapon-pole-range+))
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

(defun reset-explored-layer (unexplored-layer)
  (with-accessors ((layer layer)) unexplored-layer
      ;; reset layer
    (loop-matrix (layer x y)
       (setf (matrix-elt layer y x) +unexplored-tile-value+))))

(defun all-player-id-visible-from-ai (game-state)
  (let ((all-visibles '()))
    (map-ai-entities game-state
                     #'(lambda (k v)
                         (declare (ignore k))
                         (let ((visibles (able-to-see-mesh:other-faction-visible-players v)))
                           (loop for visible in visibles do
                                (pushnew visible all-visibles :key #'id :test #'=)))))
    (map 'list #'id all-visibles)))

(defmethod update-unexplored-layer ((object blackboard))
  (with-accessors ((visited-tiles visited-tiles)
                   (main-state main-state)
                   (concerning-tiles concerning-tiles)
                   (unexplored-layer unexplored-layer)) object
    (with-accessors ((layer layer)) unexplored-layer
      (reset-explored-layer unexplored-layer)
      (let ((all-visibles (all-player-id-visible-from-ai main-state)))
        ;; calculate concerning tiles tiles occupied by enemies
        (map-player-entities main-state  #'(lambda (k player)
                                             (declare (ignore k))
                                             (update-unexplored-layer-player object
                                                                             player
                                                                             :all-visibles-from-ai
                                                                             all-visibles)))
        ;; add-concerning tiles
        (loop-matrix (layer x y)
           (setf (matrix-elt layer            y x)
                 (matrix-elt concerning-tiles y x)))
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
         (let* ((max-angle           (if (risky-exploring-p object)
                                         (calc-angle-sight player)
                                         +2pi+))
                (max-line-sight      (if (risky-exploring-p object)
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
           (when (find (id player) all-visibles-from-ai :test #'=)
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

(defun 2d-ray-casting (player
                       &key
                         (end-line-sight  (calc-end-line-sight player))
                         (max-angle-sight (calc-angle-sight player)))
  (with-accessors ((pos pos)
                   (dir dir)
                   (state state)
                   (visibility-cone able-to-see-mesh:visibility-cone)) player
    (let* ((center         (aabb-center (mesh:aabb player)))
           (player-pos     (pos->game-state-pos player))
           (elevation      (elt center 1))
           (start-vec      (2d-vector-rotate (vec2 (elt dir 0) (elt dir 2))
                                             (d- (half-angle visibility-cone))))
           (saw-so-far     '()))
      (loop
         for angle from 0.0 below max-angle-sight by 0.01 do
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
               ;; lauch ray
               (loop named draw-loop for point in (rest line) do
                    (displace-2d-vector (point x y)
                      (with-check-matrix-borders ((map-state state) x y)
                        (if (or (d> (approx-terrain-height@pos state
                                                               (coord-map->chunk x)
                                                               (coord-map->chunk y))
                                    elevation)
                                (not (map-element-empty-p (element-mapstate@ state x y))))
                            (return-from draw-loop nil)
                            (pushnew point saw-so-far :test #'ivec2=))))))))
      saw-so-far)))
