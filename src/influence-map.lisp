;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

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

(in-package :influence-map)

(defclass dijkstra-layer ()
  ((layer
    :initform nil
    :initarg  :layer
    :accessor layer
    :type matrix)))

(defmethod marshal:class-persistant-slots ((object dijkstra-layer))
  '(layer))

(defmethod matrix-elt ((object dijkstra-layer) row col)
  (matrix-elt (layer object) row col))

(defun skippablep (type pos)
  (declare (ignore pos))
  (find type
        (list +wall-type+
              +tree-type+
              +furniture-type+
              +magic-furniture-type+
              +container-type+
              +pillar-type+
              +chair-type+
              +table-type+)
        :test #'eq))

(defgeneric smooth-dijkstra-layer (object state &key skippable-predicate))

(defgeneric next-dijkstra-position (object entity-player scale-factor-cost-concern
                                    &key find-cost-map-fn))

;; note: keep the order of tiles deterministic!
(defun get-djk-neigh-fn ()
  (flet ((state-neigh-fn (column row w-offset h-offset &key add-center)
           (declare (ignore w-offset h-offset))
           (gen-4-neighbour-ccw-vector column row
                                       :add-center add-center)))
  #'(lambda (game-state row column predicate)
      (game-state:get-neighborhood game-state row column predicate
                                   :neigh-fn #'state-neigh-fn))))

(defmethod smooth-dijkstra-layer ((object dijkstra-layer) (state game-state)
                                  &key
                                    (skippable-predicate #'skippablep)
                                    (neigh-fn (get-djk-neigh-fn)))
  "Note: skipped tiles will get nil as value!"
  (labels ((neig-comp (n i)
             (elt (cdr n) i))
           (neig-row (n)
             (neig-comp n 1))
           (neig-col (n)
             (neig-comp n 0))
           (islandp (neigh matrix)
             (or (misc:vector-empty-p neigh)
                 (every #'(lambda (a)
                            (null (matrix-elt matrix (neig-row a) (neig-col a))))
                        neigh))))
    (let* ((modifiedp       nil)
           (matrix          (layer object))
           (working-copy    (clone matrix))
           (not-skippable-p #'(lambda (el pos)
                                (not (funcall skippable-predicate (el-type el) pos)))))
      (loop-matrix (working-copy x y)
         (if (not (funcall skippable-predicate (el-type-in-pos state x y) (ivec2 x y)))
             (let* ((neighbour (funcall neigh-fn state y x not-skippable-p))
                    (center    (matrix-elt working-copy y x))
                    (min       (loop for cell across neighbour
                                  when (matrix-elt working-copy
                                                   (elt (cdr cell) 1)  ; row
                                                   (elt (cdr cell) 0)) ; column
                                  minimize
                                    (matrix-elt working-copy
                                                (elt (cdr cell) 1)     ; row
                                                (elt (cdr cell) 0))))) ; column
               (when (and center
                          ;; if the tile is  an "island", do not
                          ;; change the value
                          (not (islandp neighbour working-copy))
                          (d>= center (d+ (d min) 2.0)))
                 (setf modifiedp t)
                 (setf (matrix-elt working-copy y x)
                       (d+ 1.0 (d min)))))
             (when (not (null (matrix-elt working-copy y x)))
               (setf modifiedp t)
               (setf (matrix-elt working-copy y x) nil))))
      (if (not modifiedp)
          object
          (progn
            (setf (layer object) working-copy)
            (smooth-dijkstra-layer object state
                                   :skippable-predicate skippable-predicate))))))

(defmethod next-dijkstra-position ((object dijkstra-layer)
                                   entity-player
                                   scale-factor-cost-concern
                                   &key (find-cost-map-fn #'next-dijk-pos-cost-map-fn))
  (next-dijkstra-position (layer object) entity-player
                          scale-factor-cost-concern
                          :find-cost-map-fn find-cost-map-fn))

(defun next-dijk-pos-cost-map-fn (entity pos)
  (with-accessors ((state state)) entity
    (2d-utils:displace-2d-vector (pos x y)
      (if (game-state:door@pos-p state x y) ; doors as if they have no cost
          +open-terrain-cost+
          (game-state:get-cost state x y)))))

(defmethod next-dijkstra-position ((object matrix) entity-player
                                   scale-factor-cost-concern
                                   &key (find-cost-map-fn #'next-dijk-pos-cost-map-fn))
  "Return values: new path, cost to reach new position (as specified by find-cost-map-fn)"
  (with-accessors ((state state)) entity-player
    (labels ((find-cost-layer (a)
               (let ((raw (matrix:matrix-elt object (elt a 1) (elt a 0))))
                 (and raw
                      (d* scale-factor-cost-concern raw)))))
      (let* ((position (map-utils:pos-entity-chunk->cost-pos (pos entity-player)))
             (x-player (elt position 0))
             (y-player (elt position 1))
             (neighbour-elements (gen-valid-4-neighbour-counterclockwise object
                                                                         x-player
                                                                         y-player
                                                                         :add-center nil))
             (remove-predicate   #'(lambda (a)   (null (find-cost-layer a))))
             ;; null cost  are removed from  'neighbour-elements' when
             ;; this function is  used, below, so calling  #'< can not
             ;; fail here
             (compare-fn         #'(lambda (a b) (< (find-cost-layer a)
                                                    (find-cost-layer b))))
             (min                     (handler-bind ((error
                                                      #'(lambda (c)
                                                          (declare (ignore c))
                                                          (invoke-restart 'use-value nil))))
                                        (find-min-max compare-fn
                                                      (remove-if remove-predicate
                                                                 neighbour-elements)))))
        (if min
            (values (vector position
                            (sequence->ivec2 min))
                    (funcall find-cost-map-fn entity-player min))
            (values nil nil)))))) ; no valid move found

(defun make-dijkstra-layer (w h bg-value)
  (make-instance 'dijkstra-layer
                 :layer (make-matrix w h (d bg-value))))

(defun wrap-matrix-as-dijkstra-map (matrix)
  (make-instance 'dijkstra-layer
                 :layer matrix))

(defun layer->pixmap (map &key (key #'identity))
  (flet ((gen-map-fn (m q)
           #'(lambda (x)
               (d+ (d* m x) q))))
    (let* ((matrix   (funcall key map))
           (max      (loop for i across (data matrix) when (numberp i) maximize i))
           (min      (loop for i across (data matrix) when (numberp i) minimize i))
           (line     (2d-utils:line-eqn (vec2:vec2 min 0.0) (vec2:vec2 max 1.0)))
           (map-fn   (gen-map-fn (d (elt line 0)) (d (elt line 1))))
           (output   (pixmap:make-pixmap (width matrix) (height matrix)))
           (gradient (color-utils:make-gradient
                      (color-utils:make-gradient-color 0.0 §c0000ffff)
                      (color-utils:make-gradient-color 1.0 §cff0000ff))))
      (color-utils:add-color gradient
                             (color-utils:make-gradient-color (funcall map-fn 0.0)
                                                              §c000000ff))
      (loop-matrix (output x y)
        (let ((value-matrix (matrix-elt matrix y x)))
          (if value-matrix
              (setf (pixel@ output x y)
                    (color-utils:vec4->ubvec4
                     (color-utils:pick-color gradient
                                             (funcall map-fn value-matrix))))
              (setf (pixel@ output x y)
                    (ubvec4:ubvec4 0 0 0 255)))))
      output)))

(defun dijkstra-layer->pixmap (map)
  (layer->pixmap map :key #'layer))

(defun linear-decay-fn (max)
  (let ((m (if (d< max 0.0)
               2.0
               -2.0))
        (clamp-fn (if (d< max 0.0)
                      #'min
                      #'max)))
    #'(lambda (dist)
        (d (funcall clamp-fn 0.0
                    (d+ (d* m (d dist)) max))))))

(defun %apply-influence (matrix influence xin yin &key (decay-fn (linear-decay-fn influence)))
  "coordinates in cost space"
  (let ((from (vec2 (d xin) (d yin))))
    (loop-matrix (matrix x y)
       (let ((dist (vec2-length (vec2- from (vec2 (d x) (d y))))))
         (setf (matrix-elt matrix y x)
               (d+ (matrix-elt matrix y x)
                   (funcall decay-fn dist)))))))

(defgeneric apply-influence (object influencer))

(defmethod apply-influence ((object matrix) (influencer mesh:triangle-mesh))
  (with-accessors ((state state) (entity-pos pos) (entity-id id)) influencer
    (let ((coord          (map-utils:pos-entity-chunk->cost-pos entity-pos))
          (influence-sign (if (game-state:faction-player-p state entity-id)
                              -1.0
                              1.0)))
      (%apply-influence object
                      (d* influence-sign (character:calculate-influence (ghost influencer)))
                      (elt coord 0)
                      (elt coord 1)))))

(defmethod apply-influence ((object matrix) (influencer list))
  (map nil
       #'(lambda (v) (apply-influence object v))
       influencer))

(defun im->pixmap (map)
  (layer->pixmap map))
