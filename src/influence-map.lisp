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

(in-package :influence-map)

(defclass dijkstra-layer ()
  ((layer
    :initform nil
    :initarg  :layer
    :accessor layer
    :type matrix)))

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

(defgeneric next-dijkstra-position (object entity-player scale-factor-cost-concern))

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
  (let* ((matrix          (layer object))
         (working-copy    (clone matrix))
         (not-skippable-p #'(lambda (el pos) (not (funcall skippable-predicate (el-type el) pos)))))
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
                       ;; if empty the tile is  an "island", so do not
                       ;; change the value
                       (not (misc:vector-empty-p neighbour))
                       (d>= center (d+ (d min) 2.0)))
              (setf (matrix-elt working-copy y x)
                    (d+ 1.0 (d min)))))
          (setf (matrix-elt working-copy y x) nil)))
    (if (matrix= matrix working-copy
                 :test #'(lambda (v1 v2)
                           (with-epsilon (1e-7)
                             (every #'(lambda (a b)
                                        (cond
                                          ((and (null a) (null b))
                                           t)
                                          ((or (null a) (null b))
                                           nil)
                                          (t
                                           (epsilon= a b))))
                                    v1 v2))))
        object
        (progn
          (setf (layer object) working-copy)
          (smooth-dijkstra-layer object state :skippable-predicate skippable-predicate)))))

(defmethod next-dijkstra-position ((object dijkstra-layer) entity-player scale-factor-cost-concern)
  (with-accessors ((layer layer)) object
    (with-accessors ((state state)) entity-player
      (labels ((find-cost-layer (a)
                 (let ((raw (matrix:matrix-elt layer (elt a 1) (elt a 0))))
                     (and raw
                          (d* scale-factor-cost-concern raw))))
                 (find-cost (a) ;; doors as if they have no cost
                   (if (game-state:door@pos-p state (elt a 0) (elt a 1))
                       +open-terrain-cost+
                       (game-state:get-cost state (elt a 0) (elt a 1)))))
        (let* ((position (map-utils:pos-entity-chunk->cost-pos (pos entity-player)))
               (x-player (elt position 0))
               (y-player (elt position 1))
               (neighbour-elements (gen-valid-4-neighbour-counterclockwise layer
                                                                           x-player
                                                                           y-player
                                                                           :add-center nil))
               (compare-fn         #'(lambda (a b)
                                       (let ((cost-a (find-cost-layer a))
                                             (cost-b (find-cost-layer b)))
                                         (cond
                                           ((null cost-a)
                                            nil)
                                           ((null cost-b)
                                            t)
                                           (t
                                            (< cost-a cost-b))))))
               (min                (find-min-max compare-fn neighbour-elements)))
          (values (vector position
                          (sequence->ivec2 min))
                  (find-cost min)))))))

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
