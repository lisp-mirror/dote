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

(defmethod smooth-dijkstra-layer ((object dijkstra-layer) (state game-state)
                                  &key (skippable-predicate #'skippablep))
  (let* ((matrix          (layer object))
         (working-copy    (clone matrix))
         (not-skippable-p #'(lambda (el pos) (not (funcall skippable-predicate (el-type el) pos)))))
    (loop-matrix (working-copy x y)
       (if (not (funcall skippable-predicate (el-type-in-pos state x y)
                         (ivec2 x y)))
           (let* ((neighbour (game-state:get-neighborhood state y x not-skippable-p))
                  (center    (matrix-elt working-copy y x))
                  (min       (loop for cell across neighbour minimize
                                  (matrix-elt working-copy
                                              (elt (cdr cell) 1)     ; row
                                              (elt (cdr cell) 0))))) ; column
             (when (d> (dabs (d- center min)) 2.0)
               (setf (matrix-elt working-copy y x)
                     (d+ 1.0 min))))
           (setf (matrix-elt working-copy y x) nil)))
    (if (matrix= matrix working-copy
                 :test #'(lambda (v1 v2)
                           (with-epsilon (1e-3)
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
          (smooth-dijkstra-layer object state)))))

(defun make-dijkstra-layer (w h bg-value)
  (make-instance 'dijkstra-layer
                 :layer (make-matrix w h (d bg-value))))

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

(defmethod apply-influence ((object matrix) (influencer hash-table))
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (apply-influence object v))
           influencer))

(defun im->pixmap (map)
  (layer->pixmap map))
