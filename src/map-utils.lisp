;; dawn of the Era: a tactical game.
;; Copyright (C) 2015  cage

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along      with      this      program.       If      not,      see
;; <http://www.gnu.org/licenses/>.

(in-package :map-utils)

(definline coord-chunk->matrix (a)
  "convert from terrain chunk to matrix"
  (declare (optimize (speed 1) (safety 0) (debug 0)))
  (declare (num:desired-type a))
  (floor (num:d/ a +terrain-chunk-tile-size+)))

(defun coord-layer->map-state (a)
  (truncate (num:d* (num:desired a) +terrain-chunk-size-scale+)))

(definline coord-chunk->costs (a)
  "convert from terrain chunk to costs matrix"
  (coord-chunk->matrix (num:d a)))

(definline map-manhattam-distance (from to)
  (+ (abs (- (elt to 0) (elt from 0)))
     (abs (- (elt to 1) (elt from 1)))))

(definline map-manhattam-distance-cost (from to)
  (* +open-terrain-cost+ (map-manhattam-distance from to)))

(defun facingp (pos dir target-pos &key (max-distance 1))
  "dir is a 3d vector"
  (let* ((dir-to-target (ivec2- target-pos pos))
         (dot-product   (vec2-dot-product (vec2-normalize (vec2 (d (elt dir-to-target 0))
                                                                (d (elt dir-to-target 1))))
                                          (vec2 (d (elt dir 0))
                                                (d (elt dir 2))))))
    (and (f<= (ivec2-length dir-to-target)
              max-distance)
         (epsilon= dot-product 1.0))))

(defun facing-pos (pos dir)
  "dir and pos are a 3d vector, return an ivec2"
  (let* ((pos-map    (pos-entity-chunk->cost-pos pos))
         (comp       (position-if #'(lambda (a) (not (epsilon= 0.0 a))) dir))
         (comp-2     (if (= comp 0)
                         0
                         1))
         (compl-comp (if (= comp-2 0)
                          1
                          0))
         (offset     (if (< (elt dir comp) 0)
                         -1
                         1))
         (res        (ivec2 0 0)))
    (setf (elt res comp-2)
          (+ (elt pos-map comp-2) offset))
    (setf (elt res compl-comp)
          (elt pos-map compl-comp))
    res))

(defun pos-entity-chunk->cost-pos (pos)
  (ivec2:ivec2 (coord-chunk->costs (elt pos 0))
               (coord-chunk->costs (elt pos 2))))

(defun coord-map->chunk (a &key (tile-offset (num:d/ +terrain-chunk-tile-size+ 2.0)))
  "convert from logical (i.e. matrix of integer) to actual (float) rendering coordinate"
  (num:d+ (num:d* (num:desired a) +terrain-chunk-size-scale+) tile-offset))

(defun coord-terrain->chunk (a &key (tile-offset (num:d/ +terrain-chunk-tile-size+ 2.0)))
  "convert from terrain matrix to actual rendering coordinate"
  (num:d+ (num:d* (num:desired a)
                  (num:d+ +terrain-chunk-tile-size+ +terrain-chunk-size-scale+))
          tile-offset))

(defgeneric pos->game-state-pos (object))

(defmethod pos->game-state-pos ((object entity))
  (with-accessors ((pos pos)) object
    (ivec2 (coord-chunk->matrix (elt pos 0))
           (coord-chunk->matrix (elt pos 2)))))
