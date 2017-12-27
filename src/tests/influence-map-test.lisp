;; dawn of the Era: a tactical game.
;; Copyright (C) 2017  cage

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

(in-package :inmap-test)

(defsuite inmap-suite (all-suite))

(defgeneric smooth-dijkstra-layer-test (object sample-fn))

(defmethod smooth-dijkstra-layer-test ((object dijkstra-layer) (sample-fn function))
  (let* ((matrix          (layer object))
         (working-copy    (clone matrix)))
    (loop-matrix (working-copy x y)
       (let* ((neighbour (funcall sample-fn matrix x y :add-center nil))
              (center    (matrix-elt working-copy y x))
              (min       (loop for cell in neighbour
                            when (matrix-elt working-copy
                                             (elt cell 1)  ; row
                                             (elt cell 0)) ; column
                            minimize
                              (matrix-elt working-copy
                                          (elt cell 1)     ; row
                                          (elt cell 0))))) ; column
         (when (d>= center (d+ (d min) 2.0))
           (setf (matrix-elt working-copy y x)
                 (d+ 1.0 (d min))))))
    (if (matrix= matrix working-copy
                 :test #'(lambda (v1 v2)
                           (with-epsilon (1e-3)
                             (every #'(lambda (a b)
                                        (cond
                                          ((and (null a) (null b))
                                           t)
                                          (t
                                           (epsilon= a b))))
                                    v1 v2))))
        object
        (progn
          (setf (layer object) working-copy)
          (smooth-dijkstra-layer-test object sample-fn)))))

(deftest test-smooth-dijkstra-map-8-4-sample (inmap-suite)
  (let ((map (make-dijkstra-layer 4 4 0.0)))
    (loop for y from 0 below 4 do
         (setf (matrix-elt (layer map) y 0) 10.0))
    (setf (matrix-elt (layer map) 0 1) 10.0)
    (setf (matrix-elt (layer map) 1 1) 10.0)
    (smooth-dijkstra-layer-test map #'gen-valid-4-neighbour-ccw)
    (assert-true
        (every #'(lambda (a b) (epsilon= a b))
               (data (layer map))
               #(2.0 1.0 0.0 0.0
                 2.0 1.0 0.0 0.0
                 1.0 0.0 0.0 0.0
                 1.0 0.0 0.0 0.0))
      (layer map))))

(deftest test-smooth-dijkstra-map-8-8-sample (inmap-suite)
  (let ((map (make-dijkstra-layer 4 4 0.0)))
    (loop for y from 0 below 4 do
         (setf (matrix-elt (layer map) y 0) 10.0))
    (setf (matrix-elt (layer map) 0 1) 10.0)
    (setf (matrix-elt (layer map) 1 1) 10.0)
    (smooth-dijkstra-layer-test map
                                #'(lambda (matrix x y &key (add-center nil))
                                    (misc:seq->list
                                     (gen-valid-neighbour-position-in-box matrix x y
                                                                          1 1 ; size
                                                                          :add-center
                                                                          add-center))))
    (assert-true
        (every #'(lambda (a b) (epsilon= a b))
               (data (layer map))
               #(2.0 1.0 0.0 0.0
                 1.0 1.0 0.0 0.0
                 1.0 0.0 0.0 0.0
                 1.0 0.0 0.0 0.0))
      (layer map))))
