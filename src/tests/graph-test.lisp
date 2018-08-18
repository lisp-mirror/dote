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

(in-package :graph-test)

(defsuite graph-suite (all-suite))

(defun testing-layer-graph ()
  (make-tile-multilayer-graph (matrix:define-matrix (3 3)
                                2.0 1.0 2.0
                                2.0 1.0 2.0
                                2.0 1.0 2.0)
                              (matrix:define-matrix (3 3)
                                1.0   1.0 1.0
                                1.0  10.0 1.0
                                1.0   1.0 1.0)))

(deftest test-a*-multilayer (graph-suite)
  (assert-true
      (let ((tree (graph:astar-search (testing-layer-graph)
                                      (graph:node->node-id (testing-layer-graph) #(1 0))
                                      (graph:node->node-id (testing-layer-graph) #(1 2))
                                      :heuristic-cost-function
                                      (game-state::heuristic-manhattam 1.0))))
        (multiple-value-bind (raw-path cost)
            (graph:graph->path tree (graph:node->node-id (testing-layer-graph)  #(1 2)))
          (let ((path (map 'vector
                           #'(lambda (id) (graph:node-id->node (testing-layer-graph) id))
                           raw-path)))
            (and (equalp path #((1 0) (2 0) (2 1) (2 2) (1 2)))
                 (=      cost 11.0)))))))


(defun testing-dijkstra-search-multilayer-graph ()
  (make-tile-multilayer-graph (matrix:define-matrix (3 3)
                                2.0 1.0 2.0
                                2.0 1.0 2.0
                                3.0 1.0 2.0)
                              (matrix:define-matrix (3 3)
                                1.0   1.0  1.0
                                1.0  10.0  1.0
                                1.0   1.0  1.0)))

(deftest dijkstra-search-multilayer-all (graph-suite)
  (let* ((graph (testing-dijkstra-search-multilayer-graph))
         (costs (all-minimum-path-costs graph
                                        (node->node-id graph #(1 0)))))
    (assert-equality #'(lambda (a b) (every #'num:epsilon= a b))
        (matrix:data costs)
        (matrix:data (matrix:define-matrix (3 3)
                        3.0    0  3.0
                        6.0 11.0  6.0
                       10.0 11.0  9.0)))))
