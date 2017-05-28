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

(in-package graph)

(defclass graph ()
  ((node-id-map-table
    :initform (make-hash-table :test #'equal)
    :initarg :node-id-map-table
    :accessor node-id-map-table)))

(defgeneric get-first-near (object node))

(defgeneric get-first-near-as-id (object node))

(defgeneric traverse-cost (object from to))

(defgeneric (setf traverse-cost) (value object from to))

(defgeneric delete-arc (object from to))

(defgeneric delete-all-arcs (object node))

(defgeneric add-arc (object from to cost))

(defgeneric add-node (object node))

(defgeneric node->node-id (object node))

(defgeneric node-id->node (object node-id))

(defgeneric astar-search (object from to &key heuristic-cost-function))

(defgeneric dijkstra-search (object from))

(defgeneric dfs (object from-id))

(defgeneric bfs (object from-id))

(defgeneric random-node-id (object))

(defmethod node-id->node ((object graph) node-id)
  (gethash node-id (node-id-map-table object)))

(defun euclidean-distance ()
  #'(lambda (a b)
      (2d-utils:2d-vector-magn (2d-utils:2d-vector-diff a b))))

(defclass tile-based-graph (graph)
  ((matrix
    :initform nil
    :initarg :matrix
    :accessor matrix)
   (ids
    :initform nil
    :initarg :ids
    :accessor ids)))

(defun matrix->graph (matrix-costs)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (declare (matrix:matrix matrix-costs))
  (make-instance 'graph:tile-based-graph :matrix matrix-costs))

(defmethod initialize-instance :after ((object tile-based-graph) &key
                                                                   (w nil) (h nil)
                                                                   &allow-other-keys)
  (when (and w h)
    (setf (matrix object) (matrix:gen-matrix-frame w h nil)))
  (setf (ids object) (matrix:gen-matrix-frame (matrix:width  (matrix object))
                                              (matrix:height (matrix object))
                                              nil))
  (let ((start-id 0))
    (matrix:loop-matrix ((ids object) x y)
      (setf (matrix:matrix-elt (ids object) y x) start-id)
      (incf start-id))))

(defmethod get-first-near ((object tile-based-graph) (node sequence))
  (remove-if #'(lambda (coord) (not
                                (matrix:valid-index-p (matrix object)
                                                      (second coord)
                                                      (first coord))))
             (matrix:gen-4-neighbour-counterclockwise
              (elt node 0)
              (elt node 1) :add-center nil)))

(defmethod get-first-near-as-id ((object tile-based-graph) (node number))
  (mapcar #'(lambda (n) (node->node-id object n))
          (get-first-near object (node-id->node object node))))

(defmethod traverse-cost ((object tile-based-graph) (from list) (to list))
  (if (find to (matrix:gen-4-neighbour-counterclockwise (first from) (second from))
            :test #'equalp)
      (matrix:matrix-elt* (matrix object) to)
      nil))

(defmethod (setf traverse-cost) (value (object tile-based-graph) from to)
  (declare (ignore from))
  (setf (matrix:matrix-elt (matrix object) (elt to 1) (elt to 0)) value))

(defmethod add-arc ((object tile-based-graph) from to cost)
  (setf (traverse-cost object from to) cost))

(defmethod node->node-id ((object tile-based-graph) node)
  (matrix:matrix-elt* (ids object) node))

(defmethod node-id->node ((object tile-based-graph) node-id)
  (let ((w (matrix:width (ids object))))
    (list (mod node-id w) (floor (/ node-id w)))))

(defmethod random-node-id ((object tile-based-graph))
  (node->node-id object (list (mod (num:lcg-next) (matrix:width (matrix object)))
                              (mod (num:lcg-next) (matrix:height (matrix object))))))

(defun equal-function (costs)
  #'(lambda (a b) (= (matrix:matrix-elt costs (second a) (first a))
                     (matrix:matrix-elt costs (second b) (first b)))))

(defun compare-function (costs)
  #'(lambda (a b) (< (matrix:matrix-elt costs (second a) (first a))
                     (matrix:matrix-elt costs (second b) (first b)))))


(defclass tile-multilayers-graph (graph)
  ((layers
    :initform (misc:make-fresh-array 0)
    :initarg :layers
    :accessor layers)
   (ids
    :initform nil
    :initarg :ids
    :accessor ids)))

(defun make-tile-multilayer-graph (&rest layers)
  (let ((arr (misc:make-fresh-array (length layers) nil 'matrix:matrix t)))
    (misc:copy-list-into-array layers arr)
    (make-instance 'tile-multilayers-graph
                   :layers arr)))

(defmethod initialize-instance :after ((object tile-multilayers-graph) &key &allow-other-keys)
  (setf (ids object) (matrix:gen-matrix-frame (matrix:width  (elt (layers object) 0))
                                              (matrix:height  (elt (layers object) 0))
                                              nil))
  (let ((start-id 0))
    (matrix:loop-matrix ((ids object) x y)
      (setf (matrix:matrix-elt (ids object) y x) start-id)
      (incf start-id))))

(defmethod get-first-near ((object tile-multilayers-graph) (node sequence))
  (with-accessors ((layers layers)) object
    (let ((mat (elt layers 0)))
      (remove-if #'(lambda (coord) (not
                                    (matrix:valid-index-p mat
                                                          (second coord)
                                                          (first coord))))
                 (matrix:gen-4-neighbour-counterclockwise
                  (elt node 0)
                  (elt node 1) :add-center nil)))))

(defmethod get-first-near-as-id ((object tile-multilayers-graph) (node number))
  (mapcar #'(lambda (n) (node->node-id object n))
          (get-first-near object (node-id->node object node))))

(defmethod traverse-cost ((object tile-multilayers-graph) (from sequence) (to sequence))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-accessors ((layers layers)) object
    (declare ((simple-array matrix:matrix (*)) layers))
    (reduce #'(lambda (a b)
                (num:d+ a (matrix:matrix-elt* b to)))
            layers
            :initial-value 0.0)))

(defmethod node->node-id ((object tile-multilayers-graph) node)
  (matrix:matrix-elt* (ids object) node))

(defmethod node-id->node ((object tile-multilayers-graph) node-id)
  (let ((w (matrix:width (ids object))))
    (list (mod node-id w) (floor (/ node-id w)))))

(defmethod random-node-id ((object tile-multilayers-graph))
  (node->node-id object (list (mod (num:lcg-next) (matrix:width (matrix object)))
                              (mod (num:lcg-next) (matrix:height (matrix object))))))

(defclass matrix-graph (graph)
  ((matrix
    :initform nil
    :initarg :matrix
    :accessor matrix)))

(defmethod print-object ((object matrix-graph) stream)
  (format stream "~a" (matrix object)))

(defmethod initialize-instance :after ((object matrix-graph) &key (size 16)
                                       &allow-other-keys)
  (setf (matrix object) (matrix:gen-matrix-frame size size nil)))

(defmethod get-first-near ((object matrix-graph) node)
  (let ((row (matrix:row->sequence object node)))
    (loop
       for i in row
       for ct from 0 below (length row) when i collect
       ct)))

(defmethod get-first-near-as-id ((object matrix-graph) node-id)
  (get-first-near object (node-id->node object node-id)))

(defmethod traverse-cost ((object matrix-graph) from to)
  (matrix:matrix-elt (matrix object) from to))

(defmethod (setf traverse-cost) (value (object matrix-graph) from to)
  (setf (matrix:matrix-elt (matrix object) from to) value)
  (setf (matrix:matrix-elt (matrix object) to from) value))

(defmethod add-arc ((object matrix-graph) from to cost)
  (setf (traverse-cost object from to) cost))

(defmethod delete-arc ((object matrix-graph) from to)
  (setf (matrix:matrix-elt (matrix object) from to) 0)
  (setf (matrix:matrix-elt (matrix object) to from) 0))

(defmethod delete-all-arcs ((object matrix-graph) node)
  (loop for i from 0 below (matrix:width (matrix object)) do
       (setf (matrix:matrix-elt (matrix object) node i) 0)))

(defmethod node->node-id ((object matrix-graph) node)
  node)

(defmethod node-id->node ((object matrix-graph) node-id)
  node-id)

(defmethod random-node-id ((object matrix-graph))
  (node->node-id object (mod (num:lcg-next) (matrix:height (matrix object)))))

(alexandria:define-constant +start-keyword+ :start :test #'eq)

(alexandria:define-constant +destination-keyword+ :destination :test #'eq)

(alexandria:define-constant +cost-keyword+ :cost :test #'eq)

(defclass arc-graph ()
  ((start
    :initform nil
    :initarg :start
    :accessor start)
   (destination
    :initform nil
    :initarg :destination
    :accessor destination)
   (cost
    :initform 0
    :initarg :cost
    :accessor cost)))

(defmethod print-object ((object arc-graph) stream)
  (format stream "{start ~a -> ~a cost ~a}"
          (start object)
          (destination object)
          (cost object)))

(defmethod to-sexp ((object arc-graph))
  (list +start-keyword+ (to-sexp (start object))
        +destination-keyword+ (to-sexp (destination object))
        +cost-keyword+ (to-sexp (cost object))))

(defclass list-graph (graph)
  ((adjacency-list
    :initform (rb-tree:make-root-rb-node nil :red)
    :initarg :adjacency-list
    :accessor adjacency-list)
   (test-node-id-function
    :initform #'(lambda (a b) (= a b))
    :initarg :test-node-id-function
    :accessor test-node-id-function)))

(defmacro search-tree (tree datum)
  `(bs-tree:data (bs-tree:search ,tree ,datum
                                 :equal #'=
                                 :compare #'<
                                 :key-datum #'identity
                                 :key #'first)))

(defmacro insert-tree (tree datum)
  `(setf ,tree (bs-tree:insert ,tree ,datum
                               :equal #'=
                               :compare #'<
                               :key-datum #'first
                               :key #'first)))

(defmethod print-object ((object list-graph) stream)
  (format stream "~a" (adjacency-list object)))

(defgeneric find-node (object node-id))

(defgeneric (setf find-node) (value object node-id))

(defgeneric find-arc (object node-id destination-id))

(defgeneric position-arc (object node-id destination-id))

(defgeneric graph->path (object end-node &optional path cost))

(defmethod node->node-id ((object list-graph) node)
  (declare (ignore object))
  node)

(defmethod node-id->node ((object list-graph) node-id)
  (declare (ignore object))
  node-id)

(defmethod get-first-near ((object list-graph) node)
  (let ((from (find-node object (node->node-id object node))))
    (if from
        (rest from)
        nil)))

(defmethod get-first-near-as-id ((object list-graph) (node-id number)) ;; TEST
  (mapcar #'destination (get-first-near object (node-id->node object node-id))))

(defmethod traverse-cost ((object list-graph) from to)
  (let* ((near (get-first-near object from))
         (node (find to near :key #'destination :test (test-node-id-function object))))
    (if node
        (cost node)
        nil)))

(defmethod (setf traverse-cost) (value (object list-graph) from to)
  (let ((from-adj (find-node object from)))
    (when (not from-adj)
      (add-node object from)))
  (let ((to-adj (find-node object to)))
    (when (not to-adj)
      (add-node object to)))

  (let* ((position-arc-from (position-arc object from to))
         (position-arc-to (position-arc object to from)))
    (if position-arc-from
        (setf (cost (nth position-arc-from (find-node object from)))
              value)
        (setf (find-node object from)
              (concatenate 'list
                           (find-node object from)
                           (list (make-instance 'arc-graph
                                                :start from
                                                :destination to
                                                :cost value)))))
    (if position-arc-to
        (setf (cost (nth position-arc-to (find-node object to)))
              value)
        (setf (find-node object to)
              (concatenate 'list
                           (find-node object to)
                           (list (make-instance 'arc-graph
                                                :start to
                                                :destination from
                                                :cost value)))))))

(defmethod delete-arc ((object list-graph) from to)
  (let* ((position-arc-from (position-arc object from to))
         (position-arc-to (position-arc object to from)))
    (when (and position-arc-from position-arc-to)
      (let ((adj-from (find-node object from))
            (adj-to (find-node object to)))
        (setf (find-node object from)
              (concatenate 'list
                           (list (first adj-from))
                           (misc:safe-delete@ (rest adj-from) (1- position-arc-from))))

        (setf (find-node object to)
              (concatenate 'list
                           (list (first adj-to))
                           (misc:safe-delete@ (rest adj-to) (1- position-arc-to))))))))

(defmethod delete-all-arcs ((object list-graph) node)
  (let ((target (find-node object node)))
    (when target
      (setf (find-node object node) (list node)))))

(defmethod add-arc ((object list-graph) from to cost)
  (setf (traverse-cost object from to) cost))

(defmethod add-node ((object list-graph) node)
  (insert-tree (adjacency-list object) (list node)))

(defmethod find-node ((object list-graph) node-id)
  (search-tree (adjacency-list object) node-id))

(defmethod (setf find-node) (value (object list-graph) node-id)
  (setf (search-tree (adjacency-list object) node-id) value))

(defmethod find-arc ((object list-graph) node-id destination-id)
  (let* ((node-adj (find-node object node-id))
         (arcs (rest node-adj)))
    (find destination-id arcs
          :key #'destination
          :test (test-node-id-function object))))

(defmethod position-arc ((object list-graph) node-id destination-id)
  (let* ((node-adj (find-node object node-id))
         (arcs (rest node-adj))
         (raw-pos (position destination-id arcs
                            :key #'destination
                            :test (test-node-id-function object))))
    (when raw-pos
      (incf raw-pos))
    raw-pos))

(defmethod random-node-id ((object list-graph))
  (let ((node-list nil))
    (bs-tree:map (adjacency-list object) #'(lambda (d) (push d node-list)))
    (node->node-id object (nth (mod (num:lcg-next) (length node-list)) node-list))))

(defmethod graph->path ((object list-graph) end-node
                        &optional (path '())
                        (cost (cost (second (find-node object end-node)))))
  (labels ((parent (adjacency-list)
             (destination (second adjacency-list))))
    (if (funcall (test-node-id-function object)
                 end-node (parent (find-node object end-node)))
        (values (push end-node path) cost)
        (graph->path object (parent (find-node object end-node))
                     (push end-node path) cost))))

(defparameter *cumulative-cost-plus-heuristic* (rb-tree:make-root-rb-node nil 'black))

(defun equal-function* ()
  #'(lambda (a b)
      (let ((ca (bs-tree:data (bs-tree:search *cumulative-cost-plus-heuristic* a
                                              :key #'first
                                              :compare #'<
                                              :equal #'=)))
            (cb (bs-tree:data (bs-tree:search *cumulative-cost-plus-heuristic* b
                                              :key #'first
                                              :compare #'<
                                              :equal #'=))))
        (and ca cb (= (second ca) (second cb))))))

(defun compare-function* ()
  #'(lambda (a b)
      (let ((ca (bs-tree:data (bs-tree:search *cumulative-cost-plus-heuristic* a
                                              :key #'first
                                              :compare #'<
                                              :equal #'=)))
            (cb (bs-tree:data (bs-tree:search *cumulative-cost-plus-heuristic* b
                                              :key #'first
                                              :compare #'<
                                              :equal #'=))))
        (and ca cb (< (second ca) (second cb))))))

(defun heuristic-manhattam ()
  #'(lambda (object a b start-node)
      (declare (ignore object))
      ;;(declare (list a b start-node))
      (declare (optimize (speed 0) (safety 3) (debug 3)))
      (let* ((cost  (+ (abs (- (elt b 0) (elt a 0)))
                       (abs (- (elt b 1) (elt a 1)))))
             (dx1   (- (elt b 0) (elt a 0)))
             (dy1   (- (elt b 1) (elt a 1)))
             (dx2   (- (elt start-node 0) (elt a 0)))
             (dy2   (- (elt start-node 1) (elt a 1)))
             (cross (abs (- (* dx1 dy2) (* dx2 dy1)))))
        (+ cost (* cross 0.05)))))

(defmethod astar-search ((object graph) from-id to-id
                         &key (heuristic-cost-function #'(lambda (object a b)
                                                           (declare (ignore object a b)) 0)))
  (labels ((find-node-in-set (node-id the-set)
             (let ((found (bs-tree:search the-set node-id :key #'first :compare #'< :equal #'=)))
               (if found
                   (bs-tree:data found)
                   (list node-id 0))))
           (find-cost-in-node-set (node-id the-set)
               (second (find-node-in-set node-id the-set))))
    (let ((cumulative-cost (rb-tree:make-root-rb-node nil 'black))
          (*cumulative-cost-plus-heuristic* (rb-tree:make-root-rb-node nil 'black))
          (bst (make-instance 'list-graph))
          (frontier (make-instance 'list-graph)))
      (pq:with-min-queue (queue (equal-function*) (compare-function*) #'identity)
        (pq:push-element queue from-id)
        (setf (traverse-cost frontier from-id from-id) 0)
        (do ((visited (pq:pop-element queue) (pq:pop-element queue)))
            ((not visited) (values bst cumulative-cost))
          (let* ((node-frontier (find-node frontier visited))
                 (parent (second node-frontier)))
            (setf (traverse-cost bst visited (destination parent))
                  (find-cost-in-node-set visited cumulative-cost))
          (when (and to-id (equalp visited to-id))
            (return-from astar-search (values bst cumulative-cost)))
          (let ((next-nodes (get-first-near-as-id object visited)))
            (loop for next in next-nodes do
                 (let* ((new-cost (+ (find-cost-in-node-set visited cumulative-cost)
                                     (traverse-cost object (node-id->node object visited)
                                                    (node-id->node object next))))
                        (new-cost-plus-heuristic (+ new-cost
                                                    (funcall heuristic-cost-function
                                                             object
                                                             (node-id->node object to-id)
                                                             (node-id->node object next)
                                                             (node-id->node object from-id)))))
                   (when (or
                          (null (find-node frontier next))
                          (and  (null (find-node bst next))
                                (< new-cost (find-cost-in-node-set
                                             next cumulative-cost))))
                     (setf *cumulative-cost-plus-heuristic*
                           (bs-tree:insert *cumulative-cost-plus-heuristic*
                                             (list next new-cost-plus-heuristic)
                                             :key #'first
                                             :key-datum #'first
                                             :compare #'<
                                             :equal #'=))
                     (setf cumulative-cost
                           (bs-tree:insert cumulative-cost
                                             (list next new-cost)
                                             :key #'first
                                             :key-datum #'first
                                             :compare #'<
                                             :equal #'=))
                     (pq:push-element queue next)
                     (delete-all-arcs frontier next)
                     (setf (traverse-cost frontier next visited) new-cost)))))))))))

(defmethod dijkstra-search ((object graph) from-id)
  (astar-search object from-id nil
                :heuristic-cost-function #'(lambda (a b)
                                             (declare (ignore a b)) 0)))

(defmethod to-sexp ((object list-graph)) ;; NO
  (let ((visit (dfs object (random-node-id object))))
    (mapcar #'(lambda (node-id)
                (let ((node (find-node object node-id)))
                  (concatenate 'list
                               (list (to-sexp (first node)))
                               (mapcar #'(lambda (adj) (to-sexp adj))
                                       (rest node)))))
            visit)))

(defmethod from-sexp ((object list-graph) sexp) ;; NO
  (mapc #'(lambda (node)

            (push
             (concatenate 'list
                          (list (first node))
                          (mapcar #'(lambda (adj)
                                      (make-instance 'arc-graph
                                                     :start (getf adj +start-keyword+)
                                                     :destination (getf adj +destination-keyword+)
                                                     :cost (getf adj +cost-keyword+)))
                                  (rest node)))
             (adjacency-list object)))
        sexp)
  object)

(defmacro with-container ((container) &body body)
  (ecase container
    (:stack
     `(st:with-stack (#'= #'identity)
        ,@body))
    (:queue
     `(qu:with-queue (#'= #'identity)
        ,@body))))

(defmacro gen-basic-visit (name package)
  (labels ((conc-package (name)
             (alexandria:format-symbol package "~:@(~a~)" name)))
    (alexandria:with-gensyms (visited res object from-id)
      `(defmethod ,(alexandria:format-symbol t "~:@(~a~)" name)
           ((,object graph) ,from-id)
         (with-container (,(alexandria:make-keyword package))
           (,(conc-package 'push) ,from-id)
           (do ((,visited (,(conc-package 'pop)) (,(conc-package 'pop)))
                (,res nil))
               ((not ,visited) ,res)
             (when (not (find ,visited ,res :key ,(conc-package '*key-function*)
                              :test ,(conc-package '*equal-function*)))
               (push ,visited ,res)
               (loop for i in (get-first-near-as-id ,object ,visited) do
                    (,(conc-package 'push) i)))))))))

(gen-basic-visit dfs stack)

(gen-basic-visit bfs queue)
