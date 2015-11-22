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

(in-package :game-state)

(alexandria:define-constant +start-day+     6 :test #'=)

(alexandria:define-constant +end-day+      20 :test #'=)

(alexandria:define-constant +start-night+  21 :test #'=)

(alexandria:define-constant +end-night+     5 :test #'=)

(alexandria:define-constant +zenith-night+ 23 :test #'=)

(alexandria:define-constant +zenith-day+   13 :test #'=)

(alexandria:define-constant +yellow-light-color+  §cffff99ff :test #'vec4~)

(alexandria:define-constant +white-light-color+   §cffffffff :test #'vec4~)

(alexandria:define-constant +blueish-light-color+ §c7ba8e4ff :test #'vec4~)

(defun hour->light-color (h)
  (cond
    ((or (<= 0 h +start-day+)
	 (<= +start-night+ h +zenith-night+))
     +blueish-light-color+)
    ((or (<= 7 h 8)
	 (<= 18 h +end-day+))
     nil)
    ((or (<= 9 h 10)
	 (= h 17))
     +yellow-light-color+)
    ((<= 11 h 16)
     +white-light-color+)
    (t
     (error "No valid hour..."))))

(defun hour->celestial-body-position-latitude (h)
  (if (<= +start-day+ h +end-day+) ;; day
      (d+ 30.0 (d* 8.57 (d- (d h) (d +start-day+))))
      (if (<= +start-night+ h +zenith-night+) ;; night
	  (d* 26.7 (d- (d h) (d +start-night+)))
	  (d+ 100.0 (d* 13.33 (d h))))))

(defun hour->celestial-body-position-longitude (h)
  (if (<= +start-day+ h +zenith-day+) ;; day
      (d+ (d +end-day+) (d* 4.28 (d- (d h) (d +start-day+))))
      (if (<= 14 h +end-day+)
	  (d- 153.0 (d* 7.0 (d h)))
	  (if (<= +start-night+ h +zenith-night+) ;; night
	      (d* 13.330 (d- (d h) (d +start-night+)))
	      (d- 45.0 (d+ 10.38 (d* 3.46 (d h))))))))

(defun hour->celestial-body-position (h)
  (let* ((longitude (sb-cga:rotate-around (3d-utils:vec-negate +x-axe+)
				   (deg->rad (hour->celestial-body-position-longitude h))))
	 (latitude  (sb-cga:rotate-around +y-axe+
					  (deg->rad (hour->celestial-body-position-latitude h))))
	 (transform (sb-cga:matrix* latitude longitude)))
    (sb-cga:vec* (sb-cga:transform-direction +z-axe+ transform)
     	  +maximum-map-size+)))

(defmacro gen-type (&rest names)
  `(progn
     ,@(loop for name in names collect
	    `(alexandria:define-constant
		 ,(alexandria:format-symbol t "~:@(+~a-type+~)" name)
		 ,(alexandria:make-keyword name)
	       :test #'eq))))

(gen-type ceiling floor
	  empty unknown
	  door-n door-s
	  door-w door-e
	  wall tree
	  furniture
	  magic-furniture
	  container
	  pillar
	  chair
	  table
	  walkable
	  wall-decoration
	  npc
	  pc)

(defun invalid-entity-id-map-state ()
  (- +start-id-counter+ 1))

(defclass map-state-element (identificable)
  ((entity-id
    :initform (invalid-entity-id-map-state)
    :initarg  :entity-id
    :accessor entity-id)
   (el-type
    :initform +empty-type+
    :initarg  :el-type
    :accessor el-type)
   (occlude
    :initform t
    :initarg  :occlude
    :reader   occludep
    :writer   (setf occlude))))

(defgeneric map-element-empty-p (object))

(defgeneric neighborhood-by-type (object row column type &key w-offset h-offset))

(defgeneric get-neighborhood (object row column predicate &key w-offset h-offset))

(defmethod map-element-empty-p ((object map-state-element))
  (or (eq (el-type object)
	  +empty-type+)
      (eq (el-type object)
	  +floor-type+)))

(defclass movement-path ()
  ((tiles
    :initform nil
    :accessor tiles
    :initarg  :tiles)
   (cost
    :initform nil
    :accessor cost
    :initarg  :cost)))

(defun make-movement-path (tiles cost)
  (make-instance 'movement-path :tiles tiles :cost cost))

(defclass game-state ()
  ((game-hour
    :accessor game-hour
    :initarg :game-hour
    :initform 0)
   (game-minutes
    :accessor game-minutes
    :initarg :game-minutes
    :initform 0)
   (current-time
    :accessor current-time
    :initarg :current-time
    :initform (num:desired 0.0))
   (celestial-body-position
    :accessor celestial-body-position
    :initarg :celestial-body-position
    :initform (sb-cga:vec 0.0 100.0 0.0))
   (sky-bg-color
    :accessor sky-bg-color
    :initarg :sky-bg-color
    :initform §c000000ff)
   (light-color
    :accessor light-color
    :initarg  :light-color
    :initform (vec4->vec §cffffffff)
    :documentation "The diffuse and specular color (phong shading) for sun or moon light")
   (movement-costs
    :accessor movement-costs
    :initarg  :movement-costs
    :initform nil)
   (map-state
    :accessor map-state
    :initarg  :map-state
    :initform nil)
   (all-entities
    :accessor all-entities
    :initarg  :all-entities
    :initform (rb-tree:make-root-rb-node nil :red))
   (level-difficult
    :accessor level-difficult
    :initarg  :level-difficult
    :initform 1)
   (map-cache-dir
    :accessor map-cache-dir
    :initarg  :map-cache-dir
    :initform nil)
   (window-id
    :initform nil
    :initarg  :window-id
    :accessor window-id)
   (player-entities
    :initform (make-hash-table :test 'equal)
    :initarg  :player-entities
    :accessor player-entities)
   (ai-entities
    :initform (make-hash-table :test 'equal)
    :initarg  :ai-entities
    :accessor ai-entities)
   (selected-pc
    :initform nil
    :initarg  :selected-pc
    :accessor selected-pc)
   (selected-path
    :initform nil
    :initarg  :selected-path
    :accessor selected-path)))

(defgeneric fetch-render-window (object))

(defgeneric fetch-world (object))

(defgeneric setup-game-hour (object hour))

(defgeneric prepare-map-state (object map))

(defgeneric el-type-in-pos (object x y))

(defgeneric entity-id-in-pos (object x y))

(defgeneric build-movement-path (object start end))

(defgeneric terrain-aabb-2d (object))

(defgeneric terrain-aabb-2d (object))

(defgeneric push-entity  (object entity))

(defgeneric map-level (object))

(defgeneric add-to-player-entities (object id))

(defgeneric add-to-ai-entities (object id))

(defgeneric fetch-from-player-entities (object id-entity))

(defgeneric fetch-from-ai-entities (object entity))

(defgeneric faction-player-p (object id-entity))

(defgeneric faction-ai-p (object id-entity))

(defgeneric terrain-height@pos (object x z))

(defgeneric place-player-on-map (object player faction &optional position))

(defgeneric set-invalicable-cost@ (object x y))

(defgeneric set-minimum-cost@ (object x y))

(defgeneric set-map-state-type (object x y type))

(defgeneric set-map-state-id (object x y id))

(defgeneric set-map-state-occlusion (object x y occlusion-value))

(defgeneric setup-map-state-entity (object entity type occlusion-value))

(defgeneric move-map-state-entity (object entity from))

(defgeneric approx-terrain-height@pos (object x z))

(defmethod fetch-render-window ((object game-state))
  (and (window-id object)
       (sdl2.kit-utils:fetch-window (window-id object))))

(defmethod fetch-world ((object game-state))
  (let ((w (fetch-render-window object)))
    (and w
	 (main-window:world w))))

(defmethod  setup-game-hour ((object game-state) hour)
  (with-accessors ((game-hour game-hour)
		   (sky-bg-color sky-bg-color)
		   (celestial-body-position celestial-body-position)
		   (light-color light-color)) object
    (setf game-hour               hour)
    (setf sky-bg-color            (pixmap:gen-bg-sky-colors hour))
    (setf celestial-body-position (hour->celestial-body-position hour))
    (setf light-color             (vec4->vec (or (hour->light-color game-hour)
						 sky-bg-color)))))

(defmethod get-cost ((object game-state) x y)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (get-cost-insecure object x y))

(defmethod get-cost-insecure ((object game-state) x y)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (matrix-elt (graph:matrix (movement-costs object)) y x))

(defmacro gen-map-state-reader (&rest names)
  `(progn
     ,@(loop for name in names collect
	    (let ((fn-name (alexandria:format-symbol t "~:@(~a-in-pos~)" name))
		  (fn      (alexandria:format-symbol t "~:@(~a~)" name)))
	      `(progn
		 (defgeneric ,fn-name (object x y))
		 (defmethod  ,fn-name ((object game-state) (x fixnum) (y fixnum))
		   (declare (optimize (speed 0) (safety 3) (debug 3)))
		   (,fn (matrix-elt (map-state object) y x))))))))

(gen-map-state-reader el-type entity-id occludep)

(defmethod prepare-map-state ((object game-state) (map random-terrain))
  (with-accessors ((map-state map-state)) object
     (setf map-state
 	  (gen-matrix-frame (truncate (* (width  (matrix map)) +terrain-chunk-size-scale+))
 			    (truncate (* (height (matrix map)) +terrain-chunk-size-scale+))))
     (loop for i from 0 below (length (data map-state)) do
	  (setf (elt (data map-state) i) (make-instance 'map-state-element)))))

(defun heuristic-manhattam ()
  #'(lambda (object a b start-node)
      (declare (ignore object))
      (declare ((simple-array fixnum (2)) a b start-node))
      (declare (optimize (speed 3) (safety 0) (debug 0)))
      (let* ((a-x   (d (elt a 0)))
	     (a-y   (d (elt a 1)))
	     (b-x   (d (elt b 0)))
	     (b-y   (d (elt b 1)))
	     (s-x   (d (elt start-node 0)))
	     (s-y   (d (elt start-node 1)))
	     (cost  (d+ (dabs (d- b-x a-x))
			(dabs (d- b-y a-y))))
	     (dx1   (d- b-x a-x))
	     (dy1   (d- b-y a-y))
	     (dx2   (d- s-x a-x))
	     (dy2   (d- s-y a-y))
	     (cross (abs (d- (d* dx1 dy2) (d* dx2 dy1)))))
	(d+ cost (d* cross 0.05)))))

(defmethod build-movement-path ((object game-state) start end)
  (with-accessors ((movement-costs movement-costs)) object
    (let ((tree	(graph:astar-search movement-costs
				    (graph:node->node-id movement-costs start)
				    (graph:node->node-id movement-costs end)
				    :heuristic-cost-function (heuristic-manhattam))))
      (multiple-value-bind (raw-path cost)
	  (graph:graph->path tree (graph:node->node-id movement-costs end))
	(values
	 (map 'vector #'(lambda (id) (graph:node-id->node movement-costs id)) raw-path)
	 cost)))))

(defmethod terrain-aabb-2d ((object game-state))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-accessors ((map-state map-state)) object
    (vec4 0.0
	  0.0
	  (d* (d (the fixnum (width  map-state))) +terrain-chunk-tile-size+)
	  (d* (d (the fixnum (height map-state))) +terrain-chunk-tile-size+))))

(defmethod push-entity ((object game-state) entity)
  (with-accessors ((all-entities all-entities)) object
    (setf all-entities (bs-tree:insert all-entities
				       entity
				       :equal     #'=
				       :compare   #'<
				       :key-datum #'id
				       :key       #'id))))

(defmethod find-entity-by-id ((object game-state) id)
  (with-accessors ((all-entities all-entities)) object
    (bs-tree:data (bs-tree:search all-entities
				  id
				  :equal     #'=
				  :compare   #'<
				  :key       #'id
				  :key-datum #'identity))))

(defmethod map-level ((object game-state))
  (truncate (/ (+ (level-difficult object)
		  (1+ (* 8 (num:smoothstep-interpolate (d +minimium-map-size+)
						       (d +maximum-map-size+)
						       (d (width (map-state object)))))))
	       2)))

(defmethod add-to-player-entities ((object game-state) entity)
  (with-accessors ((player-entities player-entities)) object
    (setf (gethash (id entity) player-entities) entity)))

(defmethod add-to-ai-entities ((object game-state) entity)
  (with-accessors ((ai-entities ai-entities)) object
    (setf (gethash (id entity) ai-entities) entity)))

(defmethod fetch-from-player-entities ((object game-state) id-entity)
  (with-accessors ((player-entities player-entities)) object
    (gethash id-entity player-entities)))

(defmethod fetch-from-ai-entities ((object game-state) id-entity)
  (with-accessors ((ai-entities ai-entities)) object
    (gethash id-entity ai-entities)))

(defmethod faction-player-p ((object game-state) id-entity)
  (fetch-from-player-entities object id-entity))

(defmethod faction-ai-p ((object game-state) id-entity)
  (fetch-from-ai-entities object id-entity))

(defmethod approx-terrain-height@pos ((object game-state) x z)
  "x and z in world space"
  (let* ((world-ref (fetch-world object)))
    (world::pick-height-terrain world-ref x z)))

(defmethod place-player-on-map ((object game-state) player faction &optional (pos #(0 0)))
  (with-accessors ((map-state map-state)) object
    (let ((stop nil)
	  (player-coordinates nil))
      (labels ((%place-player (pos)
		 (when (not stop)
		   (let* ((next-tiles (misc:shuffle (gen-neighbour-position (elt pos 0)
									    (elt pos 1))))
			  (empty-tiles (remove-if-not #'(lambda (pos)
							  (let ((x (elt pos 0))
								(y (elt pos 1)))
							    (with-check-matrix-borders (map-state x y)
							      (map-element-empty-p (matrix:matrix-elt map-state y x)))))
						      next-tiles)))
		     (if empty-tiles
			 (progn
			   (setf stop t)
			   (setf player-coordinates (elt empty-tiles 0)))
			 (loop for i in next-tiles do
			      (let ((x (elt i 0))
				    (y (elt i 1)))
				(with-check-matrix-borders (map-state x y)
				  (%place-player i)))))))))
	(%place-player pos)
	(setf (entity:pos player)
	      (sb-cga:vec (misc:coord-map->chunk (d (elt player-coordinates 0)))
			  (num:d+ 1.5 ; hardcoded :(  to be removed soon
				  (approx-terrain-height@pos object
							     (d (elt player-coordinates 0))
							     (d (elt player-coordinates 1))))
			  (misc:coord-map->chunk (d (elt player-coordinates 1)))))
	;; TODO set cost for player in cost player layer of game-state
	(if (eq faction +pc-type+)
	    (add-to-player-entities object player)
	    (add-to-ai-entities     object player))))))

(defmethod set-invalicable-cost@ ((object game-state) x y)
  (with-accessors ((movement-costs movement-costs)) object
    (setf (matrix-elt (graph:matrix movement-costs) y x) +invalicable-element-cost+)))

(defmethod set-minimum-cost@ ((object game-state) x y)
  (with-accessors ((movement-costs movement-costs)) object
    (setf (matrix-elt (graph:matrix movement-costs) y x) +open-terrain-cost+)))

(defmethod set-map-state-type ((object game-state) x y type)
  (setf (el-type (matrix:matrix-elt (map-state object) y x)) type))

(defmethod set-map-state-id ((object game-state) x y id)
  (setf (entity-id (matrix:matrix-elt (map-state object) y x)) id))

(defmethod set-map-state-occlusion ((object game-state) x y occlusion-value)
  (setf (occlude (matrix:matrix-elt (map-state object) y x)) occlusion-value))

(defmethod setup-map-state-entity ((object game-state) entity type occlusion-value)
  (with-accessors ((pos pos) (id id)) entity
    (let ((x-matrix (misc:coord-chunk->matrix (elt pos 0)))
	  (y-matrix (misc:coord-chunk->matrix (elt pos 2))))
      (set-map-state-type      object x-matrix y-matrix type)
      (set-map-state-id        object x-matrix y-matrix id)
      (set-map-state-occlusion object x-matrix y-matrix occlusion-value))))

(defmethod move-map-state-entity ((object game-state) entity from)
  (let* ((old-x         (elt from 0))
	 (old-y         (elt from 1))
	 (old-type      (el-type-in-pos  object old-x old-y))
	 (old-occlusion (occludep-in-pos object old-x old-y)))
    (set-map-state-type      object old-x old-y +empty-type+)
    (set-map-state-id        object old-x old-y (invalid-entity-id-map-state))
    (set-map-state-occlusion object old-x old-y nil)
    (setup-map-state-entity  object entity old-type old-occlusion)))

(defmethod get-neighborhood ((object game-state) row column predicate
			     &key (w-offset 2) (h-offset 2))
  (with-accessors ((map-state map-state)) object
    (let ((pos (remove-if-not
		#'(lambda (a) (element@-inside-p map-state (elt a 0) (elt a 1)))
		(gen-neighbour-position-in-box column row w-offset h-offset :add-center nil)))
	  (results (misc:make-fresh-array 0 nil 'map-state-element nil)))
      (loop for i across pos do
	   (let ((element (matrix-elt map-state (elt i 1) (elt i 0))))
	     (when (funcall predicate element i)
	       (vector-push-extend (cons element i) results))))
      results)))

(defmethod neighborhood-by-type ((object game-state) row column type
				     &key (w-offset 2) (h-offset 2))
  (get-neighborhood object row column
		    #'(lambda (el pos)
			(declare (ignore pos))
			(or (null type)
			    (eq (el-type el) type)))
		    :w-offset w-offset :h-offset h-offset))
