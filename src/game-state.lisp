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
	  npc pc)

(defclass map-state-element (identificable)
  ((entity-id
    :initform -1
    :initarg  :entity-id
    :accessor entity-id)
   (character-id
    :initform -1
    :initarg  :character-id
    :accessor character-id)
   (el-type
    :initform +empty-type+
    :initarg  :el-type
    :accessor el-type)
   (occlude
    :initform t
    :initarg  :occlude
    :reader   occludep
    :writer   (setf occlude))))

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
   (pc-characters
    :initform (make-hash-table :test 'equal)
    :initarg  :pc-characters
    :accessor pc-characters)
   (npc-characters
    :initform (make-hash-table :test 'equal)
    :initarg  :npc-characters
    :accessor npc-characters)
   (selected-pc
    :initform nil
    :initarg  :selected-pc
    :accessor selected-pc)))

(defgeneric setup-game-hour (object hour))

(defgeneric prepare-map-state (object map))

(defgeneric el-type-in-pos (object x y))

(defgeneric entity-id-in-pos (object x y))

(defgeneric build-movement-path (object start end))

(defgeneric terrain-aabb-2d (object))

(defgeneric terrain-aabb-2d (object))

(defgeneric push-entity  (object entity))

(defgeneric map-level (object))

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

(gen-map-state-reader el-type entity-id)

(defmethod prepare-map-state ((object game-state) (map random-terrain))
  (with-accessors ((map-state map-state)) object
     (setf map-state
 	  (gen-matrix-frame (truncate (* (width  (matrix map)) +terrain-chunk-size-scale+))
 			    (truncate (* (height (matrix map)) +terrain-chunk-size-scale+))))
     (loop for i from 0 below (length (data map-state)) do
	  (setf (elt (data map-state) i) (make-instance 'map-state-element)))))

(defmethod build-movement-path ((object game-state) start end)
  (with-accessors ((movement-costs movement-costs)) object
    (let ((tree	(graph:astar-search movement-costs
				    (graph:node->node-id movement-costs start)
				    (graph:node->node-id movement-costs end)
				    :heuristic-cost-function (graph:heuristic-manhattam))))
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
				  :key-datum #'id
				  :key       #'id))))

(defmethod map-level ((object game-state))
  (truncate (/ (+ (level-difficult object)
		  (1+ (* 8 (num:smoothstep-interpolate (d +minimium-map-size+)
						       (d +maximum-map-size+)
						       (d (width (map-state object)))))))
	       2)))
