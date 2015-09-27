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

(in-package :world)

(defun make-skydome (bg cloud-1 cloud-2 cloud-3 smoke)
  (let ((mesh (gen-skydome 100.0)))
    ;; background
    (setf (texture:interpolation-type bg) :linear)
    (texture:prepare-for-rendering bg)
    (setf (texture-object mesh) bg)
    ;; cloud layer 1
    (setf (texture::interpolation-type cloud-1) :linear)
    (texture:prepare-for-rendering cloud-1)
    (setf (elt (texture-clouds mesh) 0) cloud-1)
    ;; cloud layer 2
    (setf (texture::interpolation-type cloud-2) :linear)
    (texture:prepare-for-rendering cloud-2)
    (setf (elt (texture-clouds mesh) 1) cloud-2)
    ;; cloud layer 3
    (setf (texture::interpolation-type cloud-3) :linear)
    (texture:prepare-for-rendering cloud-3)
    (setf (elt (texture-clouds mesh) 2) cloud-3)
    ;; smoke
    (setf (texture:interpolation-type smoke) :linear)
    (setf (texture:s-wrap-mode smoke) :clamp-to-border)
    (setf (texture:t-wrap-mode smoke) :clamp-to-border)
    (setf (texture:border-color smoke) §c00000000)
    (texture:prepare-for-rendering smoke)
    (setf (texture-projector mesh) smoke)
    (prepare-for-rendering mesh)
    mesh))

(defclass doors ()
  ((door-n
    :accessor door-n
    :initarg :door-n
    :initform nil)
   (door-s
    :accessor door-s
    :initarg :door-s
    :initform nil)
   (door-e
    :accessor door-e
    :initarg :door-e
    :initform nil)
   (door-w
    :accessor door-w
    :initarg :door-w
    :initform nil)))

(defmethod destroy ((object doors))
  (destroy (door-n object))
  (destroy (door-s object))
  (destroy (door-e object))
  (destroy (door-w object))
  (setf    (door-n object) nil
	   (door-s object) nil
	   (door-e object) nil
	   (door-w object) nil))

(defclass world (transformable renderizable)
  ((camera
    :accessor camera
    :initarg :camera
    :initform nil)
   (skydome
    :accessor skydome
    :initarg :skydome
    :initform nil)
   (entities
    :accessor entities
    :initarg :entities
    :initform (make-instance 'quad-tree:quad-tree
			     :aabb (vec4:vec4 0.0 0.0
					      (num:d +map-max-size+)
					      (num:d +map-max-size+))))
    (main-state
    :initarg :main-state
    :initform nil
    :allocation :class)
   (frame-window
    :accessor frame-window
    :initarg :frame-window
    :initform nil)
   (trees-bag
    :accessor trees-bag
    :initarg  :trees-bag
    :initform nil)
   (walls-bag
    :accessor walls-bag
    :initarg  :walls-bag
    :initform nil)
   (floor-bag
    :accessor floor-bag
    :initarg  :floor-bag
    :initform nil)
   (doors-bag
    :accessor doors-bag
    :initarg  :doors-bag
    :initform (make-instance 'doors))
   (furnitures-bag
    :accessor furnitures-bag
    :initarg  :furnitures-bag
    :initform nil)
   (containers-bag
    :accessor containers-bag
    :initarg  :containers-bag
    :initform nil)
   (magic-furnitures-bag
    :accessor magic-furnitures-bag
    :initarg  :magic-furnitures-bag
    :initform nil)
   (windows-bag
    :accessor windows-bag
    :initarg  :windows-bag
    :initform nil)
   (pillars-bag
    :accessor pillars-bag
    :initarg  :pillars-bag
    :initform nil)
   (chairs-bag
    :accessor chairs-bag
    :initarg  :chairs-bag
    :initform nil)
   (tables-bag
    :accessor tables-bag
    :initarg  :tables-bag
    :initform nil)
   (wall-decorations-bag
    :accessor wall-decorations-bag
    :initarg  :wall-decorations-bag
    :initform nil)
   (walkable-bag
    :accessor walkable-bag
    :initarg  :walkable-bag
    :initform nil)
   (gui
    :accessor gui
    :initarg  :gui
    :initform (make-instance 'widget:widget
			     :x 0.0 :y 0.0
			     :width  *window-w*
			     :height *window-h*
			     :label nil))))

(defgeneric (setf main-state) (new-state object))

(defgeneric get-window-size (object))

(defgeneric frustum-intersects-p (object ent))

(defgeneric frustum-bounding-sphere-intersects-p (object ent))

(defgeneric frustum-aabb-intersects-p (object ent))

(defgeneric initialize-skydome (object &key bg clouds-1 clouds-2 clouds-3 smoke weather-type))

(defgeneric render-gui (object))

(defgeneric highlight-tile-screenspace (object renderer x y))

(defgeneric highlight-path-costs-space (object renderer path))

(defgeneric iterate-quad-tree (object function probe))

(defgeneric iterate-quad-tree-anyway (object function))

(defgeneric all-furniture-bags-not-empty-p  (object))

(defgeneric all-furnitures-but-pillars-not-empty-p (object))

(defgeneric push-interactive-entity (object entity type occlusion-value))

(defgeneric set-map-state-type (object x y type))

(defgeneric set-map-state-id (object x y id))

(defgeneric set-map-state-occlusion (object x y occlusion-value))

(defgeneric setup-map-state-entity (object entity type occlusion-value))

(defgeneric setup-map-state-tile (object x y type id occlusion-value))

(defmethod iterate-quad-tree ((object world) function probe)
  (quad-tree:iterate-nodes-intersect (entities object)
				    function
				    probe))

(defmethod iterate-quad-tree ((object world) function probe)
  (quad-tree:iterate-nodes-intersect (entities object)
				    function
				    probe))


(defmacro walk-quad-tree ((object &optional (aabb `(aabb-2d (camera ,object)))) &body body)
  `(iterate-quad-tree ,object
		      #'(lambda (entities)
			  (loop named entity-loop for entity across (quad-tree:data entities) do
			       ,@body))
		      ,aabb))

(defmethod iterate-quad-tree-anyway ((object world) function)
  (quad-tree:iterate-nodes (entities object) function))

(defmacro walk-quad-tree-anyway ((object) &body body)
  `(iterate-quad-tree-anyway ,object
		      #'(lambda (entities)
			  (loop named entity-loop for entity across (quad-tree:data entities) do
			       ,@body))))

(defmethod main-state ((object world))
  (slot-value object 'main-state))

(defmethod destroy ((object world))
  (destroy (skydome object))
  (setf    (skydome object) nil)
  (map nil #'destroy (trees-bag object))
  (setf (trees-bag object) nil)
  (destroy (walls-bag object))
  (setf (walls-bag object) nil)
  (destroy (floor-bag object))
  (setf (floor-bag object) nil)
  (destroy (doors-bag object))
  (setf (doors-bag object) nil)
  (map nil #'destroy (furnitures-bag object))
  (setf (furnitures-bag object) nil)
  (map nil #'destroy (containers-bag object))
  (setf (containers-bag object) nil)
  (map nil #'destroy (magic-furnitures-bag object))
  (setf (magic-furnitures-bag object) nil)
  (map nil #'destroy (pillars-bag object))
  (setf (pillars-bag object) nil)
  (map nil #'destroy (chairs-bag object))
  (setf (chairs-bag object) nil)
  (map nil #'destroy (tables-bag object))
  (setf (tables-bag object) nil)
  (map nil #'destroy (wall-decorations-bag object))
  (setf (wall-decorations-bag object) nil)
  (map nil #'destroy (walkable-bag object))
  (setf (walkable-bag object) nil)
  (destroy (windows-bag object))
  (setf (windows-bag object) nil)
  (destroy (gui object))
  (setf (gui object) nil)
  (quad-tree:iterate-nodes (entities object)
			   #'(lambda (a) (map 'nil #'destroy (quad-tree:data a))))
  (quad-tree:iterate-nodes (entities object)
			   #'(lambda (a) (setf (quad-tree:data a) nil)))
  (setf (entities object) nil))

(defmethod (setf main-state) (new-state (object world))
  (with-slots (main-state skydome camera) object
    (setf main-state new-state)
    (setf (entity:state skydome) new-state)
    (setf (entity:state camera) new-state)))

(defmacro gen-accessors-matrix (name)
  (let ((fn-name (list (alexandria:format-symbol t "~:@(setf~)")
		       (alexandria:format-symbol t "~:@(~a~)" name)))
	(get-name (alexandria:format-symbol t "~:@(~a~)" name)))
    `(defmethod ,fn-name (new-value (object world))
       (with-slots (,name) object
	 (setf (elt ,name 0) new-value))
       (setf (,get-name (camera object)) new-value))))

(gen-accessors-matrix model-matrix)

(gen-accessors-matrix view-matrix)

(gen-accessors-matrix projection-matrix)

(defmethod build-projection-matrix ((object world) near far fov ratio)
  (build-projection-matrix (camera object) near far fov ratio))

(defmethod initialize-instance :after ((object world) &key &allow-other-keys)
  (setf (camera object) (make-instance 'camera :pos (vec 0.0 0.0 1.0)))
  ;; gui
  (let* ((char  (character:make-warrior :human))
	 (chest (random-container:generate-container 10))
	 (obj1   (random-shoes:generate-shoes 10))
	 (obj2   (random-elm:generate-elm 10))
	 (obj3   (random-potion:generate-potion 10))
	 (texture-portrait (texture:gen-name-and-inject-in-database
			    (avatar-portrait:build-avatar "m"))))
    (texture:prepare-for-rendering texture-portrait)
    (setf (character:portrait char) texture-portrait)
    (setf (character:first-name char)  "first"
	  (character:last-name char)   "last")
    (setf (character:inventory char)
	  (list (random-ring:generate-ring     10)
		(random-weapon:generate-weapon 10 :sword)
		(random-armor:generate-armor   10)))
    (add-child chest obj1)
    (add-child chest obj2)
    (add-child chest obj3)
    (setf (character:portrait char) texture-portrait)
    (let* ((toolbar (make-instance 'widget:main-toolbar
				   :x 0.0
				   :y 0.0
				   :width  (num:d *window-w*)
				   :height (num:d *window-h*)))
	   (gen-player-test (widget:make-player-generator))
	   (report          (widget:make-player-report-win  char))
	   (inventory-test  (widget:make-inventory-window char chest))
	   (message         (widget:make-message-box "Test message"
						     "test"
						     :info
						     (cons (_ "yes") nil)
						     (cons (_ "no")  nil))))
      (add-child (gui object) toolbar)
      (add-child (gui object) gen-player-test)
      (add-child (gui object) inventory-test)
      (add-child (gui object) report)
      (add-child (gui object) message))))

(defmethod calculate ((object world) dt)
  (incf (current-time (main-state object)) dt)
  (setf (widget:label (widget:text-fps (elt (mtree-utils:children (gui object)) 0)))
	(format nil "~,2f" (main-window:fps (frame-window object))))
  (calculate (gui object) dt)
  (calculate (camera object)  dt)
  (calculate (skydome object) dt)
  (calculate-frustum (camera object))
  (calculate-aabb    (camera object))
  (quad-tree:iterate-nodes (entities object)
			   #'(lambda (a) (map 'nil #'(lambda(e) (calculate e dt))
					      (quad-tree:data a)))))

(defmethod render ((object world) (renderer world))
  (render (camera  object) object)
  (render (skydome object) object)
  (walk-quad-tree (object)
    (when (and (not (water-mesh-p entity))
	       (frustum-intersects-p object entity))
      (render entity renderer)))
  (walk-quad-tree (object)
    (when (and (water-mesh-p entity)
	       (frustum-intersects-p object entity))
      (render entity renderer))))

(defmethod render-for-reflection ((object world) (renderer world))
  (render-for-reflection (camera  object) object)
  (render-for-reflection (skydome object) object)
  (walk-quad-tree (object)
    (when (frustum-intersects-p object entity)
      (render-for-reflection entity renderer))))

(defmethod pick-pointer-position ((object world) renderer x y)
  (walk-quad-tree (object)
    (multiple-value-bind (picked cost-matrix-position matrix-position raw-position)
	(pick-pointer-position entity renderer x y)
      (when picked
	(return-from pick-pointer-position
	  (values cost-matrix-position matrix-position raw-position)))))
  nil)

(defmethod highlight-tile-screenspace ((object world) renderer x y)
  "Coordinates in screen space"
    (walk-quad-tree-anyway (object)
      (multiple-value-bind (picked cost-matrix-position matrix-position raw-position)
	  (pick-pointer-position entity renderer x y)
	(declare (ignore raw-position))
	(when picked
	  (turn-off-highligthed-tiles object)
	  (set-tile-highlight entity
			      (elt matrix-position 1) ; row
			      (elt matrix-position 0) ; column
			      :clear-highligthed-set nil
			      :add-to-highligthed-set t)
	  (return-from highlight-tile-screenspace cost-matrix-position))))
    nil)

(defmethod highlight-path-costs-space ((object world) renderer path)
  "Path contains coordinates from cost matrix"
  (declare (ignore renderer))
  (turn-off-highligthed-tiles object)
  (map nil
       #'(lambda (coord)
	   (block walking
	     (walk-quad-tree-anyway (object)
	       (when (pickable-mesh-p entity)
		 (let ((tile-coord (cost-coord->lookup-tile entity (elt coord 1) (elt coord 0))))
		   (handler-bind ((null-tile-element
				   #'(lambda(e)
				       (declare (ignore e))
				       (invoke-restart 'pickable-mesh::use-value nil)))
				  (out-of-bonds-tile-element
				   #'(lambda(e)
				       (declare (ignore e))
				       (invoke-restart 'pickable-mesh::use-value nil))))
		     (when (set-tile-highlight entity (elt tile-coord 1) (elt tile-coord 0)
					       :add-to-highligthed-set t
					       :clear-highligthed-set nil)
		       (return-from walking))))))))
       path))

(defmethod selected-pc ((object world))
  (selected-pc (main-state object)))

(defmethod turn-off-highligthed-tiles ((object world))
  (walk-quad-tree (object)
    (when (pickable-mesh-p entity)
      (turn-off-highligthed-tiles entity)))
  object)

(defmethod main-light-pos ((object world))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (celestial-body-position (main-state object)))

(defmethod main-light-pos-eye-space ((object world))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (transform-point (main-light-pos object)
		   (elt (the +transform-matrix-cointainer+
			     (view-matrix (camera  object))) 0)))

(defmethod main-light-color ((object world))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (light-color (main-state object)))

(defmethod elapsed-time ((object world))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (current-time (main-state object)))

(defmethod push-entity ((object world) entity)
  (bubbleup-modelmatrix entity)
  (quad-tree:push-down (entities object) entity)
  (let ((paths '()))
    (quad-tree:iterate-nodes (entities object)
			    #'(lambda (q)
				(when (not (misc:vector-empty-p (quad-tree:data q)))
				  (setf paths (mapcar #'quad-tree:node-quadrant
						      (quad-tree:path-to q))))))))

(defmethod (setf compiled-shaders) (new-value (object world))
  (with-slots (compiled-shaders) object
    (setf compiled-shaders  new-value)
    (setf (compiled-shaders (skydome object)) new-value)))

(defmethod get-camera-pos ((object world))
  (pos (camera object)))

(defmethod frustum-intersects-p ((object world) ent)
  (or (frustum-bounding-sphere-intersects-p object ent)
      (frustum-aabb-intersects-p            object ent)))

(defmethod frustum-bounding-sphere-intersects-p ((object world) ent)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  (let* ((sphere (bounding-sphere ent))
	 (center (sphere-center sphere))
	 (radius (sphere-radius sphere))
	 (planes (frustum-planes (camera object))))
    (declare ((simple-array vec4:vec4 (6)) planes))
    (loop for plane across planes do
	 (let* ((normal   (vec (elt plane 0) (elt plane 1) (elt plane 2)))
		(distance (elt plane 3))
		(totally-outside (num:d< (num:d+ (dot-product normal center) distance)
					 (num:d- radius))))
	   (when totally-outside
	     (return-from frustum-bounding-sphere-intersects-p nil))))
    t))

(defmethod frustum-aabb-intersects-p ((object world) ent)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (world object))
  ;;      e          h
  ;;       +--------+
  ;;      /|       /|
  ;;     / |      / |
  ;; f  +--------+ g|
  ;;    |  |     |  |
  ;;    |a +-----|--+ d
  ;;    | /      | /
  ;;    |/       |/
  ;;  b +--------+ c
  (let* ((aabb  (aabb ent))
	 (min-x (min-x aabb))
	 (min-y (min-y aabb))
	 (min-z (min-z aabb))
	 (max-x (max-x aabb))
	 (max-y (max-y aabb))
	 (max-z (max-z aabb))
	 (a     (vec min-x min-y min-z))
	 (b     (vec min-x min-y max-z))
	 (c     (vec max-x min-y max-z))
	 (d     (vec max-x min-y min-z))
	 (e     (vec min-x max-y min-z))
	 (f     (vec min-x max-y max-z))
	 (g     (vec max-x max-y max-z))
	 (h     (vec max-x max-y min-z))
	 (planes (frustum-planes (camera object))))
    (declare ((simple-array vec4:vec4 (6)) planes))
    ;; if a plane with normal on  the opposite side af all this aabb's
    ;; vertices exists, there is no intersection.
    (loop for plane across planes do
	 (let ((totally-outside (and (not (plane-point-same-side-p plane a))
				     (not (plane-point-same-side-p plane b))
				     (not (plane-point-same-side-p plane c))
				     (not (plane-point-same-side-p plane d))
				     (not (plane-point-same-side-p plane e))
				     (not (plane-point-same-side-p plane f))
				     (not (plane-point-same-side-p plane g))
				     (not (plane-point-same-side-p plane h)))))
	   (when totally-outside
	     (return-from frustum-aabb-intersects-p nil))))
    t))

(defmethod initialize-skydome ((object world)
			       &key
				 (bg       (texture:get-texture texture:+skydome-bg+))
				 (clouds-1 (texture:get-texture texture:+cloud-1+))
				 (clouds-2 (texture:get-texture texture:+cloud-2+))
				 (clouds-3 (texture:get-texture texture:+cloud-3+))
				 (smoke    (texture:get-texture texture:+smoke-tray+))
				 (weather-type :foggy-night-clear-day))
  (setf (skydome object)   (make-skydome bg clouds-1 clouds-2 clouds-3 smoke)
	(weather-type (skydome object)) weather-type))

(defmethod get-window-size ((object world))
  (sdl2:get-window-size (frame-window object)))

(defmethod render-gui ((object world))
  (render (gui object) object))

(defmethod all-furniture-bags-not-empty-p ((object world))
  (and (furnitures-bag       object)
       (containers-bag       object)
       (magic-furnitures-bag object)
       (pillars-bag          object)))

(defmethod all-furnitures-but-pillars-not-empty-p ((object world))
  (and (furnitures-bag       object)
       (containers-bag       object)
       (magic-furnitures-bag object)))

(defmethod push-interactive-entity ((object world) entity type occlusion-value)
  "Use for any interactive entity (i.e. has a character)"
  (push-entity             object entity)  ; add to quadtree
  (push-entity (main-state object) entity) ; add to entity tree of game-state
  (setup-map-state-entity  object  entity type occlusion-value)) ; add to game-state's matrix

(defmethod set-map-state-type ((object world) x y type)
  (setf (el-type (matrix:matrix-elt (map-state (main-state object)) y x)) type))

(defmethod set-map-state-id ((object world) x y id)
  (setf (entity-id (matrix:matrix-elt (map-state (main-state object)) y x)) id))

(defmethod set-map-state-occlusion ((object world) x y occlusion-value)
  (setf (occlude (matrix:matrix-elt (map-state (main-state object)) y x)) occlusion-value))

(defmethod setup-map-state-entity ((object world) entity type occlusion-value)
  (with-accessors ((pos pos) (id identificable:id)) entity
    (let ((x-matrix (misc:coord-chunk->matrix (elt pos 0)))
	  (y-matrix (misc:coord-chunk->matrix (elt pos 2))))
      (set-map-state-type      object x-matrix y-matrix type)
      (set-map-state-id        object x-matrix y-matrix id)
      (set-map-state-occlusion object x-matrix y-matrix occlusion-value))))

(defmethod setup-map-state-tile ((object world) x y type id occlusion-value)
  (set-map-state-type      object x y type)
  (set-map-state-id        object x y id)
  (set-map-state-occlusion object x y occlusion-value))
