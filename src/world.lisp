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
  (let ((mesh (mesh:gen-skydome 100.0)))
    ;; background
    (setf (texture:interpolation-type bg) :linear)
    (texture:prepare-for-rendering bg)
    (setf (mesh:texture-object mesh) bg)
    ;; cloud layer 1
    (setf (texture::interpolation-type cloud-1) :linear)
    (texture:prepare-for-rendering cloud-1)
    (setf (elt (mesh:texture-clouds mesh) 0) cloud-1)
    ;; cloud layer 2
    (setf (texture::interpolation-type cloud-2) :linear)
    (texture:prepare-for-rendering cloud-2)
    (setf (elt (mesh:texture-clouds mesh) 1) cloud-2)
    ;; cloud layer 3
    (setf (texture::interpolation-type cloud-3) :linear)
    (texture:prepare-for-rendering cloud-3)
    (setf (elt (mesh:texture-clouds mesh) 2) cloud-3)
    ;; smoke
    (setf (texture:interpolation-type smoke) :linear)
    (setf (texture:s-wrap-mode smoke) :clamp-to-border)
    (setf (texture:t-wrap-mode smoke) :clamp-to-border)
    (setf (texture:border-color smoke) §c00000000)
    (texture:prepare-for-rendering smoke)
    (setf (mesh:texture-projector mesh) smoke)
    (mesh:prepare-for-rendering mesh)	
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
    ;; TODO change with a quad-tree
    :initform (misc:make-fresh-array 0 nil 'triangle-mesh nil))
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
   (windows-bag 
    :accessor windows-bag
    :initarg  :windows-bag
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

(defgeneric push-entity (object entity))

(defgeneric get-window-size (object))

(defgeneric frustum-intersects-p (object ent))

(defgeneric frustum-bounding-sphere-intersects-p (object ent))
  
(defgeneric frustum-aabb-intersects-p (object ent))

(defgeneric initialize-skydome (object &key bg clouds-1 clouds-2 clouds-3 smoke weather-type))

(defgeneric render-gui (object))

(defmethod main-state ((object world))
  (slot-value object 'main-state))

(defmethod destroy ((object world))
  (destroy (skydome object))
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
  (destroy (windows-bag object))
  (setf (windows-bag object) nil)
  (destroy (gui object))
  (setf (gui object) nil)
  ;; TODO sostituire con quadtree
  (loop for entity across (entities object) do
       (destroy entity))
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

(defmethod initialize-instance :after ((object world) &key &allow-other-keys)
  (setf (camera object) (make-instance 'camera :pos (vec 0.0 0.0 1.0)))
  ;; gui
  (let* ((toolbar (make-instance 'widget:main-toolbar
				 :x 0.0 :y 0.0
				 :width  (num:d *window-w*)
				 :height (num:d *window-h*)))
	 (player-character (widget:make-player-generator)))
    (add-child (gui object) toolbar)
    (add-child (gui object) player-character)))

(defmethod calculate ((object world) dt)
  (incf (current-time (main-state object)) dt)
  (setf (widget:label (widget:text-fps (elt (mtree-utils:children (gui object)) 0)))
	(format nil "~,2f" (main-window:fps (frame-window object))))
  (calculate (gui object) dt)
  (calculate (camera object)  dt)
  (calculate (skydome object) dt)
  (calculate-frustum (camera object))
  ;; TODO quadtree needed here
  (loop for entity across (entities object) do
       (calculate entity dt)))

(defmethod render ((object world) (renderer world))
  (render (camera  object) object)
  (render (skydome object) object)
  ;; TODO quadtree needed here
  (loop for entity across (entities object) do
       (when (frustum-intersects-p object entity)
	 (render entity object))))

(defmethod render-for-reflection ((object world) (renderer world))
  (render-for-reflection (camera  object) object)
  (render-for-reflection (skydome object) object)
  ;; TODO quadtree needed here
  (loop for entity across (entities object) do
       (when (frustum-intersects-p object entity)
	 (render-for-reflection entity object))))

(defmethod pick-pointer-position ((object world) renderer x y)
  (loop for entity across (entities object) do
       (let ((pos (pick-pointer-position entity renderer x y)))
	 (when pos
	   (return-from pick-pointer-position pos))))
  nil)

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
  (vector-push-extend entity (entities object)))

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
	(mesh:weather-type (skydome object)) weather-type))

(defmethod get-window-size ((object world))
  (sdl2:get-window-size (frame-window object)))

(defmethod render-gui ((object world))
  (render (gui object) object))
