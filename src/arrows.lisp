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

(in-package :arrows)

(alexandria:define-constant +arrow-speed+         0.0005     :test #'=)

(alexandria:define-constant +arrow-model-name+   "model.obj" :test #'string=)

(defclass arrow ()
  ((trajectory
    :initform nil
    :initarg  :trajectory
    :accessor trajectory)
   (launcher-entity
    :initform nil
    :initarg  :launcher-entity
    :accessor launcher-entity)
   (attack-event-fn
    :initform nil
    :initarg  :attack-event-fn
    :accessor attack-event-fn)
   (hitted
    :initform nil
    :initarg  :hitted
    :reader   hittedp
    :writer   (setf hitted))))

(defun arrowp (a)
  (typep a 'arrow))

(defclass arrow-mesh (triangle-mesh arrow) ())

(defun hittable-by-arrow-p (entity)
  (and
   (not (terrain-chunk:terrain-chunk-p entity))
   (not (particles:particles-cluster-p entity))
   (not (arrowp entity))
   (renderp entity)))

(defun arrow-collision-p (arrow)
  (with-accessors ((trajectory trajectory)
		   (pos pos)
		   (state state)) arrow
    (game-state:with-world (world state)
      (quad-tree:iterate-nodes-intersect (world:entities world)
					  #'(lambda (node)
					      (loop for entity across (quad-tree:data node) do
						   (when (and (hittable-by-arrow-p entity)
							      (not (= (id arrow) (id entity)))
							      (overlapp (aabb entity) (aabb arrow)))
						     (return-from arrow-collision-p entity))))
					  (aabb-2d arrow)))))

(defmethod calculate :after ((object arrow-mesh) dt)
  (with-accessors ((trajectory trajectory)
		   (pos pos)
		   (state state)
		   (hittedp hittedp)
		   (launcher-entity launcher-entity)
		   (attack-event-fn attack-event-fn)) object
    (when (not hittedp)
      (game-state:with-world (world state)
	(let ((camera (world:camera world)))
	  (if (not (3d-utils:insidep (world:world-aabb world)
				     pos))
	      (progn
		;; remove from world
		(remove-entity-by-id world (id object))
		(setf (hitted object) nil)
		(setf (camera:followed-entity camera) nil)
		(setf (camera:mode camera) :fp))
	      (let ((intersected-entity (arrow-collision-p object)))
		(if (and intersected-entity
			 (not (= (id intersected-entity)
				 (id launcher-entity))))
		    (progn
		      ;; send attack event
		      (funcall attack-event-fn launcher-entity intersected-entity)
		      ;; remove from world
		      (remove-entity-by-id world (id object))
		      (setf (hitted object) t)
		      (setf (camera:followed-entity camera) nil)
		      (setf (camera:mode camera) :fp))
		    (progn
		      (incf (displacement trajectory) +arrow-speed+)
		      (setf pos (ray-ends trajectory pos))
		      ;; update quadtree
		      (world:move-entity world object nil :update-costs nil))))))))))

(defclass arrow-attack-spell-mesh (triangle-mesh arrow)
  ((aabb-size
    :initarg  :aabb-size
    :initform (d/ +terrain-chunk-tile-size+ 4.0)
    :accessor aabb-size)))

(defmethod aabb ((object arrow-attack-spell-mesh))
  (with-slots (aabb bounding-sphere) object
    (with-accessors ((model-matrix model-matrix)
		     (aabb-size aabb-size)) object
      (declare ((simple-vector 1) model-matrix))
      (let* ((p1  (transform-point (vec     aabb-size   (d- aabb-size)   aabb-size)
				   (elt model-matrix 0)))
	     (p2  (transform-point (vec (d- aabb-size)  aabb-size       (d- aabb-size))
				   (elt model-matrix 0)))
	     (res (make-instance 'aabb)))
	(expand res p1)
	(expand res p2)
	(setf bounding-sphere (aabb->bounding-sphere res)
	      aabb            res)
	res))))

(defmethod aabb-2d ((object arrow-attack-spell-mesh))
  (flatten-to-aabb2-xz-positive (aabb object)))

(defmethod render ((object arrow-attack-spell-mesh) renderer)
  (do-children-mesh (c object)
    (render c renderer)))

(defmethod removeable-from-world ((object arrow-attack-spell-mesh))
  (and (not (vector-empty-p (mtree:children object)))
       (removeable-from-world (elt (mtree:children object) 0))))

;; (defmethod render :after ((object arrow-attack-spell-mesh) renderer)
;;   (render-debug object renderer))

(defmethod calculate :after ((object arrow-attack-spell-mesh) dt)
  (with-accessors ((trajectory trajectory)
		   (pos pos)
		   (state state)
		   (hittedp hittedp)
		   (launcher-entity launcher-entity)
		   (attack-event-fn attack-event-fn)) object
    (when (not hittedp)
      (game-state:with-world (world state)
	(let ((camera (world:camera world)))
	  (if (not (3d-utils:insidep (world:world-aabb world)
				     pos))
	      (progn
		;; remove from world
		(remove-entity-by-id world (id object))
		(setf (hitted object) nil)
		(setf (camera:followed-entity camera) nil)
		(setf (camera:mode camera) :fp))
	      (let ((intersected-entity (arrow-collision-p object)))
		(if (and intersected-entity
			 (not (= (id intersected-entity)
				 (id launcher-entity))))
		    (progn
		      ;; send attack event
		      (funcall attack-event-fn launcher-entity intersected-entity)
		      (particles::set-respawn (elt (mtree:children object) 0) nil)
		      (setf (renderp (elt (mtree:children object) 0)) nil)
		      (setf (hitted object) t)
		      (setf (camera:followed-entity camera) nil)
		      (setf (camera:mode camera) :fp))
		    (progn
		      (incf (displacement trajectory) +arrow-speed+)
		      (setf pos (ray-ends trajectory pos))
		      ;; update quadtree
		      (world:move-entity world object nil :update-costs nil))))))))))

(defstruct arrow-db-entry id mesh)

(defparameter *arrows-factory-db* '())

(defun clean-db ()
  (map 'nil #'(lambda (a) (destroy (arrow-db-entry-mesh a))) *arrows-factory-db*)
  (setf *arrows-factory-db* nil))

(defun %get-arrow (id)
  (let ((res (find-if #'(lambda (a) (string= (arrow-db-entry-id a) id)) *arrows-factory-db*)))
    (and res (arrow-db-entry-mesh res))))

(defun get-arrow (name)
   (let* ((path (text-utils:strcat name fs:*directory-sep* +arrow-model-name+))
	  (id (res:get-resource-file path +arrows-resource+)))
     (or (%get-arrow id)
	 (let ((new-arrow (make-instance 'arrow-mesh))
	       (raw-mesh  (obj-mesh:load id)))
	   (clone-into raw-mesh new-arrow)
	   (prepare-for-rendering new-arrow)
	   (push (make-arrow-db-entry :id id :mesh new-arrow) *arrows-factory-db*)
	   new-arrow))))

(defun launch-ray (attacker defender)
  (let* ((ghost-atk    (entity:ghost attacker))
	 (weapon       (character:worn-weapon ghost-atk))
	 (weapon-type  (if weapon
			   (cond
			     ((character:bowp weapon)
			      :bow)
			     ((character:crossbowp weapon)
			      :crossbow)
			     (t
			      :spell))
			   :spell))
	 (attack-chance     (if (eq weapon-type :spell)
				(character:actual-attack-spell-chance ghost-atk)
				(character:actual-range-attack-chance ghost-atk)))
	 (ray-dir           (normalize (vec- (aabb-center (aabb defender))
					     (aabb-center (aabb attacker)))))
	 (ray               (make-instance 'ray
					   :ray-direction ray-dir
					   :displacement +arrow-speed+)))
    (if (and weapon-type
	     (d< (dabs (secure-dacos (dot-product (dir attacker)
						  ray-dir)))
		 +visibility-cone-half-hangle+))
	(progn
	  (when (not (die-utils:pass-d100.0 attack-chance))
	    (setf (ray-direction ray)
		  (transform-direction (ray-direction ray)
				       (rotate-around +y-axe+
						      (d* (dexpt -1.0
								 (d (lcg-next-in-range 1 3)))
							  (if (eq weapon-type :bow)
							      (lcg-next-in-range 0.05 1.0)
							      (lcg-next-in-range 0.01 0.5)))))))
	  ray)
	nil)))

(defun %common-launch-projectile (world attacker defender mesh attack-event-fn
				  &key
				    (position        (aabb-center (aabb attacker)))
				    (camera-follow-p t))
  (let ((ray (launch-ray attacker defender)))
    (if ray
	(progn
	  (setf (hitted mesh) nil)
	  (setf (attack-event-fn mesh) attack-event-fn)
	  (setf (pos mesh) position)
	  (setf (trajectory mesh) ray)
	  (setf (dir mesh) (normalize (vec-negate (ray-direction (trajectory mesh)))))
	  (setf (launcher-entity mesh) attacker)
	  (setf (compiled-shaders mesh) (compiled-shaders world))
	  (when camera-follow-p
	    (setf (camera:followed-entity (world:camera world)) mesh)
	    (setf (camera:mode (world:camera world)) :follow))
	  t)
	nil)))

(defun launch-arrow (name world attacker defender)
  (let* ((mesh (get-arrow name))
	 (successp (%common-launch-projectile world
					      attacker
					      defender
					      mesh
					      #'battle-utils:send-attack-long-range-event
					      :camera-follow-p t)))
    (when successp
      (world:push-entity world mesh))))

(defun launch-attack-spell (spell world attacker defender &key (invisiblep nil))
  (let* ((mesh (make-instance 'arrow-attack-spell-mesh
			      :aabb-size (d* (d/ +terrain-chunk-tile-size+ 8.0)
					     (d (spell:level spell)))))
	 (successp (%common-launch-projectile world
					      attacker
					      defender
					      mesh
					      #'battle-utils:send-attack-spell-event
					      :camera-follow-p t)))
    ;; testing aabb
    ;; (prepare-for-rendering mesh)
    ;; (setf (render-aabb mesh) t)
    ;;end testing
    (when successp
      (when (not invisiblep)
	(let* ((shaders (compiled-shaders world))
	       (bullet  (funcall (spell:arrow spell)
				 +zero-vec+
				 +entity-forward-direction+
				 shaders))
	       (blocker (make-instance 'mesh:blocker-render-children))
	       (target-effect (funcall (spell:visual-effect-target spell)
				       +zero-vec+
				       shaders)))
	  (mtree:add-child mesh bullet)
	  (mtree:add-child (elt (mtree:children mesh) 0) blocker)
	  (mtree:add-child blocker target-effect)))
      (world:push-entity world mesh))))

(defun launch-spell (spell world attacker defender)
  (let* ((range        (spell:range spell))
	 (pos-attacker (pos attacker))
	 (x-attacker   (map-utils:coord-chunk->matrix (elt pos-attacker 0)))
	 (z-attacker   (map-utils:coord-chunk->matrix (elt pos-attacker 2)))
	 (pos-defender (pos defender))
	 (x-defender   (map-utils:coord-chunk->matrix (elt pos-defender 0)))
	 (z-defender   (map-utils:coord-chunk->matrix (elt pos-defender 2)))
	 (dist         (map-utils:map-manhattam-distance (ivec2:ivec2 x-attacker z-attacker)
							 (ivec2:ivec2 x-defender z-defender))))
    (if (<= dist range)
	(if (die-utils:pass-d100.0 (character:actual-spell-chance (ghost attacker)))
	    (let* ((shaders (compiled-shaders world))
		   (target-effect (funcall (spell:visual-effect-target spell)
					   (copy-vec (aabb-center (aabb defender)))
					   shaders)))
	      (setf (end-of-life-callback target-effect)
		    #'(lambda () (battle-utils:send-spell-event attacker defender)))
	      (world:push-entity world target-effect))
	    (billboard:apply-tooltip attacker
				     (format nil (_ "fail"))
				     :color     billboard:+damage-color+
				     :font-type gui:+tooltip-font-handle+
				     :activep   t))
	(billboard:apply-tooltip attacker
				 (format nil (_ "too far"))
				 :color     billboard:+damage-color+
				 :font-type gui:+tooltip-font-handle+
				 :activep   t))))
