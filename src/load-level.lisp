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

(in-package :load-level)

(defun setup-water (world map)
  (loop for water-aabb in (random-terrain:lakes-aabb map) do
       (let ((water (make-instance 'mesh:water :measures water-aabb)))
	 (setf (compiled-shaders water) (compiled-shaders world))
	 (mesh:prepare-for-rendering water)
	 (world:push-entity world water))))

(defun coord-layer->map-state (a)
  (truncate (num:d* (num:desired a) +terrain-chunk-size-scale+)))

(defun set-map-state-type (world x y type)
  (setf (el-type (matrix-elt (map-state (main-state world)) y x)) type))

(defun set-map-state-id (world x y id)
  (setf (entity-id (matrix-elt (map-state (main-state world)) y x)) id))

(defun setup-map-state-tile (world x y type id)
  (set-map-state-type world x y type)
  (set-map-state-id   world x y id))

(defun setup-trees (world map)
  (let ((tree-bag-sorted nil))
    (loop for tree-pos in (random-terrain:trees map) do
	 (let* ((tree-data (num:random-select-by-frequency
			    (world:trees-bag world)
			    :key #'(lambda (a) (/ 1 (length (mesh:triangles a))))
			    :sort (prog1
				      (if tree-bag-sorted nil t)
				    (when (not tree-bag-sorted)
				      (setf tree-bag-sorted t)))
			    :sort-predicate #'<
			    :normalize t))
		(tree      (mesh:fill-shell-from-mesh tree-data 'mesh:tree-mesh-shell)))
	   (setf (compiled-shaders tree) (compiled-shaders world))
	   (setf (entity:pos tree)
		 (vec (coord-terrain->chunk (elt tree-pos 0))
		      +zero-height+
		      (coord-terrain->chunk (elt tree-pos 1))))
	   (setup-map-state-tile world
				 (coord-layer->map-state (elt tree-pos 0))
				 (coord-layer->map-state (elt tree-pos 1))
				 +tree-type+
				 (identificable:id tree))
	   (world:push-entity world tree)))))

(defun setup-door (world type min-x min-y x y)
  (let* ((door-type-fn (ecase type
			   (:door-n
			    #'door-n)
			   (:door-s
			    #'door-s)
			   (:door-e
			    #'door-e)
			   (:door-w
			    #'door-w)))
	 (door-type (ecase type
		      (:door-n
		       +door-n-type+)
		      (:door-s
		       +door-s-type+)
		      (:door-e
		       +door-e-type+)
		      (:door-w
		       +door-w-type+)))
	 (door-shell (mesh:fill-shell-from-mesh (funcall door-type-fn (doors-bag world))
						'mesh:triangle-mesh-shell)))
    (setf (compiled-shaders door-shell) (compiled-shaders world))
    ;; set the mesh
    (setf (entity:pos door-shell)
	  (vec (d+ (d* +terrain-chunk-size-scale+ min-x)
		   (coord-map->chunk (d (+ x min-x))))
	       +zero-height+
	       (d+ (d* +terrain-chunk-size-scale+ min-y)
		   (coord-map->chunk (d (+ y min-y))))))
    (setup-map-state-tile world
			  (truncate (+ x (coord-layer->map-state min-x)))
			  (truncate (+ y (coord-layer->map-state min-y)))
			  door-type
			  (identificable:id door-shell))
    (world:push-entity world door-shell)))

(defun setup-furnitures (world min-x min-y x y)
  (when (furnitures-bag world)
    (let* ((choosen             (random-elt (furnitures-bag world)))
	   (shell               (mesh:fill-shell-from-mesh choosen))
	   (mesh-x              (d+ (d* +terrain-chunk-size-scale+ min-x)
				    (coord-map->chunk (d (+ x min-x)))))
	   (mesh-y              (d+ (d* +terrain-chunk-size-scale+ min-y)
				    (coord-map->chunk (d (+ y min-y))))))
      (setf (compiled-shaders shell)               (compiled-shaders world)
            (entity:pos shell)	                   (vec mesh-x +zero-height+ mesh-y))
      (setup-map-state-tile world
			    (truncate (+ x (coord-layer->map-state min-x)))
			    (truncate (+ y (coord-layer->map-state min-y)))
			    +furniture-type+
			    (identificable:id shell))
      ;; TODO
      ;; add character
      (world:push-entity world shell))))

(defun setup-wall (world bmp min-x min-y x y &key (chance 5))
  (let* ((dice-roll       (lcg-next-upto chance))
	 (mesh            (walls-bag world))
	 (decal-side-free (and (< (1+ y) (width bmp))
			       (not (random-labyrinth:invalicablep
				     (matrix-elt bmp (1+ y) x)))))
	 (shell           (mesh:fill-shell-from-mesh mesh
						     (if (and (null (mesh:normal-map mesh))
							      (= (mod dice-roll chance) 0)
							      decal-side-free)
							 'mesh:wall-mesh-shell
							 'mesh:triangle-mesh-shell))))
    (setf (compiled-shaders shell) (compiled-shaders world))
    (setf (entity:pos shell)
	  (vec (d+ (d* +terrain-chunk-size-scale+ min-x)
		   (coord-map->chunk (d (+ x min-x))))
	       +zero-height+
	       (d+ (d* +terrain-chunk-size-scale+ min-y)
		   (coord-map->chunk (d (+ y min-y))))))
    (setup-map-state-tile world
			  (truncate (+ x (coord-layer->map-state min-x)))
			  (truncate (+ y (coord-layer->map-state min-y)))
			  +wall-type+
			  (identificable:id shell))
    (when (typep shell 'mesh:wall-mesh-shell)
      (setf (mesh:texture-projector shell)
	    (random-elt (texture:list-of-texture-by-tag
			 texture:+texture-tag-int-decal+)))
      (setf (texture:s-wrap-mode     (mesh:texture-projector shell)) :clamp-to-border)
      (setf (texture:t-wrap-mode     (mesh:texture-projector shell)) :clamp-to-border)
      (setf (texture:border-color    (mesh:texture-projector shell)) §c00000000)
      (texture:prepare-for-rendering (mesh:texture-projector shell))
      (mesh:setup-projective-texture shell))
    shell))

(defun setup-window (world min-x min-y x y)
  (let* ((mesh            (windows-bag world))
	 (shell           (mesh:fill-shell-from-mesh mesh 'mesh:triangle-mesh-shell)))
    (setf (compiled-shaders shell) (compiled-shaders world))
    (setf (entity:pos shell)
	  (vec (d+ (d* +terrain-chunk-size-scale+ min-x)
		   (coord-map->chunk (d (+ x min-x))))
	       +zero-height+
	       (d+ (d* +terrain-chunk-size-scale+ min-y)
		   (coord-map->chunk (d (+ y min-y))))))
    (setup-map-state-tile world
			  (truncate (+ x (coord-layer->map-state min-x)))
			  (truncate (+ y (coord-layer->map-state min-y)))
			  +wall-type+
			  (identificable:id shell))
    shell))

(defun setup-walls (world map)
  (loop
     for aabb in (labyrinths-aabb map)
     for bmp  in (mapcar #'(lambda (a)
			     (clean-and-redraw-mat a)
			     (shared-matrix        a))
			 (labyrinths map))             do
       (let* ((min-x   (iaabb2-min-x aabb))
	      (min-y   (iaabb2-min-y aabb)))
	 (loop-matrix (bmp x y)
	    (cond
	      ((wallp (matrix-elt bmp y x))
	       (let* ((wall-shell (setup-wall world bmp min-x min-y x y)))
		 (world:push-entity world wall-shell)))
	      ((windowp (matrix-elt bmp y x))
	       (let* ((window-shell (setup-window world min-x min-y x y)))
		 (world:push-entity world window-shell)))
	      ((door-n-p (matrix-elt bmp y x))
	       (setup-door world :door-s min-x min-y x y))
	      ((door-s-p (matrix-elt bmp y x))
	       (setup-door world :door-n min-x min-y x y))
	      ((door-e-p (matrix-elt bmp y x))
	       (setup-door world :door-e min-x min-y x y))
	      ((door-w-p (matrix-elt bmp y x))
	       (setup-door world :door-w min-x min-y x y))
	      ((furniturep (matrix-elt bmp y x))
	       (setup-furnitures world min-x min-y x y)))))))
		
(defun setup-single-floor (world aabb)
  (let* ((rect            (aabb2->rect2 aabb))
	 (ref-mesh        (floor-bag world))
	 (min-x           (aabb2-min-x aabb))
	 (min-y           (aabb2-min-y aabb))
	 (w               (d* +terrain-chunk-size-scale+
			      +terrain-chunk-tile-size+ (elt rect 2)))
	 (h               (d* +terrain-chunk-size-scale+
			      +terrain-chunk-tile-size+ (elt rect 3)))
	 (w/2             (d/ w 2.0))
	 (h/2             (d/ h 2.0))
	 (actual-x        (d+ (d* +terrain-chunk-size-scale+ min-x)
			      (coord-map->chunk min-x :tile-offset 0.0)
			      (coord-map->chunk (d/ w 2.0) :tile-offset 0.0)))
	 (actual-z        (d+ (d* +terrain-chunk-size-scale+ min-y)
			      (coord-map->chunk min-y :tile-offset 0.0)
			      (coord-map->chunk (d/ h 2.0) :tile-offset 0.0)))
	 (transformation  (sb-cga:matrix*
			   (sb-cga:translate (vec actual-x +zero-height+ actual-z))
			   (sb-cga:translate (3d-utils:vec-negate (sb-cga:vec w/2 0.0 h/2)))))
	 (mesh            (building-floor-mesh:floor-tile w h :wrapper-transformation
							  transformation)))
    (setf (compiled-shaders mesh) (compiled-shaders world))
    (setf (mesh:texture-object  mesh) (mesh:texture-object ref-mesh)
	  (mesh:normal-map      mesh) (mesh:normal-map ref-mesh)
	  (mesh:material-params mesh) (clone (mesh:material-params ref-mesh)))
    (prepare-for-rendering mesh)
    (building-floor-mesh:setup-texture-coord-scaling mesh)
    ;;for each tile
    (loop for x from 0.0 below (d/ w +terrain-chunk-size-scale+) do
	 (loop for y from 0.0 below (d/ h +terrain-chunk-size-scale+) do
	      (setup-map-state-tile world
				    (truncate (+ x (coord-layer->map-state min-x)))
				    (truncate (+ y (coord-layer->map-state min-y)))
				    +floor-type+
				    (Identificable:id mesh))))
    mesh))

(defun build-ceiling-mesh (w h)
  (let* ((tag        (elt +available-level-ceil+ *building-level*))
	 (texture    (random-elt (texture:list-of-texture-by-tag tag)))
	 (normal-map (gen-normalmap-if-needed texture))
	 (sw         (d* w +terrain-chunk-tile-size+))
	 (sh         (d* h +terrain-chunk-tile-size+))
	 (mesh       (mesh:gen-ceiling sw sh w h (d* +wall-h+ 0.1))))
    (setf (texture:use-mipmap texture) t)
    (setf (texture:interpolation-type texture) :linear)
    (interfaces:prepare-for-rendering texture)
    (when normal-map
      (mesh:gen-tangents mesh)
      (setf (texture:use-mipmap normal-map) t)
      (setf (texture:interpolation-type normal-map) :linear)
      (setf (texture:tags normal-map) #("normalmap ceiling"))
      (interfaces:prepare-for-rendering normal-map))
    (setf (mesh:texture-object mesh) texture)
    (mesh:get-material-from-texture mesh)
    (setf (mesh:normal-map mesh)     normal-map)
    (mesh:transform-vertices mesh (sb-cga:translate* 0.0 (d* 1.05 +wall-h+) 0.0))
    (interfaces:prepare-for-rendering mesh)
    mesh))

(defun setup-single-ceiling (world aabb)
  (let* ((rect            (aabb2->rect2 aabb))
	 (min-x           (iaabb2-min-x aabb))
	 (min-y           (iaabb2-min-y aabb))
	 (w               (d* +terrain-chunk-size-scale+ (elt rect 2)))
	 (h               (d* +terrain-chunk-size-scale+ (elt rect 3)))
	 (mesh            (build-ceiling-mesh w h)))
    (setf (compiled-shaders mesh) (compiled-shaders world))
    (setf (entity:pos mesh)
	  (vec (d+ (d* +terrain-chunk-size-scale+ min-x)
		   (coord-map->chunk min-x :tile-offset 0.0)
		   (coord-map->chunk (d/ w 2.0) :tile-offset 0.0))
	       +zero-height+
	       (d+ (d* +terrain-chunk-size-scale+ min-y)
		   (coord-map->chunk min-y :tile-offset 0.0)
		   (coord-map->chunk (d/ h 2.0) :tile-offset 0.0))))
    mesh))

(defun setup-floor (world map)
  (loop for aabb in (labyrinths-aabb map) do
       (let ((mesh (setup-single-floor world aabb)))
	 (pickable-mesh:populate-lookup-triangle-matrix mesh)
	 (push-entity world mesh))))

(defun setup-ceiling (world map)
  (loop for aabb in (labyrinths-aabb map) do
       (push-entity world (setup-single-ceiling world aabb))))

(defun setup-terrain (world map)
  (let ((whole (terrain-chunk:make-terrain-chunk map (compiled-shaders world))))
    (loop for aabb in (labyrinths-aabb map) do
	 (when config:+debug-mode+
	   (misc:dbg "Clip ~a -> ~a" aabb (labyrinths-aabb map)))
     	 (terrain-chunk:clip-with-aabb whole
     				       (map 'vector
     					    #'(lambda (a)
     						(coord-terrain->chunk a :tile-offset 0.0))
     					    aabb)
				       :regenerate-rendering-data t
     				       :clip-if-inside t))
    (pickable-mesh:populate-lookup-triangle-matrix whole)
    (push-entity world whole)))

(defun load-level (world game-state compiled-shaders file)
  (let* ((actual-file (res:get-resource-file file +maps-resource+
					     :if-does-not-exists :error))
	 (resource-cache:*cache-reference-file* actual-file))
    (load actual-file :verbose nil :print nil)
    (initialize-skydome world)
    (setf (main-state world)          game-state)
    (setf (compiled-shaders world)    compiled-shaders)
    (setup-game-hour game-state       *game-hour*)
    (setf (movement-costs game-state) (cost-matrix *map*))
    (prepare-map-state game-state     *map*)
    (setf (trees-bag world)           *trees*)
    (setf (walls-bag world)           *wall*)
    (setf (windows-bag world)         *window*)
    (setf (doors-bag world)           (make-instance 'world:doors
						     :door-n *door-n*
						     :door-s *door-s*
						     :door-e *door-e*
						     :door-w *door-w*))
    (setf (floor-bag world)      *floor*)
    (setf (furnitures-bag world) *furnitures*)
    (setup-terrain  world        *map*)
    (setup-floor    world        *map*)
    (setup-ceiling  world        *map*)
    (setup-walls    world        *map*)
    (setup-trees    world        *map*)
    ;; keep  water for  last until  the data  structure to  hosts the
    ;; entities in world is a vector
    (setup-water    world        *map*)
    ;; setup map-state
    (values *map* *trees* *wall*)))
