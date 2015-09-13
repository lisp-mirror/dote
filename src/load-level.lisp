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

(alexandria:define-constant +terrain-chunk-cache-name+ "terrain-chunk" :test #'string=)

(defun setup-water (world map)
  (loop for water-aabb in (random-terrain:lakes-aabb map) do
       (let ((water (make-instance 'mesh:water :measures water-aabb)))
	 (setf (compiled-shaders water) (compiled-shaders world))
	 (mesh:prepare-for-rendering water)
	 (push-entity world water))))

(defun setup-trees (world map)
  (let ((tree-bag-sorted nil))
    (loop for tree-pos in (random-terrain:trees map) do
	 (let* ((tree-data (num:random-select-by-frequency
			    (trees-bag world)
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
	   (push-interactive-entity world tree +tree-type+ :occlude)))))

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
    ;; the character
    (setf (entity:ghost door-shell)
	  (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
    (push-interactive-entity world door-shell door-type :occlude)))

(defun calculate-furnitures-shares (level)
  (let* ((normal-share    (d+ 0.66 (d* (d- (d level) 1.0) 0.01)))
	 (all-rest        (d- 1.0 normal-share))
	 (weight          (dlerp (d/ (d level) +maximum-level-difficult+) 0.0 0.1))
	 (magic-share     (d+ (d* (d- 0.33 weight) all-rest)))
	 (container-share (d+ (d* (d+ 0.67 weight) all-rest))))
    (list (cons normal-share    +furniture-type+)
	  (cons magic-share     +magic-furniture-type+)
	  (cons container-share +container-type+))))

(defun setup-furnitures (world min-x min-y x y freq)
  (when (all-furnitures-but-pillars-not-empty-p world)
    (let* ((dice-roll (random-select-by-frequency freq :key #'car :sort nil :normalize nil))
	   (type-of-furniture (cdr dice-roll))
	   (choosen          (if (eq type-of-furniture +furniture-type+)
				 (random-elt (furnitures-bag world))
				 (if (eq type-of-furniture +magic-furniture-type+)
				     (random-elt (magic-furnitures-bag world))
				     (random-elt (containers-bag world)))))
	   (shell            (mesh:fill-shell-from-mesh choosen))
	   (mesh-x           (d+ (d* +terrain-chunk-size-scale+ min-x)
				 (coord-map->chunk (d (+ x min-x)))))
	   (mesh-y           (d+ (d* +terrain-chunk-size-scale+ min-y)
				 (coord-map->chunk (d (+ y min-y))))))
      (setf (compiled-shaders shell)               (compiled-shaders world)
            (entity:pos shell)	                   (vec mesh-x +zero-height+ mesh-y)
	    (entity:dir shell)                     (random-elt
						    (vector (vec  1.0 0.0  0.0)
							    (vec  0.0 0.0  1.0)
							    (vec -1.0 0.0  0.0)
							    (vec  0.0 0.0 -1.0))))
      (setf (entity:ghost shell)
	    (cond
	      ((eq type-of-furniture +furniture-type+)
	       (random-inert-object:generate-inert-object
		(game-state:map-level (main-state world))))
	      ((eq type-of-furniture +magic-furniture-type+)
	       (random-fountain:generate-fountain (game-state:map-level (main-state world))))
	      ((eq type-of-furniture +container-type+)
	       ;; TODO
	       ;; add character
	       )))
      (push-interactive-entity world shell type-of-furniture nil))))

(defun %relative-coord-furniture->cood-mat-state (min rel-coord)
  (truncate (+ (coord-layer->map-state min) rel-coord)))

(defun common-setup-furniture (world bag min-x min-y x y)
  (let* ((choosen  (random-elt bag))
	 (shell    (mesh:fill-shell-from-mesh choosen))
	 (mesh-x   (d+ (d* +terrain-chunk-size-scale+ min-x)
		       (coord-map->chunk (d (+ x min-x)))))
	 (mesh-y   (d+ (d* +terrain-chunk-size-scale+ min-y)
		       (coord-map->chunk (d (+ y min-y))))))
    (setf (compiled-shaders shell) (compiled-shaders world)
	  (entity:pos shell)	   (vec mesh-x +zero-height+ mesh-y))
    (setf (entity:ghost shell)
	  (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
    (values shell world)))

(defun setup-pillar (world min-x min-y x y)
  (when (pillars-bag world)
    (let ((shell (common-setup-furniture world (pillars-bag world) min-x min-y x y)))
      (push-interactive-entity world shell +pillar-type+ :occlude))))

(defun setup-walkable (world min-x min-y x y)
  (when (walkable-bag world)
    (let ((shell (common-setup-furniture world (walkable-bag world) min-x min-y x y)))
      (setf (entity:scaling shell) (vec (num:lcg-next-in-range 1.0 3.0)
					1.0
					(num:lcg-next-in-range 1.0 3.0)))
      (push-interactive-entity world shell +walkable-type+ nil)))) ;; does not occlude

(defun state-type (state-matrix x y)
  (game-state:el-type (matrix-elt state-matrix y x)))

(defun find-next-by-type (world x y type)
  "Note: no error check is done"
  (let ((state       (map-state (main-state world)))
	(all-next-to (matrix:gen-4-neighbour-counterclockwise x y :add-center nil)))
    (loop for i in all-next-to do
	 (when (eq (state-type state (elt i 0) (elt i 1)) type)
	   (return-from find-next-by-type (values i all-next-to))))
    (values nil all-next-to)))

(defun setup-chair (world min-x min-y x y)
  (when (chairs-bag world)
    (let* ((shell      (common-setup-furniture world (chairs-bag world) min-x min-y x y))
	   (x-world    (%relative-coord-furniture->cood-mat-state min-x x))
	   (y-world    (%relative-coord-furniture->cood-mat-state min-y y))
	   (table-near (find-next-by-type world x-world y-world +table-type+)))
      (when table-near
	(setf (entity:dir shell) (vec (d (- (elt table-near 0) x-world))
				      0.0
				      (d (- (elt table-near 1) y-world)))))
      (push-interactive-entity world shell +chair-type+ nil))))

(defun setup-table (world min-x min-y x y)
  (when (tables-bag world)
    (let ((shell (common-setup-furniture world (tables-bag world) min-x min-y x y)))
      (push-interactive-entity world shell +table-type+ nil))))

(defun setup-wall-decoration (world min-x min-y x y)
  (when (wall-decorations-bag world)
    (let* ((shell     (common-setup-furniture world
					      (wall-decorations-bag world)
					      min-x min-y x y))
	   (x-world   (%relative-coord-furniture->cood-mat-state min-x x))
	   (y-world   (%relative-coord-furniture->cood-mat-state min-y y))
	   (wall-near (find-next-by-type world x-world y-world +wall-type+)))
      (when wall-near
	(setf (entity:dir shell) (vec (d (- x-world (elt wall-near 0)))
				      0.0
				      (d (- y-world (elt wall-near 1)))))
	(setf (entity:pos shell)
	      (transform-point (entity:pos shell)
			       (sb-cga:translate* (d* (d (- (elt wall-near 0) x-world))
						      (d* 0.5 +terrain-chunk-tile-size+))
						  +wall-decoration-y+
						  (d* (d (- (elt wall-near 1) y-world))
						      (d* 0.5 +terrain-chunk-tile-size+)))))
	(push-interactive-entity world shell +wall-decoration-type+ nil)))))

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
							 'mesh:decorated-wall-mesh-shell
							 'mesh:wall-mesh-shell))))
    (setf (compiled-shaders shell) (compiled-shaders world))
    (setf (entity:pos shell)
	  (vec (d+ (d* +terrain-chunk-size-scale+ min-x)
		   (coord-map->chunk (d (+ x min-x))))
	       +zero-height+
	       (d+ (d* +terrain-chunk-size-scale+ min-y)
		   (coord-map->chunk (d (+ y min-y))))))
    (setf (entity:ghost shell)
	  (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
    (game-event:register-for-end-turn shell)
    (push-interactive-entity world shell +wall-type+ :occlude)
    (when (typep shell 'mesh:decorated-wall-mesh-shell)
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
	 (shell           (mesh:fill-shell-from-mesh mesh 'mesh:window-mesh-shell)))
    (setf (compiled-shaders shell) (compiled-shaders world))
    (setf (entity:pos shell)
	  (vec (d+ (d* +terrain-chunk-size-scale+ min-x)
		   (coord-map->chunk (d (+ x min-x))))
	       +zero-height+
	       (d+ (d* +terrain-chunk-size-scale+ min-y)
		   (coord-map->chunk (d (+ y min-y))))))
    (setf (entity:ghost shell)
	  (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
    (game-event:register-for-end-turn shell)
    (push-interactive-entity world shell +wall-type+ nil)
    shell))

(defun setup-labyrinths (world map)
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
	       (setup-wall world bmp min-x min-y x y))
	      ((windowp (matrix-elt bmp y x))
	       (setup-window world min-x min-y x y))
	      ((door-n-p (matrix-elt bmp y x))
	       (setup-door world :door-s min-x min-y x y))
	      ((door-s-p (matrix-elt bmp y x))
	       (setup-door world :door-n min-x min-y x y))
	      ((door-e-p (matrix-elt bmp y x))
	       (setup-door world :door-e min-x min-y x y))
	      ((door-w-p (matrix-elt bmp y x))
	       (setup-door world :door-w min-x min-y x y))
	      ((furniture-pillar-p  (matrix-elt bmp y x))
	       (setup-pillar world min-x min-y x y))
	      ((furniture-walkable-p  (matrix-elt bmp y x))
	       (setup-walkable world min-x min-y x y))
	      ((furniture-table-p  (matrix-elt bmp y x))
	       (setup-table world min-x min-y x y))
	      ((furniture-other-p (matrix-elt bmp y x))
	       (setup-furnitures world min-x min-y x y
				 (sort (calculate-furnitures-shares (game-state:level-difficult
								     (main-state world)))
				       #'<
				       :key #'car)))))
	 ;; we  can setup  wall decorations  and chair  only after  we
	 ;; arranged  the  other furnitures  as  the  first two  do  a
	 ;; look-up on game state matrix.
	 (loop-matrix (bmp x y)
	    (cond
	      ((furniture-chair-p  (matrix-elt bmp y x))
	       (setup-chair world min-x min-y x y))
	      ((furniture-wall-decoration-p  (matrix-elt bmp y x))
	       (setup-wall-decoration world min-x min-y x y)))))))

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
				     (Identificable:id mesh)
				     nil)))
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
    (setf (mesh:normal-map mesh) normal-map)
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
       (let ((mesh (setup-single-ceiling world aabb)))
	 (push-entity world mesh))))

(defun make-cache-key (world filename)
  (resource-cache:regular-file-strings->cache-key (map-cache-dir (main-state world))
						  filename))

(defun make-terrain-chunk-cache-key (world row column)
  (resource-cache:regular-file-strings->cache-key (map-cache-dir (main-state world))
						  (format nil "~a-~a-~a"
							  +terrain-chunk-cache-name+
							  row column)))

(defun cache-miss-p (filename)
  (resource-cache:cache-miss* filename))

(defun %terrain-remove-mesh-data (terrain)
  (setf (mesh:normals       terrain) nil
	(mesh:edges         terrain) nil
	(mesh:tangents      terrain) nil
	(mesh:texture-coord terrain) nil
	(pickable-mesh:pick-overlay-values terrain) (pickable-mesh:init-pick-overlay-value)))

(defun revive-terrain-chunk (whole cache-key)
  (let ((chunk   (copy-flat   whole))
	(revived (deserialize 'terrain-chunk:terrain-chunk cache-key)))
    (setf (pickable-mesh:lookup-tile-triangle chunk)
	  (pickable-mesh:lookup-tile-triangle revived))
    (setf (mesh:triangles chunk) (mesh:triangles revived))
    (setf (mesh:vertices  chunk) (mesh:vertices revived))
    (setf (pick-overlay-values chunk) (pick-overlay-values whole))
    (mesh:reset-aabb chunk)
    chunk))

(defun build-and-cache-terrain-chunk (x z whole cache-key)
  (let ((chunk (terrain-chunk:clip-with-aabb whole
					     (vec4:vec4 x z
							(d+ x +quad-tree-leaf-size+)
							(d+ z +quad-tree-leaf-size+))
					     :clip-if-inside nil)))
    (pickable-mesh:populate-lookup-triangle-matrix chunk)
    (let ((in-cache (copy-flat chunk)))
      (%terrain-remove-mesh-data in-cache)
      (with-open-file (stream cache-key
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(interfaces:serialize-to-stream in-cache stream)))
    chunk))

(defun setup-terrain (world map)
  (let* ((whole          (terrain-chunk:make-terrain-chunk map
							   (compiled-shaders world)
							   :generate-rendering-data nil))
	 (quadtree-depth (quad-tree:quad-sizes->level
			  (aabb2-max-x (game-state:terrain-aabb-2d (main-state world)))
			  +quad-tree-leaf-size+))
	 (channel (lparallel:make-channel))
	 (cache-channels-count  0))
    (loop for aabb in (labyrinths-aabb map) do
	 (terrain-chunk:nclip-with-aabb whole
					(map 'vector
					     #'(lambda (a)
						 (coord-terrain->chunk a :tile-offset 0.0))
					     aabb)
					:regenerate-rendering-data nil
					:clip-if-inside t))
    (pickable-mesh:populate-lookup-triangle-matrix whole)
    (setf (quad-tree:aabb (entities world)) (entity:aabb-2d whole))
    (quad-tree:subdivide  (entities world)  quadtree-depth)
    (loop for x from 0.0 below (aabb2-max-x (entity:aabb-2d whole)) by +quad-tree-leaf-size+ do
    	 (loop for z from 0.0 below (aabb2-max-y (entity:aabb-2d whole))
    	    by +quad-tree-leaf-size+ do
	      (let ((chunk-cache-key (make-terrain-chunk-cache-key world z x)))
		(incf cache-channels-count)
		(if (resource-cache:cache-miss* chunk-cache-key)
		    (lparallel:submit-task channel
					   'build-and-cache-terrain-chunk
					   x
					   z
					   whole
					   chunk-cache-key)
		      (lparallel:submit-task channel
					     'revive-terrain-chunk
					     whole
					     chunk-cache-key)))))
    (loop for i from cache-channels-count above 0 do
	 (let ((chunk (lparallel:receive-result channel)))
	   (prepare-for-rendering chunk)
	   (push-entity world chunk)))))

(defun load-level (world game-state compiled-shaders file)
  (let* ((actual-file (res:get-resource-file file +maps-resource+
					     :if-does-not-exists :error))
	 (resource-cache:*cache-reference-file* actual-file))
    (load actual-file :verbose nil :print nil)
    (resource-cache:ensure-cache-running
      (initialize-skydome world)
      (setf (main-state world)          game-state)
      (setf (compiled-shaders world)    compiled-shaders)
      (setup-game-hour game-state       *game-hour*)
      (setf (map-cache-dir game-state)  *raw-seed*)
      (setf (movement-costs game-state) (graph:matrix->graph (cost-matrix *map*)))
      (prepare-map-state game-state     *map*)
      (setf (trees-bag world)           *trees*)
      (setf (walls-bag world)           *wall*)
      (setf (windows-bag world)         *window*)
      (setf (doors-bag world)           (make-instance 'doors
						       :door-n *door-n*
						       :door-s *door-s*
						       :door-e *door-e*
						       :door-w *door-w*))
      (setf (floor-bag             world) *floor*)
      (setf (furnitures-bag        world) *furnitures*)
      (setf (containers-bag        world) *containers-furnitures*)
      (setf (magic-furnitures-bag  world) *magic-furnitures*)
      (setf (pillars-bag           world) *pillar-furnitures*)
      (setf (chairs-bag            world) *chair-furnitures*)
      (setf (tables-bag            world) *table-furnitures*)
      (setf (wall-decorations-bag  world) *wall-decoration-furnitures*)
      (setf (walkable-bag          world) *walkable-furnitures*)
      (setup-terrain               world  *map*)
      (setup-floor                 world  *map*)
      ;;(setup-ceiling              world  *map*)
      (setup-labyrinths            world  *map*)
      (setup-trees                 world  *map*)
      (setup-water                 world  *map*)
      ;; free memory associated with *map*, *wall* etc.
      (clean-global-wars))))
