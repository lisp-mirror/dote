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

(alexandria:define-constant +terrain-chunk-cache-name+    "terrain-chunk" :test #'string=)

(alexandria:define-constant +min-fountain-spell-recharge+ 2               :test #'=)

(defparameter *available-objects-generators*
  (list (list :weapons
              (list #'(lambda (map-level) (random-weapon:generate-weapon map-level :bow)))
              (list #'(lambda (map-level) (random-weapon:generate-weapon map-level :crossbow)))
              (list #'(lambda (map-level) (random-weapon:generate-weapon map-level :mace)))
              (list #'(lambda (map-level) (random-weapon:generate-weapon map-level :spear)))
              (list #'(lambda (map-level) (random-weapon:generate-weapon map-level :staff)))
              (list #'(lambda (map-level) (random-weapon:generate-weapon map-level :sword))))
        (list :clothes
              (list #'random-armor:generate-armor)
              (list #'random-elm:generate-elm)
              (list #'random-ring:generate-ring)
              (list #'random-shield:generate-shield)
              (list #'random-shoes:generate-shoes))
        (list #'random-potion:generate-potion)
        (list #'random-trap:generate-trap)))

(defun setup-water (world map)
  (loop for water-aabb in (random-terrain:lakes-aabb map) do
       (let ((water (make-instance 'mesh:water :measures water-aabb)))
         (setf (compiled-shaders water) (compiled-shaders world))
         (mesh:prepare-for-rendering water)
         (push-entity world water))))

(defun height-tree->level (tree)
  (let* ((tree-aabb (mesh:aabb tree))
         (h (- (3d-utils:max-y tree-aabb)
               (3d-utils:min-y  tree-aabb))))
    (truncate (alexandria:clamp (1+ (* 0.3 h))
                                random-inert-object:+minimum-level+
                                random-inert-object:+maximum-level+))))

(defun setup-trees (world map)
  (let ((tree-bag-sorted nil))
    (let* ((camera (camera world))
           (c-fov  (camera:frustum-fov  camera))
           (c-near (camera:frustum-near camera))
           (c-far  (camera:frustum-far  camera)))
      (transformable:build-projection-matrix world 5.0 200.0 30.0
                                             (num:desired (/ *window-w* *window-h*)))
      (loop for i from 0 below (length (trees-bag world)) do
           (let ((tree (elt (trees-bag world) i))
                 (camera (camera world)))
             (setf (compiled-shaders tree) (compiled-shaders world))
             (setf (entity:pos tree) +zero-vec+)
             (mesh:reset-aabb  tree)
             (camera:fit-to-aabb camera (mesh:aabb tree))
             (let ((impostor-mesh (billboard:make-impostor-mesh
                                   (mesh:aabb tree)
                                   (billboard:make-impostor-texture world tree))))
               (interfaces:prepare-for-rendering impostor-mesh)
               (setf (compiled-shaders impostor-mesh) (compiled-shaders world))
               (setf (mesh:impostor tree) impostor-mesh))))
               ;; (pixmap:save-pixmap (billboard:make-impostor-pixmap world tree)
               ;;                          (fs:file-in-package
               ;;                           (format nil  "impostor-~a.tga" i))))))
      (transformable:build-projection-matrix world c-near c-far c-fov
                                             (num:desired (/ *window-w* *window-h*))))
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
           (setf (mesh:impostor tree)
                 (mesh:fill-shell-from-mesh (mesh:impostor tree-data)
                                            'billboard:tree-impostor-shell))
           (setf (compiled-shaders tree) (compiled-shaders world))
           (setf (entity:pos tree)
                 (vec (map-utils:coord-terrain->chunk (elt tree-pos 0))
                      +zero-height+
                      (map-utils:coord-terrain->chunk (elt tree-pos 1))))
           (setf (entity:pos (mesh:impostor tree))
                 (entity:pos tree))
           ;; to ensure the tree lies in a leaf node of the quadtree...
           (let ((saved-aabb (slot-value tree 'mesh:aabb)))
             (setf (mesh:aabb tree) (make-instance '3d-utils:aabb
                                              :aabb-p1 (vec 0.0 0.0 0.0)
                                              :aabb-p2 (vec 0.0 10.0 0.0)))
             (push-interactive-entity world tree +tree-type+ :occlude)
             ;; events
             ;; attack
             (game-event:register-for-attack-melee-event      tree)
             (game-event:register-for-attack-long-range-event tree)
             ;; attack-spell
             (game-event:register-for-attack-spell-event tree)
             (setf (mesh:aabb tree) saved-aabb)
             (setf (entity:ghost tree)
                   (random-inert-object:generate-inert-object (height-tree->level tree))))))))

(defun setup-door (world type min-x min-y x y labyrinth-mesh)
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
         (door-shell (mesh:fill-shell-from-mesh-w-renderer-data (funcall door-type-fn
                                                                         (doors-bag world))
                                                                'mesh:door-mesh-shell)))
    (setf (compiled-shaders door-shell) (compiled-shaders world))
    ;; set the mesh
    (setf (entity:pos door-shell)
          (vec (d+ (d* +terrain-chunk-size-scale+ min-x)
                   (map-utils:coord-map->chunk (d (+ x min-x))))
               +zero-height+
               (d+ (d* +terrain-chunk-size-scale+ min-y)
                   (map-utils:coord-map->chunk (d (+ y min-y))))))
    ;; the character
    (setf (entity:ghost door-shell)
          (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
    ;; events
    (game-event:register-for-open-door-event    door-shell)
    (game-event:register-for-close-door-event   door-shell)
    ;; attack
    (game-event:register-for-attack-melee-event      door-shell)
    (game-event:register-for-attack-long-range-event door-shell)
    ;; attack-spell
    (game-event:register-for-attack-spell-event door-shell)
    (push-interactive-entity world door-shell door-type :occlude
                             :add-to-gamestate t
                             :add-to-world     nil)
    (cond
      ((eq door-type +door-n-type+)
       (mtree:add-child (mesh:door-n-instanced labyrinth-mesh) door-shell))
      ((eq door-type +door-s-type+)
       (mtree:add-child (mesh:door-s-instanced labyrinth-mesh) door-shell))
      ((eq door-type +door-e-type+)
       (mtree:add-child (mesh:door-e-instanced labyrinth-mesh) door-shell))
      ((eq door-type +door-w-type+)
       (mtree:add-child (mesh:door-w-instanced labyrinth-mesh) door-shell)))))

(defun calculate-furnitures-shares (level)
  (let* ((normal-share    (d+ 0.66 (d* (d- (d level) 1.0) 0.01)))
         (all-rest        (d- 1.0 normal-share))
         (weight          (dlerp (d/ (d level) +maximum-level-difficult+) 0.0 0.1))
         (magic-share     (d+ (d* (d- 0.33 weight) all-rest)))
         (container-share (d+ (d* (d+ 0.67 weight) all-rest))))
    (list (cons normal-share    +furniture-type+)
          (cons magic-share     +magic-furniture-type+)
          (cons container-share +container-type+))))

(defun furniture-type->shell-type (furniture-type)
  (cond
    ((eq furniture-type +magic-furniture-type+)
     'mesh:fountain-mesh-shell)
    ((eq furniture-type +container-type+)
     'mesh:container-mesh-shell)
    (t
     'mesh:furniture-mesh-shell)))

(defun calc-recharge-spell-fountain-count (world)
  (let ((level (game-state:map-level (main-state world))))
    (truncate (max +min-fountain-spell-recharge+
                  (- +maximum-level-difficult+ (lcg-next-upto level))))))

(defun create-furniture-ghost (world furniture-shell furniture-type keychain)
  (cond
    ((eq furniture-type +furniture-type+)
     (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
    ((eq furniture-type +magic-furniture-type+)
     (random-fountain:generate-fountain (game-state:map-level (main-state world))))
    ((eq furniture-type +container-type+)
     ;; lock unlock event
     (game-event:register-for-lock-object-event   furniture-shell)
     (game-event:register-for-unlock-object-event furniture-shell)
     (random-container:generate-container (game-state:map-level (main-state world))
                                          :keychain keychain))))

(defun tune-magic-furniture (world furniture-shell)
  (setf (mesh:spell-recharge-count furniture-shell)
        (calc-recharge-spell-fountain-count world))
  (game-event:register-for-activate-switch-event furniture-shell))

(defun setup-furnitures (world min-x min-y x y freq keychain)
  (when (all-furnitures-but-pillars-not-empty-p world)
    (let* ((dice-roll      (random-select-by-frequency freq :key #'car :sort nil :normalize nil))
           (furniture-type (cdr dice-roll))
           (choosen        (if (eq furniture-type +furniture-type+)
                               (random-elt (furnitures-bag world))
                               (if (eq furniture-type +magic-furniture-type+)
                                   (random-elt (magic-furnitures-bag world))
                                   (random-elt (containers-bag world)))))
           (shell          (mesh:fill-shell-from-mesh choosen
                                                      (furniture-type->shell-type furniture-type)))
           (mesh-x         (d+ (d* +terrain-chunk-size-scale+ min-x)
                               (map-utils:coord-map->chunk (d (+ x min-x)))))
           (mesh-y         (d+ (d* +terrain-chunk-size-scale+ min-y)
                               (map-utils:coord-map->chunk (d (+ y min-y)))))
           (ghost          (%create-furniture-ghost world shell furniture-type keychain)))
      (setf (compiled-shaders shell)  (compiled-shaders world)
            (entity:pos shell)        (vec mesh-x +zero-height+ mesh-y)
            (entity:dir shell)        (random-elt (vector (vec  1.0 0.0  0.0)
                                                          (vec  0.0 0.0  1.0)
                                                          (vec -1.0 0.0  0.0)
                                                          (vec  0.0 0.0 -1.0)))
            (entity:ghost shell)      ghost)
      (when (eq furniture-type +magic-furniture-type+)
        (setf (mesh:spell-recharge-count shell)
              (calc-recharge-spell-fountain-count world)))
      ;; events
      ;; attack
      (game-event:register-for-attack-melee-event      shell)
      (game-event:register-for-attack-long-range-event shell)
      ;; attack-spell
      (game-event:register-for-attack-spell-event shell)
      ;; if magic-furniture other interactions are possibles
      (when (eq furniture-type +magic-furniture-type+)
        (tune-magic-furniture world shell))
      (push-interactive-entity world shell furniture-type nil))))

(defun %relative-coord-furniture->cood-mat-state (min rel-coord)
  (truncate (+ (map-utils:coord-layer->map-state min) rel-coord)))

(defun common-setup-furniture (world bag min-x min-y x y)
  "Note: will add ghost as well"
  (let* ((choosen  (random-elt bag))
         (shell    (mesh:fill-shell-from-mesh-w-renderer-data choosen 'mesh:furniture-mesh-shell))
         (mesh-x   (d+ (d* +terrain-chunk-size-scale+ min-x)
                       (map-utils:coord-map->chunk (d (+ x min-x)))))
         (mesh-y   (d+ (d* +terrain-chunk-size-scale+ min-y)
                       (map-utils:coord-map->chunk (d (+ y min-y))))))
    (setf (compiled-shaders shell) (compiled-shaders world)
          (entity:pos shell)       (vec mesh-x +zero-height+ mesh-y))
    (setf (entity:ghost shell)
          (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
    (values shell world)))

(defun setup-pillar (world min-x min-y x y labyrinth-mesh)
  (when (pillars-bag world)
    (let ((shell (common-setup-furniture world (pillars-bag world) min-x min-y x y)))
      (push-interactive-entity world shell +pillar-type+ :occlude
                               :add-to-gamestate t
                               :add-to-world     nil)
      ;; event
      ;; attack
      (game-event:register-for-attack-melee-event      shell)
      (game-event:register-for-attack-long-range-event shell)
      ;; attack-spell
      (game-event:register-for-attack-spell-event shell)
      (mtree:add-child (mesh:pillar-instanced labyrinth-mesh) shell))))

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

(defun map-state-delta->orientation (dx dy)
  (cond
    ((and (= dx 0) (= dy -1))
     :n)
    ((and (= dx 1) (= dy 0))
     :w)
    ((and (= dx 0) (= dy 1))
     :s)
    ((and (= dx -1) (= dy 0))
     :e)
    (t (error "orientation invalid"))))

(defun setup-chair (world min-x min-y x y labyrinth-mesh)
  (when (chairs-bag world)
    (let* ((x-world     (%relative-coord-furniture->cood-mat-state min-x x))
           (y-world     (%relative-coord-furniture->cood-mat-state min-y y))
           (mesh-x      (d+ (d* +terrain-chunk-size-scale+ min-x)
                            (map-utils:coord-map->chunk (d (+ x min-x)))))
           (mesh-y      (d+ (d* +terrain-chunk-size-scale+ min-y)
                            (map-utils:coord-map->chunk (d (+ y min-y)))))
           (table-near  (find-next-by-type world x-world y-world +table-type+))
           (orientation (if table-near ; choose orientation
                            (let* ((dx (truncate (- (elt table-near 0) x-world)))
                                   (dy (truncate (- (elt table-near 1) y-world))))
                              (map-state-delta->orientation dx dy))
                            nil))
           (shell      (mesh:fill-shell-from-mesh-w-renderer-data
                        (if orientation ; choose orientation
                            (ecase orientation
                              (:n (world:chair-n (world:chairs-bag world)))
                              (:s (world:chair-s (world:chairs-bag world)))
                              (:e (world:chair-e (world:chairs-bag world)))
                              (:w (world:chair-w (world:chairs-bag world))))
                            (world:chair-n (world:chairs-bag world))))))
      (setf (compiled-shaders shell) (compiled-shaders world)
            (entity:pos shell)     (vec mesh-x +zero-height+ mesh-y))
      (setf (entity:ghost shell)
            (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
      (push-interactive-entity world shell +chair-type+ nil
                               :add-to-gamestate t
                               :add-to-world     nil)
      ;; event
      ;; attack
      (game-event:register-for-attack-melee-event      shell)
      (game-event:register-for-attack-long-range-event shell)
      ;; attack-spell
      (game-event:register-for-attack-spell-event shell)
      (case orientation
        (:n (mtree:add-child (mesh:chair-n-instanced labyrinth-mesh) shell))
        (:s (mtree:add-child (mesh:chair-s-instanced labyrinth-mesh) shell))
        (:e (mtree:add-child (mesh:chair-e-instanced labyrinth-mesh) shell))
        (:w (mtree:add-child (mesh:chair-w-instanced labyrinth-mesh) shell))
        (otherwise
         (mtree:add-child (mesh:chair-n-instanced labyrinth-mesh) shell))))))

(defun setup-table (world min-x min-y x y labyrinth-mesh)
  (when (tables-bag world)
    (let ((shell (common-setup-furniture world (tables-bag world) min-x min-y x y)))
      (push-interactive-entity world shell +table-type+ nil
                               :add-to-gamestate t
                               :add-to-world     nil)
      ;; event
      ;; attack
      (game-event:register-for-attack-melee-event      shell)
      (game-event:register-for-attack-long-range-event shell)
      ;; attack-spell
      (game-event:register-for-attack-spell-event shell)
      (mtree:add-child (mesh:table-instanced labyrinth-mesh) shell))))

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

(defun %wall-position (min-x min-y x y)
  "note we have to take into account  the fact that whe have two tiles
in the map for each element of the heightmap of the terrain. So we use
'coord-terrain->chunk' for the corner of the labyrinth but not for the
wall's    coordinates    as    they   are    already    scaled:    see
'random-terrain:make-labyrinths'"
  (vec (d+ (map-utils:coord-terrain->chunk min-x :tile-offset 0.0)
           (map-utils:coord-map->chunk x))
       +zero-height+
       (d+ (map-utils:coord-terrain->chunk min-y :tile-offset 0.0)
           (map-utils:coord-map->chunk y))))

(defun setup-wall (world bmp min-x min-y x y labyrinth-mesh &key (chance 5))
  (let* ((dice-roll       (lcg-next-upto chance))
         (mesh            (walls-bag world))
         (decal-side-free (and (< (1+ y) (width bmp))
                               (not (random-labyrinth:invalicablep
                                     (matrix-elt bmp (1+ y) x)))))
         (type            (if (and (null (mesh:normal-map mesh))
                                   (= (mod dice-roll chance) 0)
                                   decal-side-free)
                              'mesh:decorated-wall-mesh-shell
                              'mesh:wall-mesh-shell))
         (shell           (if (eq type 'mesh:decorated-wall-mesh-shell)
                              (mesh:fill-shell-from-mesh mesh type)
                              (mesh:fill-shell-from-mesh-w-renderer-data mesh type))))
    (setf (compiled-shaders shell) (compiled-shaders world))
    (setf (entity:pos shell) (%wall-position min-x min-y x y))
    (setf (entity:ghost shell)
          (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
    ;; events
    (game-event:register-for-end-turn shell)
    ;; attack
    (game-event:register-for-attack-melee-event      shell)
    (game-event:register-for-attack-long-range-event shell)
    ;; attack-spell
    (game-event:register-for-attack-spell-event shell)
    (if (typep shell 'mesh:decorated-wall-mesh-shell)
        (progn
          (setf (mesh:texture-projector shell)
                (random-elt (texture:list-of-texture-by-tag
                             texture:+texture-tag-int-decal+)))
          (setf (texture:s-wrap-mode     (mesh:texture-projector shell)) :clamp-to-border)
          (setf (texture:t-wrap-mode     (mesh:texture-projector shell)) :clamp-to-border)
          (setf (texture:border-color    (mesh:texture-projector shell)) §c00000000)
          (texture:prepare-for-rendering (mesh:texture-projector shell))
          (mesh:setup-projective-texture shell)
          (push-interactive-entity world shell +wall-type+ :occlude
                             :add-to-gamestate t
                             :add-to-world     t))
        (progn
          (push-interactive-entity world shell +wall-type+ :occlude
                                   :add-to-gamestate t
                                   :add-to-world     nil)
          (mtree:add-child (mesh:wall-instanced labyrinth-mesh) shell)))
    shell))

(defun setup-window (world min-x min-y x y labyrinth-mesh)
  (let* ((mesh            (windows-bag world))
         (shell           (mesh:fill-shell-from-mesh-w-renderer-data mesh
                                                                     'mesh:window-mesh-shell)))
    (setf (compiled-shaders shell) (compiled-shaders world))
    (setf (entity:pos shell) (%wall-position min-x min-y x y))
    (setf (entity:ghost shell)
          (random-inert-object:generate-inert-object (game-state:map-level (main-state world))))
    (game-event:register-for-end-turn shell)
    (push-interactive-entity world shell +wall-type+ nil
                             :add-to-gamestate t
                             :add-to-world     nil)
    ;; attack
    (game-event:register-for-attack-melee-event      shell)
    (game-event:register-for-attack-long-range-event shell)
    ;; attack spell
    (game-event:register-for-attack-spell-event shell)
    (mtree:add-child (mesh:window-instanced labyrinth-mesh) shell)
    shell))

(defun find-container-fn (world)
  #'(lambda (a)
      (when (not (game-state:map-element-empty-p a))
        (let ((mesh (game-state:find-entity-by-id (main-state world)
                                                  (game-state:entity-id a)))
              (type (game-state:el-type a)))
          (assert (not (null mesh)))
          (and (interactive-entity:containerp (entity:ghost mesh))
               (typep mesh 'mesh:container-mesh-shell)
               (eq type +container-type+))))))

(defun shifto (l out)
  (fresh (a d new-tail new-list y)
    (conso   a d l)
    (conso   a nil new-tail)
    (appendo d new-tail new-list)
    (== new-list  out)))

(defun not-compatible-o (keys containers)
  (conde
    ((nullo keys)
     +fail+)
    ((fresh (a-k a-c)
       (caro keys   a-k)
       (caro containers a-c)
       (project (a-k a-c)
         (== (string= (interactive-entity:object-keycode a-k)
                      (interactive-entity:object-keycode (entity:ghost a-c))) t))
       +succeed+))
    (else
     (fresh (d-k d-c)
       (cdro keys   d-k)
       (cdro containers d-c)
       (not-compatible-o d-k d-c)))))

(defun compatible-o (keys containers)
  (condu
    ((not-compatible-o keys containers)
     +fail+)
    (else
     +succeed+)))

(defun compatible-first-key-keycode (keys keycode)
  (and keys
       (not (string= (interactive-entity:object-keycode (first keys)) keycode))))

(defun compatible-arrangement-keys (keys containers start out)
  (conde
    ((compatible-o keys containers)
     (== out keys)
     +succeed+)
    (else
     (fresh (x)
       (shifto keys x)
       (project (keys)
         (== (compatible-first-key-keycode keys start) t))
       (compatible-arrangement-keys x containers start out)))))

(defun container-distance (a b)
  (let* ((pos-a (vec2:vec2 (elt (entity:pos a) 0)  (elt (entity:pos a) 2)))
         (pos-b (vec2:vec2 (elt (entity:pos b) 0)  (elt (entity:pos b) 2)))
         (pos-diff (vec2:vec2- pos-a pos-b)))
    (vec2:vec2-length pos-diff)))

(defun average-container-distance (keys containers)
  (let ((distance-sum (loop for container in containers sum
                           (let ((pos (position-if #'(lambda (a)
                                                       (string= (interactive-entity:object-keycode
                                                                 (entity:ghost container))
                                                                (interactive-entity:object-keycode a)))
                                                   keys)))
                             (container-distance container
                                             (if pos
                                                 (elt containers pos)
                                                 container))))))
    (/ distance-sum (length containers))))

(defun incompatible-key (keys container out)
  (conde
    ((nullo keys) ;; useless ?
      +fail+)
    ((project (keys container)
       (== (not keys) nil)
       (== (compatible-first-key-keycode keys
                                         (interactive-entity:object-keycode (entity:ghost container)))
           t)
       (== out (interactive-entity:object-keycode (first keys)))))
    (else
     (project (keys)
       (== (not keys) nil))
     (incompatible-key (rest keys) container out))))

(defun arrange-compatible-keys (container-start containers keys dist)
  (run 1 (q)
    (fresh (x y start)
      (conde
        ((fresh (l)
           (== keys `(,l)) ; list has length 1
           (conde
             ((project (l keys)
                ;; ensure container's keycode is not compatible with key
                (== (string= (interactive-entity:object-keycode l)
                             (interactive-entity:object-keycode (entity:ghost (first containers)))) t))
              (fresh (first cdr second rest)
                (conso first cdr  containers) ; we need to get the first,
                                              ; the second and all others elements
                (conso second rest cdr)       ; of the containers
                (== y `(,second ,first . ,rest)) ; swap first and second element
                  (== x keys)))
               (else                        ; key    is   already
                                            ; compatible with container on
                                            ; the  same   position  in
                                            ; container's list, success.
                (== y containers)
                (== x keys)))))
          (else
           (incompatible-key keys container-start start)
           (== y containers)
           (project (start)
             (compatible-arrangement-keys keys containers (coerce start 'string) x))
           (project (x)
             (progn
               #+debug-mode (misc:dbg "avg dist: ~f~%"  (average-container-distance x containers))
               (== (> (average-container-distance x containers) dist) t)))))
      (== q `(,x ,y)))))

(defun all-containers-from-map (world)
  "A list of all containers from element-matrix in world's game-state"
  (map 'list #'(lambda (a)
                 (game-state:find-entity-by-id (main-state world)
                                               (game-state:entity-id a)))
       (remove-if-not (find-container-fn world)
                      (matrix:data (game-state:map-state
                                    (main-state world))))))

(defun dump-containers (world)
  (let ((containers (all-containers-from-map world)))
    (misc:dbg "all containers ~a" (length containers))
    (loop for container in containers do
         (let ((children (mtree:children (entity:ghost container))))
           (misc:dbg "container ~a keycode ~a: ~{~a~%~}"
                     (identificable:id container)
                     (interactive-entity:object-keycode (entity:ghost container))
                     (loop for child across children collect
                          (description-for-humans child)))))))

(defun rearrange-keys-for-containers (world keychain)
  (let ((containers (all-containers-from-map world)))
    (when (not (vector-empty-p keychain)) ;; if keychain is not empty
                                          ;; containers  is  not  null.
                                          ;; So no need to check the latter.
      (let ((arranged (loop
                         named inner
                         for i from (game-state:map-level (main-state world)) downto 1 do
                           (let ((results (arrange-compatible-keys (first containers)
                                                                   containers
                                                                   (coerce keychain 'list)
                                                                   0)))
                             (when (not (null results))
                               (return-from inner (first results)))))))
        (destructuring-bind (arranged-keys arranged-containers)
            arranged
          (let ((container-cancel-keycode
                 (find-if #'(lambda (a)
                              (not (null (interactive-entity:object-keycode (entity:ghost a)))))
                          containers)))
            (when (not (null container-cancel-keycode))
              (n-setf-path-value (character:basic-interaction-params
                                  (entity:ghost container-cancel-keycode))
                                 (list basic-interaction-parameters:+can-be-opened+)
                                 t)) ;; can be opened without a key
            (loop for i from 0 below (length arranged-keys) do
                 (mtree:add-child (entity:ghost (elt arranged-containers i))
                                  (elt arranged-keys i))))
          (misc:dbg "arranged keys ~a ~% containers ~a"
                    (map 'list #'(lambda (a)
                                   (format nil "[~a ~a]"
                                           (identificable:id a)
                                           (interactive-entity:object-keycode a)))
                         arranged-keys)
                    (map 'list #'(lambda (a)
                                   (format nil "[~a ~a]"
                                           (identificable:id a)
                                           (interactive-entity:object-keycode (entity:ghost a))))
                         arranged-containers)))))))

(defun fill-containers-with-objects (world)
  (let ((containers (all-containers-from-map world))
        (map-level  (game-state:map-level (main-state world))))
    (when (not (null containers))
      (loop for container in containers do
           (let* ((available-slots (- +container-capacity+
                                      (length (mtree:children (entity:ghost container)))))
                  (objects-count   (lcg-next-upto available-slots)))
             (dotimes (i objects-count)
               (let* ((generator (mtree:random-choose-leaf *available-objects-generators*))
                      (obj       (funcall generator map-level)))
                 (game-event:register-for-end-turn obj)
                 (mtree:add-child (entity:ghost container) obj))))))))

(defun setup-labyrinths (world map)
  (let ((keychain (make-fresh-array 0 nil t nil)))
    (loop
       for aabb in (labyrinths-aabb map)
       for bmp  in (mapcar #'(lambda (a)
                               (clean-and-redraw-mat a)
                               (shared-matrix        a))
                                   (labyrinths map))
       do
         (let* ((min-x   (aabb2-min-x aabb))
                (min-y   (aabb2-min-y aabb))
                (labyrinth-mesh (make-instance 'mesh:labyrinth-mesh)))
           (loop-matrix (bmp x y)
              (cond
                ((wallp (matrix-elt bmp y x))
                 (setup-wall world bmp min-x min-y x y labyrinth-mesh))
                ((windowp (matrix-elt bmp y x))
                 (setup-window world min-x min-y x y labyrinth-mesh))
                ((door-n-p (matrix-elt bmp y x))
                 (setup-door world :door-s min-x min-y x y labyrinth-mesh))
                ((door-s-p (matrix-elt bmp y x))
                 (setup-door world :door-n min-x min-y x y labyrinth-mesh))
                ((door-e-p (matrix-elt bmp y x))
                 (setup-door world :door-e min-x min-y x y labyrinth-mesh))
                ((door-w-p (matrix-elt bmp y x))
                 (setup-door world :door-w min-x min-y x y labyrinth-mesh))
                ((furniture-pillar-p  (matrix-elt bmp y x))
                 (setup-pillar world min-x min-y x y labyrinth-mesh))
                ((furniture-walkable-p  (matrix-elt bmp y x))
                 (setup-walkable world min-x min-y x y))
                ((furniture-table-p  (matrix-elt bmp y x))
                 (setup-table world min-x min-y x y labyrinth-mesh))
                ((furniture-other-p (matrix-elt bmp y x))
                 (setup-furnitures world min-x min-y x y
                                   (sort (calculate-furnitures-shares (game-state:level-difficult
                                                                       (main-state world)))
                                         #'<
                                         :key #'car)
                                   keychain))))
           ;; we  can setup  wall decorations  and chair  only after  we
           ;; arranged  the  other furnitures  as  the  first two  do  a
           ;; look-up on game state matrix.
           (loop-matrix (bmp x y)
              (cond
                ((furniture-chair-p  (matrix-elt bmp y x))
                 (setup-chair world min-x min-y x y labyrinth-mesh))
                ((furniture-wall-decoration-p  (matrix-elt bmp y x))
                 (setup-wall-decoration world min-x min-y x y))))
           (setf (compiled-shaders labyrinth-mesh) (compiled-shaders world))
           (prepare-for-rendering labyrinth-mesh)
           (push-labyrinth-entity world labyrinth-mesh)))
    (rearrange-keys-for-containers world keychain)
    (fill-containers-with-objects world)
    #+debug-mode (dump-containers world)))

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
                              (map-utils:coord-map->chunk min-x :tile-offset 0.0)
                              (map-utils:coord-map->chunk (d/ w 2.0) :tile-offset 0.0)))
         (actual-z        (d+ (d* +terrain-chunk-size-scale+ min-y)
                              (map-utils:coord-map->chunk min-y :tile-offset 0.0)
                              (map-utils:coord-map->chunk (d/ h 2.0) :tile-offset 0.0)))
         (transformation  (sb-cga:matrix*
                           (sb-cga:translate (vec actual-x +zero-height+ actual-z))
                           (sb-cga:translate (3d-utils:vec-negate (sb-cga:vec w/2 0.0 h/2)))))
         (mesh            (building-floor-mesh:floor-tile w h
                                                          :wrapper-transformation transformation)))
    (setf (compiled-shaders mesh) (compiled-shaders world))
    (setf (mesh:texture-object  mesh) (mesh:texture-object         ref-mesh)
          (mesh:normal-map      mesh) (mesh:normal-map             ref-mesh)
          (mesh:material-params mesh) (clone (mesh:material-params ref-mesh)))
    (prepare-for-rendering mesh)
    (mesh:reset-aabb mesh)
    (setf (origin-offset mesh) (vec (3d-utils:min-x (mesh:aabb mesh))
                                    +zero-height+
                                    (3d-utils:min-z (mesh:aabb mesh))))
    (building-floor-mesh:setup-texture-coord-scaling mesh)
    ;;for each tile
    (loop for x from 0.0 below (d/ w +terrain-chunk-size-scale+) do
         (loop for y from 0.0 below (d/ h +terrain-chunk-size-scale+) do
              (world:setup-map-state-tile world
                                          (truncate (+ x (map-utils:coord-layer->map-state min-x)))
                                          (truncate (+ y (map-utils:coord-layer->map-state min-y)))
                                          +floor-type+
                                          (identificable:id mesh)
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
                   (map-utils:coord-map->chunk min-x :tile-offset 0.0)
                   (map-utils:coord-map->chunk (d/ w 2.0) :tile-offset 0.0))
               +zero-height+
               (d+ (d* +terrain-chunk-size-scale+ min-y)
                   (map-utils:coord-map->chunk min-y :tile-offset 0.0)
                   (map-utils:coord-map->chunk (d/ h 2.0) :tile-offset 0.0))))
    mesh))

(defun setup-floor (world map)
  (loop for aabb in (labyrinths-aabb map) do
       (let ((mesh  (setup-single-floor world aabb))
             (state (main-state world)))
         (pickable-mesh:populate-lookup-triangle-matrix mesh)
         (push-entity world mesh)
         (when (not (entity:find-entity-by-id state (identificable:id mesh)))
           (push-entity state mesh)))))

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
    (setf (origin-offset chunk) (origin-offset revived))
    (setf (pickable-mesh:lookup-tile-triangle chunk)
          (pickable-mesh:lookup-tile-triangle revived))
    (setf (mesh:triangles chunk) (mesh:triangles revived))
    (setf (mesh:vertices  chunk) (mesh:vertices revived))
    (setf (pick-overlay-values chunk) (pick-overlay-values whole))
    (mesh:reset-aabb chunk)
    chunk))

(defun chunk-best-aabb (whole x z)
  (let ((hmap (heightmap whole)))
    (vec4:vec4 x z
               (dmin (d+ x +quad-tree-leaf-size+)
                     (map-utils:coord-terrain->chunk (width  hmap) :tile-offset 0.0))
               (dmin (d+ z +quad-tree-leaf-size+)
                     (map-utils:coord-terrain->chunk (height hmap) :tile-offset 0.0)))))

(defun build-and-cache-terrain-chunk (x z whole cache-key)
  (let* ((chunk (terrain-chunk:clip-with-aabb whole
                                              (chunk-best-aabb whole x z)
                                             :remove-orphaned-vertices t
                                             :regenerate-rendering-data t
                                             :clip-if-inside nil))
         (min-y (3d-utils:min-y (mesh:aabb chunk))))
    (setf (pickable-mesh:origin-offset chunk) (vec x min-y z))
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
  (let* ((border-color   (skydome-bottom-color (main-state world)))
         (whole          (terrain-chunk:make-terrain-chunk map
                                                           (compiled-shaders world)
                                                           :map-border-color        border-color
                                                           :generate-rendering-data nil))
         (quadtree-depth (quad-tree:quad-sizes->level
                          (aabb2-max-x (game-state:terrain-aabb-2d (main-state world)))
                          +quad-tree-leaf-size+))
         (channel (lparallel:make-channel))
         (cache-channels-count  0))
    (loop for aabb in (labyrinths-aabb map) do
         (let* ((offset (d/ +terrain-chunk-tile-size+ 2.0))
                (act-aabb (2d-utils:make-aabb2
                           (map-utils:coord-terrain->chunk (2d-utils:aabb2-min-x aabb)
                                                           :tile-offset (d- offset))
                           (map-utils:coord-terrain->chunk (2d-utils:aabb2-min-y aabb)
                                                           :tile-offset  (d- offset))
                           (map-utils:coord-terrain->chunk (2d-utils:aabb2-max-x aabb)
                                                           :tile-offset offset)
                           (map-utils:coord-terrain->chunk (2d-utils:aabb2-max-y aabb)
                                                           :tile-offset offset))))
           (terrain-chunk:nclip-with-aabb whole
                                          act-aabb
                                          :remove-orphaned-vertices  nil
                                          :regenerate-rendering-data nil
                                          :clip-if-inside              t)))
    (pickable-mesh:populate-lookup-triangle-matrix whole)
    (setf (quad-tree:aabb (entities world)) (entity:aabb-2d whole))
    (quad-tree:subdivide  (entities world)  quadtree-depth)
    ;; I guess this loop works  only because labyrinths can not extend
    ;; beyond the borders of the whole map...
    (loop for x from 0.0 below (aabb2-max-x (entity:aabb-2d whole)) by +quad-tree-leaf-size+ do
         (loop
            for z from 0.0 below (aabb2-max-y (entity:aabb-2d whole)) by +quad-tree-leaf-size+ do
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
         (let ((chunk  (lparallel:receive-result channel)))
           (prepare-for-rendering chunk)
           (push-entity world chunk)))))

(defun load-level (window world game-state compiled-shaders file)
  (let* ((actual-file (res:get-resource-file file +maps-resource+
                                             :if-does-not-exists :error))
         (resource-cache:*cache-reference-file* actual-file))
    (setf *main-window* window)
    (setf *renderer*    world)
    (update-progress 0.0)
    (load actual-file :verbose nil :print nil)
    (update-progress 0.4)
    (resource-cache:ensure-cache-running
      (initialize-skydome world)
      (update-progress 0.45)
      (setf (main-state world)          game-state)
      (setf (compiled-shaders world)    compiled-shaders)
      (setup-game-hour game-state       *game-hour*)
      (setf (map-cache-dir game-state)  *raw-seed*)
      (setf (costs-from-map     game-state)  (cost-matrix *map*))
      (setf (costs-from-players game-state)  (matrix:make-matrix (matrix:width  (cost-matrix *map*))
                                                                 (matrix:height (cost-matrix *map*))
                                                                 +minimum-player-layer-cost+))
      (setf (movement-costs game-state)
            (graph:make-tile-multilayer-graph (costs-from-map game-state)
                                              (costs-from-players game-state)))
      (prepare-map-state game-state     *map*)
      (update-progress 0.5)
      (setf (trees-bag world)           *trees*)
      (setf (walls-bag world)           *wall*)
      (setf (windows-bag world)         *window*)
      (setf (doors-bag world)           (make-instance 'doors
                                                       :door-n *door-n*
                                                       :door-s *door-s*
                                                       :door-e *door-e*
                                                       :door-w *door-w*))
      (setf (chairs-bag world)          (make-instance 'chairs
                                                       :chair-n *chair-n*
                                                       :chair-s *chair-s*
                                                       :chair-e *chair-e*
                                                       :chair-w *chair-w*))
      (setf (floor-bag             world) *floor*)
      (setf (furnitures-bag        world) *furnitures*)
      (setf (containers-bag        world) *containers-furnitures*)
      (setf (magic-furnitures-bag  world) *magic-furnitures*)
      (setf (pillars-bag           world) *pillar-furnitures*)
      (setf (tables-bag            world) *table-furnitures*)
      (setf (wall-decorations-bag  world) *wall-decoration-furnitures*)
      (setf (walkable-bag          world) *walkable-furnitures*)
      (setf (traps-bag        world) *trap*)
      (setup-terrain               world  *map*)
      (update-progress 0.6)
      (setup-floor                 world  *map*)
      (update-progress 0.7)
      ;;(setup-ceiling              world  *map*)
      (setup-labyrinths            world  *map*)
      (update-progress 0.8)
      (setup-trees                 world  *map*)
      (update-progress 0.9)
      (setup-water                 world  *map*)
      (update-progress 1.0)
      ;; free memory associated with *map*, *wall* etc.
      (clean-global-vars)
      ;; initialize blackboard
      (let ((blackboard (make-instance 'blackboard:blackboard
                                       :main-state game-state)))
        (game-event:register-for-end-turn blackboard)
        (setf (game-state:blackboard game-state) blackboard)))))
