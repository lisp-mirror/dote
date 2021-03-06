;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

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

(in-package :saved-game)

(define-constant +map-saved-filename+ "map" :test #'string=)

(defparameter *map-loaded-p* nil)

(defparameter *xpos*     2.0)

(defparameter *ypos*    40.0)

(defparameter *zpos*   -20.0)

(defparameter *xeye*     2.0)

(defparameter *yeye*     0.0)

(defparameter *zeye*     8.0)

(defparameter *xup*      0.0)

(defparameter *yup*      1.0)

(defparameter *zup*      0.0)

(defparameter *far*    440.0)

(defparameter *near*     5.0)

(defparameter *fov*     50.0)

(defclass saved-entity ()
  ((player-ghost
    :initform nil
    :initarg  :player-ghost
    :accessor player-ghost
    :type     player-character)
   (original-map-pos
    :initform nil
    :initarg  :original-map-pos
    :accessor original-map-pos
    :type     ivec2:ivec2)
   (original-dir
    :initform nil
    :initarg  :original-dir
    :accessor original-dir
    :type     ivec2:ivec2)))

(defmethod marshal:class-persistant-slots ((object saved-entity))
  '(player-ghost
    original-dir
    original-map-pos))

(defclass saved-entity-w-faction (saved-entity)
  ((original-faction
    :initform nil
    :initarg  :original-faction
    :accessor original-faction
    :type     symbol)))

(defmethod marshal:class-persistant-slots ((object saved-entity-w-faction))
  (append '(original-faction)
          (call-next-method)))

(defclass saved-player (saved-entity-w-faction)
  ((sprite-infos
    :initform nil
    :initarg  :sprite-infos
    :accessor sprite-infos
    :type     sprite-fs-res)
   (original-sprite-id
    :initform nil
    :initarg  :original-sprite-id
    :accessor original-sprite-id
    :type     fixnum)))

(defmethod marshal:class-persistant-slots ((object saved-player))
  (append '(sprite-infos
            original-sprite-id
            original-faction)
          (call-next-method)))

(defclass saved-container (saved-entity) ())

(defmethod marshal:class-persistant-slots ((object saved-container))
  (append '()
          (call-next-method)))

(defclass saved-door (saved-entity)
  ((opened
    :initform nil
    :initarg  :openedp
    :reader   openedp
    :writer   (setf opened))))

(defmethod marshal:class-persistant-slots ((object saved-door))
  (append '(opened)
          (call-next-method)))

(defclass saved-magic-furniture (saved-entity)
  ((original-spell-recharge-count
    :initform -1
    :initarg  :original-spell-recharge-count
    :accessor original-spell-recharge-count
    :type     fixnum)))

(defmethod marshal:class-persistant-slots ((object saved-magic-furniture))
  (append '(original-spell-recharge-count)
          (call-next-method)))

(defclass saved-game ()
  ((original-map-file
    :initform nil
    :initarg  :original-map-file
    :accessor original-map-file
    :type     string)
   (dmg-points
    :initform nil
    :initarg  :dmg-points
    :accessor dmg-points
    :type     matrix
    :documentation  "All  the  dmg  points per  position  of invalicables entities.")
   (delta-tiles
    :initform nil
    :initarg  :delta-tiles
    :accessor delta-tiles
    :type     matrix
    :documentation     "A     snapshot     of    the     matrix     in
    game-state:map-state-element when the game was saved.")
   (saved-difficult-level
    :initform nil
    :initarg  :saved-difficult-level
    :accessor saved-diffcult-level)
   (saved-players
    :initform nil
    :initarg  :saved-players
    :accessor saved-players
    :type     list)
   (saved-traps
    :initform nil
    :initarg  :saved-traps
    :accessor saved-traps
    :type     list)
   (saved-doors
    :initform nil
    :initarg  :saved-doors
    :accessor saved-doors
    :type     list)
   (saved-containers
    :initform nil
    :initarg  :saved-containers
    :accessor saved-containers
    :type     list)
   (saved-magic-furnitures
    :initform nil
    :initarg  :saved-magic-furnitures
    :accessor saved-magic-furnitures
    :type     list)
   ;;;; from blackboard
   (saved-exhausted-fountains-ids
    :initform '()
    :initarg  :saved-exhausted-fountains-ids
    :accessor saved-exhausted-fountains-ids)
   (saved-visited-tiles
    :initform nil
    :initarg  :saved-visited-tiles
    :accessor saved-visited-tiles
    :type matrix
    :documentation "matrix element @ (x, y) is non nil if the tile has been visited,
                   false otherwise")
   (saved-unexplored-layer
    :initform nil
    :initarg  :saved-unexplored-layer
    :accessor saved-unexplored-layer
    :type     dijkstra-layer)
   (saved-concerning-tiles
    :initform nil
    :initarg  :saved-concerning-tiles
    :accessor saved-concerning-tiles
    :type     matrix)
   (saved-concerning-tiles-invalicables
    :initform nil
    :initarg  :saved-concerning-tiles-invalicables
    :accessor saved-concerning-tiles-invalicables
    :type     matrix
    :documentation "the concerning tiles near a NPC (for unexplored-layer only)")
   (saved-concerning-tiles-facing
    :initform nil
    :initarg  :saved-concerning-tiles-facing
    :accessor saved-concerning-tiles-facing
    :type     matrix
    :documentation "the concerning tiles in front o a NPC")
   (saved-attack-enemy-melee-positions
    :initform '()
    :initarg  :saved-attack-enemy-melee-positions
    :accessor saved-attack-enemy-melee-positions
    :documentation   "This  holds   the   positions   AI  with   melee
    weapon (except pole  weapon) should reach to attack  the enemy (it
    is a list of def-target instances)")
   (saved-attack-enemy-pole-positions
    :initform '()
    :initarg  :saved-attack-enemy-pole-positions
    :accessor saved-attack-enemy-pole-positions
    :documentation  "This  holds the  positions  AI  with pole  weapon
    should  reach to  attack the  enemy (it  is a  list of  def-target
    instances)")
   (saved-attack-enemy-bow-positions
    :initform '()
    :initarg  :saved-attack-enemy-bow-positions
    :accessor saved-attack-enemy-bow-positions
    :documentation "This holds the positions AI with bow weapon should
    reach to attack the enemy (it is a list of def-target instances)")
   (saved-attack-enemy-crossbow-positions
    :initform '()
    :initarg  :saved-attack-enemy-crossbow-positions
    :accessor saved-attack-enemy-crossbow-positions
    :documentation "This  holds the positions AI  with crossbow weapon
    should  reach to  attack the  enemy (it  is a  list of  def-target
    instances)")
   (saved-fow-texture
    :initform nil
    :initarg  :saved-fow-texture
    :accessor saved-fow-texture
    :type     texture)))

(defmethod marshal:class-persistant-slots ((object saved-game))
  '(original-map-file
    dmg-points
    delta-tiles
    saved-difficult-level
    saved-players
    saved-traps
    saved-doors
    saved-containers
    saved-magic-furnitures
    saved-exhausted-fountains-ids
    saved-visited-tiles
    saved-unexplored-layer
    saved-concerning-tiles
    saved-concerning-tiles-invalicables
    saved-concerning-tiles-facing
    saved-attack-enemy-melee-positions
    saved-attack-enemy-pole-positions
    saved-attack-enemy-bow-positions
    saved-attack-enemy-crossbow-positions
    saved-fow-texture))

(defmethod el-type-in-pos ((object saved-game) (x fixnum) (y fixnum))
  (el-type (matrix-elt (delta-tiles object) y x)))

(defmethod entity-id-in-pos ((object saved-game) (x fixnum) (y fixnum))
  (entity-id (matrix-elt (delta-tiles object) y x)))

(defmacro def-*@pos-p-saved-game (&rest names)
  `(progn
     ,@(loop for n in names collect
            `(defmethod ,n ((object saved-game) (x fixnum) (y fixnum))
               (,n (delta-tiles object) x y)))))

(def-*@pos-p-saved-game
    empty@pos-p
    trap@pos-p
  container@pos-p
  door@pos-p
  magic-furniture@pos-p)

(defmethod pawn@pos-p ((object saved-game) x y)
  (or (entity-ai-in-pos     object x y)
      (entity-player-in-pos object x y)))

(defmethod entity-ai@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (let ((entity-type (el-type-in-pos object x y)))
    (eq entity-type +npc-type+)))

(defmethod entity-player@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (let ((entity-type (el-type-in-pos object x y)))
    (eq entity-type +pc-type+)))

(defun sprite->saved-entity (sprite)
  (make-instance 'saved-player
                 :sprite-infos       (sprite:fs-resources     sprite)
                 :player-ghost       (ghost                   sprite)
                 :original-faction   (my-faction              sprite)
                 :original-dir       (dir                     sprite)
                 :original-map-pos   (calculate-cost-position sprite)
                 :original-sprite-id (id                      sprite)))

(defun trap->saved-entity (mesh)
  (make-instance 'saved-entity-w-faction
                 :original-faction (my-faction              mesh)
                 :original-map-pos (calculate-cost-position mesh)
                 :player-ghost     (ghost mesh)))

(defun container->saved-entity (mesh)
  (make-instance 'saved-container
                 :original-map-pos (calculate-cost-position mesh)
                 :player-ghost     (ghost                   mesh)))

(defun door->saved-door (mesh)
  (make-instance 'saved-door
                 :original-map-pos (calculate-cost-position mesh)
                 :openedp          (openp                   mesh)
                 :player-ghost     (ghost                   mesh)))

(defun magic-furniture->saved-magic-furniture (mesh)
  (make-instance 'saved-magic-furniture
                 :original-map-pos              (calculate-cost-position mesh)
                 :original-spell-recharge-count (spell-recharge-count    mesh)
                 :player-ghost                  (ghost                   mesh)))

(defun build-dmg-matrix (game-state)
  (with-accessors ((map-state map-state)) game-state
    (let ((res (make-matrix (width map-state) (height map-state) nil)))
      (loop-matrix (map-state x y)
         (let* ((element   (matrix:matrix-elt map-state y x))
                (entity-id (entity-id element)))
           (when (map-element-occupied-by-invalicable-p element :include-door t)
             (let ((entity (find-entity-by-id game-state entity-id)))
               (setf (matrix-elt res y x)
                     (current-damage-points (ghost entity)))))))
      res)))

(defun save-game (resource-dir game-state)
  (with-accessors ((blackboard blackboard)) game-state
    (let* ((saved-file                    (res:get-resource-file +map-saved-filename+
                                                                 resource-dir
                                                                 :if-does-not-exists :create))
           (saved-difficult               (level-difficult              game-state))
           (current-map-state             (map-state                    game-state))
           (saved-players                 (append (map-ai-entities      game-state
                                                                        #'sprite->saved-entity
                                                                        :match-dead-characters t)
                                                  (map-player-entities  game-state
                                                                        #'sprite->saved-entity
                                                                        :match-dead-characters t)))
           (saved-traps                   (mapcar #'trap->saved-entity
                                                  (fetch-all-traps      game-state)))
           (saved-containers              (mapcar #'container->saved-entity
                                                  (fetch-all-containers game-state)))
           (saved-doors                   (mapcar #'door->saved-door
                                                  (fetch-all-doors      game-state)))
           (saved-magic-furnitures        (mapcar #'magic-furniture->saved-magic-furniture
                                                  (fetch-all-magic-furnitures  game-state)))
           (dmg-matrix                    (build-dmg-matrix game-state))
           (exhausted-fountains-ids               (exhausted-fountains-ids         blackboard))
           (visited-tiles                         (visited-tiles                   blackboard))
           (unexplored-layer                      (unexplored-layer                blackboard))
           (concerning-tiles                      (concerning-tiles                blackboard))
           (concerning-tiles-invalicables         (concerning-tiles-invalicables   blackboard))
           (concerning-tiles-facing               (concerning-tiles-facing         blackboard))
           (attack-enemy-melee-positions          (attack-enemy-melee-positions    blackboard))
           (attack-enemy-pole-positions           (attack-enemy-pole-positions     blackboard))
           (attack-enemy-bow-positions            (attack-enemy-bow-positions      blackboard))
           (attack-enemy-crossbow-positions       (attack-enemy-crossbow-positions blackboard))
           (fow-texture                           (texture-fow                     game-state))
           (to-save                 (make-instance 'saved-game
                                                   :saved-doors            saved-doors
                                                   :saved-containers       saved-containers
                                                   :saved-traps            saved-traps
                                                   :saved-players          saved-players
                                                   :saved-magic-furnitures saved-magic-furnitures
                                                   :saved-difficult-level  saved-difficult
                                                   :dmg-points             dmg-matrix
                                                   :delta-tiles            current-map-state
                                                   :original-map-file
                                                   (game-map-file game-state)
                                                   :saved-exhausted-fountains-ids
                                                   exhausted-fountains-ids
                                                   :saved-visited-tiles
                                                   visited-tiles
                                                   :saved-unexplored-layer
                                                   unexplored-layer
                                                   :saved-concerning-tiles
                                                   concerning-tiles
                                                   :saved-concerning-tiles-invalicables
                                                   concerning-tiles-invalicables
                                                   :saved-concerning-tiles-facing
                                                   concerning-tiles-facing
                                                   :saved-attack-enemy-melee-positions
                                                   attack-enemy-melee-positions
                                                   :saved-attack-enemy-pole-positions
                                                   attack-enemy-pole-positions
                                                   :saved-attack-enemy-bow-positions
                                                   attack-enemy-bow-positions
                                                   :saved-attack-enemy-crossbow-positions
                                                   attack-enemy-crossbow-positions
                                                   :saved-fow-texture
                                                   ;; during  the game
                                                   ;;(for   efficience
                                                   ;;purposes)    only
                                                   ;;the   "bits"   of
                                                   ;;this  texture are
                                                   ;;modified;     but
                                                   ;;only  the  "data"
                                                   ;;slots          is
                                                   ;;serialized: so we
                                                   ;;need to  sync the
                                                   ;;first    to   the
                                                   ;;latter
                                                   (pixmap:sync-bits-to-data fow-texture))))
      (fs:dump-sequence-to-file (serialize to-save) saved-file)
      saved-file)))

(defun start-player-generator (world)
  #'(lambda (w e)
      (mtree:add-child (world:gui world)
                       (widget:make-player-generator world))
      (widget:hide-and-remove-parent-cb w e)))

(defun start-player-fetcher (world shaders)
  #'(lambda (w e)
      (mtree:add-child (world:gui world)
                       (load-save-window:make-window shaders
                                                     :load
                                                     :players-only t
                                                     :title        (_ "Load Characters")))
      (widget:hide-and-remove-parent-cb w e)))

(defun choose-game-characters (window)
  (with-accessors ((world            world)
                   (compiled-shaders main-window:root-compiled-shaders)) window
    (let* ((message (widget:make-message-box (_ "Pick characters form previous game session?")
                                             (_ "Choose characters")
                                             :question
                                             (cons (_ "No")
                                                   (start-player-generator world))
                                             (cons (_ "Yes")
                                                   (start-player-fetcher world
                                                                         compiled-shaders)))))
      (setf (compiled-shaders message) compiled-shaders)
      (mtree:add-child (world:gui world) message))))

(defun init-new-map (window difficult-level)
  (with-accessors ((world world)
                   (window-game-state    main-window:window-game-state)
                   (root-compiled-shaders main-window:root-compiled-shaders)) window
    ;(setf (level-difficult  (main-state world)) difficult-level)
    (setf (world:gui world)
          (make-instance 'widget:widget
                         :x      0.0
                         :y      0.0
                         :width  *window-w*
                         :height *window-h*
                         :label  nil))
    (mtree:add-child (world:gui world) (world:toolbar world))
    (choose-game-characters window)
    (mtree:add-child (world:gui world)
                     (full-screen-masks:make-burn-mask
                      (level-name (main-state world))
                      (level-name-color (main-state world))))
    (mtree:add-child (world:gui world)
                     (full-screen-masks:make-fade-curtain root-compiled-shaders
                                                          :direction :out
                                                          :speed     0.25))
    (setf (interfaces:compiled-shaders (world:gui world)) root-compiled-shaders)
    (setf saved-game:*map-loaded-p* t)
    ;; workaround! approx-terrain-height@pos  fails if we do  not call
    ;; this before
    (interfaces:calculate  world 0.0)
    (place-opponents world difficult-level)
    ;; play bg music
    (sound:play-music sound:+bg-battle-1+ :check-inhibit-from-config t)
    (setf (main-window::delta-time-elapsed window) (sdl2:get-ticks))
    ;; bg color
    (let ((color (skydome-bottom-color (main-window:window-game-state window))))
      (gl:clear-color (elt color 0)
                      (elt color 1)
                      (elt color 2)
                      1.0))))

(defun prepare-for-map-loading (window)
  (with-accessors ((world world)
                   (root-compiled-shaders main-window:root-compiled-shaders)) window
    (clean-up-system window)
    (clean-parallel-kernel)
    (identificable:init-counter)
    #+debug-mode (main-window:clean-up-placeholder)
    (init-parallel-kernel)
    (sdl2.kit-utils:clean-restart-gl-context window)
    (init-system-when-gl-context-active window)
    (setf (interfaces:compiled-shaders (world:gui world)) root-compiled-shaders)
    (setf (world:opening-mode world) nil)
    (setf (camera:mode (world:camera world)) :fp)
    (camera:install-drag-interpolator (world:camera world)
                                      :spring-k +camera-drag-spring-k+)
    ;; setup projection
    (transformable:build-projection-matrix world *near* *far* *fov*
                                           (num:desired (/ *window-w* *window-h*)))
    ;; setup visibility placeholder
    (able-to-see-mesh:setup-placeholder world root-compiled-shaders)))

(defun init-new-map-from-dump (window saved-dump)
  (assert saved-dump)
  (with-accessors ((world world)
                   (root-compiled-shaders main-window:root-compiled-shaders)) window
    (prepare-for-map-loading window)
    ;; (setf (level-difficult  (main-state world))
    ;;       (saved-diffcult-level saved-dump))
    (load-map window (original-map-file saved-dump) (saved-diffcult-level saved-dump))
    (setf (world:gui world)
          (make-instance 'widget:widget
                         :x 0.0 :y 0.0
                         :width  *window-w*
                         :height *window-h*
                         :label  nil))
    (mtree:add-child (world:gui world) (world:toolbar world))
    (mtree:add-child (world:gui world)
                     (full-screen-masks:make-burn-mask
                      (level-name (main-state world))
                      (level-name-color (main-state world))))
    (setf (interfaces:compiled-shaders (world:gui world)) root-compiled-shaders)
    (setf (main-window::delta-time-elapsed window) (sdl2:get-ticks))
    (setf saved-game:*map-loaded-p* t)
    ;; play bg music
    (sound:play-music sound:+bg-battle-1+ :check-inhibit-from-config t)
    ;; bg color
    (let ((color (skydome-bottom-color (main-window:window-game-state window))))
      (gl:clear-color (elt color 0)
                      (elt color 1)
                      (elt color 2)
                      1.0))
    ;; workaround! approx-terrain-height@pos  fails if we do  not call
    ;; this before
    (interfaces:calculate  world 0.0)))

(defun init-system-when-gl-context-active (window)
  (with-accessors ((root-compiled-shaders main-window:root-compiled-shaders)
                   (world            main-window:world)
                   (window-game-state main-window:window-game-state)) window
    (setf window-game-state (make-instance 'game-state:game-state))
    (setf (game-state:window-id window-game-state)
          (sdl2.kit-utils:fetch-window-id window))
    (when (gconf:config-tree-clip)
      (mesh:set-tree-clip))
    (game-event:register-for-window-accept-input-event window)
    (gl:front-face :ccw)
    (gl:enable :depth-test :cull-face)
    (gl:depth-func :less)
    (gl:polygon-mode :front-and-back :fill)
    (gl:clear-color 0 0 0 1)
    (gl:clear-depth 1.0)
    (setf root-compiled-shaders (shaders-utils:compile-library))
    ;; we need a valid opengl context to start texture's database
    (texture:init-db)
    ;; we need a valid opengl context to load spells database
    (spell:load-spell-db)
    (status-orb:init-db)
    (gui:setup-gui root-compiled-shaders)
    ;; set up world
    (setf (main-window:world window) nil)
    (tg:gc :full t)
    (setf world                       (make-instance 'world
                                                     :frame-window window
                                                     :opening-mode t))
    (setf (main-state world)          window-game-state)
    (setf (compiled-shaders  world)   root-compiled-shaders)
    (mtree:add-child (world:gui world) (widget:make-splash-progress-gauge))
    (setf (interfaces:compiled-shaders (world:gui world)) root-compiled-shaders)
    (setf (camera:mode (world:camera (main-window:world window))) :fp)
    (camera:install-path-interpolator (world:camera world)
                                      (sb-cga:vec 0.0  15.0 0.0)
                                      (sb-cga:vec 64.0 30.0 0.0)
                                      (sb-cga:vec 64.0 20.0 64.0)
                                      (sb-cga:vec 0.0  30.0 64.0)
                                      (sb-cga:vec 64.0  90.0 64.0))
    (camera:install-drag-interpolator (world:camera world) :spring-k +camera-drag-spring-k+)
    ;; setup projection
    (transformable:build-projection-matrix world *near* *far* *fov*
                                           (num:desired (/ *window-w* *window-h*)))
    ;; setup visibility placeholder
    (able-to-see-mesh:setup-placeholder world root-compiled-shaders)
    (setf (main-window:delta-time-elapsed window) (sdl2:get-ticks))))

(defun init-parallel-kernel ()
  (setf *workers-number* (if (> (os-utils:cpu-number) 1)
                             (os-utils:cpu-number)
                             1))
  (setf lparallel:*kernel* (lparallel:make-kernel *workers-number*)))

(defun clean-parallel-kernel ()
  (lparallel:end-kernel :wait t))

(defun init-system ()
  (tg:gc :full t)
  (handler-bind ((error
                  #'(lambda(e)
                      (declare (ignore e))
                      (invoke-restart 'cl-i18n:return-empty-translation-table))))
    (setf cl-i18n:*translation-file-root* +catalog-dir+)
    (cl-i18n:load-language +text-domain+ :locale (cl-i18n:find-locale)))
  (init-parallel-kernel)
  (identificable:init-counter)
  (player-messages-text:init-player-messages-db)
  (resources-utils:init)
  (game-configuration:init)
  (strategic-ai:build-decision-tree)
  (setf saved-game:*map-loaded-p* nil)
  (sdl2:gl-set-attr :context-profile-mask  1)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3))

(defun clean-up-system (window)
  (setf (main-window:window-game-state window) nil)
  (absee-mesh:clean-visibility-target-placeholder)
  (interfaces:destroy (main-window:world window))
  (interfaces:destroy (main-window:root-compiled-shaders window))
  (texture:clean-db)
  (status-orb:clean-db)
  (arrows:clean-db)
  (spell:clean-spell-db)
  (gui:clean-font-db)
  (sprite:clean-sprite-db)
  (game-event:clean-all-events-vectors)
  (setf (main-window:world window) nil)
  (tg:gc :full t))

(defun load-map (window map-file difficult-level)
  (with-accessors ((world world)
                   (root-compiled-shaders main-window:root-compiled-shaders)) window
    (setf *map-loaded-p* nil)
    (setf (level-difficult  (main-state world)) difficult-level)
    (load-level:load-level window
                           world
                           (main-window:window-game-state window)
                           map-file)
    (camera:look-at (world:camera world)
                    *xpos* *ypos* *zpos* *xeye* *yeye* *zeye* *xup* *yup* *zup*)
    (setf (camera:mode (world:camera world)) :fp)))

(defun delete-in-map-destination-p (orig dest x y)
  (and (empty@pos-p orig x y)
       (not (empty@pos-p dest x y))))

(defun delete-in-map-destination (game-state x y)
  (let ((entity-to-delete (entity-in-pos game-state x y)))
    (assert entity-to-delete)
    (remove-from-game entity-to-delete)))

(defun error-message-save-game-outdated (window)
  (with-accessors ((root-compiled-shaders main-window:root-compiled-shaders)
                   (world                 main-window:world)) window
    (let* ((error-text (_ "The map file of this level has been modified after the game was saved."))
           (error-msg  (widget:make-message-box error-text
                                                (_ "Saved game is outdated")
                                                :error
                                                (cons (_ "OK")
                                                      #'widget:hide-and-remove-parent-cb))))
      (setf (compiled-shaders error-msg) root-compiled-shaders)
      (mtree:add-child (gui world) error-msg))))

(defun clean-handle (obj)
   (when obj
     (let ((tex (portrait obj)))
       (destroy tex)
       (post-deserialization-fix tex)))
   obj)

(defun fix-texture-handlers-players (dump)
  (with-accessors ((saved-players saved-players)) dump
    (map nil #'(lambda (a) (clean-handle (player-ghost a)))
         saved-players)
    (loop for player in saved-players do
         (let ((ghost (player-ghost player)))
           (with-accessors ((helm       helm)
                            (shoes      shoes)
                            (armor      armor)
                            (left-hand  left-hand)
                            (right-hand right-hand)
                            (ring       ring)) ghost

             (clean-handle helm)
             (clean-handle shoes)
             (clean-handle armor)
             (clean-handle left-hand)
             (clean-handle right-hand)
             (clean-handle ring)
             (loop for item in (inventory ghost) do
                  (clean-handle item)))))))

(defun fix-id-carryed-items-players (dump)
  (flet ((reset-id (a)
           (and a
                (refresh-id a))))
    (with-accessors ((saved-players saved-players)) dump
      (map nil #'(lambda (a) (reset-id (player-ghost a)))
           saved-players)
      (loop for player in saved-players do
           (let ((ghost (player-ghost player)))
             (with-accessors ((helm       helm)
                              (shoes      shoes)
                              (armor      armor)
                              (left-hand  left-hand)
                              (right-hand right-hand)
                              (ring       ring)) ghost
               (reset-id helm)
               (reset-id shoes)
               (reset-id armor)
               (reset-id left-hand)
               (reset-id right-hand)
               (reset-id ring)
               (loop for item in (inventory ghost) do
                    (reset-id item))))))))

(defun fix-texture-handlers-containers (dump)
  (with-accessors ((saved-containers saved-containers)) dump
    (loop for container in saved-containers do
         (mtree:top-down-visit (player-ghost container)
                               #'(lambda (n) (clean-handle n))))))

(defun fix-texture-handles (dump)
  (with-accessors ((saved-fow-texture saved-fow-texture)) dump
    (destroy                  saved-fow-texture)
    (post-deserialization-fix saved-fow-texture)
    (fix-texture-handlers-players    dump)
    (fix-texture-handlers-containers dump)))

(defun find-by-pos (needle)
  #'(lambda (a)
      (ivec2:ivec2= needle
                    (original-map-pos a))))

(defun place-player-in-map-destination (world dump x y faction shaders &key (force-position t))
  (let* ((map-pos       (ivec2:ivec2 x y))
         (saved-player  (find-if (find-by-pos map-pos)
                                 (saved-players dump))))
    (place-player-in-map-destination* world saved-player x y faction shaders
                                      :force-position force-position)))

(defun place-player-in-map-destination* (world saved-player x y faction shaders
                                        &key (force-position t))
  (let* ((resource-data (if (faction-player-p faction)
                            +human-player-sprite-resource+
                            +ai-player-sprite-resource+))
         (map-pos       (ivec2:ivec2 x y))
         (sprite-info   (sprite-infos saved-player))
         (new-ghost     (player-ghost saved-player))
         (sprite        (sprite:load-sprite-player new-ghost
                                                   (dir sprite-info)
                                                   shaders
                                                   resource-data)))
    (character::init-logs new-ghost)
    (sprite:update-wearable-spritesheet sprite)
    (world:place-player-on-map world
                               sprite
                               faction
                               :position         map-pos
                               :force-position-p force-position)
    (setf (dir sprite) (original-dir saved-player))
    (when (faction-ai-p faction)
      (setf (renderp sprite) nil)
      (let ((position (calculate-cost-position sprite)))
        (2d-utils:displace-2d-vector (position x y)
          (blackboard:reset-per-turn-visited-tiles (blackboard (state sprite)))
          (set-tile-visited (state sprite) sprite x y))))
    (when (status-faint-p (ghost sprite))
      (set-death-status sprite))))

(defun place-trap-in-map-destination (game-state dump x y shaders)
  (let* ((map-pos    (ivec2:ivec2 x y))
         (saved-trap (find-if (find-by-pos map-pos)
                              (saved-traps dump)))
         (faction    (original-faction saved-trap))
         (ghost      (player-ghost     saved-trap)))
    (mesh:build-and-place-trap-on-map game-state
                                      ghost
                                      faction
                                      shaders
                                      (map-utils:coord-map->chunk x)
                                      (map-utils:coord-map->chunk y))))

(defun place-container-in-map-destination (game-state dump x y)
  (let* ((map-pos         (ivec2:ivec2 x y))
         (saved-container (find-if (find-by-pos map-pos)
                                   (saved-containers dump)))
         (ghost          (player-ghost saved-container))
         (dest-container (entity-in-pos game-state x y)))
    (setf (ghost dest-container) ghost)))

(defun restore-door-status (game-state dump x y)
  (let* ((map-pos    (ivec2:ivec2 x y))
         (saved-door (find-if (find-by-pos map-pos)
                              (saved-doors dump))))
    (when (openedp saved-door)
      (let* ((mesh       (entity-in-pos game-state x y))
             (id-door    (id mesh))
             (door-event (game-event:make-simple-event-w-dest 'game-event:open-door-event
                                                              nil
                                                              id-door)))
        (game-event:propagate-open-door-event door-event)))))

(defun restore-magic-furniture-status (game-state dump x y)
  (let* ((map-pos         (ivec2:ivec2 x y))
         (saved-furniture (find-if (find-by-pos map-pos)
                                   (saved-magic-furnitures dump)))
         (mesh            (entity-in-pos game-state x y)))
    (setf (spell-recharge-count mesh)
          (original-spell-recharge-count saved-furniture))
    game-state))

(defun restore-entities (window saved-dump)
  (with-accessors ((delta-tiles delta-tiles)) saved-dump
    (with-accessors ((window-game-state     main-window:window-game-state)
                     (root-compiled-shaders main-window:root-compiled-shaders)
                     (world                 main-window:world)) window
      (with-accessors ((map-state  map-state)
                       (blackboard blackboard)) window-game-state
        (loop-matrix (delta-tiles x y)
           (cond
             ((delete-in-map-destination-p saved-dump map-state x y)
              (delete-in-map-destination window-game-state x y))
             ((entity-ai@pos-p saved-dump x y)
              (place-player-in-map-destination world
                                               saved-dump
                                               x y
                                               +npc-type+
                                               root-compiled-shaders
                                               :force-position t))
             ((entity-player@pos-p saved-dump x y)
              (place-player-in-map-destination world
                                               saved-dump
                                               x y
                                               +pc-type+
                                               root-compiled-shaders
                                               :force-position t))
             ((trap@pos-p saved-dump x y)
              (place-trap-in-map-destination window-game-state saved-dump x y
                                             root-compiled-shaders))
             ((container@pos-p saved-dump x y)
              (place-container-in-map-destination window-game-state
                                                  saved-dump
                                                  x y))
             ((door@pos-p saved-dump x y)
              (restore-door-status window-game-state saved-dump x y))
             ((magic-furniture@pos-p saved-dump x y)
              (restore-magic-furniture-status window-game-state
                                              saved-dump
                                              x y))))))))

(defun restore-damage (window saved-dump)
  ;; restore dmg points for wall and like
  (with-accessors ((dmg-points dmg-points)) saved-dump
    (with-accessors ((window-game-state main-window:window-game-state)) window
      (loop-matrix (dmg-points x y)
         (let ((new-dmg-points (matrix-elt dmg-points y x)))
           (when new-dmg-points
             (let ((entity (entity-in-pos window-game-state x y)))
               (setf (current-damage-points (ghost entity)) new-dmg-points))))))))

(defun restore-blackboard (blackboard saved-dump)
  (with-accessors ((original-map-file original-map-file)
                     (dmg-points dmg-points)
                     (saved-exhausted-fountains-ids         saved-exhausted-fountains-ids)
                     (saved-visited-tiles                   saved-visited-tiles)
                     (saved-unexplored-layer                saved-unexplored-layer)
                     (saved-concerning-tiles                saved-concerning-tiles)
                     (saved-concerning-tiles-invalicables   saved-concerning-tiles-invalicables)
                     (saved-concerning-tiles-facing         saved-concerning-tiles-facing)
                     (saved-attack-enemy-melee-positions    saved-attack-enemy-melee-positions)
                     (saved-attack-enemy-pole-positions     saved-attack-enemy-pole-positions)
                     (saved-attack-enemy-bow-positions      saved-attack-enemy-bow-positions)
                     (saved-attack-enemy-crossbow-positions saved-attack-enemy-crossbow-positions))
        saved-dump
    (setf (exhausted-fountains-ids         blackboard) saved-exhausted-fountains-ids
          (visited-tiles                   blackboard) saved-visited-tiles
          (unexplored-layer                blackboard) saved-unexplored-layer
          (concerning-tiles                blackboard) saved-concerning-tiles
          (concerning-tiles-invalicables   blackboard) saved-concerning-tiles-invalicables
          (concerning-tiles-facing         blackboard) saved-concerning-tiles-facing
          (attack-enemy-melee-positions    blackboard) saved-attack-enemy-melee-positions
          (attack-enemy-pole-positions     blackboard) saved-attack-enemy-pole-positions
          (attack-enemy-bow-positions      blackboard) saved-attack-enemy-bow-positions
          (attack-enemy-crossbow-positions blackboard) saved-attack-enemy-crossbow-positions)))

(defun restore-fow (window saved-dump)
  (with-accessors ((window-game-state main-window:window-game-state)) window
      ;; restore FOW texture
    (setf (entity:texture-fow window-game-state) (saved-fow-texture saved-dump))
    ;; ensure a  simple array for 'bits'  slot otherwise a
    ;; kraken will be released
    (pixmap:cristallize-bits (entity:texture-fow window-game-state))
    (loop-matrix ((entity:texture-fow window-game-state) x y)
       (when-let ((entity (entity-in-pos window-game-state x y)))
         (if (thrown-down-in-fow-p window-game-state :x x :y y)
             (throw-down-in-fow entity)
             (popup-from-fow    entity))))
    window))

(defun load-game (window resource-dir)
  (let ((saved-dump (make-instance 'saved-game))
        (saved-file (res:get-resource-file +map-saved-filename+
                                           resource-dir
                                           :if-does-not-exists :error)))
    (setf saved-dump (deserialize saved-dump saved-file))
    (with-accessors ((original-map-file original-map-file)) saved-dump
      (if (not (fs:file-outdated-p saved-file
                                   (load-level:get-level-file-abs-path original-map-file)))
          (progn
            (init-new-map-from-dump window saved-dump)
            ;; workaround!     the    next     is    needed    because
            ;; init-new-map-from-dump   clean  the   texture  database
            ;; (texture:*texture-factory-db*)
            (fix-texture-handles          saved-dump)
            ;; refresh all ids
            (fix-id-carryed-items-players saved-dump)
            (with-accessors ((window-game-state     main-window:window-game-state)
                             (root-compiled-shaders main-window:root-compiled-shaders)
                             (world                 main-window:world)) window
              (with-accessors ((map-state  map-state)
                               (blackboard blackboard)) window-game-state
                ;; restore entities
                (restore-entities window saved-dump)
                ;; restore dmg points for wall and like
                (restore-damage window saved-dump)
                ;; restore FOW
                (restore-fow window saved-dump)
                ;; restore blackboard
                (restore-blackboard blackboard saved-dump)
                ;; update all labyrinths (instanced meshes)
                (maphash #'(lambda (k v)
                             (declare (ignore k))
                             (update-for-rendering v))
                         (labyrinth-entities window-game-state))
                ;; some enemy  (AI) need  to be rendered  because they
                ;; was  visibles  when  the   game  was  saved  (note:
                ;; probably useless)
                (update-rendering-needed-ai window-game-state)
                ;; update  all visibility,  check  if  this makes  the
                ;; above function call useless
                (update-all-visibility-state window-game-state)
                ;; select a pc
                (select-and-slide-to-first-pc world)
                ;; start a new turn
                (let ((start-event (make-instance 'game-event:start-turn
                                                  :reset-characters     nil
                                                  :increment-turn-count nil)))
                  (game-event:propagate-start-turn start-event
                                                   :function-after-propagation
                                                   #'game-event:send-refresh-toolbar-event))))
            window)
          (progn
            (error-message-save-game-outdated window)
            nil)))))

(defun add-all-fetch-player-windows (npcs compiled-shaders world)
  (let ((callbacks
         (loop for npc in npcs collect
              (let ((local-npc npc))
                #'(lambda (w e)
                    (place-player-in-map-destination* world local-npc 0 0 +pc-type+
                                                      compiled-shaders
                                                      :force-position nil)
                    (select-and-slide-to-first-pc world)
                    (widget:hide-and-remove-parent w e))))))
    (load-save-window:add-all-fetch-player-windows (mapcar #'player-ghost npcs)
                                                   callbacks
                                                   compiled-shaders
                                                   world)))

(defun load-players (window resource-dir)
  (let ((saved-dump (make-instance 'saved-game))
        (saved-file (res:get-resource-file +map-saved-filename+
                                           resource-dir
                                           :if-does-not-exists :error)))
    (setf saved-dump (deserialize saved-dump saved-file))
    (with-accessors ((saved-players     saved-players)
                     (original-map-file original-map-file)) saved-dump
      (with-accessors ((window-game-state     main-window:window-game-state)
                       (root-compiled-shaders main-window:root-compiled-shaders)
                       (world                 main-window:world)) window
      (if (not (fs:file-outdated-p saved-file
                                   (load-level:get-level-file-abs-path original-map-file)))
          (progn
            ;; workaround!     the    next     is    needed    because
            ;; init-new-map-from-dump   clean  the   texture  database
            ;; (texture:*texture-factory-db*)
            (fix-texture-handles          saved-dump)
            ;; refresh all ids
            (fix-id-carryed-items-players saved-dump)
            ;; restore entitites
            (let ((npcs (remove-if #'(lambda (a)
                                       (or (status-faint-p (player-ghost a))
                                           (not (eq (original-faction a)
                                                    +pc-type+))))
                                   saved-players)))
              (add-all-fetch-player-windows npcs
                                            root-compiled-shaders
                                            world))
            window)
          (progn
            (error-message-save-game-outdated window)
            nil))))))
