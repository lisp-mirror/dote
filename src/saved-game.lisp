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
  ((mesh-infos
    :initform nil
    :initarg  :mesh-infos
    :accessor mesh-infos
    :type     md2-fs-res)
   (original-mesh-id
    :initform nil
    :initarg  :original-mesh-id
    :accessor original-mesh-id
    :type     fixnum)))

(defmethod marshal:class-persistant-slots ((object saved-player))
  (append '(mesh-infos
            original-mesh-id
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

(defclass saved-game ()
  ((original-map-file
    :initform nil
    :initarg  :original-map-file
    :accessor original-map-file
    :type     string)
   (delta-tiles
    :initform nil
    :initarg  :delta-tiles
    :accessor delta-tiles
    :type     matrix
    :documentation     "A     snapshot     of    the     matrix     in
    game-state:map-state-element when the game was saved.")
   (saved-entities
    :initform nil
    :initarg  :saved-entities
    :accessor saved-entities
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
    :type     list)))

(defmethod marshal:class-persistant-slots ((object saved-game))
  '(original-map-file
    delta-tiles
    saved-entities
    saved-traps
    saved-doors
    saved-containers))

(defmethod el-type-in-pos ((object saved-game) (x fixnum) (y fixnum))
  (el-type (matrix-elt (delta-tiles object) y x)))

(defmethod entity-id-in-pos ((object saved-game) (x fixnum) (y fixnum))
  (entity-id (matrix-elt (delta-tiles object) y x)))

(defmethod empty@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (empty@pos-p (delta-tiles object) x y))

(defmethod trap@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (trap@pos-p (delta-tiles object) x y))

(defmethod container@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (container@pos-p (delta-tiles object) x y))

(defmethod door@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (door@pos-p (delta-tiles object) x y))

(defmethod pawn-@pos-p ((object saved-game) x y)
  (or (entity-ai-in-pos     object x y)
      (entity-player-in-pos object x y)))

(defmethod entity-ai@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (let ((entity-type (el-type-in-pos object x y)))
    (eq entity-type +npc-type+)))

(defmethod entity-player@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (let ((entity-type (el-type-in-pos object x y)))
    (eq entity-type +pc-type+)))

(defun mesh->saved-entity (mesh)
  (make-instance 'saved-player
                 :mesh-infos       (fs-resources            mesh)
                 :player-ghost     (ghost                   mesh)
                 :original-faction (my-faction              mesh)
                 :original-dir     (dir                     mesh)
                 :original-map-pos (calculate-cost-position mesh)
                 :original-mesh-id (id                      mesh)))

(defun trap->saved-entity (mesh)
  (make-instance 'saved-entity-w-faction
                 :original-faction (my-faction              mesh)
                 :original-map-pos (calculate-cost-position mesh)
                 :player-ghost     (ghost mesh)))

(defun container->saved-entity (mesh)
  (make-instance 'saved-container
                 :original-map-pos (calculate-cost-position mesh)
                 :player-ghost     (ghost mesh)))

(defun door->saved-door (mesh)
  (make-instance 'saved-door
                 :original-map-pos (calculate-cost-position mesh)
                 :openedp          (openp mesh)
                 :player-ghost     (ghost mesh)))

(defun save-game (resource-dir game-state)
  (let* ((saved-file        (res:get-resource-file +map-saved-filename+
                                                   resource-dir
                                                   :if-does-not-exists :create))
         (current-map-state (map-state game-state))
         (saved-players     (append (map-ai-entities     game-state #'mesh->saved-entity)
                                    (map-player-entities game-state #'mesh->saved-entity)))
         (saved-traps       (mapcar #'trap->saved-entity      (fetch-all-traps game-state)))
         (saved-containers  (mapcar #'container->saved-entity (fetch-all-containers game-state)))
         (saved-doors       (mapcar #'door->saved-door        (fetch-all-doors     game-state)))
         (to-save           (make-instance 'saved-game
                                           :saved-doors       saved-doors
                                           :saved-containers  saved-containers
                                           :saved-traps       saved-traps
                                           :saved-entities    saved-players
                                           :delta-tiles       current-map-state
                                           :original-map-file (game-map-file game-state))))
    (fs:dump-sequence-to-file (serialize to-save) saved-file)
    saved-file))

(defun init-new-map (window)
  (with-accessors ((world world)
                   (root-compiled-shaders main-window:root-compiled-shaders)) window
    ;; (setf (interfaces:compiled-shaders (world:gui world)) root-compiled-shaders)
    ;; (mtree:add-child (world:gui world) (widget:make-splash-progress-gauge))
    (setf (camera:mode (world:camera world)) :fp)
    (camera:install-drag-interpolator (world:camera world)
                                      :spring-k +camera-drag-spring-k+)
    ;; setup projection
    (transformable:build-projection-matrix world *near* *far* *fov*
                                           (num:desired (/ *window-w* *window-h*)))
    ;; setup visibility placeholder
    (able-to-see-mesh:setup-placeholder world root-compiled-shaders)
    (setf (world:gui world)
          (make-instance 'widget:widget
                         :x 0.0 :y 0.0
                         :width  *window-w*
                         :height *window-h*
                         :label  nil))
    (mtree:add-child (world:gui world) (world:toolbar world))
    (mtree:add-child (world:gui world)
                     (widget:make-player-generator world))
    (mtree:add-child (world:gui world)
                     (full-screen-masks:make-burn-mask
                      (level-name (main-state world))
                      (level-name-color (main-state world))))
    (setf (interfaces:compiled-shaders (world:gui world)) root-compiled-shaders)
    (setf saved-game:*map-loaded-p* t)
    ;; workaround! approx-terrain-height@pos  fails if we do  not call
    ;; this before
    (interfaces:calculate  world 0.0)
    ;; test
    ;; testing opponents
    (world:add-ai-opponent world :warrior :male)
    (world:add-ai-opponent world :wizard  :male)
    ;;;;;
    (setf (main-window::delta-time-elapsed window) (sdl2:get-ticks))
    ;; bg color
    (let ((color (skydome-bottom-color (main-window:window-game-state window))))
      (gl:clear-color (elt color 0)
                      (elt color 1)
                      (elt color 2)
                      1.0))))

(defun init-new-map-from-dump (window saved-dump)
  (assert saved-dump)
  (with-accessors ((world world)
                   (root-compiled-shaders main-window:root-compiled-shaders)) window
    (clean-up-system window)
    (clean-parallel-kernel)
    (identificable:init-counter)
    #+debug-mode (main-window:clean-up-placeholder)
    (init-parallel-kernel)
    (init-system-when-gl-context-active window)
    (setf (interfaces:compiled-shaders (world:gui world)) root-compiled-shaders)
    (setf (camera:mode (world:camera world)) :fp)
    (camera:install-drag-interpolator (world:camera world)
                                      :spring-k +camera-drag-spring-k+)
    ;; setup projection
    (transformable:build-projection-matrix world *near* *far* *fov*
                                           (num:desired (/ *window-w* *window-h*)))
    ;; setup visibility placeholder
    (able-to-see-mesh:setup-placeholder world root-compiled-shaders)
    (load-map window (original-map-file saved-dump))
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
    (game-event:register-for-window-accept-input-event window)
    (gl:front-face :ccw)
    (gl:enable :depth-test :cull-face)
    (gl:depth-func :less)
    (gl:polygon-mode :front-and-back :fill)
    (gl:clear-color 0 0 0 1)
    (gl:clear-depth 1.0)
    (setf root-compiled-shaders (shaders-utils:compile-library))
    ;; we need a valid opengl context to load spells database
    (spell:load-spell-db)
    ;; we need a valid opengl context to start texture's database
    (texture:init-db)
    (gui:setup-gui root-compiled-shaders)
    ;; set up world
    (setf (main-window:world window) nil)
    (tg:gc :full t)
    (setf (main-window:world window) (make-instance 'world :frame-window window))
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
  (setf saved-game:*map-loaded-p* nil)
  (sdl2.kit:start)
  (sdl2:gl-set-attr :context-profile-mask  1)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3))

(defun clean-up-system (window)
  (setf (main-window:window-game-state window) nil)
  (absee-mesh:clean-visibility-target-placeholder)
  (interfaces:destroy (main-window:world window))
  (interfaces:destroy (main-window:root-compiled-shaders window))
  (texture:clean-db)
  (arrows:clean-db)
  (spell:clean-spell-db)
  (gui:clean-font-db)
  (md2:clean-db)
  (game-event:clean-all-events-vectors)
  (setf (main-window:world window) nil)
  (tg:gc :full t))

(defun load-map (window map-file)
  (with-accessors ((world world)
                   (root-compiled-shaders main-window:root-compiled-shaders)) window
    (setf *map-loaded-p* nil)
    (load-level:load-level window
                           world
                           (main-window:window-game-state window)
                           root-compiled-shaders map-file)
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
  (with-accessors ((saved-entities saved-entities)) dump
    (map nil #'(lambda (a) (clean-handle (player-ghost a)))
         saved-entities)
    (loop for player in saved-entities do
         (let ((ghost (player-ghost player)))
           (with-accessors ((elm        elm)
                            (shoes      shoes)
                            (armor      armor)
                            (left-hand  left-hand)
                            (right-hand right-hand)
                            (ring       ring)) ghost

             (clean-handle elm)
             (clean-handle shoes)
             (clean-handle armor)
             (clean-handle left-hand)
             (clean-handle right-hand)
             (clean-handle ring)
             (loop for item in (inventory ghost) do
                  (clean-handle item)))))))

(defun fix-texture-handlers-containers (dump)
  (with-accessors ((saved-containers saved-containers)) dump
    (loop for container in saved-containers do
         (mtree:top-down-visit (player-ghost container)
                               #'(lambda (n) (clean-handle n))))))

(defun fix-texture-handles (dump)
  (fix-texture-handlers-players    dump)
  (fix-texture-handlers-containers dump))

(defun find-by-pos (needle)
  #'(lambda (a)
      (ivec2:ivec2= needle
                    (original-map-pos a))))

(defun place-player-in-map-destination (world dump x y faction shaders)
  (let* ((resource-data (if (faction-player-p faction)
                            +human-player-models-resource+
                            +ai-player-models-resource+))
         (map-pos       (ivec2:ivec2 x y))
         (saved-player  (find-if (find-by-pos map-pos)
                                 (saved-entities dump)))
         (mesh-info     (mesh-infos saved-player))
         (new-ghost     (player-ghost saved-player))
         (mesh          (md2:load-md2-player new-ghost
                                             (dir mesh-info)
                                             shaders
                                             resource-data)))
    (character::init-logs new-ghost)
    (world:place-player-on-map world
                               mesh
                               faction
                               :position         (ivec2:ivec2 x y)
                               :force-position-p t)
    (setf (dir mesh) (original-dir saved-player))
    (when (faction-ai-p faction)
      (setf (renderp mesh) nil)
      (let ((position (calculate-cost-position mesh)))
        (2d-utils:displace-2d-vector (position x y)
          (blackboard:reset-per-turn-visited-tiles (blackboard (state mesh)))
          (set-tile-visited (state mesh) mesh x y))))))

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

(defun load-game (window resource-dir)
  (let ((saved-dump (make-instance 'saved-game))
        (saved-file (res:get-resource-file +map-saved-filename+
                                           resource-dir
                                           :if-does-not-exists :error)))
    (setf saved-dump (deserialize saved-dump saved-file))
    (with-accessors ((delta-tiles       delta-tiles)
                     (original-map-file original-map-file)) saved-dump
      (if (not (fs:file-outdated-p saved-file
                                   (load-level:get-level-file-abs-path original-map-file)))
          (progn
            (init-new-map-from-dump window saved-dump)
            ;; workaround!     the    next     is    needed    because
            ;; init-new-map-from-dump   clean  the   texture  database
            ;; (texture:*texture-factory-db*)
            (fix-texture-handles saved-dump)
            (with-accessors ((window-game-state     main-window:window-game-state)
                             (root-compiled-shaders main-window:root-compiled-shaders)
                             (world                 main-window:world)) window
              (with-accessors ((map-state map-state)) window-game-state
                (loop-matrix (delta-tiles x y)
                   (cond
                     ((delete-in-map-destination-p saved-dump map-state x y)
                      (delete-in-map-destination window-game-state x y))
                     ((entity-ai@pos-p saved-dump x y)
                      (place-player-in-map-destination world
                                                       saved-dump
                                                       x y
                                                       +npc-type+
                                                       root-compiled-shaders))
                     ((entity-player@pos-p saved-dump x y)
                      (place-player-in-map-destination world
                                                       saved-dump
                                                       x y
                                                       +pc-type+
                                                       root-compiled-shaders))
                     ((trap@pos-p saved-dump x y)
                      (place-trap-in-map-destination window-game-state saved-dump x y
                                                     root-compiled-shaders))
                     ((container@pos-p saved-dump x y)
                      (place-container-in-map-destination window-game-state
                                                          saved-dump
                                                          x y))
                     ((door@pos-p saved-dump x y)
                      (let* ((map-pos    (ivec2:ivec2 x y))
                             (saved-door (find-if (find-by-pos map-pos)
                                                  (saved-doors saved-dump))))
                        (when (openedp saved-door)
                          (let* ((mesh       (entity-in-pos window-game-state x y))
                                 (id-door    (id mesh))
                                 (door-event (game-event:make-simple-event-w-dest 'game-event:open-door-event
                                                                                  nil
                                                                                  id-door)))
                            (game-event:propagate-open-door-event door-event))))))))))
          (error-message-save-game-outdated window))))
  window)
