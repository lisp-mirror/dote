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

(defclass saved-player ()
  ((mesh-infos
    :initform nil
    :initarg  :mesh-infos
    :accessor mesh-infos
    :type     md2-fs-res)
   (player-ghost
    :initform nil
    :initarg  :player-ghost
    :accessor player-ghost
    :type     player-character)
   (original-map-pos
    :initform nil
    :initarg  :original-map-pos
    :accessor original-map-pos
    :type     ivec2:ivec2)
   (original-mesh-id
    :initform nil
    :initarg  :original-mesh-id
    :accessor original-mesh-id
    :type     fixnum)))

(defmethod marshal:class-persistant-slots ((object saved-player))
  '(mesh-infos
    player-ghost
    original-map-pos
    original-mesh-id))

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
   (saved-players
    :initform nil
    :initarg  :saved-players
    :accessor saved-players
    :type     list)))

(defmethod marshal:class-persistant-slots ((object saved-game))
  '(original-map-file
    delta-tiles
    saved-players))

(defmethod el-type-in-pos ((object saved-game) (x fixnum) (y fixnum))
  (el-type (matrix-elt (delta-tiles object) y x)))

(defmethod entity-id-in-pos ((object saved-game) (x fixnum) (y fixnum))
  (entity-id (matrix-elt (delta-tiles object) y x)))

(defmethod empty@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (empty@pos-p (delta-tiles object) x y))

(defmethod pawn-@pos-p ((object saved-game) x y)
  (or (entity-ai-in-pos     object x y)
      (entity-player-in-pos object x y)))

(defmethod entity-ai@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (let ((entity-type (el-type-in-pos object x y)))
    (eq entity-type +npc-type+)))

(defmethod entity-player@pos-p ((object saved-game) (x fixnum) (y fixnum))
  (let ((entity-type (el-type-in-pos object x y)))
    (eq entity-type +pc-type+)))

(defun mesh->saved-player (mesh)
  (make-instance 'saved-player
                 :mesh-infos       (fs-resources            mesh)
                 :player-ghost     (ghost                   mesh)
                 :original-map-pos (calculate-cost-position mesh)
                 :original-mesh-id (id                      mesh)))

(defun save-game (resource-dir game-state)
  (let* ((saved-file        (res:get-resource-file +map-saved-filename+
                                                   resource-dir
                                                   :if-does-not-exists :create))
         (current-map-state (map-state game-state))
         (saved-player      (append (map-ai-entities     game-state #'mesh->saved-player)
                                    (map-player-entities game-state #'mesh->saved-player)))
         (to-save           (make-instance 'saved-game
                                           :saved-players     saved-player
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
    ;; test
    ;; testing opponents
    (interfaces:calculate  world 0.0)
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
    (setf saved-game:*map-loaded-p* t)
    (setf (main-window::delta-time-elapsed window) (sdl2:get-ticks))
    ;; bg color
    (let ((color (skydome-bottom-color (main-window:window-game-state window))))
      (gl:clear-color (elt color 0)
                      (elt color 1)
                      (elt color 2)
                      1.0))))

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

(defun fix-texture-handles (dump)
  (flet ((clean (obj)
           (when obj
             (let ((tex (portrait obj)))
               (destroy tex)
               (post-deserialization-fix tex)))
           obj))
    (with-accessors ((saved-players saved-players)) dump
      (map nil #'(lambda (a) (clean (player-ghost a)))
           saved-players)
      (loop for player in saved-players do
           (let ((ghost (player-ghost player)))
             (with-accessors ((elm        elm)
                              (shoes      shoes)
                              (armor      armor)
                              (left-hand  left-hand)
                              (right-hand right-hand)
                              (ring       ring)) ghost

               (clean elm)
               (clean shoes)
               (clean armor)
               (clean left-hand)
               (clean right-hand)
               (clean ring)
               (loop for item in (inventory ghost) do
                    (misc:dbg "clean ~a"  (portrait item))
                    (clean item))))))))

(defun place-player-in-map-destination (world dump x y faction shaders)
  (let* ((resource-data (if (faction-player-p faction)
                            +human-player-models-resource+
                            +ai-player-models-resource+))
         (map-pos       (ivec2:ivec2 x y))
         (saved-player  (find-if #'(lambda (a)
                                     (ivec2:ivec2= map-pos
                                                   (original-map-pos a)))
                                 (saved-players dump)))
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
    (when (faction-ai-p faction)
      (setf (renderp mesh) nil)
      (let ((position (calculate-cost-position mesh)))
        (2d-utils:displace-2d-vector (position x y)
          (blackboard:reset-per-turn-visited-tiles (blackboard (state mesh)))
          (set-tile-visited (state mesh) mesh x y))))))

(defun load-game (window resource-dir)
  (let ((saved-dump (make-instance 'saved-game))
        (saved-file (res:get-resource-file +map-saved-filename+
                                           resource-dir
                                           :if-does-not-exists :error)))
    (tg:gc :full t)
    (setf saved-dump (deserialize saved-dump saved-file))
    (with-accessors ((delta-tiles       delta-tiles)
                     (original-map-file original-map-file)) saved-dump
      (if (not (fs:file-outdated-p saved-file
                                   (load-level:get-level-file-abs-path original-map-file)))
          (progn
            (tg:gc :full t)
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
                                                       root-compiled-shaders)))))))
          (error-message-save-game-outdated window))))
  window)
