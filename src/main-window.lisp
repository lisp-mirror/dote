;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along      with      this      program.       If      not,      see
;; <http://www.gnu.org/licenses/>.

(in-package :main-window)

(defparameter *far*    440.0)

(defparameter *near*     5.0)

(defparameter *fov*     50.0)

(defparameter *placeholder* nil)

(defparameter *dt*        .0017)

(defclass test-window (identificable transformable gl-window)
  ((root-compiled-shaders
    :initform nil
    :accessor root-compiled-shaders
    :initarg :root-compiled-shaders)
   (window-game-state
    :initform (make-instance 'game-state)
    :accessor  window-game-state
    :initarg :window-game-state)
   (world
    :initform nil
    :accessor world
    :initarg :world)
   (accept-input-p
    :initform t
    :accessor accept-input-p
    :initarg :accept-input-p)
   (cpu-time-elapsed
    :initform 1
    :accessor cpu-time-elapsed
    :initarg  :cpu-time-elapsed)
   (delta-time-elapsed
    :initform (sdl2:get-ticks)
    :accessor delta-time-elapsed
    :initarg  :delta-time-elapsed)
   (frames-count
    :initform 0
    :accessor frames-count
    :initarg  :frames-count)
   (fps
    :initform 0
    :accessor fps
    :initarg  :fps)))

(defmacro with-gl-context (&body body)
  `(progn
     (call-next-method)
     ,@body))

(defmacro with-accept-input ((window) &body body)
  `(when (accept-input-p ,window)
     ,@body))

(defgeneric keydown-event (object ts repeat-p keysym))

(defparameter *test-trees* '("test.lsys"                  ;  0
                             "tropical/palm.lsys"         ;  1
                             "tropical/palm-2.lsys"       ;  2
                             "general/dead-tree.lsys"     ;  3
                             "general/dead-tree-2.lsys"   ;  4
                             "tropical/eucalyptus.lsys"   ;  5
                             "temperate/oak.lsys"         ;  6
                             "temperate/cypress.lsys"     ;  7
                             "temperate/fir.lsys"         ;  8
                             "temperate/fir-2.lsys"       ;  9
                             "temperate/willow.lsys"      ; 10
                             "temperate/fern.lsys"        ; 11
                             "temperate/lemon.lsys"       ; 12
                             "general/dead-tree-3.lsys")) ; 13

(defgeneric set-player-path (object x y timestamp))

(defun load-map (window)
  (with-accessors ((world world)
                   (root-compiled-shaders root-compiled-shaders)) window
    (saved-game::prepare-for-map-loading window)
    (saved-game:load-map window "test.lisp" (truncate +maximum-level-difficult+))
    (saved-game:init-new-map window (truncate +maximum-level-difficult+))
    #+debug-mode
    (progn
      (setf *placeholder* (trees:gen-tree
                           (res:get-resource-file (elt *test-trees* 0)
                                                  constants:+trees-resource+
                                                  :if-does-not-exists :error)
                           :flatten t))
      (setf (interfaces:compiled-shaders *placeholder*) root-compiled-shaders)
      (setf (entity:pos *placeholder*)
            (vec (map-utils:coord-map->chunk 1.0)
                 +zero-height+
                 (map-utils:coord-map->chunk 1.0)))
      (world:push-entity world *placeholder*))))

(defmethod initialize-instance :after ((object test-window) &key &allow-other-keys)
  (with-accessors ((root-compiled-shaders root-compiled-shaders)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (world world) (mesh mesh)
                   (window-game-state window-game-state)
                   (delta-time-elapsed delta-time-elapsed)) object
    (sound:init-sound-system (gconf:config-sound-volume))
    (saved-game:init-system-when-gl-context-active object)
    (mtree:add-child (world:gui world)
                     (op-seq:make-opening world))))

(defmacro with-gui ((world) &body body)
  (alexandria:with-gensyms (3d-projection-matrix 3d-view-matrix)
    `(let ((,3d-projection-matrix (elt (projection-matrix (camera ,world)) 0))
           (,3d-view-matrix       (elt (view-matrix       (camera ,world)) 0)))
       (unwind-protect
            (progn
              (gl:disable :depth-test)
              (gl:depth-mask :nil)
              (setf (projection-matrix ,world)
                    (ortho 0.0 (num:d *window-w*) 0.0 (num:d *window-h*) -1.0 1.0))
              (setf (view-matrix (camera ,world)) (identity-matrix))
              ,@body)
         (gl:depth-mask t)
         (gl:enable :depth-test)
         (setf (projection-matrix ,world)    ,3d-projection-matrix
               (view-matrix (camera ,world)) ,3d-view-matrix)))))

(defmethod render ((object test-window))
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (with-accessors ((root-compiled-shaders   root-compiled-shaders)
                   (world              world)
                   (projection-matrix  projection-matrix)
                   (cpu-time-elapsed   cpu-time-elapsed)
                   (delta-time-elapsed delta-time-elapsed)
                   (frames-count       frames-count)
                   (fps fps))                              object
    (if (or (world:opening-mode world)
            saved-game:*map-loaded-p*)
        (let* ((now      (sdl2:get-ticks))
               (dt       (num:f- now delta-time-elapsed))
               (float-dt (num:d dt)))
          (setf delta-time-elapsed now)
          (incf cpu-time-elapsed float-dt)
          (do ((fdt (num:d/ float-dt 1000.0)))
              ((not (num:d> fdt 0.0)))
            (let ((actual-dt (min (/ 1.0 +game-fps+) fdt)))
              (decf fdt actual-dt)
              (interfaces:calculate world (d actual-dt))))
          ;; rendering
          (gl:clear :color-buffer :depth-buffer)
          (interfaces:render world world)
          ;; gui
          (with-gui (world)
            (world:render-gui world))
          (incf frames-count)
          (setf fps (num:d/ (num:d frames-count)
                            (num:d/ (num:d cpu-time-elapsed) 1000.0))))
        (progn
          ;; rendering
          (gl:clear-color 0 0 0 1)
          (gl:clear :color-buffer :depth-buffer)
          (with-gui (world)
            (world:render-gui world))))))

(defun clean-up-placeholder ()
  (when *placeholder*
    (interfaces:destroy *placeholder*)
    (setf *placeholder* nil)))

(defmethod close-window ((w test-window))
  (unwind-protect
       (misc:dbg "Bye!")
    (progn
      ;; You MUST call-next-method.  But do it last, because everything
      ;; goes away when you do (your window, gl-context, etc)!
      (saved-game:clean-up-system w)
      (saved-game:clean-parallel-kernel)
      (sound:close-sound-system)
      #+debug-mode (clean-up-placeholder)
      (tg:gc :full t)
      (call-next-method))))

(defmethod mousewheel-event ((object test-window) ts x y)
  #+debug-mode (misc:dbg "wheel ~a ~a ~a" ts x y)
  (setf (mode (world:camera (world object))) :drag)
  (let* ((sign   (num:d (num:sign y)))
         (offset (d* sign (d/ +gui-zoom-entity+ 4.0))))
    (camera:drag-camera (world:camera (world object)) (vec .0 offset .0))))

(defmethod textinput-event ((object test-window) ts text)
  (with-accessors ((world world)) object
    (with-accessors ((selected-pc selected-pc)) world
    (if (string= text "S")
        (progn
          (game-state:setup-game-hour (window-game-state object)
                                      (mod (1+ (game-hour (window-game-state object))) 24)))
        (progn
          (when (string= text "P")
            (let ((camera (world:camera world)))
              (misc:dbg "~a ~a
                       up ~a
                       ~a"
                        (entity:pos    camera)
                        (camera:target camera)
                        (entity:up     camera)
                        (entity:dir    camera))))
          (when (find text (list (gconf:config-forward)
                                 (gconf:config-back) (gconf:config-left) (gconf:config-right)
                                 (gconf:config-upward) (gconf:config-downward)
                                 (gconf:config-go-to-active-character)
                                 (gconf:config-rotate-camera-cw)
                                 (gconf:config-rotate-camera-ccw)
                                 (gconf:config-reset-camera)
                                 (gconf:config-next-character)
                                 (gconf:config-prev-character)
                                 "o" "i" "u" "d" "V" "B"
                                 "n" "N" "V" "v" "U" "D"
                                 "L" "p")
                      :test #'string=)
            (when (string= text (gconf:config-forward))
              (slide-forward world))
            (when (string= text (gconf:config-back))
              (slide-back world))
            (when (string= text (gconf:config-left))
              (slide-left world))
            (when (string= text (gconf:config-right))
              (slide-right world))
            (when (string= text (gconf:config-upward))
              (slide-upward world))
            (when (string= text (gconf:config-downward))
              (slide-downward world))
            (when (string= text (gconf:config-go-to-active-character))
              (slide-to-active-player world))
            (when (string= text (gconf:config-rotate-camera-cw))
              (rotate-90-around-player-cw world))
            (when (string= text (gconf:config-rotate-camera-ccw))
              (rotate-90-around-player-ccw world))
            (when (string= text (gconf:config-reset-camera))
              (reset-camera world))
            (when (string= text (gconf:config-next-character))
              (select-next-player world))
            (when (string= text (gconf:config-prev-character))
              (select-previous-player world))
            ;; test
            (when (string= text "v")
              (incf *fov* +1.0))
            (when (string= text "V")
              (incf *fov* -1.0))
            (when (string= text "o")
              (misc:dbg "f ~a" *far*)
              (incf *far* +1.0))
            (when (string= text "B")
              (misc:dbg "f ~a" *far*)
              (incf *far* -1.0))
            (when (string= text "n")
              (misc:dbg "n ~a" *near*)
              (incf *near* +.1))
            (when (string= text "N")
              (misc:dbg "n ~a" *near*)
              (incf *near* -.1))
            (when (string= text "p")
              ;;(closing-sequence:start-victory-sequence world)
              (let ((target (first (player-entities (window-game-state object))))
                    (x      6)
                    (y      17))
                (misc:dbg "placeholder see ~a? ~a ~a" target
                          (absee-mesh:placeholder-visible-p target x y)
                          (absee-mesh:placeholder-able-to-see-p x y target))))
            (when (string= text "D")
              (world:apply-tremor-0 world))
            (when (string= text "L")
              (setf (world:opening-mode world) nil)
              (load-map object)))
          (when (find text '("Y" "y" "X" "x") :test #'string=)
            (when (string= text "Y")
              (incf (elt (camera:target (world:camera world)) 1) 1.0))
            (when (string= text "y")
              (incf (elt (camera:target (world:camera world)) 1) -1.0))
            (when (string= text "X")
              (incf (elt (camera:target (world:camera world)) 0) 1.0))
            (when (string= text "x")
              (incf (elt (camera:target (world:camera world)) 0) -1.0)))
          (transformable:build-projection-matrix world *near* *far* *fov*
                                                 (num:desired (/ *window-w* *window-h*)))
          (camera:look-at* (world:camera world)))))))

#+(and debug-mode debug-ai)
(defun %change-ai-layer (window scancode)
  (flet ((%change-layer (type)
           (setf (world:influence-map-type (world window)) type)))
    (when (eq :scancode-3 scancode)
      (%change-layer :influence-map))
    (when (eq :scancode-4 scancode)
      (%change-layer :concerning-invalicables))
    (when (eq :scancode-5 scancode)
      (%change-layer :concerning-facing))
    (when (eq :scancode-6 scancode)
      (%change-layer :smoothed-concerning-layer))
    (when (eq :scancode-7 scancode)
      (%change-layer :concerning-layer))
    (when (eq :scancode-8 scancode)
      (%change-layer :unexplored-layer))
    (when (eq :scancode-9 scancode)
      (%change-layer :attack-enemy-melee-layer))
    (when (eq :scancode-0 scancode)
      (%change-layer :attack-enemy-pole-layer))
    (when (eq :scancode-minus scancode)
      (%change-layer :attack-enemy-bow-layer))
    (when (eq :scancode-equals scancode)
      (%change-layer :attack-enemy-crossbow-layer))))

(defun skip-opening (world)
  (mtree:top-down-visit (world:gui world)
                        #'(lambda (n)
                            (when (typep n 'interfaces:inner-animation)
                                (setf (interfaces:animation-speed n)
                                      (d* 500.0 (interfaces:animation-speed n)))))))

(defmethod keydown-event ((object test-window) ts repeat-p keysym)
  (with-accessors ((world world)) object
    (let ((gui-event (make-instance 'gui-events:key-pressed
                                    :char-event (misc:code->char (sdl2:sym-value keysym)
                                                                 :limit-to-ascii t))))
      (when (not (widget:on-key-pressed (world:gui world) gui-event))
        (let ((scancode (sdl2:scancode keysym)))
          #+(and debug-mode debug-ai)   (%change-ai-layer object scancode)
          (when (eq :scancode-escape scancode)
            (when (world:opening-mode world)
              (skip-opening world))
            #+debug-mode (close-window object))
          (when (eq :scancode-f1 scancode)
            (mesh:flip-tree-clip))
          (when (eq :scancode-f2 scancode)
            (make-screenshot world)
            #+ (and debug-mode debug-ai)
            (pixmap:save-pixmap (world:type-influence->pixmap world)
                                (fs:file-in-package "ai-layer.tga")))
          #+debug-mode
          (progn
            (when (eq :scancode-4 scancode)
            (with-accessors ((world world) (mesh mesh)) object
              (let* ((game-state         (window-game-state object)))
                (with-accessors ((selected-pc selected-pc)) world
                  (let ((pos (mesh:calculate-cost-position selected-pc)))
                    (misc:dbg "cost ~a" pos)
                    (misc:dbg "w conc ~a"
                              (game-state:build-movement-path-pc
                               game-state
                               pos
                               (ivec2 16 16)
                               :heuristic-cost-function
                               (game-state:heuristic-alt-pc game-state))))))))
            (when (eq :scancode-3 scancode)
              (with-accessors ((world world) (mesh mesh)) object
                (let* ((game-state         (window-game-state object))
                       (blackboard         (blackboard game-state)))
                  (with-accessors ((selected-pc selected-pc)) world
                    (let ((pos (mesh:calculate-cost-position selected-pc)))
                      (misc:dbg "w conc ~a"
                                (blackboard:path-with-concerning-tiles blackboard
                                                                       pos
                                                                       (ivec2 3 1)))
                      (misc:dbg "w conc ~a"
                                (game-state:build-movement-path-pc
                                 (main-state world)
                                 pos
                                 (ivec2 16 16)
                                 :heuristic-cost-function
                                 (heuristic-alt-pc game-state))))))))
            (when *placeholder*
              (let* ((old-pos (entity:pos *placeholder*)))
                (when (eq :scancode-1 scancode)
                  (let* ((window-game-state  (window-game-state object))
                         (blackboard         (blackboard window-game-state))
                         (concerning-matrix  (blackboard:concerning-tiles->costs-matrix blackboard))
                         (x-cost            (map-utils:coord-chunk->costs (elt old-pos 0)))
                         (y-cost            (map-utils:coord-chunk->costs (elt old-pos 2))))
                    (misc:dbg "position ~a costs ~a, ~a cost: ~a what ~a id ~a~%
approx h ~a facing ~a occlude? ~a inside-room ~a concerning cost ~a ai-entitites ~a"
                              old-pos
                              (map-utils:coord-chunk->costs (elt old-pos 0))
                              (map-utils:coord-chunk->costs (elt old-pos 2))
                              (get-cost       (window-game-state object)
                                              (map-utils:coord-chunk->costs (elt old-pos 0))
                                              (map-utils:coord-chunk->costs (elt old-pos 2)))
                              (el-type-in-pos   (window-game-state object)
                                                (map-utils:coord-chunk->costs (elt old-pos 0))
                                                (map-utils:coord-chunk->costs (elt old-pos 2)))
                              (entity-id-in-pos (window-game-state object)
                                                (map-utils:coord-chunk->costs (elt old-pos 0))
                                                (map-utils:coord-chunk->costs (elt old-pos 2)))
                              (approx-terrain-height@pos (window-game-state object)
                                                         (elt old-pos 0)
                                                         (elt old-pos 2))
                              (map-utils:facing-pos old-pos (entity:dir *placeholder*))
                              (game-state:occludep-in-pos (window-game-state object)
                                                          (map-utils:coord-chunk->costs (elt old-pos
                                                                                             0))
                                                          (map-utils:coord-chunk->costs (elt old-pos
                                                                                             2)))
                              (mesh:inside-room-p *placeholder*)
                              (matrix:matrix-elt concerning-matrix y-cost x-cost)
                              (game-state:ai-entities window-game-state))))
                (when (and (eq :scancode-up scancode))
                  (setf (entity:pos *placeholder*)
                        (vec (elt old-pos 0)
                             0.0
                             (num:d+ (elt old-pos 2) +terrain-chunk-tile-size+)))
                  (world:move-entity world *placeholder* nil :update-costs nil))
                (when (eq :scancode-down scancode)
                  (setf (entity:pos *placeholder*)
                        (vec (elt old-pos 0)
                             0.0
                             (num:d- (elt old-pos 2) +terrain-chunk-tile-size+)))
                  (world:move-entity world *placeholder* nil :update-costs nil))
                (when (eq :scancode-left scancode)
                  (setf (entity:pos *placeholder*)
                        (vec (num:d+ (elt old-pos 0) +terrain-chunk-tile-size+)
                             0.0
                             (elt old-pos 2)))
                  (world:move-entity world *placeholder* nil :update-costs nil))
                (when (eq :scancode-right scancode)
                  (setf (entity:pos *placeholder*)
                        (vec (num:d- (elt old-pos 0) +terrain-chunk-tile-size+)
                             0.0
                             (elt old-pos 2)))
                  (world:move-entity world *placeholder* nil :update-costs nil)))
              (let ((height-terrain (world:pick-height-terrain world
                                                               (elt (entity:pos *placeholder*) 0)
                                                               (elt (entity:pos *placeholder*) 2))))
                (when height-terrain
                  (setf (elt (entity:pos *placeholder*) 1) height-terrain))))))))))

(defmethod keyboard-event ((object test-window) state ts repeat-p keysym)
  (when (eq state :keydown)
    (keydown-event object ts repeat-p keysym)))

(defmethod mousebutton-event ((object test-window) state ts b x y)
  (with-accessors ((world world)) object
    (with-accessors ((selected-pc selected-pc)
                     (main-state main-state)) world
      (with-accept-input (object)
        (let ((gui-event (make-instance 'gui-events:mouse-pressed
                                        :button-event (gui-events:mouse-button->code b)
                                        :x-event (num:d x)
                                        :y-event (num:d (- *window-h* y)))))
          (if (eq state :mousebuttondown)
              (when (not (widget:on-mouse-pressed (world:gui world) gui-event))
                (cond
                  ((eq (world:toolbar-selected-action world)
                       widget:+action-move+)
                   (when (not (world:pick-player-entity world world x y :bind t))
                     (let* ((selected-path (game-state:selected-path main-state)))
                       (when selected-path
                         (let ((movement-event (make-instance 'game-event:move-entity-along-path-event
                                                              :path
                                                              (game-state:tiles selected-path)
                                                              :cost
                                                              (game-state:cost  selected-path)
                                                              :id-destination
                                                              (id selected-pc))))
                           (game-event:propagate-move-entity-along-path-event movement-event)
                           (world:reset-toolbar-selected-action world))))))
                  ((eq (world:toolbar-selected-action world)
                       widget:+action-attack-short-range+)
                   (let ((attacked (world:pick-any-entity world world x y)))
                     (battle-utils:attack-short-range world selected-pc attacked)))
                  ((eq (world:toolbar-selected-action world)
                       widget:+action-launch-spell+)
                   (when (character:spell-loaded (entity:ghost selected-pc))
                     (let ((attacked (world:pick-any-entity world world x y))
                           (spell    (character:spell-loaded (entity:ghost selected-pc))))
                       (if (spell:attack-spell-p spell)
                           (battle-utils:attack-launch-spell world selected-pc attacked)
                           (battle-utils:launch-spell world selected-pc attacked)))))
                  ((eq (world:toolbar-selected-action world)
                       widget:+action-attack-long-range+)
                   (let ((attacked (world:pick-any-entity world world x y)))
                     (when attacked
                       (battle-utils:attack-long-range world selected-pc attacked))))
                  ((eq (world:toolbar-selected-action world)
                       widget:+action-attack-long-range-imprecise+)
                   (let ((attacked (world:pick-any-entity world world x y)))
                     (when attacked
                       (battle-utils:attack-long-range-imprecise world selected-pc attacked))))
                  ((world:human-interaction-allowed-p world)
                   (world:pick-player-entity world world x y :bind t))))
              (when (not (widget:on-mouse-released (world:gui world) gui-event))
                (misc:dbg "~s button: ~A at ~A, ~A" state b x y))))))))

(let ((old-tile-position nil)
      (old-timestamp     0))
  (defmethod set-player-path ((object test-window) x y timestamp)
    (when (> (/ (- timestamp old-timestamp) 1000.0)
             1/30)
      (setf old-timestamp timestamp)
      (with-accessors ((world world)
                       (main-state main-state)) object
        (with-accessors ((selected-pc selected-pc)
                         (main-state main-state)) world
          (alexandria:when-let* ((player-position       (entity:pos selected-pc))
                                 (cost-player-position  (entity:calculate-cost-position selected-pc))
                                 (cost-pointer-position (world:pick-pointer-position world world x y))
                                 (cost-pointer-pos-x    (elt cost-pointer-position 0))
                                 (cost-pointer-pos-y    (elt cost-pointer-position 1))
                                 (cost-pointer          (or (get-cost main-state
                                                                      cost-pointer-pos-x
                                                                      cost-pointer-pos-y))
                                                        +invalicable-element-cost+)
                                 (ghost                 (entity:ghost selected-pc)))
            (when (or (null old-tile-position)
                      (not (ivec2= old-tile-position cost-pointer-position)))
              (setf old-tile-position cost-pointer-position)
              (when (not (path-same-ends-p main-state
                                           cost-player-position
                                           cost-pointer-position))
                (let ((min-cost      (map-utils:map-manhattam-distance-cost cost-pointer-position
                                                                            cost-player-position))
                      (player-movement-points (character:current-movement-points ghost)))
                  (when (and (>= player-movement-points +open-terrain-cost+)
                             (<= min-cost player-movement-points)
                             (<= cost-pointer player-movement-points)
                             (not (position-confined-in-labyrinth-p main-state
                                                                    cost-pointer-position)))
                    (multiple-value-bind (path cost)
                        (game-state:build-movement-path-pc main-state
                                                           cost-player-position
                                                           cost-pointer-position
                                                           :heuristic-cost-function
                                                           (heuristic-alt-pc main-state))
                      (when (and path
                                 (<= cost player-movement-points))
                        (setf (game-state:selected-path main-state)
                              (game-state:make-movement-path path cost))
                        (world:highlight-path-costs-space world world path)))))))))))))

(defmethod mousemotion-event ((object test-window) ts mask x y xr yr)
  (with-accessors ((world world)) object
    (with-accessors ((selected-pc selected-pc)) world
      ;; dragging
      (if (> mask 0)
          (let ((gui-event (make-instance 'gui-events:mouse-dragged
                                          :x-event  (d x)
                                          :y-event  (d (- *window-h* y))
                                          :dx-event (d xr)
                                          :dy-event (d- (d yr)))))
            (when (not (widget:on-mouse-dragged (world:gui world) gui-event))
              (cond
                ((eq (camera:mode (world:camera world)) :fp)
                 (let ((offset (vec2* (vec2-negate (vec2 (d xr) (d yr)))
                                      (gconf:config-camera-fp-scaling-movement))))
                   (camera:reorient-fp-camera (world:camera world) offset))))))
          ;; no dragging
          (with-accept-input (object)
            (if (not selected-pc)
                (world:highlight-tile-screenspace world world x y)
                (cond
                  ((eq (world:toolbar-selected-action world) widget:+action-move+)
                   (set-player-path object x y ts))
                  ((eq (world:toolbar-selected-action world) widget:+action-attack-short-range+)
                   (world:highlight-tile-screenspace world world x y)))))))))

(defmethod game-event:on-game-event ((object test-window)
                                     (event game-event:window-accept-input-event))
  (setf (accept-input-p object) (game-event:accept-input-p event)))

(defmethod other-event ((object test-window) event)
  (misc:dbg "other ~a" event))

#+ debug-mode
(defun main-debug ()
  (sdl2.kit:init)
  (saved-game:init-system)
  (let ((w (make-instance 'test-window
                          :fullscreen nil
                          :w          *window-w*
                          :h          *window-h*
                          :title      +program-name+)))
    (setf (idle-render w) t)
    (when (gconf:config-fullscreen)
      (sdl2.kit-utils:go-fullscreen w))
    (sdl2.kit-utils:move-mouse-to-center-screen w)
    (tg:gc :full t))
  (sdl2.kit:start))

(define-start-function %main ()
  (saved-game:init-system)
  (let ((w (make-instance 'test-window
                          :fullscreen nil
                          :w          *window-w*
                          :h          *window-h*
                          :title      +program-name+)))
    (setf (idle-render w) t)
    (when (gconf:config-fullscreen)
      (sdl2.kit-utils:go-fullscreen w))
    (sdl2.kit-utils:move-mouse-to-center-screen w)
    (tg:gc :full t)))

(defun main ()
  (sdl2:make-this-thread-main #'%main))
