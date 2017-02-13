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

(in-package :main-window)

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

(defparameter *placeholder* nil)

(defparameter *dt*        .0017)

(defclass test-window (identificable transformable gl-window)
  ((compiled-shaders
    :initform nil
    :accessor compiled-shaders
    :initarg :compiled-shaders)
   (shaders-dictionary
    :initform nil
    :accessor shaders-dictionary
    :initarg :shaders-dictionary)
   (game-state
    :initform (make-instance 'game-state)
    :accessor  game-state
    :initarg :game-state)
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

(defparameter *map-loaded-p* nil)

(defgeneric set-player-path (object x y))

(defun load-map (window)
  (with-accessors ((world world)
		   (compiled-shaders compiled-shaders)) window
    (load-level:load-level window world (game-state window) compiled-shaders "test.lisp")
    (setf *placeholder* (trees:gen-tree
			 (res:get-resource-file (elt *test-trees* 0)
						constants:+trees-resource+
						:if-does-not-exists :error)
			 :flatten t))
    (setf (interfaces:compiled-shaders *placeholder*) compiled-shaders)
    (setf (entity:pos *placeholder*)
	  (vec (map-utils:coord-map->chunk 1.0)
	       +zero-height+
	       (map-utils:coord-map->chunk 1.0)))
    (world:push-entity world *placeholder*)
    (camera:look-at (world:camera world)
		    *xpos* *ypos* *zpos* *xeye* *yeye* *zeye* *xup* *yup* *zup*)
    (setf (mode (world:camera world)) :fp)))

(defmethod initialize-instance :after ((object test-window) &key &allow-other-keys)
  (with-accessors ((vao vao) (compiled-shaders compiled-shaders)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (world world) (mesh mesh)
		   (game-state game-state)
		   (delta-time-elapsed delta-time-elapsed)) object
    (setf (game-state:window-id game-state) (sdl2.kit-utils:fetch-window-id object))
    (game-event:register-for-window-accept-input-event object)
    (gl:front-face :ccw)
    (gl:enable :depth-test :cull-face)
    (gl:depth-func :less)
    (gl:polygon-mode :front-and-back :fill)
    (gl:clear-color 0 0 0 1)
    (gl:clear-depth 1.0)
    (setf compiled-shaders (compile-library))
    ;; we need a valid opengl context to load spells database
    (spell:load-spell-db)
    (gui:setup-gui compiled-shaders)
    ;; set up world
    (setf world (make-instance 'world :frame-window object))
    (mtree:add-child (world:gui world) (widget:make-splash-progress-gauge))
    (setf (interfaces:compiled-shaders (world:gui world)) compiled-shaders)
    (camera:look-at (world:camera world)
		    *xpos* *ypos* *zpos* *xeye* *yeye* *zeye* *xup* *yup* *zup*)
    (setf (mode (world:camera world)) :fp)
    (camera:install-path-interpolator (world:camera world)
				      (vec 0.0  15.0 0.0)
				      (vec 64.0 30.0 0.0)
				      (vec 64.0 20.0 64.0)
				      (vec 0.0  30.0 64.0)
				      (vec 64.0  90.0 64.0))
    (camera:install-drag-interpolator (world:camera world) :spring-k +camera-drag-spring-k+)
    (camera:install-orbit-interpolator (world:camera world) 5.0 5.0 10.0)
    ;; setup projection
    (transformable:build-projection-matrix world *near* *far* *fov*
					   (num:desired (/ *window-w* *window-h*)))
    (setf delta-time-elapsed (sdl2:get-ticks))))

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
  (with-accessors ((compiled-shaders   compiled-shaders)
		   (world              world)
		   (projection-matrix  projection-matrix)
		   (cpu-time-elapsed   cpu-time-elapsed)
		   (delta-time-elapsed delta-time-elapsed)
		   (frames-count       frames-count)
		   (fps fps))                              object
    (if *map-loaded-p*
	(progn
	  (let* ((now      (sdl2:get-ticks))
		 (dt       (num:f- now delta-time-elapsed))
		 (float-dt (num:d dt)))
	    (setf delta-time-elapsed now)
	    (incf cpu-time-elapsed float-dt)
	    (do ((fdt (num:d/ float-dt 1000.0)))
		((not (num:d> fdt 0.0)))
	      (let ((actual-dt (min (/ +game-fps+ 1000) fdt)))
		(decf fdt actual-dt)
		(interfaces:calculate world (d actual-dt)))))
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

(defmethod close-window ((w test-window))
  (with-accessors ((vao vao)) w
    (unwind-protect
	 (misc:dbg "Bye!")
      (progn
	;; You MUST call-next-method.  But do it last, because everything
	;; goes away when you do (your window, gl-context, etc)!
	(interfaces:destroy (world w))
	(interfaces:destroy (compiled-shaders w))
	(texture:clean-db)
	(arrows:clean-db)
	(spell:clean-spell-db)
	(gui:clean-font-db)
	(md2:clean-db)
	(game-event:clean-all-events-vectors)
	(lparallel:end-kernel :wait t)
	(tg:gc :full t)
	(call-next-method)))))

(defmethod mousewheel-event ((object test-window) ts x y)
  (misc:dbg "wheel ~a ~a ~a" ts x y)
  (setf (mode (world:camera (world object))) :drag)
  (camera:drag-camera (world:camera (world object)) (vec .0 1.0 .0)))

(defmethod textinput-event ((object test-window) ts text)
  (if (string= text "S")
      (progn
	(game-state:setup-game-hour (game-state object)
				    (mod (1+ (game-hour (game-state object))) 24)))
      (progn
	(when (string= text "P")
	  (let ((camera (world:camera (world object))))
	    (misc:dbg "~a ~a
                       up ~a
                       ~a"
		      (entity:pos    camera)
		      (camera:target camera)
		      (entity:up     camera)
		      (entity:dir    camera))))
	(when (find text (list *forward* *back* *left* *right*
			       *upward* *downward*
			       *go-to-active-player*
			       "o" "i" "u" "d" "V" "B"
			       "n" "N" "V" "v" "U" "D"
			       "L" "p")
		    :test #'string=)
	  (when (string= text *forward*)
	    (slide-forward (world object)))
	  (when (string= text *back*)
	    (slide-back (world object)))
	  (when (string= text *left*)
	    (slide-left (world object)))
	  (when (string= text *right*)
	    (slide-right (world object)))
	  (when (string= text *upward*)
	    (slide-upward (world object)))
	  (when (string= text *downward*)
	    (slide-downward (world object)))
	  (when (string= text *go-to-active-player*)
	    (slide-to-active-player (world object)))
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
	    (world:push-entity (world object)
			       (particles:make-teleport
				(vec (map-utils:coord-map->chunk 5.0)
				     +zero-height+
	     			     (map-utils:coord-map->chunk 5.0))
				;;(vec-negate +z-axe+)
				;;10
				;; (random-elt (texture:list-of-texture-by-tag
				;; 	     texture:+texture-tag-decals-circular-wave+))
	     			(compiled-shaders object))))
	  (when (string= text "L")
	    (load-map object)
	    ;; gui
	    (setf (world:gui (world object))
		    (make-instance 'widget:widget
				   :x 0.0 :y 0.0
				   :width  *window-w*
				   :height *window-h*
				   :label nil))
	      (mtree:add-child (world:gui (world object)) (world:toolbar (world object)))
	      ;; test
	      (mtree:add-child (world:gui (world object))
	       		       (widget:make-player-generator (world object)))
	      (setf (interfaces:compiled-shaders (world:gui (world object)))
		    (compiled-shaders object))
	      (setf *map-loaded-p* t)
	      ;; testing opponents
	      (interfaces:calculate (world object) 0.0)
	      (world:add-ai-opponent (world object) :warrior :male)
	      (setf (delta-time-elapsed object) (sdl2:get-ticks))
	      ;; bg color
	      (let ((color (pixmap:skydome-bottom-color (game-hour (game-state object)))))
		(gl:clear-color (elt color 0)
				(elt color 1)
				(elt color 2)
				1.0))))
	(when (find text '("Y" "y" "X" "x") :test #'string=)
	  (when (string= text "Y")
	    (incf (elt (camera:target (world:camera (world object))) 1) 1.0))
	  (when (string= text "y")
	    (incf (elt (camera:target (world:camera (world object))) 1) -1.0))
	  (when (string= text "X")
	    (incf (elt (camera:target (world:camera (world object))) 0) 1.0))
	  (when (string= text "x")
	    (incf (elt (camera:target (world:camera (world object))) 0) -1.0)))
	(transformable:build-projection-matrix (world object) *near* *far* *fov*
					       (num:desired (/ *window-w* *window-h*)))
	(camera:look-at* (world:camera (world object)))))
  (when (string= "q" text)
    (setf *placeholder* nil)
    (close-window object)))

(defmethod keydown-event ((object test-window) ts repeat-p keysym)
  (with-accessors ((world world)) object
    (let ((gui-event (make-instance 'gui-events:key-pressed
				    :char-event (misc:code->char (sdl2:sym-value keysym)
								 :limit-to-ascii t))))
      (when (not (widget:on-key-pressed (world:gui world) gui-event))
	(let ((scancode (sdl2:scancode keysym)))
	  (when (eq :scancode-escape scancode)
	    (setf *placeholder* nil)
	    (close-window object))
	  (when (eq :scancode-f8 scancode)
	    (with-accessors ((world world) (mesh mesh)) object
	      (cl-gl-utils:with-render-to-file ((fs:file-in-package "screenshot.tga")
						*window-w* *window-h*)
		(interfaces:render world world))))
	  (when *placeholder*
	    (let* ((old-pos (entity:pos *placeholder*)))
	      (when (eq :scancode-f1 scancode)
		(misc:dbg "position ~a costs ~a, ~a cost: ~a what ~a id ~a~% approx h ~a facing ~a"
			  old-pos
			  (map-utils:coord-chunk->costs (elt old-pos 0))
			  (map-utils:coord-chunk->costs (elt old-pos 2))
			  (get-cost       (game-state object)
					  (map-utils:coord-chunk->costs (elt old-pos 0))
					  (map-utils:coord-chunk->costs (elt old-pos 2)))
			  (el-type-in-pos   (game-state object)
					    (map-utils:coord-chunk->costs (elt old-pos 0))
					    (map-utils:coord-chunk->costs (elt old-pos 2)))
			  (entity-id-in-pos (game-state object)
					    (map-utils:coord-chunk->costs (elt old-pos 0))
					    (map-utils:coord-chunk->costs (elt old-pos 2)))
			  (approx-terrain-height@pos (game-state object)
					    (elt old-pos 0)
					    (elt old-pos 2))
			  (map-utils:facing-pos old-pos (entity:dir *placeholder*))))
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
		(setf (elt (entity:pos *placeholder*) 1) height-terrain)))))))))

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
							      :path (game-state:tiles selected-path)
							      :cost (game-state:cost  selected-path)
							      :id-destination (id selected-pc))))
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

		  (t
		   (world:pick-player-entity world world x y :bind t))))
	      (when (not (widget:on-mouse-released (world:gui world) gui-event))
		(misc:dbg "~s button: ~A at ~A, ~A" state b x y))))))))

(defmethod set-player-path ((object test-window) x y)
  (with-accessors ((world world)
		   (main-state main-state)) object
    (with-accessors ((selected-pc selected-pc)
		     (main-state main-state)) world
      (let* ((player-position       (entity:pos selected-pc))
	     (cost-player-position  (ivec2 (map-utils:coord-chunk->costs (elt player-position 0))
					   (map-utils:coord-chunk->costs (elt player-position 2))))
	     (cost-pointer-position (world:pick-pointer-position world world x y))
	     (cost-pointer          (or (and cost-pointer-position
					     (game-state:get-cost main-state
								  (elt cost-pointer-position 0)
								  (elt cost-pointer-position 1)))
					+invalicable-element-cost+))
	     (ghost                 (entity:ghost selected-pc)))
	(when (and cost-pointer-position
		   (not (path-same-ends-p main-state
					  cost-player-position
					  cost-pointer-position)))
	  (let ((min-cost      (map-utils:map-manhattam-distance-cost cost-pointer-position
								      cost-player-position))
		(player-movement-points (character:current-movement-points ghost)))
	    (when (and (>= player-movement-points +open-terrain-cost+)
		       (<= min-cost player-movement-points)
		       (<= cost-pointer player-movement-points))
	      (multiple-value-bind (path cost)
		  (and cost-player-position
		       cost-pointer-position
		       (game-state:build-movement-path main-state
						       cost-player-position
						       cost-pointer-position))
		(when (and path
			   (<= cost player-movement-points))
		  (setf (game-state:selected-path main-state)
			(game-state:make-movement-path path cost))
		  (world:highlight-path-costs-space world world path))))))))))

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
		 (multiple-value-bind (w h)
		     (sdl2:get-window-size (sdl-window object))
		   (sdl2:warp-mouse-in-window (sdl-window object) (/ w 2) (/ h 2))
		   (let ((offset (vec2:vec2 (d- (d/ (desired w) 2.0) (desired x))
					    (d- (d/ (desired h) 2.0) (desired y)))))
		     (camera:reorient-fp-camera (world:camera world) offset)))))))
	  ;; no dragging
	  (with-accept-input (object)
	    (if (not selected-pc)
		(world:highlight-tile-screenspace world world x y)
		(cond
		  ((and (< (vec2:vec2-length (vec2:vec2 (d xr) (d yr))) 2)
			(eq (world:toolbar-selected-action world) widget:+action-move+))
		   (set-player-path object x y))
		  ((eq (world:toolbar-selected-action world) widget:+action-attack-short-range+)
		   (world:highlight-tile-screenspace world world x y)))))))))

(defmethod game-event:on-game-event ((object test-window)
				     (event game-event:window-accept-input-event))
  (setf (accept-input-p object) (game-event:accept-input-p event)))

(defmethod other-event ((object test-window) event)
  (misc:dbg "other ~a" event))

(defun main ()
  (tg:gc :full t)
  (handler-bind ((error
		  #'(lambda(e)
		      (declare (ignore e))
		      (invoke-restart 'cl-i18n:return-empty-translation-table))))
    (setf cl-i18n:*translation-file-root* +catalog-dir+)
    (cl-i18n:load-language +text-domain+ :locale (cl-i18n:find-locale)))
  (setf *workers-number* (if (> (os-utils:cpu-number) 1)
			     (os-utils:cpu-number)
			     1))
  (setf lparallel:*kernel* (lparallel:make-kernel *workers-number*))
  (setf identificable:*entity-id-counter* +start-id-counter+)
  (player-messages-text:init-player-messages-db)
  (setf *map-loaded-p* nil)
  (start)
  (sdl2:gl-set-attr :context-profile-mask  1)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (let ((w (make-instance 'test-window :w *window-w* :h *window-h* :title +program-name+)))
    (multiple-value-bind (wi he)
	(sdl2:get-window-size (sdl-window w))
      (sdl2:warp-mouse-in-window (sdl-window w) (/ wi 2) (/ he 2)))
    (setf (idle-render w) t)
    (tg:gc :full t)))
