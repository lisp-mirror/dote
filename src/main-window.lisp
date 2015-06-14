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

;; (defparameter *xpos*    0.0)

;; (defparameter *ypos*   42.0)

;; (defparameter *zpos*    0.0)

(defparameter *xpos*    128.0)

(defparameter *ypos*     42.0)

(defparameter *zpos*     128.0)

(defparameter *xeye*   32.0)

(defparameter *yeye*    0.0)

(defparameter *zeye*   32.0)

(defparameter *far*   440.0)

(defparameter *near*    5.0)

(defparameter *fov*    50.0)

(defparameter *placeholder* nil)

(defparameter *dt* .0017)

(defclass test-window (transformable gl-window)
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
   (cpu-time-elapsed
    :initform 1
    :accessor cpu-time-elapsed
    :initarg  :cpu-time-elapsed)
   (delta-time-elapsed
    :initform 0
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

(defmethod initialize-instance ((object test-window) &key &allow-other-keys)
  (with-accessors ((vao vao) (compiled-shaders compiled-shaders)
		   (projection-matrix projection-matrix)
		   (model-matrix model-matrix)
		   (view-matrix view-matrix)
		   (world world) (mesh mesh)
		   (delta-time-elapsed delta-time-elapsed)) object
    (with-gl-context ; this call is needed to set up a opengl context
      (gl:front-face :ccw)
      (gl:enable :depth-test :cull-face)
      (gl:depth-func :less)
      (gl:polygon-mode :front-and-back :fill)
      (gl:clear-color 0.039215688 0.17254902 0.08235294 1.0)
      (gl:clear-color 1 0 1 1)
      (gl:clear-depth 1.0)
      (setf compiled-shaders (compile-library))
      (gui:setup-gui compiled-shaders)
      ;; set up world
      (setf world (make-instance 'world :frame-window object))
      (setf (interfaces:compiled-shaders (world::gui world)) compiled-shaders)
      (load-level:load-level world (game-state object) compiled-shaders "test.lisp")
      ;; setup camera
      (camera:look-at (world:camera (world object))
		      *xpos* *ypos* *zpos* *xeye* *yeye* *zeye* 0.0 1.0 0.0)
      (setf (mode (world:camera (world object))) :fp)
      (camera:install-path-interpolator (world:camera (world object))
					(vec  0.0  15.0 0.0)
					(vec  64.0 30.0 0.0)
					(vec  64.0 20.0 64.0)
					(vec  0.0  30.0 64.0)
					(vec  64.0  90.0 64.0))
      (camera:install-drag-interpolator (world:camera (world object)))
      (camera:install-orbit-interpolator (world:camera (world object)) 5.0 5.0 10.0)
      ;; setup projection
      (transformable:build-projection-matrix (world object) *near* *far* *fov*
					     (num:desired (/ *window-w* *window-h*)))
      (setf *placeholder* (trees:gen-tree
			   (res:get-resource-file (elt *test-trees* 0)
						  constants:+trees-resource+
						  :if-does-not-exists :error)
			   :flatten t))
      (setf (interfaces:compiled-shaders *placeholder*) compiled-shaders)
      (setf (entity:pos *placeholder*)
	    (vec (misc:coord-map->chunk 1.0)
		 +zero-height+
		 (misc:coord-map->chunk 1.0)))
      (world:push-entity world *placeholder*)
      (let ((body (md2:load-md2-model "ortnok/"
				      :mesh-file "body01.md2"
				      :animation-file "body-animation.lisp"
				      :texture-file   "body-texture.tga"
				      :tags-file      "body01.tag"))
	    (head (md2:load-md2-model "ortnok/"
				      :mesh-file "head01.md2"
				      :animation-file "head-animation.lisp"
				      :texture-file   "head-texture.tga"
				      :tags-file      nil)))
	(setf (interfaces:compiled-shaders body) compiled-shaders
	      (interfaces:compiled-shaders head) compiled-shaders)
	(md2:set-animation body :move)
	(md2:set-animation head :stand)
	(setf (md2:tag-key-parent head) md2:+tag-head-key+)
	(mtree-utils:add-child body head)
	(world:push-entity world body)
	(setf delta-time-elapsed (sdl2:get-ticks))))))

(defmacro with-gui ((world) &body body)
  (alexandria:with-gensyms (3d-projection-matrix 3d-view-matrix)
    `(let ((,3d-projection-matrix (elt (projection-matrix (camera ,world)) 0))
	   (,3d-view-matrix       (elt (view-matrix       (camera ,world)) 0)))
       (unwind-protect
	    (progn
	      (gl:disable :depth-test)
	      (gl:depth-mask :false)
	      (setf (projection-matrix ,world)
		    (ortho 0.0 (num:d *window-w*) 0.0 (num:d *window-h*) -1.0 1.0))
	      (setf (view-matrix (camera ,world)) (identity-matrix))
	      ,@body)
	 (gl:depth-mask :true)
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
    (let* ((now      (sdl2:get-ticks))
	   (dt       (num:f- now delta-time-elapsed))
	   (float-dt (num:d dt)))
      (setf delta-time-elapsed now)
      (incf cpu-time-elapsed float-dt)
      (do ((fdt (num:d/ float-dt 1000.0)))
	  ((not (num:d> fdt 0.0)))
	(let ((actual-dt (min +game-fps+ fdt)))
	  (decf fdt actual-dt)
	  (world::calculate world actual-dt))))
    ;; rendering
    (gl:clear :color-buffer)
    (gl:clear :depth-buffer)
    (interfaces:render world world)
    ;; gui
    (with-gui (world)
      (world:render-gui world))
    (incf frames-count)
    (setf fps (num:d/ (num:d frames-count) (num:d/ (num:d cpu-time-elapsed) 1000.0)))))

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
	(gui:clean-font-db)
	(lparallel:end-kernel :wait t)
	(tg:gc :full t)
	(call-next-method)))))

(defmethod mousewheel-event ((object test-window) ts x y)
  (misc:dbg "wheel ~a ~a ~a" ts x y)
  (camera::drag-camera (world:camera (world object)) (vec2:vec2 1.0 1.0)))

(defmethod textinput-event ((object test-window) ts text)
  (if (string= text "s")
      (progn
	(game-state:setup-game-hour (game-state object)
				    (mod (1+ (game-hour (game-state object))) 24)))
      (progn
	(when (string= text "P")
	  (let ((camera (world:camera (world object))))
	    (misc:dbg "~a ~a
                       up ~a
                       ~a"
		      (camera::pos    camera)
		      (camera:target camera)
		      (camera::up     camera)
		      (camera::dir    camera))))
	(when (find text '("o" "i" "r" "l" "u" "d" "f" "F" "n" "N" "V" "v") :test #'string=)
	  (when (string-equal text "o")
	    (incf (elt (entity:pos (world:camera (world object))) 2) 1.0))
	  (when (string-equal text "i")
	    (incf (elt (entity:pos (world:camera (world object))) 2) -1.0))
	  (when (string= text "r")
	    (incf (elt (entity:pos (world:camera (world object))) 0) 1.0))
	  (when (string= text "l")
	    (incf (elt (entity:pos (world:camera (world object))) 0) -1.0))
	  (when (string-equal text "u")
	    (misc:dbg "~a" (elt (entity:pos (world:camera (world object))) 1))
	    (incf (elt (entity:pos (world:camera (world object))) 1) 1.0))
	  (when (string-equal text "d")
	    (incf (elt (entity:pos (world:camera (world object))) 1) -1.0))
	  (when (string= text "v")
	    (incf *fov* +1.0))
	  (when (string= text "V")
	    (incf *fov* -1.0))
	  (when (string= text "f")
	    (misc:dbg "f ~a" *far*)
	    (incf *far* +1.0))
	  (when (string= text "F")
	    (misc:dbg "f ~a" *far*)
	    (incf *far* -1.0))
	  (when (string= text "n")
	    (misc:dbg "n ~a" *near*)
	    (incf *near* +.1))
	  (when (string= text "N")
	    (misc:dbg "n ~a" *near*)
	    (incf *near* -.1)))
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
	    (let ((old-pos (entity:pos *placeholder*)))
	      (when (eq :scancode-f1 scancode)
		(misc:dbg "position ~a costs ~a, ~a cost: ~a what ~a id ~a"
			  old-pos
			  (misc:coord-chunk->costs (elt old-pos 0))
			  (misc:coord-chunk->costs (elt old-pos 2))
			  (get-cost       (game-state object)
					  (misc:coord-chunk->costs (elt old-pos 0))
					  (misc:coord-chunk->costs (elt old-pos 2)))
			  (el-type-in-pos   (game-state object)
					    (misc:coord-chunk->costs (elt old-pos 0))
					    (misc:coord-chunk->costs (elt old-pos 2)))
			  (entity-id-in-pos (game-state object)
					    (misc:coord-chunk->costs (elt old-pos 0))
					    (misc:coord-chunk->costs (elt old-pos 2)))))
	      (when (and (eq :scancode-up scancode))
		(setf (entity:pos *placeholder*)
		      (vec (elt old-pos 0)
			   (elt old-pos 1)
			   (num:d+ (elt old-pos 2) +terrain-chunk-tile-size+))))
	      (when (eq :scancode-down scancode)
		(setf (entity:pos *placeholder*)
		      (vec (elt old-pos 0)
			   (elt old-pos 1)
			   (num:d- (elt old-pos 2) +terrain-chunk-tile-size+))))
	      (when (eq :scancode-left scancode)
		(setf (entity:pos *placeholder*)
		      (vec (num:d+ (elt old-pos 0) +terrain-chunk-tile-size+)
			   (elt old-pos 1)
			   (elt old-pos 2))))
	      (when (eq :scancode-right scancode)
		(setf (entity:pos *placeholder*)
		      (vec (num:d- (elt old-pos 0) +terrain-chunk-tile-size+)
			   (elt old-pos 1)
			   (elt old-pos 2)))))))))))

(defmethod keyboard-event ((object test-window) state ts repeat-p keysym)
  (when (eq state :keydown)
    (keydown-event object ts repeat-p keysym)))

(defmethod mousebutton-event ((object test-window) state ts b x y)
  (with-accessors ((world world)) object
    (let ((gui-event (make-instance 'gui-events:mouse-pressed
				    :button-event (gui-events:mouse-button->code b)
				    :x-event (num:d x)
				    :y-event (num:d (- *window-h* y)))))
      (if (eq state :mousebuttondown)
	  (when (not (widget:on-mouse-pressed (world:gui world) gui-event))
	    (misc:dbg "~s button: ~A at ~A, ~A" state b x y))
	  (when (not (widget:on-mouse-released (world:gui world) gui-event))
	    (misc:dbg "~s button: ~A at ~A, ~A" state b x y))))))

(defmethod mousemotion-event ((object test-window) ts mask x y xr yr)
  (with-accessors ((world world)) object
    (if (> mask 0) ;; dragging
	(let ((gui-event (make-instance 'gui-events:mouse-dragged
					:x-event  (num:d x)
					:y-event  (num:d (- *window-h* y))
					:dx-event (num:d xr)
					:dy-event (num:d- (num:d yr)))))
	  (when (not (widget:on-mouse-dragged (world:gui world) gui-event))
	    (cond
	      ((eq (camera:mode (world:camera (world object))) :fp)
	       (multiple-value-bind (w h)
		   (sdl2:get-window-size (sdl-window object))
		 (sdl2:warp-mouse-in-window (sdl-window object) (/ w 2) (/ h 2))
		 (let ((offset (vec2:vec2 (num:d- (num:d/ (num:desired w) 2.0) (num:desired x))
					  (num:d- (num:d/ (num:desired h) 2.0) (num:desired y)))))
		   (camera:reorient-fp-camera (world:camera (world object)) offset))))
	      ((eq (camera:mode (world:camera (world object))) :drag)
	       ;; does not works!
	       (multiple-value-bind (w h)
		   (sdl2:get-window-size (sdl-window object))
		 (sdl2:warp-mouse-in-window (sdl-window object) (/ w 2) (/ h 2))
		 (let ((offset (vec2:vec2 (num:d- (num:d/ (num:desired w) 2.0) (num:desired x))
					  (num:d- (num:d/ (num:desired h) 2.0) (num:desired y)))))
		   (camera:drag-camera (world:camera (world object)) offset)))))))
	;; picking test
	(when (not (selected-pc world))
	  (world:highlight-tile-screenspace world world x y)))))


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
