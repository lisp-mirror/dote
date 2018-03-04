;; dawn of the Era: a tactical game.
;; Copyright (C) 2015  cage

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

(in-package :md2-mesh)

(alexandria:define-constant +magic-number+            "IDP2"             :test #'string=)

(alexandria:define-constant +version+                 8                  :test #'=)

(alexandria:define-constant +element-type+            '(unsigned-byte 8) :test 'equalp)

(alexandria:define-constant +size-elem-texture-coord+ 2                  :test #'=)

(alexandria:define-constant +size-textures-struct+    4                  :test #'=)

(alexandria:define-constant +size-elem-triangles+     2                  :test #'=)

(alexandria:define-constant +size-triangles-struct+  12                  :test #'=)

(alexandria:define-constant +size-triangles-chunk+    6                  :test #'=)

(alexandria:define-constant +size-frame-name+        16                  :test #'=)

(alexandria:define-constant +size-vertex-struct+      4                  :test #'=)

(alexandria:define-constant +default-animation-table+
    '((:stand          0  39 20) ; name starting-frame ending-frame fps
      (:move          40  45 20)
      (:attack        46  53 20)
      (:attack-spell 138 142 10)
      (:spell        138 142 10)
      (:bored        123 134 20)
      (:pain          54  65 20)
      (:death        178 197 20)
      (:critical     160 168 20)
      (:critical2     84  94 20)
      (:critical3     72  83 20))
  :test #'equalp)

(defmacro define-header-offsets ((&rest names))
  `(misc:define-offset-size md2-mesh md2
     ,@(loop
          for name in names
          for start from 0 by 4 collect
            `(,name ,start 4))))

(defclass md2-fs-res ()
  ((dir
    :initform nil
    :initarg  :dir
    :accessor dir)
   (mesh-file
    :initform "body01.md2"
    :initarg  :mesh-file
    :accessor mesh-file)
   (animation-file
    :initform "body-animation.lisp"
    :initarg  :animation-file
    :accessor animation-file)
   (texture-file
    :initform "body-texture.tga"
    :initarg  :texture-file
    :accessor texture-file)
   (tags-file
    :initform  "body01.tag"
    :initarg   :tags-file
    :accessor  tags-file)
   (resource-path
    :initform  nil
    :initarg   :resource-path
    :accessor  resource-path)))

(defmethod marshal:class-persistant-slots ((object md2-fs-res))
  '(dir
    mesh-file
    animation-file
    texture-file
    tags-file
    resource-path))

(defmethod clone-into :after ((from md2-fs-res) (to md2-fs-res))
  (setf (dir            to) (copy-seq (dir            from))
        (mesh-file      to) (copy-seq (mesh-file      from))
        (animation-file to) (copy-seq (animation-file from))
        (texture-file   to) (copy-seq (texture-file   from))
        (tags-file      to) (copy-seq (tags-file      from))
        (resource-path  to) (copy-seq (resource-path  from)))
    to)

(defmethod clone ((object md2-fs-res))
  (with-simple-clone (object 'md2-fs-res)))

(defun make-md2-fs-res (&key
                          (dir            nil)
                          (mesh-file      "body01.md2")
                          (animation-file "body-animation.lisp")
                          (texture-file   "body-texture.tga")
                          (tags-file      "body01.tag")
                          (resource-path  nil))
  (make-instance 'md2-fs-res
                 :dir             dir
                 :mesh-file       mesh-file
                 :animation-file  animation-file
                 :texture-file    texture-file
                 :tags-file       tags-file
                 :resource-path   resource-path))

(defclass md2-mesh (able-to-see-mesh keyframe-trigger)
  ((fs-resources
    :initform (make-md2-fs-res)
    :initarg  :fs-resources
    :accessor fs-resources)
   (frames
    :initform (misc:make-array-frame 0)
    :initarg :frames
    :accessor frames)
   (parsing-errors
    :initform '()
    :initarg :parsing-errors
    :accessor parsing-errors)
   (starting-frame
    :initform 0
    :initarg :starting-frame
    :accessor starting-frame
    :type fixnum)
   (end-frame
    :initform 39
    :initarg :end-frame
    :accessor end-frame
    :type fixnum)
   (animation-table
    :initform +default-animation-table+
    :initarg :animation-table
    :accessor animation-table)
   (current-frame-offset
    :initform 0
    :initarg :current-frame-offset
    :accessor current-frame-offset
    :type fixnum)
   (current-animation-time
    :initform 0
    :initarg :current-animation-time
    :accessor current-animation-time
    :type single-float)
   (array-mixer
    :initform  (make-instance 'gpu-lerp:array-mixer)
    :initarg   :array-mixer
    :accessor  array-mixer)
   (fps
    :initform 20
    :initarg :fps
    :accessor fps
    :type single-float)
   (stop-animation
    :initform nil
    :initarg :stop-animation
    :accessor stop-animation)
   (cycle-animation
    :initform t
    :initarg :cycle-animation
    :accessor cycle-animation)
   (current-action
    :initform :stand
    :initarg :current-action
    :accessor current-action)))

(defmethod initialize-instance :after ((object md2-mesh) &key &allow-other-keys)
  (with-accessors ((array-mixer array-mixer)) object
    (setf (gpu-lerp:compiled-shaders array-mixer) (compiled-shaders object))
    (gpu-lerp:prepare array-mixer)))

(defmethod print-object ((object md2-mesh) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a" (id object))))

(defmethod character:pclass-of-useful-in-attack-tactic-p ((object md2-mesh))
  (character:pclass-of-useful-in-attack-tactic-p (ghost object)))

(defmethod on-game-event ((object md2-mesh) (event game-event:game-interrupt-terminated-event))
  "Note no need to check for target id, because just an entity can
   exists in interrupt state at any time in the game"
  (with-accessors ((ghost ghost)) object
    (if (and ghost
             (has-interrupt-plan-p ghost))
        (progn
          (unset-interrupt-plan ghost)
          t)
        nil)))

(defmacro with-end-attack-event ((object event attacked-by-entity) &body body)
  `(check-event-targeted-to-me (,object ,event)
    (with-accessors ((,attacked-by-entity attacked-by-entity)) ,object
      ,@body
      ;; this will ensure unregistering and setting attacker to nil
      (call-next-method))
    t))

(defmacro with-maybe-reply-attack ((object attacked-by-entity) &body body)
  (alexandria:with-gensyms (ghost chance state world weapon-short-range weapon-long-range)
    `(with-accessors ((,state state)) ,object
       (let* ((,ghost (ghost ,object))
              (,chance (actual-reply-attack-chance ,ghost)))
         ;; TEST ;;;;;;;;;;;;,
         (when (or t
                   (with-no-terror-status (,object)
                     (with-no-terror-status (,object)
                       (and (not (entity:reply-attack-p ,attacked-by-entity)) ; avoid 'ping pong'
                            ( dice:pass-d100.0 ,chance)))))
           ;; attack!
           (game-state:with-world (,world ,state)
             (let ((,weapon-short-range (weapon-type-short-range ,ghost))
                   (,weapon-long-range  (weapon-type-long-range  ,ghost)))
               (cond
                 (,weapon-short-range
                  (setf (entity:reply-attack ,object) t)
                  (battle-utils:attack-short-range ,world ,object ,attacked-by-entity))
                 (,weapon-long-range
                  (setf (entity:reply-attack ,object) t)
                  (battle-utils:attack-long-range ,world ,object ,attacked-by-entity))))))
         ,@body))))

(defmethod on-game-event ((object md2-mesh) (event end-attack-melee-event))
  (with-end-attack-event (object event attacked-by-entity)
    (with-maybe-reply-attack (object attacked-by-entity)
      (with-remove-interrupt-character-plan))))

(defmethod on-game-event ((object md2-mesh) (event end-attack-long-range-event))
  (with-end-attack-event (object event attacked-by-entity)
    (with-maybe-reply-attack (object attacked-by-entity)
      (with-remove-interrupt-character-plan))))

(defmethod on-game-event ((object md2-mesh) (event end-attack-spell-event))
  (with-end-attack-event (object event attacked-by-entity) ;; no reply to spell
    (setf (spell-loaded (entity:ghost object)) nil)))

(defmethod on-game-event ((object md2-mesh) (event end-spell-event))
  (with-end-attack-event (object event attacked-by-entity) ;; no reply to spell
    (setf (spell-loaded (entity:ghost object)) nil)))

(defmethod on-game-event ((object md2-mesh) (event end-defend-from-attack-spell-event))
  (with-end-attack-event (object event attacked-by-entity) ;; no reply to spell
    (with-remove-interrupt-character-plan)))

(defmethod on-game-event ((object md2-mesh) (event end-defend-from-spell-event))
  (with-end-attack-event (object event attacked-by-entity) ;; no reply to spell
    (with-remove-interrupt-character-plan)))

(defun manage-occurred-damage (entity
                               event
                               damage
                               ambushp
                               register-for-end-attack-fn
                               send-end-attack-event-fn
                               weapon)
  (when ambushp
    (billboard:apply-tooltip entity
                             billboard:+tooltip-surprise-attack-char+
                             :enqueuedp nil
                             :color     billboard:+damage-color+
                             :font-type gui:+tooltip-font-handle+))
  (apply-damage entity damage) ;; it is ok for damage to be nil.
  (setf (attacked-by-entity entity) (attacker-entity event))
  (funcall register-for-end-attack-fn entity)
  (if damage
      (battle-utils:send-effects-after-attack (attacker-entity event) entity :weapon weapon)
      (funcall send-end-attack-event-fn entity)))

(defmethod on-game-event ((object md2-mesh) (event attack-melee-event))
  (check-event-targeted-to-me (object event)
    (multiple-value-bind (damage ambush)
        (battle-utils:defend-from-attack-short-range event)
      (manage-occurred-damage object
                              event
                              damage
                              ambush
                              #'register-for-end-attack-melee-event
                              #'send-end-attack-melee-event
                              (worn-weapon (entity:ghost object)))
      (battle-utils:reward-exp-melee-attack event damage) ; damage can have null value
      t)))

(defun update-concernining-tiles-on-event (event)
  (let* ((attacker (game-event:attacker-entity event))
         (defender (game-state:find-entity-by-id (entity:state attacker)
                                                 (game-event:id-destination event))))
    (blackboard:update-concerning-zones-around-entity defender)
    (blackboard:add-tail-concerning-zone attacker defender)))

(defmethod on-game-event ((object md2-mesh) (event attack-long-range-event))
  (check-event-targeted-to-me (object event)
    (multiple-value-bind (damage ambush)
        (battle-utils:defend-from-attack-long-range event)
      (when damage
        (update-concernining-tiles-on-event event))
      (manage-occurred-damage object
                              event
                              damage
                              ambush
                              #'register-for-end-attack-long-range-event
                              #'send-end-attack-long-range-event
                              (worn-weapon (entity:ghost object)))
      (battle-utils:reward-exp-long-range-attack event damage) ; damage can have null value
      t)))

(defmethod on-game-event ((object md2-mesh) (event attack-spell-event))
  (check-event-targeted-to-me (object event)
      (with-accessors ((state state)
                       (id id)) object
        (multiple-value-bind (damage ambush)
            (battle-utils:defend-from-attack-spell event)
          (when (and damage
                     ;; ignore  spell if  launched  by friends  that's
                     ;; concerning but from another point of view! :)
                     (not (eq (my-faction object)
                              (my-faction (find-entity-by-id state (id-origin event))))))
            (update-concernining-tiles-on-event event))
          #+debug-mode (misc:dbg "apply ~a" damage)
          (manage-occurred-damage object
                                  event
                                  damage
                                  ambush
                                  #'register-for-end-attack-spell-event
                                  #'send-end-attack-spell-event
                                  (game-event:spell event))
          (battle-utils:reward-exp-launch-attack-spell event damage) ; damage can have null value
          t))))

(defmethod on-game-event ((object md2-mesh) (event spell-event))
  (check-event-targeted-to-me (object event)
    (when (battle-utils:defend-from-spell event)
      (setf (attacked-by-entity object) (attacker-entity event))
      (game-event:register-for-end-attack-spell-event object)
      (battle-utils:reward-exp-launch-spell (attacker-entity event) object (spell event))
      t)))

(defmethod on-game-event ((object md2-mesh) (event update-visibility))
  (with-accessors ((state state)
                   (id id)) object
    (labels ((stop-movements (already-stopped interrupt-if-ai)
               (when (not already-stopped)
                 (%stop-movement object
                                 :decrement-movement-points t
                                 :interrupt-plan-if-ai      interrupt-if-ai)))
             (seenp (entity &key
                            (saved-render-p nil)
                            (maintain-render nil)
                            (interrupt-plan-if-ai t))
               (let ((already-stopped nil))
                 (when (find-if #'(lambda (a) (= id (id a))) (visible-players entity))
                   (when (not maintain-render)
                     (setf (renderp object) t))
                   (when (and (typep (from-event event)
                                     'move-entity-entered-in-tile-event)
                              (current-path (ghost object))
                              (not (eq (my-faction object) (my-faction entity))))
                     (when (not saved-render-p)
                       (setf already-stopped t)
                       (stop-movements nil interrupt-plan-if-ai))
                     ;;;;; TEST;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     (when (or t (dice:pass-d100.0 (actual-ambush-attack-chance (ghost entity))))
                       (cond
                         ((battle-utils:long-range-attack-possible-p entity object)
                          (game-state:with-world (world state)
                            (stop-movements already-stopped t)
                            (battle-utils:attack-long-range world entity object)))
                         ((battle-utils:short-range-attack-possible-p entity object)
                          (game-state:with-world (world state)
                            (stop-movements already-stopped t)
                            (battle-utils:attack-short-range world entity object))))))
                   t)
                 nil))
             (seep (entity)
               (loop
                  for seen in (visible-players entity
                                               :predicate #'(lambda (a)
                                                              (not (eq (my-faction object)
                                                                       (my-faction a))))) do
                    (setf (renderp seen) t))))
      (if (= (id object) (id-origin event))
          (let ((saved-renderp (renderp object)))
            (game-state:loop-player-entities state
                                            #'(lambda (v)
                                                (update-visibility-cone v)))
            (game-state:loop-ai-entities state
                                        #'(lambda (v)
                                            (update-visibility-cone v)))
            (if (faction-ai-p state id)
                (progn
                  (setf (renderp object) nil)
                  (game-state:loop-player-entities state
                                                  #'(lambda (v)
                                                      (seenp v
                                                             :maintain-render nil
                                                             :saved-render-p   saved-renderp)))
                  (seep object))
                (progn
                  (game-state:loop-ai-entities state
                                              #'(lambda (v)
                                                  (seenp v
                                                         :maintain-render t
                                                         :saved-render-p   saved-renderp)))
                  (game-state:loop-ai-entities state
                                              #'(lambda (v)
                                                  (setf (renderp v) nil)))
                  (game-state:loop-player-entities state
                                                  #'(lambda (v)
                                                      (seep v)))))
            t)
          nil))))

(defun path->dir (path &key (start-index 0))
  (let* ((start      (ivec2 (elt (elt path start-index) 0)
                            (elt (elt path start-index) 1)))
         (end        (ivec2 (elt (elt path (1+ start-index)) 0)
                            (elt (elt path (1+ start-index)) 1)))
         (new-dir-2d (ivec2- end start)))
    (vec (d (elt new-dir-2d 0)) 0.0  (d (elt new-dir-2d 1)))))

(defmethod on-game-event ((object md2-mesh) (event move-entity-along-path-event))
  (with-accessors ((ghost ghost)
                   (dir dir)
                   (stop-animation  stop-animation)
                   (cycle-animation cycle-animation))  object
    (with-accessors ((current-path current-path)) ghost
      (if (= (id object) (id-destination event))
          (let ((disable-input-event (make-instance 'window-accept-input-event
                                                    :accept-input-p nil)))
            (game-event:propagate-window-accept-input-event disable-input-event)
            (setf (game-state:selected-path (state object)) nil)
            (setf current-path (path event))
            (setf stop-animation  nil)
            (setf cycle-animation t)
            (setf (current-action object) :move)
            (set-animation object :move :recalculate t)
            (setf dir (path->dir current-path :start-index 0))
            t)
          nil))))

(defun %stop-movement (player &key
                                (decrement-movement-points t)
                                (interrupt-plan-if-ai      t))
  (with-accessors ((ghost ghost)
                   (dir dir)
                   (id id)
                   (state state)) player
    (with-accessors ((current-path current-path)) ghost
      (let ((end-event (make-instance 'move-entity-along-path-end-event
                                      :id-origin id
                                      :tile-pos  (alexandria:last-elt current-path))))
        (when decrement-movement-points
          (decrement-move-points-entering-tile player))
        (when (and interrupt-plan-if-ai
                   (faction-ai-p state (id player)))
          (set-interrupt-plan ghost))
        (propagate-move-entity-along-path-end-event end-event)
        player))))

(defun %try-deactivate-trap-cb (world player trap)
  #'(lambda (w e)
      (declare (ignore w e))
      (let ((ghost-trap  (ghost trap))
            (ghost-player (ghost player)))
        (world:remove-all-windows world)
        (if (die-utils:pass-d1.0 (d/ (actual-deactivate-trap-chance ghost-player)
                                     (level                         ghost-trap)))
            (send-deactivate-trap-event player trap)
            (send-trap-triggered-event trap player)))))

(defun %try-deactivate-trap-from-ai (world player trap)
  (funcall (%try-deactivate-trap-cb world player trap) nil nil))

(defun %try-deactivate-trap (world player trap)
  (world:post-entity-message world
                             player
                             (format nil (_ "Trap found! deactivate (chance ~a)?")
                                           (d/ (actual-deactivate-trap-chance (ghost player))
                                               (level                         (ghost trap))))
                             t
                             (cons (_ "OK")
                                   (%try-deactivate-trap-cb world player trap))
                             (cons (_ "No")
                                   #'(lambda (w e)
                                       (declare (ignore w e))
                                       (send-trap-triggered-event trap player)
                                       (world:remove-all-windows world)))))

(defun manage-door-ai (state player path idx-path-maybe-door)
  (when (game-state:door-in-next-path-tile-p state path idx-path-maybe-door)
    (when-let* ((id-door (game-state:door-in-next-path-tile-p state path idx-path-maybe-door))
                (door    (game-state:find-entity-by-id state id-door)))
      (when (not (openp door))
        (let ((door-event (game-event:make-simple-event-w-dest 'game-event:open-door-event
                                                               (id player)
                                                               id-door)))
          (game-event:propagate-open-door-event door-event)
          (%stop-movement player
                          :decrement-movement-points t
                          :interrupt-plan-if-ai      t))))))

(defun %on-move-entity-entered-in-tile-event-ai (player event)
  (with-accessors ((ghost ghost)
                   (dir dir)
                   (id id)
                   (state state)) player
    (with-accessors ((current-path current-path)) ghost
      (let ((leaving-tile (alexandria:first-elt current-path))
            (pos-entity   (calculate-cost-position player)))
        (setf current-path (subseq current-path 1))
        ;; note: starting  from now first element of the path is my tile position
        (if (= (length current-path) 1) ;; entering in last tile, stop
            (%stop-movement player
                            :decrement-movement-points t
                            :interrupt-plan-if-ai      nil)
            (progn
              (decrement-move-points-entering-tile player)
              (setf dir (path->dir current-path :start-index 0))))
        (game-state:with-world (world state)
          ;; update state matrix and quadtree
          (world:move-entity world player leaving-tile)
          #+ (and debug-mode debug-ai)
          (propagate-update-highlight-path (make-instance 'update-highlight-path
                                                          :tile-pos current-path))
          (update-visibility-cone player)
          ;; update visited tiles in blackboard
          (game-state:set-tile-visited state player (elt pos-entity 0) (elt pos-entity 1))
          ;; manage traps
          (let ((trap-ostile    (trap-ostile-p player))
                (step-on-trap-p nil))
            (when trap-ostile
              (setf step-on-trap-p t)
              (%stop-movement player
                              :decrement-movement-points t
                              :interrupt-plan-if-ai      t)
              (%try-deactivate-trap-from-ai world player trap-ostile)))
          (send-update-visibility-event player event)
          (send-refresh-toolbar-event))))))

(defmethod on-game-event ((object md2-mesh) (event move-entity-entered-in-tile-event))
  (with-accessors ((ghost ghost)
                   (dir dir)
                   (id id)
                   (state state)) object
    (with-accessors ((current-path current-path)) ghost
      (when (= (id-origin event) id)
        (if (faction-ai-p state id)
            (%on-move-entity-entered-in-tile-event-ai object event)
            (let ((leaving-tile (alexandria:first-elt current-path))) ;; human side
              (battle-utils:reward-movement object)
              (setf current-path (subseq current-path 1))
              (if (= (length current-path) 1)
                  (%stop-movement object :decrement-movement-points t)
                  (progn
                    (decrement-move-points-entering-tile object)
                    (setf dir (path->dir current-path :start-index 0))))
              (game-state:with-world (world state)
                ;; update state matrix and quadtree
                (world:move-entity world object leaving-tile)
                (propagate-update-highlight-path (make-instance 'update-highlight-path
                                                                :tile-pos current-path))
                ;; update-cone for ai visibility check below
                (update-visibility-cone object)
                ;; traps
                (let ((trap-ostile (trap-ostile-p object)))
                  (when trap-ostile
                    (%stop-movement object :decrement-movement-points t)
                    (%try-deactivate-trap world object trap-ostile)))
                (send-update-visibility-event object event)
                (send-refresh-toolbar-event)))))))
  nil)

(defmethod on-game-event ((object md2-mesh) (event move-entity-along-path-end-event))
  (with-accessors ((id id)
                   (ghost ghost)
                   (state state)) object
    (check-event-originated-by-me (object event)
      (let ((enable-input-event (make-instance 'window-accept-input-event
                                               :accept-input-p t)))
        (game-event:propagate-window-accept-input-event enable-input-event)
        (setf (current-action object) :stand)
        (set-animation object :stand :recalculate t)
        (when (faction-ai-p state (id object))
          (send-action-terminated-event))
        t))))

(defmethod on-game-event ((object md2-mesh) (event rotate-entity-cw-event))
  (with-accessors ((ghost ghost)
                   (dir dir)
                   (pos pos)) object
    (if (= (id object) (id-destination event))
        (when (> (current-movement-points ghost) 0)
          (decrement-move-points-rotate object)
          (setf (current-action object) :rotate)
          (setf dir (sb-cga:transform-direction dir (rotate-around +y-axe+ (d- +pi/2+))))
          (update-visibility-cone object)
          ;; test
          ;;(misc:dbg "visibility test: ~a" (mapcar #'id (visible-players object)))
          (send-update-visibility-event object event)
          (send-refresh-toolbar-event)
          t)
        nil)))

(defmethod on-game-event ((object md2-mesh) (event rotate-entity-ccw-event))
  (with-accessors ((ghost ghost)
                   (dir dir)) object
    (if (= (id object) (id-destination event))
        (when (or (not (decrement-movement-points-p event))
                  (> (current-movement-points ghost) 0))
          (when (decrement-movement-points-p event)
            (decrement-move-points-rotate object))
          (setf (current-action object) :rotate)
          (setf dir (sb-cga:transform-direction dir (rotate-around +y-axe+ +pi/2+)))
          (update-visibility-cone object)
          ;;(misc:dbg "visibility test: ~a" (visible-players object))
          (send-update-visibility-event object event)
          (send-refresh-toolbar-event)
          t)
        nil)))

(defmacro with-cause-events-accessors ((mesh event immune-status-accessor) &body body)
  `(with-accessors ((ghost ghost) ; anaphora
                    (id id)
                    (state state)) ,mesh
     (with-accessors ((recurrent-effects recurrent-effects)
                      (,immune-status-accessor ,immune-status-accessor)
                      (status status)) ghost
       (with-accessors ((event-data event-data)) ,event
         (if (and (= id (id-destination ,event))
                  ;; TEST ;;;;;
                  (or t (dice:pass-d1.0 (random-object-messages:msg-chance event-data)))
                  (not ,immune-status-accessor)
                  (null status)) ;; ensure player  can not  suffers
                                 ;; more than one abnormal status
             (progn
               ,@body)
             nil)))))

(defmethod on-game-event ((object md2-mesh) (event cause-poisoning-event))
  (with-accessors ((ghost ghost)
                   (id id)
                   (state state)) object
    (with-accessors ((recurrent-effects recurrent-effects)
                     (immune-poison-status immune-poison-status)
                     (status status)) ghost
      (with-accessors ((event-data event-data)) event
        (if (and (= id (id-destination event))
                 ;;; TEST ;;;;
                 (or t (dice:pass-d1.0 (random-object-messages:msg-chance event-data)))
                 (not immune-poison-status)
                 (null status)) ;; ensure player  can not  suffers
                                ;; more than one abnormal status
            (game-state:with-world (world state)
              (push event-data recurrent-effects)
              (action-scheduler:with-enqueue-action-and-send-remove-after
                  (world action-scheduler:refresh-status-bar-action)
                (setf status +status-poisoned+)
                (billboard:enqueue-tooltip object
                                           billboard:+tooltip-poison-char+
                                           :color     billboard:+poison-damage-color+
                                           :font-type gui:+tooltip-font-handle+)
                (send-refresh-toolbar-event :reset-health-status-animation t))
              t)
            nil)))))

(defmacro with-simple-cause-status ((mesh event immune-accessor new-status tooltip)
                                    &body body)
  (alexandria:with-gensyms (world)
    `(with-cause-events-accessors (,mesh ,event ,immune-accessor)
       (game-state:with-world (,world state)
         (action-scheduler:with-enqueue-action-and-send-remove-after
             (,world action-scheduler:refresh-status-bar-action)
           (setf status ,new-status)
           (billboard:enqueue-tooltip ,mesh
                                      ,tooltip
                                      :color     billboard:+poison-damage-color+
                                      :font-type gui:+tooltip-font-handle+)
                (send-refresh-toolbar-event :reset-health-status-animation t))
         ,@body))))

(defmethod on-game-event ((object md2-mesh) (event cause-terror-event))
  (with-simple-cause-status (object event immune-terror-status +status-terror+
                                    billboard:+tooltip-terror-char+)
    (with-accessors ((event-data event-data)) event
      (when (numberp (random-object-messages:msg-duration event-data))
        (pq:push-element (postponed-messages (ghost object))
                         (make-cancel-terror-event (id-origin event)
                                                   (id object)
                                                   (+ (random-object-messages:msg-duration event-data)
                                                      (game-state:game-turn state))
                                                   :original-event event))))))

(defmethod on-game-event ((object md2-mesh) (event cause-berserk-event))
  (with-simple-cause-status (object event immune-berserk-status +status-berserk+
                                    billboard:+tooltip-berserk-char+)
    (with-accessors ((event-data event-data)) event
      (when (numberp (random-object-messages:msg-duration event-data))
        (pq:push-element (postponed-messages (ghost object))
                         (make-cancel-berserk-event (id-origin event)
                                                    (id object)
                                                    (+ (random-object-messages:msg-duration event-data)
                                                       (game-state:game-turn state))
                                                    :original-event event))))))

(defmethod on-game-event ((object md2-mesh) (event cause-faint-event))
  (with-simple-cause-status (object event immune-berserk-status +status-berserk+
                                    billboard:+tooltip-faint-char+)
    (set-death-status object)))

(defun %common-cure-status-events (mesh event
                                   &optional (action-fn #'(lambda (a b)
                                                            (declare (ignore a b))
                                                            t)))
  (with-accessors ((ghost ghost)
                   (id id)
                   (state state)) mesh
    (with-accessors ((status status)
                     (recurrent-effects recurrent-effects)) ghost
      (with-accessors ((event-data event-data)) event
        (if (and (= id (id-destination event))
                 (dice:pass-d1.0 (random-object-messages:msg-chance event-data)))
            (progn
              (billboard:enqueue-tooltip mesh
                                       billboard:+tooltip-heal-char+
                                       :color     billboard:+healing-color+
                                       :font-type gui:+tooltip-font-handle+)
              (setf status nil) ;; note: the player can be affected only by one at a time
              (funcall action-fn mesh ghost)
              (send-refresh-toolbar-event)
              t)
            nil)))))

(defmethod on-game-event ((object md2-mesh) (event cure-poisoning-event))
  (%common-cure-status-events object event
                              #'(lambda (mesh ghost)
                                  (declare (ignore mesh))
                                  (setf (recurrent-effects ghost)
                                        (delete-if #'(lambda (a)
                                                       (typep a
                                                              'random-object-messages:cause-poison-msg))
                                                   (recurrent-effects ghost))))))

(defmethod on-game-event ((object md2-mesh) (event cure-terror-event))
  (%common-cure-status-events object event))

(defmethod on-game-event ((object md2-mesh) (event cure-berserk-event))
  (%common-cure-status-events object event))

(defmethod on-game-event ((object md2-mesh) (event cure-faint-event))
  (%common-cure-status-events object event
                              #'(lambda (mesh ghost)
                                  (declare (ignore mesh))
                                  (with-accessors ((damage-points damage-points)
                                                   (current-damage-points current-damage-points))
                                      ghost
                                    (unset-death-status object)
                                    (setf current-damage-points
                                          (d (truncate (* damage-points
                                                          battle-utils:+recover-from-faint-dmg-fraction+))))))))

(defmacro with-common-cancel-event (object event status-affected text-message-bag)
  `(with-accessors ((ghost ghost) (id id) (state state)) ,object
     (with-accessors ((status status)) ghost
       (if (and (= id (id-destination ,event))
                (eq status ,status-affected))
           (progn
             (game-state:with-world (world state)
               (world:post-entity-message world object
                                          (random-elt ,text-message-bag)
                                          (cons (_ "Move to")
                                                (world:point-to-entity-and-hide-cb world object))))
             (setf status nil)
             (send-refresh-toolbar-event)
             t)
           nil))))

(defmethod on-game-event ((object md2-mesh) (event cancel-terror-event))
  (with-common-cancel-event object event +status-terror+ *terror-recover*))

(defmethod on-game-event ((object md2-mesh) (event cancel-berserk-event))
  (with-common-cancel-event object event +status-berserk+ *berserk-recover*))

(defmethod on-game-event ((object md2-mesh) (event cancel-faint-event))
  (with-common-cancel-event object event +status-faint+ *faint-recover*))

(defmacro with-common-cancel-immune-event (object event immune-accessor text-message-bag)
  `(with-accessors ((ghost ghost) (id id) (state state)) ,object
     (with-accessors ((,immune-accessor ,immune-accessor)) ghost
       (if (= id (id-destination ,event))
           (when ,immune-accessor
             (game-state:with-world (world state)
               (world:post-entity-message world object
                                          (random-elt ,text-message-bag)
                                          nil
                                          (cons (_ "Move to")
                                                (world:point-to-entity-and-hide-cb world object))))
             (setf ,immune-accessor nil)
             (send-refresh-toolbar-event)
             t)
           nil))))

(defmethod on-game-event ((object md2-mesh) (event cancel-immune-berserk-event))
  (with-common-cancel-immune-event object event immune-berserk-status *cancel-immune-berserk*))

(defmethod on-game-event ((object md2-mesh) (event cancel-immune-faint-event))
  (with-common-cancel-immune-event object event immune-faint-status *cancel-immune-faint*))

(defmethod on-game-event ((object md2-mesh) (event cancel-immune-terror-event))
  (with-common-cancel-immune-event object event immune-terror-status *cancel-immune-terror*))

(defmethod on-game-event ((object md2-mesh) (event cancel-immune-poisoning-event))
  (with-common-cancel-immune-event object event immune-poison-status *cancel-immune-poisoning*))

(defmacro with-immune-events-accessors ((mesh event immune-status-accessor status-affected)
                                       &body body)
  `(with-accessors ((ghost ghost)
                    (id id)
                    (state state)) ,mesh
     (with-accessors ((,immune-status-accessor ,immune-status-accessor)
                      (status status)) ghost
       (with-accessors ((event-data event-data)) ,event
         (if (and (= id (id-destination ,event))
                  (dice:pass-d1.0 (random-object-messages:msg-chance event-data))
                  (not ,immune-status-accessor)
                  (not (eq status ,status-affected))) ;; ensure player  do not  suffers
                                                      ;; from this status
             (progn
               ,@body)
             nil)))))

(defmacro with-simple-immune-status ((mesh event immune-accessor status-affected tooltip)
                                     &body body)
  (alexandria:with-gensyms (world)
    `(with-immune-events-accessors (,mesh ,event ,immune-accessor ,status-affected)
       (game-state:with-world (,world state)
         (action-scheduler:with-enqueue-action-and-send-remove-after
             (,world action-scheduler:refresh-status-bar-action)
           (setf ,immune-accessor t)
           (billboard:enqueue-tooltip ,mesh
                                      ,tooltip
                                      :color     billboard:+poison-damage-color+
                                      :font-type gui:+tooltip-font-handle+)
           (send-refresh-toolbar-event :reset-health-status-animation t))
         ,@body))))

(defmethod on-game-event ((object md2-mesh) (event immune-berserk-event))
  (with-simple-immune-status (object event immune-berserk-status +status-berserk+
                                    billboard:+tooltip-immune-berserk-char+)
    (with-accessors ((event-data event-data)) event
      (when (numberp (random-object-messages:msg-duration event-data))
        (pq:push-element (postponed-messages (ghost object))
                         (make-cancel-immune-berserk-event (id-origin event)
                                                           (id object)
                                                           (+ (random-object-messages:msg-duration event-data)
                                                              (game-state:game-turn state))
                                                           :original-event event))))))

(defmethod on-game-event ((object md2-mesh) (event immune-terror-event))
  (with-simple-immune-status (object event immune-terror-status +status-terror+
                                    billboard:+tooltip-immune-terror-char+)
    (with-accessors ((event-data event-data)) event
      (when (numberp (random-object-messages:msg-duration event-data))
        (pq:push-element (postponed-messages (ghost object))
                         (make-cancel-immune-terror-event (id-origin event)
                                                          (id object)
                                                          (+ (random-object-messages:msg-duration event-data)
                                                             (game-state:game-turn state))
                                                          :original-event event))))))

(defmethod on-game-event ((object md2-mesh) (event immune-faint-event))
  (with-simple-immune-status (object event immune-faint-status +status-faint+
                                    billboard:+tooltip-immune-faint-char+)
    (with-accessors ((event-data event-data)) event
      (when (numberp (random-object-messages:msg-duration event-data))
        (pq:push-element (postponed-messages (ghost object))
                         (make-cancel-immune-faint-event (id-origin event)
                                                          (id object)
                                                          (+ (random-object-messages:msg-duration event-data)
                                                             (game-state:game-turn state))
                                                          :original-event event))))))

(defmethod on-game-event ((object md2-mesh) (event immune-poisoning-event))
  (with-simple-immune-status (object event immune-poison-status +status-poisoned+
                                    billboard:+tooltip-immune-poison-char+)
    (with-accessors ((event-data event-data)) event
      (when (numberp (random-object-messages:msg-duration event-data))
        (pq:push-element (postponed-messages (ghost object))
                         (make-cancel-immune-poisoning-event (id-origin event)
                                                             (id object)
                                                             (+ (random-object-messages:msg-duration event-data)
                                                                (game-state:game-turn state))
                                                             :original-event event))))))

(defmethod on-game-event ((object md2-mesh) (event heal-damage-event))
  (with-accessors ((ghost ghost)
                   (id id)
                   (state state)) object
    (with-accessors ((current-damage-points current-damage-points)
                     (damage-points damage-points)
                     (status status)) ghost
      (with-accessors ((event-data event-data)) event
        (if (and (= id (id-destination event))
                 ;; TEST
                 (or t (dice:pass-d1.0 (random-object-messages:msg-chance event-data)))
                 (not (or (character:status-faint-p ghost)
                          (character:status-berserk-p ghost))))
            (progn
              (setf current-damage-points
                    (min damage-points
                         (d+ current-damage-points
                             (random-object-messages:msg-points event-data))))
              (billboard:enqueue-tooltip object
                                         (format nil
                                                 +standard-float-print-format+
                                                 (random-object-messages:msg-points event-data))
                                         :color     billboard:+healing-color+
                                         :font-type gui:+tooltip-font-handle+)
              (send-refresh-toolbar-event)
              t)
            nil)))))

(defmethod on-game-event ((object md2-mesh) (event wear-object-event))
  (with-accessors ((ghost ghost) (id id) (state state)) object
    (with-accessors ((messages event-data)) event
      (with-accessors ((modifiers-effects modifiers-effects)) ghost
        (if (= id (id-destination event))
            (let* ((triggered            (remove-if
                                          (random-object-messages:untrigged-effect-p-fn
                                           basic-interaction-parameters:+effect-when-worn+)
                                          messages))
                   ;; all effects targeted to self are managed by modifier-object-event
                   (all-effects-to-other (remove-if-not
                                          (random-object-messages:to-other-target-effect-p-fn)
                                          messages))
                   (magic-effect (find-if #'(lambda (a)
                                              (typep a 'random-object-messages:magic-effect-msg))
                                          triggered)))
              (decrement-move-points-wear object)
              (random-object-messages:propagate-effects-msg (id-origin event) id triggered)
              (dolist (effect all-effects-to-other)
                (push effect modifiers-effects))
              ;; spells
              (when (and magic-effect
                         ;; TEST ;;;;;;;;;;;;;;;;;;;;;
                         (or t (die-utils:pass-d100.0 (smoothstep-interpolate 0.0
                                                                              100.0
                                                                              (d (smartness ghost))))))
                (let* ((spell-id (random-object-messages:msg-spell-id magic-effect))
                       (spell    (spell:get-spell spell-id)))
                  (setf (spell-loaded ghost) spell)
                  (game-state:with-world (world state)
                    (let ((ident (spell:spell-id->string-for-human (spell:identifier spell))))
                      (world:post-entity-message world
                                                 object
                                                 (format nil
                                                         (_ "You can cast ~a!")
                                                         ident)
                                                 nil)))))
              (send-refresh-toolbar-event :reset-health-status-animation t)
              t)
            nil)))))

(defmethod on-game-event ((object md2-mesh) (event unwear-object-event))
  (with-accessors ((ghost ghost) (id id) (state state)) object
    (if (= id (id-destination event))
        (progn
          (decrement-move-points-wear object)
          (remove-from-modifiers ghost (id-origin event))
          (send-refresh-toolbar-event)
          t)
        nil)))

(defmethod on-game-event ((object md2-mesh) (event modifier-object-event))
  (with-accessors ((ghost ghost) (id id) (state state)) object
    (if (= id (id-destination event))
        (progn
          (push (event-data event) (modifiers-effects ghost))
          (send-refresh-toolbar-event)
          t)
        nil)))

(defmethod on-game-event ((object md2-mesh) (event trap-triggered-event))
  (check-event-targeted-to-me (object event)
    (with-accessors ((state state)) object
      (let* ((trap (game-state:find-entity-by-id state (id-origin event))))
        (battle-utils:trigger-trap-attack trap object)))))

(defmethod on-game-event :after ((object md2-mesh) (event end-turn))
  (with-accessors ((ghost ghost)
                   (state state)) object
    (game-state:with-world (world state)
      (when ghost
        ;; TEST
        (when (faction-ai-p (state object) (id object))
          (with-accessors ((blackboard blackboard:blackboard)) state
            (let ((reachable-fn (blackboard:reachable-p-w/concening-tiles-fn blackboard)))
              (misc:dbg "all-tactics ~a"
                        (blackboard:build-all-attack-tactics blackboard reachable-fn)))
            (setf (character:movement-points (ghost object)) 50.0)))
        ;;;;;;;;;;;;;;;;;;;;;;;
        (reset-spell-points    ghost)
        (reset-movement-points ghost)
        (traverse-recurrent-effects      object)
        (let ((decayed-items (remove-decayed-items ghost (end-turn-count event))))
          (dolist (item decayed-items)
            (remove-from-modifiers ghost (id item))
            (world:post-entity-message world object
                                       (format nil
                                               (_ "~a broken")
                                               (description-type item))
                                       nil
                                       (cons (_ "Move to")
                                             (world:point-to-entity-and-hide-cb world object)))))
        ;; clear blacklisted actions for planner
        (character:clear-blacklist ghost)
        (send-refresh-toolbar-event)
        nil))))

(defmethod my-faction ((object md2-mesh))
  (with-accessors ((state state)
                   (id id)) object
    (cond
      ((faction-player-p state id)
       game-state:+pc-type+)
      (t
       game-state:+npc-type+))))

(defun add-trigger-hit-melee (entity damage max-damage)
  (with-accessors ((pos  pos)
                   (dir  dir)
                   (aabb aabb)) entity
    (let* ((center-entity (aabb-center aabb))
           (fn #'(lambda (entity)
                   (cond
                     ((< 0.2 (/ damage max-damage) 0.5)
                      (particles:add-hit-0-effect-billboard entity pos))
                     ((< 0.5 (/ damage max-damage) 0.7)
                      (particles:add-blood-level-0 entity center-entity dir)
                      (particles:add-hit-1-effect-billboard entity pos))
                     ((< 0.7 (/ damage max-damage) 0.9)
                      (particles:add-blood-level-1 entity center-entity dir)
                      (particles:add-hit-2-effect-billboard entity pos))
                     ((> (/ damage max-damage) 0.9)
                      (particles:add-blood-level-2 entity center-entity dir)
                      (particles:add-hit-3-effect-billboard entity pos))))))
      (add-start-one-shot-trigger entity :pain fn))))

(defmethod apply-damage ((object md2-mesh) damage &key (tooltip-active-p t))
  (with-accessors ((ghost ghost)
                   (id id)
                   (state state)
                   (pos pos)
                   (dir dir)
                   (aabb aabb)
                   (compiled-shaders compiled-shaders)
                   (current-action current-action)
                   (cycle-animation cycle-animation)
                   (stop-animation stop-animation)) object
    (with-accessors ((recurrent-effects recurrent-effects)
                     (status status)
                     (current-damage-points current-damage-points)) ghost
      (when (not (entity-dead-p object))
        (if (null damage)
            (billboard:enqueue-tooltip object
                                       (format nil (_ "miss"))
                                       :color     billboard:+damage-color+
                                       :font-type gui:+tooltip-font-handle+
                                       :activep   tooltip-active-p)
            (progn
              (setf current-damage-points (d- current-damage-points damage))
              (if (entity-dead-p object)
                  (set-death-status object)
                  (progn
                    (add-trigger-hit-melee object damage (actual-damage-points ghost))
                    (setf current-action  :pain)
                    (set-animation object :pain :recalculate t)
                    (setf stop-animation nil)
                    (setf cycle-animation nil)))
              (billboard:enqueue-tooltip object
                                         (format nil
                                                 +standard-float-print-format+
                                                 (d- damage))
                                         :color     billboard:+damage-color+
                                         :font-type gui:+tooltip-font-handle+
                                         :activep   tooltip-active-p)))))))

(defmethod traverse-recurrent-effects ((object md2-mesh))
  (with-accessors ((ghost ghost)
                   (id id)
                   (state state)
                   (current-action current-action)
                   (cycle-animation cycle-animation)
                   (stop-animation stop-animation)) object
    (with-accessors ((recurrent-effects recurrent-effects)
                     (status status)
                     (current-damage-points current-damage-points)) ghost
      (loop for effect in recurrent-effects do
           (cond
             ((typep effect 'random-object-messages:cause-poison-msg)
              (when (not (entity-dead-p object))
                (setf current-damage-points (d- current-damage-points
                                                (random-object-messages:msg-damage effect)))
                (if (entity-dead-p object)
                    (set-death-status object)
                    (progn
                      (setf current-action  :pain)
                      (set-animation object :pain :recalculate t)
                      (setf stop-animation nil)
                      (setf cycle-animation nil)))
                (billboard:enqueue-tooltip object
                                           (format nil
                                                   +standard-float-print-format+
                                                   (d- (random-object-messages:msg-damage effect)))
                                           :color billboard:+damage-color+
                                           :font-type gui:+tooltip-font-handle+))))))))

(defmethod process-postponed-messages ((object md2-mesh))
  (with-accessors ((ghost ghost)
                   (state state)) object
    (with-accessors ((postponed-messages postponed-messages)) ghost
      (when (not (pq:emptyp postponed-messages))
        (let ((msg (pq:pop-element postponed-messages)))
          (if (= (trigger-turn msg)
                 (game-state:game-turn state))
              (progn
                (cond
                  ((typep msg 'cancel-terror-event)
                   (propagate-cancel-terror-event msg))
                  ((typep msg 'cancel-faint-event)
                   (propagate-cancel-faint-event msg))
                  ((typep msg 'cancel-berserk-event)
                   (propagate-cancel-berserk-event msg))
                  ((typep msg 'cancel-immune-terror-event)
                   (propagate-cancel-immune-terror-event msg))
                  ((typep msg 'cancel-immune-faint-event)
                   (propagate-cancel-immune-faint-event msg))
                  ((typep msg 'cancel-immune-berserk-event)
                   (propagate-cancel-immune-berserk-event msg)))
                (process-postponed-messages object))
              (pq:push-element postponed-messages msg)))))))

(defmethod set-death-status ((object md2-mesh))
  (with-accessors ((id               id)
                   (ghost            ghost)
                   (aabb             aabb)
                   (state            state)
                   (compiled-shaders compiled-shaders)
                   (current-action   current-action)
                   (cycle-animation  cycle-animation)
                   (stop-animation   stop-animation)) object
    (with-accessors ((blackboard blackboard:blackboard)) state
    (with-accessors ((status                status)
                     (current-damage-points current-damage-points)) ghost
      (setf (status ghost) +status-faint+)
      (setf current-action  :death)
      (set-animation object :death :recalculate t)
      (setf stop-animation nil)
      (setf cycle-animation nil)
      (setf current-damage-points (d 0.0))
      ;; also make the blackboard "forget" about this character if is from human side
      (when (faction-player-p state id)
        (blackboard:remove-entity-from-all-attack-pos blackboard object))
      (particles:add-blood-death object (aabb-center aabb) +y-axe+)))))

(defmethod unset-death-status ((object md2-mesh))
  (with-accessors ((id               id)
                   (ghost            ghost)
                   (aabb             aabb)
                   (state            state)
                   (compiled-shaders compiled-shaders)
                   (current-action   current-action)
                   (cycle-animation  cycle-animation)
                   (stop-animation   stop-animation)) object
    (with-accessors ((blackboard blackboard:blackboard)) state
    (with-accessors ((status                status)
                     (current-damage-points current-damage-points)) ghost
      (setf (status ghost) nil)
      (setf current-action  :stand)
      (set-animation object :stand :recalculate t)
      (setf stop-animation nil)
      (setf cycle-animation t)))))

(defmethod set-attack-status ((object md2-mesh))
  (with-accessors ((ghost ghost)
                   (current-action current-action)
                   (cycle-animation cycle-animation)
                   (stop-animation stop-animation)) object
    (with-accessors ((status status)
                     (current-damage-points current-damage-points)) ghost
      (setf current-action  :attack)
      (set-animation object :attack :recalculate t)
      (setf stop-animation nil)
      (setf cycle-animation nil))))

(defmethod set-attack-spell-status ((object md2-mesh))
  (with-accessors ((ghost ghost)
                   (current-action current-action)
                   (cycle-animation cycle-animation)
                   (stop-animation stop-animation)) object
    (with-accessors ((status status)
                     (current-damage-points current-damage-points)) ghost
      (setf current-action  :attack-spell)
      (set-animation object :attack-spell :recalculate t)
      (setf stop-animation nil)
      (setf cycle-animation nil))))

(defmethod set-spell-status ((object md2-mesh))
  (with-accessors ((ghost ghost)
                   (current-action current-action)
                   (cycle-animation cycle-animation)
                   (stop-animation stop-animation)) object
    (with-accessors ((status status)
                     (current-damage-points current-damage-points)) ghost
      (setf current-action  :spell)
      (set-animation object :spell :recalculate t)
      (setf stop-animation nil)
      (setf cycle-animation nil))))

(defmethod actual-aabb-for-bullets ((object md2-mesh))
  (mesh:standard-aabb object))

(defmethod actual-aabb-for-visibility ((object md2-mesh))
  (actual-aabb-for-bullets object))

(defgeneric push-errors (object the-error))

(defgeneric load (object file))

(defgeneric load-texture (object file))

(defgeneric load-animations (object file))

(defgeneric bind-vbo (object &optional refresh))

(defgeneric set-animation (object animation &key recalculate))

(defgeneric %alloc-arrays (object))

(defgeneric calculate-move (object dt))

(defgeneric wear-item      (object item))

(defgeneric step-in-trap-p (object))

(defgeneric trap-ostile-p (object))

(defgeneric place-trap (object trap-ghost))

(defgeneric next-move-position (object strategy))

(defgeneric build-player-path (object to-pos))

(defgeneric validate-player-path (object path))

(defmethod destroy ((object md2-mesh))
  #+debug-mode (misc:dbg "destroy md2 mesh ~a ~a" (id object) (map 'list #'id (frames object)))
  (map nil #'destroy (frames object))
  (call-next-method))

(define-header-offsets (magic-number
                        version
                        texture-width
                        texture-height
                        frame-size
                        num-texture-names
                        num-vertices
                        num-texture-coords
                        num-triangles
                        num-gl-cmds
                        num-frames
                        offset-texture-names
                        offset-texture-coords
                        offset-triangles
                        offset-frames
                        offset-gl-cmds
                        offset-ends))

(defmacro define-header-function ((&rest names))
  `(progn
     ,@(loop for name in names collect
            `(misc:define-parse-header-chunk
                 (,name
                  ,(alexandria:format-symbol t "~@:(+md2-~a-offset+~)" name)
                  ,(alexandria:format-symbol t "~@:(+md2-~a-size+~)" name)
                  md2-mesh nil)))))

(define-header-function (magic-number
                         version
                         texture-width
                         texture-height
                         frame-size
                         num-texture-names
                         num-vertices
                         num-texture-coords
                         num-triangles
                         num-gl-cmds
                         num-frames
                         offset-texture-names
                         offset-texture-coords
                         offset-triangles
                         offset-frames
                         offset-gl-cmds
                         offset-ends))

(defmethod push-errors ((object md2-mesh) the-error)
  (push the-error (parsing-errors object)))

(defun read-texture-coords (stream offset size w-texture h-texture)
  (let ((raw-texture (misc:read-array stream size :offset offset)))
    (declare ((simple-array (unsigned-byte 8)) raw-texture))
    (loop
       for i from 0 below (1- (length raw-texture)) by +size-textures-struct+ collect
         (list
          (num:d/ (num:desired (misc:2byte->word (elt raw-texture i)
                                                 (elt raw-texture (+ i 1))))
                  (num:desired w-texture))
          (num:d/ (num:desired (misc:2byte->word (elt raw-texture (+ i 2))
                                                 (elt raw-texture (+ i 3))))
                  (num:desired h-texture))))))

(defun read-triangles-index (stream offset size)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (let ((raw-triangles (misc:read-array stream size :offset offset)))
    (declare ((simple-array (unsigned-byte 8)) raw-triangles))
    (alexandria:flatten
     (loop
        for i from 0 below (1- (length raw-triangles)) by +size-triangles-struct+ collect
          (loop
             for j from i below (+ i +size-triangles-struct+) by +size-elem-triangles+ collect
               (misc:2byte->word (elt raw-triangles j)
                                 (elt raw-triangles (+ j 1))))))))

(defstruct md2-vertex
  pos
  normal)

(defparameter *read-frame-queue* (lparallel.queue:make-queue :initial-contents '(t)
                                                             :fixed-capacity 1))

(defun read-frame (stream num-vertices offset)
  (lparallel.queue:pop-queue *read-frame-queue*)
  (file-position stream offset)
  (let* ((scale-x (misc:read-ieee-float-32 stream))
         (scale-y (misc:read-ieee-float-32 stream))
         (scale-z (misc:read-ieee-float-32 stream))
         (translate-x (misc:read-ieee-float-32 stream))
         (translate-y (misc:read-ieee-float-32 stream))
         (translate-z (misc:read-ieee-float-32 stream))
         (name (text-utils:clean-unprintable-chars
                (misc:bytes->string (misc:read-list stream +size-frame-name+))))
         (vertices-normals (misc:read-array stream (* +size-vertex-struct+ num-vertices)))
         (vertices (misc:make-array-frame (floor (/ (length vertices-normals) 4))
                                          nil 'md2-vertex t))

         (normals (misc:make-array-frame (floor (/ (length vertices-normals) 4))
                                          nil 'vec t)))
    (loop
       for i from 0 below (length vertices-normals) by 4
       for j from 0 by 1 do
         (setf (elt vertices j)
               (make-md2-vertex
                :pos (vec
                      (+ (* (elt vertices-normals i) scale-x) translate-x)
                      (+ (* (elt vertices-normals (+ i 1)) scale-y) translate-y)
                      (+ (* (elt vertices-normals (+ i 2)) scale-z) translate-z)))))
    (loop
       for i from 3 below (length vertices-normals) by 4
       for j from 0 by 1 do
         (setf (elt normals j)
               (elt +md2-normal-lut+ (elt vertices-normals i))))
    (dotimes (ct (length vertices))
      (setf (md2-vertex-normal (elt vertices ct)) (elt normals ct)))
    (lparallel.queue:push-queue t *read-frame-queue*)
    (values vertices name)))

(defmethod %alloc-arrays ((object md2-mesh))
  (with-accessors ((renderer-data-vertices renderer-data-vertices)
                   (renderer-data-count-vertices renderer-data-count-vertices)
                   (renderer-data-texture renderer-data-texture)
                   (renderer-data-count-texture renderer-data-count-texture)
                   (renderer-data-normals renderer-data-normals)
                   (renderer-data-count-normals renderer-data-count-normals)
                   (renderer-data-tangents renderer-data-tangents)
                   (renderer-data-count-tangents renderer-data-count-tangents)
                   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
                   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
                   (starting-frame starting-frame)
                   (triangles-mesh triangles)
                   (end-frame end-frame)
                   (frames frames)) object
    (let ((start-frame (elt frames starting-frame)))
      (setf renderer-data-vertices (gl:alloc-gl-array
                                    :float
                                    (mesh:renderer-data-count-vertices start-frame))
            renderer-data-texture (gl:alloc-gl-array
                                   :float
                                   (mesh:renderer-data-count-texture start-frame))
            renderer-data-normals (gl:alloc-gl-array
                                   :float
                                   (mesh:renderer-data-count-normals start-frame))
            renderer-data-tangents (gl:alloc-gl-array
                                    :float
                                    (mesh:renderer-data-count-tangents start-frame))
            renderer-data-aabb-obj-space (gl:alloc-gl-array :float +aabb-vertex-count+)
            renderer-data-count-vertices (mesh:renderer-data-count-vertices start-frame)
            renderer-data-count-texture (mesh:renderer-data-count-texture start-frame)
            renderer-data-count-normals (mesh:renderer-data-count-normals start-frame)
            renderer-data-count-tangents (mesh:renderer-data-count-tangents start-frame))
      (gl-utils:copy-gl-array (mesh:renderer-data-texture start-frame)
                              renderer-data-texture
                              renderer-data-count-texture)
      (gl-utils:copy-gl-array (mesh:renderer-data-tangents start-frame)
                              renderer-data-tangents
                              renderer-data-count-tangents)
      (setf triangles-mesh (triangles start-frame)
            renderer-data-normals-obj-space
            (gl:alloc-gl-array :float (normals-obj-space-vertex-count object)))
            ;; setup finalizer
      (let ((gl-arr-vert     (slot-value object 'renderer-data-vertices))
            (gl-arr-tex      (slot-value object 'renderer-data-texture))
            (gl-arr-norm     (slot-value object 'renderer-data-normals))
            (gl-arr-norm-obj (slot-value object 'renderer-data-normals-obj-space))
            (gl-arr-tan      (slot-value object 'renderer-data-tangents))
            (gl-arr-aabb     (slot-value object 'renderer-data-aabb-obj-space))
            (vbos            (slot-value object 'vbo))
            (vaos            (slot-value object 'vao))
            #+debug-mode
            (id              (slot-value object 'id)))
        (tg:finalize object
                     #'(lambda ()
                         #+debug-mode (misc:dbg "finalize destroy md2 mesh ~a" id)
                         (free-memory* (list gl-arr-vert
                                             gl-arr-tex
                                             gl-arr-norm
                                             gl-arr-norm-obj
                                             gl-arr-tan
                                             gl-arr-aabb)
                                       vbos vaos)))))))

;;;; NOTE just for testing purpose! ;;;;;;;;;;;;;;;;;;;;;
(defun %%transform-model (model)
  (warn "%%transform-model it's just for testing")
  (let ((transf (matrix* (scale (vec 0.05 0.05 0.05))
                         (quaternion:quat->matrix
                          (quaternion:quat-rotate-to-vec
                           +entity-forward-direction+
                           (vec -1.0 0.0 0.0)
                           :fallback-axis +y-axe+))
                         (quaternion:quat->matrix
                          (quaternion:quat-rotate-to-vec
                           +entity-up-direction+
                           (vec 0.0 0.0 -1.0)
                           :fallback-axis +z-axe+)))))
    (loop for tags in (tags-table model) do
         (loop for tag across (cdr tags) do
              (setf (elt tag 3)
                    (transform-point (elt tag 3) transf))))
    (loop for f across (frames model) do
         (transform-vertices f transf))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod load ((object md2-mesh) file)
  (with-accessors ((renderer-data-vertices renderer-data-vertices)
                   (renderer-data-count-vertices renderer-data-count-vertices)
                   (renderer-data-texture renderer-data-texture)
                   (renderer-data-count-texture renderer-data-count-texture)
                   (renderer-data-normals renderer-data-normals)
                   (renderer-data-count-normals renderer-data-count-normals)
                   (renderer-data-tangents renderer-data-tangents)
                   (renderer-data-count-tangents renderer-data-count-tangents)
                   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
                   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
                   (starting-frame starting-frame)
                   (triangles-mesh triangles)
                   (end-frame end-frame)
                   (frames frames)) object
      (with-open-file (stream file :if-does-not-exist :error :element-type +element-type+)
        (macrolet ((parse-header-integer (name)
                     `(misc:byte->int
                       (,(alexandria:format-symbol t "~@:(parse-~a~)" name)
                         object stream))))
          (let ((magic-num (misc:bytes->string (parse-magic-number object stream))))
            (if (string= magic-num +magic-number+)
                (let* ((*read-frame-queue* (lparallel.queue:make-queue
                                               :initial-contents '(t)
                                               :fixed-capacity 1))
                       (version (parse-header-integer version))
                       (texture-width (parse-header-integer texture-width))
                       (texture-height (parse-header-integer texture-height))
                       (frame-size (parse-header-integer frame-size))
                       (num-texture-names (parse-header-integer num-texture-names))
                       (num-vertices (parse-header-integer num-vertices))
                       (num-texture-coords (parse-header-integer num-texture-coords))
                       (num-triangles (parse-header-integer num-triangles))
                       (num-frames (parse-header-integer num-frames))
                       (offset-texture-coords (parse-header-integer offset-texture-coords))
                       (offset-triangles (parse-header-integer offset-triangles))
                       (offset-frames (parse-header-integer offset-frames))
                       (texture-coords (read-texture-coords stream offset-texture-coords
                                                            (* +size-textures-struct+
                                                               num-texture-coords)
                                                            texture-width texture-height))
                       ;; triangles = (v v v tex tex tex)
                       (triangles (read-triangles-index stream offset-triangles
                                                        (* +size-triangles-struct+
                                                           num-triangles))))
                  (declare (ignore version num-texture-names))
                  (setf frames (misc:make-array-frame num-frames nil 'triangle-mesh t))
                  (file-position stream offset-frames)
                    (lparallel:pdotimes (count num-frames)
                      (let ((vertices (read-frame stream num-vertices (+ offset-frames
                                                                         (* count frame-size)))))
                        (let ((frame-mesh (make-instance 'triangle-mesh)))
                          (loop
                             for i from 0 below (length triangles) by +size-triangles-chunk+ do
                             ;; triangles = (v v v tex tex tex)
                             ;; first triangle
                               (texel-v frame-mesh (nth (nth (+ i 5) triangles) texture-coords))
                               (normal-v frame-mesh (md2-vertex-normal
                                                     (elt
                                                      vertices
                                                      (nth (+ i 2) triangles))))
                               (tangent-v frame-mesh +y-axe+) ;; no normalmap yet for md2 mesh
                               (vertex-v frame-mesh (md2-vertex-pos
                                                     (elt vertices (nth (+ i 2) triangles)))
                                         :gen-triangle t :gen-normal nil :manifoldp nil
                                         :compact-vertices nil)
                             ;; second one
                               (texel-v frame-mesh (nth (nth (+ i 4) triangles) texture-coords))
                               (normal-v frame-mesh (md2-vertex-normal
                                                     (elt
                                                      vertices
                                                      (nth (+ i 1) triangles))))
                               (tangent-v frame-mesh +y-axe+)
                               (vertex-v frame-mesh (md2-vertex-pos
                                                     (elt
                                                      vertices
                                                      (nth (+ i 1) triangles)))
                                         :gen-triangle t :gen-normal nil :manifoldp nil
                                         :compact-vertices nil)
                             ;; third
                               (texel-v frame-mesh (nth (nth (+ i 3) triangles) texture-coords))
                               (normal-v frame-mesh (md2-vertex-normal
                                                     (elt vertices (nth i triangles))))
                               (tangent-v frame-mesh +y-axe+)
                               (vertex-v frame-mesh (md2-vertex-pos
                                                     (elt
                                                      vertices
                                                      (nth i triangles)))
                                         :gen-triangle t :gen-normal nil :manifoldp nil
                                         :compact-vertices nil))
                          (setf (elt frames count) frame-mesh))))
                    ;;;;; NOTE just for testing purpose! ;;;;;;;;;;;;;;;;;;;;;
                    (%%transform-model object)
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    (map nil #'prepare-for-rendering (frames object))
                    (%alloc-arrays object))
                (push-errors object ;; (if (string= magic-num +magic-number+) else...
                             (format nil "Wrong magic number ~a expected ~a found instead"
                                     +magic-number+ magic-num))))))))

(defmethod load-texture ((object md2-mesh) file)
  (multiple-value-bind (texture errors)
      (texture:get-texture file)
    (if (null errors)
        (progn
          (setf (texture-object object) texture)
          (when (not (texture:initializedp texture))
            (texture:prepare-for-rendering (texture-object object)))
          (loop for i across (frames object) do (setf (texture-object i) texture)))
        (mapc #'(lambda (e) (push-errors object e)) errors))))

(defmethod load-animations ((object md2-mesh) file)
  (when (filesystem-utils:file-exists-p file)
    (with-open-file (stream file)
      (setf (animation-table object) (read stream)))
    (setf (animation-table object)
          (mapcar #'(lambda (r)
                      (list (first r)
                            (coerce (second r) 'fixnum)
                            (coerce (third r) 'fixnum)
                            (coerce (fourth r) 'single-float)))
                  (animation-table object)))))

(defmethod prepare-for-rendering ((object md2-mesh))
  (with-accessors ((vbo vbo) (vao vao)
                   (renderer-data-vertices renderer-data-vertices)
                   (renderer-data-normals renderer-data-normals)
                   (renderer-data-texture renderer-data-texture)
                   (texture-object texture-object)) object
    (destroy object)
    (map nil #'prepare-for-rendering (frames object))
    (setf vbo (gl:gen-buffers +vbo-count+)
          vao (gl:gen-vertex-arrays +vao-count+))
    (%alloc-arrays object)
    (bind-vbo object nil)))

(defmacro with-common-calc-move ((entity end) &body body)
  (with-gensyms (movement-event)
    `(if (not (vec~ (pos ,entity) ,end (d/ +terrain-chunk-tile-size+ 8.0)))
         ,@body
         (let ((,movement-event (make-instance 'move-entity-entered-in-tile-event
                                               :id-origin (id ,entity)
                                               :tile-pos  ,end)))
        (propagate-move-entity-entered-in-tile-event ,movement-event)))))

(defun %calculate-move-ai (state entity dt dir current-path end)
  (declare (ignore dt))
  (with-common-calc-move (entity end)
    (if (game-state:invalicable-in-next-path-tile-p state entity current-path 1)
        (%stop-movement entity :decrement-movement-points t :interrupt-plan-if-ai t)
        ;; open door if any and if closed
        (let ((door-opened-p (manage-door-ai state entity current-path 1)))
          (when (not door-opened-p) ;; door just opened the current plan has been interrupted
            (if (gconf:config-smooth-movements)
                (setf (pos entity)
                      (vec+ (pos entity) (vec* dir +model-move-speed+)))
                (setf (pos entity) end)))))))

(defun %calculate-move-pc (state entity dt dir current-path end)
  (declare (ignore dt))
  (with-accessors ((ghost ghost)) entity
    (with-common-calc-move (entity end)
      (if (game-state:invalicable-in-next-path-tile-p state entity current-path 1)
          (%stop-movement entity :decrement-movement-points t)
          (if (gconf:config-smooth-movements)
              (setf (pos entity)
                    (vec+ (pos entity) (vec* dir +model-move-speed+)))
              (setf (pos entity) end))))))

(defmethod calculate-move ((object md2-mesh) dt)
  (with-accessors ((ghost ghost)
                   (id  id)
                   (pos pos)
                   (dir dir)
                   (state state)) object
    (with-accessors ((current-path current-path)) ghost
      (let* ((x-chunk (map-utils:coord-map->chunk (elt (elt current-path 1) 0)))
             (z-chunk (map-utils:coord-map->chunk (elt (elt current-path 1) 1)))
             (y       (d+ 1.5 ; hardcoded :(  to be removed soon
                          (game-state:approx-terrain-height@pos (state object)
                                                                x-chunk
                                                                z-chunk)))
             (end (vec x-chunk y z-chunk))
             (dir (let ((new-dir (vec- end (pos object))))
                    (if (vec~ new-dir +zero-vec+)
                        (vec 0.0 1.0 0.0)
                        (normalize new-dir)))))
        (if (faction-ai-p state id)
            (%calculate-move-ai state object dt dir current-path end)
            (%calculate-move-pc state object dt dir current-path end))))))

(defmethod wear-item ((object md2-mesh) item)
  (with-accessors ((ghost ghost)
                   (id id)) object
    (let ((c-slot (item->available-player-character-slot ghost item)))
      (when c-slot
        (setf (slot-value ghost c-slot) item)
        (remove-from-inventory ghost item)
        (let* ((messages (random-object-messages:params->effects-messages item))
               (event    (make-instance 'game-event:wear-object-event
                                        :id-origin      (id item)
                                        :id-destination id
                                        :event-data     messages)))
          (game-event:propagate-wear-object-event event))))))

(defmethod calculate ((object md2-mesh) dt)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (single-float dt))
  (with-accessors ((renderer-data-vertices renderer-data-vertices)
                   (renderer-data-count-vertices renderer-data-count-vertices)
                   (renderer-data-normals renderer-data-normals)
                   (renderer-data-count-normals renderer-data-count-normals)
                   (array-mixer array-mixer)
                   (tags-table tags-table)
                   (tags-matrices tags-matrices)
                   (tag-key-parent tag-key-parent)
                   (frames frames)
                   (starting-frame starting-frame)
                   (end-frame end-frame)
                   (current-frame-offset current-frame-offset)
                   (fps fps)
                   (current-animation-time current-animation-time)
                   (stop-animation stop-animation)
                   (cycle-animation cycle-animation)
                   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
                   (data-aabb renderer-data-aabb-obj-space)
                   (current-action current-action)
                   (ghost ghost)) object
    (declare (single-float current-animation-time fps))
    (declare (fixnum end-frame starting-frame current-frame-offset))
    (declare (simple-array frames))
    (declare (string tag-key-parent))
    (declare (list tags-table tags-matrices))
    ;; AI thinking...
    (when (and ghost
               (thinkerp ghost))
      (actuate-strategy object))
    (let ((frames-numbers (the fixnum (1+ (- end-frame starting-frame)))))
      ;; triggers
      (when (= current-frame-offset 0)
        (elaborate-start-trigger object current-action))
      (when (= current-frame-offset (1- frames-numbers))
        (elaborate-end-trigger object current-action))
      ;; animation
      (ecase current-action
        (:stand
         ;; nothing to do
         )
        (:death
         ;; TODO set death plane
         )
        (:attack
         (when stop-animation
           (setf cycle-animation t)
           (setf stop-animation nil)
           (set-animation object :stand :recalculate nil)
           (setf (current-action object) :stand)))
        (:attack-spell
         (when stop-animation
           (setf cycle-animation t)
           (setf stop-animation nil)
           (set-animation object :stand :recalculate nil)
           (setf (current-action object) :stand)))
        (:spell
         (when stop-animation
           (setf cycle-animation t)
           (setf stop-animation nil)
           (set-animation object :stand :recalculate nil)
           (setf (current-action object) :stand)))
        (:rotate
         (update-visibility-cone object))
        (:move
         (calculate-move object dt)
         (update-visibility-cone object))
        (:pain
         (when stop-animation
           (setf cycle-animation t)
           (setf stop-animation nil)
           (set-animation object :stand :recalculate nil)
           (setf (current-action object) :stand)
           (send-end-attack-melee-event object)
           (send-end-attack-long-range-event object)
           (send-end-attack-spell-event object))))
      (unless stop-animation
        (let ((next-time (+ current-animation-time dt))
              (frame-freq (/ 1.0 fps)))
          (declare (single-float next-time frame-freq))
          (declare (fixnum frames-numbers))
          (if (> next-time frame-freq)
              (setf current-frame-offset (mod (the unsigned-byte (1+ current-frame-offset))
                                              frames-numbers)
                    current-animation-time 0.0)
              (setf current-animation-time next-time))
          (let* ((next-frame-offset    (mod (the fixnum (1+ current-frame-offset)) frames-numbers))
                 (interpolation-factor (d* fps current-animation-time))
                 (starting-frame-idx   (f+ starting-frame current-frame-offset))
                 (next-frame-idx       (f+ starting-frame next-frame-offset)))
            (declare (single-float interpolation-factor))
            (declare (fixnum next-frame-offset starting-frame-idx next-frame-idx))
            (when (and (not cycle-animation)
                       (= next-frame-offset 0))
              (setf stop-animation t))
            #+md2-gpu-lerp
            (progn
              (gpu-lerp:mix array-mixer
                            interpolation-factor
                            (renderer-data-vertices (svref frames starting-frame-idx))
                            (renderer-data-vertices (svref frames next-frame-idx))
                            renderer-data-vertices)
              (gpu-lerp:mix array-mixer
                            interpolation-factor
                            (renderer-data-normals (svref frames starting-frame-idx))
                            (renderer-data-normals (svref frames next-frame-idx))
                            renderer-data-normals))
            #-md2-gpu-lerp
            (progn
              (gl-utils:lerp-gl-array (renderer-data-vertices
                                       (svref frames starting-frame-idx))
                                      (renderer-data-vertices
                                       (svref frames next-frame-idx))
                                      renderer-data-vertices
                                      renderer-data-count-vertices
                                      interpolation-factor)
              (gl-utils:lerp-gl-array (renderer-data-normals
                                       (svref frames starting-frame-idx))
                                      (renderer-data-normals
                                       (svref frames next-frame-idx))
                                      renderer-data-normals
                                      renderer-data-count-normals
                                      interpolation-factor))
            (when tags-table
              (loop for i in tags-matrices do
                   (let* ((orn1   (elt (the (simple-array (simple-array * (*)))
                                            (find-tag-cdr (car i) tags-table))
                                       starting-frame-idx))
                          (orn2   (elt (the (simple-array (simple-array * (*)))
                                            (find-tag-cdr (car i) tags-table))
                                       next-frame-idx))
                          (orn (vector (vec-lerp (elt orn1 0) (elt orn2 0) interpolation-factor)
                                       (vec-lerp (elt orn1 1) (elt orn2 1) interpolation-factor)
                                       (vec-lerp (elt orn1 2) (elt orn2 2) interpolation-factor)
                                       (vec-lerp (elt orn1 3) (elt orn2 3) interpolation-factor))))
                     (declare ((simple-array (simple-array single-float (3)) (4)) orn1 orn2))
                     (nsetup-tag-matrix (cdr i) orn))))
            (when (render-normals object)
              #+md2-gpu-lerp
              (gpu-lerp:mix array-mixer
                            interpolation-factor
                            (renderer-data-normals-obj-space (svref frames starting-frame-idx))
                            (renderer-data-normals-obj-space (svref frames next-frame-idx))
                            renderer-data-normals-obj-space)
              #-md2-gpu-lerp
              (gl-utils:lerp-gl-array (renderer-data-normals-obj-space
                                       (svref frames starting-frame-idx))
                                      (renderer-data-normals-obj-space
                                       (svref frames next-frame-idx))
                                      renderer-data-normals-obj-space
                                      (normals-obj-space-vertex-count object)
                                      interpolation-factor))
            (with-slots (aabb) object
              (reset aabb)
              (loop for i fixnum from 0 below renderer-data-count-vertices by 3 do
                   (let* ((pos (the (unsigned-byte 32) i))
                          (vert (vec (gl-utils:fast-glaref renderer-data-vertices pos)
                                     (gl-utils:fast-glaref renderer-data-vertices (+ pos 1))
                                     (gl-utils:fast-glaref renderer-data-vertices (+ pos 2)))))
                     (expand aabb vert)))
              (setf (bounding-sphere object) (aabb->bounding-sphere aabb))
              (when (render-aabb object)
                (make-data-for-opengl-aabb-obj-space object))))))
      (bubbleup-modelmatrix object)
      (when (not (mtree-utils:rootp object))
        (let ((tag-matrix (find-tag-cdr tag-key-parent
                                        (tags-matrices (mtree-utils:parent object)))))
          (when tag-matrix
            (with-model-matrix (model-matrix object)
              (setf (model-matrix object)
                    (matrix* model-matrix
                             tag-matrix))))))
      (bind-vbo object t)
      (do-children-mesh (i object)
        (calculate i dt)))))

(defmethod bind-vbo ((object md2-mesh) &optional (refresh nil))
  (with-accessors ((vbo vbo) (vao vao)
                   (renderer-data-tangents renderer-data-tangents)
                   (renderer-data-texture renderer-data-texture)
                   (renderer-data-vertices renderer-data-vertices)
                   (renderer-data-normals renderer-data-normals)
                   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
                   (renderer-data-normals-obj-space renderer-data-normals-obj-space)) object
    ;; vertices
    (gl:bind-buffer :array-buffer (vbo-vertex-buffer-handle vbo))
    (if refresh
        (gl:buffer-sub-data :array-buffer renderer-data-vertices)
        (gl:buffer-data :array-buffer :dynamic-draw renderer-data-vertices))
    ;; normals
    (gl:bind-buffer :array-buffer (vbo-normals-buffer-handle vbo))
    (if refresh
        (gl:buffer-sub-data :array-buffer renderer-data-normals)
        (gl:buffer-data :array-buffer :dynamic-draw renderer-data-normals))
    ;; tangents
    (gl:bind-buffer :array-buffer (vbo-tangents-buffer-handle vbo))
    (if refresh
        (gl:buffer-sub-data :array-buffer renderer-data-tangents)
        (gl:buffer-data :array-buffer :dynamic-draw renderer-data-tangents))
    ;; texture
    (gl:bind-buffer :array-buffer (vbo-texture-buffer-handle vbo))
    (gl:buffer-data :array-buffer :static-draw renderer-data-texture)
    (when (render-normals object)
      (gl:bind-buffer :array-buffer (vbo-normals-object-space-buffer-handle vbo))
      (if refresh
          (gl:buffer-sub-data :array-buffer renderer-data-normals-obj-space)
          (gl:buffer-data :array-buffer :dynamic-draw renderer-data-normals-obj-space)))
    (when (render-aabb object)
      (gl:bind-buffer :array-buffer (vbo-aabb-object-space-buffer-handle vbo))
      (if refresh
          (gl:buffer-sub-data :array-buffer renderer-data-aabb-obj-space)
          (gl:buffer-data :array-buffer :dynamic-draw renderer-data-aabb-obj-space)))
    (prepare-for-rendering-phong object)))

(defmethod set-animation ((object md2-mesh) animation &key (recalculate t))
  (with-accessors ((starting-frame starting-frame)
                   (end-frame end-frame)
                   (current-frame-offset current-frame-offset)
                   (fps fps)
                   (current-animation-time current-animation-time)
                   (animation-table animation-table)
                   (ghost ghost)) object
    (let ((anim-spec (assoc animation animation-table :test #'eql)))
      (when anim-spec
        (setf starting-frame         (second anim-spec)
              end-frame              (third anim-spec)
              fps                    (fourth anim-spec)
              current-animation-time 0.0
              current-frame-offset   0)
        (when recalculate
          (with-no-thinking (ghost)
            (calculate object 0.0)))))))

(defun %load-common-md2-parameters (model modeldir resource-path
                                    tags-file animation-file texture-file)
  (load-texture model (res:get-resource-file (text-utils:strcat modeldir texture-file)
                                             resource-path
                                             :if-does-not-exists :error))
  (load-animations model (res:get-resource-file (text-utils:strcat modeldir animation-file)
                                                resource-path
                                                :if-does-not-exists :error))
  (when tags-file
    (load-tags model (res:get-resource-file (text-utils:strcat modeldir tags-file)
                                            resource-path
                                            :if-does-not-exists :error))))

(defun load-md2-model (modeldir &key
                                  (shaders  nil)
                                  (material (make-mesh-material .1 1.0 0.1 0.0 128.0))
                                  (mesh-file +model-filename+)
                                  (texture-file +model-texture-filename+)
                                  (animation-file +model-animations-filename+)
                                  (tags-file      nil)
                                  (resource-path  +models-resource+))
  (let ((model  (make-instance 'md2-mesh :render-normals nil :render-aabb nil))
        (fs-res (make-md2-fs-res :dir             modeldir
                                 :mesh-file       mesh-file
                                 :animation-file  animation-file
                                 :texture-file    texture-file
                                 :tags-file       tags-file
                                 :resource-path   resource-path)))
    (setf (fs-resources    model) fs-res)
    (setf (material-params model) material)
    (%load-common-md2-parameters model modeldir resource-path tags-file animation-file texture-file)
    (load model (res:get-resource-file (text-utils:strcat modeldir mesh-file)
                                       resource-path
                                       :if-does-not-exists :error))
    (setf (compiled-shaders model) shaders)
    (setf (compiled-shaders (array-mixer model)) shaders)
    #+debug-mode (misc:dbg "error md2 parsing ~a" (parsing-errors model))
    (prepare-for-rendering model)
    ;;(map nil #'mesh:remove-mesh-data (frames model))
    (set-animation model :stand :recalculate t)
    model))

;;;;;;;;;;;;;;;; testing only! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun forged-potion ()
  (let ((potion (random-potion:generate-potion 10))
        (effect-cause-berserk (make-instance 'basic-interaction-parameters:healing-effect-parameters
                                            :trigger basic-interaction-parameters:+effect-when-consumed+
                                            :duration 2
                                            :chance   0.9)))
    (n-setf-path-value (basic-interaction-params potion)
                       '(:healing-effects :cause-berserk)
                       effect-cause-berserk)
    potion))

(defun forged-potion-cure-berserk ()
  (let ((potion (random-potion:generate-potion 10))
        (effect-cure (basic-interaction-parameters:define-healing-effect
                         (duration unlimited
                                   trigger  when-consumed
                                   chance   0.9
                                   target   self))))
    (n-setf-path-value (basic-interaction-params potion)
                       '(:healing-effects :heal-berserk)
                       effect-cure)
    potion))

(defun forged-potion-cure-dmg ()
  (let ((potion (random-potion:generate-potion 10))
        (effect-cure (basic-interaction-parameters:define-heal-dmg-effect (points 3.0
                                                     trigger  when-consumed
                                                     chance   0.9
                                                     target   self))))
    (n-setf-path-value (basic-interaction-params potion)
                       '(:healing-effects :heal-damage-points)
                       effect-cure)
    (clean-effects potion)))

(defun forged-ring ()
  (let ((ring (random-ring:generate-ring 1))
        (effect-cause-berserk (make-instance 'basic-interaction-parameters:healing-effect-parameters
                                             :trigger
                                             basic-interaction-parameters:+effect-when-worn+
                                             :duration 2
                                             :chance   0.9))
        (immune-terror                (make-instance 'basic-interaction-parameters:healing-effect-parameters
                                                     :trigger  basic-interaction-parameters:+effect-when-worn+
                                                     :duration 2
                                                     :chance 0.99
                                                     :target basic-interaction-parameters:+target-self+)))

    (n-setf-path-value (basic-interaction-params ring)
                       '(:healing-effects :cause-berserk)
                       effect-cause-berserk)
    (n-setf-path-value (basic-interaction-params ring)
                       '(:healing-effects :immune-terror)
                        immune-terror)

    (clean-effects ring)
    ring))

(defun forged-sword ()
  (let ((sword (random-weapon:generate-weapon 10 :sword))
        (effect-modifier  (make-instance 'basic-interaction-parameters:effect-parameters
                                         :trigger basic-interaction-parameters:+effect-when-worn+
                                         :duration basic-interaction-parameters:+duration-unlimited+
                                         :modifier 5.0))
        (poisoning        (make-instance 'basic-interaction-parameters:poison-effect-parameters
                                         :chance 0.9
                                         :target basic-interaction-parameters:+target-other+
                                         :points-per-turn 2.0)))
    (n-setf-path-value (basic-interaction-params sword)
                       '(:effects :melee-attack-chance)
                       effect-modifier)
    (n-setf-path-value (basic-interaction-params sword)
                        '(:healing-effects :cause-poison)
                        poisoning)
    (clean-effects sword)
    sword))

(defun forged-spear ()
  (random-weapon:generate-weapon 10 :spear))

(defun forged-bow ()
  (let ((bow (random-weapon:generate-weapon 10 :bow))
        (effect-modifier  (make-instance 'basic-interaction-parameters:effect-parameters
                                         :trigger basic-interaction-parameters:+effect-when-worn+
                                         :duration basic-interaction-parameters:+duration-unlimited+
                                         :modifier 5.0))
        (poisoning        (make-instance 'basic-interaction-parameters:poison-effect-parameters
                                         :chance 0.9
                                         :target basic-interaction-parameters:+target-other+
                                         :points-per-turn 2.0)))
    (n-setf-path-value (basic-interaction-params bow)
                       '(:effects :melee-attack-chance)
                       effect-modifier)
    (n-setf-path-value (basic-interaction-params bow)
                       '(:healing-effects :cause-poison)
                       poisoning)
    (clean-effects bow)
    bow))

(defun forged-trap ()
  (random-trap:generate-trap 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-md2-player (ghost dir compiled-shaders resource-path)
  (let ((body (get-md2-shell :shaders        compiled-shaders
                             :dir            dir
                             :mesh-file      "body01.md2"
                             :animation-file "body-animation.lisp"
                             :texture-file   "body-texture.tga"
                             :tags-file      "body01.tag"
                             :resource-path  resource-path))
        (head (get-md2-shell :shaders        compiled-shaders
                             :dir            dir
                             :mesh-file      "head01.md2"
                             :animation-file "head-animation.lisp"
                             :texture-file   "head-texture.tga"
                             :tags-file      nil
                             :resource-path  resource-path)))
    (setf (interfaces:compiled-shaders body) compiled-shaders
          (interfaces:compiled-shaders head) compiled-shaders)
    (setf (compiled-shaders (array-mixer body)) compiled-shaders)
    (setf (compiled-shaders (array-mixer head)) compiled-shaders)
    (set-animation body :stand :recalculate t)
    (set-animation head :stand :recalculate t)
    (setf (md2:tag-key-parent head) md2:+tag-head-key+)
    (mtree-utils:add-child body head)
    (setf (ghost body) ghost)
    (setf (thinker (ghost body)) t)
      ;;;;;;;;;;;;;;;;;;;;;;;; test
    (let ((forged-potion              (forged-potion))
          (forged-potion-cure-dmg     (forged-potion-cure-dmg))
          (forged-potion-cure-berserk (forged-potion-cure-berserk))
          (forged-ring                (forged-ring))
          (forged-sword               (forged-sword))
          (forged-bow                 (forged-bow))
          (forged-spear               (forged-spear))
          (forged-trap                (forged-trap)))
      ;; (game-event:register-for-end-turn forged-potion)
      ;; (game-event:register-for-end-turn forged-potion-cure-dmg)
      ;; (game-event:register-for-end-turn forged-potion-cure-berserk)
      ;; (game-event:register-for-end-turn forged-ring)
      ;; (game-event:register-for-end-turn forged-bow)
      (add-to-inventory (ghost body) forged-potion)
      (add-to-inventory (ghost body) forged-potion-cure-dmg)
      (add-to-inventory (ghost body) forged-potion-cure-berserk)
      (add-to-inventory (ghost body) forged-ring)
      (add-to-inventory (ghost body) forged-bow)
      (add-to-inventory (ghost body) forged-sword)
      (add-to-inventory (ghost body) forged-spear)
      (add-to-inventory (ghost body) forged-trap)
      (setf (movement-points (ghost body)) 50.0)
      (setf (spell-points    (ghost body)) 40.0)
      ;; note:   wear-item-event  will   not  be   catched  as   the
      ;; registration happens when the entity is added to world
      (wear-item body forged-sword))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    body))

(alexandria:define-constant +magic-num-md2-tag-file '(74 68 80 50) :test #'equalp)

(alexandria:define-constant +tag-file-name-size+ 64 :test #'=)

(define-condition md2-tag-error (text-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~a" (text condition)))))

(defclass md2-tag () ())

(misc:define-offset-size md2-mesh tag (id 0 4) (version 4 4) (num-tags 8 4)
                         (num-frames 12 4) (offset-names 16 4) (offset-tags 20 4)
                         (offset-end 24 4) (offset-extract-end 28 4))

(misc:define-parse-header-chunk (tag-id +tag-id-offset+ +tag-id-size+ md2-tag nil))

(misc:define-parse-header-chunk (tag-num +tag-num-tags-offset+ +tag-num-tags-size+ md2-tag nil))

(misc:define-parse-header-chunk (frames-num +tag-num-frames-offset+ +tag-num-frames-size+
                                            md2-tag nil))

(misc:define-parse-header-chunk (offset-names +tag-offset-names-offset+
                                              +tag-offset-names-size+
                                              md2-tag nil))

(misc:define-parse-header-chunk (offset-tags +tag-offset-tags-offset+
                                              +tag-offset-tags-size+
                                              md2-tag nil))

(defclass md2-mesh-shell (md2-mesh) ())

(defmethod destroy ((object md2-mesh-shell))
  #+debug-mode (misc:dbg "destroy md2 mesh shell ~a ~a"
                         (id object)
                         (map 'list #'id (frames object)))
  (setf (frames object) nil)) ; useless

(defmethod prepare-for-rendering ((object md2-mesh-shell))
  (with-accessors ((vbo vbo) (vao vao)
                   (renderer-data-vertices renderer-data-vertices)
                   (renderer-data-normals renderer-data-normals)
                   (renderer-data-texture renderer-data-texture)
                   (texture-object texture-object)) object
    (setf vbo (gl:gen-buffers +vbo-count+)
          vao (gl:gen-vertex-arrays +vao-count+))
    (%alloc-arrays object)
    (bind-vbo object nil)))

(defmethod fill-mesh-data ((object md2-mesh-shell) (source md2-mesh))
  (with-accessors ((vao-host vao) (vbo-host vbo) (texture-host texture-object)
                   (normal-map-host normal-map)  (triangles-host triangles)
                   (children-host children)
                   (material-params-host material-params)
                   (compiled-shaders-host compiled-shaders)
                   (frames frames)
                   (triangles triangles)
                   (animation-table animation-table)
                   (material-params material-params)
                   (aabb-host aabb)
                   (tags-table     tags-table)
                   (tags-matrices  tags-matrices)
                   (tag-key-parent tag-key-parent)) object
    (setf triangles             (triangles        source)
          frames                (frames           source)
          animation-table       (animation-table  source)
          vao-host              (vao              source)
          vbo-host              (vbo              source)
          texture-host          (texture-object   source)
          normal-map-host       (normal-map       source)
          triangles-host        (triangles        source)
          material-params-host  (material-params  source)
          compiled-shaders-host (compiled-shaders source)
          tags-table            (tags-table       source)
          tags-matrices         (tags-matrices    source)
          tag-key-parent        (tag-key-parent   source))
    (do-children-mesh (child source)
      (let ((new-child (make-instance 'md2-mesh-shell)))
        (fill-mesh-data new-child child)
        (mtree-utils:add-child object new-child)))
      object))

(defmethod fill-mesh-data-w-renderer-data ((object md2-mesh-shell) (source md2-mesh))
  ;; md2-mesh has not rendering data on its own, in a sense.
  ;; And we should not share them too.
  (fill-mesh-data object source))

(defmethod load-texture ((object md2-mesh-shell) file)
  (multiple-value-bind (texture errors)
      (texture:get-texture file)
    (if (null errors)
        (progn
          (setf (texture-object object) texture)
          (when (texture:initializedp texture)
            (texture:prepare-for-rendering (texture-object object)))
        (mapc #'(lambda (e) (push-errors object e)) errors)))))

(defun %old-state (entity)
  (with-accessors ((state state)) entity
    (let* ((pos       (map-utils:pos->game-state-pos entity))
           (element   (matrix:matrix-elt (game-state:map-state state)
                                         (elt pos 1) ; row <-> y
                                         (elt pos 0)))
           (old-state (game-state:old-state-element element)))
      (and old-state
           (typep old-state 'game-state:map-state-element)
           (game-state:old-state-element element)))))

(defmethod step-in-trap-p ((object md2-mesh-shell))
  (with-accessors ((state state)) object
    (let* ((old-state (%old-state object)))
      (and old-state
           (eq (game-state:el-type old-state)
               game-state:+trap-type+)
           old-state))))

(defmethod trap-ostile-p ((object md2-mesh-shell))
  (with-accessors ((state state)) object
    (if (step-in-trap-p object)
        (let* ((old-state (%old-state object))
               (trap      (and old-state
                               (game-state:find-entity-by-id state
                                                             (game-state:entity-id old-state)))))
          (and trap
               (not (eq (my-faction object)
                        (my-faction trap)))
               trap)))))

(defmethod place-trap ((object md2-mesh-shell) trap-ghost)
  (with-accessors ((state state)) object
    (game-state:with-world (world state)
      (when (trap-can-be-placed-p object)
        (let* ((choosen   (random-elt (world:traps-bag world)))
               (shell     (mesh:fill-shell-from-mesh-w-renderer-data choosen
                                                                     'mesh:trap-mesh-shell))
               (h         (game-state:approx-terrain-height@pos state
                                                                (elt (pos object) 0)
                                                                (elt (pos object) 2)))
               (pos-shell (sb-cga:vec (elt (pos object) 0)
                                      h
                                      (elt (pos object) 2))))
          (setf (pos              shell) pos-shell
                (my-faction       shell) (md2-mesh:my-faction object)
                (ghost            shell) trap-ghost
                (compiled-shaders shell) (compiled-shaders object))
          (game-event:register-for-deactivate-trap-event shell)
          (world:push-trap-entity world shell)
          (decrement-move-points-place-trap object)
          (send-refresh-toolbar-event))))))

(defmethod next-move-position ((object md2-mesh) (strategy (eql +explore-strategy+)))
  (with-accessors ((state state)) object
    (with-accessors ((blackboard blackboard:blackboard)) state
        (ai-utils:next-explore-position blackboard object))))

(defmethod build-player-path ((object md2-mesh) to-pos)
  "to-pos in cost coordinates (ivec2)"
  (assert (ivec2p to-pos))
  (with-accessors ((ghost ghost)
                   (state state)
                   (pos pos)) object
    (let* ((cost-player-pos      (calculate-cost-position object))
           (cost-destination     (game-state:get-cost state
                                                      (elt to-pos 0)
                                                      (elt to-pos 1)))
           (ghost                (entity:ghost object))
           (min-cost             (map-utils:map-manhattam-distance-cost to-pos
                                                                        cost-player-pos))
           (player-movement-points (current-movement-points ghost)))
      (if (and (>= player-movement-points +open-terrain-cost+)
               (<= min-cost               player-movement-points)
               (<= cost-destination       player-movement-points))
          (multiple-value-bind (path cost)
              (game-state:build-movement-path state
                                              cost-player-pos
                                              to-pos)
            (if (and path
                     (<= cost player-movement-points))
                (game-state:make-movement-path path cost)
                nil))
          nil))))

(defmethod validate-player-path ((object md2-mesh) path)
  (with-accessors ((ghost ghost)
                   (state state)
                   (pos pos)) object
    (let* ((cost-player-pos        (calculate-cost-position object))
           (ends                   (alexandria:last-elt path))
           (cost-destination       (game-state:get-cost state
                                                        (elt ends 0)
                                                        (elt ends 1)))
           (ghost                  (entity:ghost object))
           (min-cost               (map-utils:map-manhattam-distance-cost ends
                                                                          cost-player-pos))
           (player-movement-points (current-movement-points ghost)))
      (if (and (>= player-movement-points +open-terrain-cost+)
               (<= min-cost               player-movement-points)
               (<= cost-destination       player-movement-points))
          (multiple-value-bind (path cost)
              (game-state:build-movement-path state
                                              cost-player-pos
                                              ends)
            (if (and path
                     (<= cost player-movement-points))
                (game-state:make-movement-path path cost)
                nil))
          nil))))

(defparameter *md2-mesh-factory-db* '())

(defun clean-db ()
  (map 'nil #'destroy *md2-mesh-factory-db*)
  (setf *md2-mesh-factory-db* nil))

(defun %db-equal-fn (dir mesh-file resource-path)
  #'(lambda (a)
      (and
       (string=    dir           (dir           (fs-resources a)))
       (string=    mesh-file     (mesh-file     (fs-resources a)))
       (tree-equal resource-path (resource-path (fs-resources a)) :test #'string=))))

(defun get-md2-shell (&key
                        (shaders        nil)
                        (material       (make-mesh-material .8 1.0 0.1 0.0 128.0))
                        (dir            nil)
                        (mesh-file      "body01.md2")
                        (animation-file "body-animation.lisp")
                        (texture-file   "body-texture.tga")
                        (tags-file      "body01.tag")
                        (resource-path  nil))
  (let* ((res (find-if (%db-equal-fn dir mesh-file resource-path) *md2-mesh-factory-db*)))
    (if res
        (let ((shell (fill-shell-from-mesh-w-renderer-data res 'md2-mesh-shell)))
          (setf (fs-resources shell)
                (make-md2-fs-res :dir             dir
                                 :mesh-file       mesh-file
                                 :animation-file  animation-file
                                 :texture-file    texture-file
                                 :tags-file       tags-file
                                 :resource-path   resource-path))
          (setf (material-params shell) material)
          (%load-common-md2-parameters shell dir resource-path tags-file
                                       animation-file texture-file)
          ;; NOTE just for testing purpose! ;;;;;;;;;;;;;;;;;;;;;
          (%%transform-model shell)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (prepare-for-rendering shell)
          shell)
        (let ((model (load-md2-model dir
                                     :shaders        shaders
                                     :material       material
                                     :mesh-file      mesh-file
                                     :texture-file   texture-file
                                     :animation-file animation-file
                                     :tags-file      tags-file
                                     :resource-path  resource-path)))
          (push model *md2-mesh-factory-db*)
          (get-md2-shell :dir            dir
                         :material       material
                         :mesh-file      mesh-file
                         :texture-file   texture-file
                         :animation-file animation-file
                         :tags-file      tags-file
                         :resource-path  resource-path)))))
