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

(in-package :sprite-character)

(alexandria:define-constant +fow-standard-dispel-size+        4   :test #'=)

(alexandria:define-constant +sprite-window-size+             64   :test #'=)

(alexandria:define-constant +sprite-world-size+              10.0 :test #'=)

(alexandria:define-constant +sprite-default-animation-speed+  0.5 :test #'=)

(alexandria:define-constant +sprite-anim-north-dir+           0 :test #'=)

(alexandria:define-constant +sprite-anim-west-dir+            1 :test #'=)

(alexandria:define-constant +sprite-anim-south-dir+           2 :test #'=)

(alexandria:define-constant +sprite-anim-east-dir+            3 :test #'=)

(alexandria:define-constant +default-animation-table+
    '((:stand        ((8  9)
                      (9  9)
                      (10 9)
                      (11 9)))
      (:move         ((8  9)
                      (9  9)
                      (10 9)
                      (11 9)))
      (:melee        ((12 6)
                      (13 6)
                      (14 6)
                      (15 6)))
      (:long-range   ((16 10)
                      (17 10)
                      (18 10)
                      (19 10)))
      (:attack-spell ((0 7)
                      (1 7)
                      (2 7)
                      (3 7)
                      (4 7)))
      (:spell        ((0 7)
                      (1 7)
                      (2 7)
                      (3 7)
                      (4 7)))
      (:pole         ((4 8)
                      (5 8)
                      (6 8)
                      (7 8)))
      (:bored        ((4 8)
                      (5 8)
                      (6 8)
                      (7 8)))
      (:pain         ((20 3)
                      (20 3)
                      (20 3)
                      (20 3)))
      (:death        ((20 6)
                      (20 6)
                      (20 6)
                      (20 6))))
  :test #'equalp)

(defclass sprite-fs-res ()
  ((dir
    :initform nil
    :initarg  :dir
    :accessor dir)
   (spritesheet-file
    :initform "spritesheet.tga"
    :initarg  :spritesheet-file
    :accessor spritesheet-file)
   (resource-path
    :initform  nil
    :initarg   :resource-path
    :accessor  resource-path)))

(defmethod marshal:class-persistant-slots ((object sprite-fs-res))
  '(dir
    spritesheet-file
    resource-path))

(defmethod clone-into :after ((from sprite-fs-res) (to sprite-fs-res))
  (setf (dir              to) (copy-seq (dir              from))
        (spritesheet-file to) (copy-seq (spritesheet-file from))
        (resource-path  to)   (copy-seq (resource-path  from)))
  to)

(defmethod clone ((object sprite-fs-res))
  (with-simple-clone (object 'sprite-fs-res)))

(defun make-sprite-fs-res (&key
                             (dir            nil)
                             (spritesheet-file "spritesheet.tga")
                             (resource-path  nil))
  (make-instance 'sprite-fs-res
                 :dir              dir
                 :spritesheet-file spritesheet-file
                 :resource-path    resource-path))

(defclass sprite-mesh (able-to-see-mesh keyframe-trigger inner-animation)
  ((fs-resources
    :initform (make-sprite-fs-res)
    :initarg  :fs-resources
    :accessor fs-resources)
   (active-animation-row
    :initform 0
    :initarg  :active-animation-row
    :accessor active-animation-row)
   (active-animation-column
    :initform 0
    :initarg  :active-animation-column
    :accessor active-animation-column)
   (active-animation-count
    :initform 0
    :initarg  :active-animation-count
    :accessor active-animation-count)
   (animation-rows-count
    :initform 0
    :initarg  :animation-rows-count
    :accessor animation-rows-count)
   (texture-window-width
    :initform 1.0
    :initarg  :texture-window-width
    :accessor texture-window-width)
   (texture-window-height
    :initform 1.0
    :initarg  :texture-window-height
    :accessor texture-window-height)
   (animation-table
    :initform +default-animation-table+
    :initarg :animation-table
    :accessor animation-table)
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
    :initarg  :current-action
    :accessor current-action)
   (current-animation-time
    :initform 0
    :initarg :current-animation-time
    :accessor current-animation-time
    :type single-float)
   (fps
    :initform 20.0
    :initarg  :fps
    :accessor fps
    :type single-float)
   (orb
    :initform nil
    :initarg  :orb
    :accessor orb)))

(defun setup-orb (mesh)
  (with-slots (aabb) mesh
    (with-accessors ((orb              orb)
                     (compiled-shaders compiled-shaders)) mesh
      (with-accessors ((aabb-p2 aabb-p2)) aabb
        (setf orb (status-orb:get-orb aabb-p2
                                      compiled-shaders
                                      status-orb:+texture-active+ t t))
        (mtree:add-child mesh orb)))))

(defalias attach-orb #'setup-orb)

(defun setup-orb-texture (mesh texture-name)
  (with-accessors ((orb orb)) mesh
    (when orb
      (setf (texture-object orb) (texture:get-texture texture-name)))))

(defun orb-active (mesh)
  (setup-orb-texture mesh status-orb:+texture-active+))

(defun orb-inactive (mesh)
  (setup-orb-texture mesh status-orb:+texture-inactive+))

(defun orb-remove (mesh)
  (setf (orb mesh) nil))

(defmethod initialize-instance :after ((object sprite-mesh) &key &allow-other-keys)
  (with-accessors ((texture-object        texture-object)
                   (texture-window-width  texture-window-width)
                   (texture-window-height texture-window-height)
                   (animation-rows-count  animation-rows-count)) object
    (let ((texture-height (pixmap:height texture-object))
          (texture-width  (pixmap:width  texture-object)))
      (setf texture-window-height (d (/ +sprite-window-size+ texture-height)))
      (setf texture-window-width  (d (/ +sprite-window-size+ texture-width)))
      (setf animation-rows-count  (/ texture-height +sprite-window-size+))
      ;;  - c +-------+ d
      ;;  |   |\      |     c has coordinates (s, 1.0)
      ;;  |   | \ T2  |
      ;;h |   |  \    |
      ;;  |   |   \   |
      ;;  |   |    \  |      ^ t
      ;;  |   | T1  \ |      |
      ;;  |   |      \|      |    s
      ;;  -   +-------+      +---->
      ;;      a        b
      (let ((tex-w texture-window-width)
            (tex-h texture-window-height))
        (quad-w-explicit-texture-coords object
                                        +sprite-world-size+
                                        +sprite-world-size+
                                        (vector (vec2 0.0   0.0)    ; a
                                                (vec2 tex-w 0.0)    ; b
                                                (vec2 0.0   tex-h)  ; c
                                                (vec2 tex-w 0.0)    ; b
                                                (vec2 tex-w tex-h)  ; d
                                                (vec2 0.0   tex-h)) ; c
                                        +zero-vec+ nil t)))))

(defmethod print-object ((object sprite-mesh) stream)
  (with-accessors ((id    id)
                   (ghost ghost)) object
    (print-unreadable-object (object stream :type t :identity nil)
      (if ghost
          (format stream "~a ~a ~a" id        (my-faction object) (character:player-class ghost))
          (format stream "~a ~a no-class" id  (my-faction object))))))

(defmethod character:pclass-of-useful-in-attack-tactic-p ((object sprite-mesh))
  (character:pclass-of-useful-in-attack-tactic-p (ghost object)))

(defmethod on-game-event ((object sprite-mesh) (event game-event:game-interrupt-terminated-event))
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
         (when (with-no-terror-status (,object)
                 (with-no-berserk-status (,object)
                   (and (not (entity:reply-attack-p ,attacked-by-entity)) ; avoid 'ping pong'
                        (dice:pass-d100.0 ,chance))))
           ;; attack!
           (game-state:with-world (,world ,state)
             (let ((,weapon-short-range (weapon-type-short-range ,ghost))
                   (,weapon-long-range  (weapon-type-long-range  ,ghost)))
               (cond
                 (,weapon-short-range
                  (setf (entity:reply-attack ,object) t)
                  (battle-utils:attack-short-range ,world ,object ,attacked-by-entity))
                 (,weapon-long-range
                  ;; an additional test for long range weapon
                  (when (dice:pass-d100.0 (d* 1.2 ,chance))
                    (setf (entity:reply-attack ,object) t)
                    (battle-utils:attack-long-range ,world ,object ,attacked-by-entity)))))))
         ,@body))))

(defmethod on-game-event ((object sprite-mesh) (event end-attack-melee-event))
  (with-end-attack-event (object event attacked-by-entity)
    (with-maybe-reply-attack (object attacked-by-entity)
      (with-remove-interrupt-character-plan))))

(defmethod on-game-event ((object sprite-mesh) (event end-attack-long-range-event))
  (with-end-attack-event (object event attacked-by-entity)
    (with-maybe-reply-attack (object attacked-by-entity)
      (with-remove-interrupt-character-plan))))

(defmethod on-game-event ((object sprite-mesh) (event end-attack-spell-event))
  (with-end-attack-event (object event attacked-by-entity) ;; no reply to spell
    (setf (spell-loaded (entity:ghost object)) nil)))

(defmethod on-game-event ((object sprite-mesh) (event end-spell-event))
  (with-end-attack-event (object event attacked-by-entity) ;; no reply to spell
    (setf (spell-loaded (entity:ghost object)) nil)))

(defmethod on-game-event ((object sprite-mesh) (event end-defend-from-attack-spell-event))
  (with-end-attack-event (object event attacked-by-entity) ;; no reply to spell
    (with-remove-interrupt-character-plan)))

(defmethod on-game-event ((object sprite-mesh) (event end-defend-from-spell-event))
  (with-end-attack-event (object event attacked-by-entity) ;; no reply to spell
    (with-remove-interrupt-character-plan)))

(defun manage-occurred-damage (entity
                               event
                               damage
                               ambushp
                               register-for-end-attack-fn
                               send-end-attack-event-fn
                               weapon)
  #+(and debug-mode god-mode)
  (when (faction-player-p (state entity) (id entity))
    (setf damage nil))
  (when ambushp
    (billboard:enqueue-tooltip entity
                               billboard:+tooltip-surprise-attack-char+
                               :color                 billboard:+damage-color+
                               :font-type             gui:+tooltip-font-handle+
                               :add-only-if-renderd-p t))
  (apply-damage entity damage) ;; it is ok for damage to be nil.
  (setf (attacked-by-entity entity) (attacker-entity event))
  (funcall register-for-end-attack-fn entity)
  (if damage
      (battle-utils:send-effects-after-attack (attacker-entity event) entity :weapon weapon)
      (funcall send-end-attack-event-fn entity)))

(defmethod on-game-event ((object sprite-mesh) (event attack-melee-event))
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

(defmethod on-game-event ((object sprite-mesh) (event attack-long-range-event))
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

(defmethod on-game-event ((object sprite-mesh) (event attack-spell-event))
  (check-event-targeted-to-me (object event)
      (with-accessors ((state state)
                       (id id)) object
        (multiple-value-bind (damage ambush)
            (battle-utils:defend-from-attack-spell event)
          (let ((faction-launcher (my-faction (find-entity-by-id state (id-origin event)))))
            (when (and damage
                       (faction-player-p faction-launcher)
                       ;; ignore  spell if  launched  by friends  that's
                       ;; concerning but from another point of view! :)
                       (not (eq (my-faction object)
                                faction-launcher)))
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
            t)))))

(defmethod on-game-event ((object sprite-mesh) (event spell-event))
  (check-event-targeted-to-me (object event)
    (when (battle-utils:defend-from-spell event)
      (setf (attacked-by-entity object) (attacker-entity event))
      (game-event:register-for-end-attack-spell-event object)
      (battle-utils:reward-exp-launch-spell (attacker-entity event) object (spell event))
      t)))

(defun event-can-render-character-seen-p (event)
  (find event (list 'game-event:open-door-event
                    'game-event:move-entity-entered-in-tile-event)
        :test #'(lambda (a b) (typep a b))))

(defmethod on-game-event ((object sprite-mesh) (event update-visibility))
  (with-accessors ((state state)
                   (id id)) object
    (labels ((stop-movements (already-stopped interrupt-if-ai interrupting-id)
               (when (not already-stopped)
                 (%stop-movement object
                                 :decrement-movement-points t
                                 :interrupt-plan-if-ai      interrupt-if-ai
                                 :interrupting-id           interrupting-id)))
             (seenp (entity &key
                            (saved-render-p nil)
                            (maintain-render nil)
                            (interrupt-plan-if-ai t))
               (let ((already-stopped nil))
                 (when (find-if #'(lambda (a) (= id (id a))) (visible-players entity))
                   (when (not maintain-render)
                     (setf (renderp object) t))
                   (when (and (event-can-render-character-seen-p (from-event event))
                              (current-path (ghost object))
                              (not (eq (my-faction object) (my-faction entity))))
                     (when (not saved-render-p)
                       (setf already-stopped t)
                       (stop-movements nil interrupt-plan-if-ai (id entity)))
                     (when (dice:pass-d100.0 (actual-ambush-attack-chance (ghost entity)))
                       (cond
                         ((battle-utils:long-range-attack-possible-p entity object)
                          (game-state:with-world (world state)
                            (stop-movements already-stopped t (id entity))
                            (battle-utils:attack-long-range world entity object)))
                         ((battle-utils:short-range-attack-possible-p entity object)
                          (game-state:with-world (world state)
                            (stop-movements already-stopped t (id entity))
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
                                                             :saved-render-p  saved-renderp)))
                  (seep object))
                (progn
                  (game-state:loop-ai-entities state
                                              #'(lambda (v)
                                                  (seenp v
                                                         :maintain-render t
                                                         :saved-render-p  saved-renderp)))
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

(defmethod on-game-event ((object sprite-mesh) (event move-entity-along-path-event))
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
            (update-visibility-cone object)
            ;;(send-update-visibility-event object event)
            t)
          nil))))

(defun maybe-set-inactive-orb (player)
  "note: ai player has no orbs but the function 'orb-inactive is going
to take care of that"
  (with-accessors ((ghost ghost)) player
    (when (< (current-movement-points ghost)
             +open-terrain-cost+)
      (orb-inactive player))))

(defun %stop-movement-ai (player end-event interrupting-id interrupt-plan-if-ai-p)

  (with-accessors ((ghost ghost)
                   (state state)) player
    (declare (ignorable state))
    #+debug-mode (assert (faction-ai-p state (id player)))
    (if interrupt-plan-if-ai-p
        (progn
          (put-in-working-memory player +w-memory-interrupting-by-id+ interrupting-id)
          (misc:dbg "stop movement from ~a" interrupting-id)
          (when (not (ignore-interrupt-p player))
            (misc:dbg "interrupt from ~a" interrupting-id)
            (set-interrupt-plan ghost)
            (propagate-move-entity-along-path-end-event end-event)))
        (propagate-move-entity-along-path-end-event end-event))
    player))

(defun %stop-movement (player &key
                                (decrement-movement-points t)
                                (interrupt-plan-if-ai      t)
                                (interrupting-id           nil))
  (with-accessors ((ghost ghost)
                   (dir dir)
                   (id id)
                   (state state)) player
    (with-accessors ((current-path current-path)) ghost
      (let ((end-event (make-instance 'move-entity-along-path-end-event
                                      :id-origin id
                                      :tile-pos  (alexandria:last-elt current-path))))
        (when decrement-movement-points
          (decrement-move-points-entering-tile player)
          (maybe-set-inactive-orb player))
        (if (faction-ai-p state (id player))
            (%stop-movement-ai player end-event interrupting-id interrupt-plan-if-ai)
            (propagate-move-entity-along-path-end-event end-event))
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
          (set-memory-seen-before-door player)
          (game-event:propagate-open-door-event door-event)
          (send-update-visibility-event player door-event))))))

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
          (world:move-character world player leaving-tile)
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
                              :interrupt-plan-if-ai      t
                              :interrupting-id           trap-ostile)
              (%try-deactivate-trap-from-ai world player trap-ostile)))
          (send-update-visibility-event player event)
          ;; FOW
          (if (thrown-down-in-fow-p state :x (ivec2-x pos-entity) :y (ivec2-y pos-entity))
              (throw-down-in-fow player)
              (popup-from-fow    player))
          (send-refresh-toolbar-event))))))

(defmethod on-game-event ((object sprite-mesh) (event move-entity-entered-in-tile-event))
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
                (world:move-character world object leaving-tile)
                (propagate-update-highlight-path (make-instance 'update-highlight-path
                                                                :tile-pos current-path))
                ;; update-cone for ai visibility check below
                (update-visibility-cone object)
                ;; remove fow
                (popup-from-fow object)
                ;; traps
                (let ((trap-ostile (trap-ostile-p object)))
                  (when trap-ostile
                    (%stop-movement object :decrement-movement-points t)
                    (%try-deactivate-trap world object trap-ostile)))
                (send-update-visibility-event object event)
                (send-refresh-toolbar-event :reset-time-warning-enemy-met t)))))))
  nil)

(defun fow-affected-tiles (game-state x y size)
  (matrix:gen-valid-neighbour-position-in-box (game-state:map-state game-state)
                                              x y size size :add-center nil))

(defun calculate-dispel-fow-size (mesh)
  (with-accessors ((ghost ghost)) mesh
    (if (elm-worn-p ghost)
        (truncate (/ +fow-standard-dispel-size+ 2))
        +fow-standard-dispel-size+)))

(defmethod popup-from-fow :before ((object sprite-mesh)
                                   &key
                                   (size (calculate-dispel-fow-size object))
                                     &allow-other-keys)
  (with-accessors ((state state)
                   (id    id)) object
    (when (faction-player-p state id)
      (with-player-cost-pos-ivec2 (object pos)

        (2d-utils:displace-2d-vector (pos x y)
          (let ((tiles (fow-affected-tiles state x y size)))
            (loop for tile across tiles do
                 (2d-utils:displace-2d-vector (tile tile-x tile-y)
                   (when (thrown-down-in-fow-p state :x tile-x :y tile-y)
                     (popup-from-fow state
                                     :size 0
                                     :x tile-x
                                     :y tile-y
                                     :update-gpu-texture nil)
                     (when-let ((entity (and (valid-id-p (game-state:entity-id-in-pos state
                                                                                      tile-x
                                                                                      tile-y))
                                             (game-state:entity-in-pos state tile-x tile-y))))
                       (when (/= id (id entity))
                         (popup-from-fow  entity)
                         (when (parent-labyrinth entity)
                           (prepare-for-rendering (parent-labyrinth entity)))))))))
            (popup-from-fow state :size 0 :x x :y y :update-gpu-texture nil))
        (update-for-rendering (texture-fow state))))))

(defmethod throw-down-in-fow ((object sprite-mesh) &key &allow-other-keys)
  (with-accessors ((state state)
                   (id    id)) object
    (when (faction-ai-p state id)
      (call-next-method))
    object))

(defmethod on-game-event ((object sprite-mesh) (event move-entity-along-path-end-event))
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

(defmethod on-game-event ((object sprite-mesh) (event rotate-entity-cw-event))
  (with-accessors ((ghost ghost)
                   (dir dir)
                   (pos pos)) object
    (if (= (id object) (id-destination event))
        (when (> (current-movement-points ghost) 0)
          (decrement-move-points-rotate object)
          (maybe-set-inactive-orb object)
          (setf (current-action object) :rotate)
          (setf dir (sb-cga:transform-direction dir (rotate-around +y-axe+ (d- +pi/2+))))
          (update-visibility-cone object)
          ;; test
          ;;(misc:dbg "visibility test: ~a" (mapcar #'id (visible-players object)))
          (send-update-visibility-event object event)
          (send-refresh-toolbar-event :reset-time-warning-enemy-met t)
          t)
        nil)))

(defmethod on-game-event ((object sprite-mesh) (event rotate-entity-ccw-event))
  (with-accessors ((ghost ghost)
                   (dir dir)) object
    (if (= (id object) (id-destination event))
        (when (or (not (decrement-movement-points-p event))
                  (> (current-movement-points ghost) 0))
          (when (decrement-movement-points-p event)
            (decrement-move-points-rotate object)
            (maybe-set-inactive-orb object))
          (setf (current-action object) :rotate)
          (setf dir (sb-cga:transform-direction dir (rotate-around +y-axe+ +pi/2+)))
          (update-visibility-cone object)
          ;;(misc:dbg "visibility test: ~a" (visible-players object))
          (send-update-visibility-event object event)
          (send-refresh-toolbar-event :reset-time-warning-enemy-met t)
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
                  (dice:pass-d1.0 (random-object-messages:msg-chance event-data))
                  (not ,immune-status-accessor)
                  (null status)) ;; ensure player  can not  suffers
                                 ;; more than one abnormal status
             (progn
               ,@body)
             nil)))))

(defmethod on-game-event ((object sprite-mesh) (event cause-poisoning-event))
  (with-accessors ((ghost ghost)
                   (id id)
                   (state state)) object
    (with-accessors ((recurrent-effects recurrent-effects)
                     (immune-poison-status immune-poison-status)
                     (status status)) ghost
      (with-accessors ((event-data event-data)) event
        (if (and (= id (id-destination event))
                 (dice:pass-d1.0 (random-object-messages:msg-chance event-data))
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
                                           :color                 billboard:+poison-damage-color+
                                           :duration              billboard:+tooltip-slow-duration+
                                           :animation-speed
                                           billboard:+tooltip-slow-anim-speed+
                                           :font-type             gui:+tooltip-font-handle+
                                           :add-only-if-renderd-p t)
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
                                      :duration              billboard:+tooltip-slow-duration+
                                      :animation-speed       billboard:+tooltip-slow-anim-speed+
                                      :color                 billboard:+poison-damage-color+
                                      :font-type             gui:+tooltip-font-handle+
                                      :add-only-if-renderd-p t)
                (send-refresh-toolbar-event :reset-health-status-animation t))
         ,@body))))

(defmethod on-game-event ((object sprite-mesh) (event cause-terror-event))
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

(defmethod on-game-event ((object sprite-mesh) (event cause-berserk-event))
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

(defmethod on-game-event ((object sprite-mesh) (event cause-faint-event))
  (with-simple-cause-status (object event immune-berserk-status +status-berserk+
                                    billboard:+tooltip-faint-char+)
    (check-event-targeted-to-me (object event)
      (set-death-status object))))

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
                                         :color                 billboard:+healing-color+
                                         :font-type             gui:+tooltip-font-handle+
                                         :add-only-if-renderd-p t)
              (setf status nil) ;; note: the player can be affected only by one at a time
              (funcall action-fn mesh ghost)
              (send-refresh-toolbar-event)
              t)
            nil)))))

(defmethod on-game-event ((object sprite-mesh) (event cure-poisoning-event))
  (%common-cure-status-events object event
                              #'(lambda (mesh ghost)
                                  (declare (ignore mesh))
                                  (setf (recurrent-effects ghost)
                                        (delete-if #'(lambda (a)
                                                       (typep a
                                                              'random-object-messages:cause-poison-msg))
                                                   (recurrent-effects ghost))))))

(defmethod on-game-event ((object sprite-mesh) (event cure-terror-event))
  (%common-cure-status-events object event))

(defmethod on-game-event ((object sprite-mesh) (event cure-berserk-event))
  (%common-cure-status-events object event))

(defmethod on-game-event ((object sprite-mesh) (event cure-faint-event))
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

(defmethod on-game-event ((object sprite-mesh) (event cancel-terror-event))
  (with-common-cancel-event object event +status-terror+ *terror-recover*))

(defmethod on-game-event ((object sprite-mesh) (event cancel-berserk-event))
  (with-common-cancel-event object event +status-berserk+ *berserk-recover*))

(defmethod on-game-event ((object sprite-mesh) (event cancel-faint-event))
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

(defmethod on-game-event ((object sprite-mesh) (event cancel-immune-berserk-event))
  (with-common-cancel-immune-event object event immune-berserk-status *cancel-immune-berserk*))

(defmethod on-game-event ((object sprite-mesh) (event cancel-immune-faint-event))
  (with-common-cancel-immune-event object event immune-faint-status *cancel-immune-faint*))

(defmethod on-game-event ((object sprite-mesh) (event cancel-immune-terror-event))
  (with-common-cancel-immune-event object event immune-terror-status *cancel-immune-terror*))

(defmethod on-game-event ((object sprite-mesh) (event cancel-immune-poisoning-event))
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
                                      :color                 billboard:+poison-damage-color+
                                      :font-type             gui:+tooltip-font-handle+
                                      :duration              billboard:+tooltip-slow-duration+
                                      :animation-speed       billboard:+tooltip-slow-anim-speed+
                                      :add-only-if-renderd-p t)
           (send-refresh-toolbar-event :reset-health-status-animation t))
         ,@body))))

(defmethod on-game-event ((object sprite-mesh) (event immune-berserk-event))
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

(defmethod on-game-event ((object sprite-mesh) (event immune-terror-event))
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

(defmethod on-game-event ((object sprite-mesh) (event immune-faint-event))
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

(defmethod on-game-event ((object sprite-mesh) (event immune-poisoning-event))
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

(defmethod on-game-event ((object sprite-mesh) (event heal-damage-event))
  (with-accessors ((ghost ghost)
                   (id id)
                   (state state)) object
    (with-accessors ((current-damage-points current-damage-points)
                     (damage-points damage-points)
                     (status status)) ghost
      (with-accessors ((event-data event-data)) event
        (if (and (= id (id-destination event))
                 (dice:pass-d1.0 (random-object-messages:msg-chance event-data))
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
                                         :color                 billboard:+healing-color+
                                         :font-type             gui:+tooltip-font-handle+
                                         :add-only-if-renderd-p t)
              (send-refresh-toolbar-event)
              t)
            nil)))))

(defmethod on-game-event ((object sprite-mesh) (event wear-object-event))
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
                         (die-utils:pass-d100.0 (smoothstep-interpolate 0.0
                                                                        100.0
                                                                        (d (smartness ghost)))))
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

(defmethod on-game-event ((object sprite-mesh) (event unwear-object-event))
  (with-accessors ((ghost ghost) (id id) (state state)) object
    (if (= id (id-destination event))
        (progn
          (decrement-move-points-wear object)
          (remove-from-modifiers ghost (id-origin event))
          (send-refresh-toolbar-event)
          t)
        nil)))

(defmethod on-game-event ((object sprite-mesh) (event modifier-object-event))
  (with-accessors ((ghost ghost) (id id) (state state)) object
    (if (= id (id-destination event))
        (progn
          (push (event-data event) (modifiers-effects ghost))
          (send-refresh-toolbar-event)
          t)
        nil)))

(defmethod on-game-event ((object sprite-mesh) (event trap-triggered-event))
  (check-event-targeted-to-me (object event)
    (with-accessors ((state state)) object
      (let* ((trap (game-state:find-entity-by-id state (id-origin event))))
        (battle-utils:trigger-trap-attack trap object)))))

(defmethod on-game-event :after ((object sprite-mesh) (event start-turn))
  (with-accessors ((state state)
                   (ghost ghost)) object
    (game-state:with-world (world state)
      (when (eq (game-state:faction-turn state)
                (my-faction              object))
        (reset-spell-points    ghost)
        (reset-movement-points ghost)
        (traverse-recurrent-effects      object)
        (let ((decayed-items (remove-decayed-items ghost (turn-count event))))
          (dolist (item decayed-items)
            (remove-from-modifiers ghost (id item))
            (world:post-entity-message world object
                                       (format nil
                                               (_ "~a broken")
                                               (description-type item))
                                       nil
                                       (cons (_ "Move to")
                                             (world:point-to-entity-and-hide-cb world object))))))
        nil)))

(defmethod on-game-event :after ((object sprite-mesh) (event end-turn))
  (with-accessors ((ghost ghost)
                   (state state)) object
    ;; REMOVED:
    ;; moved to "game-event:on-game-event ((object world) (event end-turn))"
    ;; clear blacklisted actions for planner
    ;;(character:clear-blacklist ghost)
    ;; (send-refresh-toolbar-event)
    nil))

(defmethod my-faction ((object sprite-mesh))
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

(defmethod apply-damage ((object sprite-mesh) damage &key (tooltip-active-p t))
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
                                       :color                 billboard:+damage-color+
                                       :font-type             gui:+tooltip-font-handle+
                                       :activep               tooltip-active-p
                                       :add-only-if-renderd-p t)
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
                                         :color                 billboard:+damage-color+
                                         :font-type             gui:+tooltip-font-handle+
                                         :activep               tooltip-active-p
                                         :add-only-if-renderd-p t)))))))

(defmethod traverse-recurrent-effects ((object sprite-mesh))
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
                (when (renderp object)
                  (billboard:enqueue-tooltip object
                                             (format nil
                                                     +standard-float-print-format+
                                                     (d- (random-object-messages:msg-damage effect)))
                                             :color                 billboard:+damage-color+
                                             :font-type             gui:+tooltip-font-handle+
                                             :add-only-if-renderd-p t)))))))))

(defmethod process-postponed-messages ((object sprite-mesh))
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

(defmethod set-death-status ((object sprite-mesh))
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
        (game-state:with-world (world state)
          (game-state:select-next-pc state)
          (setf (status ghost) +status-faint+)
          (setf current-action  :death)
          (set-animation object :death :recalculate t)
          (setf stop-animation nil)
          (setf cycle-animation nil)
          (setf current-damage-points (d 0.0))
          ;; also make the blackboard "forget" about this character if is from human side
          (when (faction-player-p state id)
            (orb-remove object)
            (blackboard:remove-entity-from-all-attack-pos blackboard object))
          (setf (renderp object) t)
          (set-interrupt-plan ghost)
          (particles:add-blood-death object (aabb-center aabb) +y-axe+)
          (when (battle-utils:victoryp state)
            (closing-sequence:start-victory-sequence world))
          (when (battle-utils:defeatedp state)
            (closing-sequence:start-game-over-sequence world)))))))

(defmethod unset-death-status ((object sprite-mesh))
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
      (when (faction-player-p state id)
        (setup-orb object))
      (setf (status ghost) nil)
      (setf current-action  :stand)
      (set-animation object :stand :recalculate t)
      (setf stop-animation nil)
      (setf cycle-animation t)))))

(defmethod set-attack-status ((object sprite-mesh))
  (with-accessors ((ghost ghost)
                   (current-action current-action)
                   (cycle-animation cycle-animation)
                   (stop-animation stop-animation)) object
    (with-accessors ((status status)
                     (current-damage-points current-damage-points)) ghost
      (setf current-action  :melee)
      (set-animation object :melee :recalculate t)
      (setf stop-animation nil)
      (setf cycle-animation nil))))

(defmethod set-attack-spell-status ((object sprite-mesh))
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

(defmethod set-spell-status ((object sprite-mesh))
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

(defmethod actual-aabb-for-bullets ((object sprite-mesh))
  (mesh:standard-aabb object))

(defmethod actual-aabb-for-visibility ((object sprite-mesh))
  (actual-aabb-for-bullets object))

(defgeneric push-errors (object the-error))

(defgeneric set-animation (object animation &key recalculate))

(defgeneric calculate-move (object dt))

(defgeneric wear-item      (object item))

(defgeneric step-in-trap-p (object))

(defgeneric trap-ostile-p (object))

(defgeneric place-trap (object trap-ghost))

(defgeneric next-move-position (object strategy))

(defgeneric build-player-path (object to-pos))

(defgeneric validate-player-path (object path))

(defmethod destroy ((object sprite-mesh))
  #+debug-mode (misc:dbg "destroy sprite mesh ~a" (id object))
  (do-children-mesh (child object)
    (destroy object))
  (call-next-method))

(defmethod load-sprite (spritesheet-file)
  (texture:with-prepared-texture (spritesheet spritesheet-file)
    (let* ((mesh (make-instance 'sprite-mesh
                                :texture-object  spritesheet
                                :animation-speed +sprite-default-animation-speed+)))
      mesh)))

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
    (if (and (game-state:invalicable-in-next-path-tile-p state entity current-path 1)
             (not (game-state:door-in-next-path-tile-p        state current-path        1)))
        (%stop-movement entity
                        :decrement-movement-points t
                        :interrupt-plan-if-ai      t
                        :interrupting-id           :wall)
        ;; open door if any and if closed
        (let ((door-opened-p (manage-door-ai state entity current-path 1)))
          (when (not door-opened-p) ;; door was open, the plan has not been interrupted
            (if (and (gconf:config-smooth-movements)
                     (renderp entity))
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

(defmethod calculate-move ((object sprite-mesh) dt)
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

(defmethod wear-item ((object sprite-mesh) item)
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

(defmethod add-to-inventory ((object sprite-mesh) item)
  (add-to-inventory (ghost object) item))

(defun calculate-sprite-orientation (sprite)
  (with-accessors ((state                   state)
                   (pos                     pos)
                   (dir                     dir)
                   (current-action          current-action)
                   (active-animation-column active-animation-column)
                   (active-animation-row    active-animation-row)
                   (active-animation-count  active-animation-count)
                   (animation-table         animation-table)) sprite
    (game-state:with-world (world state)
      (let* ((camera      (world:camera world))
             (camera->pos (let ((diff (vec- pos (pos camera))))
                            (setf (vec-y diff) 0.0)
                            (normalize diff)))
             (cross       (cross-product camera->pos dir))
             (dot         (dot-product   camera->pos dir))
             (anim-spec   (animation-specs animation-table current-action)))
        (flet ((%set-animation (dir)
                 (setf active-animation-row   (animation-starting-row anim-spec dir))
                 (setf active-animation-count (animation-row-count anim-spec dir))))
          (with-epsilon (2e-1)
            (cond
              ((epsilon= dot  1.0)
               (%set-animation +sprite-anim-north-dir+))
              ((epsilon= dot -1.0)
               (%set-animation +sprite-anim-south-dir+))
              ((epsilon= dot  0.0)
               (if (> (vec-y cross) 0)
                   (%set-animation +sprite-anim-west-dir+)
                   (%set-animation +sprite-anim-east-dir+))))))))))

(defmethod calculate ((object sprite-mesh) dt)
  (declare (optimize (debug 0) (speed 3)))
  (declare (single-float dt))
  (with-accessors ((pos                     pos)
                   (dir                     dir)
                   (stop-animation          stop-animation)
                   (cycle-animation         cycle-animation)
                   (current-action          current-action)
                   (active-animation-column active-animation-column)
                   (active-animation-row    active-animation-row)
                   (active-animation-count  active-animation-count)
                   (current-animation-time  current-animation-time)
                   (el-time                 el-time)
                   (animation-speed         animation-speed)
                   (animation-table         animation-table)
                   (fps                     fps)
                   (ghost                   ghost)) object
    (declare (single-float current-animation-time el-time animation-speed fps))
    (declare (fixnum active-animation-column active-animation-count))
    ;; (declare (simple-array frames))
    ;; (declare (string tag-key-parent))
    ;; (declare (list tags-table tags-matrices))
    ;; AI thinking...
    (when (and ghost
               (thinkerp ghost))
      (actuate-strategy object))
    ;; triggers
    (when (= active-animation-column 0)
      (elaborate-start-trigger object current-action))
    (when (= active-animation-column (1- active-animation-count))
      (elaborate-end-trigger object current-action))
    ;; animation
    (ecase current-action
      (:stand
       ;; nothing to do
       )
      (:death
       ;; TODO set death plane
       )
      ((:melee :long-range :pole)
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
    (when (not stop-animation)
      (calculate-sprite-orientation object)
      (setf el-time (d+ el-time (d* animation-speed dt)))
      (let ((next-time  (+ current-animation-time (* animation-speed dt)))
            (frame-freq (/ 1.0 fps)))
        (declare (single-float next-time frame-freq))
        (if (> next-time frame-freq)
            (setf active-animation-column (rem (f+ 1 active-animation-column)
                                               active-animation-count)
                  current-animation-time 0.0)
            (setf current-animation-time next-time))
        (when (and (not cycle-animation)
                   (= active-animation-column (1- active-animation-count)))
          (setf stop-animation t))
        (bubbleup-modelmatrix object)
        (do-children-mesh (i object)
          (calculate i dt))))))

(defmethod render ((object sprite-mesh) renderer)
  ;; (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((projection-matrix        projection-matrix)
                   (compiled-shaders         compiled-shaders)
                   (model-matrix             model-matrix)
                   (triangles                triangles)
                   (scaling                  scaling)
                   (texture-object           texture-object)
                   (vao                      vao)
                   (view-matrix              view-matrix)
                   (active-animation-row     active-animation-row)
                   (active-animation-column  active-animation-column)
                   (texture-window-width     texture-window-width)
                   (texture-window-height    texture-window-height)
                   (animation-rows-count     animation-rows-count)
                   (renderp                  renderp)) object
    ;; (declare (vec4 font-color))
    ;; (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    ;; (declare (list triangles))
    (when renderp
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (use-program compiled-shaders :sprite-character)
          (gl:active-texture            :texture0)
          (texture:bind-texture texture-object)
          (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
          (uniformf compiled-shaders :texel-t-offset (d* (d- (d (1- animation-rows-count))
                                                              (d active-animation-row))
                                                         texture-window-height))
          (uniformf compiled-shaders :texel-s-offset
                    (d* (d active-animation-column)
                        texture-window-width))
          (uniform-matrix compiled-shaders
                          :post-scaling 4
                          (vector (scale scaling))
                          nil)
          (uniform-matrix compiled-shaders
                          :modelview-matrix 4
                          (vector (matrix* camera-vw-matrix
                                           (elt view-matrix  0)
                                           (elt model-matrix 0)))
                          nil)
          (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
          (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
          (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))

(defun animation-starting-row (anim-spec-row facing)
  (let ((all (cadr anim-spec-row)))
    (car (elt all facing))))

(defun animation-row-count (anim-spec-row facing)
  (let ((all (cadr anim-spec-row)))
    (cadr (elt all facing))))

(defun animation-specs (animation-table animation-type)
  (assoc animation-type animation-table :test #'eql))

(defmethod set-animation ((object sprite-mesh) animation &key (recalculate t))
  (with-accessors ((fps fps)
                   (animation-table         animation-table)
                   (current-animation-time  current-animation-time)
                   (active-animation-column active-animation-column)
                   (active-animation-row    active-animation-row)
                   (active-animation-count  active-animation-count)
                   (ghost                   ghost)) object
    (let ((anim-spec (animation-specs animation-table animation)))
      (when anim-spec
        (setf active-animation-column 0
              active-animation-row    (animation-starting-row anim-spec +sprite-anim-north-dir+)
              active-animation-count  (animation-row-count    anim-spec +sprite-anim-north-dir+)
              current-animation-time  0.0)
        (when recalculate
          (with-no-thinking (ghost)
            (calculate object 0.0)))))))

;; used only in get-sprite-shell
(defun load-sprite-model (sprite-dir shaders &key
                                               (spritesheet-file +model-spritesheet+)
                                               (resource-path    +models-resource+))
  (let* ((spritesheet-path (res:get-resource-file (text-utils:strcat sprite-dir
                                                                     spritesheet-file)
                                                  resource-path
                                                  :if-does-not-exists :error))
         (model            (load-sprite spritesheet-path))
         (fs-res           (make-sprite-fs-res :dir              sprite-dir
                                               :spritesheet-file spritesheet-file
                                               :resource-path    resource-path)))
    (setf (fs-resources    model) fs-res)
    (setf (compiled-shaders model) shaders)
    (prepare-for-rendering model)
    ;;(map nil #'mesh:remove-mesh-data (frames model))
    (set-animation model :stand :recalculate t)
    model))

(defun load-sprite-player (ghost dir compiled-shaders resource-path)
  (let ((shell (get-sprite-shell :shaders        compiled-shaders
                                 :dir            dir
                                 :spritesheet-file   +model-spritesheet+
                                 :resource-path      resource-path)))
    (setf (interfaces:compiled-shaders shell) compiled-shaders)
    (set-animation shell :stand :recalculate t)
    (setf (ghost shell) ghost)
    (setf (thinker (ghost shell)) t)
    shell))

(defclass sprite-mesh-shell (sprite-mesh) ())

(defmethod destroy ((object sprite-mesh-shell))
  #+debug-mode (misc:dbg "destroy sprite mesh shell ~a" (id object))
  (with-accessors ((vao vao)
                   (vbo vbo)
                   (texture texture-object)
                   (normal-map normal-map)
                   (triangles triangles)
                   (children children)
                   (frames frames)
                   (animation-table animation-table)
                   (material-params material-params)
                   (aabb aabb)
                   (tags-table     tags-table)
                   (tags-matrices  tags-matrices)
                   (tag-key-parent tag-key-parent)) object
    (with-slots (compiled-shaders) object
      (setf compiled-shaders nil)
      (setf vao              nil)
      (setf vbo              nil)
      (setf texture          nil)
      (setf normal-map       nil)
      (setf triangles        nil)
      (setf aabb             nil)
      (do-children-mesh (child object)
        (destroy child))
      object)))

;; (defmethod prepare-for-rendering ((object sprite-mesh-shell))
;;   (common-sprite-prepare-for-rendering object))

(defmethod fill-mesh-data ((object sprite-mesh-shell) (source sprite-mesh))
  (with-accessors ((vao-host vao) (vbo-host vbo)
                   (texture-host texture-object)
                   (normal-map-host normal-map)
                   (triangles-host triangles)
                   (children-host children)
                   (material-params-host material-params)
                   (compiled-shaders-host compiled-shaders)
                   (frames frames)
                   (animation-table animation-table)
                   (material-params material-params)
                   (aabb-host aabb)
                   (tags-table     tags-table)
                   (tags-matrices  tags-matrices)
                   (tag-key-parent tag-key-parent)) object
    (setf texture-host          (texture-object   source)
          triangles-host        (triangles        source)
          compiled-shaders-host (compiled-shaders source))
    object))

(defmethod fill-mesh-data-w-renderer-data ((object sprite-mesh-shell) (source sprite-mesh))
  ;; sprite-mesh has not rendering data on its own, in a sense.
  ;; And we should not share them too.
  (fill-mesh-data object source))

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

(defmethod step-in-trap-p ((object sprite-mesh-shell))
  (with-accessors ((state state)) object
    (let* ((old-state (%old-state object)))
      (and old-state
           (eq (game-state:el-type old-state)
               game-state:+trap-type+)
           old-state))))

(defmethod trap-ostile-p ((object sprite-mesh-shell))
  "Note: if faction of the trap is nil the trap *is* ostile to object"
  (with-accessors ((state state)) object
    (if (step-in-trap-p object)
        (let* ((old-state (%old-state object))
               (trap      (and old-state
                               (game-state:find-entity-by-id state
                                                             (game-state:entity-id old-state)))))
          (and trap
               (not (eq (my-faction object)
                        (my-faction trap)))
               trap))
        nil)))

(defmethod place-trap ((object sprite-mesh-shell) trap-ghost)
  (with-accessors ((state            state)
                   (pos              pos)
                   (compiled-shaders compiled-shaders)) object
    (when (trap-can-be-placed-p object)
      (let ((faction (my-faction object)))
        (build-and-place-trap-on-map state
                                     trap-ghost
                                     faction
                                     compiled-shaders
                                     (vec-x pos)
                                     (vec-z pos))
        (decrement-move-points-place-trap object)
        (send-refresh-toolbar-event)))))

(defmethod next-move-position ((object sprite-mesh) (strategy (eql +explore-strategy+)))
  (with-accessors ((state state)) object
    (with-accessors ((blackboard blackboard:blackboard)) state
        (ai-utils:next-explore-position blackboard object))))

(defparameter *sprite-mesh-factory-db* '())

(defun clean-sprite-db ()
  (map 'nil #'destroy *sprite-mesh-factory-db*)
  (setf *sprite-mesh-factory-db* nil))

(defun %db-equal-fn (dir spritesheet-file resource-path)
  #'(lambda (a)
      (and
       (string=    dir              (dir              (fs-resources a)))
       (string=    spritesheet-file (spritesheet-file (fs-resources a)))
       (tree-equal resource-path (resource-path (fs-resources a)) :test #'string=))))

(defun get-sprite-shell (&key
                           (shaders        nil)
                           (dir            nil)
                           (spritesheet-file   +model-spritesheet+)
                           (resource-path  nil))
  (let* ((res (find-if (%db-equal-fn dir spritesheet-file resource-path)
                       *sprite-mesh-factory-db*)))
    (if res
        (let ((shell (fill-shell-from-mesh-w-renderer-data res 'sprite-mesh-shell)))
          (setf (fs-resources shell)
                (make-sprite-fs-res :dir              dir
                                    :spritesheet-file spritesheet-file
                                    :resource-path    resource-path))
          (prepare-for-rendering shell)
          shell)
        (let ((model (load-sprite-model dir shaders
                                        :spritesheet-file spritesheet-file
                                        :resource-path    resource-path)))
          (push model *sprite-mesh-factory-db*)
          (get-sprite-shell :dir              dir
                            :spritesheet-file spritesheet-file
                            :resource-path    resource-path)))))
