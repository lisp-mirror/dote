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

(in-package :md2-mesh)

(alexandria:define-constant +magic-number+ "IDP2"                :test #'string=)

(alexandria:define-constant +version+ 8                          :test #'=)

(alexandria:define-constant +element-type+ '(unsigned-byte 8)    :test 'equalp)

(alexandria:define-constant +size-elem-texture-coord+ 2          :test #'=)

(alexandria:define-constant +size-textures-struct+ 4             :test #'=)

(alexandria:define-constant +size-elem-triangles+ 2              :test #'=)

(alexandria:define-constant +size-triangles-struct+ 12           :test #'=)

(alexandria:define-constant +size-triangles-chunk+ 6             :test #'=)

(alexandria:define-constant +size-frame-name+ 16                 :test #'=)

(alexandria:define-constant +size-vertex-struct+ 4               :test #'=)

(alexandria:define-constant +default-animation-table+
    '((stand 0 39 20) ; name starting-frame ending-frame fps
      (move 40 45 20)
      (attack 46 53 20)
      (bored 123 134 20)
      (pain 54 65 20)
      (death 178 197 20)
      (critical 160 168 20)
      (critical2 84 94 20)
      (critical3 72 83))
  :test #'equalp)

(defmacro define-header-offsets ((&rest names))
  `(misc:define-offset-size md2-mesh md2
     ,@(loop
	  for name in names
	  for start from 0 by 4 collect
	    `(,name ,start 4))))

(defclass md2-mesh (able-to-see-mesh)
  ((frames
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

(defmethod print-object ((object md2-mesh) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defmethod on-game-event ((object md2-mesh) (event attack-melee-event))
  (check-event-targeted-to-me (object event)
    (multiple-value-bind (damage ambush)
	(battle-utils:defend-from-attack-short-range event)
      (when ambush
	(billboard:apply-tooltip object
				 billboard:+tooltip-surprise-attack-char+
				 :color     billboard:+damage-color+
				 :font-type gui:+tooltip-font-handle+))
      (apply-damage object damage)
      (setf (attacked-by-entity object) (attacker-entity event))
      (game-event:register-for-end-attack-melee-event object)
      t)))

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
	 (when (and (not (entity:reply-attack-p ,attacked-by-entity))
		    (or t
			(dice:pass-d1.0 ,chance)))
	   ;; attack!
	   (game-state:with-world (,world ,state)
	     (let ((,weapon-short-range (character:weapon-type-short-range ,ghost))
		   (,weapon-long-range  (character:weapon-type-long-range  ,ghost)))
	       (cond
		 (,weapon-short-range
		  (setf (entity:reply-attack ,object) t)
		  (battle-utils:attack-short-range ,world ,object ,attacked-by-entity))
		 (,weapon-long-range
		  (setf (entity:reply-attack ,object) t)
		  (battle-utils:attack-long-range ,world ,object ,attacked-by-entity)))
	       ,@body)))))))

(defmethod on-game-event ((object md2-mesh) (event end-attack-melee-event))
  (with-end-attack-event (object event attacked-by-entity)
    (with-maybe-reply-attack (object attacked-by-entity))))

(defmethod on-game-event ((object md2-mesh) (event end-attack-long-range-event))
  (with-end-attack-event (object event attacked-by-entity)
    (with-maybe-reply-attack (object attacked-by-entity))))

(defmethod on-game-event ((object md2-mesh) (event attack-long-range-event))
  (check-event-targeted-to-me (object event)
    (multiple-value-bind (damage ambush)
	(battle-utils:defend-from-attack-long-range event)
      (when ambush
	(billboard:apply-tooltip object
				 billboard:+tooltip-surprise-attack-char+
				 :color     billboard:+damage-color+
				 :font-type gui:+tooltip-font-handle+))

      (apply-damage object damage)
      (setf (attacked-by-entity object) (attacker-entity event))
      (game-event:register-for-end-attack-long-range-event object)
      t)))

(defmethod on-game-event ((object md2-mesh) (event update-visibility))
  (with-accessors ((state state)
		   (id id)) object
    (flet ((check (key entity)
	     (declare (ignore key))
	     (when (find-if #'(lambda (a) (= id (id a))) (visible-players entity))
	       (setf (renderp object) t)
	       (return-from on-game-event nil))))
      (when (game-state:faction-ai-p state id)
	(setf (renderp object) nil))
      (game-state:map-player-entities state #'(lambda (k v)
						(declare (ignore k))
						(update-visibility-cone v)))
      (game-state:map-player-entities state #'check)
      nil)))

(defun path->dir (path &key (start-index 0))
  (let* ((start      (ivec2 (elt (elt path start-index) 0)
			    (elt (elt path start-index) 1)))
	 (end        (ivec2 (elt (elt path (1+ start-index)) 0)
			    (elt (elt path (1+ start-index)) 1)))
	 (new-dir-2d (ivec2- end start)))
    (vec (d (elt new-dir-2d 0)) 0.0  (d (elt new-dir-2d 1)))))

(defmethod on-game-event ((object md2-mesh) (event move-entity-along-path-event))
  (with-accessors ((ghost ghost)
		   (dir dir)) object
    (with-accessors ((current-path current-path)) ghost
      (if (= (id object) (id-destination event))
	  (let ((disable-input-event (make-instance 'window-accept-input-event
						    :accept-input-p nil)))
	    (game-event:propagate-window-accept-input-event disable-input-event)
	    (setf (game-state:selected-path (state object)) nil)
	    (set-animation object :move)
	    (setf (current-action object) :move)
	    (setf current-path (path event))
	    (setf dir (path->dir current-path :start-index 0))
	    t)
	  nil))))

(defmethod on-game-event ((object md2-mesh) (event move-entity-entered-in-tile-event))
  (with-accessors ((ghost ghost)
		   (dir dir)
		   (id id)
		   (state state)) object
    (with-accessors ((current-path current-path)) ghost
      (when (= (id-origin event) id)
	(let ((leaving-tile (alexandria:first-elt current-path)))
	  (setf current-path (subseq current-path 1))
	  (if (= (length current-path) 1)
	      (let ((end-event (make-instance 'move-entity-along-path-end-event
					      :id-origin id
					      :tile-pos  (alexandria:last-elt current-path))))
		(decrement-move-points-entering-tile object)
		(propagate-move-entity-along-path-end-event end-event))
	      (progn
		(decrement-move-points-entering-tile object)
		(setf dir (path->dir current-path :start-index 0))))
	  (game-state:with-world (world state)
	    ;; update state matrix and quadtree
	    (world:move-entity world object leaving-tile))
	  (propagate-update-highlight-path (make-instance 'update-highlight-path
							  :tile-pos current-path))
	  (update-visibility-cone object)
	  (send-update-visibility-event object)
	  (send-refresh-toolbar-event)))
      nil)))

(defmethod on-game-event ((object md2-mesh) (event move-entity-along-path-end-event))
  (with-accessors ((id id)
		   (ghost ghost)) object
    (when (= (id-origin event) id)
      (let ((enable-input-event (make-instance 'window-accept-input-event
					       :accept-input-p t)))
	(game-event:propagate-window-accept-input-event enable-input-event)
	(setf (current-action object) :stand)
	(set-animation object :stand))))
  nil)

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
	  ;;(misc:dbg "visibility test: ~a" (visible-players object))
	  (send-update-visibility-event object)
	  (send-refresh-toolbar-event)
	  t)
	nil)))

(defmethod on-game-event ((object md2-mesh) (event rotate-entity-ccw-event))
  (with-accessors ((ghost ghost)
		   (dir dir)) object
    (if (= (id object) (id-destination event))
	(when (> (current-movement-points ghost) 0)
	  (decrement-move-points-rotate object)
	  (setf (current-action object) :rotate)
	  (setf dir (sb-cga:transform-direction dir (rotate-around +y-axe+ +pi/2+)))
	  (update-visibility-cone object)
	  ;;(misc:dbg "visibility test: ~a" (visible-players object))
	  (send-update-visibility-event object)
	  (send-refresh-toolbar-event)
	  t)
	nil)))

(defmacro with-cause-events-accessors ((mesh event immune-status-accessor) &body body)
  `(with-accessors ((ghost ghost)
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

(defmethod on-game-event ((object md2-mesh) (event cause-poisoning-event))
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
	    (progn
	      (push event-data recurrent-effects)
	      (billboard:apply-tooltip object
				       billboard:+tooltip-poison-char+
				       :color     billboard:+poison-damage-color+
				       :font-type gui:+tooltip-font-handle+)
	      (setf status +status-poisoned+)
	      (send-refresh-toolbar-event)
	      t)
	    nil)))))

(defmacro with-simple-cause-status ((mesh event immune-accessor new-status tooltip)
				     &body body)
  `(with-cause-events-accessors (,mesh ,event ,immune-accessor)
     (billboard:apply-tooltip ,mesh
			      ,tooltip
			      :color     billboard:+poison-damage-color+
			      :font-type gui:+tooltip-font-handle+)
     (setf status ,new-status)
     ,@body
     (send-refresh-toolbar-event)))

(defmethod on-game-event ((object md2-mesh) (event cause-terror-event))
  (with-simple-cause-status (object event immune-terror-status +status-terror+
				    billboard:+tooltip-terror-char+)
    (with-accessors ((event-data event-data)) event
      (when (numberp (random-object-messages:msg-duration event-data))
	(pq:push-element (character:postponed-messages (ghost object))
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
	(pq:push-element (character:postponed-messages (ghost object))
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
	      (billboard:apply-tooltip mesh
				       billboard:+tooltip-heal-char+
				       :color     billboard:+healing-color+
				       :font-type gui:+tooltip-font-handle+)
	      (setf status nil)
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
	   (progn
	     (game-state:with-world (world state)
	       (world:post-entity-message world object
					  (random-elt ,text-message-bag)
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
  `(with-immune-events-accessors (,mesh ,event ,immune-accessor ,status-affected)
     (billboard:apply-tooltip ,mesh
			      ,tooltip
			      :color     billboard:+poison-damage-color+
			      :font-type gui:+tooltip-font-handle+)
     (setf ,immune-accessor t)
     ,@body
     (send-refresh-toolbar-event)))

(defmethod on-game-event ((object md2-mesh) (event immune-berserk-event))
  (with-simple-immune-status (object event immune-berserk-status +status-berserk+
				    billboard:+tooltip-immune-berserk-char+)
    (with-accessors ((event-data event-data)) event
      (when (numberp (random-object-messages:msg-duration event-data))
	(pq:push-element (character:postponed-messages (ghost object))
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
	(pq:push-element (character:postponed-messages (ghost object))
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
	(pq:push-element (character:postponed-messages (ghost object))
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
	(pq:push-element (character:postponed-messages (ghost object))
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
		 (dice:pass-d1.0 (random-object-messages:msg-chance event-data))
		 (not (or (eq status +status-faint+)
			  (eq status +status-berserk+))))
	    (progn
	      (setf current-damage-points
		    (min damage-points
			 (d+ current-damage-points
			     (random-object-messages:msg-points event-data))))
	      (billboard:apply-tooltip object
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
	    (let ((triggered            (remove-if
					 (random-object-messages:untrigged-effect-p-fn
					  basic-interaction-parameters:+effect-when-worn+)
					 messages))
		  (all-effects-to-other (remove-if-not
					 (random-object-messages:to-other-target-effect-p-fn)
					 messages)))
	      (decrement-move-points-wear object)
	      (random-object-messages:propagate-effects-msg (id-origin event) id triggered)
	      (dolist (effect all-effects-to-other)
		(push effect modifiers-effects))
	      (send-refresh-toolbar-event)
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

(defmethod on-game-event :after ((object md2-mesh) (event end-turn))
  (misc:dbg "end turn md2mesh tooltip ct ~a" (tooltip-count object))
  (with-accessors ((ghost ghost)
		   (state state)) object
    (game-state:with-world (world state)
      (when ghost
	(character:reset-magic-points    ghost)
	(character:reset-movement-points ghost)
	(traverse-recurrent-effects      object)
	(let ((decayed-items (remove-decayed-items ghost (end-turn-count event))))
	  (dolist (item decayed-items)
	    (remove-from-modifiers ghost (id item))
	    (world:post-entity-message world object
				       (format nil
					       (_"~a broken")
					       (description-type item))
				       (cons (_ "Move to")
					     (world:point-to-entity-and-hide-cb world object)))))
	(send-refresh-toolbar-event)
	nil))))

(defun blood-splill-level (damage max-damage)
  (cond
    ((< (/ damage max-damage) 0.2)
     #'particles:make-blood-level-0)
    ((<= 0.2 (/ damage max-damage) 0.7)
     #'particles:make-blood-level-1)
    ((> (/ damage max-damage) 0.7)
     #'particles:make-blood-level-2)))

(defmethod apply-damage ((object md2-mesh) damage)
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
	      (billboard:apply-tooltip object
	       			   (format nil "miss")
	       			   :color billboard:+damage-color+
	       			   :font-type gui:+tooltip-font-handle+)

	      (progn
		(setf current-damage-points (d- current-damage-points damage))
		(if (entity-dead-p object)
		    (set-death-status object)
		    (let ((blood (funcall (blood-splill-level damage (character:actual-damage-points ghost))
					  (aabb-center aabb)
					  dir
					  compiled-shaders)))
		      (game-state:with-world (world state)
			(world:push-entity world blood))
		      (setf current-action  :pain)
		      (set-animation object :pain)
		      (setf stop-animation nil)
		      (setf cycle-animation nil)))
		(billboard:apply-tooltip object
					 (format nil
						 +standard-float-print-format+
						 (d- damage))
					 :color billboard:+damage-color+
					 :font-type gui:+tooltip-font-handle+)))))))

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
		      (set-animation object :pain)
		      (setf stop-animation nil)
		      (setf cycle-animation nil)))
		(billboard:apply-tooltip object
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
  (with-accessors ((ghost ghost)
		   (aabb aabb)
		   (state state)
		   (compiled-shaders compiled-shaders)
		   (current-action current-action)
		   (cycle-animation cycle-animation)
		   (stop-animation stop-animation)) object
    (with-accessors ((status status)
		     (current-damage-points current-damage-points)) ghost
      (setf (status ghost) +status-faint+)
      (setf current-action  :death)
      (set-animation object :death)
      (setf stop-animation nil)
      (setf cycle-animation nil)
      (setf current-damage-points (d 0.0))
      (game-state:with-world (world state)
	(world:push-entity world
			   (particles:make-blood-death (aabb-center aabb)
						       +y-axe+
						       compiled-shaders))))))

(defmethod set-attack-status ((object md2-mesh))
  (with-accessors ((ghost ghost)
		   (current-action current-action)
		   (cycle-animation cycle-animation)
		   (stop-animation stop-animation)) object
    (with-accessors ((status status)
		     (current-damage-points current-damage-points)) ghost
      (setf current-action  :attack)
      (set-animation object :attack)
      (setf stop-animation nil)
      (setf cycle-animation nil))))

(defgeneric push-errors (object the-error))

(defgeneric load (object file))

(defgeneric load-texture (object file))

(defgeneric load-animations (object file))

(defgeneric bind-vbo (object &optional refresh))

(defgeneric set-animation (object animation &key recalculate))

(defgeneric %alloc-arrays (object))

(defgeneric calculate-move (object dt))

(defgeneric wear-item      (object item))

(defmethod destroy :after ((object md2-mesh))
  (when +debug-mode+
    (misc:dbg "destroy md2 mesh ~a ~a" (id object) (map 'list #'id (frames object))))
  (map nil #'destroy (frames object)))

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
	    (id              (slot-value object 'id)))
	(tg:finalize object
		     #'(lambda ()
			 (when +debug-mode+
			   (misc:dbg "finalize destroy md2 mesh ~a" id))
			 (free-memory* (list gl-arr-vert
					     gl-arr-tex
					     gl-arr-norm
					     gl-arr-norm-obj
					     gl-arr-tan
					     gl-arr-aabb)
				       vbos vaos)))))))

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
		      (loop for tags in (tags-table object) do
			   (loop for tag across (cdr tags) do
				(setf (elt tag 3)
				      (transform-point (elt tag 3) transf))))
		      (loop for f across (frames object) do
			   (transform-vertices f transf)))
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
	  (texture:prepare-for-rendering (texture-object object))
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

(defmethod calculate-move ((object md2-mesh) dt)
  (declare (ignore dt))
  (with-accessors ((ghost ghost)
		   (pos pos)
		   (dir dir)) object
    (let* ((x-chunk (coord-map->chunk (elt (elt (character:current-path ghost) 1) 0)))
	   (z-chunk (coord-map->chunk (elt (elt (character:current-path ghost) 1) 1)))
	   (y       (d+ 1.5 ; hardcoded :(  to be removed soon
			(game-state:approx-terrain-height@pos (state object) x-chunk z-chunk)))
	   (end (vec x-chunk y z-chunk))
	   (dir (let ((new-dir (vec- end (pos object))))
		  (if (vec~ new-dir +zero-vec+)
		      (vec 0.0 1.0 0.0)
		      (normalize new-dir)))))
      (if (not (vec~ (pos object) end (d/ +terrain-chunk-tile-size+ 8.0)))
	  (setf (pos object) (vec+ (pos object) (vec* dir +model-move-speed+)))
	  (let ((movement-event (make-instance 'move-entity-entered-in-tile-event
					       :id-origin (id object)
					       :tile-pos  end)))
	    (propagate-move-entity-entered-in-tile-event movement-event))))))

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
    (ecase current-action
      (:stand
       ;; nothing to do
       )
      (:death
       ;; nothing to do
       )
      (:attack
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
	 (send-end-attack-long-range-event object))))
    (unless stop-animation
      (let ((next-time (+ current-animation-time dt))
	    (frames-numbers (the fixnum (1+ (- end-frame starting-frame))))
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
	  			  interpolation-factor)
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
      (let ((tag-matrix (find-tag-cdr tag-key-parent (tags-matrices (mtree-utils:parent object)))))
	(when tag-matrix
	  (with-model-matrix (model-matrix object)
	    (setf (model-matrix object)
		  (matrix* model-matrix
			   tag-matrix))))))
    (bind-vbo object t)
    (do-children-mesh (i object)
      (calculate i dt))))

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
		   (animation-table animation-table)) object
    (let ((anim-spec (assoc animation animation-table :test #'eql)))
      (when anim-spec
	(setf starting-frame (second anim-spec)
	      end-frame (third anim-spec)
	      fps (fourth anim-spec)
	      current-animation-time 0.0
	      current-frame-offset 0)
	(when recalculate
	  (calculate object 0.0))))))

(defun load-md2-model (modeldir &key
				  (material (make-mesh-material .1 1.0 0.1 0.0 128.0))
				  (mesh-file +model-filename+)
				  (texture-file +model-texture-filename+)
				  (animation-file +model-animations-filename+)
				  (tags-file      nil)
				  (resource-path  +models-resource+))
  (let ((model (make-instance 'md2-mesh :render-normals nil :render-aabb nil)))
    (setf (material-params model) material)
    (load-texture model (res:get-resource-file (text-utils:strcat modeldir texture-file)
					       resource-path
					       :if-does-not-exists :error))
    (load-animations model (res:get-resource-file (text-utils:strcat modeldir animation-file)
						  resource-path
						  :if-does-not-exists :error))
    (when tags-file
      (load-tags model (res:get-resource-file (text-utils:strcat modeldir tags-file)
					      resource-path
					      :if-does-not-exists :error)))
    (load model (res:get-resource-file (text-utils:strcat modeldir mesh-file)
				       resource-path
				       :if-does-not-exists :error))
    (when +debug-mode+
      (misc:dbg "error md2 parsing ~a" (parsing-errors model)))
    (prepare-for-rendering model)
    (map nil #'mesh::remove-mesh-data (frames model))
    (set-animation model :stand)
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
  (let ((ring (random-ring:generate-ring 10))
	(effect-cause-berserk (make-instance 'basic-interaction-parameters:healing-effect-parameters
					     :trigger
					     basic-interaction-parameters:+effect-when-worn+
					     :duration 2
					     :chance   0.9)))
    (n-setf-path-value (basic-interaction-params ring)
		       '(:healing-effects :cause-berserk)
		       effect-cause-berserk)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun load-md2-player (ghost dir compiled-shaders resource-path)
  (let ((body (md2:load-md2-model dir
				  :mesh-file      "body01.md2"
				  :animation-file "body-animation.lisp"
				  :texture-file   "body-texture.tga"
				  :tags-file      "body01.tag"
				  :resource-path  resource-path))
	(head (md2:load-md2-model dir
				  :mesh-file      "head01.md2"
				  :animation-file "head-animation.lisp"
				  :texture-file   "head-texture.tga"
				  :tags-file      nil
				  :resource-path  resource-path)))
    (setf (interfaces:compiled-shaders body) compiled-shaders
	  (interfaces:compiled-shaders head) compiled-shaders)
      (md2:set-animation body :stand)
      (md2:set-animation head :stand)
      (setf (md2:tag-key-parent head) md2:+tag-head-key+)
      (mtree-utils:add-child body head)
      (setf (ghost body) ghost)
      ;;;;;;;;;;;;;;;;;;;;;;;; testing
      (let ((forged-potion              (forged-potion))
	    (forged-potion-cure-dmg     (forged-potion-cure-dmg))
	    (forged-potion-cure-berserk (forged-potion-cure-berserk))
	    (forged-ring                (forged-ring))
	    (forged-sword               (forged-sword))
	    (forged-bow                 (forged-bow)))
	(game-event:register-for-end-turn forged-potion)
	(game-event:register-for-end-turn forged-potion-cure-dmg)
	(game-event:register-for-end-turn forged-potion-cure-berserk)
	(game-event:register-for-end-turn forged-ring)
	(game-event:register-for-end-turn forged-bow)
	(add-to-inventory (ghost body) forged-potion)
	(add-to-inventory (ghost body) forged-potion-cure-dmg)
	(add-to-inventory (ghost body) forged-potion-cure-berserk)
	(add-to-inventory (ghost body) forged-ring)
	(add-to-inventory (ghost body) forged-bow)
	(add-to-inventory (ghost body) forged-sword)
	(setf (movement-points (ghost body)) 100.0)
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
