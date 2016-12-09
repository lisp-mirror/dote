(in-package :game-event)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *all-events-container* (make-hash-table :test 'equalp))

  (defparameter *events-names-counter* 0)

  (defun fresh-events-container-name ()
    (prog1
	*events-names-counter*
      (incf *events-names-counter*))))

(defun clean-all-events-vectors ()
  (maphash #'(lambda (k v)
	       (loop for i from 0 below (length v) do (setf (elt v i) nil))
	       (setf (gethash k *all-events-container*)
		     (make-fresh-array 0 nil 'entity:entity)))
	   *all-events-container*))

(defclass generic-game-event ()
  ((id-origin
    :initform nil
    :initarg  :id-origin
    :accessor id-origin)
   (age
    :initform 0
    :initarg  :age
    :accessor age)
   (name
    :initform nil
    :initarg  :name
    :accessor name)
   (priority
    :initform 0
    :initarg  :priority
    :accessor priority)
   (event-data
    :initform 0
    :initarg  :event-data
    :accessor event-data)))

(defclass game-event-w-destination (generic-game-event)
  ((id-destination
    :initform nil
    :initarg  :id-destination
    :accessor id-destination)))

(defclass game-event-procrastinated (generic-game-event)
  ((trigger-turn
    :initform nil
    :initarg  :trigger-turn
    :accessor trigger-turn)))

(defgeneric on-game-event (object event))

(defmethod on-game-event (object event)
  nil)

(defmacro defevent (name parents slots)
  (let* ((event-hash-key    (fresh-events-container-name))
	 (event-name        (format        nil "~:@(~a~)" name))
	 (event-symbol      (format-symbol t   "~a"       event-name))
	 (propagate-symbol  (format-symbol t   "~:@(propagate-~a~)" name))
	 (get-vector-symbol (gensym))
	 (register-symbol   (format-symbol t "~:@(register-for-~a~)" name))
	 (unregister-symbol (format-symbol t "~:@(unregister-for-~a~)" name)))
    `(progn
       (defmacro ,get-vector-symbol ()
	 `(gethash ,,(identity event-hash-key) *all-events-container*))
       (setf (,get-vector-symbol)
	     (make-fresh-array 0 nil 'entity:entity))
       (defclass ,event-symbol ,(or parents `(generic-game-event))
	 ,slots)
       (defun ,register-symbol (el)
	 (vector-push-extend el (,get-vector-symbol)))
       (defun ,unregister-symbol (el)
	 (setf (,get-vector-symbol)
	       (delete (identificable:id el) (,get-vector-symbol) :test #'= :key #'identificable:id)))
       (defun ,propagate-symbol (event)
	 (loop for ent across (,get-vector-symbol) do
	      (when (on-game-event ent event)
		(return-from ,propagate-symbol t)))
	 nil))))

(defevent end-turn ()
  ((end-turn-count
    :initform 0
    :initarg  :end-turn-count
    :accessor end-turn-count)))

(defevent update-visibility () ())

(defun send-update-visibility-event (origin-entity)
  (let ((event (make-instance 'update-visibility :id-origin (identificable:id origin-entity))))
    (propagate-update-visibility event)))

(defevent camera-drag-ends () ())

(defevent healing-effect-turn (game-event-w-destination)
    ((parameters
      :initform nil
      :initarg  :parameters
      :accessor parameters)
     (effect
      :initform nil
      :initarg  :effect
      :accessor effect)))

(defclass cancel-healing-effect-game-event (healing-effect-game-event game-event-procrastinated)
  ())

(defevent move-entity-along-path-event (game-event-w-destination)
  ((path
    :initform nil
    :initarg  :path
    :accessor path)
   (cost
    :initform nil
    :initarg  :cost
    :accessor cost)))

(defevent move-entity-along-path-end-event ()
  ((tile-pos
    :initform nil
    :initarg  :tile-pos
    :accessor tile-pos)))

(defevent move-entity-entered-in-tile-event ()
  ((tile-pos
    :initform nil
    :initarg  :tile-pos
    :accessor tile-pos)))

(defevent rotate-entity-cw-event (game-event-w-destination) ())

(defevent rotate-entity-ccw-event (game-event-w-destination) ())

(defevent window-accept-input-event ()
  ((accept-input-p
    :initform nil
    :initarg  :accept-input-p
    :accessor accept-input-p)))

(defevent refresh-toolbar-event ()
  ((accept-input-p
    :initform nil
    :initarg  :accept-input-p
    :accessor accept-input-p)))

(defun send-refresh-toolbar-event ()
  (let ((refresh-toolbar-event (make-instance 'game-event:refresh-toolbar-event)))
    (propagate-refresh-toolbar-event refresh-toolbar-event)))

(defevent update-highlight-path ()
  ((tile-pos
    :initform nil
    :initarg  :tile-pos
    :accessor tile-pos)))

(defevent open-door-event (game-event-w-destination) ())

(defevent close-door-event (game-event-w-destination) ())

(defun make-simple-event-w-dest (class id-from id-to)
  (make-instance class :id-origin id-from :id-destination id-to))

(defmacro gen-cause-*-events (&rest event-type)
  `(progn
     ,@(loop for ev in event-type collect
	    `(defevent
		,(misc:format-fn-symbol t "cause-~a-event" ev)
		(game-event-w-destination) ()))))

(gen-cause-*-events poisoning terror faint berserk)

(defmacro gen-make-cause-*-events (&rest event-type)
  `(progn
     ,@(loop for ev in event-type collect
	    `(defun
		 ,(misc:format-fn-symbol t "make-cause-~a-event" ev)
		 (id-from id-to
		  ,(misc:format-fn-symbol t "cause-~a-effects" ev))
	       (make-instance ',(misc:format-fn-symbol t "cause-~a-event" ev)
			      :id-origin      id-from
			      :id-destination id-to
			      :event-data     ,(misc:format-fn-symbol t "cause-~a-effects" ev))))))

(gen-make-cause-*-events poisoning terror faint berserk)

(defmacro gen-cure-*-events (&rest event-type)
  `(progn
     ,@(loop for ev in event-type collect
	    `(defevent
		,(misc:format-fn-symbol t "cure-~a-event" ev)
		(game-event-w-destination) ()))))

(gen-cure-*-events poisoning terror faint berserk)

(defmacro gen-make-cure-*-events (&rest event-type)
  `(progn
     ,@(loop for ev in event-type collect
	    `(defun
		 ,(misc:format-fn-symbol t "make-cure-~a-event" ev)
		 (id-from id-to
		  ,(misc:format-fn-symbol t "cure-~a-effects" ev))
	       (make-instance ',(misc:format-fn-symbol t "cure-~a-event" ev)
			      :id-origin      id-from
			      :id-destination id-to
			      :event-data     ,(misc:format-fn-symbol t "cure-~a-effects" ev))))))

(gen-make-cure-*-events poisoning terror faint berserk)

(defmacro gen-cancel-*-events (&rest event-type)
  `(progn
     ,@(loop for ev in event-type collect
	    `(defevent
		,(misc:format-fn-symbol t "cancel-~a-event" ev)
		(game-event-w-destination game-event-procrastinated) ()))))

(gen-cancel-*-events poisoning        terror        faint        berserk
		     immune-poisoning immune-terror immune-faint immune-berserk)

(defmacro gen-make-cancel-*-events (&rest event-type)
  `(progn
     ,@(loop for ev in event-type collect
	    `(defun
		 ,(misc:format-fn-symbol t "make-cancel-~a-event" ev)
		 (id-from id-to trigger-turn &key (original-event nil))
	       (make-instance ',(misc:format-fn-symbol t "cancel-~a-event" ev)
			      :id-origin      id-from
			      :id-destination id-to
			      :trigger-turn   trigger-turn
			      :event-data     original-event)))))

(gen-make-cancel-*-events poisoning        terror        faint        berserk
		          immune-poisoning immune-terror immune-faint immune-berserk)

(defmacro gen-immune-*-events (&rest event-type)
  `(progn
     ,@(loop for ev in event-type collect
	    `(defevent
		,(misc:format-fn-symbol t "immune-~a-event" ev)
		(game-event-w-destination) ()))))

(gen-immune-*-events poisoning terror faint berserk)

(defmacro gen-make-immune-*-events (&rest event-type)
  `(progn
     ,@(loop for ev in event-type collect
	    `(defun
		 ,(misc:format-fn-symbol t "make-immune-~a-event" ev)
		 (id-from id-to
		  ,(misc:format-fn-symbol t "immune-~a-effects" ev))
	       (make-instance ',(misc:format-fn-symbol t "immune-~a-event" ev)
			      :id-origin      id-from
			      :id-destination id-to
			      :event-data     ,(misc:format-fn-symbol t "immune-~a-effects" ev))))))

(gen-make-immune-*-events poisoning terror faint berserk)

(defevent heal-damage-event (game-event-w-destination) ())

(defun make-heal-damage-event (id-from id-to heal-damage-effects)
   (make-instance 'heal-damage-event :id-origin id-from :id-destination
                  id-to :event-data heal-damage-effects))

(defevent modifier-object-event (game-event-w-destination) ())

(defun make-modifier-object-event (id-from id-to modifier-effects)
   (make-instance 'modifier-object-event :id-origin id-from :id-destination
                  id-to :event-data modifier-effects))

(defevent wear-object-event (game-event-w-destination) ())

(defevent unwear-object-event (game-event-w-destination) ())

(defevent attack-melee-event (game-event-w-destination)
  ((attacker-entity
    :initform nil
    :initarg  :attacker-entity
    :accessor attacker-entity)))

(defevent end-attack-melee-event (game-event-w-destination) ())

(defun send-end-attack-melee-event (dest)
  (propagate-end-attack-melee-event (make-instance 'end-attack-melee-event
						   :id-destination (identificable:id dest))))

(defevent attack-long-range-event (game-event-w-destination)
  ((attacker-entity
    :initform nil
    :initarg  :attacker-entity
    :accessor attacker-entity)))

(defevent end-attack-long-range-event (game-event-w-destination)
  ((attacker-entity
    :initform nil
    :initarg  :attacker-entity
    :accessor attacker-entity)))

(defun send-end-attack-long-range-event (dest)
  (propagate-end-attack-long-range-event (make-instance 'end-attack-long-range-event
							:id-destination (identificable:id dest))))

(defmacro check-event-targeted-to-me ((entity event) &body body)
  `(if (= (identificable:id ,entity) (id-destination ,event))
	 (progn
	   ,@body)
	 nil))

(defevent attack-spell-event (game-event-w-destination)
  ((attacker-entity
    :initform nil
    :initarg  :attacker-entity
    :accessor attacker-entity)
   (spell
    :initform nil
    :initarg  :spell
    :accessor spell)))

(defevent end-attack-spell-event (game-event-w-destination)
  ((attacker-entity
    :initform nil
    :initarg  :attacker-entity
    :accessor attacker-entity)
   (spell
    :initform nil
    :initarg  :spell
    :accessor spell)))

(defun send-end-attack-spell-event (dest)
  (propagate-end-attack-spell-event (make-instance 'end-attack-spell-event
						   :id-destination (identificable:id dest))))


(defevent spell-event (game-event-w-destination)
  ((attacker-entity
    :initform nil
    :initarg  :attacker-entity
    :accessor attacker-entity)
   (spell
    :initform nil
    :initarg  :spell
    :accessor spell)))

(defevent end-spell-event (game-event-w-destination)
  ((attacker-entity
    :initform nil
    :initarg  :attacker-entity
    :accessor attacker-entity)
   (spell
    :initform nil
    :initarg  :spell
    :accessor spell)))

(defun send-end-spell-event (dest)
  (propagate-end-spell-event (make-instance 'end-spell-event
					    :id-destination (identificable:id dest))))

(defevent lock-object-event (game-event-w-destination) ())

(defun send-lock-event (origin dest)
  (propagate-lock-object-event (make-instance 'lock-object-event
					      :id-origin      (identificable:id origin)
					      :id-destination (identificable:id dest))))

(defevent unlock-object-event (game-event-w-destination)
  ((force-unlock
    :initform nil
    :initarg  :force-unlock
    :reader   force-unlock-p
    :writer   (setf force-unlock))))

(defun send-unlock-event (origin dest &key (force nil))
  (propagate-unlock-object-event (make-instance 'unlock-object-event
						:id-origin      (identificable:id origin)
						:id-destination (identificable:id dest)
						:force-unlock   force)))

(defevent trap-triggered-event (game-event-w-destination) ())

(defun send-trap-triggered-event (origin dest)
  (propagate-trap-triggered-event (make-instance 'trap-triggered-event
						 :id-origin      (identificable:id origin)
						 :id-destination (identificable:id dest))))

(defevent other-interaction-event (game-event-w-destination) ())

(defun send-other-interaction-event (origin dest)
  (propagate-other-interaction-event (make-instance 'other-interaction-event
						    :id-origin      (identificable:id origin)
						    :id-destination (identificable:id dest))))

(defevent deactivate-trap-event (game-event-w-destination) ())

(defun send-deactivate-trap-event (origin dest)
  (propagate-deactivate-trap-event (make-instance 'deactivate-trap-event
						  :id-origin      (identificable:id origin)
						  :id-destination (identificable:id dest))))
