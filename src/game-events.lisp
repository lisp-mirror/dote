(in-package :game-event)

(defclass generic-game-event ()
  ((origin
    :initform nil
    :initarg  :origin
    :accessor origin)
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
   (data
    :initform 0
    :initarg  :data
    :accessor data)))

(defclass game-event-w-destination (generic-game-event)
  ((destination
    :initform nil
    :initarg  :destination
    :accessor destination)))

(defclass game-event-procrastinated (generic-game-event)
  ((trigger-turn
    :initform nil
    :initarg  :trigger-turn
    :accessor trigger-turn)))

(defmacro defevent (name slots)
  (let* ((event-name       (format        nil "~:@(~a~)" name))
	 (event-symbol     (format-symbol t   "~a"       event-name))
	 (param-symbol     (format-symbol t   "~:@(~a-members~)" name))
	 (on-event-symbol  (format-symbol t   "~:@(get-on-~a~)" name))
	 (propagate-symbol (format-symbol t   "~:@(propagate-~a~)" name)))
  `(progn
     (defclass ,event-symbol (generic-game-event)
       ,slots)
     (defparameter ,param-symbol
       (make-fresh-array 0 nil 'entity:entity))
     (defun ,(format-symbol t "~:@(register-for-~a~)" name) (el)
       (vector-push-extend el ,param-symbol))
     (defun ,(format-symbol t "~:@(unregister-for-~a~)" name) (el)
       (setf ,param-symbol (remove el ,param-symbol :test #'identificable:id)))
     (defgeneric ,on-event-symbol (entity))
     (defun ,propagate-symbol (event)
       (loop for ent across ,param-symbol do
	    (when (and (,on-event-symbol ent)
		       (funcall (,on-event-symbol ent) ent event))
	      (return-from ,propagate-symbol ent)))
       nil))))

(defevent end-turn ())

(defevent healing-effect-turn
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
