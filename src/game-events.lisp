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
	       (declare (ignore k))
	       (loop for i from 0 below (length v) do (setf (elt v i) nil)))
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
	       (remove el (,get-vector-symbol) :test #'= :key #'identificable:id)))
       (defun ,propagate-symbol (event)
	 (loop for ent across (,get-vector-symbol) do
	      (when (on-game-event ent event)
		(return-from ,propagate-symbol t)))
	 nil))))

(defevent end-turn () ())

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

(defevent update-highlight-path ()
  ((tile-pos
    :initform nil
    :initarg  :tile-pos
    :accessor tile-pos)))
