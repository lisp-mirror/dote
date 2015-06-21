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
    :accessor priority)))

(defmacro defevent (name &rest (slots nil))
  `(defclass ,(format-symbol t "~:@(~a~)" name) (generic-game-event)
     (,@slots)))
