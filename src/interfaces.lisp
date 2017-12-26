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

(in-package :interfaces)

(defclass destructible () ())

(defgeneric destroy (object))

(defmethod destroy ((object destructible)))

(defgeneric current-time (object))

(defgeneric main-light-pos (renderer))

(defgeneric main-light-pos-eye-space (renderer))

(defgeneric main-light-color (renderer))

(defgeneric elapsed-time (renderer))

(defgeneric get-camera-pos (renderer))

(defgeneric clone (object))

(defmethod clone (object))

(defgeneric clone-into (from to))

(defmethod  clone-into (from to))

(defgeneric copy-flat (object))

(defmethod  copy-flat (object))

(defgeneric copy-flat-into (from to))

(defmethod  copy-flat-into (from to))

(defmacro with-simple-clone ((object type))
  (alexandria:with-gensyms (res)
    `(let ((,res (make-instance ,type)))
       (clone-into ,object ,res)
       ,res)))

(defmacro with-simple-copy-flat ((object type))
  (alexandria:with-gensyms (res)
    `(let ((,res (make-instance ,type)))
       (copy-flat-into ,object ,res)
       ,res)))

(defgeneric initializedp (object))

(defgeneric serialize (object))

(defgeneric serialize-to-stream (object stream))

(defgeneric deserialize (object file))

(defmethod serialize (object)
  (format nil "~s" (marshal:marshal object)))

(defmethod serialize-to-stream (object stream)
  (prin1 (marshal:marshal object) stream))

(defmethod deserialize (object file)
  (declare (ignore object))
  (marshal:unmarshal (read-from-string (filesystem-utils:slurp-file file))))

(defclass renderizable ()
  ((compiled-shaders
    :initform nil
    :accessor compiled-shaders
    :initarg :compiled-shaders)))

(defmethod clone-into :after ((from renderizable) (to renderizable))
  (setf (compiled-shaders to) (compiled-shaders from))
  to)

(defmethod clone :after ((object renderizable))
  (with-simple-clone (object 'renderizable)))

(defmethod copy-flat-into :after ((from renderizable) (to renderizable))
  (setf (compiled-shaders to) (compiled-shaders from))
  to)

(defmethod copy-flat :after ((object renderizable))
  (with-simple-copy-flat (object 'renderizable)))

(defgeneric prepare-for-rendering (object))

(defgeneric update-for-rendering (object))

(defgeneric render (object renderer))

(defgeneric render-debug (object renderer))

(defgeneric render-for-reflection (object renderer))

(defgeneric pick-pointer-position (object renderer x y))

(defmethod  render-for-reflection (object renderer)
  (render object renderer))

(defgeneric calculate (object dt))

(defmethod prepare-for-rendering ((object renderizable)))

(defmethod render ((object renderizable) renderer))

(defmethod calculate ((object renderizable) dt))

(defmethod pick-pointer-position ((object renderizable) renderer x y)
  nil)

(defgeneric main-state (object))

(defgeneric to-sexp (object))

(defmethod to-sexp ((object number))
  object)

(defmethod to-sexp ((object (eql nil)))
  nil)

(defgeneric from-sexp (object sexp))

(defgeneric description-for-humans (object))

(defmethod description-for-humans ((object t))
  (if object " " nil))

(defmethod description-for-humans ((object (eql nil)))
  nil)

(defclass fading-away-entity ()
  ((fading-away-fn
    :initform #'(lambda (a b) (declare (ignore a b)) (values (sb-cga:identity-matrix) -1.0))
    :initarg :fading-away-fn
    :accessor fading-away-fn)))

(defclass inner-animation (fading-away-entity)
  ((start-time
    :initform 0.0
    :initarg :start-time
    :accessor start-time)
   (el-time
    :initform 0.0
    :initarg :el-time
    :accessor el-time)
   (animation-speed
    :initform 0.15
    :initarg :animation-speed
    :accessor animation-speed)))

(defclass animated-spritesheet ()
  ((frequency-animation
    :initform 5
    :initarg  :frequency-animation
    :accessor frequency-animation)
   (frame-count
    :initform 0
    :initarg  :frame-count
    :accessor frame-count)
   (texture-window-width
    :initform 0.0
    :initarg  :texture-window-width
    :accessor texture-window-width)
   (texture-horizontal-offset
    :initform 0.1
    :initarg  :texture-horizontal-offset
    :accessor texture-horizontal-offset)
   (animation-loop
    :initform nil
    :initarg  :animation-loop-p
    :reader animation-loop-p
    :writer (setf animation-loop))))

(definline animated-billboard-last-frame-reached-p (animation)
  (with-accessors ((texture-horizontal-offset texture-horizontal-offset)
                   (texture-window-width texture-window-width)) animation
    (not (< texture-horizontal-offset (num:d- 1.0 texture-window-width)))))

(defclass end-life-trigger ()
  ((repeat-trigger
    :initform nil
    :initarg  :repeat-trigger
    :reader   repeat-trigger-p
    :writer (setf repeat-trigger))
   (triggered
    :initform nil
    :initarg  :triggered
    :reader   triggered-p
    :writer (setf triggered))
   (end-of-life-callback
    :initform nil
    :initarg  :end-of-life-callback
    :accessor end-of-life-callback)))

(defmacro with-maybe-trigger-end-of-life ((object &optional (trigger-predicate nil)) &body body)
  (with-gensyms (triggered-p end-of-life-callback repeat-trigger-p)
    `(with-accessors ((,triggered-p triggered-p)
                      (,end-of-life-callback end-of-life-callback)
                      (,repeat-trigger-p repeat-trigger-p)) ,object
       (declare (function ,end-of-life-callback))
       (when (and (or ,repeat-trigger-p
                      (not ,triggered-p))
                  ,(if trigger-predicate
                       trigger-predicate
                       t))
         (and ,end-of-life-callback
              (funcall ,end-of-life-callback))
         (setf (triggered ,object) t)
         ,@body))))

(defun remove-end-of-life-callback (object)
  "First value is t if slot exists the second is the old value of the slot"
  (if (slot-exists-p object 'end-of-life-callback)
      (let ((old (slot-value object 'end-of-life-callback)))
        (setf (slot-value object 'end-of-life-callback) nil)
        (values t old))
      (values nil nil)))

(defgeneric removeable-from-world-p (object))

(defmethod  removeable-from-world-p ((object t))
  nil)

(defgeneric almost-removeable-from-world-p (object))

(defgeneric apply-damage (object damage &key &allow-other-keys))

(defgeneric faction-player-p (object &optional id-entity))

(defgeneric faction-ai-p (object &optional id-entity))

(defgeneric my-faction (object))

(defgeneric pawnp (object))

(defmethod pawnp (object)
  (declare (ignore object))
  nil)

(defgeneric (setf my-faction) (object value))
