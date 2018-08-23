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

(defmethod destroy ((object (eql nil)))
  nil)

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

;; to use with ms:initialize-unmarshalled-instance
(defgeneric post-deserialization-fix (object))

(defmethod post-deserialization-fix (object)
  object)

(defmethod post-deserialization-fix ((object (eql nil)))
  nil)

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

(defgeneric make-data-for-opengl (object))

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

(gen-type-p inner-animation)

(defclass animated-spritesheet ()
  ((anim-delay
    :initform 0
    :initarg  :anim-delay
    :accessor anim-delay)
   (frequency-animation
    :initform 5
    :initarg  :frequency-animation
    :accessor frequency-animation)
   (frame-count
    :initform -1
    :initarg  :frame-count
    :accessor frame-count
    :documentation "the total number of frames rendered in this animation (before a loop occurs)")
   (frame-idx
    :initform -1
    :initarg  :frame-idx
    :accessor frame-idx)
   (frames-number
    :initform -1
    :initarg  :frames-number
    :accessor frames-number
    :documentation "the total number of frames in this animation (before a loop occurs)")
   (texture-window-width
    :initform 0.0
    :initarg  :texture-window-width
    :accessor texture-window-width)
   (animation-loop
    :initform nil
    :initarg  :animation-loop-p
    :reader animation-loop-p
    :writer (setf animation-loop))))

(defun animated-billboard-last-frame-reached-p (animation)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((frame-count frame-count)
                   (frames-number frames-number)) animation
    (declare (fixnum frame-count frames-number))
    (= frame-count (1- frames-number))))

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
    :initform (constantly t)
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

(defclass keyframe-trigger ()
  ((end-frame-triggers
    :initform '()
    :initarg  :end-frame-triggers
    :accessor end-frame-triggers
    :documentation
    "Each element of the list  is a cons (:animation-type . function).
    Each function take as parameter this entity")
   (start-frame-triggers
    :initform '()
    :initarg  :start-frame-triggers
    :accessor start-frame-triggers
    :writer (setf repeat-trigger)
    :documentation
    "Each element of the list  is a cons (:animation-type . function).
    Each function take as parameter this entity")))

(defmacro with-ecase-keyframe-type (type start-form end-form)
  `(ecase ,type
      ((:start)
       ,start-form)
      ((:end)
       ,end-form)))

(defun find-triggers-list (object which)
  (with-accessors ((end-frame-triggers   end-frame-triggers)
                   (start-frame-triggers start-frame-triggers)) object
    (with-ecase-keyframe-type which
      start-frame-triggers
      end-frame-triggers)))

(defgeneric find-trigger (object animation-type which))

(defgeneric find-start-trigger (object animation-type))

(defgeneric find-end-trigger (object animation-type))

(defgeneric add-trigger (object animation-type which fn))

(defgeneric add-start-trigger (object animation-type fn))

(defgeneric add-end-trigger (object animation-type fn))

(defgeneric remove-trigger (object animation-type which))

(defgeneric remove-start-trigger (object animation-type))

(defgeneric remove-end-trigger (object animation-type))

(defgeneric elaborate-trigger (object animation-type which))

(defgeneric elaborate-start-trigger (object animation-type))

(defgeneric elaborate-end-trigger (object animation-type))

(defgeneric add-animation-one-shot-trigger (object animation-type which fn))

(defgeneric add-start-one-shot-trigger (object animation-type fn))

(defgeneric add-end-one-shot-trigger (object animation-type fn))

(defmethod find-trigger ((object keyframe-trigger) (animation-type symbol) which)
  (let* ((triggers (find-triggers-list object which))
         (trigger  (assoc animation-type triggers)))
    (and trigger
         (cdr trigger))
    object))

(defmethod find-start-trigger ((object keyframe-trigger) (animation-type symbol))
  (find-trigger object animation-type :start))

(defmethod find-end-trigger ((object keyframe-trigger) (animation-type symbol))
  (find-trigger object animation-type :end))

(defmethod add-trigger ((object keyframe-trigger) (animation-type symbol) which fn)
  (with-accessors ((end-frame-triggers   end-frame-triggers)
                   (start-frame-triggers start-frame-triggers)) object
    (with-ecase-keyframe-type which
      (push (cons animation-type fn) start-frame-triggers)
      (push (cons animation-type fn) end-frame-triggers))
    object))

(defmethod add-start-trigger ((object keyframe-trigger) (animation-type symbol) fn)
  (add-trigger object animation-type :start fn))

(defmethod add-end-trigger ((object keyframe-trigger) (animation-type symbol) fn)
  (add-trigger object animation-type :end fn))

(defmethod remove-trigger ((object keyframe-trigger) (animation-type symbol) which)
  (with-accessors ((end-frame-triggers   end-frame-triggers)
                   (start-frame-triggers start-frame-triggers)) object
    (let* ((triggers (find-triggers-list object which))
           (trigger  (assoc animation-type triggers)))
      (when trigger
        (flet ((%remove-trigger (all-triggers)
                 (remove-if #'(lambda (a) (eq (car a) (car trigger))) all-triggers)))
          (with-ecase-keyframe-type which
            (setf start-frame-triggers (%remove-trigger start-frame-triggers))
            (setf end-frame-triggers (%remove-trigger end-frame-triggers))))))
    object))

(defmethod remove-start-trigger ((object keyframe-trigger) (animation-type symbol))
  (remove-trigger object animation-type :start))

(defmethod remove-end-trigger ((object keyframe-trigger) (animation-type symbol))
  (remove-trigger object animation-type :end))

(defmethod elaborate-trigger ((object keyframe-trigger) (animation-type symbol) which)
  (let* ((triggers (find-triggers-list object which))
         (trigger  (assoc animation-type triggers)))
    (when trigger
      (funcall (cdr trigger) object))))

(defmethod elaborate-start-trigger ((object keyframe-trigger) (animation-type symbol))
  (elaborate-trigger object animation-type :start))

(defmethod elaborate-end-trigger ((object keyframe-trigger) (animation-type symbol))
  (elaborate-trigger object animation-type :end))

(defmethod add-animation-one-shot-trigger ((object keyframe-trigger) (animation-type symbol)
                                           which fn)
  (with-accessors ((end-frame-triggers   end-frame-triggers)
                   (start-frame-triggers start-frame-triggers)) object
    (with-ecase-keyframe-type which
       (add-start-trigger object
                          animation-type
                          #'(lambda (entity)
                              (funcall fn entity)
                              (remove-start-trigger entity animation-type)))
       (add-end-trigger object
                        animation-type
                        #'(lambda (entity)
                            (funcall fn entity)
                            (remove-end-trigger entity animation-type))))
    object))

(defmethod add-start-one-shot-trigger ((object keyframe-trigger) animation-type fn)
  (add-animation-one-shot-trigger object animation-type :start fn))

(defmethod add-end-one-shot-trigger ((object keyframe-trigger) animation-type fn)
  (add-animation-one-shot-trigger object animation-type :start fn))

(defgeneric removeable-from-world-p (object))

(defmethod  removeable-from-world-p ((object t))
  nil)

(defgeneric almost-removeable-from-world-p (object))

(defgeneric apply-damage (object damage &key &allow-other-keys))

(defgeneric remove-from-game (object))

(defgeneric faction-player-p (object &optional id-entity))

(defgeneric faction-ai-p (object &optional id-entity))

(defgeneric my-faction (object))

(defgeneric pawnp (object))

(defmethod pawnp (object)
  (declare (ignore object))
  nil)

(defgeneric (setf my-faction) (object value))

(defgeneric force-reinitialize (object))
