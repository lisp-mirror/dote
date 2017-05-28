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

(in-package :random-object-messages)

(define-constant +poison-turn-arg-key+          :turn           :test #'eq)

(define-constant +modifier-characteristic-key+  :char           :test #'eq)

(define-constant +spell-id-key+                 :spell-id       :test #'eq)

(defun untrigged-effect-p-fn (valid-trigger)
  #'(lambda (a)
      (not (funcall (trigged-effect-p-fn valid-trigger) a))))

(defun trigged-effect-p-fn (valid-trigger)
  #'(lambda (a)
      (eq
       (random-object-messages:msg-trigger a)
       valid-trigger)))

(defun to-other-target-effect-p-fn ()
  #'(lambda (a)
      (eq
       (random-object-messages:msg-target a)
       basic-interaction-parameters:+target-other+)))

(defclass generic-msg ()
  ((msg-origin
    :initform 0.0
    :initarg :msg-origin
    :accessor msg-origin)
   (msg-target
    :initform nil
    :initarg :msg-target
    :accessor msg-target)
   (msg-trigger
    :initform nil
    :initarg :msg-trigger
    :accessor msg-trigger)))

(defmethod marshal:class-persistant-slots ((object generic-msg))
  '(msg-target
    msg-trigger
    msg-origin))

(defclass generic-msg-w-chance ()
  ((msg-chance
    :initform 0.0
    :initarg :msg-chance
    :accessor msg-chance)))

(defmethod marshal:class-persistant-slots ((object generic-msg-w-chance))
  (append '(msg-chance)
           (call-next-method)))

(defclass generic-msg-w-duration ()
  ((msg-duration
    :initform 0.0
    :initarg :msg-duration
    :accessor msg-duration)))

(defmethod marshal:class-persistant-slots ((object generic-msg-w-duration))
  (append '(msg-duration)
           (call-next-method)))

(defclass cause-poison-msg (generic-msg generic-msg-w-chance)
  ((msg-damage
    :initform 0.0
    :initarg :msg-damage
    :accessor msg-damage)))

(defmethod marshal:class-persistant-slots ((object cause-poison-msg))
  (append '(msg-damage)
           (call-next-method)))

(defmethod print-object ((object cause-poison-msg) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "dmg ~a target ~a chance ~a"
            (msg-damage object)
            (msg-target object)
            (msg-chance object))))

(defmacro gen-cause-*-msg (&rest msg-type)
  `(progn
     ,@(loop for msg in msg-type collect
            (let ((class-name (misc:format-fn-symbol t "cause-~a-msg" msg)))
            `(progn
               (defclass ,class-name (generic-msg
                                      generic-msg-w-chance
                                      generic-msg-w-duration)
                 ())
               (defmethod marshal:class-persistant-slots ((object ,class-name))
                 (append '()
                         (call-next-method))))))))

(gen-cause-*-msg terror faint berserk)

(defmacro gen-cure-*-msg (&rest msg-type)
  `(progn
     ,@(loop for msg in msg-type collect
            (let ((class-name (misc:format-fn-symbol t "cure-~a-msg" msg)))
            `(progn
               (defclass ,class-name (generic-msg generic-msg-w-chance) ())
               (defmethod marshal:class-persistant-slots ((object ,class-name))
                 (append '()
                         (call-next-method))))))))

(gen-cure-*-msg poison terror faint berserk)

(defmethod print-object ((object cure-poison-msg) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "target ~a chance ~a"
            (msg-target object)
            (msg-chance object))))

(defmacro gen-immune-*-msg (&rest msg-type)
  `(progn
     ,@(loop for msg in msg-type collect
            (let ((class-name (misc:format-fn-symbol t "immune-~a-msg" msg)))
            `(progn
               (defclass ,class-name (generic-msg
                                      generic-msg-w-chance
                                      generic-msg-w-duration)
                 ())
               (defmethod marshal:class-persistant-slots ((object ,class-name))
                 (append '()
                         (call-next-method))))))))

(gen-immune-*-msg poison terror faint berserk)

(defmacro gen-cancel-immune-*-msg (&rest msg-type)
  `(progn
     ,@(loop for msg in msg-type collect
            (let ((class-name (misc:format-fn-symbol t "cancel-immune-~a-msg" msg)))
            `(progn
               (defclass ,class-name (generic-msg
                                      generic-msg-w-chance
                                      generic-msg-w-duration)
                 ())
               (defmethod marshal:class-persistant-slots ((object ,class-name))
                 (append '()
                         (call-next-method))))))))

(gen-cancel-immune-*-msg poison terror faint berserk)

(defclass heal-damage-msg (generic-msg generic-msg-w-chance)
  ((msg-points
    :initform :use
    :initarg  :msg-points
    :accessor msg-points)))

(defclass magic-effect-msg (generic-msg)
  ((msg-spell-id
    :initform nil
    :initarg  :msg-spell-id
    :accessor msg-spell-id)))

(defclass modifier-effect-msg (generic-msg generic-msg-w-duration)
  ((msg-characteristic
    :initform nil
    :initarg  :msg-characteristic
    :accessor msg-characteristic)
   (msg-object-description
    :initform ""
    :initarg  :msg-object-description
    :accessor msg-object-description)
   (msg-modifier
    :initform 0.0
    :initarg  :msg-modifier
    :accessor msg-modifier)))

(defmethod print-object ((object modifier-effect-msg) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "target ~a"
            (msg-target object))))

(defmethod marshal:class-persistant-slots ((object modifier-effect-msg))
   (append '(msg-characteristic
             msg-object-description
             msg-modifier)
           (call-next-method)))

(defgeneric effect->msg (object id-origin &optional other-initargs))

(defgeneric effect->msg-complement (object id-origin &optional other-initargs))

(defmethod effect->msg ((object magic-effect-parameters) id-origin  &optional (other-initargs '()))
  (declare (ignore other-initargs))
  (make-instance 'magic-effect-msg
                 :msg-spell-id           (spell-id object)
                 :msg-trigger            (trigger  object)
                 :msg-target             +target-self+
                 :msg-origin             id-origin))

(defmethod effect->msg ((object effect-parameters) id-origin  &optional (other-initargs '()))
  (make-instance 'modifier-effect-msg
                 :msg-target             +target-self+
                 :msg-trigger            (trigger  object)
                 :msg-duration           (duration object)
                 :msg-origin             id-origin
                 :msg-characteristic     (getf other-initargs +modifier-characteristic-key+)
                 :msg-object-description ""
                 :msg-modifier           (modifier object)))

(defmethod effect->msg ((object poison-effect-parameters) id-origin
                        &optional (other-initargs '()))
  (declare (ignore other-initargs))
  (make-instance 'cause-poison-msg
                 :msg-damage  (points-per-turn object)
                 :msg-target  (target          object)
                 :msg-origin  id-origin
                 :msg-chance  (chance          object)
                 :msg-trigger (trigger         object)))

(defmethod effect->msg-complement ((object poison-effect-parameters) id-origin
                        &optional (other-initargs '()))
  (declare (ignore other-initargs))
  nil)

(defmethod effect->msg ((object heal-damage-points-effect-parameters) id-origin
                        &optional (other-initargs '()))
  (declare (ignore other-initargs))
  (make-instance 'heal-damage-msg
                    :msg-points  (points    object)
                    :msg-trigger (trigger   object)
                    :msg-origin  id-origin
                    :msg-target  (target    object)
                    :msg-chance  (chance    object)))

(defmethod effect->msg ((object healing-effect-parameters) id-origin
                        &optional (other-initargs '()))
  (cond
    ((eq other-initargs +heal-poison+)
     (make-instance 'cure-poison-msg
                    :msg-trigger (trigger object)
                    :msg-origin  id-origin
                    :msg-target  (target  object)
                    :msg-chance  (chance  object)))
    ((eq other-initargs +heal-terror+)
     (make-instance 'cure-terror-msg
                    :msg-trigger (trigger object)
                    :msg-origin  id-origin
                    :msg-target  (target  object)
                    :msg-chance  (chance  object)))
    ((eq other-initargs +heal-berserk+)
     (make-instance 'cure-berserk-msg
                    :msg-trigger (trigger   object)
                    :msg-origin  id-origin
                    :msg-target  (target    object)
                    :msg-chance  (chance    object)))
    ((eq other-initargs +heal-faint+)
     (make-instance 'cure-faint-msg
                    :msg-trigger (trigger   object)
                    :msg-origin  id-origin
                    :msg-target  (target    object)
                    :msg-chance  (chance    object)))
    ((eq other-initargs +cause-poison+)
     (make-instance 'cause-poison-msg
                    :msg-trigger (trigger   object)
                    :msg-origin id-origin
                    :msg-target (target     object)
                    :msg-chance (chance     object)))
    ((eq other-initargs +cause-terror+)
     (make-instance 'cause-terror-msg
                    :msg-trigger  (trigger  object)
                    :msg-duration (duration object)
                    :msg-origin   id-origin
                    :msg-target   (target   object)
                    :msg-chance   (chance   object)))
    ((eq other-initargs +cause-berserk+)
     (make-instance 'cause-berserk-msg
                    :msg-trigger  (trigger  object)
                    :msg-duration (duration object)
                    :msg-origin   id-origin
                    :msg-target   (target   object)
                    :msg-chance   (chance   object)))
    ((eq other-initargs +cause-faint+)
     (make-instance 'cause-faint-msg
                    :msg-trigger  (trigger  object)
                    :msg-duration (duration object)
                    :msg-origin   id-origin
                    :msg-target   (target   object)
                    :msg-chance   (chance   object)))
    ((eq other-initargs +immune-terror+)
     (make-instance 'immune-terror-msg
                    :msg-trigger  (trigger  object)
                    :msg-duration (duration object)
                    :msg-origin   id-origin
                    :msg-target   (target   object)
                    :msg-chance   (chance   object)))
    ((eq other-initargs +immune-berserk+)
     (make-instance 'immune-berserk-msg
                    :msg-trigger  (trigger  object)
                    :msg-duration (duration object)
                    :msg-origin   id-origin
                    :msg-target   (target   object)
                    :msg-chance   (chance   object)))
    ((eq other-initargs +immune-faint+)
     (make-instance 'immune-faint-msg
                    :msg-trigger  (trigger  object)
                    :msg-duration (duration object)
                    :msg-origin   id-origin
                    :msg-target   (target   object)
                    :msg-chance   (chance   object)))
    ((eq other-initargs +immune-poison+)
     (make-instance 'immune-poison-msg
                    :msg-trigger  (trigger  object)
                    :msg-duration (duration object)
                    :msg-origin   id-origin
                    :msg-target   (target   object)
                    :msg-chance   (chance   object)))))

(defmethod effect->msg-complement ((object healing-effect-parameters) id-origin
                        &optional (other-initargs '()))
  (cond
    ((eq other-initargs +cause-terror+)
     (make-instance 'cure-terror-msg
                    :msg-trigger (trigger object)
                    :msg-origin   id-origin
                    :msg-target   +target-self+
                    :msg-chance  1.0))
    ((eq other-initargs +cause-berserk+)
     (make-instance 'cure-berserk-msg
                    :msg-trigger (trigger object)
                    :msg-origin  id-origin
                    :msg-target  +target-self+
                    :msg-chance  1.0))
    ((eq other-initargs +cause-faint+)
     (make-instance 'cure-faint-msg
                    :msg-trigger (trigger object)
                    :msg-origin  id-origin
                    :msg-target  +target-self+
                    :msg-chance  1.0))
    ((eq other-initargs +immune-terror+)
     (make-instance 'cancel-immune-terror-msg
                    :msg-trigger (trigger object)
                    :msg-origin  id-origin
                    :msg-target  +target-self+
                    :msg-chance  1.0))
    ((eq other-initargs +immune-berserk+)
     (make-instance 'cancel-immune-berserk-msg
                    :msg-trigger (trigger object)
                    :msg-origin  id-origin
                    :msg-target  +target-self+
                    :msg-chance  1.0))
    ((eq other-initargs +immune-faint+)
     (make-instance 'cancel-immune-faint-msg
                    :msg-trigger (trigger object)
                    :msg-origin  id-origin
                    :msg-target  +target-self+
                    :msg-chance  1.0))
    ((eq other-initargs +immune-poison+)
     (make-instance 'cancel-immune-poison-msg
                    :msg-trigger (trigger object)
                    :msg-origin  id-origin
                    :msg-target  +target-self+
                    :msg-chance  1.0))))

(defun modifier-effect->effects-messages (params id-origin)
  (remove-if-null
   (loop for effect in params collect
        (effect->msg (cdr effect) id-origin
                     (list +modifier-characteristic-key+
                           (car effect))))))

(defun magic-effect->effects-messages (effect id-origin)
  (if effect
      (list (effect->msg effect id-origin))
      nil))

(defun health-effect->effects-messages (params id-origin)
  (remove-if-null
   (loop for effect in params collect
        (cond
          ;; cause
          ((eq (car effect) +cause-poison+)
           (effect->msg (cdr effect) id-origin))
          ((eq (car effect) +cause-berserk+)
           (effect->msg (cdr effect) id-origin +cause-berserk+))
          ((eq (car effect) +cause-faint+)
           (effect->msg (cdr effect) id-origin +cause-faint+))
          ((eq (car effect) +cause-terror+)
           (effect->msg (cdr effect) id-origin +cause-terror+))
          ;; healing
          ((eq (car effect) +heal-poison+)
           (effect->msg (cdr effect) id-origin +heal-poison+))
          ((eq (car effect) +heal-terror+)
           (effect->msg (cdr effect) id-origin +heal-terror+))
          ((eq (car effect) +heal-berserk+)
           (effect->msg (cdr effect) id-origin +heal-berserk+))
          ((eq (car effect) +heal-faint+)
           (effect->msg (cdr effect) id-origin +heal-faint+))
          ;; immune
          ((eq (car effect) +immune-poison+)
           (effect->msg (cdr effect) id-origin +immune-poison+))
          ((eq (car effect) +immune-terror+)
           (effect->msg (cdr effect) id-origin +immune-terror+))
          ((eq (car effect) +immune-berserk+)
           (effect->msg (cdr effect) id-origin +immune-berserk+))
          ((eq (car effect) +immune-faint+)
           (effect->msg (cdr effect) id-origin +immune-faint+))
          ((eq (car effect) +heal-damage-points+)
           (effect->msg (cdr effect) id-origin +heal-damage-points+))))))

(defun health-effect->effects-messages-complement (params id-origin)
  (remove-if-null
   (loop for effect in params collect
        (cond
          ((eq (car effect) +cause-berserk+)
           (effect->msg-complement (cdr effect) id-origin +cause-berserk+))
          ((eq (car effect) +cause-faint+)
           (effect->msg-complement (cdr effect) id-origin +cause-faint+))
          ((eq (car effect) +cause-terror+)
           (effect->msg-complement (cdr effect) id-origin +cause-terror+))
          ((eq (car effect) +immune-terror+)
           (effect->msg-complement (cdr effect) id-origin +immune-terror+))
          ((eq (car effect) +immune-berserk+)
           (effect->msg-complement (cdr effect) id-origin +immune-berserk+))
          ((eq (car effect) +immune-faint+)
           (effect->msg-complement (cdr effect) id-origin +immune-faint+))
          ((eq (car effect) +immune-poison+)
           (effect->msg-complement (cdr effect) id-origin +immune-poison+))))))

(defun remove-null-effects (params path)
  (remove-if #'(lambda (a) (null (cdr a)))
             (misc:plist-path-value params path)))

(defun params->effects-messages (entity)
  (let* ((params  (character:basic-interaction-params entity))
         (health  (health-effect->effects-messages
                   (remove-null-effects params (list +healing-effects+))
                   (id entity)))
         (effects (modifier-effect->effects-messages
                   (remove-null-effects params (list +effects+))
                   (id entity)))
         (spells  (magic-effect->effects-messages
                   (misc:plist-path-value params (list +magic-effects+))
                   (id entity))))
    (concatenate 'list health effects spells)))

(defun params->effects-messages-complement (entity)
  (let* ((params  (character:basic-interaction-params entity))
         (health (health-effect->effects-messages-complement
                  (remove-if #'(lambda (a) (null (cdr a)))
                             (misc:plist-path-value params (list +healing-effects+)))
                  (id entity))))
    health))

(defgeneric propagate-effects-msg  (sender target all-effects))

(defmethod propagate-effects-msg  ((sender identificable) (target identificable) all-effects)
  (propagate-effects-msg (id sender) (id target) all-effects))

(defmethod propagate-effects-msg ((sender number) (target number) all-effects)
  (dolist (effect all-effects)
    (cond
      ;; poison
      ((typep effect 'cause-poison-msg)
       (game-event:propagate-cause-poisoning-event
        (game-event:make-cause-poisoning-event sender target effect)))
      ((typep effect 'cure-poison-msg)
       (game-event:propagate-cure-poisoning-event
        (game-event:make-cure-poisoning-event sender target effect)))
      ((typep effect 'immune-poison-msg)
       (game-event:propagate-immune-poisoning-event
        (game-event:make-immune-poisoning-event sender target effect)))
      ((typep effect 'cancel-immune-poison-msg)
       (game-event:propagate-cancel-immune-poisoning-event
        (game-event:make-cancel-immune-poisoning-event sender target effect)))
      ;; terror
      ((typep effect 'cause-terror-msg)
       (game-event:propagate-cause-terror-event
        (game-event:make-cause-terror-event sender target effect)))
      ((typep effect 'cure-terror-msg)
       (game-event:propagate-cure-terror-event
        (game-event:make-cure-terror-event sender target effect)))
      ((typep effect 'immune-terror-msg)
       (game-event:propagate-immune-terror-event
        (game-event:make-immune-terror-event sender target effect)))
      ((typep effect 'cancel-immune-terror-msg)
       (game-event:propagate-cancel-immune-terror-event
        (game-event:make-cancel-immune-terror-event sender target effect)))
      ;; berserk
      ((typep effect 'cause-berserk-msg)
       (game-event:propagate-cause-berserk-event
        (game-event:make-cause-berserk-event sender target effect)))
      ((typep effect 'cure-berserk-msg)
       (game-event:propagate-cure-berserk-event
        (game-event:make-cure-berserk-event sender target effect)))
      ((typep effect 'immune-berserk-msg)
       (game-event:propagate-immune-berserk-event
        (game-event:make-immune-berserk-event sender target effect)))
      ((typep effect 'cancel-immune-berserk-msg)
       (game-event:propagate-cancel-immune-berserk-event
        (game-event:make-cancel-immune-berserk-event sender target effect)))
      ;; faint
      ((typep effect 'cause-faint-msg)
       (game-event:propagate-cause-faint-event
        (game-event:make-cause-faint-event sender target effect)))
      ((typep effect 'cure-faint-msg)
       (game-event:propagate-cure-faint-event
        (game-event:make-cure-faint-event sender target effect)))
      ((typep effect 'immune-faint-msg)
       (game-event:propagate-immune-faint-event
        (game-event:make-immune-faint-event sender target effect)))
      ((typep effect 'cancel-immune-faint-msg)
       (game-event:propagate-cancel-immune-faint-event
        (game-event:make-cancel-immune-faint-event sender target effect)))
      ;; heal dmg
      ((typep effect 'heal-damage-msg)
       (game-event:propagate-heal-damage-event
        (game-event:make-heal-damage-event sender target effect)))
      ;; modifier
      ((typep effect 'modifier-effect-msg)
       (game-event:propagate-modifier-object-event
        (game-event:make-modifier-object-event sender target effect))))))
