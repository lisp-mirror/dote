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

(in-package :action-scheduler)

(defclass game-action (identificable)
  ((priority
    :initform      0
    :initarg       :priority
    :accessor      priority
    :type          fixnum
    :documentation "lower -> more priority")
   (launch-action-fn
    :initform      (constantly t)
    :initarg       :launch-action-fn
    :accessor      launch-action-fn
    :type          function
    :documentation "Value is nil if something wrong happened")
   (capturable-types
    :initform      '()
    :initarg       :capturable-types
    :accessor      capturable-types
    :documentation
    "types that can be capured the current action (this instance) is a an action scheduler")))

(defmethod print-object ((object game-action) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "priority ~a" (priority object))))

;; unused
(defclass launch-arrow-action                (game-action) ())

(defclass attack-long-range-action           (game-action) ())

(defclass attack-long-range-imprecise-action (game-action) ())

(defclass attack-launch-spell-action         (game-action) ())

(defclass launch-spell-action                (game-action) ())

(defclass end-spell-action                   (game-action) ())

(defclass end-attack-spell-action            (game-action) ())

(defclass send-spell-fx-action               (game-action) ())

(defclass trigger-trap-attack-action         (game-action) ())

(defclass tactical-plane-action              (game-action) ())

(defclass tooltip-show-action                (game-action)
  ((label
    :initform  nil
    :initarg  :label
    :accessor label)))

(defmethod print-object ((object tooltip-show-action) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "label --~a-- pr ~a" (label object) (priority object))))

(defclass particle-effect-action             (game-action) ())

(defclass refresh-status-bar-action          (game-action) ())

(defclass blood-spill-action                 (game-action) ())

(defun %make-queue ()
  (make-instance 'qu:simple-queue))

(defclass action-scheduler (game-action)
  ((current-action
    :initform nil
    :initarg  :current-action
    :accessor current-action
    :type     game-action)
   (scheduled-actions
    :initform (%make-queue)
    :initarg  :scheduled-actions
    :accessor scheduled-actions)))

(defmethod initialize-instance :after ((object action-scheduler) &key &allow-other-keys)
  (setf (capturable-types object)
        '(send-spell-fx-action
          tooltip-show-action
          particle-effect-action
          refresh-status-bar-action
          blood-spill-action)))

(defmethod print-object ((object action-scheduler) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "current: ~a~%" (current-action object))
    (format stream "--queue--~%")
    (loop
       for i from 0
       for a across (qu:container (scheduled-actions object)) do
         (format stream "~a ~a~%" i a))
    (format stream "-- queue ends here--~%")))

(defmethod game-event:on-game-event ((object action-scheduler) (event game-event:end-turn))
  (with-accessors ((current-action current-action)
		   (scheduled-actions scheduled-actions)) object
    (setf current-action nil)
    (setf scheduled-actions (%make-queue))
    nil))

(defmethod on-game-event ((object action-scheduler) (event game-action-terminated))
  (declare (ignore event))
  #+debug-mode
  (progn
    (misc:dbg "subst start---------")
    (misc:dbg "~a" object))
  (substitute-action object)
  #+debug-mode
  (progn
    (misc:dbg "subst end---------")
    (misc:dbg "~a" object)))

(defgeneric recursive-current-action (object))

(defgeneric enqueue-action (object new-action))

(defgeneric substitute-action (object))

(defgeneric actions-queue-empty-p (object))

(defgeneric priority-range (object))

(defgeneric next-minimum-priority (object))

(defgeneric next-maximum-priority (object))

(defparameter *equeue-merge-to-subscheduler-p* t)

(defmethod enqueue-action ((object action-scheduler) (new-action game-action))
  (with-accessors ((current-action current-action)
		   (scheduled-actions scheduled-actions)
                   (capturable-types capturable-types)) object
    (if (not current-action)
	(progn
	  (setf current-action new-action)
	  (funcall (launch-action-fn current-action)))
        (if (and *equeue-merge-to-subscheduler-p*
                 (typep current-action 'action-scheduler)
                 (find-if  #'(lambda (a) (typep new-action a)) capturable-types))
            (enqueue-action current-action new-action)
            (qu:q-push scheduled-actions new-action)))
    (qu:q-sort scheduled-actions #'(lambda (a b) (< (priority a) (priority b))))
    #+debug-mode (misc:dbg "enqueued:~% ~a" object)))

(defmethod substitute-action ((object action-scheduler))
  (with-accessors ((current-action current-action)
		   (scheduled-actions scheduled-actions)) object
    (if (typep current-action 'action-scheduler)
        (progn
          (substitute-action current-action)
          (when (not (current-action current-action))
            (setf current-action nil)
            (substitute-action object)))
        (if (qu:q-empty-p scheduled-actions) ; not a subscheduler
            (setf current-action nil)
            (progn
              (setf current-action
                    (qu:q-pop scheduled-actions))
              (funcall (launch-action-fn current-action)))))))

(defmethod actions-queue-empty-p ((object action-scheduler))
  (not (current-action object)))

(defmethod priority-range ((object action-scheduler))
  "return (values maximum minimum) as numbers , (i. e. minimum and maximum priority)"
  (let ((range (loop for i in (map 'list
                                   #'priority
                                   (qu:container (scheduled-actions object)))
                  maximize i into max
                  minimize i into min finally (return (list max min)))))
    (values (first range) (second range))))

(defmethod next-minimum-priority ((object action-scheduler))
  (let ((min (priority-range object)))
    (1+ min)))

(defmethod next-maximum-priority ((object action-scheduler))
  (multiple-value-bind (min-priority max-priority)
      (priority-range object)
    (declare (ignore min-priority))
    (1- max-priority)))

(defmethod recursive-current-action ((object action-scheduler))
  (with-accessors ((current-action current-action)) object
    (if (typep current-action 'action-scheduler)
        (recursive-current-action current-action)
        current-action)))

(defparameter *default-action-priority* 0)

(defmacro with-enqueue-action ((world
                                &optional
                                (type-of-action 'action-scheduler:game-action)
                                (priority '*default-action-priority*))
                               &body body)
  (alexandria:with-gensyms (fn action-box)
    `(let* ((,fn         (lambda () (let ((*default-action-priority* ,priority)) ,@body)))
            (,action-box (make-instance ',type-of-action
                                        :launch-action-fn ,fn
                                        :priority         ,priority)))
       (action-scheduler:enqueue-action ,world ,action-box))))

(defmacro end-of-life-remove-from-action-scheduler (entity action-type-checked)
  `(game-state:with-world (world (entity:state ,entity))
     (setf (interfaces:end-of-life-callback ,entity)
           #'(lambda () (game-event:with-send-action-terminated-check-type
                            (world ,action-type-checked)
                          (world:remove-all-removeable world))))))

(defmacro with-enqueue-action-and-send-remove-after ((world action
                                                            &optional
                                                            (priority '*default-action-priority*))
                                                     &body body)
  (let ((params `(,world ,action ,priority)))
    `(action-scheduler:with-enqueue-action ,params
       (game-event:with-send-action-terminated-check-type ,(subseq params 0 2)
         ,@body))))

(defun tt ()
  (let ((scheduler (make-instance 'action-scheduler))
        (res       '()))
    (with-enqueue-action (scheduler action-scheduler)
      (appendf res (list :a1)))
    (with-enqueue-action (scheduler particle-effect-action)
      (appendf res (list :a2)))
    (with-enqueue-action (scheduler send-spell-fx-action)
      (appendf res (list :a3)))
    (format t "~2%prima -------~4%~a" scheduler)
    (substitute-action scheduler)
    (format t "~2%dopo -------~4%~a" scheduler)
    res))
