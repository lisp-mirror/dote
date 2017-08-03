;; dawn of the Era: a tactical game.
;; Copyright (C) 2017 cage

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

(in-package :goap)

(defstruct action
  (name          :idle :type symbol)
  (preconditions '()   :type list)
  (context-preconditions  '()   :type list)
  (effects       '()   :type list)
  (cost          1     :type fixnum))

(defclass planner-state ()
  ((variables
    :initarg   :variables
    :initform  0
    :accessor  variables)))

(defmethod print-object ((object planner-state) stream)
  (format stream "vars ~a" (variables object)))

(defmethod clone-into ((from planner-state) (to planner-state))
  (setf (variables to) (copy-tree (variables from)))
  to)

(defmethod clone ((object planner-state))
  (with-simple-clone (object 'planner-state)))

(defun action-equals (a b)
  (eq (action-name a) (action-name b)))

(defclass planner (graph)
  ((actions-bag
    :initarg  :actions-bag
    :initform '()
    :accessor actions-bag)
   (action-to-reach
    :initarg   :action-to-reach
    :initform  nil
    :accessor  action-to-reach)
   (parent-state
    :initarg   :parent-state
    :initform  nil
    :accessor  parent-state
    :type      planner)
   (g-cost
    :initarg   :g-cost
    :initform  0
    :accessor  g-cost)
   (current-state
    :initarg  :current-state
    :initform nil
    :accessor current-state
    :type     planner-state)
   (goal-state
    :initarg  :goal-state
    :initform nil
    :accessor goal-state
    :type     planner-state)))

(defmethod print-object ((object planner) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
            "curr: ~a~2%goal: ~a~%cost ~a action to reach: ~s"
            ;;(actions-bag   object)
            (current-state object)
            (goal-state    object)
            (g-cost        object)
            (and (action-to-reach object)
                 (action-name   (action-to-reach object))))))

(defmethod clone-into ((from planner) (to planner))
  (setf (actions-bag     to) (mapcar #'copy-action (actions-bag from))
        (action-to-reach to) (action-to-reach from)
        (parent-state    to) (parent-state    from)
        (g-cost          to) (g-cost          from)
        (current-state   to) (clone (current-state from))
        (goal-state      to) (clone (goal-state from)))
  to)

(defmethod clone ((object planner))
  (with-simple-clone (object 'planner)))

(defgeneric reachables-states (object))

(defgeneric goal-reached-p (object))

(defun make-action-condition (id value)
  (cons id value))

(defun condition-name (a)
  (car a))

(defun condition-value (a)
  (cdr a))

(defun condition-equals (a b)
  (and (eq (condition-name a)
           (condition-name b))
       (compare-condition-value (condition-value a)
                                (condition-value b))))

(defgeneric compare-condition-value (a b))

(defmethod  compare-condition-value ((a function) (b function))
  (eq (funcall a) (funcall b)))

(defmethod  compare-condition-value ((a function) b)
  (declare (ignorable a b))
  nil)

(defmethod  compare-condition-value (a (b function))
  (declare (ignorable a b))
  nil)

(defmethod  compare-condition-value (a b)
  (eq a b))

(defun conditions-superset (a b)
  (loop for i in b do
       (let ((found (find-condition-in-conditions-list a i)))
         (when (or (not found)
                   (not (condition-equals i found)))
           (return-from conditions-superset nil))))
  t)

(defun conditions-equals (a b)
  (and (= (length a) (length b))
       (conditions-superset a b)))

(defun find-condition-in-conditions-list (sequence value)
  (find (condition-name value)
        sequence
        :key #'condition-name))

(defun find-condition-in-state (state value)
  (find-condition-in-conditions-list (variables state) value))

(defun find-condition-in-actions (actions value &key (accessor-fn #'action-effects))
  (find-condition-in-conditions-list (funcall accessor-fn actions) value))

(defun find-condition-in-actions-fx (actions value)
  (find-condition-in-actions actions value :accessor-fn #'action-effects))

(defun find-condition-in-actions-preconditions (actions value)
  (find-condition-in-actions actions value :accessor-fn #'action-preconditions))

(defun state-diff (current goals)
  (let ((sum 0))
    (loop for goal in (variables goals) do
         (let ((found-in-current (find-condition-in-state current goal)))
           (when (or  (not found-in-current)
                      (not (compare-condition-value (condition-value goal)
                                                    (condition-value found-in-current))))
             (incf sum))))
    sum))

(defun state-equals (a b)
  (set-equal (variables a) (variables b) :test #'condition-equals))

(defun action-satisfise-goal-p (state goal action)
  (loop for goal-condition in (variables goal) do
       (let ((found-in-action  (find-condition-in-actions-fx action
                                                             goal-condition))
             (found-in-state   (find-condition-in-state      state
                                                             goal-condition)))
         ;; note: only the value of the variables can be different
         (when (and found-in-action
                    found-in-state
                    (condition-equals goal-condition found-in-action)
                    (not (condition-equals goal-condition found-in-state)))
           (return-from action-satisfise-goal-p found-in-action))))
  nil)

(defun action-context-preconditions-satisfied-p (action)
  (loop for precondition in (action-context-preconditions action) do
       (when (not (funcall precondition))
         (return-from action-context-preconditions-satisfied-p nil)))
  t)

(defun %apply-action (state goal-state action)
  (flet ((comp-name (a b) (eq (condition-name a)
                              (condition-name b)))
         (update-variables-to-state (action-vars target-state)
           (loop for i in (variables target-state) do
                (let ((effect-variable (assoc (car i) action-vars)))
                  (when effect-variable
                    (setf (cdr i) (cdr effect-variable)))))))
    (let* ((union-goal-fx       (union (action-effects action)
                                       (variables goal-state)
                                       :test #'comp-name))
           (union-goal-precon   (union (action-preconditions action)
                                       (variables goal-state)
                                       :test #'comp-name)))
      (assert (action-satisfise-goal-p state goal-state action))
      (setf (variables goal-state)
            (union union-goal-fx union-goal-precon :test #'comp-name))
      (update-variables-to-state (action-effects       action) goal-state)
      (update-variables-to-state (action-preconditions action) goal-state)
      (update-variables-to-state (action-effects       action) state)
      (loop for i in (action-preconditions action) do
           (let ((effect-variable (assoc (car i) (variables state))))
             (when (not effect-variable)
               (push (cons (car i) nil) (variables state)))))
      (loop for i in (action-effects action) do
           (let ((effect-variable (assoc (car i) (variables state))))
             (when (not effect-variable)
               (push (cons (car i) (cdr i)) (variables state))))))))

(defun apply-action (planner action)
    (let* ((new-planner          (clone planner))
           (goal-state           (goal-state new-planner))
           (state                (current-state new-planner)))
      (%apply-action state goal-state action)
      (setf (g-cost          new-planner) (+ (g-cost planner) (action-cost action))
            (action-to-reach new-planner) action)
      new-planner))

(defmethod reachables-states ((object planner))
  (with-accessors ((actions-bag actions-bag)
                   (goal-state goal-state)
                   (current-state current-state)) object
    (loop
       for action in actions-bag
       when (and (action-context-preconditions-satisfied-p action)
                 (action-satisfise-goal-p current-state goal-state action))
       collect
         (apply-action object action))))

(defgeneric plan-search (object))

(defgeneric build-plan (object))

(defgeneric add-action (object action))

(defgeneric find-action (object action))

(defun state-equal-p (a b)
  (conditions-equals (variables a) (variables b)))

(defun %goal-reached-p (current goal)
  (conditions-superset (variables current) (variables goal)))

(defmethod goal-reached-p ((object planner))
  (%goal-reached-p (current-state object) (goal-state object)))

(defmethod plan-search ((object planner))
  (labels ((find-node-in-set (the-set node)
             (let ((found (find (variables (current-state node))
                                the-set
                                :key #'(lambda (a) (variables (current-state a)))
                                :test #'conditions-equals)))
               found))
           (find-node-in-frontier (queue node)
             (pq:find-element queue
                              (current-state node)
                              :key-fn  #'current-state
                              :test-fn #'state-equal-p))
           (queue-comp (a b)
             (< (g-cost a) (g-cost b)))
           (queue-eq (a b)
             (= (g-cost a) (g-cost b))))
    (macrolet ((put-node-in-set (node the-set)
                 `(push ,node ,the-set))
               (remove-node-in-set (the-set node)
                 `(setf ,the-set (remove (variables ,node)
                                         ,the-set
                                         :key #'variables
                                         :test #'conditions-equals))))
      (let ((closed '()))
        (pq:with-min-queue (frontier #'queue-eq #'queue-comp #'identity)
          (pq:push-element frontier object)
          (do ((current (pq:pop-element frontier) (pq:pop-element frontier)))
              ((not current) ; the exit condition
               current)      ; the return value when exit condition is not nil
            (when (goal-reached-p  current)
              (return-from plan-search current))
            (put-node-in-set current closed)
            (loop for neighbor in (reachables-states current) do
                 (let* ((action-to-neighbor (action-to-reach neighbor))
                        (new-cost       (+ (g-cost current)
                                           (action-cost action-to-neighbor))))
                   (when (and (find-node-in-frontier frontier neighbor)
                              (< new-cost (g-cost (find-node-in-frontier frontier neighbor))))
                     (pq:remove-element frontier neighbor))
                   (when (and (find-node-in-set closed neighbor)
                              (< new-cost (g-cost (find-node-in-set closed neighbor))))
                     (remove-node-in-set closed neighbor))
                   (when (and (not (find-node-in-set closed neighbor))
                              (not (pq:find-element frontier neighbor)))
                     (setf (g-cost          neighbor) new-cost
                           (parent-state    neighbor) current
                           (action-to-reach neighbor) action-to-neighbor)
                     (pq:push-element frontier neighbor))))))))))

(defmethod build-plan ((object planner))
  (let ((raw-plan (plan-search object))
        (results  nil))
  (labels ((%build-plan (graph)
             (when (and graph
                        (action-to-reach graph))
               (push (action-name (action-to-reach graph)) results)
               (%build-plan (parent-state graph)))))
    (%build-plan raw-plan)
    (reverse results))))

(defmethod add-action ((object planner) action)
  (push action (actions-bag object)))

(defmethod find-action ((object planner) name)
  (find name (actions-bag object) :test #'eq :key #'action-name))
