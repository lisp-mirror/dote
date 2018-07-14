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

(define-constant +ultimate-goal+                  :curb-threat :test #'eq)

(define-constant +action-max-cost+                1000000      :test #'eq)

(define-constant +simulated-attack-sample-number+      50      :test #'=)

(defstruct action
  (name                   :idle :type symbol)
  (preconditions          '()   :type list)
  (context-preconditions  '()   :type list)
  (effects                '()   :type list)
  (cost                     1   :type fixnum))

(defun maximize-action-cost (action)
  (setf (action-cost action) +action-max-cost+))

(defmacro gen-add-symbol-to-action-list-slot (slot)
  (let* ((fn-name      (format-fn-symbol t "add-action-~a" slot))
         (slot-fn-name (format-fn-symbol t "action-~as"    slot)))
    `(defun ,fn-name (action ,slot)
       (pushnew ,slot (,slot-fn-name action) :test #'condition-equals)
       action)))

(defmacro gen-find-symbol-to-action-list-slot (slot)
  (let* ((fn-name      (format-fn-symbol t "find-action-~a" slot))
         (slot-fn-name (format-fn-symbol t "action-~as"    slot)))
    `(defun ,fn-name (action ,slot)
       (find ,slot (,slot-fn-name action) :test #'eq :key #'condition-name))))

(defmacro gen-remove-symbol-to-action-list-slot (slot)
  (let* ((fn-name      (format-fn-symbol t "remove-action-~a" slot))
         (slot-fn-name (format-fn-symbol t "action-~as"    slot)))
    `(defun ,fn-name (action ,slot)
       (setf (,slot-fn-name action)
             (remove ,slot (,slot-fn-name action) :test #'eq :key #'condition-name)))))

(gen-add-symbol-to-action-list-slot effect)

(gen-add-symbol-to-action-list-slot precondition)

(gen-add-symbol-to-action-list-slot context-precondition)

(gen-find-symbol-to-action-list-slot effect)

(gen-find-symbol-to-action-list-slot precondition)

(defun find-action-context-precondition (action context-precondition)
  (find context-precondition
        (action-context-preconditions action)
        :test #'eq))

(gen-remove-symbol-to-action-list-slot effect)

(gen-remove-symbol-to-action-list-slot precondition)

(gen-remove-symbol-to-action-list-slot context-precondition)

(defun find-link-between-action (parent child)
  "find an effect of a that is precondition of b"
  (loop for parent-effect in (action-effects parent) do
       (let ((found (find-action-precondition child (condition-name parent-effect))))
         (when (and found
                    (condition-equals parent-effect found))
           (return-from find-link-between-action parent-effect))))
  nil)

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

(defgeneric reachables-states (object strategy-expert player-entity
                               &key ignore-context-preconditions))

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
           (when (or (not found-in-current)
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

(defun action-context-preconditions-satisfied-p (action strategy-expert player-entity)
  (misc:dbg "testing action ~a" action)
  (loop for precondition in (action-context-preconditions action) do
       (misc:dbg "testing precondition ~a -> ~a" precondition
                 (and (funcall precondition strategy-expert player-entity)
                      t))
       (when (not (funcall precondition strategy-expert player-entity))
         (return-from action-context-preconditions-satisfied-p nil)))
  (misc:dbg "choosen!")
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

(defmethod reachables-states ((object planner) strategy-expert player-entity
                              &key (ignore-context-preconditions nil))
  (with-accessors ((actions-bag actions-bag)
                   (goal-state goal-state)
                   (current-state current-state)) object
    (loop
       for action in actions-bag
       ;; action-satisfise-goal-p comes for first to exploit short circuit effect of 'and'
       when (and (action-satisfise-goal-p current-state goal-state action)
                 (or ignore-context-preconditions
                     (action-context-preconditions-satisfied-p action
                                                               strategy-expert
                                                               player-entity)))
       collect
         (apply-action object action))))

(defgeneric plan-search (object strategy-expert player-entity))

(defgeneric build-plan (object strategy-expert player-entity))

(defgeneric add-action (object action))

(defgeneric find-action (object action-name))

(defgeneric fetch-sink-action (object))

(defun state-equal-p (a b)
  (conditions-equals (variables a) (variables b)))

(defun %goal-reached-p (current goal)
  (conditions-superset (variables current) (variables goal)))

(defmethod goal-reached-p ((object planner))
  (%goal-reached-p (current-state object) (goal-state object)))

(defmethod plan-search ((object planner) strategy-expert player-entity)
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
            (when (goal-reached-p current)
              (return-from plan-search current))
            (put-node-in-set current closed)
            (loop for neighbor in (reachables-states current strategy-expert player-entity) do
                 (let* ((action-to-neighbor (action-to-reach neighbor))
                        (new-cost           (+ (g-cost current)
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

(defmethod build-plan ((object planner) strategy-expert player-entity)
  (let ((raw-plan (plan-search object strategy-expert player-entity))
        (results  nil))
    (labels ((%build-plan (graph)
               (when (and graph
                          (action-to-reach graph))
                 (push (action-name (action-to-reach graph)) results)
                 (%build-plan (parent-state graph)))))
      (%build-plan raw-plan)
      (when (and results
                 (not (eq (first-elt results) ai-utils:+idle-action+)))
        (push ai-utils:+plan-stopper+ results))
      (reverse results))))

(defmethod add-action ((object planner) action)
  (push action (actions-bag object)))

(defmethod find-action ((object planner) action-name)
  (find action-name (actions-bag object) :test #'eq :key #'action-name))

(defmethod fetch-sink-action ((object planner))
  (find-action object ai-utils:+sink-action+))

(defun dfs-action-search (planner action-start)
  (labels ((action-satisfise-p (action-node action-neighbor)
             (loop for precondition in (action-preconditions action-node) do
                  (let ((found-in-neighbor (find-condition-in-actions-fx action-neighbor
                                                                         precondition)))
                    ;; note: only the value of the variables can be different
                    (when (and found-in-neighbor
                               (condition-equals precondition found-in-neighbor))
                      (return-from action-satisfise-p found-in-neighbor))))
             nil)
           (get-connected (current-action)
             (loop
                for candidate in (actions-bag planner)
                when (action-satisfise-p current-action candidate)
                collect candidate))
           (action->string (action)
             (format nil "~a" (action-name action))))
    (let ((graph `(:graph nil))
          (edges '()))
      (stack:with-stack (#'(lambda (a b) (eq (action-name a) (action-name b)))
                           #'identity)
        (stack:push action-start)
        (do ((visited (stack:pop) (stack:pop))
             (res nil))
            ((not visited) res)
          (when (not
                 (find visited res :key stack:*key-function* :test
                       stack:*equal-function*))
            (push visited res)
            (setf graph
                  (append graph
                          (list (list :node (list (list :id    (action->string visited))
                                                  (list :label (action->string visited)))))))
            (loop for i in (get-connected visited) do
                 (setf edges
                       (append edges
                               (list (list :edge (list (list :from (action->string visited))
                                                       (list :to   (action->string i)))))))
                 (stack:push i)))))
      (append graph edges))))

(defun render-action-planner-ps (planner action-name output-file)
  (let ((action (find-action planner action-name)))
    (when action
      (let ((graph (dfs-action-search planner action)))
        (s-dot:render-s-dot output-file "ps" graph)))))

(defun dump-planner-chart (planner)
  (loop for i in (actions-bag planner) do
       (render-action-planner-ps planner
                                 (action-name i)
                                 (fs:file-in-package (format nil
                                                             "~a.eps"
                                                             (action-name i))))))

(defun %build-precondition (sym)
  (if (symbolp sym)
      (let ((name (symbol-name sym)))
        (if (char= (elt name 0) #\!)
            `(lambda (strategy-expert player-entity)
               (not (funcall (symbol-function ',(misc:format-fn-symbol t
                                                                       "~a"
                                                                       (subseq name 1)))
                             strategy-expert player-entity)))
            `(symbol-function ',sym)))
      sym))

(defun sink-precondition-test (a b)
  (declare (ignore a b))
  nil)

(defparameter *planner* nil)

(defun lint-find-fountain (planner)
  (let ((action-find-fountain (find-action planner ai-utils:+find-fountain-action+))
        (mandatory-test-name  #'is-there-useful-reachable-fountain-p))
    (when action-find-fountain
      (when (not (find-action-context-precondition action-find-fountain
                                                   mandatory-test-name))
        (error (format nil "error: action ~a need context precondition ~a"
                       ai-utils:+find-fountain-action+
                       #'is-there-useful-reachable-fountain-p))))))

(defun lint-attack-with-magic-user (planner ghost)
  (let ((action-attack (find-action planner ai-utils:+attack-action+))
        (magic-user-p  (character:pclass-of-magic-user-p ghost)))
    (when (and action-attack
               magic-user-p)
      (warn (format nil "error: action ~a ignored for a magic user as ~a"
                    ai-utils:+attack-action+
                    (character:player-class ghost))))))

(defun lint-planner (planner &key (ghost nil))
  (lint-find-fountain planner)
  (when ghost
    (lint-attack-with-magic-user planner ghost)))

(defmacro define-planner (&body forms)
  (labels ((get-param (params a &optional (default nil))
             (getf params a default))
           (consify (sequence)
             (loop for pair in sequence collect
                  (cons (first pair) (second pair))))
           (build-action (planner action)
             `(add-action ,planner
                          (make-action :name                  ,(get-param action :name)
                                       :preconditions         ',(consify (get-param action
                                                                                    :preconditions))
                                       :effects               ',(consify (get-param  action
                                                                                     :effects))
                                       :cost                  ,(get-param  action :cost)
                                       :context-preconditions
                                       (list ,@(mapcar #'%build-precondition
                                                       (get-param  action
                                                                   :context-preconditions)))))))
    (with-gensyms (planner idle-action sink-action)
      `(let ((,planner (make-instance 'planner))
             (,idle-action (make-action :name                  ai-utils:+idle-action+
                                        :preconditions         nil
                                        :context-preconditions nil
                                        :effects               (list (cons +ultimate-goal+ t))
                                        :cost                  +action-max-cost+))
             (,sink-action (make-action :name                  ai-utils:+sink-action+
                                        :preconditions         nil
                                        :context-preconditions (list #'sink-precondition-test)
                                        :effects               nil
                                        :cost                  +action-max-cost+)))

         ,@(loop for action in forms collect
                (build-action planner action))
         (add-action ,planner ,idle-action)
         (add-action ,planner ,sink-action)
         ;;(lint-planner ,planner)
         (setf *planner* ,planner)))))

(defun find-planner-file (character strategy)
  (assert (typep character 'character:player-character))
  (assert (find strategy
                (list +explore-strategy+
                      +defend-strategy+
                      +retreat-strategy+
                      +attack-strategy+)))
  (let* ((resource-dir (cond
                         ((eq strategy +explore-strategy+)
                          +explore-planner-dir+)
                         ((eq strategy +defend-strategy+)
                          +defend-planner-dir+)
                         ((eq strategy +retreat-strategy+)
                          +retreat-planner-dir+)
                         ((eq strategy +attack-strategy+)
                          +attack-planner-dir+)))
         (file-name      (string-downcase (symbol-name (character:player-class character))))
         (file-full-path (text-utils:strcat file-name ai-utils:+planner-file-extension+)))
    (res:get-resource-file file-full-path resource-dir)))

(defun load-planner-file (file ghost)
  (fs:file-is-link-if-else (file link)
    (progn
      #+debug-mode (misc:dbg "planner ~a point to ~a" file link)
      (load-planner-file link ghost))
    (with-load-forms-in-var (*planner* out file)
      (let ((goal-state  (make-instance 'planner-state
                                        :variables (list (cons +ultimate-goal+  t))))
            (start-state (make-instance 'planner-state
                                        :variables (list (cons +ultimate-goal+  nil)))))
        (setf (goal-state    out) goal-state
              (current-state out) start-state)
        (lint-planner out :ghost ghost)
        out))))

(defmacro defgoap-test (name args &body body)
  (let* ((function-clear-cache-name (format-fn-symbol t "~a-clear-cache" name))
         (function-name             (format-fn-symbol t "~:@(~a~)" name))
         (cache-name                (format-fn-symbol t "~:@(cache~)")))
    (multiple-value-bind (forms declaration)
        (parse-body body)
      (with-gensyms (res)
        `(let ((,cache-name +cache-invalid-value+))
           (defun ,function-clear-cache-name ()
             (setf ,cache-name +cache-invalid-value+))
           (defun ,function-name (,@args)
             ,@declaration
             (if (not (eq ,cache-name +cache-invalid-value+))
                 (progn
                   #+debug-mode (misc:dbg "using goap cache for ~a ~a" ',function-name ,cache-name)
                   ,cache-name)
                 (let ((,res (progn ,@forms)))
                   (setf ,cache-name ,res)
                   ,cache-name))))))))

(defun debug-break (strategy-expert entity)
  (declare (ignore strategy-expert entity))
  (break)
  t)

(defun enough-health-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (let ((res (not (ai-utils:too-low-health-p entity))))
    (misc:dbg "not too low health ~a" res)
    res))

(defun near-to-death-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (ai-utils:near-to-death-health-p entity))

(defun has-weapon-in-inventory-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (character:find-item-in-inventory-if (entity:ghost entity)
                                       #'interactive-entity:weaponp))

;; unused
(defun reachable-position-p (entity attack-goals weapon-type)
  (find-if #'(lambda (a) (= (blackboard:entity-id a) (id entity)))
           (getf attack-goals weapon-type)))

(defgoap-test reachable-opt/path-current-weapon-and-mp (strategy-expert entity)
  "using  the current  movement points  of the  entity is  possible to
reach the enemy with optimal path?"
  (let* ((reachable-fn (reachable-p-w/concening-tiles-fn strategy-expert))
         (res          (best-attackable-position-exists-path strategy-expert
                                                             entity
                                                             reachable-fn)))
    res))

(defgoap-test is-in-attack-pos-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (blackboard:entity-in-valid-attackable-pos-p entity))

(defgoap-test can-move-near-attack-pos (strategy-expert entity)
  (let* ((reachable-fn (reachable-p-w/concening-tiles-unlimited-cost-fn strategy-expert))
         (res
          (blackboard:best-path-near-attack-goal-w-current-weapon strategy-expert
                                                                  entity
                                                                  :cut-off-first-tile nil
                                                                  :reachable-fn-p
                                                                  reachable-fn)))
    (and res
         (> (length res) 1))))

(defgoap-test can-move-near-enemy-pos (strategy-expert entity)
  (let* ((res (blackboard:best-path-near-enemy-pos-w-current-weapon strategy-expert
                                                                    entity
                                                                    :cut-off-first-tile nil)))
    (and res
         (> (length res) 1))))

(defgoap-test can-move-near-enemy-pos-insecure (strategy-expert entity)
  (let* ((res (blackboard::insecure-path-near-enemy-pos-w-current-weapon strategy-expert
                                                                    entity
                                                                    :cut-off-first-tile nil)))
    (and res
         (> (length res) 1))))

(defgoap-test exists-attack-goal-w-current-weapon-p (strategy-expert entity)
  "check the mere existence"
  (ai-utils:attackable-position-exists strategy-expert entity
                                       #'blackboard:reachable-constantly-t))

(defgoap-test exists-opt-attack-goal-w-current-weapon-p (strategy-expert entity)
  (let ((res (best-attackable-position-exists-path strategy-expert entity
                                              #'blackboard:reachablep)))
    res))

(defgoap-test exists-insecure-attack-goal-w-current-weapon-p (strategy-expert entity)
  (let ((res (insecure-attackable-position-exists-path strategy-expert entity
                                                       #'blackboard:reachablep)))
    res))

(defgoap-test exists-visible-enemy (strategy-expert entity)
  (declare (ignore strategy-expert))
  (let ((res (ai-utils:target-attack/damage-spell entity)))
    res))

(defun simulated-weapon-damage-average (attacker defender)
  (let ((sum (loop repeat +simulated-attack-sample-number+ sum
                  (battle-utils:simulate-attack-w-current-weapon attacker defender))))
    (num:d/ sum
            (num:d +simulated-attack-sample-number+))))

(defun probably-stronger-than-enemy-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (with-accessors ((state        state)
                   (entity-ghost ghost)) entity
    (if (or (character:pclass-wizard-p entity-ghost)
            (character:pclass-healer-p entity-ghost))
        (if (and ;; (dice:pass-d6 4)
                 (= (length (player-entities state))
                    1))
            :only-one-opponent
            (let* ((target       (ai-utils:target-attack/damage-spell entity))
                   (target-ghost (ghost target)))
              (and target
                   (< (num:d/ (character:combined-power target-ghost)
                              (character:combined-power entity-ghost))
                      0.5))))
        (let* ((target       (ai-utils:target-attack/damage-spell entity))
               (target-ghost (ghost target))
               (simulated-damage (simulated-weapon-damage-average entity target)))
          (>= simulated-damage
              (num:d* 0.5 (character:current-damage-points target-ghost)))))))

(defgoap-test reachable-opt/path-attack-current-weapon-and-mp (strategy-expert entity)
    "using the  current movement points  of the entity is  possible to
reach  and attack  the enemy  with optimal  path?  Note:  path can  be
composed by just one tile, see 'attackable-position-exists-path'"
  (let* ((reachable-fn (reachable-p-w/concening-tiles-fn strategy-expert)))
    (multiple-value-bind (reachablep cost)
        (best-attackable-position-exists-path strategy-expert entity reachable-fn)
      (let ((attack-cost (battle-utils:cost-attack-w-current-weapon entity)))
        (and reachablep
             attack-cost ;; attack-cost is nil if no weapon is carried
             (<= (+ cost attack-cost)
                 (character:current-movement-points (entity:ghost entity))))))))

(defgoap-test reachable-insecure/path-attack-current-weapon-and-mp (strategy-expert entity)
    "using the  current movement points  of the entity is  possible to
reach  and attack  the enemy  with a suboptimal  path?  Note:  path can  be
composed by just one tile, see 'attackable-position-exists-path'"
  (let* ((reachable-fn (reachable-p-w/o-concening-tiles-fn strategy-expert)))
    (multiple-value-bind (reachablep cost)
        (insecure-attackable-position-exists-path strategy-expert entity reachable-fn)
      (misc:dbg "path insecure ~a" reachablep cost)
      (let ((attack-cost (battle-utils:cost-attack-w-current-weapon entity)))
        (and reachablep
             attack-cost ;; attack-cost is nil if no weapon is carried
             (<= (+ cost attack-cost)
                 (character:current-movement-points (entity:ghost entity))))))))

(defgoap-test friend-needs-help-p (strategy-expert entity)
  (ai-utils:friend-who-needs-help strategy-expert entity :exclude-me t))

(defun no-friend-needs-help-p (strategy-expert entity)
  (not (friend-needs-help-p strategy-expert entity)))

(defgoap-test someone-needs-help-p (strategy-expert entity)
  (ai-utils:friend-who-needs-help strategy-expert entity :exclude-me nil))

(defgoap-test none-needs-help-p (strategy-expert entity)
  (not (someone-needs-help-p strategy-expert entity)))

(defgoap-test has-weapon-inventory-or-worn-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (with-accessors ((ghost entity::ghost)) entity
    (or (character:weapon-type ghost)
        (character:find-item-in-inventory-if ghost #'interactive-entity:weaponp))))

(defun %has-enough-sp-if (entity predicate)
  (character:with-no-terror-status (entity)
    (with-accessors ((ghost ghost)) entity
      (spell:filter-spell-set (character:available-spells-list ghost)
                              predicate))))

(defun %has-enough-sp-p (entity tag)
  (character:with-no-terror-status (entity)
    (character:castable-spells-list-by-tag (entity:ghost entity) tag)))

(defgoap-test has-enough-sp-heal-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (%has-enough-sp-p entity spell:+spell-tag-heal+))

(defgoap-test has-enough-sp-damage-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (%has-enough-sp-p entity spell:+spell-tag-damage+))

(defgoap-test has-enough-sp-attack-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (%has-enough-sp-if entity #'(lambda (a) (not (spell:attack-spell-p a)))))

(defgoap-test has-enough-sp-break-wall-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (%has-enough-sp-p entity spell:+spell-tag-remove-wall+))

(defgoap-test has-enough-sp-teleport-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (%has-enough-sp-p entity spell:+spell-tag-teleport+))

;; unused
(defgoap-test there-is-reachable-help-needed-friend-heal-spell-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (when-let ((available-spells (ai-utils:available-heal-spells entity)))
    (ai-utils:reachable-help-needed-friend-heal-spell-p available-spells entity)))

(defgoap-test there-is-attackable-opponents-attack-spell-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (when-let ((available-spells (ai-utils:available-attack-spells entity)))
    (let ((res (ai-utils:attackable-opponents-attack-spell available-spells entity)))
      (dbg "there-is-attackable-opponents-attack-spell-p ~a"
           res)
      res)))

(defmacro gen-is-status-tests (status)
  (let ((name-fn      (format-fn-symbol t "is-status-~a-p"       (symbol-name status)))
        (name-test-fn (format-fn-symbol :character "status-~a-p" (symbol-name status))))
    `(defun  ,name-fn (strategy-expert entity)
       (declare (ignore strategy-expert))
       (,name-test-fn (entity:ghost entity)))))

(gen-is-status-tests poisoned)

(gen-is-status-tests terror)

(gen-is-status-tests berserk)

(gen-is-status-tests faint)

(defun disobey-1-out-100 (strategy-expert entity)
  (declare (ignore strategy-expert entity))
  (dice:pass-d100.0 1))

(defun disobey-1-out-10 (strategy-expert entity)
  (declare (ignore strategy-expert entity))
  (dice:pass-d10.0 1))

(defun pass-1d10 (strategy-expert entity)
  (declare (ignore strategy-expert entity))
  (dice:pass-d10.0 1))

(defun pass-1d4 (strategy-expert entity)
  (declare (ignore strategy-expert entity))
  (dice:pass-d4 1))

(defun pass-1d6 (strategy-expert entity)
  (declare (ignore strategy-expert entity))
  (dice:pass-d6 1))

(defun able-to-move-if (entity predicate)
  (with-accessors ((state entity:state)
                   (ghost entity:ghost)) entity
    (let* ((neigh-cost (gen-neigh-costs entity
                                        #'(lambda (a)
                                            (game-state:get-cost state (elt a 0) (elt a 1))))))
      (>= (character:current-movement-points ghost)
          (funcall predicate neigh-cost)))))

(defgoap-test can-rotate-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (with-accessors ((ghost ghost)) entity
    (>= (character:current-movement-points ghost)
        +rotate-entity-cost-cost+)))

(defgoap-test can-minimally-move-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (able-to-move-if entity #'num:find-min))

(defgoap-test able-to-explore-p (strategy-expert entity)
  (with-accessors ((state entity:state)
                   (ghost entity:ghost)) entity
    (multiple-value-bind (path cost)
        (ai-utils:next-explore-position strategy-expert entity)
      (let* ((res (and path
                       (<= cost
                           (character:current-movement-points ghost)))))
        (misc:dbg "able-to-explore cost ~a -> ~a" cost res)
        res))))

(defgoap-test is-able-to-flee-p (strategy-expert entity)
  (with-accessors ((state entity:state)
                   (ghost entity:ghost)) entity
    (multiple-value-bind (pos cost-next-flee-pos)
        (ai-utils:next-flee-position strategy-expert entity)
      (and pos
           (>= (character:current-movement-points ghost)
               cost-next-flee-pos)))))

(defgoap-test is-there-hiding-place-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (with-accessors ((state entity:state)) entity
    (let ((all-opponents-can-see-me (all-other-factions-can-see-entity state entity)))
      (ai-utils:go-find-hiding-place entity all-opponents-can-see-me))))

(defgoap-test is-visible-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (with-accessors ((state entity:state)) entity
    (let* ((opposite-faction (faction->opposite-faction (my-faction entity)))
           (visibles-from-pcs
            (absee-mesh:visible-players-in-state-from-faction state opposite-faction)))
      (find entity visibles-from-pcs :test #'test-id=))))

(defgoap-test has-wall-near-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (let ((wall (find-nearest-visible-wall entity)))
    wall))

(defgoap-test has-trap-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (with-accessors ((ghost entity:ghost)) entity
    (character:find-item-in-inventory-if ghost #'interactive-entity:trapp)))

(defgoap-test can-place-trap-p (strategy-expert entity)
  (with-accessors ((main-state main-state)) strategy-expert
    (with-accessors ((map-state map-state)) main-state
      (entity:with-player-cost-pos (entity x y)
        (let ((neigh (matrix:gen-valid-4-neighbour-ccw map-state
                                                       x y
                                                       :add-center nil)))
          (when (= (length neigh) 4) ; just the most simple case
            (flet ((tile-high-cost-p (pos)
                     (> (get-cost main-state (elt pos 0) (elt pos 1))
                        (/ +invalicable-element-cost+ 4))))
              (let ((a (elt neigh 0))
                    (b (elt neigh 1))
                    (c (elt neigh 2))
                    (d (elt neigh 3)))
                (and (or (and (tile-high-cost-p a)
                              (tile-high-cost-p c))
                         (and (tile-high-cost-p b)
                              (tile-high-cost-p d)))
                     (mesh:trap-can-be-placed-p entity))))))))))

(defgoap-test find-good-places-to-protect (strategy-expert entity)
  (ai-utils:good-places-to-protect strategy-expert entity))

(defgoap-test is-near-weak-friend-p (strategy-expert entity)
  (let ((res (ai-utils:near-weak-friend-p strategy-expert entity)))
    #+ (and debug-mode debug-ai)
    (dbg "is-near-weak-friend-p -> ~a" res)
    res))

(defgoap-test can-attack-when-near-pos-p (strategy-expert entity)
  "note: places-near contains only reachable tiles so
path-near-goal-w/o-concerning-tiles always returns a non nil value"
  (let ((res (ai-utils:attack-when-near-pos-p strategy-expert entity)))
    #+ (and debug-mode debug-ai)
    (dbg "can-attack-when-near-pos-p -> ~a" res)
    res))

(defgoap-test enough-mp-to-attack-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (when-let ((attack-cost (battle-utils:cost-attack-w-current-weapon entity)))
    (<= attack-cost
        (character:current-movement-points (entity:ghost entity)))))

(defgoap-test is-there-useful-reachable-fountain-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (ai-utils:useful-reachable-fountain entity))

(defgoap-test exists-reachable-pos-to-launch-attack-spell (strategy-expert entity)
  (declare (ignore strategy-expert))
  (let ((res (ai-utils:reachable-attackable-opponents-attack-spell entity)))
    res))

(defgoap-test exists-reachable-pos-to-launch-heal-spell (strategy-expert entity)
  (declare (ignore strategy-expert))
  (let ((res (ai-utils:reachable-healing-friend-heal-spell entity)))
    res))

(defgoap-test exists-reachable-pos-to-launch-damage-spell (strategy-expert entity)
  (declare (ignore strategy-expert))
  (let ((res (ai-utils:reachable-attackable-opponents-damage-spell entity)))
    res))

(defmacro with-ensure-available-spells ((available-spells entity selector) &body body)
  `(when-let ((,available-spells (,selector ,entity)))
     ,@body))

(defgoap-test can-launch-attack-spell-current-pos-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (with-ensure-available-spells (available-spells
                                 entity
                                 ai-utils:available-attack-spells)
    (ai-utils:attackable-opponents-attack-spell available-spells entity)))

(defgoap-test can-launch-damage-spell-current-pos-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (with-ensure-available-spells (available-spells
                                 entity
                                 ai-utils:available-damage-spells)
    (ai-utils:attackable-opponents-damage-spell available-spells entity)))

(defgoap-test can-launch-heal-spell-current-pos-p (strategy-expert entity)
  (declare (ignore strategy-expert))
  (with-ensure-available-spells (available-spells
                                 entity
                                 ai-utils:available-heal-spells)
    (ai-utils:exists-entity-receive-heal-spell-p available-spells entity)))

(defun invalidate-tests-cache ()
  (none-needs-help-p-clear-cache)
  (has-weapon-inventory-or-worn-p-clear-cache)
  (has-enough-sp-heal-p-clear-cache)
  (has-enough-sp-damage-p-clear-cache)
  (has-enough-sp-attack-p-clear-cache)
  (has-enough-sp-break-wall-p-clear-cache)
  (has-enough-sp-teleport-p-clear-cache)
  (there-is-attackable-opponents-attack-spell-p-clear-cache)
  (can-rotate-p-clear-cache)
  (can-minimally-move-p-clear-cache)
  (able-to-explore-p-clear-cache)
  (has-wall-near-p-clear-cache)
  (has-trap-p-clear-cache)
  (can-place-trap-p-clear-cache)
  (find-good-places-to-protect-clear-cache)
  (is-near-weak-friend-p-clear-cache)
  (can-attack-when-near-pos-p-clear-cache)
  (enough-mp-to-attack-p-clear-cache)
  (can-launch-heal-spell-current-pos-p-clear-cache)
  (can-launch-damage-spell-current-pos-p-clear-cache)
  (can-launch-attack-spell-current-pos-p-clear-cache)
  (is-there-useful-reachable-fountain-p-clear-cache)
  (exists-reachable-pos-to-launch-damage-spell-clear-cache)
  (exists-reachable-pos-to-launch-heal-spell-clear-cache)
  (exists-reachable-pos-to-launch-attack-spell-clear-cache)
  (exists-attack-goal-w-current-weapon-p-clear-cache)
  (exists-opt-attack-goal-w-current-weapon-p-clear-cache)
  (exists-insecure-attack-goal-w-current-weapon-p-clear-cache)
  (exists-visible-enemy-clear-cache)
  (reachable-opt/path-attack-current-weapon-and-mp-clear-cache)
  (reachable-insecure/path-attack-current-weapon-and-mp-clear-cache)
  (friend-needs-help-p-clear-cache)
  (someone-needs-help-p-clear-cache)
  (there-is-reachable-help-needed-friend-heal-spell-p-clear-cache)
  (is-there-hiding-place-p-clear-cache)
  (is-able-to-flee-p-clear-cache)
  (is-visible-p-clear-cache)
  (reachable-opt/path-current-weapon-and-mp-clear-cache)
  (is-in-attack-pos-p-clear-cache)
  (can-move-near-attack-pos-clear-cache)
  (can-move-near-enemy-pos-clear-cache)
  (can-move-near-enemy-pos-insecure-clear-cache))
