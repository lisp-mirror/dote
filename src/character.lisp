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

(in-package :character)

(define-constant +max-inventory-slots-page+           3.0                  :test #'=)

(define-constant +weight-for-half-capacity-inventory+ 20.0                 :test #'=)

(define-constant +spell-tag-invisible-to-players+     :invisible-to-player :test #'eq)

(define-constant +all-strategies+                     (list +explore-strategy+
                                                            +attack-strategy+
                                                            +defend-strategy+
                                                            +retreat-strategy+)
  :test #'equalp)

(defclass np-character (identificable interactive-entity entity-w-portrait m-tree)
  ((first-name
    :initform ""
    :initarg :first-name
    :accessor first-name
    :type string)
   (last-name
    :initform ""
    :initarg :last-name
    :accessor last-name
    :type string)
   (description
    :initform ""
    :initarg :description
    :accessor description
    :type string)
   (weight
    :initarg :weight
    :initform 35
    :accessor weight
    :type integer)
   (damage-points
    :initarg :damage-points
    :initform 0.0
    :accessor damage-points
    :type integer)
   (current-damage-points
    :initarg :current-damage-points
    :initform 0.0
    :accessor current-damage-points
    :type integer)
   (spell-loaded
    :initarg  :spell-loaded
    :initform nil
    :accessor spell-loaded)
   (level
    :initarg :level
    :initform 1
    :accessor level)
   (age
    :initarg :age
    :initform 0
    :accessor age)))

(defmethod marshal:class-persistant-slots ((object np-character))
  (append  '(first-name
             last-name
             description
             weight
             damage-points
             current-damage-points
             level
             age)
           (call-next-method)))
;;;; interaction

(defmethod print-object ((object np-character) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "dmg: ~a lvl: ~a" (damage-points object) (level object))))

(defmethod description-type ((object np-character))
  (format nil "~a ~a ~a"
          (first-name object)
          (last-name  object)
          +gui-static-text-delim+))

(defmethod description-for-humans :around ((object np-character))
  (strcat
   (format nil (_ "~:[~;Edge weapon~]~:[~;Impact weapon~]~:[~;Range weapon~]~:[~;Range weapon~]~:[~;Fountain~]~:[~;Potion~]~:[~;Elm~]~:[~;Armor~]~:[~;Ring~]~:[~;Shoes~]~:[~;Trap~] ~a ~a~a")
           (can-cut-p   object)
           (can-smash-p object)
           (can-launch-bolt-p object)
           (can-launch-arrow-p object)
           (fountainp  object)
           (potionp    object)
           (elmp       object)
           (armorp     object)
           (ringp      object)
           (shoesp     object)
           (trapp      object)
           (first-name object)
           (last-name  object)
           +gui-static-text-delim+)
   (call-next-method)))

(defgeneric restart-age (object))

(defmethod restart-age ((object np-character))
  (setf (age object) 0.0))

(defun init-postponed-messages-functions (pq)
  (setf (pq:key-function     pq) #'game-event:trigger-turn)
  (setf (pq:compare-function pq) #'<)
  (setf (pq:equal-function   pq) #'=)
  pq)

(defclass planner-character (ai-logger)
  ((current-plan
    :initform nil
    :initarg  :current-plan
    :accessor current-plan
    :documentation "A list with the current plan sorted from the first
                    to accomplish to the last")
   (original-current-plan
    :initform nil
    :initarg  :original-current-plan
    :accessor original-current-plan
    :documentation "A list with the original current plan sorted from the first
                    to accomplish to the last")
   (planners
    :initform '()
    :initarg  :planners
    :accessor planners
    :documentation "An alist of current planners, key is a strategy as in contsnt.lisp (i.e)
                    +explore-strategy+, +attack-strategy+, +defend-strategy+
                    +retreat-strategy+")
   (thinker
    :initform nil
    :initarg  :thinkerp
    :reader   thinkerp
    :writer   (setf thinker)
    :documentation "Should this character 'think'?")
   (planner-working-memory
    :initform '()
    :initarg  :planner-working-memory
    :accessor planner-working-memory
    :documentation "during the plan some data will be saved here")
   (force-idle-plan
    :initform nil
    :initarg  :force-idle-plan
    :reader   force-idle-plan-p
    :writer   (setf force-idle-plan))))

(defun init-logs (player)
  (with-accessors ((logs logs)) player
    (let ((movement-log (make-ai-log :clean-trigger +ai-log-clean-end-turn+)))
      (setf (gethash +ai-log-pos-occupied+ logs) movement-log)))
  player)

(defmethod initialize-instance :after ((object planner-character) &key &allow-other-keys)
  (init-logs object))

(defgeneric set-plan (object plan))

(defgeneric build-planner (object strategy-decision))

(defgeneric build-planners (object))

(defgeneric setup-planners (object))

(defgeneric clear-blacklist (object))

(defgeneric find-plan (object strategy))

(defgeneric elaborate-current-tactical-plan (object strategy-expert player-entity
                                             strategy-decision))

(defgeneric has-interrupt-plan-p (object))

(defgeneric set-interrupt-plan (object))

(defgeneric unset-interrupt-plan (object))

(defgeneric disgregard-tactical-plan (object))

(defgeneric pop-action-plan (object))

(defgeneric erase-working-memory (object))

(defgeneric action-terminal-p (object action))

(defgeneric last-pos-occupied (object))

(defgeneric append-to-last-pos-occupied (object new-pos))

(defgeneric last-pos-occupied-filled-p (object))

(defmacro with-no-thinking ((character) &body body)
  (with-gensyms (saved)
    `(if ,character
         (let ((,saved (thinkerp ,character)))
           (setf (thinker ,character) nil)
           ,@body
           (setf (thinker ,character) ,saved))
         (progn ,@body))))

(defmacro with-one-shot-force-idle ((character-instance) &body body)
  `(with-accessors ((force-idle-plan-p force-idle-plan-p)
                    (current-plan          current-plan)) ,character-instance
     (if force-idle-plan-p
         (progn
           (misc:dbg "forced idle!")
           (set-plan ,character-instance (list ai-utils:+idle-action+))
           (setf (force-idle-plan ,character-instance) nil)
           current-plan)
         (progn ,@body))))

(defmethod set-plan ((object planner-character) plan)
  (with-accessors ((current-plan current-plan)
                   (original-current-plan original-current-plan)) object
    (setf current-plan          plan)
    (setf original-current-plan (copy-list current-plan))))

(defmethod build-planner ((object planner-character) strategy-decision)
  (let* ((planner-file (goap:find-planner-file object strategy-decision)))
    (goap:load-planner-file planner-file object)))

(defmethod build-planners ((object planner-character))
  (loop for strategy in +all-strategies+ collect
       (cons strategy (build-planner object strategy))))

(defmethod setup-planners ((object planner-character))
  (setf (planners object) (build-planners object)))

(defmethod clear-blacklist ((object planner-character))
  (setup-planners object))

(defmethod find-plan ((object planner-character) strategy)
  (cdr (assoc strategy (planners object) :test #'eq)))

(defun %inhibit-current-tactical-plan (character)
  #+debug-mode (misc:dbg "planner is inhibited, switching to idle")
  (set-plan character (list ai-utils:+idle-action+))
  (current-plan character))

(defmethod elaborate-current-tactical-plan ((object planner-character)
                                            strategy-expert
                                            player-entity
                                            (strategy-decision (eql nil)))
  (declare (ignore strategy-decision))
  (if (gconf:config-inhibit-planner)
      (%inhibit-current-tactical-plan object)
      (with-one-shot-force-idle (object)
        (with-accessors ((current-plan current-plan)) object
          (if current-plan
              current-plan
              (elaborate-current-tactical-plan object
                                               strategy-expert
                                               player-entity
                                               (blackboard:strategy-decision strategy-expert)))))))


(defmethod elaborate-current-tactical-plan ((object planner-character)
                                            strategy-expert
                                            player-entity
                                            strategy-decision)
  (with-one-shot-force-idle (object)
    (with-accessors ((blacklisted-plan-goals blacklisted-plan-goals)
                     (planners planners)) object
      (let ((planner (find-plan object strategy-decision)))
        (set-plan object (goap:build-plan planner strategy-expert player-entity))
        #+(and debug-mode debug-ai)
        (misc:dbg "NEW current new plan (~a) ~a" (id player-entity) current-plan)
        (elaborate-current-tactical-plan object strategy-expert player-entity nil)))))

(defmethod set-interrupt-plan ((object planner-character))
  (misc:dbg "set interrupt plan")
  (setf (current-plan object) (list ai-utils:+interrupt-action+)))

(defmethod unset-interrupt-plan ((object planner-character))
  (disgregard-tactical-plan object))

(defmethod has-interrupt-plan-p ((object planner-character))
  (with-accessors ((current-plan current-plan)) object
    (and current-plan
         (eq (elt current-plan 0) ai-utils:+interrupt-action+))))

(defmethod disgregard-tactical-plan ((object planner-character))
  (with-accessors ((current-plan current-plan)) object
    (misc:dbg "disg")
    (setup-planners object)
    (setf current-plan nil)))

(defmethod pop-action-plan ((object planner-character))
  (with-accessors ((current-plan current-plan)) object
    (pop current-plan)))

(defmethod erase-working-memory ((object planner-character))
  (with-accessors ((planner-working-memory planner-working-memory)) object
    (setf planner-working-memory '())))

(defmethod action-terminal-p ((object planner-character) action)
  (with-accessors ((original-current-plan original-current-plan)) object
    (let ((pos (position action original-current-plan :test #'eq)))
      (and pos
           (>= (length original-current-plan)
               2)
           (eq (last-elt original-current-plan)
               ai-utils:+plan-stopper+)
           (= pos
              (- (length original-current-plan) 2))))))

(defmethod last-pos-occupied ((object planner-character))
  (with-accessors ((logs logs)) object
    (ai-log-data (gethash +ai-log-pos-occupied+ logs))))

(defmethod append-to-last-pos-occupied ((object planner-character) new-pos)
  (let ((positions (last-pos-occupied object)))
    (setf (ai-log-data (get-log object +ai-log-pos-occupied+))
          (lcat positions (list new-pos)))
    object))

(defmethod last-pos-occupied-filled-p ((object planner-character))
  (not (null (last-pos-occupied object))))

(defparameter *standard-capital-characteristic* 200)

(defclass player-character (np-character planner-character)
  ((model-origin-dir
    :initform ""
    :initarg  :model-origin-dir
    :accessor model-origin-dir)
   (current-path
    :initform nil
    :initarg  :current-path
    :accessor current-path)
   (gender
    :initarg :gender
    :initform :male
    :accessor gender)
   (player-class
    :initarg :player-class
    :initform nil
    :accessor player-class)
   (strength
    :initarg :strength
    :initform 0
    :accessor strength
    :type integer)
   (stamina
    :initarg :stamina
    :initform 0
    :accessor stamina
    :type integer)
   (dexterity
    :initarg :dexterity
    :initform 0
    :accessor dexterity
    :type integer)
   (agility
    :initarg :agility
    :initform 0
    :accessor agility
    :type integer)
   (smartness
    :initarg :smartness
    :initform 0
    :accessor smartness
    :type integer)
   (empaty
    :initarg :empaty
    :initform 0
    :accessor empaty
    :type integer)
   (movement-points
    :initarg :movement-points
    :initform 0
    :accessor movement-points
    :type integer)
   (current-movement-points
    :initarg :current-movement-points
    :initform 0
    :accessor current-movement-points
    :type integer)
   (spell-points
    :initarg :spell-points
    :initform 0
    :accessor spell-points
    :type integer)
   (current-spell-points
    :initarg :current-spell-points
    :initform 0
    :accessor current-spell-points
    :type integer)
   (dodge-chance
    :initarg :dodge-chance
    :initform 0
    :accessor dodge-chance
    :type integer)
   (melee-attack-chance
    :initarg :melee-attack-chance
    :initform 0
    :accessor melee-attack-chance
    :type integer)
   (range-attack-chance
    :initarg :range-attack-chance
    :initform 0
    :accessor range-attack-chance
    :type integer)
   (melee-attack-damage
    :initarg :melee-attack-damage
    :initform 0
    :accessor melee-attack-damage
    :type integer)
   (range-attack-damage
    :initarg :range-attack-damage
    :initform 0
    :accessor range-attack-damage
    :type integer)
   (edge-weapons-chance-bonus
    :initarg :edge-weapons-chance-bonus
    :initform +unknown-ability-bonus+
    :accessor edge-weapons-chance-bonus
    :type integer)
   (edge-weapons-damage-bonus
    :initarg :edge-weapons-damage-bonus
    :initform +unknown-ability-bonus+
    :accessor edge-weapons-damage-bonus
    :type integer)
   (impact-weapons-chance-bonus
    :initarg :impact-weapons-chance-bonus
    :initform +unknown-ability-bonus+
    :accessor impact-weapons-chance-bonus
    :type integer)
   (impact-weapons-damage-bonus
    :initarg :impact-weapons-damage-bonus
    :initform +unknown-ability-bonus+
    :accessor impact-weapons-damage-bonus
    :type integer)
   (pole-weapons-chance-bonus
    :initarg :pole-weapons-chance-bonus
    :initform +unknown-ability-bonus+
    :accessor pole-weapons-chance-bonus
    :type integer)
   (pole-weapons-damage-bonus
    :initarg :pole-weapons-damage-bonus
    :initform +unknown-ability-bonus+
    :accessor pole-weapons-damage-bonus
    :type integer)
   (modifiers-effects
    :initarg  :modifiers-effects
    :initform '()
    :accessor modifiers-effects)
   (unlock-chance
    :initarg :unlock-chance
    :initform +unknown-ability-bonus+
    :accessor unlock-chance
    :type integer)
   (deactivate-trap-chance
    :initarg :deactivate-trap-chance
    :initform +unknown-ability-bonus+
    :accessor deactivate-trap-chance
    :type integer)
   (reply-attack-chance
    :initarg :reply-attack-chance
    :initform +unknown-ability-bonus+
    :accessor reply-attack-chance
    :type integer)
   (ambush-attack-chance
    :initarg :ambush-attack-chance
    :initform +unknown-ability-bonus+
    :accessor ambush-attack-chance
    :type integer)
   (spell-chance
    :initarg :spell-chance
    :initform 0
    :accessor spell-chance
    :type integer)
   (attack-spell-chance
    :initarg :attack-spell-chance
    :initform 0
    :accessor attack-spell-chance
    :type integer)
   (status
    :initarg  :status
    :initform nil
    :accessor status)
   (immune-faint-status
    :initarg :immune-faint-status
    :initform nil
    :accessor immune-faint-status)
   (immune-berserk-status
    :initarg :immune-berserk-status
    :initform nil
    :accessor immune-berserk-status)
   (immune-poison-status
    :initarg :immune-poison-status
    :initform nil
    :accessor immune-poison-status)
   (immune-terror-status
    :initarg :immune-terror-status
    :initform nil
    :accessor immune-terror-status)
   (recurrent-effects
    :initarg  :recurrent-effects
    :initform nil
    :accessor recurrent-effects)
   (postponed-messages
    :initform (init-postponed-messages-functions (make-instance 'pq:priority-queue))
    :initarg  :postponed-messages
    :accessor postponed-messages)
   (race
    :initarg :race
    :initform :human
    :accessor race)
   (exp-points
    :initarg :exp-points
    :initform 0.0
    :accessor exp-points)
   (elm
    :initarg :elm
    :initform nil
    :accessor elm)
   (shoes
    :initarg :shoes
    :initform nil
    :accessor shoes)
   (armor
    :initarg :armor
    :initform nil
    :accessor armor)
   (left-hand
    :initarg :left-hand
    :initform nil
    :accessor left-hand)
   (right-hand
    :initarg :right-hand
    :initform nil
    :accessor right-hand)
   (ring
    :initarg :ring
    :initform nil
    :accessor ring)
   (inventory
    :initarg :inventory
    :initform '()
    :accessor inventory)))

(gen-type-p player-character)

(defmethod initialize-instance :after ((object player-character) &key &allow-other-keys)
  ;; copy some new points to current
  (setf (current-damage-points   object) (damage-points object))
  (setf (current-movement-points object) (movement-points object))
  (setf (current-spell-points    object) (spell-points object)))

(defmethod print-object ((object player-character) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
            "description ~a name ~a ~a~%class: ~a~%gender: ~a~%strength: ~a ~%stamina: ~a ~%dexterity: ~a ~%agility: ~a ~%smartness: ~a ~%empaty: ~a ~%weight: ~a ~%damage-points: ~a ~%movement-points: ~a ~%spell-points: ~a ~%dodge-chance: ~a ~%melee-attack-chance: ~a ~%range-attack-chance: ~a ~%melee-attack-damage: ~a ~%range-attack-damage: ~a ~%edge-weapons-chance-bonus: ~a ~%edge-weapons-damage-bonus: ~a ~%impact-weapons-chance-bonus: ~a ~%impact-weapons-damage-bonus: ~a ~%pole-weapons-chance-bonus: ~a ~%pole-weapons-damage-bonus: ~a ~%unlock-chance: ~a ~%deactivate-trap-chance: ~a ~%reply-attack-chance: ~a ~%ambush-attack-chance: ~a ~%spell-chance: ~a ~%attack-spell-chance: ~a ~%status: ~a  ~%race: ~a ~%level: ~a ~%exp-points: ~a~%interaction ~a inventory ~a"
            (description                 object)
            (first-name                  object)
            (last-name                   object)
            (player-class                object)
            (gender                      object)
            (strength                    object)
            (stamina                     object)
            (dexterity                   object)
            (agility                     object)
            (smartness                   object)
            (empaty                      object)
            (weight                      object)
            (damage-points               object)
            (movement-points             object)
            (spell-points                object)
            (dodge-chance                object)
            (melee-attack-chance         object)
            (range-attack-chance         object)
            (melee-attack-damage         object)
            (range-attack-damage         object)
            (edge-weapons-chance-bonus   object)
            (edge-weapons-damage-bonus   object)
            (impact-weapons-chance-bonus object)
            (impact-weapons-damage-bonus object)
            (pole-weapons-chance-bonus   object)
            (pole-weapons-damage-bonus   object)
            (unlock-chance               object)
            (deactivate-trap-chance      object)
            (reply-attack-chance         object)
            (ambush-attack-chance        object)
            (spell-chance                object)
            (attack-spell-chance         object)
            (status                      object)
            (race                        object)
            (level                       object)
            (exp-points                  object)
            (basic-interaction-params    object)
            (mapcar #'description-for-humans (inventory object)))))

(defmethod marshal:class-persistant-slots ((object player-character))
  (append '(model-origin-dir
            current-path
            gender
            player-class
            strength
            stamina
            dexterity
            agility
            smartness
            empaty
            weight
            movement-points
            current-movement-points
            spell-points
            current-spell-points
            dodge-chance
            melee-attack-chance
            range-attack-chance
            melee-attack-damage
            range-attack-damage
            edge-weapons-chance-bonus
            edge-weapons-damage-bonus
            impact-weapons-chance-bonus
            impact-weapons-damage-bonus
            pole-weapons-chance-bonus
            pole-weapons-damage-bonus
            modifiers-effects
            unlock-chance
            deactivate-trap-chance
            reply-attack-chance
            ambush-attack-chance
            spell-chance
            attack-spell-chance
            status
            immune-faint-status
            immune-berserk-status
            immune-poison-status
            immune-terror-status
            recurrent-effects
            race
            exp-points
            elm
            shoes
            armor
            left-hand
            right-hand
            ring
            inventory)
          (call-next-method)))

(defmethod serialize ((object player-character))
  (when (portrait object)
    (setf (texture:filename (portrait object))
          (format nil "~a" (num:fnv-hash-32 (pixmap:bits (portrait object))))))
  (format nil "~s" (marshal:marshal object)))

(defmethod deserialize ((object player-character) file)
  (declare (ignore object))
  (let ((res (marshal:unmarshal (read-from-string (filesystem-utils:slurp-file file)))))
    ;; (when (portrait res)
    ;;   (texture:gen-name-and-inject-in-database (portrait res)))
    (init-postponed-messages-functions (postponed-messages res))
    res))

;; events

(defmethod game-event:on-game-event :after ((object player-character) (event game-event:end-turn))
  ;;(misc:dbg " end turn character ~a(~a) ~a" (type-of object) (id object) (type-of event))
  (remove-decayed-items object (game-event:end-turn-count event))
  nil)

(defgeneric random-fill-slots (object capital characteristics))

(defgeneric inventory-slot-pages-number (object))

(defgeneric player-class->class-description (object))

(defgeneric player-gender->gender-description (object))

(defgeneric reset-movement-points (object))

(defgeneric reset-spell-points (object))

(defgeneric add-to-inventory (object item))

(defgeneric find-item-in-inventory (object item))

(defgeneric find-item-in-inventory-if (object predicate))

(defgeneric remove-decayed-items (object turn-count))

(defgeneric remove-from-inventory (object item))

(defgeneric remove-from-modifiers (object item-origin-id))

(defgeneric sum-modifiers (object modifier-name))

(defgeneric item->player-character-slot (object item))

(defgeneric item->available-player-character-slot (object item))

(defgeneric worn-weapon (object))

(defgeneric weapon-type (object))

(defgeneric weapon-type-short-range (object))

(defgeneric weapon-type-long-range (object))

(defgeneric weapon-type-minimum-range-p (object))

(defgeneric available-spells-list (object))

(defgeneric available-spells-list-by-tag (object tag))

(defgeneric castable-spells-list-by-tag (object tag))

(defgeneric castable-attack-spells-list (object))

(defgeneric calculate-influence (object))

(defgeneric calculate-influence-weapon (object weapon-type))

(defgeneric combined-power (object))

(defgeneric movement-stuck-p (object strategy-expert current-pos new-pos))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-pclass-fn-name (name)
    (format-symbol t "~:@(pclass-~a-p~)" name)))

(defmacro gen-player-class-test (name)
  (let ((name-fn (symbol-pclass-fn-name name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object player-character))
         (eq (player-class object) ,(make-keyword name))))))

(gen-player-class-test ranger)

(gen-player-class-test warrior)

(gen-player-class-test archer)

(gen-player-class-test wizard)

(gen-player-class-test healer)

(defmacro gen-player-class-in-set-test (name &rest classes)
  (let ((name-fn (format-symbol t "~:@(pclass-of-~a-p~)" name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object player-character))
         (or ,@(loop for c in classes collect
                    `(,(symbol-pclass-fn-name c) object)))))))

(gen-player-class-in-set-test magic-user wizard healer)

(gen-player-class-in-set-test useful-in-attack-tactic ranger warrior archer)

(gen-player-class-in-set-test no-damage-spells warrior archer)

(gen-player-class-in-set-test no-attack-spells warrior archer ranger)

(defmacro gen-player-status-test (status)
  (let ((name-fn (format-fn-symbol t "status-~a-p" (symbol-name status))))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object player-character))
         (eq (character:status object)
             ,(format-fn-symbol :interactive-entity "+status-~a+" status))))))

(gen-player-status-test poisoned)

(gen-player-status-test terror)

(gen-player-status-test berserk)

(gen-player-status-test faint)

(defmacro gen-with-no-status (status)
  (let ((name-macro    (format-fn-symbol t "with-no-~a-status" (symbol-name status)))
        (name-accessor (format-fn-symbol t "status-~a-p"       (symbol-name status))))
    `(defmacro ,name-macro ((character) &body body)
       (let ((acc ',name-accessor))
         `(and (not (,acc (entity:ghost ,character)))
               ,@body)))))

(gen-with-no-status terror)

(gen-with-no-status berserk)

(gen-with-no-status faint)

(defmethod random-fill-slots ((object player-character) capital characteristics)
  (loop for charact in characteristics do
       (when (> capital 0)
         (let ((random-value (+ (caadr charact)
                                (if (/= 0 (cadadr charact))
                                    (num:lcg-next-in-range
                                     (- (cadadr charact)) (cadadr charact))
                                    0))))
           (if (< random-value capital)
               (progn
                 (incf (slot-value object (car charact)) random-value)
                 (decf capital random-value))
               (progn
                 (incf (slot-value object (car charact)) capital)
                 (setf capital 0))))))
  capital)

(defmethod inventory-slot-pages-number ((object player-character))
  (ceiling (enzyme-kinetics +max-inventory-slots-page+
                            +weight-for-half-capacity-inventory+
                            (d (weight object)))))

(defmethod player-class->class-description ((object player-character))
  (with-accessors ((player-class player-class)) object
    (ecase player-class
      (:warrior
       (_ "warrior"))
      (:archer
       (_ "archer"))
      (:wizard
       (_ "wizard"))
      (:healer
       (_ "healer"))
      (:ranger
       (_ "ranger")))))

(defmethod player-gender->gender-description ((object player-character))
  (with-accessors ((gender gender)) object
    (ecase gender
      (:male
       (_ "male"))
      (:female
       (_ "female")))))

(defmethod reset-movement-points ((object player-character))
  (setf (current-movement-points object) (actual-movement-points object)))

(defmethod reset-spell-points ((object player-character))
  (setf (current-spell-points object) (spell-points object)))

(defmethod add-to-inventory ((object player-character) item)
  (game-event:register-for-end-turn item)
  (push item (character:inventory object)))

(defmethod find-entity-by-id ((object player-character) id)
  "Search everywhere, in inventory!"
  (with-accessors ((elm        elm)
                   (shoes      shoes)
                   (armor      armor)
                   (left-hand  left-hand)
                   (right-hand right-hand)
                   (ring       ring)) object
    (or (find-item-in-inventory object id)
        (and elm        (= (id elm)        id) elm)
        (and shoes      (= (id shoes)      id) shoes)
        (and armor      (= (id armor)      id) armor)
        (and left-hand  (= (id left-hand)  id) left-hand)
        (and right-hand (= (id right-hand) id) right-hand)
        (and ring       (= (id ring)       id) ring))))

(defmethod find-item-in-inventory ((object player-character) id)
  (find id (inventory object) :key #'id :test #'=))

(defmethod find-item-in-inventory-if ((object player-character) predicate)
  (find-if predicate (inventory object)))

(defmethod remove-decayed-items ((object player-character) turn-count)
  (with-accessors ((inventory inventory)
                   (elm        elm)
                   (shoes      shoes)
                   (armor      armor)
                   (left-hand  left-hand)
                   (right-hand right-hand)
                   (ring       ring)) object
    (flet ((decayedp (item)
             (let* ((decay-params (interaction-get-decay item))
                    (decay-point  (and decay-params (interaction:points decay-params))))
               (and decay-point
                    (>= (age item) decay-point)))))
      (let* ((new-inventory (remove-if #'(lambda (a) (decayedp a)) inventory))
             (removed-items (remove-if #'(lambda (a) (not (decayedp a))) inventory)))
        (when (and elm
                   (decayedp elm))
          (push elm removed-items)
          (setf elm nil))
        (when (and shoes
                   (decayedp shoes))
          (push shoes removed-items)
          (setf shoes nil))
        (when (and armor
                   (decayedp armor))
          (push armor removed-items)
          (setf armor nil))
        (when (and left-hand
                   (decayedp left-hand))
          (push left-hand removed-items)
          (setf left-hand nil))
        (when (and right-hand
                   (decayedp right-hand))
          (push right-hand removed-items)
          (setf right-hand nil))
        (when (and ring
                   (decayedp ring))
          (push ring removed-items)
          (setf ring nil))
        (setf inventory new-inventory)
        (values removed-items inventory)))))

(defmethod remove-from-inventory ((object player-character) item)
  (setf (inventory object) (remove (id item) (inventory object) :key #'id :test #'=)))

(defmethod remove-from-modifiers ((object player-character) item-origin-id)
  (with-accessors ((modifiers-effects modifiers-effects)) object
    (setf modifiers-effects
          (remove-if #'(lambda (a)
                         (= (random-object-messages:msg-origin a)
                            item-origin-id))
                     modifiers-effects))))

(defmethod sum-modifiers ((object player-character) modifier-name)
  (with-accessors ((modifiers-effects modifiers-effects)) object
    (reduce #'(lambda (a b) (d+ a (random-object-messages:msg-modifier b)))
            (remove-if #'(lambda (a)
                           (or (not (typep a 'random-object-messages:modifier-effect-msg))
                               (not (eq    modifier-name
                                           (random-object-messages:msg-characteristic a)))))
                       modifiers-effects)
            :initial-value 0.0)))

(defmethod item->player-character-slot ((object player-character) item)
  (with-accessors ((left-hand  left-hand)
                   (right-hand right-hand)) object
    (when item
      (let ((item-id (id item)))
        (cond
          ((and (interactive-entity:ringp item)
                (ring object)
                (= item-id (id (ring object))))
           'character:ring)
          ((and (interactive-entity:armorp item)
                (armor object)
                (= item-id (id (armor object))))
           'character:armor)
          ((and (interactive-entity:elmp item)
                (elm object)
                (= item-id (id (elm object))))
           'character:elm)
          ((and (interactive-entity:shoesp item)
                (shoes object)
                (= item-id (id (shoes object))))
           'character:shoes)
          ((or (interactive-entity:weaponp item)
               (interactive-entity:shieldp item))
           (cond
             ((and (left-hand object)
                   (= item-id (id (left-hand object))))
              'character:left-hand)
             ((and (right-hand object)
                   (= item-id (id (right-hand object))))
              'character:right-hand))))))))

(defmethod item->available-player-character-slot ((object player-character) item)
  (with-accessors ((left-hand  left-hand)
                   (right-hand right-hand)) object
    (and item
         (cond
           ((interactive-entity:ringp item)
            'character:ring)
           ((interactive-entity:armorp item)
            'character:armor)
           ((interactive-entity:elmp item)
            'character:elm)
           ((interactive-entity:shoesp item)
            'character:shoes)
           ((or (interactive-entity:weaponp item)
                (interactive-entity:shieldp item))
            (if (null left-hand)
                'character:left-hand
                (if (null right-hand)
                    'character:right-hand
                    nil)))))))

(defmethod worn-weapon ((object player-character))
  (cond
    ((weaponp (left-hand object))
     (left-hand object))
    ((weaponp (right-hand object))
     (right-hand object))
    (t
     nil)))

(defmethod weapon-type ((object player-character))
  (or (weapon-type-long-range  object)
      (weapon-type-short-range object)))

(defmethod weapon-type (object)
  nil)

(defmethod weapon-type-short-range ((object player-character))
  (let* ((weapon       (worn-weapon object))
         (weapon-type  (when weapon
                         (cond
                           ((can-cut-p weapon)
                            :edge)
                           ((can-smash-p weapon)
                            :impact)
                           ((mounted-on-pole-p weapon)
                            :pole)
                           (t
                            nil)))))
    weapon-type))

(defmethod weapon-type-long-range ((object player-character))
  (let* ((weapon       (worn-weapon object))
         (weapon-type  (when weapon
                         (cond
                           ((bowp weapon)
                            :bow)
                           ((crossbowp weapon)
                            :crossbow)
                           (t
                            nil)))))
    weapon-type))

(defmethod weapon-type-minimum-range-p ((object player-character))
  (or (weapon-type-edge-p    object)
      (weapon-type-impact-p  object)))

(defmacro gen-wear-weapon-of-type (&rest types)
  (with-gensyms (character weapon-type)
    `(progn
       ,@(loop for i in types collect
              (let ((name (misc:format-fn-symbol t "weapon-type-~a-p" i)))
                `(defun ,name (,character)
                   (let* ((,weapon-type (weapon-type ,character)))
                     (and ,weapon-type (eq ,weapon-type ,i)))))))))

(gen-wear-weapon-of-type :edge :impact :pole :bow :crossbow)

(defun %filter-spell-cost-clsr (character)
  #'(lambda (a) (> (spell:cost a) (current-spell-points character))))

(defun remove-from-spell-list-by-tag (spell-list tag)
  (spell:filter-spell-set spell-list
                          #'(lambda (a) (find tag (spell:tags a)))))

(defmethod available-spells-list ((object player-character))
  ;;; TEST ;;;;
  ;; (return-from available-spells-list spell::*spells-db*)
  (let ((affordables (spell:filter-spell-db (%filter-spell-cost-clsr object))))
    (when (pclass-of-no-attack-spells-p object)
      (setf affordables (remove-if #'(lambda (a) (spell:attack-spell-p a)) affordables)))
    (when (pclass-of-no-damage-spells-p object)
      (setf affordables (remove-if #'(lambda (a) (find spell:+spell-tag-damage+ (spell:tags a)))
                                   affordables)))
    ;; remove if invisible for player (e.g. if this spell is meant for fountains only)
    (setf affordables (remove-from-spell-list-by-tag affordables
                                                     +spell-tag-invisible-to-players+))
    affordables))

(defmethod available-spells-list-by-tag ((object player-character) tag)
  (spell:filter-spell-set (available-spells-list object)
                          #'(lambda (a) (not (find tag (spell:tags a))))))

(defmethod castable-spells-list-by-tag ((object player-character) tag)
  (available-spells-list-by-tag object tag))

(defmethod castable-attack-spells-list ((object player-character))
  (spell:filter-spell-set (available-spells-list object)
                          #'(lambda (a) (not (spell:attack-spell-p a)))))

(defmethod calculate-influence ((object player-character))
  (d+ (calculate-influence-weapon object (weapon-type object))
      (combined-power object)))

(defmethod calculate-influence-weapon ((object player-character) (weapon-type (eql nil)))
  0.0)

(defmethod calculate-influence-weapon ((object player-character) (weapon-type (eql :bow)))
  (d +weapon-bow-range+))

(defmethod calculate-influence-weapon ((object player-character) (weapon-type (eql :crossbow)))
  (d +weapon-crossbow-range+))

(defmethod calculate-influence-weapon ((object player-character) (weapon-type (eql :edge)))
  (d +weapon-melee-range+))

(defmethod calculate-influence-weapon ((object player-character) (weapon-type (eql :impact)))
  (d +weapon-melee-range+))

(defmethod calculate-influence-weapon ((object player-character) (weapon-type (eql :pole)))
  (d +weapon-pole-range+))

(defmacro %with-gen-simple-weight (name-weigth &body body)
  `(flet
       ,(loop for (name . weight) in name-weigth collect
             `(,(misc:format-fn-symbol t "~a-weight" name) ()
                ,weight))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-power-weights (&key
                               (movement 0.3)
                               (damage   0.9)
                               (melee    1.0)
                               (range    2.0)
                               (magic    1.0)
                               (dodge    0.2))
    (list (cons :movement movement)
          (cons :damage   damage)
          (cons :melee    melee)
          (cons :range    range)
          (cons :magic    magic)
          (cons :dodge    dodge)))

  (defparameter *power-weights* (make-power-weights)))

(defmethod combined-power ((object player-character))
  (labels ((melee-weapon-switch (character)
             (if (weapon-type-short-range character)
                 1.0
                 0.0))
           (range-weapon-switch (character)
             (if (weapon-type-long-range character)
                 1.0
                 0.0))
           (weapon-type-switch (weapon-type desidered-weapon-type)
             (if (eq weapon-type desidered-weapon-type)
                 1.0
                 0.0))
           (weapon-flip (a weapon-type desired-weapon-type)
             (d* (weapon-type-switch weapon-type desired-weapon-type) ; on or off
                 a))
           (to-0-1 (v)
             (d (/ v 100))))
    (%with-gen-simple-weight ((movement . (cdr (assoc :movement *power-weights*)))
                              (damage   . (cdr (assoc :damage   *power-weights*)))
                              (melee    . (cdr (assoc :melee    *power-weights*)))
                              (range    . (cdr (assoc :range    *power-weights*)))
                              (magic    . (cdr (assoc :magic    *power-weights*)))
                              (dodge    . (cdr (assoc :dodge    *power-weights*))))
      (let* ((weapon-type                        (weapon-type object))
             (damage-points               (current-damage-points               object))
             (movement-points             (current-movement-points             object))
             (spell-points                (current-spell-points                object))
             (dodge-chance                (to-0-1 (actual-dodge-chance        object)))
             (melee-attack-chance         (to-0-1 (actual-melee-attack-chance object)))
             (melee-attack-damage         (actual-melee-attack-damage         object))
             (range-attack-chance         (to-0-1 (actual-range-attack-chance object)))
             (range-attack-damage         (actual-range-attack-damage         object))
             (edge-weapons-chance-bonus   (to-0-1 (actual-edge-weapons-chance-bonus object)))
             (edge-weapons-damage-bonus   (actual-edge-weapons-damage-bonus   object))
             (impact-weapons-chance-bonus (to-0-1 (actual-impact-weapons-chance-bonus object)))
             (impact-weapons-damage-bonus (actual-impact-weapons-damage-bonus object))
             (pole-weapons-chance-bonus   (to-0-1 (actual-pole-weapons-chance-bonus   object)))
             (pole-weapons-damage-bonus   (actual-pole-weapons-damage-bonus   object))
             (spell-change                (to-0-1 (actual-spell-chance                object)))
             (attack-spell-chance         (to-0-1 (actual-attack-spell-chance         object)))
             (status-contribute           (if (or (status-terror-p object)
                                                  (status-faint-p  object))
                                              0.0
                                              1.0))
             (melee-contribute (d* (melee-weapon-switch object) ; on or off
                                   (d+ (d* (d+ melee-attack-damage
                                               (weapon-flip edge-weapons-damage-bonus
                                                            weapon-type :edge)
                                               (weapon-flip impact-weapons-damage-bonus
                                                            weapon-type :impact)
                                               (weapon-flip pole-weapons-damage-bonus
                                                            weapon-type :pole))
                                           (d+ melee-attack-chance
                                               (weapon-flip edge-weapons-chance-bonus
                                                            weapon-type :edge)
                                               (weapon-flip impact-weapons-chance-bonus
                                                            weapon-type :impact)
                                               (weapon-flip pole-weapons-chance-bonus
                                                            weapon-type :pole))))))
             (range-contribute (d* (range-weapon-switch object) ; on or off
                                   range-attack-damage
                                   range-attack-chance))
             (raw-power        (d+ (d* (damage-weight)   damage-points)
                                   (d* (movement-weight) movement-points)
                                   (d* (melee-weight)    melee-contribute)
                                   (d* (range-weight)    range-contribute)
                                   (d* (magic-weight)
                                       spell-points
                                       (dmax spell-change
                                             attack-spell-chance))
                                   (d* (dodge-weight)
                                       dodge-chance))))
        (d* status-contribute raw-power)))))

(define-constant +stuck-threshold+ 10 :test #'=)

(defmethod movement-stuck-p ((object player-character) strategy-expert current-pos next-pos)
  (with-accessors ((unexplored-layer blackboard:unexplored-layer)) strategy-expert
    (with-accessors ((last-pos-occupied last-pos-occupied)) object
      (let* ((matrix-unexplored (inmap:layer unexplored-layer))
             (length-pos        (length last-pos-occupied))
             (pos-eql           #'(lambda (a b)
                                    (ivec2:ivec2= a b)))
             (test-null-fn      #'(lambda (mat x y)
                                    (or (funcall pos-eql (ivec2:ivec2 x y) current-pos)
                                        (not (null (matrix:matrix-elt mat y x))))))
             (approchables      (matrix:flood-fill* matrix-unexplored
                                                    (ivec2:ivec2-x current-pos)
                                                    (ivec2:ivec2-y current-pos)
                                                    :position-acceptable-p test-null-fn
                                                    :max-iteration         (1+ +stuck-threshold+))))
        (if (< (length approchables) +stuck-threshold+)
            t
            (if (< length-pos 3)
                nil
                (let ((a1 (elt last-pos-occupied (- length-pos 1)))
                      (b  (elt last-pos-occupied (- length-pos 2)))
                      (a2 (elt last-pos-occupied (- length-pos 3))))
                  (and (ivec2:ivec2= a1 a2)
                       (ivec2:ivec2= b next-pos)))))))))

(defmacro gen-actual-characteristic (name)
  (let ((fn       (format-fn-symbol t "actual-~a" name))
        (accessor (format-fn-symbol t "~a"        name))
        (constant (format-fn-symbol t "+~a+"      name)))
    `(progn
       (defgeneric ,fn (object))
       (defmethod  ,fn ((object player-character))
         (d+ (,accessor object)
             (sum-modifiers object ,constant))))))

(defmacro gen-actual-characteristics (&rest names)
  `(progn
     ,@(loop for name in names collect
            `(gen-actual-characteristic ,name))))

;; "actual" means  the actual (no  pun intended) *maximum* value  of the
;; characteristic
(gen-actual-characteristics damage-points
                            movement-points
                            spell-points
                            dodge-chance
                            melee-attack-chance
                            range-attack-chance
                            melee-attack-damage
                            range-attack-damage
                            edge-weapons-chance-bonus
                            edge-weapons-damage-bonus
                            impact-weapons-chance-bonus
                            impact-weapons-damage-bonus
                            pole-weapons-chance-bonus
                            pole-weapons-damage-bonus
                            unlock-chance
                            deactivate-trap-chance
                            reply-attack-chance
                            ambush-attack-chance
                            spell-chance
                            attack-spell-chance)

(defmacro gen-make-player (player-class)
  (with-gensyms (char rest-capital)
    `(defun ,(format-symbol t "~@:(make-~a~)" player-class)
         (capital race
          &optional (charact nil) (slots '((strength (50 10))
                                           (stamina  (40 10))
                                           (dexterity (5 3))
                                           (agility   (5 3))
                                           (empaty    (2 0))
                                           (smartness (2 0))
                                           (weight    (25 10)))))
       (let* ((,char (or charact (make-instance 'player-character)))
              (,rest-capital (random-fill-slots ,char capital slots)))
         (setf (damage-points ,char)   (d (truncate (* (stamina ,char) 0.5)))
               (movement-points ,char) (d (truncate (* (agility ,char) 0.5)))
               (spell-points ,char) (d (truncate (/ (lerp 0.1
                                                          (lerp 0.5
                                                                (smartness ,char)
                                                                (empaty ,char))
                                                          (dexterity ,char))
                                                    3)))
               (dodge-chance ,char) (d (truncate (max 0 (- (agility ,char) (/ (weight ,char) 2)))))
               (melee-attack-chance ,char) (lerp 0.3 (strength ,char) (agility ,char))
               (range-attack-chance ,char) (lerp 0.1
                                                 (dexterity ,char)
                                                 (agility   ,char))
               (melee-attack-damage ,char) (d (truncate (* (strength ,char) 0.25)))
               (range-attack-damage ,char) (d (truncate (* (dexterity ,char) 0.25)))
               (unlock-chance ,char) (d (truncate (/ (lerp 0.9
                                                           (agility ,char)
                                                           (dexterity ,char))

                                                     10)))
               (deactivate-trap-chance ,char) (d (truncate
                                                  (/ (lerp 0.8
                                                           (agility ,char)
                                                           (dexterity ,char))
                                                     10)))
               (reply-attack-chance ,char)  (d (truncate (/ (agility ,char) 5)))
               (ambush-attack-chance ,char) (d (truncate
                                                (/ (lerp 0.9
                                                         (agility ,char)
                                                         (strength ,char))
                                                   10)))
               (spell-chance ,char) (d (truncate (/ (lerp 0.9
                                                          (smartness ,char)
                                                          (empaty ,char))
                                                    3)))
               (attack-spell-chance ,char) (d (truncate (/ (lerp 0.9
                                                                 (agility ,char)
                                                                 (smartness ,char))
                                                           3)))
               (race ,char) race)
         (if (> ,rest-capital 0)
             (,(format-symbol t "~@:(make-~a~)" player-class)
               ,rest-capital race ,char slots)
             (progn
               (setf (current-spell-points    ,char) (spell-points ,char)
                     (current-movement-points ,char) (movement-points ,char)
                     (current-damage-points    ,char) (damage-points ,char))
               (values ,char ,rest-capital)))))))

(gen-make-player player)

(defun make-warrior (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((strength (50 10))
                                                                          (stamina  (40 10))
                                                                          (dexterity (5 3))
                                                                          (agility   (10 3))
                                                                          (empaty    (2 0))
                                                                          (smartness (2 0))
                                                                          (weight    (52 23))))))
    (setf (player-class player) :warrior)
    player))

(defun make-wizard (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((smartness (40 10))
                                                                          (agility   (30 10))
                                                                          (stamina  (20 10))
                                                                          (strength (10 10))
                                                                          (dexterity (5 3))
                                                                          (empaty    (2 0))
                                                                          (weight    (30 5))))))
    (setf (player-class player) :wizard)
    player))

(defun make-healer (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((empaty (40 10))
                                                                          (smartness (20 20))
                                                                          (agility   (15 10))
                                                                          (stamina  (10 10))
                                                                          (strength (10 10))
                                                                          (dexterity (5 3))
                                                                          (weight    (42 12))))))
    (setf (player-class player) :healer)
    player))

(defun make-archer (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((dexterity (50 0))
                                                                          (stamina  (40 10))
                                                                          (strength (10 10))
                                                                          (empaty    (1 1))
                                                                          (agility   (10 3))
                                                                          (smartness (5 3))
                                                                          (weight    (30 55))))))
    (setf (player-class player) :archer)
    player))

(defun make-ranger (race)
  (let ((player (make-player *standard-capital-characteristic* race nil '((agility (60 0))
                                                                          (dexterity  (40 10))
                                                                          (stamina (15 10))
                                                                          (strength (15 10))
                                                                          (empaty    (10 10))
                                                                          (smartness (5 3))
                                                                          (weight    (56 20))))))
    (incf (deactivate-trap-chance player) 3)
    (setf (player-class player) :ranger)
    player))

;; character definition

(defparameter *character-parameters* nil)

(defmacro define-character (&rest parameters)
  (let ((params (build-plist parameters)))
    `(setf *character-parameters*
           (list ,@(loop for i in params collect
                        `(cons ,(car i) ,(if (consp (cdr i))
                                             (cadr i)
                                             (cdr i))))))))

(defmacro with-character-parameters ((params file) &body body)
  `(let* ((*character-parameters* nil))
     (load ,file)
     (let ((,params *character-parameters*))
       ,@body)))

(gen-trivial-plist-gets t
                        (lambda (l k) (cdr (assoc k l)))
                        fetch
                        +description+
                        +first-name+
                        +last-name+
                        +portrait+
                        +strength+
                        +stamina+
                        +dexterity+
                        +agility+
                        +smartness+
                        +empaty+
                        +weight+
                        +damage-points+
                        +movement-points+
                        +spell-points+
                        +dodge-chance+
                        +melee-attack-chance+
                        +range-attack-chance+
                        +melee-attack-damage+
                        +range-attack-damage+
                        +edge-weapons-chance-bonus+
                        +edge-weapons-damage-bonus+
                        +impact-weapons-chance-bonus+
                        +impact-weapons-damage-bonus+
                        +pole-weapons-chance-bonus+
                        +pole-weapons-damage-bonus+
                        +unlock-chance+
                        +deactivate-trap-chance+
                        +reply-attack-chance+
                        +ambush-attack-chance+
                        +spell-chance+
                        +attack-spell-chance+
                        +status+
                        +race+
                        +level+
                        +exp-points+)

(defun randomize-a-bit (val &key (displacement 0.1))
  (if (numberp val)
      (floor (num:gaussian-probability (* displacement val) val))
      val))

(defun calculate-randomized-damage-points (level
                                           min-level max-level
                                           min-damage max-damage
                                           displacement)
  (let ((val (num:dlerp (num:smoothstep-interpolate (d min-level) (d max-level) (d level))
                        min-damage
                        max-damage)))
    (num:gaussian-probability (d* displacement val) val)))

(defun params->np-character (params)
  (let ((results (make-instance 'np-character
                                :description   (fetch-description                   params)
                                :first-name    (fetch-first-name                    params)
                                :last-name     (fetch-last-name                     params)
                                :portrait      (and (fetch-portrait params)
                                                    (texture:get-texture (fetch-portrait params)))
                                :weight        (fetch-weight                        params)
                                :damage-points (fetch-damage-points                 params)
                                :level         (fetch-level                         params)
                                :age           0)))
    (when (portrait results)
      (setf (texture:s-wrap-mode (portrait results)) :clamp-to-border)
      (setf (texture:t-wrap-mode (portrait results)) :clamp-to-border)
      (setf (texture:border-color (portrait results)) §c00000000)
      (texture:prepare-for-rendering (portrait results)))
    results))

(defun params->player-character (params)
  (let ((results (make-instance
                  'player-character
                  :description                 (fetch-description                   params)
                  :first-name                  (fetch-first-name                    params)
                  :last-name                   (fetch-last-name                     params)
                  :portrait                    (and (fetch-portrait params)
                                                    (texture:get-texture (fetch-portrait params)))
                  :strength                    (fetch-strength                      params)
                  :stamina                     (fetch-stamina                       params)
                  :dexterity                   (fetch-dexterity                     params)
                  :agility                     (fetch-agility                       params)
                  :smartness                   (fetch-smartness                     params)
                  :empaty                      (fetch-empaty                        params)
                  :weight                      (fetch-weight                        params)
                  :damage-points               (fetch-damage-points                 params)
                  :movement-points             (fetch-movement-points               params)
                  :spell-points                (fetch-spell-points                  params)
                  :dodge-chance                (fetch-dodge-chance                  params)
                  :melee-attack-chance         (fetch-melee-attack-chance           params)
                  :range-attack-chance         (fetch-range-attack-chance           params)
                  :melee-attack-damage         (fetch-melee-attack-damage           params)
                  :range-attack-damage         (fetch-range-attack-damage           params)
                  :edge-weapons-chance-bonus   (fetch-edge-weapons-chance-bonus     params)
                  :edge-weapons-damage-bonus   (fetch-edge-weapons-damage-bonus     params)
                  :impact-weapons-chance-bonus (fetch-impact-weapons-chance-bonus   params)
                  :impact-weapons-damage-bonus (fetch-impact-weapons-damage-bonus   params)
                  :pole-weapons-chance-bonus   (fetch-pole-weapons-chance-bonus     params)
                  :pole-weapons-damage-bonus   (fetch-pole-weapons-damage-bonus     params)
                  :unlock-chance               (fetch-unlock-chance                 params)
                  :deactivate-trap-chance      (fetch-deactivate-trap-chance        params)
                  :reply-attack-chance         (fetch-reply-attack-chance           params)
                  :ambush-attack-chance        (fetch-ambush-attack-chance          params)
                  :spell-chance                (fetch-spell-chance                  params)
                  :attack-spell-chance         (fetch-attack-spell-chance           params)
                  :status                      (fetch-status                        params)
                  :race                        (fetch-race                          params)
                  :level                       (fetch-level                         params)
                  :age                         0
                  :exp-points                  (fetch-exp-points                    params))))
    (when (portrait results)
      (setf (texture:s-wrap-mode (portrait results)) :clamp-to-border)
      (setf (texture:t-wrap-mode (portrait results)) :clamp-to-border)
      (setf (texture:border-color (portrait results)) §c00000000)
      (texture:prepare-for-rendering (portrait results)))
    results))

(defun load-randomize-character (file)
  (with-character-parameters (params file)
    (let ((results (make-instance 'player-character
                                  :first-name                  (randomize-a-bit
                                                                (fetch-first-name params))
                                  :last-name                   (randomize-a-bit
                                                                (fetch-last-name params))
                                  :portrait                    (texture:get-texture
                                                                (fetch-portrait params))
                                  :strength                    (randomize-a-bit
                                                                (fetch-strength params))
                                  :stamina                     (randomize-a-bit
                                                                (fetch-stamina params))
                                  :dexterity                   (randomize-a-bit
                                                                (fetch-dexterity params))
                                  :agility                     (randomize-a-bit
                                                                (fetch-agility params))
                                  :smartness                   (randomize-a-bit
                                                                (fetch-smartness params))
                                  :empaty                      (randomize-a-bit
                                                                (fetch-empaty params))
                                  :weight                      (randomize-a-bit
                                                                (fetch-weight params))
                                  :damage-points               (randomize-a-bit
                                                                (fetch-damage-points params))
                                  :movement-points             (randomize-a-bit
                                                                (fetch-movement-points params))
                                  :spell-points                (randomize-a-bit
                                                                (fetch-spell-points params))
                                  :dodge-chance                (randomize-a-bit
                                                                (fetch-dodge-chance params))
                                  :melee-attack-chance         (randomize-a-bit
                                                                (fetch-melee-attack-chance params))
                                  :range-attack-chance         (randomize-a-bit
                                                                (fetch-range-attack-chance params))
                                  :melee-attack-damage         (randomize-a-bit
                                                                (fetch-melee-attack-damage params))
                                  :range-attack-damage         (randomize-a-bit
                                                                (fetch-range-attack-damage params))
                                  :edge-weapons-chance-bonus   (randomize-a-bit
                                                                (fetch-edge-weapons-chance-bonus
                                                                 params))
                                  :edge-weapons-damage-bonus   (randomize-a-bit
                                                                (fetch-edge-weapons-damage-bonus
                                                                 params))
                                  :impact-weapons-chance-bonus (randomize-a-bit
                                                                (fetch-impact-weapons-chance-bonus
                                                                 params))
                                  :impact-weapons-damage-bonus (randomize-a-bit
                                                                (fetch-impact-weapons-damage-bonus
                                                                 params))
                                  :pole-weapons-chance-bonus   (randomize-a-bit
                                                                (fetch-pole-weapons-chance-bonus
                                                                 params))
                                  :pole-weapons-damage-bonus   (randomize-a-bit
                                                                (fetch-pole-weapons-damage-bonus
                                                                 params))
                                  :unlock-chance               (randomize-a-bit
                                                                (fetch-unlock-chance params))
                                  :deactivate-trap-chance      (randomize-a-bit
                                                                (fetch-deactivate-trap-chance
                                                                 params))
                                  :reply-attack-chance         (randomize-a-bit
                                                                (fetch-reply-attack-chance params))
                                  :ambush-attack-chance        (randomize-a-bit
                                                                (fetch-ambush-attack-chance params))
                                  :spell-chance                (randomize-a-bit
                                                                (fetch-spell-chance params))
                                  :attack-spell-chance         (randomize-a-bit
                                                                (fetch-attack-spell-chance params))
                                  :status                      (randomize-a-bit
                                                                (fetch-status params))
                                  :race                        (randomize-a-bit
                                                                (fetch-race params))
                                  :level                       (randomize-a-bit
                                                                (fetch-level params))
                                  :exp-points                  (randomize-a-bit
                                                                (fetch-exp-points params)))))
      results)))
