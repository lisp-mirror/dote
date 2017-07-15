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

(in-package :blackboard)

;; atk := (pos . mp)
(defun atk-mp (atk)
  (cdr atk))

(defun atk-pos (atk)
  (car atk))

(defun atk-mp-o (atk out)
  (cdro atk out))

(defun atk-pos-o (atk out)
  (caro atk out))

(defun def-pos (def)
  (car def))

(defun def-goal-pos (def)
  (second def))

(defun def-pos-o (def out)
  (caro def out))

(defun def-goal-pos-o (def out)
  (secondo def out))

(defun def-mp-o (def out)
  (thirdo def out))

(defun def-mp (def)
  (third def))

(defun attack-equals-p (a b)
  (and (ivec2= (def-goal-pos a) (def-goal-pos b))
       (ivec2= (def-pos a)      (def-pos b))
       (=      (def-mp a)       (def-mp  b))))

;; atk := (pos . mp)
;; def := (def-slot-pos atk-pos mp)

(defun make-def-o (def-pos atk-pos atk-mp out)
  (project (def-pos atk-pos atk-mp)
    (== out `(,def-pos ,atk-pos ,atk-mp))))

(defun reachablep (atk-pos def-pos mp)
  (<= (ivec2-length (ivec2- atk-pos def-pos))
      mp))

(defun tactic-attacker-pos (tactic)
  (elt tactic 1))

(defun tactic-defender-slot-pos (tactic)
  (elt tactic 0))

(defun tactic-valid-p (tactic)
  (= (length tactic) 3))

(defparameter *reachable-p-fn* #'reachablep)

(defun reachableo (def)
  (fresh (atk-pos def-pos mp)
    (def-pos-o def  def-pos)
    (def-goal-pos-o def atk-pos)
    (def-mp-o       def mp)
    (project (atk-pos def-pos mp)
      (let ((res (funcall *reachable-p-fn* atk-pos def-pos mp)))
        (== res t)))))

(defun attach-attacker@-o (defenders attacker list-length pos out &optional (ct 0))
  (conda
   ((project (ct list-length)    ;; after position
      (== (>= ct list-length) t)
      (== out '())))
   ((project  (ct  pos)            ; position  of  the element  where
      (== (= ct pos) t)            ; attacker must be attached to
      (fresh (c d cdd tmp new-defender)
        (conso c d defenders)
        (cdro  c cdd)
        (conda
            ((nullo cdd)
             (fresh (def-pos atk-pos atk-mp)
               (def-pos-o c def-pos)
               (atk-mp-o  attacker atk-mp)
               (atk-pos-o attacker atk-pos)
               (make-def-o def-pos atk-pos atk-mp new-defender)
               (reachableo new-defender)))
            (else
             (== c new-defender)))
        (attach-attacker@-o d attacker list-length pos tmp (1+ ct))
        (conso new-defender tmp out))))
   (else
    (project (ct pos)
      (fresh (c d tmp)
        (conso c d defenders)
        (attach-attacker@-o d attacker list-length pos tmp (1+ ct))
        (conso  c tmp out))))))

(defun attach-attacker-o (defender attacker list-length ct out)
  (conda
   ((project (ct list-length)
      (== (>= ct list-length) t)
      (== out '())))
   (else
    (project (ct)
      (fresh (tmp tmp2)
        (attach-attacker@-o defender attacker list-length ct tmp)
        (attach-attacker-o  defender attacker list-length (1+ ct)  tmp2)
        (conso tmp tmp2 out))))))

;; defenders (list (def1 def2 def3 ...) ...)
(defun substo-all (defenders defender-number attacker out)
  (conde
   ((nullo defenders)
    (== out '()))
   (else
    (fresh (car-def cdr-def tmp tmp2)
      (conso  car-def cdr-def defenders)
      (attach-attacker-o car-def attacker defender-number 0 tmp)
      (substo-all cdr-def   defender-number attacker tmp2)
      (appendo tmp tmp2 out)))))

(defun %make-attack-tactics (defenders defender-number attackers out)
  (conde
   ((nullo attackers)
    (== out '()))
   (else
    (fresh (car-atk cdr-atk tmp res)
      (conso car-atk cdr-atk attackers)
      (substo-all defenders defender-number car-atk tmp)
      (%make-attack-tactics tmp defender-number cdr-atk res)
      (appendo tmp res out)))))

(defun make-attack-tactics (defenders attackers)
  (car (run* (q)
         (%make-attack-tactics (list defenders) (length defenders) attackers q))))

(defun attacker-class->attacker (game-state atk)
  (let* ((entity (entity:find-entity-by-id game-state (blackboard:entity-id atk)))
         (mp     (character:actual-movement-points    (entity:ghost entity)))
         (pos    (mesh:calculate-cost-position entity)))
    (cons pos mp)))

(defun defender-class->defender (def)
  (let ((positions (blackboard:goal-pos def)))
    (mapcar #'(lambda (a) (list a)) positions)))

(defgeneric fetch-defender-positions (object))

(defgeneric fetch-attacker-positions (object))

(define-constant +pole-key-tactics+     :pole     :test #'eq)

(define-constant +melee-key-tactics+    :melee    :test #'eq)

(define-constant +bow-key-tactics+      :bow      :test #'eq)

(define-constant +crossbow-key-tactics+ :crossbow :test #'eq)

(defmethod fetch-defender-positions ((object blackboard:blackboard))
  (with-accessors ((main-state                      main-state)
                   (attack-enemy-pole-positions     attack-enemy-pole-positions)
                   (attack-enemy-melee-positions    attack-enemy-melee-positions)
                   (attack-enemy-bow-positions      attack-enemy-bow-positions)
                   (attack-enemy-crossbow-positions attack-enemy-crossbow-positions))
      object
    (flet ((%fetch (l)
             (loop for i in l append (defender-class->defender i))))
      (list
       +pole-key-tactics+     (%fetch attack-enemy-pole-positions)
       +melee-key-tactics+    (%fetch attack-enemy-melee-positions)
       +bow-key-tactics+      (%fetch attack-enemy-bow-positions)
       +crossbow-key-tactics+ (%fetch attack-enemy-crossbow-positions)))))

(defmethod fetch-attacker-positions ((object blackboard))
  (with-accessors ((main-state main-state)) object
    (let ((res '()))
      (flet ((fetch (entity)
               (let* ((ghost  (ghost entity))
                      (status (character:status ghost)))
                 (when (or (not status)
                           (and (not (member status
                                             (list interactive-entity:+status-terror+
                                                   interactive-entity:+status-berserk+
                                                   interactive-entity:+status-faint+)))))
                   (push (cons (mesh:calculate-cost-position entity)
                               (truncate (character:actual-movement-points ghost)))
                         res)))))
        (map-ai-entities main-state #'(lambda (k v)
                                        (declare (ignore k))
                                        (fetch v)))
      res))))

(defun find-defender-id-by-goal-position (blackboard position)
  (with-accessors ((attack-enemy-pole-positions     attack-enemy-pole-positions)
                   (attack-enemy-melee-positions    attack-enemy-melee-positions)
                   (attack-enemy-bow-positions      attack-enemy-bow-positions)
                   (attack-enemy-crossbow-positions attack-enemy-crossbow-positions))
      blackboard
      (flet ((find-id (defenders)
               (loop for defender in defenders do
                    (when (find position (goal-pos defender) :test #'ivec2:ivec2=)
                      (return-from find-id (entity-id defender))))))
        (or (find-id attack-enemy-pole-positions)
            (find-id attack-enemy-melee-positions)
            (find-id attack-enemy-bow-positions)
            (find-id attack-enemy-crossbow-positions)))))

(defun attack-tactic->id-entities (blackboard attack-tactic)
  "values are id-attacker id-defender if any"
  (with-accessors ((main-state main-state)) blackboard
    (let* ((attacker-position (tactic-attacker-pos      attack-tactic))
           (defender-position (tactic-defender-slot-pos attack-tactic)))
      (values (game-state:find-ai-id-by-position main-state attacker-position)
              (find-defender-id-by-goal-position blackboard defender-position)))))

(defun build-single-attack-tactics (blackboard position-attakers position-defenders)
  (let ((all (blackboard:make-attack-tactics position-defenders position-attakers)))
    (setf all (remove-if #'(lambda (plan) (every #'(lambda (a) (null (cdr a))) plan))
                         all))
    (when all
      (setf all (misc:random-elt all))
      (setf all (remove-if-not #'tactic-valid-p all)))
    (mapcar #'(lambda (plan)
                (multiple-value-bind (attacker-id defender-id)
                    (attack-tactic->id-entities blackboard plan)
                    (make-instance 'attacker
                                   :target-pos (tactic-defender-slot-pos plan)
                                   :target-id  defender-id
                                   :entity-id  attacker-id)))
            all)))

(defun build-all-attack-tactics (blackboard)
   (let* ((all-defender-pos (fetch-defender-positions blackboard))
          (all-attacker-pos (fetch-attacker-positions blackboard)))
     (when (> (length all-attacker-pos)
              (length all-defender-pos))
       (setf all-attacker-pos (subseq all-attacker-pos 0 (length all-defender-pos))))
     (list
      +pole-key-tactics+     (build-single-attack-tactics blackboard
                                                          all-attacker-pos
                                                          (getf all-defender-pos
                                                                +pole-key-tactics+))
      +melee-key-tactics+    (build-single-attack-tactics blackboard
                                                          all-attacker-pos
                                                          (getf all-defender-pos
                                                                +melee-key-tactics+))
      +bow-key-tactics+      (build-single-attack-tactics blackboard
                                                          all-attacker-pos
                                                          (getf all-defender-pos
                                                                +bow-key-tactics+))
      +crossbow-key-tactics+ (build-single-attack-tactics blackboard
                                                          all-attacker-pos
                                                          (getf all-defender-pos
                                                                +crossbow-key-tactics+)))))
