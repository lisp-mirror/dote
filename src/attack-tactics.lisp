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

(in-package :attack-tactics)

;; atk := (pos . mp)
(defun atk-mp (atk)
  (cdr atk))

(defun atk-pos (atk)
  (car atk))

(defun atk-mp-o (atk out)
  (cdro atk out))

(defun atk-pos-o (atk out)
  (caro atk out))

;; def := (pos slot-pos mp)
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
;; def := (pos slot-pos mp)

(defun make-def-o (def-pos atk-pos atk-mp out)
  (project (def-pos atk-pos atk-mp)
    (== out `(,def-pos ,atk-pos ,atk-mp))))

(defun reachablep (atk-pos def-pos mp)
  (<= (ivec2-length (ivec2- atk-pos def-pos))
      mp))

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

(defun make-attack-tactics (defenders defender-number attackers out)
  (conde
   ((nullo attackers)
    (== out '()))
   (else
    (fresh (car-atk cdr-atk tmp res)
      (conso car-atk cdr-atk attackers)
      (substo-all defenders defender-number car-atk tmp)
      (make-attack-tactics tmp defender-number cdr-atk res)
      (appendo tmp res out)))))

(defun attacker-class->attacker (game-state atk)
  (let* ((entity (entity:find-entity-by-id game-state (blackboard:entity-id atk)))
         (mp     (character:actual-movement-points    (entity:ghost entity)))
         (pos    (vec2:sequence->vec2                 (mesh:calculate-cost-position entity))))
    (cons pos mp)))

(defun defender-class->defender (def)
  (let ((positions (blackboard:goal-pos def)))
    (mapcar #'(lambda (a) (list (vec2:sequence->vec2 a))) positions)))
