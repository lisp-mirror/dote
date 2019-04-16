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

(in-package :world)

(defun make-weapon (level type)
  (random-weapon:generate-weapon (alexandria:clamp level
                                                   random-weapon:+minimum-level+
                                                   random-weapon:+maximum-level+)
                                 type))

(defmacro gen-make-weapon (type)
  `(defun ,(misc:format-fn-symbol t "make-~a" type) (map-level)
     (make-weapon map-level ,(alexandria:make-keyword type))))

(gen-make-weapon sword)

(gen-make-weapon bow)

(gen-make-weapon crossbow)

(gen-make-weapon mace)

(gen-make-weapon spear)

(gen-make-weapon staff)

(defun maybe-add-fx-chance (map-level)
  (die-utils:pass-d20 (truncate map-level)))

(defmacro with-maybe-fx ((map-level) &body body)
  `(when (maybe-add-fx-chance ,map-level)
     ,@body))

(defun best-weapon-type (player)
  (with-accessors ((edge-weapons-chance-bonus   edge-weapons-chance-bonus)
                   (impact-weapons-chance-bonus impact-weapons-chance-bonus)
                   (pole-weapons-chance-bonus   pole-weapons-chance-bonus)) player
    (let* ((all-bonuses  (list (cons :edge   edge-weapons-chance-bonus)
                               (cons :impact impact-weapons-chance-bonus)
                               (cons :pole   pole-weapons-chance-bonus)))
           (bonus-sorted (shellsort all-bonuses #'(lambda (a b) (d> (cdr a) (cdr b)))))
           (best-weapon  (first bonus-sorted))
           (candidates   (remove-if-not #'(lambda (a) (epsilon= (cdr a) (cdr best-weapon)))
                                        bonus-sorted)))
      (car (misc:random-elt candidates)))))

(defmacro gen-make-item (package type)
  `(defun ,(misc:format-fn-symbol t "make-~a" type) (map-level)
     (with-maybe-fx (map-level)
       (,(misc:format-fn-symbol package "generate-~a" type) map-level))))

(gen-make-item random-ring ring)

(gen-make-item random-trap trap)

(gen-make-item random-elm elm)

(gen-make-item random-shoes shoes)

(gen-make-item random-armor armor)

(gen-make-item random-shield shield)

(defun maybe-add-to-inventory (entity item)
  (and entity
       item
       (sprite:add-to-inventory entity item)))

(defun maybe-wear (entity item)
  (and entity
       item
       (sprite:wear-item entity item)))

(defun wear-all (entity &rest objects)
  (loop for object in objects do
       (maybe-wear entity object)))

(defun add-and-wear-common-items (entity map-level)
  (let* ((ring   (make-ring   map-level))
         (armor  (make-armor  map-level))
         (shoes  (make-shoes  map-level))
         (shield (make-shield map-level))
         (elm    (make-elm    map-level))
         (trap   (make-trap   map-level)))
    (wear-all entity ring armor shoes shield elm)
    (maybe-add-to-inventory entity trap)
    entity))

(defun add-and-wear-common-items-magic-user (entity map-level)
  (let* ((ring   (make-ring   map-level))
         (shoes  (make-shoes  map-level))
         (shield (make-shield map-level))
         (trap   (make-trap   map-level)))
    (wear-all entity ring shoes shield)
    (maybe-add-to-inventory entity trap)
    entity))

(defun increase-change-weapon (ghost)
  (flet ((clamp (a)
           (max 0.0 a)))
    (let* ((best-weapon-type (best-weapon-type ghost)))
      (ecase best-weapon-type
        (:edge
         (setf (edge-weapons-chance-bonus ghost)
               (clamp (edge-weapons-chance-bonus ghost))))
        (:impact
         (setf (impact-weapons-chance-bonus ghost)
               (clamp (impact-weapons-chance-bonus ghost))))
        (:pole
         (setf (pole-weapons-chance-bonus ghost)
               (clamp (pole-weapons-chance-bonus ghost))))))))

(defgeneric build-inventory* (object faction player-class map-level))

(defmethod build-inventory* ((object entity) (faction (eql +npc-type+))
                             (player-class (eql :warrior))
                             map-level)
  (with-accessors ((ghost ghost)) object
    (let* ((best-weapon-type (best-weapon-type ghost))
           (weapon           (ecase best-weapon-type
                               (:edge
                                (make-sword map-level))
                               (:impact
                                (make-mace map-level))
                               (:pole
                                (make-spear map-level)))))
      (increase-change-weapon ghost)
      (add-and-wear-common-items object map-level)
      (wear-all object weapon)))
  object)

(defmethod build-inventory* ((object entity) (faction (eql +npc-type+))
                             (player-class (eql :ranger))
                             map-level)
  (build-inventory* object faction :warrior map-level))

(defmethod build-inventory* ((object entity) (faction (eql +npc-type+))
                             (player-class (eql :archer))
                             map-level)
  (let* ((weapon           (if (die-utils:pass-d2 1)
                               (make-bow      map-level)
                               (make-crossbow map-level))))
    (add-and-wear-common-items object map-level)
    (wear-all object weapon))
  object)

(defmethod build-inventory* ((object entity) (faction (eql +pc-type+))
                             player-class map-level)
  (build-inventory* object +npc-type+ player-class map-level))

(defmethod build-inventory* ((object entity) (faction (eql +npc-type+))
                             (player-class (eql :wizard))
                             map-level)
  (with-accessors ((ghost ghost)) object
    (let* ((best-weapon-type  (best-weapon-type ghost))
           (long-range-weapon   (and (die-utils:pass-d10 (max 1 (truncate (/ map-level 2))))
                                     (make-bow      map-level)))
           (weapon            (ecase best-weapon-type
                                (:edge
                                 (make-sword map-level))
                                (:impact
                                 (make-mace map-level))
                                (:pole
                                 (make-spear map-level)))))
      (add-and-wear-common-items-magic-user object map-level)
      (wear-all object (or long-range-weapon weapon))))
  object)

(defmethod build-inventory* ((object entity) (faction (eql +npc-type+))
                             (player-class (eql :healer))
                             map-level)
  (build-inventory* object faction :wizard map-level))

(defgeneric build-inventory (object faction player-class))

(defmacro with-map-level ((map-level entity) &body body)
  `(let ((,map-level (level-difficult (state ,entity))))
     ,@body))

(defmethod build-inventory (object
                            (faction      (eql +npc-type+))
                            (player-class (eql :warrior)))
  (with-map-level (map-level object)
    (build-inventory* object faction (player-class (ghost object)) map-level)))

(defmethod build-inventory (object
                            (faction      (eql +npc-type+))
                            (player-class (eql :ranger)))
  (build-inventory object faction :warrior))

(defmethod build-inventory (object
                            (faction      (eql +npc-type+))
                            (player-class (eql :archer)))
  (with-map-level (map-level object)
    (build-inventory* object faction player-class map-level)))

(defmethod build-inventory (object
                            (faction      (eql +npc-type+))
                            (player-class (eql :wizard)))
  (build-inventory object faction :warrior))

(defmethod build-inventory (object
                            (faction      (eql +npc-type+))
                            (player-class (eql :healer)))
  (build-inventory object faction :wizard))

(defmethod build-inventory (object (faction (eql +pc-type+)) player-class)
  (build-inventory* object faction player-class 1))
