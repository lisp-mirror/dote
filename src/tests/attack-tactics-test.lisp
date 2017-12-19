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

(in-package :attack-tactics-test)

(defsuite attack-tactics-suite (all-suite))

(deftest test-attack-tactic-no-superimpose (attack-tactics-suite)
  (num:with-lcg-seed (1)
    (let* ((state      (make-instance 'game-state:game-state
                                      :map-state (matrix:make-matrix 8 8)))
           (blackboard (make-instance 'blackboard :main-state state))
           (player     (make-instance 'md2-mesh:md2-mesh :state state)))
      (setf (game-state:blackboard state) blackboard)
      (with-accessors ((attack-enemy-pole-positions     attack-enemy-pole-positions)
                       (attack-enemy-melee-positions    attack-enemy-melee-positions)) blackboard
        (setf attack-enemy-pole-positions
              (blackboard::push-target-position attack-enemy-pole-positions
                                                player
                                                (ivec2:ivec2 0 0)))
        (setf attack-enemy-melee-positions
              (blackboard::push-target-position attack-enemy-melee-positions
                                                player
                                                (ivec2:ivec2 0 0)))
        (assert-true
            (or (and (every #'(lambda (a) (null (blackboard::goal-pos a)))
                            attack-enemy-pole-positions)
                     (every #'(lambda (a) (not (null (blackboard::goal-pos a))))
                            attack-enemy-melee-positions))
                (and (every #'(lambda (a) (not (null (blackboard::goal-pos a))))
                            attack-enemy-pole-positions)
                     (every #'(lambda (a) (null (blackboard::goal-pos a)))
                            attack-enemy-melee-positions))))))))

(defun %comp-tactics (a b)
  (equalp (flatten a)
          (flatten b)))

(defun make-tactics-3-9 ()
  (let* ((def (list (cons (ivec2 1  0) nil)
                    (cons (ivec2 18 0) nil)
                    (cons (ivec2 19 0) nil)))
          (atk (list (cons (ivec2 20 0) 30)
                     (cons (ivec2 21 0) 31)
                     (cons (ivec2 11 0) 7)
                     (cons (ivec2 11 1) 8)
                     (cons (ivec2 11 2) 9)
                     (cons (ivec2 14 0) 7)
                     (cons (ivec2 15 0) 7)
                     (cons (ivec2 16 0) 7))))
    (blackboard::%build-single-attack-tactics def (rest atk) (first atk) nil)))

(defun make-tactics-compete ()
  (let* ((def (list (cons (ivec2 10 0) nil)
                    (cons (ivec2 10 1) nil)))
         (atk (list (cons (ivec2 9 0) 1)
                    (cons (ivec2 11 0) 1))))
    (blackboard::%build-single-attack-tactics def (rest atk) (first atk) nil)))

(defun make-tactics-random ()
  (let* ((def (loop repeat 50 collect
                   (cons (ivec2 (num:lcg-next-upto 50) 0)
                         nil)))
         (atk (loop repeat 50 collect
                   (cons (ivec2 (num:lcg-next-upto 50) 1)
                         (num:lcg-next-upto 50)))))
    (blackboard::%build-single-attack-tactics def (rest atk) (first atk) nil)))

(defun make-tactics-3-2 ()
  (let* ((def (list (cons (ivec2 6 10) nil)
                    (cons (ivec2 5  9) nil)
                    (cons (ivec2 7  9) nil)))
         (atk (list (cons (ivec2 0  0) 150)
                    (cons (ivec2 0  1) 150))))
    (blackboard::%build-single-attack-tactics def (rest atk) (first atk) nil)))

(deftest test-attack-tactic (attack-tactics-suite)
  (let ((*reachable-p-fn* #'blackboard:reachablep))
    (assert-true
        (%comp-tactics (make-tactics-3-9)
                       '((#(1 0) #(20 0) 30) (#(18 0) #(16 0) 7) (#(19 0) #(21 0) 31))))
    (assert-true
        (%comp-tactics (make-tactics-compete)
                       '((#(10 0) #(11 0) 1) (#(10 1)))))
    (assert-true
        (%comp-tactics (make-tactics-3-2)
                       '((#(6 10) #(0 0) 150) (#(5 9) #(0 1) 150) (#(7 9)))))))

(deftest test-attack-tactic-random (attack-tactics-suite)
  (let ((*reachable-p-fn* #'blackboard:reachablep))
    (assert-true
         (every #'(lambda (a)
             (if (= (length a) 3)
                 (<= (map-utils:map-manhattam-distance (first a) (second a))
                     (third a))
                 t))
                (make-tactics-random)))))
