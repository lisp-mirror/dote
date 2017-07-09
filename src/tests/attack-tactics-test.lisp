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
