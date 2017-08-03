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

(in-package :goap-test)

(defsuite goap-suite (all-suite))

(deftest test-heuristic-distance-state (goap-suite)
  (let ((curr (make-instance 'planner-state
                             :variables (list (cons 'a nil) (cons 'b nil))))
        (goal (make-instance 'planner-state
                             :variables (list (cons 'a t)   (cons 'b t)))))
    (assert-true (= 2 (state-diff curr goal)))
    (push (cons 'c 1) (variables goal))
    (assert-true (= 3 (state-diff curr goal)))))

(defun make-sample-planner ()
  (make-instance 'planner
                 :current-state (make-instance 'planner-state
                                               :variables (list (cons 'a nil)
                                                                (cons 'b nil)))
                 :goal-state    (make-instance 'planner-state
                                               :variables (list (cons 'a t)
                                                                (cons 'b t)))
                 :actions-bag   (list (make-action :name :ok
                                                   :preconditions (list (cons 'n t)
                                                                       (cons 'c nil)
                                                                       (cons 'e t))
                                                   :effects       (list (cons 'a t)
                                                                        (cons 'e t))))))

(defun make-planner ()
  (let* ((start-state (make-instance 'planner-state
                                     :variables (list (cons :enemy-visible   nil)
                                                      (cons :armed-with-gun  t)
                                                      (cons :weapon-loaded   nil)
                                                      (cons :enemy-lined-up  nil)
                                                      (cons :enemy-alive     t)
                                                      (cons :armed-with-bomb t)
                                                      (cons :near-enemy      nil)
                                                      (cons :alive           t))))
         (goal-state (make-instance 'planner-state
                                    :variables (list (cons :enemy-alive nil))))
         (planner    (make-instance 'planner
                                    :current-state start-state
                                    :goal-state    goal-state)))
    (add-action planner (make-action :name          :scout
                                     :preconditions (list (cons :armed-with-gun t))
                                     :effects       (list (cons :enemy-visible  t))
                                     :cost          2))
    (add-action planner (make-action :name          :approach
                                     :preconditions (list (cons :enemy-visible t))
                                     :effects       (list (cons :near-enemy    t))
                                     :cost          2))
    (add-action planner (make-action :name          :aim
                                     :preconditions (list (cons :enemy-visible  t)
                                                          (cons :weapon-loaded  t))
                                     :effects       (list (cons :enemy-lined-up t))
                                     :cost          2))
    (add-action planner (make-action :name          :shoot
                                     :preconditions (list (cons :enemy-lined-up t))
                                     :effects       (list (cons :enemy-alive    nil))
                                     :cost          2))
    (add-action planner (make-action :name          :load
                                     :preconditions (list (cons :armed-with-gun t))
                                     :effects       (list (cons :weapon-loaded  t))
                                     :cost          2))
    (add-action planner (make-action :name          :detonate-bomb
                                     :preconditions (list (cons :armed-with-bomb t)
                                                          (cons :near-enemy      t))
                                     :effects       (list (cons :alive           nil)
                                                          (cons :enemy-alive     nil))
                                     :cost          2))
    (add-action planner (make-action :name          :flee
                                     :preconditions (list (cons :enemy-visible  t))
                                     :effects       (list (cons :near-enemy     nil))
                                     :cost          2))
    planner))

(deftest sucide-plan (goap-suite)
  (assert-equalp '(:scout :approach :detonate-bomb)
      (build-plan (make-planner))))

(deftest shoot-plan (goap-suite)
  (let ((planner (make-planner)))
    ;; increase  the  cost  for   :detonate-bomb  to  make  the  plans
    ;; containing this action less useful
    (setf (action-cost (find-action planner :detonate-bomb)) 10)
    (assert-equalp '(:scout :load :aim :shoot)
        (build-plan planner))))

(deftest load-blocked-by-context-plan (goap-suite)
  (let ((planner (make-planner)))
    ;; increase a  lot the cost  for :detonate-bomb to make  the plans
    ;; containing this action less useful
    (setf (action-cost (find-action planner :detonate-bomb)) 100)
    ;; ... but the player has not got any weapon, so can not :load.
    (push #'(lambda () nil)
          (goap::action-context-preconditions (find-action planner :load)))
    ;; so they are forced to use a bomb.
    (assert-equalp '(:scout :approach :detonate-bomb)
        (build-plan planner))))

(deftest test-apply-action (goap-suite)
  (let* ((start-state (make-instance 'planner-state
                                     :variables (list (cons :enemy-visible   nil)
                                                      (cons :armed-with-gun  t)
                                                      (cons :weapon-loaded   nil)
                                                      (cons :enemy-lined-up  nil)
                                                      (cons :enemy-alive     t)
                                                      (cons :armed-with-bomb nil)
                                                      (cons :near-enemy      nil)
                                                      (cons :alive           t))))
         (goal-state (make-instance 'planner-state
                                    :variables (list (cons :enemy-alive nil))))
         (planner    (make-instance 'planner
                                    :current-state start-state
                                    :goal-state    goal-state))
         (action     (make-action :name          :detonate-bomb
                                  :preconditions (list (cons :armed-with-bomb t)
                                                       (cons :near-enemy      t))
                                  :effects       (list (cons :alive           nil)
                                                       (cons :enemy-alive     nil))
                                  :cost          25)))
    (assert-true  (tree-equal (list (cons :enemy-visible   nil)
                                    (cons :armed-with-gun  t)
                                    (cons :weapon-loaded   nil)
                                    (cons :enemy-lined-up  nil)
                                    (cons :enemy-alive     nil)
                                    (cons :armed-with-bomb nil)
                                    (cons :near-enemy      nil)
                                    (cons :alive           nil))
                              (variables (current-state (goap::apply-action planner action)))))))

(deftest test-satisfise-goal (goap-suite)
  (assert-true  (goap::action-satisfise-goal-p (make-instance 'planner-state
                                                              :variables (list (cons 'a nil)
                                                                               (cons 'b nil)))
                                               (make-instance 'planner-state
                                                              :variables (list (cons 'a t)
                                                                               (cons 'b t)))
                                               (make-action :name :ok
                                                            :effects       (list (cons 'a t)
                                                                                 (cons 'b nil)
                                                                                 (cons 'g nil)))))
  (assert-false  (goap::action-satisfise-goal-p (make-instance 'planner-state
                                                               :variables (list (cons 'a nil)
                                                                                (cons 'b nil)))
                                                (make-instance 'planner-state
                                                               :variables (list (cons 'a t)
                                                                                (cons 'b t)))
                                                (make-action :name :no
                                                             :effects       (list (cons 'a nil)
                                                                                  (cons 'b nil)
                                                                                  (cons 'g nil)))))
  (assert-false (goap::action-satisfise-goal-p (make-instance 'planner-state
                                                              :variables (list (cons 'a nil)
                                                                               (cons 'b nil)))
                                               (make-instance 'planner-state
                                                              :variables (list (cons 'a nil)
                                                                               (cons 'b t)))
                                               (make-action :name :no
                                                            :effects       (list (cons 'a t)
                                                                                 (cons 'b nil)
                                                                                 (cons 'g nil)))))
  (assert-false (goap::action-satisfise-goal-p (make-instance 'planner-state
                                                              :variables (list (cons 'a nil)
                                                                               (cons 'b nil)))
                                               (make-instance 'planner-state
                                                              :variables (list (cons 'a nil)
                                                                               (cons 'b t)))
                                               (make-action :name :no
                                                            :effects       (list (cons 'a nil)
                                                                                 (cons 'b nil)
                                                                                 (cons 'g nil))))))
