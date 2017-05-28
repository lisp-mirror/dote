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

(in-package :action-scheduler-test)

(defsuite action-scheduler-suite (all-suite))

(defun make-test-action-scheduler-special ()
  (let ((scheduler (make-instance 'action-scheduler))
        (res       '()))
    (let ((*equeue-merge-to-subscheduler-p* t))
      (with-enqueue-action (scheduler action-scheduler)
        (appendf res (list :a1)))
      (let ((*equeue-merge-to-subscheduler-p* nil))
        (with-enqueue-action (scheduler launch-arrow-action)
          (appendf res (list :a)))
        (with-enqueue-action (scheduler attack-launch-spell-action)
          (appendf res (list :b)))
        (with-enqueue-action (scheduler particle-effect-action)
          (appendf res (list :c))))
      (with-enqueue-action (scheduler blood-spill-action)
        (appendf res (list :a2)))
      (with-enqueue-action (scheduler end-spell-action) ;; not captured (type not allowed)
        (appendf res (list :a3)))
      (loop repeat 10 do
           (substitute-action scheduler)))
    res))

(defun make-test-action-scheduler-capture ()
  (let ((scheduler (make-instance 'action-scheduler))
        (res       '()))
    (with-enqueue-action (scheduler action-scheduler)
      (appendf res (list :a1)))
    (with-enqueue-action (scheduler launch-arrow-action)
      (appendf res (list :a)))
    (with-enqueue-action (scheduler attack-launch-spell-action)
      (appendf res (list :b)))
    (with-enqueue-action (scheduler particle-effect-action)    ;; captured
      (appendf res (list :c)))
    (with-enqueue-action (scheduler blood-spill-action)        ;; captured
      (appendf res (list :a2)))
    (with-enqueue-action (scheduler end-spell-action)
      (appendf res (list :a3)))
    (loop repeat 10 do
         (substitute-action scheduler))
    res))

(defun make-test-action-scheduler-capture-simple ()
  (let ((scheduler (make-instance 'action-scheduler))
        (res       '()))
    (with-enqueue-action (scheduler action-scheduler)
      (appendf res (list :a1)))
    (with-enqueue-action (scheduler particle-effect-action)
      (appendf res (list :a3)))
    (with-enqueue-action (scheduler send-spell-fx-action)
      (appendf res (list :a4)))
    (loop repeat 5 do (substitute-action scheduler))
    res))

(deftest action-scheduler-capture-test (action-scheduler-suite)
  (assert-equalp '(:a1 :c :a2 :a :b :a3)
      (make-test-action-scheduler-capture)))

(deftest action-scheduler-capture-test-simple (action-scheduler-suite)
  (assert-equalp '(:a1 :a3 :a4)
      (make-test-action-scheduler-capture-simple)))

(deftest action-scheduler-special-test (action-scheduler-suite)
  (assert-equalp '(:a1 :a2 :a :b :c :a3)
      (make-test-action-scheduler-special)))

(defparameter *scheduler* (make-instance 'action-scheduler))

(defun build-unnested-priority-test ()
  (setf *scheduler* (make-instance 'action-scheduler))
  (let ((*default-action-priority* (next-minimum-priority *scheduler*)))
    (with-enqueue-action (*scheduler* launch-arrow-action))   ; 1
    (with-enqueue-action (*scheduler* launch-arrow-action))   ; 1
    (with-enqueue-action (*scheduler* launch-arrow-action)    ; 1
      (with-enqueue-action (*scheduler* end-spell-action))))) ; 0 (default)

(defun make-unnested-priority-test-priorities ()
  (build-unnested-priority-test)
  (loop repeat 4 collect
       (prog1
           (action-scheduler::priority (current-action *scheduler*))
         (substitute-action *scheduler*))))

(deftest unnested-priority-test (action-scheduler-suite)
  (let ((priorities (make-unnested-priority-test-priorities)))
    (assert-false (every #'(lambda (a) (= a (first priorities))) priorities))))
