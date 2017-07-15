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

(in-package :kanren-test)

(defsuite kanren-suite (all-suite))

(facts friend
       (:a :b)
       (:a :c)
       (:c :d))

(deftest test-friend (kanren-suite)
  (assert-equalp '(:c)
      (run* (q)
        (fresh (x)
          (friendo :a x)
          (friendo x  :d)
          (== q x)))))

(defun shifto (l out)
  (fresh (a d new-tail new-list y)
    (conso   a d l)
    (conso   a nil new-tail)
    (appendo d new-tail new-list)
    (== new-list  out)))

(defclass complementary ()
  ((id
    :initform 0
    :initarg :id
    :accessor id)
   (object-keycode
    :initform ""
    :initarg :object-keycode
    :accessor  object-keycode)))

(defmethod equivp ((lhs complementary) (rhs complementary))
  (= (id lhs) (id rhs)))

(defun not-compatible-o (keys chests)
  (conde
    ((nullo keys)
     +fail+)
    ((fresh (a-k a-c)
       (caro keys   a-k)
       (caro chests a-c)
       (project (a-k a-c)
         (== (string= (object-keycode a-k) (object-keycode a-c)) t))
       +succeed+))
    (else
     (fresh (d-k d-c)
       (cdro keys   d-k)
       (cdro chests d-c)
       (not-compatible-o d-k d-c)))))

(defun compatible-o (keys chests)
  (condu
    ((not-compatible-o keys chests)
     +fail+)
    (else
     +succeed+)))

(defun compatible-first-key-keycode (keys keycode)
  (and keys
       (not (string= (object-keycode (first keys)) keycode))))

(defun compatible-arrangement-keys (keys chests start out)
  (conde
    ((compatible-o keys chests)
     (== out keys)
     +succeed+)
    (else
     (fresh (x)
       (shifto keys x)
       (project (keys)
         (== (compatible-first-key-keycode keys start) t))
       (compatible-arrangement-keys x chests start out)))))

(defun distance (a b)
  (expt (- (id a) (id b)) 2))

(defun average-distance (keys chests)
  (let ((distance-sum (loop for chest in chests sum
                           (let ((pos (position-if #'(lambda (a) (string= (object-keycode chest)
                                                                          (object-keycode a)))
                                                   keys)))
                             (distance chest
                                       (if pos
                                           (elt chests pos)
                                           chest))))))
    (/ distance-sum (length chests))))

(defun incompatible-key (keys chest out)
  (conde
    ((nullo keys)
      +fail+)
    ((project (keys chest)
       (== (not keys) nil)
       (== (compatible-first-key-keycode keys (object-keycode chest)) t)
       (== out (object-keycode (first keys)))))
    (else
     (project (keys)
       (== (not keys) nil))
     (incompatible-key (rest keys) chest out))))

(defun arrange-compatible-keys ()
  (let* ((chest-1  (make-instance 'complementary :id 1 :object-keycode "a"))
         (chest-2  (make-instance 'complementary :id 2 :object-keycode "b"))
         (chest-3  (make-instance 'complementary :id 3 :object-keycode "c"))
         (chest-4  (make-instance 'complementary :id 4 :object-keycode "d"))
         (key-1    (make-instance 'complementary :id 5 :object-keycode "a"))
         (key-2    (make-instance 'complementary :id 6 :object-keycode "b"))
         (key-3    (make-instance 'complementary :id 7 :object-keycode "c"))
         (key-4    (make-instance 'complementary :id 8 :object-keycode "d"))
         (chests  (list chest-1 chest-2 chest-3 chest-4))
         (keys    (list key-1 key-2 key-3 key-4)))
    (run 1 (q)
      (fresh (x start)
        (incompatible-key keys chest-1 start)
        (project (start)
          (compatible-arrangement-keys keys chests (coerce start 'string) x))
        (project (x)
          (== (> (average-distance x chests) 3) t))
        (== q x)))))

(deftest test-key-arrangement (kanren-suite)
  (assert-equalp
      '(7 8 5 6)
      (mapcar #'id (first (arrange-compatible-keys)))))

(defun make-tactics-3-3 ()
  (let* ((def (list (cons (ivec2 4 0) nil)
                    (cons (ivec2 3 0) nil)
                    (cons (ivec2 7 0) nil)))
         (atk (list (cons (ivec2 10 0) 7)
                    (cons (ivec2 20 0) 30)
                    (cons (ivec2 21 0) 31)))
         (res (blackboard:make-attack-tactics def atk)))
    (loop for i in res
         when (every #'(lambda (a) (not (null (cdr a)))) i)
       collect i)))

(defun make-tactics-2-2 ()
  (let* ((def (list (cons (ivec2 3 0) nil)
                    (cons (ivec2 1 0) nil)))
         (atk (list (cons (ivec2 10 0) 7)
                    (cons (ivec2 20 0) 30)))
         (res (blackboard:make-attack-tactics def atk)))
    (loop for i in res
         when (every #'(lambda (a) (not (null (cdr a)))) i)
       collect i)))

(defun make-tactics-3-2 ()
  (let* ((def (list (cons (ivec2 4 0) nil)
                    (cons (ivec2 2 0) nil)
                    (cons (ivec2 7 0) nil)))
         (atk (list (cons (ivec2 10 0) 7)
                    (cons (ivec2 21 0) 31)))
         (res (blackboard:make-attack-tactics def atk)))
    res))

(defun %comp-tactics (a b)
  (equalp (flatten a)
          (flatten b)))

(defun %reachablep (atk-pos def-pos mp)
  (<= (ivec2-length (ivec2- atk-pos def-pos))
      mp))

(deftest test-attack-tactic (kanren-suite)
  (let ((*reachable-p-fn* #'%reachablep))
    (assert-true
        (%comp-tactics (make-tactics-2-2)
                       '(((#(3 0) #(10 0) 7) (#(1 0) #(20 0) 30)))))
    (assert-true
        (%comp-tactics (make-tactics-3-3)
                       '(((#(4 0) #(10 0) 7) (#(3 0) #(20 0) 30) (#(7 0) #(21 0) 31))
                         ((#(4 0) #(10 0) 7) (#(3 0) #(21 0) 31) (#(7 0) #(20 0) 30))
                         ((#(4 0) #(20 0) 30) (#(3 0) #(10 0) 7) (#(7 0) #(21 0) 31))
                         ((#(4 0) #(21 0) 31) (#(3 0) #(10 0) 7) (#(7 0) #(20 0) 30))
                         ((#(4 0) #(20 0) 30) (#(3 0) #(21 0) 31) (#(7 0) #(10 0) 7))
                         ((#(4 0) #(21 0) 31) (#(3 0) #(20 0) 30) (#(7 0) #(10 0) 7)))))
    (assert-true
        (%comp-tactics (make-tactics-3-2)
                       '(((#(4 0) #(10 0) 7) (#(2 0)) (#(7 0)))
                         ((#(4 0)) (#(2 0)) (#(7 0)))
                         ((#(4 0)) (#(2 0)) (#(7 0) #(10 0) 7))
                         ((#(4 0) #(10 0) 7) (#(2 0)) (#(7 0)))
                         ((#(4 0) #(10 0) 7) (#(2 0) #(21 0) 31) (#(7 0)))
                         ((#(4 0) #(10 0) 7) (#(2 0)) (#(7 0) #(21 0) 31))
                         ((#(4 0) #(21 0) 31) (#(2 0)) (#(7 0)))
                         ((#(4 0)) (#(2 0) #(21 0) 31) (#(7 0)))
                         ((#(4 0)) (#(2 0)) (#(7 0) #(21 0) 31))
                         ((#(4 0) #(21 0) 31) (#(2 0)) (#(7 0) #(10 0) 7))
                         ((#(4 0)) (#(2 0) #(21 0) 31) (#(7 0) #(10 0) 7))
                         ((#(4 0)) (#(2 0)) (#(7 0) #(10 0) 7)))))))
