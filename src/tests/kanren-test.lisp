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

(defclass attacker ()
  ((id
    :initform 0
    :initarg :id
    :accessor id)))

(defmethod print-object ((object attacker) stream)
  (format stream "*~a" (id object)))

(defclass defender ()
  ((id
    :initform 0
    :initarg :id
    :accessor id)
   (max-attackers
    :initform 0
    :initarg  :max-attackers
    :accessor max-attackers)))

(defmethod print-object ((object defender) stream)
  (format stream "~a[~a]" (id object) (max-attackers object)))

(defmethod equivp ((lhs defender) (rhs defender))
  (= (id lhs) (id rhs)))

(defmethod equivp ((lhs defender) (rhs attacker))
  (= (id lhs) (id rhs)))

(defmethod equivp ((lhs attacker) (rhs attacker))
  (= (id lhs) (id rhs)))

(defmethod equivp ((lhs attacker) (rhs defender))
  (equivp (id lhs) (id rhs)))

(defun reachable-p (a b)
  (and (>= (id a) (id b))))

(defun reachable-o (a b)
  (project (a b)
    (== (reachable-p a b) t)))

(defun make-pair (attacker defenders out)
  (conde
   ((nullo defenders)
    +fail+)
   ((fresh (car-def cdr-def bind)
      (conso car-def cdr-def defenders)
      (reachable-o attacker car-def)    ; actual test
      (conso attacker car-def bind)     ; building res
      (== bind out)))
   (else
    (fresh (cdr-def)
      (cdro defenders cdr-def)
      (make-pair attacker cdr-def out)))))

(defun make-pairs (attackers defenders out)
  (conde
   ((nullo attackers)
    (== '() out))
   (else
    (fresh (car-atk cdr-atk res res2)
      (conso car-atk cdr-atk attackers)
      (make-pair car-atk defenders res)
      (make-pairs cdr-atk defenders res2)
      (conso res res2 out)))))

(defun make-defender (id max-attackers)
  (make-instance 'defender :id id :max-attackers max-attackers))

(defun make-attacker (n)
  (make-instance 'attacker :id n))

(defun make-attacker-tactics-test ()
  (let* ((atk (list (make-attacker 4)
                    (make-attacker 8)
                    (make-attacker 1)
                    (make-attacker 2)))
         (def (list (make-defender 8 1)
                    (make-defender 1 2)
                    (make-defender 2 1)))
         (all-reachables (run* (q)
                           (fresh (all-pairs pair)
                             (caro all-pairs pair)
                             (make-pairs atk def all-pairs)
                             (== q all-pairs)))))
    (remove-if-not #'(lambda (a)
                   (let ((grouped (misc:group-by a
                                                 :test #'(lambda (a b)
                                                           (= (id (cdr a)) (id (cdr b)))))))
                     (every #'(lambda (a)
                                (<= (length a) (max-attackers (cdr (first a)))))
                            grouped)))
                   all-reachables)))

(deftest test-attack-tactic (kanren-suite)
  (assert-true
      (tree-equal
       (make-attacker-tactics-test)
       (list (list (cons (make-attacker 4) (make-defender 2 1))
                   (cons (make-attacker 8) (make-defender 8 1))
                   (cons (make-attacker 1) (make-defender 1 2))
                   (cons (make-attacker 2) (make-defender 1 2)))
             (list (cons (make-attacker 4) (make-defender 1 2))
                   (cons (make-attacker 8) (make-defender 8 1))
                   (cons (make-attacker 1) (make-defender 1 2))
                   (cons (make-attacker 2) (make-defender 2 1))))
       :test #'equivp)))
