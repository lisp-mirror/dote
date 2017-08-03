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

(in-package :priority-queue-test)

(defsuite priority-queue-suite (all-suite))

(deftest test-heap-sort (priority-queue-suite)
  (pq:with-min-queue (queue #'= #'< #'identity)
    (let* ((input  (loop repeat 1000 collect (num:lcg-next-upto 1000)))
           (sorted (sort (copy-list input) #'<)))
      (loop for i in input do
           (pq:push-element queue i))
      (let ((heap-sorted (do* ((el  (pq:pop-element queue) (pq:pop-element queue))
                               (res (list el)              (push el res)))
                              ((pq:emptyp queue) (reverse res)))))
        (assert-equalp sorted heap-sorted)))))

(deftest test-remove-simple (priority-queue-suite)
  (pq:with-min-queue (queue #'= #'< #'identity)
    (let* ((input  (list 1 3 5 7 2 4 6 8))
           (sorted (sort (remove 8 (copy-list input)) #'<)))
      (loop for i in input do
           (pq:push-element queue i))
      (remove-element queue 8)
      (let ((heap-sorted (do* ((el  (pq:pop-element queue) (pq:pop-element queue))
                               (res (list el)              (push el res)))
                              ((pq:emptyp queue) (reverse res)))))
        (assert-equalp sorted heap-sorted)))))

(deftest test-remove-big (priority-queue-suite)
  (loop repeat 100 do
       (let ((num 1000))
         (pq:with-min-queue (queue #'= #'< #'identity)
           (let* ((input       (do ((v (num:lcg-next-upto num) (num:lcg-next-upto num))
                                    (res '()))
                                   ((= (length res) num) res)
                                 (pushnew v res :test #'=)))
                  (remove-pos  (num:lcg-next-upto num))
                  (sorted      (sort (misc:delete@ (copy-list input) remove-pos) #'<)))
             (loop for i in input do
                  (pq:push-element queue i))
             (remove-element queue (elt input remove-pos))
             (let ((heap-sorted (do* ((el  (pq:pop-element queue) (pq:pop-element queue))
                                      (res (list el)              (push el res)))
                                ((pq:emptyp queue) (reverse res)))))
               (assert-equalp sorted heap-sorted)))))))
