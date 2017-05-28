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
