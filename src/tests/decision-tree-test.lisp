;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

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

(in-package :decision-tree-test)

(defsuite decision-tree-suite (all-suite))

(alexandria:define-constant +votes-file+
    (concatenate 'string (test-dir) "data/id3/votes-noshare.expr")
  :test #'string=)

(deftest vote-test (decision-tree-suite)
  (if (not (fs:file-exists-p +votes-file+))
      (warn (format nil "file ~a does not exists, you can get a raw data file from:
                         https://archive.ics.uci.edu/ml/datasets/congressional+voting+records"
                    votes-file))
      (flet ((test-eq-fn (a b)
               (cond
                 ((and (typep a 'number)
                       (typep b 'number))
                  (num:epsilon= a b))
                 ((and (symbolp a)
                       (symbolp b))
                  (eq a b))
                 (t
                  (error (format nil
                                 "decision tree equal failed: comparing ~a and ~a!"
                                 a b))))))
        (let ((table      (with-open-file (stream +votes-file+) (read stream))) ; file exists
              (attributes (list :handicapped-infants
                                :water-project-cost-sharing
                                :adoption-of-the-budget-resolution
                                :physician-fee-freeze
                                :el-salvador-aid
                                :religious-groups-in-schools
                                :anti-satellite-test-ban
                                :aid-to-nicaraguan-contras
                                :mx-missile
                                :immigration
                                :synfuels-corporation-cutback
                                :education-spending
                                :superfund-right-to-sue
                                :crime
                                :duty-free-exports
                                :export-administration-act-south-africa
                                :party)))
          (assert-true
              (num:with-lcg-seed (1)
                (let ((tree (interfaces:to-sexp (build-tree table attributes))))
                  (tree-equal tree
                              '((:|data| :physician-fee-freeze :|path| (:u :n :y)
                                 :|decisions-count| ((:democrat . 0) (:republican . 0)) :|errors|
                                 20.843739)
                                ((:|data| :mx-missile :|path| (:y :u :n) :|decisions-count|
                                  ((:democrat . 0) (:republican . 0)) :|errors| 4.2995067)
                                 ((:|data| :democrat :|path| nil :|decisions-count|
                                    ((:republican . 1) (:democrat . 3)) :|errors| 2.1893883))
                                 ((:|data| :republican :|path| nil :|decisions-count|
                                    ((:republican . 2)) :|errors| 1.0))
                                 ((:|data| :democrat :|path| nil :|decisions-count|
                                    ((:democrat . 3) (:republican . 0)) :|errors| 1.1101184)))
                                ((:|data| :democrat :|path| nil :|decisions-count|
                                  ((:republican . 1) (:democrat . 167)) :|errors| 2.6100552))
                                ((:|data| :republican :|path| nil :|decisions-count|
                                  ((:democrat . 11) (:republican . 112)) :|errors| 13.934177)))
                              :test #'test-eq-fn))))))))
