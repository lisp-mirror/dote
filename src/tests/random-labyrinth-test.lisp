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

(in-package :random-labyrinth-test)

(defsuite random-labyrinth-suite (all-suite))

(defun test-gen (size &key
                        (random-seed 3589552221) (debug nil) (scale-fact 1)
                        (tmp-ppm-filename "tmp/lab.ppm")
                        (tmp-ps-filename "tmp/lab.ps"))
  (num:with-lcg-seed (random-seed)
    (let* ((func-sigma #'(lambda (x a) (declare (ignorable a)) (+ 10 x)))
           (func-door #'(lambda (x a) (declare (ignorable a x)) 5))
           (func-furniture #'(lambda (x a) (declare (ignorable a x)) 50))
           (func-win #'(lambda (x a) (declare (ignorable a)) (1+ x)))
           (root (generate size
                           :scale-fact scale-fact
                           :func-sigma-w func-sigma
                           :func-sigma-h func-sigma
                           :func-door func-door
                           :func-win func-win
                           :func-furniture func-furniture)))
      (clear-mat root)
      (room->mat root)
      (let ((tmp-ppm (concatenate 'string (test-dir) tmp-ppm-filename))
            (tmp-ps (concatenate 'string (test-dir)  tmp-ps-filename)))
        (if debug
            (random-labyrinth::debug-dump root tmp-ppm :draw-door-to-nowhere t :draw-id t)
            (random-labyrinth::dump root tmp-ppm :draw-door-to-nowhere t :draw-id nil))
        (random-labyrinth::dump-dot root tmp-ps)
        (values tmp-ppm tmp-ps)))))

(alexandria:define-constant +labyrinths-dir+
    (concatenate 'string (test-dir) "data/labyrinths/")
  :test #'string=)

(deftest generate-labyrinth-dbg-test (random-labyrinth-suite)
  (with-kernel
    (assert-true
        (multiple-value-bind (tmp-ppm tmp-ps)
            (test-gen 60
                      :random-seed 3589552221
                      :debug t
                      :scale-fact 5
                      :tmp-ppm-filename "tmp/lab-dbg.ppm"
                      :tmp-ps-filename "tmp/lab-dbg.ps")
          (let ((test (and
                       (= (fs:file-hash tmp-ppm)
                          (fs:file-hash (concatenate 'string
                                                     +labyrinths-dir+
                                                     "lab-60-t-5-3589552221.ppm")))
                       (= (fs:file-hash tmp-ps)
                          (fs:file-hash (concatenate 'string
                                                     +labyrinths-dir+
                                                     "lab-60-t-5-3589552221.ps"))))))
            (when test
              (uiop/filesystem:delete-file-if-exists tmp-ppm)
              (uiop/filesystem:delete-file-if-exists tmp-ps))
            test)))))

(deftest generate-labyrinth-test (random-labyrinth-suite)
  (with-kernel
    (assert-true
        (multiple-value-bind (tmp-ppm tmp-ps)
            (test-gen 60
                      :random-seed 3589552221
                      :debug nil
                      :scale-fact 2
                      :tmp-ppm-filename "tmp/lab.ppm"
                      :tmp-ps-filename "tmp/lab.ps")
          (let ((test (and
                       (= (fs:file-hash tmp-ppm)
                          (fs:file-hash (concatenate 'string
                                                     +labyrinths-dir+
                                                     "lab-60-nil-2-3589552221.ppm")))
                       (= (fs:file-hash tmp-ps)
                          (fs:file-hash (concatenate 'string
                                                     +labyrinths-dir+
                                                     "lab-60-nil-2-3589552221.ps"))))))
            (when test
              (uiop/filesystem:delete-file-if-exists tmp-ppm)
              (uiop/filesystem:delete-file-if-exists tmp-ps))
            test)))))
