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

(in-package :tremors-functions)

(define-constant +tree-shake-dt+                      0.033 :test #'=)

(define-constant +tree-shake-duration+               20.0   :test #'=)

(define-constant +explosion-level-0-shake-duration+   0.5   :test #'=)

(define-constant +explosion-level-0-shake-power+      0.05  :test #'=)

(define-constant +explosion-level-1-shake-duration+   1.0   :test #'=)

(define-constant +explosion-level-1-shake-power+      0.1   :test #'=)

(define-constant +explosion-level-2-shake-duration+   8.0   :test #'=)

(define-constant +explosion-level-2-shake-power+      0.2   :test #'=)

(define-constant +explosion-level-3-shake-duration+  10.0   :test #'=)

(define-constant +explosion-level-3-shake-power+      0.4   :test #'=)

(defun standard-tremor-fn (duration &key (power (num:d/ +terrain-chunk-tile-size+ 30.0)))
  (let ((duration duration))
    #'(lambda (entity dt)
        (declare (ignore entity))
        (when dt
          (setf duration (num:d- duration dt)))
        (let ((delta (if (num:d< duration 0.0)
                         0.0
                         power)))
          (values
           (sb-cga:translate (sb-cga:vec (num:lcg-next-in-range (num:d- delta) delta)
                                         (num:lcg-next-in-range (num:d- delta) delta)
                                         (num:lcg-next-in-range (num:d- delta) delta)))
           duration)))))

(defun tree-tremor-fn (duration &key (power (num:d/ +terrain-chunk-tile-size+ 30.0)))
  (let ((duration duration))
    #'(lambda (entity dt)
        (declare (ignore entity))
        (when dt
          (setf duration (num:d- duration dt)))
        (let ((delta (if (num:d< duration 0.0)
                         0.0
                         power)))
          (values
           (sb-cga:translate (sb-cga:vec (num:lcg-next-in-range (num:d- delta) delta)
                                         0.0
                                         0.0))
           duration)))))
