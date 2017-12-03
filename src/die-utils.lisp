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

(in-package :die-utils)

(defmacro gen-pass-dice (max)
  (with-gensyms (dice-roll)
    `(defun ,(format-fn-symbol t "pass-d~a" max) (val)
       (let ((,dice-roll (lcg-next-upto ,max)))
         (< ,dice-roll val)))))

(defmacro gen-pass-multiple-dice (&rest dice-max-values)
  `(progn
     ,@(loop for i in dice-max-values collect
            `(gen-pass-dice ,i))))

(gen-pass-multiple-dice 2 1.0 100.0 10.0 20 6 4)
