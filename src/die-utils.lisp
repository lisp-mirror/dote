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

(defmacro gen-pass-dice-if (max)
  "generate a function  that lauch a dice  if fn return true  (fn is a
function thaking die value as a single argument)"
  (with-gensyms (dice-roll)
    `(defun ,(format-fn-symbol t "pass-d~a-if" max) (fn)
       (let ((,dice-roll (lcg-next-upto ,max)))
         (funcall fn ,dice-roll)))))

(gen-pass-dice-if 2)

(gen-pass-dice-if 1.0)

(gen-pass-dice-if 100.0)

(gen-pass-dice-if 10.0)

(gen-pass-dice-if 20)

(gen-pass-dice-if 6)

(gen-pass-dice-if 4)

(defmacro gen-pass-dice-periodic (max)
  (with-gensyms (ct)
    `(let ((,ct 0))
       (defun ,(format-fn-symbol t "pass-d~a-periodic" max) (val period &key (default nil))
         (,(format-fn-symbol t "pass-d~a-if" max)
           #'(lambda (a)
               (incf ,ct)
               (if (= (rem ,ct period) 0)
                   (< a val)
                   default)))))))

(gen-pass-dice-periodic 2)

(gen-pass-dice-periodic 1.0)

(gen-pass-dice-periodic 100.0)

(gen-pass-dice-periodic 10.0)

(gen-pass-dice-periodic 20)

(gen-pass-dice-periodic 6)

(gen-pass-dice-periodic 4)
