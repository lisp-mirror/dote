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

(in-package :game-configuration)

(defstruct config
  (forward             "w")
  (back                "s")
  (left                "d")
  (right               "a")
  (upward              "u")
  (downward            "j")
  (go-to-active-player "e")
  (smooth-movements     t))


(defparameter *game-config* (make-config))

(defmacro gen-acc-fn (name)
  `(defun ,(misc:format-fn-symbol t "~a" name) ()
     (,(misc:format-fn-symbol t "config-~a" name) *game-config*)))

(gen-acc-fn forward)

(gen-acc-fn back)

(gen-acc-fn left)

(gen-acc-fn right)

(gen-acc-fn upward)

(gen-acc-fn downward)

(gen-acc-fn go-to-active-player)

(gen-acc-fn smooth-movements)
