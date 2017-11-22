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

(defclass game-config ()
  ((forward
    :initform "w"
    :initarg  :forward
    :accessor forward)
   (back
    :initform "s"
    :initarg  :back
    :accessor back)
   (left
    :initform "d"
    :initarg  :left
    :accessor left)
   (right
    :initform "a"
    :initarg :right
    :accessor right)
   (upward
    :initform "u"
    :initarg  :upward
    :accessor upward)
   (downward
    :initform "j"
    :initarg  :downward
    :accessor downward)
   (go-to-active-player
    :initform "e"
    :initarg  :go-to-active-player
    :accessor go-to-active-player)
   (smooth-movements
    :initform t
    :initarg  :smooth-movements
    :accessor smooth-movements)))

(defmethod marshal:class-persistant-slots ((object game-config))
    '(forward
      back
      left
      right
      upward
      downward
      go-to-active-player
      smooth-movements))

(defun make-default-config ()
  (make-instance 'game-config))

(defparameter *game-config* (make-default-config))

(defun init ()
  (res:get-resource-file +game-config-resource+
                         +game-config-filename+
                         :if-does-not-exists :create)
  (let* ((config-file (res:get-resource-file +game-config-resource+
                                             +game-config-filename+
                                             :if-does-not-exists :error))
         (size        (fs:file-length-if-exists config-file)))
    (if (= size 0)
        (fs:dump-sequence-to-file (serialize (make-default-config)) config-file)
        (setf *game-config* (deserialize 'game-config config-file)))))

(defmacro gen-acc-fn (name)
  `(progn
     (defun ,(misc:format-fn-symbol t "config-~a" name) ()
       (,(misc:format-fn-symbol t "~a" name) *game-config*))
     (defun ,(misc:format-fn-symbol t "set-~a" name) (val)
       ,(with-gensyms (old)
          `(let ((,old (,(misc:format-fn-symbol t "config-~a" name))))
             (setf (,(misc:format-fn-symbol t "~a" name) *game-config*) val)
             ,old)))))

(gen-acc-fn forward)

(gen-acc-fn back)

(gen-acc-fn left)

(gen-acc-fn right)

(gen-acc-fn upward)

(gen-acc-fn downward)

(gen-acc-fn go-to-active-player)

(gen-acc-fn smooth-movements)
