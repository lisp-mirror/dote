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

(define-constant +forward+                "w" :test #'string=)

(define-constant +back+                   "s" :test #'string=)

(define-constant +left+                   "d" :test #'string=)

(define-constant +right+                  "a" :test #'string=)

(define-constant +upward+                 "u" :test #'string=)

(define-constant +downward+               "j" :test #'string=)

(define-constant +rotate-camera-cw+       "e" :test #'string=)

(define-constant +rotate-camera-ccw+      "q" :test #'string=)

(define-constant +reset-camera+           "r" :test #'string=)

(define-constant +go-to-active-character+ "t" :test #'string=)

(define-constant +next-character+         "x" :test #'string=)

(define-constant +prev-character+         "z" :test #'string=)

(defclass game-config ()
  ((forward
    :initform +forward+
    :initarg  :forward
    :accessor forward)
   (back
    :initform +back+
    :initarg  :back
    :accessor back)
   (left
    :initform +left+
    :initarg  :left
    :accessor left)
   (right
    :initform +right+
    :initarg :right
    :accessor right)
   (upward
    :initform +upward+
    :initarg  :upward
    :accessor upward)
   (downward
    :initform +downward+
    :initarg  :downward
    :accessor downward)
   (rotate-camera-cw
    :initform +rotate-camera-cw+
    :initarg  :rotate-camera-cw
    :accessor rotate-camera-cw)
   (rotate-camera-ccw
    :initform +rotate-camera-ccw+
    :initarg  :rotate-camera-ccw
    :accessor rotate-camera-ccw)
   (reset-camera
    :initform +reset-camera+
    :initarg  :reset-camera
    :accessor reset-camera)
   (go-to-active-character
    :initform +go-to-active-character+
    :initarg  :go-to-active-character
    :accessor go-to-active-character)
   (next-character
    :initform +next-character+
    :initarg  :next-character
    :accessor next-character)
   (prev-character
    :initform +prev-character+
    :initarg  :prev-character
    :accessor prev-character)
   (smooth-movements
    :initform t
    :initarg  :smooth-movements
    :accessor smooth-movements)
   (inhibit-planner
    :initform nil
    :initarg  :inhibit-planner
    :accessor inhibit-planner)
   (camera-fp-scaling-movement
    :initform 2.0
    :initarg  :camera-fp-scaling-movement
    :accessor camera-fp-scaling-movement)
   (train-ai
    :initform nil
    :initarg  :train-ai
    :accessor train-ai)
   (sound-volume
    :initform 68
    :initarg  :sound-volume
    :accessor sound-volume)))

(defmethod marshal:class-persistant-slots ((object game-config))
    '(forward
      back
      left
      right
      upward
      downward
      rotate-camera-cw
      rotate-camera-ccw
      reset-camera
      go-to-active-character
      next-character
      prev-character
      smooth-movements
      inhibit-planner
      camera-fp-scaling-movement
      train-ai
      sound-volume))

(defun make-default-config ()
  (make-instance 'game-config))

(defparameter *game-config* (make-default-config))

(defun get-file-path (&key (create t))
  (when create
    (res:get-resource-file +game-config-filename+
                           +game-config-resource+
                           :if-does-not-exists :create))
  (res:get-resource-file +game-config-filename+
                         +game-config-resource+
                         :if-does-not-exists :error))

(defun dump (&optional (object-config *game-config*))
  (let* ((config-file (get-file-path :create t)))
    (fs:dump-sequence-to-file (serialize object-config) config-file)))

(defun init ()
  (let* ((config-file (get-file-path :create t))
         (size        (fs:file-length-if-exists config-file)))
    (if (= size 0)
        (dump (make-default-config))
        (setf *game-config* (deserialize 'game-config config-file)))))

(defun reset-keybindings ()
   (set-forward                +forward+)
   (set-rotate-camera-cw       +rotate-camera-cw+)
   (set-upward                 +upward+)
   (set-go-to-active-character +go-to-active-character+)
   (set-left                   +left+)
   (set-right                  +right+)
   (set-back                   +back+)
   (set-rotate-camera-ccw      +rotate-camera-ccw+)
   (set-downward               +downward+)
   (set-reset-camera           +reset-camera+)
   (set-next-character         +next-character+)
   (set-prev-character         +prev-character+)
   (dump))

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

(gen-acc-fn rotate-camera-cw)

(gen-acc-fn rotate-camera-ccw)

(gen-acc-fn reset-camera)

(gen-acc-fn go-to-active-character)

(gen-acc-fn next-character)

(gen-acc-fn prev-character)

(gen-acc-fn smooth-movements)

(gen-acc-fn inhibit-planner)

(gen-acc-fn camera-fp-scaling-movement)

(gen-acc-fn train-ai)

(gen-acc-fn sound-volume)
