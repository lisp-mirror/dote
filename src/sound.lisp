;; Dawn of the era: a tactical game.
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

(in-package :sound)

(define-constant +bg-battle-1+ "bg-battle-1.ogg" :test #'string=)

(define-constant +max-volume+  255               :test #'=)

(define-constant +min-volume+    0               :test #'=)

(defclass music-resource ()
  ((file
    :initform nil
    :initarg  :file
    :accessor file)
   (mix-obj
    :initform nil
    :initarg  :mix-obj
    :accessor mix-obj)))

(defgeneric identifier (object))

(defmethod identifier ((object music-resource))
  (file object))

(defparameter *musics-db* '())

(defparameter *music-channel* '())

(defparameter *current-volume* 128)

(defun get-volume ()
  *current-volume*)

(defun increase-volume (&key (delta 10))
  (setf *current-volume*
        (min (+ *current-volume* delta)
             +max-volume+))
  (misc:dbg "v ~a" *current-volume*)
  (set-volume))

(defun decrease-volume (&key (delta 10))
  (setf *current-volume*
        (max (- *current-volume* delta)
             +min-volume+))
  (set-volume))

(defun set-volume (&optional (vol *current-volume*))
  (let ((new-volume (clamp vol +min-volume+ +max-volume+)))
    (sdl2-mixer:volume-music new-volume)
    new-volume))

(defun init-sound-system (volume)
  (sdl2-mixer:init :ogg)
  (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
  (setf *current-volume* volume)
  (set-volume)
  (init-music-db))

(defun init-music-db ()
  (add-music +bg-battle-1+))

(defun close-sound-system ()
  (setf *musics-db* nil)
  (sdl2-mixer:halt-music)
  (sdl2-mixer:close-audio)
  (map nil #'(lambda (a) (free-music a))
       *musics-db*)
  (sdl2-mixer:quit))

(defun free-music (s)
  (sdl2-mixer:free-chunk (mix-obj s)))

(defun get-music (key)
  (find-if #'(lambda (a) (string= (identifier a) key))
           *musics-db*))

(defun remove-music (key)
  (when-let ((music (get-music key)))
    (free-music music)
    (setf *musics-db*
        (delete-if #'(lambda (a) (string= (identifier a) key))
                   *musics-db*))))

(defun add-music (key)
  (remove-music key)
  (let* ((file     (res:get-resource-file key +music-resource+))
         (mix-obj  (sdl2-mixer:load-music file))
         (db-entry (make-instance 'music-resource
                                  :file    key
                                  :mix-obj mix-obj)))
    (push db-entry *musics-db*)))

(defun play-music (key &key (loops t))
  (sdl2-mixer:play-music (mix-obj (get-music key)) (if loops -1 1)))