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

(define-constant +bg-battle-1+   "bg-battle-1.ogg"   :test #'string=)

(define-constant +boom-1+        "explosion-1.ogg"   :test #'string=)

(define-constant +boom-2+        "explosion-3.ogg"   :test #'string=)

(define-constant +boom-3+        "explosion-3.ogg"   :test #'string=)

(define-constant +teleport+      "teleport.ogg"      :test #'string=)

(define-constant +heal-1+        "heal-1.ogg"        :test #'string=)

(define-constant +heal-2+        "heal-1.ogg"        :test #'string=)

(define-constant +heal-3+        "heal-1.ogg"        :test #'string=)

(define-constant +fireball-1+    "fireball-1.ogg"    :test #'string=)

(define-constant +fireball-2+    "fireball-1.ogg"    :test #'string=)

(define-constant +fireball-3+    "fireball-1.ogg"    :test #'string=)

(define-constant +firebolt-1+    "fireball-1.ogg"    :test #'string=)

(define-constant +firebolt-2+    "fireball-1.ogg"    :test #'string=)

(define-constant +firebolt-3+    "fireball-1.ogg"    :test #'string=)

(define-constant +generic-spell+ "generic-spell.ogg" :test #'string=)

(define-constant +level-up+      "level-up.ogg"      :test #'string=)

(define-constant +max-volume+  255                   :test #'=)

(define-constant +min-volume+    0                   :test #'=)

(defclass file-res ()
  ((file
    :initform nil
    :initarg  :file
    :accessor file)))

(defgeneric identifier (object))

(defmethod identifier ((object file-res))
  (file object))

(defclass music-resource (file-res)
  ((mix-obj
    :initform nil
    :initarg  :mix-obj
    :accessor mix-obj)))

(defclass fx-resource (file-res)
  ((chunk
    :initform nil
    :initarg  :chunk
    :accessor chunk)))

(defparameter *musics-db* '())

(defparameter *fx-db* '())

(defparameter *current-volume* 128)

(defun get-volume ()
  *current-volume*)

(defun increase-volume (&key (delta 10))
  (setf *current-volume*
        (min (+ *current-volume* delta)
             +max-volume+))
  (set-volume))

(defun decrease-volume (&key (delta 10))
  (setf *current-volume*
        (max (- *current-volume* delta)
             +min-volume+))
  (set-volume))

(defun set-volume (&optional (vol *current-volume*))
  (let ((new-volume (clamp vol +min-volume+ +max-volume+)))
    (sdl2-mixer:volume-music new-volume)
    (sdl2-mixer:volume -1 (truncate (* 1.5 *current-volume*)))
    new-volume))

(defun init-sound-system (volume)
  (sdl2-mixer:init :ogg)
  (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
  (setf *current-volume* volume)
  (sdl2-mixer:allocate-channels 2)
  (set-volume)
  (init-music-db)
  (init-fx-db))

(defun init-music-db ()
  (add-music +bg-battle-1+))

(defun init-fx-db ()
  (add-fx +generic-spell+)
  (add-fx +level-up+)
  (add-fx +fireball-1+)
  (add-fx +fireball-2+)
  (add-fx +fireball-3+)
  (add-fx +firebolt-1+)
  (add-fx +firebolt-2+)
  (add-fx +firebolt-3+)
  (add-fx +boom-1+)
  (add-fx +boom-2+)
  (add-fx +boom-3+)
  (add-fx +teleport+)
  (add-fx +heal-1+)
  (add-fx +heal-2+)
  (add-fx +heal-3+))

(defun close-sound-system ()
  (sdl2-mixer:halt-music)
  (sdl2-mixer:close-audio)
  (map nil #'(lambda (a) (free-music a)) *musics-db*)
  (map nil #'(lambda (a) (free-fx    a)) *fx-db*)
  (setf *musics-db* nil)
  (setf *fx-db* nil)
  (sdl2-mixer:quit))

(defun free-music (s)
  (sdl2-mixer:free-music (mix-obj s)))

(defun free-fx (s)
  (sdl2-mixer:free-chunk (chunk s)))

(defun find-fn (key)
  #'(lambda (a) (string= (identifier a) key)))

(defun %find (key db)
  (find-if (find-fn key) db))

(defun get-music (key)
  (%find key *musics-db*))

(defun get-fx (key)
  (%find key *fx-db*))

(defun remove-music (key)
  (when-let ((music (get-music key)))
    (free-music music)
    (setf *musics-db*
          (delete-if (find-fn key) *musics-db*))))

(defun remove-fx (key)
  (when-let ((fx (get-fx key)))
    (free-fx fx)
    (setf *fx-db*
          (delete-if (find-fn key) *fx-db*))))

(defun add-music (key)
  (remove-music key)
  (let* ((file     (res:get-resource-file key +music-resource+))
         (mix-obj  (sdl2-mixer:load-music file))
         (db-entry (make-instance 'music-resource
                                  :file    key
                                  :mix-obj mix-obj)))
    (push db-entry *musics-db*)))

(defun add-fx (key)
  (remove-fx key)
  (let* ((file     (res:get-resource-file key +music-resource+))
         (chunk  (sdl2-mixer:load-wav file))
         (db-entry (make-instance 'fx-resource
                                  :file    key
                                  :chunk   chunk)))
    (push db-entry *fx-db*)))

(defun play-music (key &key (loops t))
  (sdl2-mixer:play-music (mix-obj (get-music key)) (if loops -1 1)))

(defun play-fx (key &key (loops 0))
  (sdl2-mixer:play-channel -1 (chunk (get-fx key)) loops))
