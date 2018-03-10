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

(in-package :saved-game)

(defclass saved-player ()
  ((mesh-infos
    :initform nil
    :initarg  :mesh-infos
    :accessor mesh-infos
    :type     md2-fs-res)
   (player-ghost
    :initform nil
    :initarg  :player-ghost
    :accessor player
    :type     player-character)
   (original-mesh-id
    :initform nil
    :initarg  :original-mesh-id
    :accessor original-mesh-id
    :type     fixnum)))

(defmethod marshal:class-persistant-slots ((object saved-player))
  '(mesh-infos
    player-ghost
    original-mesh-id))

(defclass saved-game ()
  ((original-map-file
    :initform nil
    :initarg  :original-map-file
    :accessor original-map-file
    :type     string)
   (canceled-tiles
    :initform nil
    :initarg  :canceled-tiles
    :accessor canceled-tiles
    :type     matrix
    :documentation "A  matrix of game-state:map-state-element.   if an
                    element of this  matrix is game-state:+empty-type+
                    and      the     corresponding      element     in
                    game-state:map-state is  not empty,  after loading
                    the level, that element must  be erased and put to
                    game-state:+empty-type+")
   (saved-players
    :initform nil
    :initarg  :saved-players
    :accessor saved-players
    :type     list)))

(defmethod marshal:class-persistant-slots ((object saved-game))
  '(original-map-file
    canceled-tiles
    saved-players))

(define-constant +map-saved-filename+ "map" :test #'string=)

(defun mesh->saved-player (mesh)
  (make-instance 'saved-player
                 :mesh-infos       (fs-resources mesh)
                 :player-ghost     (ghost        mesh)
                 :original-mesh-id (id           mesh)))

(defun save-game (resource-dir game-state)
  (let* ((saved-file        (res:get-resource-file +map-saved-filename+
                                                   resource-dir
                                                   :if-does-not-exists :create))
         (current-map-state (map-state game-state))
         (saved-player      (append (map-ai-entities     game-state #'mesh->saved-player)
                                    (map-player-entities game-state #'mesh->saved-player)))
         (to-save           (make-instance 'saved-game
                                           :saved-players     saved-player
                                           :canceled-tiles    current-map-state
                                           :original-map-file (game-map-file game-state))))
    (fs:dump-sequence-to-file (serialize to-save) saved-file)
    saved-file))
