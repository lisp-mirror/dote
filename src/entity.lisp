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

(in-package :entity)

(defclass entity (identificable)
  ((modified
    :accessor modified
    :initform nil)
   (pos
    :accessor pos
    :initarg :pos
    :initform (vec 0.0 0.0 0.0))
   (dir
    :accessor dir
    :initarg :dir
    :initform +entity-forward-direction+)
   (scaling
    :accessor scaling
    :initarg :scaling
    :initform (vec 1.0 1.0 1.0))
   (up
    :accessor up
    :initarg  :up
    :initform +entity-up-direction+)
   (ghost
    :accessor ghost
    :initarg  :ghost
    :initform nil
    :type (or character:np-character character:player-character))
   (attacked-by-entity
    :accessor attacked-by-entity
    :initarg  :attacked-by-entity
    :initform nil
    :type     entity)
   (reply-attack
    :writer   (setf reply-attack)
    :reader   reply-attack-p
    :initarg  :reply-attack-p
    :initform nil
    :type     entity)
   (state
    :accessor state
    :initarg :state
    :initform nil
    :allocation :class)))

(defmethod clone-into :after ((from entity) (to entity))
  (setf (modified to) (modified from)
        (pos to)      (copy-vec (pos from))
        (dir to)      (copy-vec (dir from))
        (scaling to)  (copy-vec (scaling from))
        (up to)       (copy-vec (up from))
        (state to)    (state    from))
  to)

(defmethod marshal:class-persistant-slots ((object entity))
  (append '(modified
            pos
            dir
            scaling
            up)
          (call-next-method)))

(defgeneric side-dir (object))

(defgeneric side-signed-dist (object target))

(defgeneric signed-dist (object target))

(defgeneric aabb-2d (object))

(defgeneric find-entity-by-id (object id))

(defgeneric remove-entity-by-id (object id))

(defgeneric remove-entity-if (object predicate))

(defgeneric entity-dead-p (object))

(defgeneric popup-from-fow (object &key &allow-other-keys))

(defgeneric throw-down-in-fow (object &key &allow-other-keys))

(defgeneric thrown-down-in-fow-p (object &key &allow-other-keys))

(defmethod popup-from-fow ((object (eql nil)) &key &allow-other-keys)
  ;; nothing to do
  )

(defmethod throw-down-in-fow ((object (eql nil)) &key &allow-other-keys)
  ;; nothing to do
  )

(defmethod thrown-down-in-fow-p ((object (eql nil)) &key &allow-other-keys)
  nil)

(defmethod entity-dead-p (object)
  (declare (ignore object))
  nil)

(defgeneric calculate-cost-position (object))

(defmethod side-dir ((object entity))
  (with-accessors ((dir dir)) object
    (with-epsilon ((num:d* 10.0 sb-cga:+default-epsilon+))
      (cond
        ((vec~ dir +x-axe+ *default-epsilon*)
         (copy-vec (vec-negate +z-axe+)))
        ((vec~ dir (vec-negate +z-axe+) *default-epsilon*)
         (copy-vec (vec-negate +x-axe+)))
        ((vec~ dir (vec-negate +x-axe+) *default-epsilon*)
         (copy-vec +z-axe+))
        ((vec~ dir +z-axe+ *default-epsilon*)
         (copy-vec +x-axe+))
        (t
         (error "invalid direction"))))))

(defun %project-on-dir (dir pos position-target)
  (let* ((diff        (vec- position-target pos))
         (dot-product (dot-product diff dir)))
    dot-product))

(defmethod side-signed-dist ((object entity) (target entity))
  (with-accessors ((side-dir        side-dir)
                   (position-object pos)) object
    (with-accessors ((position-target pos)) target
      (%project-on-dir side-dir position-object position-target))))

(defmethod signed-dist ((object entity) (target entity))
  (with-accessors ((dir             dir)
                   (position-object pos)) object
    (with-accessors ((position-target pos)) target
      (%project-on-dir dir position-object position-target))))

(defmacro end-of-life-remove-from-world (entity)
  `(game-state:with-world (world (entity:state ,entity))
     (setf (end-of-life-callback ,entity)
           #'(lambda ()
               (entity:remove-entity-by-id world (identificable:id ,entity))))))

(defmacro with-slots-for-reasoning ((mesh state ghost blackboard) &body body)
  `(with-accessors ((,state state)
                    (,ghost ghost)) ,mesh
    (with-accessors ((,blackboard blackboard:blackboard)) ,state
      ,@body)))

(defmacro with-player-cost-pos ((entity x y) &body body)
  (alexandria:with-gensyms (pos)
    `(let* ((,pos (calculate-cost-position ,entity))
            (,x   (elt ,pos 0))
            (,y   (elt ,pos 1)))
       ,@body)))

(defmacro with-player-cost-pos-ivec2 ((entity v) &body body)
  (with-gensyms (x y)
    `(with-player-cost-pos (,entity ,x ,y)
       (let ((,v (ivec2:ivec2 ,x ,y)))
         ,@body))))

(defun init-fow-texture ()
  (texture:with-prepared-texture (tex (res:get-resource-file texture:+fow+
                                                             +animation-texture-dir+))
    tex))

(defun adjust-fow-texture (world heights-matrix)
  (with-accessors ((main-state main-state)) world
    (let* ((size-map-state   (matrix:width (game-state:map-state main-state)))
           (size-world       size-map-state)
           (texture          (init-fow-texture))
           (new-texure-size  size-world)
           (old-texture-size (matrix:width texture))
           (scale            (d (/ new-texure-size old-texture-size)))
           (scaled-heights   (matrix:scale-matrix heights-matrix
                                                  +terrain-chunk-size-scale+
                                                  +terrain-chunk-size-scale+)))
      (pixmap:nscale-pixmap-nearest texture scale scale)
      (pixmap:sync-data-to-bits texture)
      (pixmap:cristallize-bits texture)
      ;; force the substitution of the whole bitmap
      (force-reinitialize texture)
      (prepare-for-rendering texture)
      (assert (= (matrix:width scaled-heights)
                 (matrix:width texture)))
      (assert (= (matrix:width  scaled-heights)
                 (matrix:height scaled-heights)))
      (setf (texture-fow main-state) texture)
      (matrix:loop-matrix (scaled-heights x y)
         (when (< (matrix:matrix-elt scaled-heights y x)
                  +zero-height+)
           (popup-from-fow main-state :x x :y y :update-gpu-texture t)))
      texture)))

(defclass affected-by-fow ()
  ((texture-fow
    :initform   (init-fow-texture)
    :initarg    :texture-fow
    :accessor   texture-fow
    :allocation :class)))
