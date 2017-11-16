(in-package :keyboard-config)

(defparameter *forward*             "w")

(defparameter *back*                "s")

(defparameter *left*                "d")

(defparameter *right*               "a")

(defparameter *upward*              "u")

(defparameter *downward*            "j")

(defparameter *go-to-active-player* "e")

(defparameter *smooth-movements*     t)

(define-constant +slide-velocity-scaling+ 3.0 :test #'=)

(defun slide-camera (world displacement)
  (let* ((camera (camera world)))
    (setf (mode camera)  :drag)
    (camera:drag-camera camera displacement)))

(defun slide-forward (world)
  (slide-camera world (vec 0.0
                           0.0
                           (d* +terrain-chunk-tile-size+
                               +slide-velocity-scaling+))))

(defun slide-back (world)
  (slide-camera world (vec 0.0
                           0.0
                           (d- (d* +terrain-chunk-tile-size+
                                   +slide-velocity-scaling+)))))

(defun slide-left (world)
  (slide-camera world (vec (d* +terrain-chunk-tile-size+
                               +slide-velocity-scaling+)
                           0.0
                           0.0)))

(defun slide-right (world)
  (slide-camera world (vec (d- (d* +terrain-chunk-tile-size+
                                   +slide-velocity-scaling+))
                           0.0
                           0.0)))

(defun slide-upward (world)
  (incf (elt (entity:pos (world:camera world)) 1)
        (d* +terrain-chunk-tile-size+
            +slide-velocity-scaling+)))

(defun slide-downward (world)
  (incf (elt (entity:pos (world:camera world)) 1)
        (d- (d* +terrain-chunk-tile-size+
                +slide-velocity-scaling+))))

(defun slide-to-active-player (world)
  (world:point-camera-to-entity world
                                (widget:bound-player (toolbar world))))
