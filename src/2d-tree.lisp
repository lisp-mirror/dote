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

(in-package :2d-tree)

(define-constant +2d-tree-initial-heading+ (vec2 0.0 1.0) :test #'vec2=)

(defclass 2d-tree (turtle)
  ((angle
    :initform 45.5
    :initarg  angle
    :accessor angle)
   (tropism-dir
    :initform (vec2 -0.51 0.40)
    :initarg  :tropism-dir
    :accessor tropism-dir)
   (tropism-scale
    :initform 0.0
    :initarg  :tropism-scale
    :accessor tropism-scale)
   (branch-length
    :initform 0.095
    :initarg  :branch-length
    :accessor branch-length)
   (branch-width
    :initform 0.07
    :initarg  :branch-width
    :accessor branch-width)))

(defmethod initialize-instance :after ((object 2d-tree) &key &allow-other-keys)
  (setf (heading object) +2d-tree-initial-heading+))

(defmethod print-object ((object turtle) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "pos ~a head ~a w ~a"
            (turtle-pos object) (heading object)
            (branch-width object))))

(defmethod clone ((object 2d-tree))
  (with-simple-clone (object '2d-tree)))

(defmethod clone-into :after ((from 2d-tree) (to 2d-tree))
  (setf (angle         to) (angle                  from)
        (tropism-dir   to) (copy-vec2 (tropism-dir from))
        (tropism-scale to) (tropism-scale          from)
        (branch-length to) (branch-length          from)
        (branch-width to)  (branch-width           from))
  to)

(defgeneric rot-ccw (object)
  (:method ((object 2d-tree))
    (with-accessors ((heading heading)
                     (angle   angle)) object
      (setf heading (vec2-normalize (vec2-rotate heading (deg->rad angle))))
      object)))

(defgeneric forward (object)
  (:method ((object 2d-tree))
    (with-accessors ((heading       heading)
                     (branch-length branch-length)
                     (tropism-scale tropism-scale)
                     (tropism-dir   tropism-dir)
                     (turtle-pos    turtle-pos)) object
      (let* ((angle-tropism (d* (min tropism-scale 1.0)
                                (vec2-perp-dot-product (vec2-normalize heading)
                                                       tropism-dir))))
        (setf heading (vec2-rotate heading angle-tropism))
        (setf turtle-pos (map 'vec2 #'fract
                              (vec2+ turtle-pos
                                     (vec2-rotate (vec2* heading branch-length)
                                                  angle-tropism))))
        object))))

(defmacro with-paths ((all-paths path start-path end-path
                                 &optional (outer-clause :do) (inner-clause :do))
                      &body body)
  (with-gensyms (ct)
    `(loop for ,path in ,all-paths ,outer-clause
          (loop
             for ,start-path in ,path
             for ,ct from 0 by 1 ,inner-clause
               (when (< (1+ ,ct) (length ,path))
                 (let ((,end-path (elt ,path (1+ ,ct))))
                   ,@body))))))

(defun to-pixmap (paths size type
                  &key
                    (num-sand-grains 10000)
                    (divisions          20.0)
                    (bg                (vec4 0.0 0.0 0.0 0.0))
                    (perturbation-clsr #'default-nodes-perturbation))
  (case type
    (:lines
     (to-pixmap-lines paths size bg))
    (:splines
     (to-pixmap-splines paths size bg divisions num-sand-grains perturbation-clsr))
    (:debug
     (to-pixmap-debug paths size bg))
    (:wedges
     (to-pixmap-wedges paths size bg))
    (otherwise
     (to-pixmap-lines paths size bg))))

(defun to-pixmap-lines (paths size bg)
  (let* ((pixmap (make-pixmap-frame size size 4 (vec4->ubvec4 bg))))
    (with-paths (paths path start end :do :do)
      (with-accessors ((start-pos turtle-pos)
                       (turtle-color turtle-color)) start
        (with-accessors ((end-pos turtle-pos)
                         (branch-width branch-width)) end
          (matrix-line-norm pixmap start-pos end-pos turtle-color
                            :antialiasp t
                            :width (max 1 (truncate (* branch-width size)))))))
    pixmap))

(defun denormalize-paths (paths size)
  (loop for path in paths collect
       (misc:list->array (loop for p in path collect
                              (progn
                                (setf (turtle-pos p)
                                      (vec2* (turtle-pos p) (d size)))
                                p)))))

(defun build-separation (a m parx pary width inc divisions)
  (let* ((x   (vec-x a))
         (y   (vec-y a))
         (res (cond
                (pary
                 (let* ((lx (- x (/ width 2))))
                   (loop
                      repeat (truncate divisions)
                      for i from lx by inc collect
                        (vec2 (d i) y))))
                (parx
                 (let* ((ly (- y (/ width 2))))
                   (loop
                      repeat (truncate divisions)
                      for i from ly by inc collect
                        (vec2 x (d i)))))
                (t
                 (let* ((m1 (- (/ 1 m)))
                        (q1 (- (vec-y a) (* m1 (vec-x a))))
                        (lx (- x (/ width 2))))
                   (loop
                      repeat (truncate divisions)
                      for i from lx by inc collect
                        (vec2 (d i) (d (d+ (* m1 x) q1)))))))))
    (misc:list->array res)))

(defun build-skeleton-3 (path path-idx pixmap-size divisions bag)
  "A----B----C"
  (let* ((ta         (elt path    path-idx))
         (tb         (elt path (+ path-idx 1)))
         (tc         (elt path (+ path-idx 2)))
         (wa         (* pixmap-size (branch-width ta)))
         (wb         (* pixmap-size (branch-width tb)))
         (inca       (/ wa divisions))
         (incb       (/ wb divisions))
         (a          (turtle-pos ta))
         (b          (turtle-pos tb))
         (c          (turtle-pos tc))
         (line-a->b  (line-eqn a b)) ;; m q - |
         (line-b->c  (line-eqn b c)) ;; m q - |
         (m-a->b     (elt line-a->b 0))
         ;; (q-a->b     (elt line-a->b 1))
         (par-x-a->b (elt line-a->b 2))
         (par-y-a->b (elt line-a->b 3))
         (m-b->c     (elt line-b->c 0))
         ;; (q-b->c     (elt line-b->c 1))
         (par-x-b->c (elt line-b->c 2))
         (par-y-b->c (elt line-b->c 3)))
    (vector-push-extend (build-separation a m-a->b
                                          par-x-a->b
                                          par-y-a->b
                                          wa inca divisions)
                        bag)
    (vector-push-extend (build-separation b m-b->c
                                          par-x-b->c
                                          par-y-b->c
                                          wb incb divisions)
                        bag)))

(defun build-skeleton (paths pixmap-size divisions)
  (let ((res (make-array-frame 0 t t nil)))
    (loop for path in paths when (> (length path) 3) do
         (let ((single-path (make-array-frame 0 t t nil)))
           (loop for i from 0 below (length path) by 2 do
                (cond
                  ((>= (- (length path) (+ i 2)) 1)
                   (build-skeleton-3 path i pixmap-size divisions single-path))))
           (vector-push-extend single-path res)))
    res))

(defun disentangle-skeleton (skel)
  (let ((new-paths         (make-array-frame 0 t t nil)))
    (loop for path across skel do
         (let ((length-wedge (length (first-elt path))))
           (loop for j from 0 below length-wedge do
                (let ((new-path (make-array-frame 0 +vec2-zero+ 'vec2 nil)))
                  (loop for i from 0 below (length path) do
                       (let* ((v2 (elt (elt path i) j))
                              (v3 (vec (vec2-x v2) (vec2-y v2) 0.0)))
                         (vector-push-extend v3 new-path)))
                  (vector-push-extend new-path new-paths)))))
    new-paths))

(defun perturbate-node-gaussian (a b pos-in-path path perturbation-value)
  (with-line-eqn ((a b) m q parx pary)
    (declare (ignore q))
      (cond
        (pary
         (let ((offset (gaussian-probability perturbation-value (vec-x a))))
           (setf (elt path pos-in-path)
                 (vec offset (vec-y a) 0.0))))
        (parx
         (let ((offset (gaussian-probability perturbation-value (vec-y a))))
           (setf (elt path pos-in-path)
                 (vec (vec-x a) offset 0.0))))
        (t
         (let* ((offset (gaussian-probability perturbation-value (vec-x a)))
                (m1    (- (/ 1 m)))
                (q1    (- (vec-y a) (* m1 (vec-x a))))
                (new-x offset)
                (new-y (d (+ (* m1 new-x) q1))))
           (setf (elt path pos-in-path)
                 (vec new-x new-y 0.0)))))))

(defun perturbate-node (a b pos-in-path path perturbation-value)
  (with-line-eqn ((a b) m q parx pary)
    (declare (ignore q))
    (let* ((offset      (lcg-next-upto (d perturbation-value)))
           (offset-neg  (d- offset))
           (rand-offset (lcg-next-in-range offset-neg offset)))
      (cond
        (pary
         (setf (elt path pos-in-path)
               (vec+ (elt path pos-in-path)
                     (vec rand-offset 0.0 0.0))))
        (parx
         (setf (elt path pos-in-path)
               (vec+ (elt path pos-in-path)
                     (vec 0.0 rand-offset 0.0))))
        (t
         (let* ((m1    (- (/ 1 m)))
                (q1    (- (vec-y a) (* m1 (vec-x a))))
                (new-x (+ rand-offset (vec-x a)))
                (new-y (d (+ (* m1 new-x) q1))))
           (setf (elt path pos-in-path)
                 (vec new-x new-y 0.0))))))))

(defun default-nodes-perturbation (pixmap)
  (let ((size         (width pixmap))
        (offset-low   233.0)
        (offset-hight 333.0)
        (offset-0      26.7))
    (flet ((sparse-tree-p (a)
             (and (<= (vec-x a) (d (* size 1/3)))
                  (<= (vec-y a) (d (* size 1/3))))))
    #'(lambda (path)
        (loop for i from 0 below (length path) do
             (let ((a (elt path i)))
               (cond
                 ((= i 0)
                  (perturbate-node-gaussian a (elt path (1+ i)) i path (/ size offset-0)))
                 ((= i (1- (length path)))
                  (if (sparse-tree-p a)
                      (perturbate-node-gaussian a (elt path (- (length path) 2))  i path
                                                (d* (/ size offset-hight) (d i)))
                      (perturbate-node-gaussian a (elt path (- (length path) 2))  i path
                                                (/ size (* 4 offset-low)))))
                 (t
                  (cond
                    ((sparse-tree-p a)
                     (perturbate-node-gaussian a (elt path (1+ i)) i path
                                               (d* (/ size offset-hight) (d i))))
                    (t
                     (perturbate-node-gaussian a (elt path (1+ i)) i path
                                               (/ size offset-low))))))))
        path))))

(defun vec->ivec2 (v)
  (let ((x (vec-x v))
        (y (vec-y v)))
    (ivec2 (round x) (round y))))

;; TODO build skeleton in normalized coordinates
(defun to-pixmap-splines (paths size bg divisions num-sand-grains perturbation-clsr)
  (let* ((pixmap          (make-pixmap-frame size size 4 (vec4->ubvec4 bg)))
         (perturbation-fn (funcall perturbation-clsr pixmap))
         (paths           (denormalize-paths paths size))
         (skeleton        (build-skeleton paths size (d divisions)))
         (dis-paths       (disentangle-skeleton skeleton)) ; 3d vec
         ;; (sparse-paths    (map 'vector #'(lambda (a) (scale-too-near-nodes a 1.0))
         ;;                        dis-paths))
         (max-path-length (length (find-min-max #'(lambda (a b) (> (length a) (length b)))
                                                dis-paths)))
         (actual-paths    (map 'vector perturbation-fn
                               (remove-if #'(lambda (a) (< (length a) max-path-length))
                                          dis-paths)))
         (splines   (loop for path across actual-paths collect
                         (interpolation:catmul-roll-interpolation* (seq->list path)))))
    (loop
       for ct from 0 by 1
       for path across actual-paths
       for spline in splines do
         #+debug-mode
         (misc:dbg "~a/~a ~a%"
                   ct (length splines)
                   (floor (* 100.0 (/ ct (length splines)))))
         (loop repeat num-sand-grains  do
              (let* ((depth       (lcg-next-upto (d (- max-path-length 1))))
                     (res         (vec->ivec2 (cm-interpol->vec (funcall spline depth))))
                     (saved-pixel (and (matrix:pixel-inside-p pixmap
                                                              (ivec2-x res)
                                                              (ivec2-y res))
                                       (pixel@ pixmap (ivec2-x res) (ivec2-y res)))))
                (when saved-pixel
                  (setf (pixel@ pixmap (ivec2-x res) (ivec2-y res))
                        (vec4->ubvec4 (map 'vec4 #'(lambda (a) (clamp a 0.0 1.0))
                                           (vec4+ (ubvec4->vec4 saved-pixel)
                                                  (vec4  1.0 0.01 0.005 0.005)))))))))
    pixmap))

(defun to-pixmap-splines-ref (paths size bg)
  (let* ((pixmap (make-pixmap-frame size size 4 (vec4->ubvec4 bg)))
         (spline-paths (loop for path in paths collect
                            (loop for p in path collect
                                 (let ((scaled (vec2* (turtle-pos p) (d size))))
                                   (vec (vec2-x scaled) (vec2-y scaled) 0.0))))))
    (loop repeat 5 do
         (let* ((perturbed (loop for path in spline-paths collect
                                (loop for p in path collect
                                     (if (and (< (vec-x p) (d (* size 3/8)))
                                              (> (vec-y p) (d (* size 1/2))))
                                         (vec+ p (vec (num:lcg-next-upto 10.0)
                                                      (num:lcg-next-upto 10.0)
                                                      0.0))
                                         (vec+ p (vec (num:lcg-next-upto 2.0)
                                                      (num:lcg-next-upto 2.0)
                                                      0.0))))))
                (splines      (loop for knots in perturbed collect
                                   (interpolation:catmul-roll-interpolation* knots))))
           (loop for spline in splines do
                (loop for i from 0.0 below 10.0 by 0.01 do
                     (let ((res (funcall spline i)))
                       (setf (pixel@ pixmap
                                     (truncate (matrix:matrix-elt res 0 0))
                                     (truncate (matrix:matrix-elt res 1 0)))
                             (ubvec4 0 0 0 255)))))))
      pixmap))

(defun to-pixmap-wedges (paths size bg &key (divisions 10.0))
  (let* ((pixmap    (make-pixmap-frame size size 4 (vec4->ubvec4 bg)))
         (paths     (denormalize-paths paths size))
         (skeleton  (build-skeleton paths size divisions))
         (dis-paths (disentangle-skeleton skeleton)))
    (loop for path across dis-paths  do
         (loop for i from 0 below (1- (length path)) do
              (misc:dbg "~a -> ~a"
                        (ivec2:sequence->ivec2 (elt path i) :trunc-fn #'truncate)
                        (ivec2:sequence->ivec2 (elt path (1+ i)) :trunc-fn #'truncate))
              (matrix-line pixmap
                           (ivec2:sequence->ivec2 (elt path i) :trunc-fn #'truncate)
                           (ivec2:sequence->ivec2 (elt path (1+ i)) :trunc-fn #'truncate)
                           (ubvec4 0 0 0 255)
                           :antialiasp t
                           :width      1)))
    pixmap))

(defun to-pixmap-debug (paths size bg)
  (let* ((pixmap   (make-pixmap-frame size size 4 (vec4->ubvec4 bg)))
         (paths    (denormalize-paths paths size)))
    (loop for path in paths  do
         (loop for i from 0 below (1- (length path)) do
              (matrix-line pixmap
                           (ivec2:sequence->ivec2 (turtle-pos (aref path i))
                                                  :trunc-fn #'truncate)
                           (ivec2:sequence->ivec2 (turtle-pos (aref path (1+ i)))
                                                  :trunc-fn #'truncate)
                           (ubvec4 0 0 0 255)
                           :antialiasp t
                           :width      1)))
    pixmap))

(defparameter *max-depth* 40)

(defparameter *tree* nil)

(defparameter *tree-paths* '())

(defparameter *path* '())

(defmacro with-tree (&body body)
  `(stack:with-stack (#'eq #'identity)
     (let* ((*tree-paths* '())
            (*tree*        (make-instance '2d-tree)))
       ,@body)))

(defmacro tpush (&body body)
  `(progn
     (stack:push (clone *tree*))
     (prog1
         ,@body
       (setf *tree* (stack:pop)))))

(defun fwd (path depth)
  (declare (ignore path depth))
  (forward *tree*)
  (list (clone *tree*)))

(defun rot (path depth)
  (declare (ignore path depth))
  (rot-ccw *tree*)
  nil)

(defun r+ (path depth)
  (rot path depth))

(defun r- (path depth)
  (setf (angle *tree*) (d- (angle *tree*)))
  (rot path depth))

(defun a! (a)
  #'(lambda (path depth)
      (declare (ignore path depth))
      (setf (angle *tree*) a)
      nil))

(defun a% (a)
  #'(lambda (path depth)
      (declare (ignore path depth))
      (setf (angle *tree*) (d* a (angle *tree*)))
      nil))

(defun l% (a)
  #'(lambda (path depth)
      (declare (ignore path depth))
      (setf (branch-length *tree*) (d* a (branch-length *tree*)))
      nil))

(defun t+ (a)
  #'(lambda (path depth)
      (declare (ignore path depth))
      (incf (tropism-scale *tree*) a)
      nil))

(defun t% (a)
  #'(lambda (path depth)
      (declare (ignore path depth))
      (setf (tropism-scale *tree*) (d* a (tropism-scale *tree*)))
      nil))

(defun w% (a)
  #'(lambda (path depth)
      (declare (ignore path depth))
      (setf (branch-width *tree*) (d* a (branch-width *tree*)))
      nil))

(defmacro p (&body body)
  `(list ,@body))

(defun clone-path (p)
  (mapcar #'clone p))

(defun exec (gram path depth)
  (cond
    ((> depth *max-depth*)
     (push (reverse path) *tree-paths*))
    ((null gram)
     (push (reverse path) *tree-paths*))
    ((symbolp (first gram))
     (let ((new-points (funcall (symbol-function (first gram)) path depth)))
       (exec (rest gram) (append new-points path) depth)))
    ((functionp (first gram))
     (let ((new-gram (funcall (first gram) path depth)))
       (exec (append new-gram (rest gram)) path depth)))
    (t
     (tpush
       (exec (first gram) (clone-path path) (1+ depth)))
     (exec (rest gram)   (clone-path path) depth))))

(defun root (path depth)
  (declare (ignore path depth))
  (list 'fwd (l% 0.50) 'fwd (l% 2.0) #'branch))

(defun branch (path depth)
  (declare (ignore path depth))
  (list 'fwd
        #'(lambda (path depth)
            (declare (ignore path))
            (when (< 2 depth 20)
              (incf (tropism-scale *tree*) 0.2))
            nil)
        (w% 0.65)
        (p 'r+ (l% 0.90) #'branch)
        (a! 10.0) 'r+
        (w% 0.8)
        (p (a! 28.0) 'r- (l% 0.85) 'fwd #'branch)
        (w% 0.8)
        (a! 10.0) 'r-
        (p (a! 28.0) 'r+ (l% 0.85) 'fwd #'branch)))

(defun make-spline-tree (&optional (divisions 8) (depth 6) (num-sand-grains 500)
                           (size  +default-size-pixmap-library+)
                           (blit-to-black-pixmap t))
  (let ((*max-depth* depth))
    (with-tree
      (exec (list #'root) (list (clone *tree*)) 0)
      (loop for path in *tree-paths* do
           (loop for p in path do
                (setf (turtle-pos p)
                      (vec2+  (turtle-pos p) (vec2 0.55 0.0)))))
      (let* ((pixmap-tree (to-pixmap (reverse *tree-paths*)
                                     size
                                     :splines
                                     :divisions       divisions
                                     :num-sand-grains num-sand-grains)))
        (if blit-to-black-pixmap
            (let ((pixmap-out (make-pixmap-frame (width pixmap-tree)
                                                 (height pixmap-tree)
                                                 4
                                                 (ivec4 0 0 0 255))))
              (with-messages-start-end ("start blitting tree"
                                        "blitting terminated")
                (pixmap:blit pixmap-tree pixmap-out 0 0 0 0
                             :function-blend (pixmap:blit-blend-lerp-fn)))
              (h-mirror-matrix pixmap-out))
            (h-mirror-matrix pixmap-tree))))))
