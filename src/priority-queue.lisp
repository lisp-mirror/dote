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

(in-package :priority-queue)

(defclass priority-queue ()
  ((heap
    :initform (misc:make-array-frame 1 nil)
    :initarg :heap
    :accessor heap)
   (key-function
    :initform #'identity
    :initarg :key-function
    :accessor key-function)
   (compare-function
    :initform #'<
    :initarg :compare-function
    :accessor compare-function)
   (equal-function
    :initform (misc:make-array-frame 1 nil)
    :initarg :equal-function
    :accessor equal-function)))

(defmethod marshal:class-persistant-slots ((object priority-queue))
  (append '(heap)
           (call-next-method)))

(defgeneric get-children-pos (object parent-pos))

(defgeneric rearrange-bottom-up (object &optional pos))

(defgeneric rearrange-top-bottom (object &optional root-pos))

(defgeneric push-element (object val))

(defgeneric pop-element (object))

(defgeneric emptyp (object))

(defgeneric find-element (object element &key key-fn test-fn))

(defgeneric remove-element (object element))

(defun get-parent-pos (pos)
  (floor (/ pos 2)))

(defmethod get-children-pos ((object priority-queue) parent-pos)
  (declare (integer parent-pos))
  (with-accessors ((heap heap)) object
    (list (and (< (* 2 parent-pos) (fill-pointer heap))
               (* 2 parent-pos))
          (and (< (1+ (* 2 parent-pos)) (fill-pointer heap))
               (1+ (* 2 parent-pos))))))

(defmethod rearrange-bottom-up ((object priority-queue)
                                &optional (pos (1- (length (heap object)))))
  (with-accessors ((heap heap)
                   (key-function key-function)
                   (compare-function compare-function)) object
    (let ((parent-pos (get-parent-pos pos)))
      (when (and (> parent-pos 0)
                 (funcall compare-function
                          (funcall key-function (elt heap pos))
                          (funcall key-function (elt heap parent-pos))))
        (let ((swp (elt heap parent-pos)))
          (setf (elt heap parent-pos) (elt heap pos))
          (setf (elt heap pos) swp))
        (rearrange-bottom-up object parent-pos)))))

(defmethod rearrange-top-bottom ((object priority-queue) &optional (root-pos 1))
  (with-accessors ((heap heap)
                   (key-function key-function)
                   (compare-function compare-function)
                   (equal-function equal-function)) object
    (let* ((children (remove-if #'null (get-children-pos object root-pos)))
           (maximum-child (cond
                            ((null children)
                             root-pos)
                            ((= (length children) 1)
                             (first children))
                            (t
                             (if (funcall compare-function
                                          (funcall key-function (elt heap (first children)))
                                          (funcall key-function (elt heap (second children))))
                                 (first children)
                                 (second children))))))
      (when (not (funcall equal-function
                          (funcall key-function (elt heap maximum-child))
                          (funcall key-function (elt heap root-pos))))
        (let ((swp (elt heap root-pos)))
          (when (funcall compare-function
                         (funcall key-function (elt heap maximum-child))
                         (funcall key-function swp))
            (setf (elt heap root-pos) (elt heap maximum-child))
            (setf (elt heap maximum-child) swp)))
        (rearrange-top-bottom object maximum-child)))))

(defmethod push-element ((object priority-queue) val)
  (with-accessors ((heap heap)) object
    (vector-push-extend val heap)
    (rearrange-bottom-up object)))

(defmethod emptyp ((object priority-queue))
  (with-accessors ((heap heap)) object
    (<= (length heap) 1)))

(defmethod pop-element ((object priority-queue))
  (with-accessors ((heap heap)) object
    (if (emptyp object)
        nil
        (prog1
            (elt heap 1)
          (if (= (length heap) 2)
              (setf (fill-pointer heap) (1- (fill-pointer heap)))
              (progn
                (setf (elt heap 1) (alexandria:last-elt heap))
                (setf (fill-pointer heap) (1- (fill-pointer heap)))
                (rearrange-top-bottom object)))))))

(defmacro with-min-queue ((queue compare sort key) &body body)
  `(let ((,queue (make-instance 'priority-queue
                                :equal-function   ,compare
                                :compare-function ,sort
                                :key-function     ,key)))
     ,@body))

(defmethod find-element ((object priority-queue) element
                         &key (key-fn  (key-function object))
                              (test-fn (equal-function object)))
  (find element
        (heap  object)
        :key   key-fn
        :test  test-fn
        :start 1))

(defmethod remove-element ((object priority-queue) element)
  (with-accessors ((heap heap)
                   (key-function key-function)
                   (equal-function equal-function)
                   (compare-function compare-function)) object
      (let ((pos        (position element
                                  heap
                                  :start 1
                                  :key   (key-function object)
                                  :test  (equal-function object)))
            (old-length (length heap)))
        (if (and pos
                 (= pos 1))
            (pop-element object)
            (when pos
              (misc:swap (elt heap pos)
                         (elt heap  (1- (length heap))))
              (setf (fill-pointer heap) (1- (fill-pointer heap)))

              (when (not (= pos (1- old-length)))
                (let ((parent-pos (get-parent-pos pos)))
                  (if (funcall compare-function (elt heap pos) (elt heap parent-pos))
                      (rearrange-bottom-up  object pos)
                      (rearrange-top-bottom object pos)))))))))
