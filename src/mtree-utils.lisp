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

(in-package mtree-utils)

;; tree := nil | node
;; node := (list atom node*)
;; example: '(1 (2) (3 (4) (5)))

(defgeneric leafp (object))

(defmethod  leafp ((object cons))
  (null (cdr object)))

(defun random-choose-leaf (tree)
  (if (leafp tree)
      (car   tree)
      (let ((children (cdr tree)))
        (random-choose-leaf (misc:random-elt children)))))

(defun traverse-apply-tree (function tree &optional (args nil))
  (append
   (if (and (consp tree)
            (not (null tree)))
       (reverse
        (append
         (reverse (loop for i in (cdr tree) collect (traverse-apply-tree function i args)))
         (list (apply function (append (list (car tree)) args)))))
       nil)))

(defun traverse-napply-tree (function tree &optional (args nil))
   (when (and (consp tree)
              (not (null tree)))
     (loop for i in (cdr tree) collect (traverse-napply-tree function i args))
     (rplaca tree (apply function (append (list (car tree)) args)))))

(defun traverse-find-if-tree (tree item &key (test #'equal) (key #'identity))
  (progn
    (traverse-apply-tree #'(lambda (x) (if (funcall test item (funcall key x))
                                           (return-from traverse-find-if-tree x)
                                           nil))
                         tree)
    nil))

(defun traverse-find-all-if-tree (tree item &key (test #'equal) (key #'identity))
  (let ((res '()))
    (traverse-apply-tree #'(lambda (x) (if (funcall test item (funcall key x))
                                           (push x res)))
                         tree)
    res))

(defun traverse-apply-tree-cdr (function tree &optional (args nil))
  (append
   (if (and (consp tree)
            (not (null tree)))
       (append
        (list (apply function (append (list tree) args)))
        (loop for i in (cdr tree) by #'cdr collect (traverse-apply-tree-cdr function i args)))
       nil)))

(defun traverse-nadd-child (tree node child &key (test #'equal) (key #'identity))
  (traverse-apply-tree-cdr
   #'(lambda (x) (when (funcall test (funcall key (car x)) node)
                   (progn
                     (rplacd x (append (list (list child)) (cdr x)))
                     (rplaca x (car x)))))
   tree)
  tree)

(defun nappend-child (tree child)
  (rplacd tree (concatenate 'list (cdr tree) (list (list child)))))

(defun traverse-ndelete-child (tree node &key (test #'equal) (key #'identity))
  (traverse-apply-tree-cdr
   #'(lambda (x) (loop
                    for i in (cdr x)
                    for ct = 0 then (1+ ct)
                    do
                      (if (funcall test (funcall key (car i)) node)
                          (rplacd x (misc-utils:safe-delete@ (cdr x) ct)))))
   tree)
  tree)

(defmacro %navigate (tree path)
  (if path
      `(nth ,(first path) (%navigate ,tree ,(rest path)))
      tree))

(defmacro navigate (tree path)
  `(%navigate ,tree ,(reverse path)))

(defun init-children ()
  (misc:make-fresh-array 0 nil t t))

(defclass m-tree ()
  ((data
    :initform nil
    :initarg :data
    :accessor data)
   (parent
    :initform nil
    :initarg :parent
    :accessor parent)
   (children
    :initform (init-children)
    :initarg :children
    :accessor children)))

(defmethod marshal:class-persistant-slots ((object m-tree))
  '(data parent children))

(defgeneric pprint-tree (object stream &optional level parent-length other-data))

(defgeneric add-child (object child &optional child-pos))

(defgeneric add-children (object children))

(defgeneric add-children* (object &rest children))

(defgeneric find-child (object to-find &key compare))

(defgeneric find-child-if (object predicate))

(defgeneric rootp (object))

(defgeneric top-down-visit (object function &optional args))

(defgeneric bottom-up-visit (object function &optional args))

(defgeneric remove-all-children (object))

(defgeneric remove-child (object needle &key key test))

(defgeneric remove-child-if (object predicate))

(defgeneric count-leafs     (object))

(defparameter *use-pprint-tree* nil)

(defmethod print-object ((object m-tree) stream)
    (if *use-pprint-tree*
        (pprint-tree object stream)
        (format stream "[data ~a children ~a]" (data object) (children object))))

(defmethod pprint-tree ((object m-tree) stream &optional (level 0) (parent-length 0)
                        (other-data nil))
  (declare (ignore other-data))
  (labels ((indent (level &optional (char " ")) (make-list level :initial-element char)))
    (with-accessors ((data data) (children children)) object
      (let ((data-length (+
                            (do ((parent (parent object) (parent parent))
                                 (data-length 0))
                                ((not parent) data-length)
                              (incf data-length (length (format nil "~a" (data parent)))))

                            (length (format nil "~a" data)))))

        (format stream "~{~a~}~a" (indent (+ level parent-length))
                data)
        (if (leafp object)
          (format stream "~%")
          (progn
            (pprint-tree (elt children 0) stream 1)
            (map nil #'(lambda (c) (pprint-tree c stream (1+ level) data-length))
                 (subseq children 1))))))))

(defmethod clone ((object m-tree))
  (make-instance 'm-tree :data (data object) :parent (parent object)
                 :children (alexandria:copy-array (children object))))

(defmethod add-child ((object m-tree) (child m-tree)
                      &optional (child-pos (length (children object))))
  (with-accessors ((children children)) object
    (setf (parent child) object)
    (if (and child-pos
             (< child-pos (length children))
             (>= child-pos 0))
        (setf children
              (let ((res (misc:make-fresh-array (1+ (length children))
                                                nil (type-of child) t)))
                (loop for i from 0 below child-pos do
                     (setf (elt res i) (elt children i)))
                (setf (elt res child-pos) child)
                (loop for i from (1+ child-pos) below (length res) do
                     (setf (elt res i) (elt children (1- i))))
                res))
        (setf children
              (let ((res (misc:make-fresh-array (1+ (length children))
                                                nil (type-of child) t)))
                (loop for i from 0 below (1- (length res)) do
                     (setf (elt res i) (elt children i)))
                (setf (elt res (1- (length res))) child)
                res)))
    (values object child)))

(defmacro do-children ((child node) &body body)
  `(loop for ,child across (children ,node) do
        ,@body))

(defmacro do-children-from-end ((child node) &body body)
  `(loop for ,child across (reverse (children ,node)) do
        ,@body))

(defmethod add-children ((object m-tree) children)
  (loop for i in children do
       (add-child object i)))

(defmethod add-children* ((object m-tree) &rest children)
  (add-children object children))

(defmethod find-child ((object m-tree) to-find &key (compare #'equalp))
  (with-accessors ((data data) (children children)) object
    (if (funcall compare data to-find)
        object
        (if (leafp object)
            nil
            (find-if-not #'null
                         (map 'vector #'(lambda (c)
                                          (find-child c to-find :compare compare))
                              children))))))

(defmethod find-child-if ((object m-tree) predicate)
  (let ((res '()))
    (labels ((%find-child-if (object predicate)
               (when (funcall predicate object)
                 (push object res))
               (do-children (child object)
                 (%find-child-if child predicate))))
      (%find-child-if object predicate)
      res)))

(defmethod leafp ((object m-tree))
  (= (length (children object)) 0))

(defmethod rootp ((object m-tree))
  (null (parent object)))

(defmethod top-down-visit ((node m-tree) function &optional (args nil))
  (apply function (concatenate 'list (list node) args))
  (loop for c across (children node) do
       (top-down-visit c function args)))

(defmethod bottom-up-visit ((node m-tree) function &optional (args nil))
  (loop for c across (children node) do
       (bottom-up-visit c function args))
  (apply function (concatenate 'list (list node) args)))

(defmethod remove-all-children ((object m-tree))
  (setf (children object) (init-children)))

(defmethod remove-child ((object m-tree) (needle m-tree) &key
                                                           (key #'identity)
                                                           (test #'eq))
  (with-accessors ((children children)) object
    (if (leafp object)
        nil
        (loop for i fixnum from 0 below (length children) do
             (if (funcall test (funcall key needle) (funcall key (elt children i)))
                 (progn
                   (setf children
                         (concatenate `(vector ,(array-element-type children)
                                                      ,(1- (length children)))
                                      (subseq children 0 i)
                                      (subseq children (1+ i))))
                   (return-from remove-child t))
                 (remove-child (elt children i) needle :key key :test test))))))

(defmethod remove-child ((object m-tree) needle &key
                                                  (key #'identity)
                                                  (test #'eq))
  (with-accessors ((children children)) object
    (if (leafp object)
        nil
        (loop for i fixnum from 0 below (length children) do
             (if (funcall test (funcall key needle) (funcall key (elt children i)))
                 (progn
                   (setf children
                         (concatenate `(vector ,(array-element-type children)
                                                      ,(1- (length children)))
                                      (subseq children 0 i)
                                      (subseq children (1+ i))))
                   (return-from remove-child t))
                 (remove-child (elt children i) needle :key key :test test))))))

(defmethod remove-child-if ((object m-tree) predicate)
  (top-down-visit object
                  #'(lambda (n)
                      (with-accessors ((children children)) n
                        (setf children (delete-if predicate children))))))


(defmethod count-leafs ((object m-tree))
  (let ((results 0))
    (top-down-visit object #'(lambda (n)
                               (when (leafp n)
                                 (incf results))))
    results))

(defun make-node (data &optional (parent nil))
  (make-instance 'm-tree :data data :parent parent))
