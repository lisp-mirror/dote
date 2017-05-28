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

(in-package :avatar-portrait)

(alexandria:define-constant +categories-separator+   "_"      :test #'string=)

(alexandria:define-constant +male-field-id+          "m"      :test #'string=)

(alexandria:define-constant +female-field-id+        "f"      :test #'string=)

(alexandria:define-constant +optional-field-id+      "o"      :test #'string=)

(alexandria:define-constant +optional-field-chance+   2       :test #'=)

(alexandria:define-constant +internal-root+          "root"   :test #'string=)

(alexandria:define-constant +root+                   "avatar" :test #'string=)

(alexandria:define-constant +ext+                    ".tga"   :test #'string=)

(alexandria:define-constant +w-avatar+               64       :test #'=)

(alexandria:define-constant +h-avatar+               64       :test #'=)

(defparameter *db* (make-node +internal-root+))

(defun path-exists-p (node path)
  (labels ((%path-exists-p (node path)
             (if (not path)
                 (return-from path-exists-p node)
                 (progn
                   (if (find (first path)
                             (children node)
                             :test #'(lambda (a b) (string= (data b) a)))
                       (%path-exists-p (find (first path)
                                             (children node)
                                             :test #'(lambda (a b) (string= (data b) a)))
                                       (rest path)))))))
    (%path-exists-p node path)))

(defun init-db (&optional (node *db*))
  (let* ((*use-pprint-tree* nil)
         (all-files (remove-if #'null
                               (map 'list
                                    #'(lambda (a)
                                        (multiple-value-bind (found registers)
                                            (scan-to-strings "\\/([^\\/]+)\\.tga$" a)
                                          (if found
                                              (elt registers 0)
                                              nil)))
                                    (map 'list #'uiop:native-namestring
                                         (uiop/filesystem:directory-files
                                          (res:get-resource-file ""
                                                                 +avatar-portrait-resource+))))))
         (fields-list (loop for i in all-files collect (split +categories-separator+ i))))
    (labels ((add-path (node fields)
               (if fields
                   (let ((root (path-exists-p node (list (first fields)))))
                     (if root
                         (add-path root (rest fields))
                         (multiple-value-bind (parent child)
                             (add-child node (make-node (first fields) node))
                           (declare (ignore parent))
                           (add-path child (rest fields))))))))
      (loop for fields in fields-list do
           (add-path node fields)))))

(defun get-prefix (parts)
  (scan-to-strings "^[^0-9]+" parts))

(defun get-suffix (part)
  (scan-to-strings ".$" part))

(defun optional-layer-p (part)
  (string= (get-suffix part) +optional-field-id+))

(defun filter-right-gender (children gender)
  (remove-if #'(lambda (a) (and (get-prefix (data a)) ;; no prefix do not remove
                                (not  (string= (get-prefix (data a)) gender))))
             children))

(defun get-avatar-parts (gender &optional (node *db*) (parts '()))
  (if (and node
           (not (misc:vector-empty-p (children node))))
      (let* ((actual-children (if (some #'(lambda (c) (leafp c)) (children node))
                                  (filter-right-gender (children node) gender)
                                  (children node)))
             (chosen (and (not (misc:vector-empty-p actual-children))
                          (elt actual-children
                               (num:lcg-next-upto (length actual-children))))))
        (when chosen
          (get-avatar-parts gender chosen (push (data chosen) parts))))
      parts))

(defun get-avatar (gender &optional (node *db*))
  (when (leafp node)
    (init-db node))
  (let* ((actual-root (elt (children node) 0))
         (bouquet (map 'list #'(lambda (a) (push +root+ a))
                       (remove-if #'null
                                  (loop for child across (children actual-root) collect
                                       (let ((path (get-avatar-parts gender child)))
                                         (when path
                                           (push (data child) path))))))))
    (map 'list #'(lambda (a) (text-utils:strcat a +ext+))
         (remove-if #'(lambda (a)
                        (if (optional-layer-p a)
                            (if  (/= (mod (num:lcg-next) +optional-field-chance+) 0)
                                 t
                                 nil)
                            nil))
                    (map 'list #'(lambda (a) (text-utils:join-with-srings a +categories-separator+))
                         bouquet)))))

(defun count-transparent-pixel (pixmap)
  (let ((count 0))
    (matrix:loop-matrix (pixmap x y)
      (if (= (elt (matrix:matrix-elt pixmap y x) 3) 0)
          (incf count)))
    (/ count (* +w-avatar+ +h-avatar+))))

(defun count-opaque-pixel-on-top-ratio (pixmap &optional (h (truncate (* +h-avatar+ 0.2))))
  (let ((count 0))
    (matrix:loop-submatrix (pixmap x y 0 0 (pixmap:width pixmap) h)
      (if (> (elt (matrix:matrix-elt pixmap y x) 3) 0)
          (incf count)))
    (/ count (* +w-avatar+ h))))

(defun count-opaque-pixel-on-left-ratio (pixmap &optional (w (truncate (* +w-avatar+ 0.2))))
  (let ((count 0))
    (matrix:loop-submatrix (pixmap x y 0 0 w (pixmap:height pixmap))
      (if (> (elt (matrix:matrix-elt pixmap y x) 3) 0)
          (incf count)))
    (/ count (* +h-avatar+ w))))

(defun count-opaque-pixel-on-down-right-ratio (pixmap &optional (offset 10))
  (let ((count 0))
    (matrix:loop-submatrix (pixmap x y (- +w-avatar+ offset) (- +h-avatar+ offset) +w-avatar+ +h-avatar+)
      (if (> (elt (matrix:matrix-elt pixmap y x) 3) 0)
          (incf count)))
    (/ count (expt offset 2))))

(defun score-pixmap (pixmap gender)
  (if (string= gender +male-field-id+)
      (+ (* 2.4 (count-transparent-pixel pixmap))
         (* 8.2 (count-opaque-pixel-on-top-ratio pixmap (truncate (* +h-avatar+ 0.05))))
         (* 4.2 (count-opaque-pixel-on-top-ratio pixmap))
         (* 1.2 (count-opaque-pixel-on-left-ratio pixmap))
         (* 0.1 (count-opaque-pixel-on-down-right-ratio pixmap)))
      (+ (* 1.2 (count-transparent-pixel pixmap))
         (* 8.2 (count-opaque-pixel-on-top-ratio pixmap (truncate (* +h-avatar+ 0.05))))
         (* 24.2 (count-opaque-pixel-on-top-ratio pixmap))
         (* 18.2 (count-opaque-pixel-on-left-ratio pixmap))
         (* 0.1 (count-opaque-pixel-on-down-right-ratio pixmap)))))

(defun sort-layers-fn ()
  #'(lambda (a b) (> (second a) (second b))))

(defun build-avatar (gender)
  (assert (or (string= gender +male-field-id+)
              (string= gender +female-field-id+)))
   (let* ((pixmap (pixmap:make-pixmap +w-avatar+ +h-avatar+))
          (names  (get-avatar gender))
          (layers (reverse (sort
                            (loop for name in names collect
                                 (let* ((path (res:get-resource-file name
                                                                     +avatar-portrait-resource+))
                                        (pixmap (pixmap:slurp-pixmap 'pixmap:tga path)))
                                   (list pixmap (score-pixmap pixmap gender) name)))
                            (sort-layers-fn)))))
     (loop
        for layer in layers
        for name in names do
          (pixmap:blit (first layer) pixmap 0 0 0 0
                       :function-blend
                       #'(lambda (src dest x-src y-src x-dst y-dst)
                           (let* ((pix-src (matrix:matrix-elt src y-src x-src))
                                  (w (elt pix-src 3)))
                             (map 'ubvec4:ubvec4  #'(lambda (a b)
                                                      (truncate (alexandria:lerp (/ w 255) b a)))
                                  pix-src
                                  (matrix:matrix-elt dest y-dst x-dst))))))
     (matrix:loop-matrix (pixmap x y)
       (when (> (elt (matrix:matrix-elt pixmap y x) 3) 100)
         (setf (elt (matrix:matrix-elt pixmap y x) 3) 255)))
     pixmap))
