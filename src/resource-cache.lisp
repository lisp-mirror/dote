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

(in-package :resource-cache)

(alexandria:define-constant +cache-parent-dir+ ".cache" :test  #'string=)

(defparameter *cache-system-running* nil)

(defparameter *cache-reference-file* *load-truename*)

(define-condition cache-error (text-error)
  ()
  (:report (lambda (condition stream)
            (format stream "Cache error: ~a" (conditions:text condition)))))

(defun cache-path ()
  (let ((home-dir (uiop:getenvp "HOME")))
    (if home-dir
        (let ((parent-cache-path (strcat home-dir
                                         *directory-sep* +cache-parent-dir+ *directory-sep*)))
          (if (uiop:directory-exists-p parent-cache-path)
              (let ((actual-cache-path
                     (strcat parent-cache-path +program-name+ *directory-sep*)))
                (values actual-cache-path (uiop:directory-exists-p actual-cache-path)))
              (error 'cache-error :text
                     (strcat
                      (format nil "no valid cache directory (\"~a\") found." +cache-parent-dir+)
                      (format nil "Please ensure that \"~a\" exists." parent-cache-path)))))
        (error 'cache-error :text "no valid home directory found in environment variable"))))

(defun init-cache ()
  (multiple-value-bind (cache-path cache-path-exists)
      (cache-path)
    (if cache-path-exists
        (progn
          (setf *cache-system-running* t)
          cache-path)
        (handler-bind ((file-error #'(lambda (c)
                                       (declare (ignore c))
                                       (setf *cache-system-running* t)
                                       (invoke-restart 'use-value nil))))

          (ensure-directories-exist cache-path)
          (setf *cache-system-running* t)
          cache-path))))

(defstruct cache-key-element
  name
  file-type)

(defmacro with-cache-system-running (&body body)
  `(if *cache-system-running*
       (progn ,@body)
       (error 'cache-error :text "the cache system has not been initialized.")))

(defun make-cache-key (&rest elements)
  (make-cache-key* elements))

(defun make-cache-key* (elements)
  (with-cache-system-running
    (do ((elements-l elements (rest elements-l))
         (path (cache-path)
               (let ((element (first elements-l)))
                 (if element
                     (strcat path (cache-key-element-name element)
                             (if (eq (cache-key-element-file-type element) :directory)
                                 *directory-sep*
                                 nil))
                     path))))
        ((not elements-l) path))))

(defun regular-file-strings->cache-key (&rest names)
  (regular-file-strings->cache-key* names))

(defun regular-file-strings->cache-key* (names)
  (let ((alist (loop for i in names collect (cons i :directory))))
    (when (> (length alist) 0)
      (setf (cdr (alexandria:last-elt alist)) :file))
    (cons->cache-key* alist)))

(defun directory-strings->cache-key (&rest names)
  (directory-strings->cache-key* names))

(defun directory-strings->cache-key* (names)
  (loop for i in names collect (cons i :directory)))

(defun cons->cache-key (&rest names-assoc)
  (cons->cache-key* names-assoc))

(defun cons->cache-key* (names-alist)
  (make-cache-key*
   (loop for name-assoc in names-alist collect
        (make-cache-key-element :name (car name-assoc) :file-type (cdr name-assoc)))))

(defgeneric cache-miss* (key))

(defmethod cache-miss* ((key string))
  (with-cache-system-running
    (or (not (uiop:probe-file* key))
        (if *cache-reference-file*
            (< (get-stat-mtime key)
               (get-stat-mtime *cache-reference-file*))
            nil))))

(defmethod cache-miss* ((names-alist list))
  (cache-miss* (cons->cache-key* names-alist)))

(defun cache-miss (&rest names-assoc)
  (cache-miss* names-assoc))

(defun file-miss (&rest names)
  (file-miss* names))

(defun file-miss* (names)
  (cache-miss* (regular-file-strings->cache-key* names)))

(defun directory-miss (&rest names)
  (directory-miss* names))

(defun directory-miss* (names)
  (cache-miss* (directory-strings->cache-key* names)))

(defun ensure-cache-directory (&rest names)
  (ensure-cache-directory* names))

(defun ensure-cache-directory* (names)
  (let ((path (make-cache-key* (loop for name in names collect
                                    (make-cache-key-element :name name :file-type :directory)))))
    (ensure-directories-exist path)))

(defmacro ensure-cache-running (&body body)
   `(let ((*cache-system-running* nil))
      (init-cache)
      ,@body))

(defun test-init-cache ()
  (init-cache))
