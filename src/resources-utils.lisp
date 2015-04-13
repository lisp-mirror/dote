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

(in-package :resources-utils)

(alexandria:define-constant +virtual-fs-dir-separator+ "/" :test #'string=)

(alexandria:define-constant +virtual-fs-dir-separator-regexp+ "\\/" :test #'string=)

(defun construct-path (p)
  (let ((splitted (split +virtual-fs-dir-separator-regexp+ p)))
    (strcat *directory-sep* (join-with-srings splitted *directory-sep*)
	    (if (cl-ppcre:scan (strcat +virtual-fs-dir-separator-regexp+ "$") p)
		*directory-sep*))))

(defun find-in-home-datadir (file)
  (let ((actual-path (join-with-srings* *directory-sep*
					(uiop:getenvp "HOME")
					+home-data-dir+
					(construct-path file))))
    (cond
      ((not (uiop:getenvp "HOME"))
	nil)
      ((or
	 (uiop:directory-exists-p actual-path)
	 (uiop:file-exists-p actual-path))
       actual-path)
      (t
       nil))))

(defun find-in-shared-datadir (file)
  (let ((actual-path (join-with-srings* *directory-sep*
					+sys-data-dir+
					(construct-path file))))
    (if (or
	 (uiop:directory-exists-p actual-path)
	 (uiop:file-exists-p actual-path))
	actual-path
	nil)))

(defun %normalize-resource-name (n)
  (typecase n
    (symbol (string-downcase (symbol-name n)))
    (otherwise (format nil "~a" n))))
  
(defun construct-resource-path (resource p)
  (typecase resource
    (cons
     (join-with-srings* +virtual-fs-dir-separator+ 
			(join-with-srings (mapcar #'%normalize-resource-name resource)
					  +virtual-fs-dir-separator+)
			p))
     (otherwise
      (join-with-srings* +virtual-fs-dir-separator+
			(%normalize-resource-name resource)
			p))))

(define-condition resource-not-found-error (file-error)
  ((resource
    :initarg :resource
    :reader  resource))
  (:report (lambda (condition stream) 
	     (format stream "Resource file not found: resource ~s path ~s"
		     (resource condition) (file-error-pathname condition)))))

(define-condition resource-not-writable-error (file-error)
  ((resource
    :initarg :resource
    :reader  resource))
  (:report (lambda (condition stream) 
	     (format stream "Resource not writable: resource ~s path ~s"
		     (resource condition) (file-error-pathname condition)))))

(defun get-resource-file (p resource &key (if-does-not-exists :error))
  (let ((home-path   (find-in-home-datadir   (construct-resource-path resource p)))
	(shared-path (find-in-shared-datadir (construct-resource-path resource p))))
    (cond
      ((eq if-does-not-exists :error)
       (or (or home-path shared-path)
	   (error 'resource-not-found-error :pathname p :resource resource)))
      ((eq if-does-not-exists :return-writable)
       (cond
	 ((and home-path
	       (file-can-write-p home-path))
	  home-path)
	 ((and shared-path
	       (file-can-write-p shared-path))
	  shared-path)
	 (t
	  (error 'resource-not-writable-error :pathname p :resource resource)))))))
