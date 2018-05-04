;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more  details.  You should have received
;; a copy of  the GNU General Public License along  with this program.
;; If not, see <http://www.gnu.org/licenses/>.

(in-package :resources-utils)

(alexandria:define-constant +virtual-fs-dir-separator+ "/" :test #'string=)

(alexandria:define-constant +virtual-fs-dir-separator-regexp+ "\\/" :test #'string=)

(defun construct-path (p)
  (let ((splitted (split +virtual-fs-dir-separator-regexp+ p)))
    (strcat *directory-sep* (join-with-strings splitted *directory-sep*)
            (if (cl-ppcre:scan (strcat +virtual-fs-dir-separator-regexp+ "$") p)
                *directory-sep*))))

(defun home-datadir ()
  (join-with-strings* *directory-sep*
                      (uiop:getenvp "HOME")
                      +home-data-dir+))

(defun shared-datadir ()
  (join-with-strings* *directory-sep*
                      +sys-data-dir+))

(defun init ()
  #+debug-mode
  (when (not (directory-exists-p (home-datadir)))
    (misc:dbg "creating ~a" (home-datadir)))
  (fs:make-directory (home-datadir)))

(defun find-in-home-datadir (file &key (return-always-path nil))
  (let ((actual-path (join-with-strings* *directory-sep*
                                         (home-datadir)
                                         (construct-path file))))
    (cond
      ((not (uiop:getenvp "HOME"))
        (error "Home directory not present"))
      ((or
         (uiop:directory-exists-p actual-path)
         (uiop:file-exists-p actual-path)
         return-always-path)
       actual-path)
      (t
       nil))))

(defun find-in-shared-datadir (file &key (return-always-path nil))
  (let ((actual-path (join-with-strings* *directory-sep*
                                         (shared-datadir)
                                         (construct-path file))))
    ;;(break)
    (if (or
         (uiop:directory-exists-p actual-path)
         (uiop:file-exists-p actual-path)
         return-always-path)
        actual-path
        nil)))

(defun %normalize-resource-name (n)
  (typecase n
    (symbol (string-downcase (symbol-name n)))
    (otherwise (format nil "~a" n))))

(defun construct-resource-path (resource p)
  (typecase resource
    (cons
     (join-with-strings* +virtual-fs-dir-separator+
                         (join-with-strings (mapcar #'%normalize-resource-name resource)
                                            +virtual-fs-dir-separator+)
                         p))
     (otherwise
      (join-with-strings* +virtual-fs-dir-separator+
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

(defun create-home-resource (path resource)
  (let ((home-path (find-in-home-datadir (construct-resource-path resource path)
                                         :return-always-path t)))
    (fs:create-file home-path)
    home-path))

(defun get-shared-resource-filename (path resource)
  (find-in-shared-datadir (construct-resource-path resource path)
                          :return-always-path t))

(defun get-resource-file (path resource &key (if-does-not-exists nil))
  (let ((home-path   (find-in-home-datadir   (construct-resource-path resource path)))
        (shared-path (find-in-shared-datadir (construct-resource-path resource path))))
    (cond
      ((null if-does-not-exists)
       (or home-path shared-path))
      ((eq if-does-not-exists :create)
       (if (not home-path)
           (create-home-resource path resource)
           home-path))
      ((eq if-does-not-exists :error)
       (or (or home-path shared-path)
           (error 'resource-not-found-error :pathname path :resource resource)))
      ((eq if-does-not-exists :return-writable)
       (cond
         ((and home-path
               (file-can-write-p home-path))
          home-path)
         ((and shared-path
               (file-can-write-p shared-path))
          shared-path)
         (t
          (error 'resource-not-writable-error :pathname path :resource resource)))))))

(defun get-resource-files-merge (resource predicate)
  "home wins"
  (let* ((home-path  (find-in-home-datadir   (construct-resource-path resource ".")))
         (shared-path (find-in-shared-datadir (construct-resource-path resource ".")))
         (res         (directory-files        home-path)))
    (loop for i in (directory-files shared-path) do
         (pushnew i res :test predicate))
    res))

(defun get-resource-files (resource &key (if-does-not-exists :error))
  (let ((home-path   (find-in-home-datadir   (construct-resource-path resource ".")))
        (shared-path (find-in-shared-datadir (construct-resource-path resource "."))))
    (cond
      ((eq if-does-not-exists :error)
       (if home-path
           (cached-directory-files home-path)
           (if shared-path
               (cached-directory-files shared-path)
               (error 'resource-not-found-error :pathname "." :resource resource))))
      ((eq if-does-not-exists :return-writable)
       (cond
         ((and home-path
               (file-can-write-p home-path))
          (cached-directory-files home-path))
         ((and shared-path
               (file-can-write-p shared-path))
           (cached-directory-files shared-path))
         (t
          (error 'resource-not-writable-error :pathname "." :resource resource)))))))

(defun strip-off-resource-path (resource path)
  (flet ((strip-prefix (pref path)
           (cl-ppcre:regex-replace (strcat pref *directory-sep* "*")
                                   path
                                   "")))
    (let ((relative-path (strip-prefix (home-datadir)
                                        (strip-prefix (shared-datadir)
                                                      path))))
      (strip-prefix (construct-resource-path resource "") relative-path))))
