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

(in-package :filesystem-utils)

(define-constant +preprocess-include+ "^%include" :test #'string=)

(define-constant +file-path-regex+ "[\\p{L},\\/,\\\\,\\.]+" :test 'string=)

(defparameter *directory-sep-regexp*
  #+windows "\\"
  #-windows "\\/")

(defparameter *directory-sep*
  #+windows "\\"
  #-windows "/")

(defun slurp-file (filename &key (convert-to-string t))
  "A simple way to slurp a file."
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence seq stream)
      (if convert-to-string
          (babel:octets-to-string seq)
          seq))))

(defun dump-sequence-to-file (seq file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-sequence seq stream)))

(defun create-file (file)
  "create file and parent dir, if necessary"
  (let ((path-splitted (fs:split-path-elements file)))
    (when (and path-splitted
               (> (length path-splitted) 1))
      (do* ((path-rest (subseq path-splitted 0 (1- (length path-splitted))) (rest path-rest))
            (path-so-far "" (if (and path-rest
                                     (not (string= "" (first-elt path-rest))))
                                (concatenate 'string
                                             path-so-far
                                             *directory-sep*
                                             (first-elt path-rest)
                                             *directory-sep*)
                                path-so-far)))
           ((null path-rest))
        (when (not (directory-exists-p path-so-far))
          (make-directory path-so-far)))
      (with-open-file (stream file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)))))

(defun has-extension (path ext)
  (let ((re (concatenate 'string ext "$")))
    (cl-ppcre:scan re path)))

(defun cat-parent-dir (parent direntry)
  (format nil "~a~a~a" parent *directory-sep* direntry))

(defmacro do-directory ((var) root &body body)
  (with-gensyms (dir)
    `(let ((,dir (nix:opendir ,root)))
       (unwind-protect
            (handler-case
                (do ((,var (cat-parent-dir ,root (nix:readdir ,dir))
                           (cat-parent-dir ,root (nix:readdir ,dir))))
                    ((cl-ppcre:scan "NIL$" ,var))
                  ,@body)
              (nix::enotdir () 0)
              (nix:eacces () 0)
              (nix:eloop () 0))
       (nix:closedir ,dir)))))

(defun search-matching-file (root-directory &key (name ".*"))
  "Scan a filesystem saving files that match the provided criteria,
   does not follow symlinks."
  (let ((matched '())
        (scanner (cl-ppcre:create-scanner name)))
    (labels ((match (dir)
               (do-directory (path) dir
                   (let ((filename (path-last-element path)))
                     (cond
                       ((regular-file-p path)
                        (when (cl-ppcre:scan scanner filename)
                          (push path matched)))
                       ((and (not (cl-ppcre:scan "^\\.\\." filename))
                             (not (cl-ppcre:scan "^\\."   filename))
                             (dirp path))
                        (match path)))))))
      (match root-directory)
      matched)))

(defun regular-file-p (path)
  (nix:s-isreg (nix:stat-mode (nix:stat path))))

(defun dirp (path)
  (nix:s-isdir (nix:stat-mode (nix:stat path))))

(defun split-path-elements (path)
  (cl-ppcre:split *directory-sep-regexp* path))

(defun path-last-element (path)
  (let ((elements (cl-ppcre:split *directory-sep-regexp* path)))
    (and elements
         (last-elt elements))))

(defun path-first-element (path)
  (let ((elements (cl-ppcre:split *directory-sep-regexp* path)))
    (and elements
         (first-elt elements))))

(defun path-to-hidden-file-p (path)
  "unix-like only"
  (let ((last-element (path-last-element path)))
    (and path (cl-ppcre:scan "^\\." last-element))))

(defun strip-dirs-from-path (p)
  (multiple-value-bind (all registers)
      (cl-ppcre:scan-to-strings (concatenate 'string
                                             *directory-sep*
                                             "([^"
                                             *directory-sep*
                                             "]+)$")
                                p)
    (declare (ignore all))
    (and (> (length registers) 0)
         (elt registers 0))))

(defun parent-dir-path (path)
  (let ((splitted (remove-if #'(lambda (a) (string= "" a))
                             (split-path-elements path))))
    (cond
      ((> (length splitted) 1)
       (let ((res (if (string= (string (elt path 0)) *directory-sep*)
                      (concatenate 'string *directory-sep* (first splitted))
                      (first splitted))))
         (loop for i in (subseq splitted 1 (1- (length splitted))) do
              (setf res (concatenate 'string res *directory-sep* i)))
         (setf res (concatenate 'string res *directory-sep*))
         res))
      ((null splitted)
       *directory-sep*)
      (t
       path))))

(defmacro define-stat-time (slot-name)
  (with-gensyms (stat)
    `(defun ,(format-symbol t "~:@(get-stat-~a~)" slot-name) (file)
       (restart-case
           (let ((,stat (nix:stat file)))
             (if ,stat
                 (,(format-symbol :nix "~:@(stat-~a~)" slot-name)
                   ,stat)))
         (use-value (value) value)))))

(define-stat-time mtime)

(define-stat-time ctime)

(define-stat-time atime)

(defun file-hash (file)
  (num:fnv-hash-32 (slurp-file file :convert-to-string nil)))

(defun file-outdated-p (file &rest dependencies)
  (handler-bind ((nix:enoent #'(lambda (c)
                                 (declare (ignore c))
                                 (invoke-restart 'use-value nil))))
    (let ((atime (get-stat-atime file))
          (mtimes (remove-if #'null (mapcar #'get-stat-mtime dependencies))))
      (if atime
          (remove-if #'(lambda (mtime) (<= mtime atime)) mtimes)
          t))))

(defun file-exists-p (f)
  (uiop:file-exists-p f))

(defun directory-exists-p (d)
  (uiop:directory-exists-p d))

(defun delete-file-if-exists (f)
  (uiop:delete-file-if-exists f))

(defun file-length-if-exists (f)
  (when (file-exists-p f)
    (with-open-file (stream f :element-type '(unsigned-byte 8))
      (file-length stream))))

(defun temporary-filename (&optional (temp-directory nil))
  (let ((tmpdir (or temp-directory (nix:getenv "TMPDIR"))))
    (if tmpdir
        (nix:mktemp (format nil "~a~a~a" tmpdir *directory-sep*
                    config:+program-name+))
        (nix:mktemp (format nil "~atmp~a~a" *directory-sep* *directory-sep*
                    config:+program-name+)))))

(defmacro with-anaphoric-temp-file ((stream &key (prefix nil) (unlink nil)) &body body)
  `(let ((temp-file (temporary-filename ,prefix))) ; anaphora
       (unwind-protect
            (with-open-file (,stream temp-file
                                     :direction         :output
                                     :if-exists         :error
                                     :if-does-not-exist :create)
              ,@body)
         ,(if unlink
              `(delete-file-if-exists temp-file)
              nil))))

(defun has-file-permission-p (file permission)
  (find permission (osicat:file-permissions file) :test #'eq))

(defun file-can-write-p (file)
  (has-file-permission-p file :user-write))

(misc:defcached cached-directory-files ((path) :test equal)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (if (gethash path cache)
      (gethash path cache)
      (progn
        (setf (gethash path cache) (uiop:directory-files path))
        (cached-directory-files path))))

(defun directory-files (path)
  (uiop:directory-files path))

(defun make-directory (path)
  (if (not (cl-ppcre:scan (concatenate 'string *directory-sep* "$") path))
      (make-directory (concatenate 'string path *directory-sep*))
      (ensure-directories-exist path)))

(defun preprocess-include-file (line resource)
  (let ((included-filepath (second (cl-ppcre:split "\\p{Z}" line))))
    (if (cl-ppcre:scan +file-path-regex+ included-filepath)
        (filesystem-utils:slurp-file (res:get-resource-file included-filepath
                                                            resource))
        "")))

(defun preprocess (file resource)
  (let ((preprocessed-p nil))
    (with-open-file (stream-input file :direction :input :if-does-not-exist :error)
      (filesystem-utils:with-anaphoric-temp-file (stream-out :prefix nil :unlink nil)
        (do ((line (read-line stream-input nil nil) (read-line stream-input nil nil)))
            ((null line))
          (cond
            ((cl-ppcre:scan +preprocess-include+ line)
             (setf preprocessed-p t)
             (format stream-out "~a" (preprocess-include-file line resource)))
            (t
             (format stream-out "~a~%" line))))
        (if preprocessed-p
            (prog1
                (progn
                  (finish-output stream-out)
                  (preprocess filesystem-utils:temp-file resource))
              (delete-file-if-exists filesystem-utils:temp-file))
            filesystem-utils:temp-file)))))

(defun package-path ()
  (uiop:pathname-parent-directory-pathname
   (asdf:component-pathname
    (asdf:find-component (symbolicate (string-upcase config:+program-name+))
                         nil))))

(defun file-in-package (name)
  (concatenate 'string (namestring (package-path)) name))

(defparameter *file-link-to* nil)

(define-constant +rel-link+ :rel)

(define-constant +abs-link+ :abs)

(defmacro see-file (&body forms)
  (if (> (length forms) 1)
      (warn "see-file: too many elements in forms, must be exactly 2"))
  (let ((path (first-elt forms)))
    (when (not (stringp path))
      (error (format nil "see-file: the path ~a is not a string" path)))
    (when (= (length path) 0)
      (error (format nil "see-file: the path ~a is to short" path)))
    (if (string= *directory-sep* (string (first-elt path)))
        `(setf *file-link-to* (cons ,path +abs-link+))
        `(setf *file-link-to* (cons ,path +rel-link+)))))

(defun link-file-path (file)
  (misc:with-load-forms-in-var (*file-link-to* link-file file)
    (if link-file
        (destructuring-bind (path . type)
            link-file
          (if (eq type +rel-link+)
              (cat-parent-dir (parent-dir-path file) path)
              path))
        nil)))

(defmacro file-is-link-if-else ((file link-file-pointed) is-link-forms is-not-link-forms)
  `(let ((,link-file-pointed (link-file-path ,file)))
     (if ,link-file-pointed
         ,is-link-forms
         ,is-not-link-forms)))
