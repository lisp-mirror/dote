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

(defun has-extension (path ext)
  (let ((re (concatenate 'string ext "$")))
    (cl-ppcre:scan re path)))

(defun cat-parent-dir (parent direntry)
  (format nil "~a~a~a" parent *directory-sep* direntry))

(defmacro do-directory ((var) root &body body)
  (alexandria:with-gensyms (dir)
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
	 (alexandria:last-elt elements))))

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
  (alexandria:with-gensyms (stat)
    `(defun ,(alexandria:format-symbol t "~:@(get-stat-~a~)" slot-name) (file)
       (restart-case
	   (let ((,stat (nix:stat file)))
	     (if ,stat
		 (,(alexandria:format-symbol :nix "~:@(stat-~a~)" slot-name)
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
	    (with-open-file (,stream temp-file :direction :output
				     :if-exists :error
				     :if-does-not-exist :create)
	      ,@body)
	 ,(if unlink
	      `(nix:unlink temp-file)
	      nil))))

(defun has-file-permission-p (file permission)
  (find permission (osicat:file-permissions file) :test #'eq))

(defun file-can-write-p (file)
  (has-file-permission-p file :user-write))

(defun package-path ()
  (uiop:pathname-parent-directory-pathname
   (asdf:component-pathname
    (asdf:find-component (alexandria:symbolicate (string-upcase config:+program-name+))
			 nil))))

(defun file-in-package (name)
  (concatenate 'string (namestring (package-path)) name))
