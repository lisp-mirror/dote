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

(in-package :all-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun test-dir ()
    (namestring (asdf:component-pathname (asdf:find-component 'dote "tests")))))

(alexandria:define-constant +tmp-dir+ (concatenate 'string (test-dir) "tmp/")
  :test #'string=)

(defmacro gen-load-pixmap (type)
  `(defun ,(alexandria:format-symbol t "~@:(load-test-~a~)" type) (name)
     (let ((path (concatenate 'string (test-dir) name))
           (file (make-instance ',(alexandria:format-symbol :pixmap "~@:(~a~)" type))))
    (pixmap:load file path)
    file)))

(gen-load-pixmap tga)

(gen-load-pixmap ppm)

(gen-load-pixmap pgm)

(defun dump-pixmap (pixmap path name)
  (pixmap:save-pixmap pixmap (format nil "~a~a" path name))
  (values name path))

(defmacro with-kernel (&body body)
  `(let* ((config:*workers-number* (if (> (os-utils:cpu-number) 1)
                                       (os-utils:cpu-number)
                                       1))
          (lparallel:*kernel* (lparallel:make-kernel config:*workers-number*)))
     (progn
       ,@body)
     (lparallel:end-kernel :wait t)))

(defsuite all-suite ())

(defun run-all-tests (&key (use-debugger nil))
  (clunit:run-suite 'all-suite :use-debugger use-debugger))
