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

(in-package :os-utils)

(alexandria:define-constant +proc-file-system+
    (concatenate 'string filesystem-utils:*directory-sep*
                 "proc")
  :test #'string=)

(alexandria:define-constant +proc-cpuinfo+
    (concatenate 'string +proc-file-system+ filesystem-utils:*directory-sep*
                 "cpuinfo")
  :test #'string=)


(declaim (ftype (function () fixnum) cpu-number))

(defun cpu-number ()
  #+windows (the fixnum 1)
  #-windows
  (with-open-file (stream +proc-cpuinfo+ :direction :input
                            :if-does-not-exist :error)
    (do ((line (read-line stream nil nil) (read-line stream nil nil))
         (cpu-count 0))
        ((not line) (the fixnum cpu-count))
      (when (cl-ppcre:scan "^processor" line)
        (incf cpu-count)))))
