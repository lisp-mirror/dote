;; Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
;; Licensed under the LLGPL License.

(in-package :profiling)

(defmacro with-profiling ((&key packages) &body body)
  `(progn
     ,@(loop for package in packages collect
            `(swank::profile-package ,package t t))
     (unwind-protect
          ,@body
       (swank::profile-report)
       (swank::unprofile-all))))
