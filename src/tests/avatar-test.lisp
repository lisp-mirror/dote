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

(in-package :avatar-test)

(defsuite avatar-suite (all-suite))

(alexandria:define-constant +avatar-path+ "data/avatars/" :test #'string=)

(alexandria:define-constant +texture-dir-tmp+ (concatenate 'string (test-dir) "tmp/")
  :test #'string=)

(defun %gen-avatar ()
  (num:with-lcg-seed (1)
    (build-avatar "m")))

(deftest test-generation (avatar-suite)
  (assert-true
      (let* ((generated (%gen-avatar))
             (res (equalp
                   (pixmap:data (load-test-tga (concatenate 'string +avatar-path+
                                                            "avatar0.tga")))
                   (pixmap:data generated))))
        (when (not res)
          (dump-pixmap generated +texture-dir-tmp+ "avatar0.tga"))
        res)))
