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

(in-package :pixmap-test)

(defsuite pixmap-suite (all-suite))

(alexandria:define-constant +pixmap-path+
    (concatenate 'string (test-dir) "data/pixmap/")
  :test #'string=)

(alexandria:define-constant +pixmap-dir+ "data/pixmap/"  :test #'string=)


(alexandria:define-constant +tmp-dir+ (concatenate 'string (test-dir) "tmp/")
  :test #'string=)

(defun %blit ()
  (let* ((blitted-filename "blit.tga")
	 (path (concatenate 'string +pixmap-dir+ "test-blit.tga"))
	 (input (all-test:load-test-tga path))
	 (res (make-pixmap-frame (pixmap:width input) (pixmap:height input)))
	 (input-path (format nil "~a~a" +pixmap-dir+ blitted-filename)))
    (blit input res 8 8 15 16 :w 16 :h 16 :function-blend (pixmap::guess-correct-replace-fn input))
    (values res input-path)))

(defun %rotate-w-repeat ()
  (let* ((output-filename "test-rotate-w-repeat.tga")
	 (path (concatenate 'string +pixmap-dir+ "test-blit.tga"))
	 (input (all-test:load-test-tga path))
	 (input-path (format nil "~a~a" +pixmap-dir+ output-filename)))
    (values (matrix->pixmap
	     (matrix:rotate-matrix input 45.0
				   :fill-value (ubvec4:ubvec4 0 0 2 1) 
				   :repeat t))
	    input-path)))

(defun %rotate-180 ()
  (let* ((not-rotated (all-test:load-test-tga
		       (concatenate 'string +pixmap-dir+ "test-rotate-180.tga")))
	 (rotated (all-test:load-test-tga
		   (concatenate 'string +pixmap-dir+ "test-rotated-180.tga"))))
    (values (matrix->pixmap
	     (matrix:rotate-matrix not-rotated 180.0
				   :fill-value (ubvec4:ubvec4 0 0 2 1) 
				   :repeat nil))
	    rotated)))

(defun %test-read-tga-from-vector ()
  (let* ((path (concatenate 'string +pixmap-dir+ "test-blit.tga"))
	 (path-slurp (concatenate 'string (test-dir) path))
	 (tga-file (make-instance 'tga))
	 (data     (filesystem-utils:slurp-file path-slurp :convert-to-string nil)))
    (load-from-vector tga-file data)
    (values tga-file path)))

(deftest test-read-tga-from-vector (pixmap-suite)
  (assert-true
      (multiple-value-bind (loaded input-file-path) (%test-read-tga-from-vector)
	(equalp  
	 (pixmap:data (load-test-tga input-file-path))
	 (pixmap:data loaded)))))

(deftest test-rotate-w-repeat (pixmap-suite)
  (with-kernel
    (assert-true
	(multiple-value-bind (rotated input-file-path) (%rotate-w-repeat)
	  (equalp  
	   (pixmap:data (load-test-tga input-file-path))
	   (pixmap:data rotated))))))

(deftest test-rotate-180 (pixmap-suite)
  (with-kernel
    (assert-true
	(multiple-value-bind (rotated reference) (%rotate-180)
	  (equalp  
	   (pixmap:data rotated)
	   (pixmap:data reference))))))

(deftest test-blit (pixmap-suite)
  (assert-true
      (multiple-value-bind (blitted input-file-path) (%blit)
	(equalp  
	 (pixmap:data (load-test-tga input-file-path))
	 (pixmap:data blitted)))))

(deftest test-make-seamless (pixmap-suite)
  (assert-true
      (let ((num:*default-epsilon* 1e-2))
	(vec4:vec4~
	 (pixmap:nrmse (load-test-tga (format nil "~a~a" +pixmap-dir+ "blood-splat-seamless.tga"))
		       (tileize (load-test-tga (format nil "~a~a" +pixmap-dir+ "blood-splat.tga"))))
	 (vec4:vec4 1.0 1.0 1.0 1.0)))))

(deftest test-dhash (pixmap-suite)
  (with-kernel
    (assert-true
	(=
	 (pixmap:dhash (load-test-tga (format nil "~a~a" +pixmap-dir+ "blood-splat-seamless.tga")))
	 (pixmap:dhash (tileize (load-test-tga (format nil "~a~a" +pixmap-dir+ 
						       "blood-splat.tga"))))))))
