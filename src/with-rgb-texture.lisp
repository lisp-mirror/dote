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

(in-package :pixmap)

(defmacro with-rgba-texture ((pixmap &key (width 512) (height 512))
			     &body body)
  `(let ((,pixmap (pixmap:make-pixmap-frame ,width ,height 4)))
     (loop-matrix (,pixmap x y) 
	,@body)
     ,pixmap))


(defmacro with-noise-rgba-texture ((pixmap pixel noise &key (width 512) (height 512)
					  (r 255) (g 255) (b 255))
				  &body body)
  `(let ((,pixmap (pixmap:make-pixmap-frame ,width ,height 4)))
     (loop-matrix (,pixmap x y) 
	(let ((,pixel (ivec4:ivec4 0 0 0 255))
	      (,noise 0.0))
	    ,@body
	    (setf ,noise (num:d/ (num:d+ ,noise 1.0) 2.0)
		  (elt ,pixel 0) (alexandria:clamp (truncate
						    (num:d* ,noise (num:desired ,r))) 0 255)
		  (elt ,pixel 1) (alexandria:clamp (truncate
						    (num:d* ,noise (num:desired ,g))) 0 255)
		  (elt ,pixel 2) (alexandria:clamp (truncate
						    (num:d* ,noise (num:desired ,b))) 0 255)
		  (elt ,pixel 3) (alexandria:clamp (truncate
						    (num:d* ,noise (num:desired ,b))) 0 255))
	    (setf (matrix:pixel@ ,pixmap x y) ,pixel))) 
     ,pixmap))
