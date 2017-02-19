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

(in-package :color-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\§ #\c
				#'(lambda (stream char1 num)
				    (declare (ignore char1 num))
				    (let ((hex (do ((ch (read-char stream nil nil) (read-char stream nil nil))
						    (res '()))
						   ((not (cl-ppcre:scan "(?i)[a,b,c,d,e,f,0-9]" (string ch)))
						    (progn
						      (unread-char ch stream)
						      (reverse res)))
						 (push ch res))))
				      (int->vec4 (hex-list->int hex)))))

  (defun hex-digit->int (hex)
    (position hex '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)
	      :test #'char-equal))

  (defun hex-list->int (hex)
    (do ((i (reverse hex) (rest i))
	 (ct 0 (1+ ct))
	 (res 0))
	((null i) res)
      (setf res (+ res (* (expt 16 ct) (hex-digit->int (first i)))))))

  (defun float->byte (f)
    (declare (desired-type f))
    (floor (d* 255.0 f)))

  (defun byte->float (f)
    (d/ (desired f) 255.0))

  (defun int->bytes (octects)
    (do ((ct 0 (1+ ct))
	 (oct octects (ash oct -8))
	 (res nil))
	((not (< ct 4)) res)
      (push (logand oct #xff) res)))

  (defun int->vec4 (octects)
    (declare (optimize (safety 0) (speed 3) (debug 0)))
    (map 'vec4 #'byte->float (int->bytes octects)))

  (defun byte-vector->vec4 (bytes)
    (map 'vec4 #'byte->float bytes))

  (defun vec4->byte-vector (floats)
    (declare (optimize (safety 0) (speed 3) (debug 0)))
    (declare (vec4 floats))
    (map 'ivec4 #'float->byte floats))

  (defun vec4->ubvec4 (floats)
    (declare (optimize (safety 0) (speed 3) (debug 0)))
    (declare (vec4 floats))
    (map 'ubvec4 #'float->byte floats)))

(defun random-mixed-color (color)
  (vec4 (round (alexandria:lerp 0.5 (elt color 0) (lcg-next-upto 256)))
	(round (alexandria:lerp 0.5 (elt color 1) (lcg-next-upto 256)))
	(round (alexandria:lerp 0.5 (elt color 2) (lcg-next-upto 256)))))

(defun rgb->hsv (r g b)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (desired-type r g b))
  (let* ((min-rgb (dmin r g b))
	 (max-rgb (dmax r g b))
	 (v max-rgb)
	 (delta (d- max-rgb min-rgb))
	 (s (if (not (epsilon= delta 0.0))
		(d/ delta max-rgb)
		0.0))
	 (h (if (epsilon= s 0.0)
		-1.0
		(let ((h-tmp (d* 60.0
				 (cond
				   ((epsilon= max-rgb r)
				    (d/ (d- g b) delta))
				   ((epsilon= max-rgb g)
				    (d+ 2.0 (d/ (d- b r) delta)))
				   (t
				    (d+ 4.0 (d/ (d- r g) delta)))))))
		  (when (d< h-tmp 0.0)
		    (d+ 60.0 h-tmp))
		  h-tmp))))
    (sb-cga:vec h s v)))

(defun rgb->hsv* (rgb)
  (rgb->hsv (elt rgb 0) (elt rgb 1) (elt rgb 2)))

(defun hsv->rgb (h s v)
  (declare (desired-type h s v))
  (let* ((chroma (d* v s))
	 (delta (d- v chroma))
	 (hp (d/ h 60.0))
	 (x (d* chroma (if (evenp (floor hp))
			   (d- hp (desired (floor hp)))
			   (d- 1.0 (d- hp (desired (floor hp)))))))
	 (rgb (cond
		((epsilon= s 0.0)
		 (vec4 0.0 0.0 0.0))
		((d<= 0.0 hp 1.0)
		 (vec4 chroma x 0.0))
		((d<= 1.0 hp 2.0)
		 (vec4 x chroma 0.0))
		((d<= 2.0 hp 3.0)
		 (vec4 0.0 chroma x))
		((d<= 3.0 hp 4.0)
		 (vec4 0.0 x chroma))
		((d<= 4.0 hp 5.0)
		 (vec4 x 0.0 chroma))
		((d<= 5.0 hp 6.0)
		 (vec4 chroma 0.0 x)))))
    (map 'vec4 #'(lambda (a) (d+ a delta)) rgb)))

(defun hsv->rgb* (hsv)
  (hsv->rgb (elt hsv 0) (elt hsv 1) (elt hsv 2)))

(definline combine-color-float (fn color-1 color-2)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec4 color-1 color-2))
  (declare (function fn))
  (map 'vec4 fn color-1 color-2))

(defgeneric mix-color (color-1 color-2 weight))

(defmethod mix-color ((color-1 integer) (color-2 integer) weight)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare ((unsigned-byte 64) color-1 color-2))
  (combine-color-float #'(lambda (a b) (dlerp weight a b))
		       (int->vec4 color-1)
		       (int->vec4 color-2)))

(defmethod mix-color ((color-1 vector) (color-2 vector) weight)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec4 color-1 color-2))
  (combine-color-float #'(lambda (a b) (dlerp weight a b))
       color-1
       color-2))

(defmethod mix-color ((color-1 integer) (color-2 vector) weight)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare ((unsigned-byte 64) color-1))
  (declare (vec4 color-2))
  (combine-color-float #'(lambda (a b) (dlerp weight a b))
       (int->vec4 color-1)
       color-2))

(defmethod mix-color ((color-1 vector) (color-2 integer) weight)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare ((unsigned-byte 64) color-2))
  (declare (vec4 color-1))
  (combine-color-float #'(lambda (a b) (dlerp weight a b))
       color-1
       (int->vec4 color-2)))

(defun subtract-color-fn ()
  (lambda (a b) (alexandria:clamp (d- a b) 0.0 1.0)))

(defun multiply-color-fn ()
  (lambda (a b) (d* a b)))

(defun sum-color-fn ()
  (lambda (a b) (alexandria:clamp (d+ a b) 0.0 1.0)))

(defun max-color-fn ()
  (lambda (a b) (dmax a b)))

(defgeneric subtract-color (color-1 color-2))

(defmethod subtract-color ((color-1 integer) (color-2 integer))
  (declare ((unsigned-byte 64) color-1 color-2))
  (combine-color-float (the function (subtract-color-fn))
		       (int->vec4 color-1)
		       (int->vec4 color-2)))

(defmethod subtract-color ((color-1 vector) (color-2 vector))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec4 color-1 color-2))
  (combine-color-float (the function (subtract-color-fn))
       color-1
       color-2))

(defmethod subtract-color ((color-1 integer) (color-2 vector))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare ((unsigned-byte 64) color-1))
  (declare (vec4 color-2))
  (combine-color-float (the function (subtract-color-fn))
       (int->vec4 color-1)
       color-2))

(defmethod subtract-color ((color-1 vector) (color-2 integer))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare ((unsigned-byte 64) color-2))
  (declare (vec4 color-1))
  (combine-color-float (the function (subtract-color-fn))
       color-1
       (int->vec4 color-2)))

(defmacro gen-combine-color (name fn)
  (let ((function-name (alexandria:format-symbol t "~@:(~a-color~)" name)))
    `(progn
       (defgeneric ,function-name (color-1 color-2))
       (defmethod ,function-name ((color-1 integer) (color-2 integer))
	 (declare (optimize (debug 0) (safety 0) (speed 3)))
	 (declare ((unsigned-byte 64) color-1 color-2))
	 (combine-color-float (the function ,fn)
			      (int->vec4 color-1)
			      (int->vec4 color-2)))
       (defmethod ,function-name ((color-1 vector) (color-2 vector))
	 (declare (optimize (debug 0) (safety 0) (speed 3)))
	 (declare (vec4 color-1 color-2))
	 (combine-color-float (the function ,fn) color-1 color-2))
       (defmethod ,function-name ((color-1 integer) (color-2 vector))
	 (declare (optimize (debug 0) (safety 0) (speed 3)))
	 (declare ((unsigned-byte 64) color-1))
	 (declare (vec4 color-2))
	 (combine-color-float (the function ,fn) (int->vec4 color-1)
			      color-2))
       (defmethod ,function-name ((color-1 vector) (color-2 integer))
	 (declare (optimize (debug 0) (safety 0) (speed 3)))
	 (declare ((unsigned-byte 64) color-2))
	 (declare (vec4 color-1))
	 (combine-color-float (the function ,fn) color-1
			      (int->vec4 color-2))))))

(gen-combine-color multiply (multiply-color-fn))

(gen-combine-color maximum (max-color-fn))

(gen-combine-color sum (sum-color-fn))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gradient-color ()
    ((intensity
      :initform 0e0
      :initarg :intensity
      :accessor intensity)
     (color
      :initform §c000000ff
      :initarg :color
      :accessor color)))

  (defmethod make-load-form ((object gradient-color) &optional environment)
    (make-load-form-saving-slots object
				 :slot-names '(intensity color)
				 :environment environment))

  (defmethod print-object ((object gradient-color) stream)
    (print-unreadable-object (object stream :type nil :identity nil)
      (format stream "~f -> ~a" (intensity object) (color object))))

  (defun make-gradient-color (intensity color)
    (make-instance 'gradient-color :intensity intensity :color color))

  (defclass gradient ()
    ((colors
      :initform '()
      :initarg :colors
      :accessor colors)))

  (defmethod make-load-form ((object gradient) &optional environment)
    (make-load-form-saving-slots object
				 :slot-names '(colors)
				 :environment environment))

  (defmethod initialize-instance :after ((object gradient) &key &allow-other-keys)
     (let ((new-colors '())) ;; no same intensity
       (loop for c in (colors object) do
	    (pushnew c new-colors :key #'intensity :test #'num:epsilon=))
       (setf (colors object) new-colors))
     (let ((max (reduce #'max (colors object) :key #'intensity)))
      (loop for i in (colors object) do
	   (setf (intensity i) (d/ (intensity i) max)))
      (setf (colors object) (sort (colors object) #'< :key #'intensity))))

  (defmethod print-object ((object gradient) stream)
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~{~a~%~}" (colors object))))

  (defgeneric pick-color (object intensity))

  (defgeneric add-color (object new-color))

  (defmethod pick-color ((object gradient) intensity)
    (with-accessors ((colors colors)) object
      (let* ((actual-intensity (alexandria:clamp intensity 0.0 1.0))
	     (interval-pos (position-if #'(lambda (a) (d>= a actual-intensity)) colors
					:key #'intensity))
	     (interval (if (= interval-pos 0)
			   (list 0 1)
			   (list (1- interval-pos) interval-pos)))
	     (m (d/ 1.0 (d- (intensity (elt colors (second interval)))
			    (intensity (elt colors (first interval))))))
	     (q (d- 1.0 (d* m (intensity (elt colors (second interval))))))
	     (weight (d+ q (d* m actual-intensity))))
	(map 'vec4 #'(lambda (a b) (dlerp weight a b))
	     (color (elt colors (first interval)))
	     (color (elt colors (second interval)))))))

  (defmethod add-color ((object gradient) new-color)
    (let ((new-colors (pushnew new-color
                               (colors object)
                               :key #'intensity
                               :test #'num:epsilon=)))
      (setf (colors object) new-colors))
    (let ((max (reduce #'max (colors object) :key #'intensity)))
      (loop for i in (colors object) do
	   (setf (intensity i) (d/ (intensity i) max)))
      (setf (colors object) (sort (colors object) #'< :key #'intensity))))


  (defun make-gradient (&rest colors)
    (make-instance 'gradient :colors colors)))

(alexandria:define-constant +rainbow-gradient+ (make-gradient
						(make-gradient-color 1.0 §cffdd00ff)
						(make-gradient-color 0.66 §c00ff00ff)
						(make-gradient-color 0.33 §cff0000ff)
						(make-gradient-color 0.0 §c00000ff))
  :test #'(lambda (a b) (declare (ignore a b)) t))

(alexandria:define-constant +grayscale-gradient+ (make-gradient
						  (make-gradient-color 0.0 §c000000ff)
						  (make-gradient-color 1.0 §cffffffff))
  :test #'(lambda (a b) (declare (ignore a b)) t))

(alexandria:define-constant +standard-sky-sunny-color+ §c6ec0ffff :test #'vec4=)

(alexandria:define-constant +skydome-gradient+ (make-gradient
						(make-gradient-color 0.0 §c000000ff)
						(make-gradient-color (* 0.04167 3) §cffa92fff)
						(make-gradient-color (* 0.04167 6) §cf86161ff)
						(make-gradient-color (* 0.04167 8) +standard-sky-sunny-color+)
						(make-gradient-color (* 0.04167 19) +standard-sky-sunny-color+)
						(make-gradient-color (* 0.04167 20) §cf86161ff)
						(make-gradient-color (* 0.04167 21) §cffa92fff)
						(make-gradient-color (* 0.04167 24) §c000000ff))
  :test #'(lambda (a b) (declare (ignore a b)) t))
