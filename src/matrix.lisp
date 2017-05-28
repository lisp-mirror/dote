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

(in-package :matrix)

(alexandria:define-constant +width-keyword+ :width :test #'eq)

(alexandria:define-constant +height-keyword+ :height :test #'eq)

(alexandria:define-constant +data-keyword+ :data :test #'eq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass matrix ()
    ((data
      :initarg :data
      :initform (misc:make-array-frame 0)
      :accessor data
      :type vector)
     (width
      :initarg :width
      :initform 0
      :accessor width
      :type integer)
     (height
      :initarg :height
      :initform 0
      :accessor height
      :type integer)))

  (defun gen-matrix-frame (width height &optional (value nil))
    (when (< width 0)
      (error 'invalid-size :size width))
    (when (< height 0)
      (error 'invalid-size :size height))
    (make-instance 'matrix
		   :data (misc:make-array-frame (* width height) value t t)
		   :width width
		   :height height))

  (defun make-matrix (width height &optional (value nil))
    (when (< width 0)
      (error 'invalid-size :size width))
    (when (< height 0)
      (error 'invalid-size :size height))
    (make-instance 'matrix
		   :data (misc:make-fresh-array (* width height) value t t)
		   :width width
		   :height height))

  (define-condition make-matrix-too-much-elements (error)
    ((w
      :initarg :w
      :reader w)
     (h
      :initarg :h
      :reader h)
     (data
      :initarg :data
      :reader data))
    (:report (lambda (condition stream)
	       (format stream
		       "Asked for a ~aX~a matrix but you provided ~a elements."
		       (w condition) (h condition) (length (data condition)))))
    (:documentation "Error when too much elements was provided"))

  (define-condition make-matrix-too-fews-elements (error)
    ((w
      :initarg :w
      :reader w)
     (h
      :initarg :h
      :reader h)
     (data
      :initarg :data
      :reader data))
    (:report (lambda (condition stream)
	       (format stream
		       "Asked for a ~aX~a matrix but you provided ~a elements."
		       (w condition) (h condition) (length (data condition)))))
    (:documentation "Error when too fews elements was provided"))

  (defun %make-matrix* (w h data)
    (let ((vec-data (misc:make-fresh-array (* w h) nil t t))
	  (actual-data
	   (restart-case
	       (progn
		 (when (> (length data) (* w h))
		   (error 'make-matrix-too-much-elements :w w :h h :data data))
		 (when (< (length data) (* w h))
		   (error 'make-matrix-too-fews-elements :w w :h h :data data))
		 data)
	     (trim (v)
	       v))))
      (setf vec-data (map-into vec-data #'(lambda (a) a) actual-data))
      (make-instance 'matrix
		     :height h
		     :width  w
		     :data   vec-data)))

  (defun make-matrix* (w h &rest data)
    (let ((vec-data (misc:make-fresh-array (* w h) nil t t))
	  (actual-data
	   (restart-case
	       (progn
		 (when (> (length data) (* w h))
		   (error 'make-matrix-too-much-elements :w w :h h :data data))
		 (when (< (length data) (* w h))
		   (error 'make-matrix-too-fews-elements :w w :h h :data data))
		 data)
	     (trim (v)
	       v))))
      (setf vec-data (map-into vec-data #'(lambda (a) a) actual-data))
      (make-instance 'matrix
		     :height h
		     :width  w
		     :data   vec-data)))

  (defun make-matrix-with-trim (w h filler &rest data)
    (make-matrix-with-trim* w h filler data))

  (defun make-matrix-with-trim* (w h filler data)
    (handler-bind ((make-matrix-too-much-elements
		    #'(lambda (c)
			(invoke-restart 'trim (subseq (data c) 0 (* (w c) (h c))))))
		   (make-matrix-too-fews-elements
		    #'(lambda (c)
			(let ((new-data (misc:make-fresh-array (* w h) filler t t)))
			  (loop for i from 0 below (length (data c)) do
			       (setf (elt new-data i) (elt (data c) i)))
			  (invoke-restart 'trim new-data)))))
      (%make-matrix* w h data)))

  (defmacro define-matrix ((w h &optional (filler 0.0)) &body data)
    `(make-matrix-with-trim ,w ,h ,filler ,@data))

  (defmacro with-matrix-op-typecase ((element) lambda-plist)
    (macrolet ((add-if-t (el body)
		 `(if (getf lambda-plist ,el nil)
		      (list ,body)
		      nil)))
      `(etypecase ,element
	 ,@(append
	    (add-if-t :vec
		      `(sb-cga:vec (map 'sb-cga:vec ,@(getf lambda-plist :vec nil))))
	    (add-if-t :vec4
		      `(vec4 (map 'vec4 ,@(getf lambda-plist :vec4 nil))))
	    (add-if-t :ivec4
		      `(ivec4 (map 'ivec4 ,@(getf lambda-plist :ivec4 nil))))
	    (add-if-t :ubvec4
		      `(ubvec4 (map 'ubvec4 ,@(getf lambda-plist :ubvec4 nil))))
	    (add-if-t :vec2
		      `(vec2 (map 'vec2 ,@(getf lambda-plist :vec2 nil))))
	    (add-if-t :vector
		      `(vector (map 'vector ,@(getf lambda-plist :vector nil))))
	    (add-if-t :list
		      `(list (map 'list ,@(getf lambda-plist :list nil))))
	    (add-if-t :single-float
		      `(single-float ,@(getf lambda-plist :single-float nil)))
	    (add-if-t :unsigned-byte-8
		      `((unsigned-byte 8) ,@(getf lambda-plist :unsigned-byte-8 nil)))
	    (add-if-t :number
		      `(number ,@(getf lambda-plist :number nil)))
	    (add-if-t :fixnum
		      `(fixnum ,@(getf lambda-plist :fixnum nil)))))))

  (defmethod clone ((object matrix))
    (make-instance 'matrix
		   :data (alexandria:copy-array (data object))
		   :width (width object)
		   :height (height object)))

  (defmethod marshal:class-persistant-slots ((object matrix))
    '(data width height))

  (defmethod to-sexp ((object matrix))
    (concatenate 'list
		 (list +width-keyword+ (width object)
		       +height-keyword+ (height object)
		       +data-keyword+)
		 (list (map 'list #'to-sexp (data object)))))

  (defmethod from-sexp ((object matrix) sexp)
    (setf (width object) (getf sexp +width-keyword+))
    (setf (height object) (getf sexp +height-keyword+))
    (setf (data object) (map 'vector #'identity (getf sexp +data-keyword+))))

  (defgeneric matrix-elt (object row col))

  (defgeneric (setf matrix-elt) (object row col value))

  (defgeneric matrix-elt-ubvec4 (object row col))

  (defgeneric (setf matrix-elt-ubvec4) (object row col value))

  (defgeneric matrix-elt-vec4 (object row col))

  (defgeneric (setf matrix-elt-vec4) (object row col value))

  (defgeneric matrix-elt* (object coord))

  (defgeneric nspecialize-data-array (object &optional type))

  (defgeneric specialize-data-array (object &optional type))

  (defgeneric valid-index-p (object r c))

  (defgeneric swap-elements (object row column row2 column2 &key destructive))

  (defgeneric map-matrix (object predicate))

  (defgeneric nmap-matrix-xy (object predicate))

  (defgeneric submatrix (object x y w h &optional value))

  (defgeneric rotate-matrix (object angle &key fill-value pivot repeat rounding-fn))

  (defgeneric rotate-matrix-w-repeat (object angle &key fill-value pivot rounding-fn))

  (defgeneric scale-matrix (object scale-x scale-y))

  (defgeneric scale-matrix-nearest (object scale-x scale-y))

  (defgeneric copy-matrix (object))

  (defgeneric rotate-matrix-180-degree (object fill-value pivot))

  (defgeneric rotate-matrix-90-degree-ccw (object fill-value pivot))

  (defgeneric rotate-matrix-90-degree-cw (object fill-value pivot))

  (defgeneric translate-matrix (object fill-value dx dy))

  (defgeneric data-as-list (object))

  (defgeneric element-type (object))

  (defgeneric row->sequence (object row &optional output-spec))

  (defgeneric %matrix-incf (matrix x y delta))

  (defgeneric h-mirror-matrix (object))

  (defgeneric v-mirror-matrix (object))

  (defgeneric matrix-mult (a b))

  (defgeneric sample@ (object x y &key
				    clamp interpolation interpolate-fn
				    behaivour-on-border-fn))

  (defgeneric pixel@ (object x y))

  (defgeneric (setf pixel@) (colorlist object x y))

  (defgeneric (setf sample@) (colorlist object x y))

  (defgeneric matrix-hline (object x y width color))

  (defgeneric matrix-vline (object x y height color))

  (defgeneric matrix-rect (object x y width height color))

  (defgeneric matrix-line (object start end color))

  (defgeneric pixel-inside-p (object x y))

  (defgeneric element@-inside-p (object x y))

  (defgeneric flood-fill (object x y &key
				       tolerance max-iteration randomize-growth
				       position-acceptable-p-fn))
  (defgeneric flood-fill* (object x y &key
					tolerance-fn
					max-iteration
					randomize-growth
					position-acceptable-p-fn))

  (defgeneric apply-kernel (object kernel &key round-fn))

  (defgeneric papply-kernel (object kernel &key round-fn))

  (defgeneric gaussian-blur (object round-fn radius))

  (defgeneric gaussian-blur-separated (object round-fn radius))

  (defgeneric pgaussian-blur-separated (object round-fn radius))

  (defgeneric psobel-edge-detection (object))

  (defgeneric pgradient-image (object &key round-fn))

  (defgeneric gradient-image (object &key round-fn))

  (defgeneric pblit-matrix (src dest x-dst y-dst &key blend-fn))

  (defgeneric blit-matrix (src dest x-dst y-dst &key blend-fn))

  (defgeneric pmatrix-blit (source destination x y
			    &key transparent-value function-blend))

  (defgeneric submatrix= (object sample x y &key test))

  (defgeneric matrix= (a b &key test))

  (defmethod matrix-elt ((object matrix) row col)
    (declare (optimize (speed 1) (safety 0) (debug 0)))
    (declare (matrix object))
    (declare (fixnum row col))
    (elt (the (simple-array * (*)) (data object))
	 (f+ (f* (the fixnum (width object)) row) col)))

  (defmethod matrix-elt-vec4 ((object matrix) row col)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (declare (matrix object))
    (declare (fixnum row col))
    (elt (the (simple-array vec4 (*)) (data object))
	 (f+ (f* (the fixnum (width object)) row) col)))

  (defmethod matrix-elt-ubvec4 ((object matrix) row col)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (declare (matrix object))
    (declare (fixnum row col))
    (elt (the (simple-array ubvec4 (*)) (data object))
	 (f+ (f* (the fixnum (width object)) row) col)))


  (defmethod (setf matrix-elt) (val (object matrix) row col)
    (declare (optimize (speed 1) (safety 0) (debug 0)))
    (declare (matrix object))
    (declare (fixnum row col))
    (setf (elt (the (simple-array * (*)) (data object))
	       (f+ (f* (the fixnum (width object)) row) col))
	  val))

  (defmethod (setf matrix-elt-vec4) (val (object matrix) row col)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (declare (matrix object))
    (declare (fixnum row col))
    (declare (vec4 val))
    (setf (elt (the (simple-array vec4 (*)) (data object))
	       (f+ (f* (the fixnum (width object)) row) col))
	  val))

  (defmethod (setf matrix-elt-ubvec4) (val (object matrix) row col)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (declare (matrix object))
    (declare (fixnum row col))
    (declare (ubvec4 val))
    (setf (elt (the (simple-array ubvec4 (*)) (data object))
	       (f+ (f* (the fixnum (width object)) row) col))
	  val)))

(defmethod matrix-elt* ((object matrix) (coord sequence))
  (elt (data object) (+ (* (width object) (elt coord 1)) (elt coord 0))))

(define-condition invalid-size (error)
  ((size
    :initarg :size
    :reader size))
  (:report (lambda (condition stream)
	     (format stream "~a size not valid" (size condition))))
  (:documentation "Error when matrix size is invalid"))

(defun gen-neighbour-position (x y &key (add-center t))
  "
if add-center is t
	       +--+--+--+
	       |6 |4 |7 |
	       +--+--+--+
	       |2 |0 |1 |
	       +--+--+--+
	       |8 |3 |5 |
	       +--+--+--+
else
	       +--+--+--+
	       |5 |3 |6 |
	       +--+--+--+
	       |1 |  |0 |
	       +--+--+--+
	       |7 |2 |4 |
	       +--+--+--+

"
  (append (and add-center (list (list x y)))
	  (list
	   (list (1+ x) y)
	   (list (1- x) y)
	   (list x (1+ y))
	   (list x (1- y))
	   (list (1+ x) (1+ y))
	   (list (1- x) (1- y))
	   (list (1+ x) (1- y))
	   (list (1- x) (1+ y)))))

(defun gen-4-neighbour-counterclockwise (x y &key (add-center t))
  "note: no bounds checking is done"
  (append (and add-center (list (list x y)))
	  (list
	   (list (1+ x) y)
	   (list x (1- y))
	   (list (1- x) y)
	   (list x (1+ y)))))

(defun gen-neighbour-position-in-box (x y w-offset h-offset &key (add-center t))
  "note: no bounds checking is done"
  (let ((results (misc:make-fresh-array 0 nil 'ivec2:ivec2 nil)))
    (loop for x-box from (- x (floor (/ w-offset 2))) to (+ x (floor (/ w-offset 2))) by 1 do
	 (loop for y-box from (- y (floor (/ h-offset 2))) to (+ y (floor (/ h-offset 2))) by 1 do
	      (when (or (/= x-box x)
			(/= y-box y)
			add-center)
		(vector-push-extend (ivec2:ivec2 x-box y-box) results))))
    results))

(defun gen-ring-box-position (x y w-offset h-offset)
  "note: no bounds checking is done, inefficient also"
  (let ((results (gen-neighbour-position-in-box x y w-offset h-offset)))
    (remove-if #'(lambda (a) (and (/= (abs (- (elt a 0) x)) (/ w-offset 2))
                                  (/= (abs (- (elt a 1) y)) (/ h-offset 2))))
               results)))

(defmacro with-check-borders ((x y x-bond y-bond w h) then else)
  `(if (and
	(>= (the fixnum ,x) (the fixnum ,x-bond))
	(<= (the fixnum ,x) (f+ (the fixnum ,x-bond) (the fixnum ,w)))
	(>= (the fixnum ,y) (the fixnum ,y-bond))
	(<= (the fixnum ,y) (f+ (the fixnum ,y-bond) (the fixnum ,h))))
       ,then
       ,else))

(defmacro with-check-matrix-borders ((matrix x y) &body then)
  `(with-check-borders (,x ,y 0 0 (f- (the fixnum (width ,matrix)) 1)
			   (f- (the fixnum (height ,matrix)) 1))
     (progn ,@then)
     nil))

(defmacro with-check-matrix-borders-then-else ((matrix x y) then else)
  `(with-check-borders (,x ,y 0 0 (f- (the fixnum (width ,matrix)) 1)
			   (f- (the fixnum (height ,matrix)) 1))
     ,then
     ,else))

(defmethod print-object ((object matrix) stream)
  (format stream "~a ~a~%" (width object) (height object))
  (dolist (row (misc:split-into-sublist (data-as-list object) (width object)))
    (format stream "~{~5,2:@<~a~>~}~%"  row)))

(defmacro matrix-incf (matrix x y delta)
  `(%matrix-incf ,matrix ,x ,y ,delta))

(defmethod nspecialize-data-array ((object matrix) &optional (type (element-type object)))
  (when type
     (let* ((start-el (elt (data object) 0))
   	   (new-data (misc:make-array-frame (length (data object)) start-el type t)))
       (setf (data object) new-data)))
  object)

(defmethod specialize-data-array ((object matrix) &optional (type (element-type object)))
  (nspecialize-data-array (clone object) type))

(defmethod valid-index-p ((object matrix) r c)
  (with-accessors ((w width) (h height)) object
    (and
     (>= c 0)
     (< c w)
     (>= r 0)
     (< r h))))

(defmethod %matrix-incf (matrix x y (delta number))
  (with-check-matrix-borders (matrix x y)
    (setf (matrix-elt matrix y x)
	  (let ((element (matrix-elt matrix y x)))
	    (with-matrix-op-typecase (element)
	      (:ivec4
	       ((lambda (a)
		  (declare (optimize (speed 1) (safety 0) (debug 0)))
		  (declare (ivec4-type a))
		  (+ a delta)) element)
	      :ubvec4
	       ((lambda (a)
		  (declare (optimize (speed 1) (safety 0) (debug 0)))
		  (declare (ubvec4-type a))
		  (+ a delta)) element)
	       :list
	       ((lambda (a) (+ a delta)) element)
	       :number
	       ((+ element delta))))))))

(defmethod %matrix-incf (matrix x y (delta sequence))
  (with-check-matrix-borders (matrix x y)
    (setf (matrix-elt matrix y x)
	  (let ((element (matrix-elt matrix y x)))
	    (with-matrix-op-typecase (element)
		(:ivec4
		 ((lambda (a b)
		    (declare (optimize (speed 1) (safety 0) (debug 0)))
		    (declare (ivec4-type a))
		    (+ a b)) element delta)
		 :ubvec4
		 ((lambda (a b)
		    (declare (optimize (speed 1) (safety 0) (debug 0)))
		    (declare (ivec4-type a))
		    (+ a b)) element delta)
		:vec4
		 ((lambda (a b)
		    (declare (optimize (speed 3) (safety 0) (debug 0)))
		    (declare (desired-type a))
		    (d+ a b)) element delta)
		:list
		 ((lambda (a b)
		    (declare (desired-type a))
		    (+ a b)) element delta)))))))

(defmacro loop-matrix ((matrix x y &optional loop-name) &body body)
  `(loop named ,loop-name for ,y fixnum from 0 below (height ,matrix) do
	(loop for ,x fixnum from 0 below (width ,matrix) do
	     ,@body)))

(defmacro loop-submatrix ((matrix x y from-x from-y below-x below-y) &body body)
  `(loop for ,y fixnum from ,from-y below ,below-y do
	(loop for ,x fixnum from ,from-x below ,below-x do
	     (with-check-matrix-borders (,matrix ,x ,y)
	       ,@body))))

(defun %subdivide (size slices-num)
  (multiple-value-bind (int frac)
      (floor (/ size slices-num))
    (let* ((last-slice (+ (* frac slices-num) int))
	   (slices (misc:make-fresh-array (1+ slices-num) int 'fixnum t))
	   (res    (misc:make-fresh-array (1+ slices-num) 0 'fixnum t)))
      (setf (alexandria:last-elt  slices) last-slice
	    (alexandria:first-elt slices) 0)
      (loop for i from 1 below (length res) do
	   (setf (elt res i) (reduce #'+ (subseq slices 0 (1+ i)))))
      res)))

(defmacro ploop-matrix ((matrix x y &key (bindings nil) (receive-results-fun 'identity))
			  &body body)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (alexandria:with-gensyms (channel slices ptr i)
    (let ((bindings-sym (alexandria:make-gensym-list (length bindings))))
      `(let* ((,channel (lparallel:make-channel))
	      (,slices  (%subdivide (height ,matrix) config:*workers-number*)))
	 (declare (fixnum config:*workers-number*))
	 (declare ((simple-array fixnum (*)) ,slices))
	 (loop for ,i fixnum from 0 below (1- (length ,slices)) do
	      (lparallel:submit-task
	       ,channel
	       (let ,(loop for j from 0 below (length bindings) collect
	    		  (list (nth j bindings-sym) (nth j bindings)))
	    	 (lambda (,matrix ,ptr)
	    	   (let ,(loop for k from 0 below (length bindings) collect
	    		      (list (nth k bindings) (nth k bindings-sym)))
	    	     (loop-submatrix (,matrix ,x ,y
					      0 (elt ,slices ,ptr)
	    				      (width ,matrix) (elt ,slices (f+ ,ptr 1)))
	    		,@body))))
	      ,matrix ,i))
	 (loop repeat config:*workers-number* do
	      (funcall (function ,receive-results-fun)
		       (lparallel:receive-result ,channel)))))))

(defmacro psetf-matrix-elt (mat row col val)
  `(progn
     (lparallel.queue:pop-queue parallel-utils:*parallel-setf-queue*)
     (prog1
	 (setf (elt (data ,mat)
		    (+ (the fixnum (* (the fixnum (width ,mat)) (the fixnum ,row))) (the fixnum ,col)))
	       ,val)
       (lparallel.queue:push-queue t parallel-utils:*parallel-setf-queue*))))

(defun i-to-xy (matrix i)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (matrix matrix))
  (declare (fixnum i))
  (let ((l (the fixnum (width matrix))))
    (declare (fixnum l))
    (values (the fixnum (mod i l)) (the fixnum (truncate (/ i l))))))

(defun i-to-xy-submatrix (i x-start y-start w)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum i x-start y-start w))
  (let ((y (f+ y-start (the fixnum (truncate (/ i w)))))
	(x (f+ x-start (mod i w))))
    (values x y)))

(defmacro ploop-submatrix ((matrix x y x-start y-start below-x below-y
				     &key (bindings nil) (receive-results-fun 'identity))
			     &body body)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (alexandria:with-gensyms (channel slices ptr i)
    (let ((bindings-sym (alexandria:make-gensym-list (length bindings))))
      `(let* ((,channel (lparallel:make-channel))
	      (,slices  (%subdivide (f- ,below-y ,y-start) config:*workers-number*)))
	 (declare (fixnum config:*workers-number*))
	 (declare ((simple-array fixnum (*)) ,slices))
	 (loop for ,i fixnum from 0 below (1- (length ,slices)) do
	      (lparallel:submit-task
	       ,channel
	       (let ,(loop for j from 0 below (length bindings) collect
	    		  (list (nth j bindings-sym) (nth j bindings)))
	    	 (lambda (,matrix ,ptr)
	    	   (let ,(loop for k from 0 below (length bindings) collect
	    		      (list (nth k bindings) (nth k bindings-sym)))
	    	     (loop-submatrix (,matrix ,x ,y
					      ,x-start (elt ,slices ,ptr)
	    				      ,below-x (elt ,slices (f+ ,ptr 1)))
	    		,@body))))
	      ,matrix ,i))
	 (loop repeat config:*workers-number* do
	      (funcall (function ,receive-results-fun)
		       (lparallel:receive-result ,channel)))))))

(defmethod swap-elements ((object matrix) row column row2 column2 &key (destructive t))
  (let ((res (if destructive object (copy-matrix object))))
    (with-check-matrix-borders (res column row)
      (with-check-matrix-borders (res column2 row2)
	(let ((saved (matrix-elt res row column)))
	  (setf (matrix-elt res row column) (matrix-elt res row2 column2)
		(matrix-elt res row2 column2) saved))))
    res))

(defmethod nmap-matrix-xy ((object matrix) predicate)
  (loop-matrix (object c r)
     (funcall predicate c r (matrix-elt object r c))))

(defmethod map-matrix ((object matrix) predicate)
  (make-instance 'matrix
		 :data (map 'vector #'(lambda (el) (funcall predicate el))
			    (data object))
		 :width (width object)
		 :height (height object)))

(defmethod submatrix ((object matrix) x y w h &optional (value 0))
  (let ((res (gen-matrix-frame w h value)))
    (loop
       for i from x below (+ x w)
       for i1 from 0 below w do
	 (loop
	    for j from y below (+ y h)
	    for j1 from 0 below h do
	      (with-check-matrix-borders (object i j)
		(setf (matrix-elt res j1 i1)
		      (matrix-elt object j i)))))
    res))

(defmethod copy-matrix ((object matrix))
  (submatrix object 0 0 (width object) (height object)))

(defun interpolate (weight px1 px2)
  (with-matrix-op-typecase (px1)
     (:ivec4
      ((lambda (c1 c2)
	 (declare (optimize (speed 1) (safety 0) (debug 0)))
	 (declare (ivec4-type c1 c2))
	 (round (dlerp weight (desired c1) (desired c2)))) px1 px2)
      :ubvec4
      ((lambda (c1 c2)
	 (declare (optimize (speed 1) (safety 0) (debug 0)))
	 (declare (ubvec4-type c1 c2))
	 (round (dlerp weight (desired c1) (desired c2)))) px1 px2)
      :vec4
      ((lambda (c1 c2)
	 (declare (optimize (speed 3) (safety 0) (debug 0)))
	 (declare (desired-type c1 c2))
	 (dlerp weight c1 c2)) px1 px2)
      :vec3
      ((lambda (c1 c2) (dlerp weight c1 c2)) px1 px2)
      :single-float
      ((dlerp weight px1 px2))
      :number
      ((alexandria:lerp weight px1 px2)))))

(defun repeat-periodic-coord (val max)
  (if (< val 0)
      (+ max
	 (* max
	    (- (/ val max)
	       (truncate (/ val max)))))
      (+ (mod (floor val) max)
	 (- val
	    (truncate val)))))

(defun confine-coord (val max)
  (alexandria:clamp val (desired 0.0) (desired max)))

(defun bilinear-interpolation (matrix x y
			       &key (interpolate-fn #'interpolate)
				 (behaivour-on-border-fn #'repeat-periodic-coord))
  ;; a          b
  ;; +----------+
  ;; |          |
  ;; |          |
  ;; +----------+
  ;; d          c
  (let* ((actual-x  x)
	 (actual-y  y)
	 (floor-x   (floor (funcall behaivour-on-border-fn
				    (floor actual-x) (width matrix))))
	 (floor-y   (floor (funcall behaivour-on-border-fn
				    (floor actual-y) (height matrix))))
	 (ceiling-x (ceiling (funcall behaivour-on-border-fn
				      (ceiling actual-x) (width matrix))))
	 (ceiling-y (ceiling (funcall behaivour-on-border-fn
				      (ceiling actual-y) (height matrix))))
	 (dx (- actual-x (floor x)))
	 (dy (- actual-y (floor y)))
	 (a (matrix-elt matrix floor-y floor-x))
	 (b (matrix-elt matrix floor-y ceiling-x))
	 (c (matrix-elt matrix ceiling-y ceiling-x))
	 (d (matrix-elt matrix ceiling-y floor-x))
	 (inter-x1 (funcall interpolate-fn dx a b))
	 (inter-x2 (funcall interpolate-fn dx d c))
	 (inter-y  (funcall interpolate-fn dy inter-x1 inter-x2)))
    inter-y))

(defmethod translate-matrix ((object matrix) fill-value dx dy)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum dx dy))
  (let* ((w   (width object))
	 (h   (height object))
	 (res (gen-matrix-frame w h fill-value)))
    (declare (fixnum w h))
    (loop-matrix (object x y)
       (let* ((new-x (f+ x dx))
	      (new-y (f+ y dy)))
	 (with-check-matrix-borders (res new-x new-y)
	   (setf (matrix-elt res new-y new-x) (matrix-elt object y x)))))
    res))

(defmethod rotate-matrix-180-degree ((object matrix) fill-value pivot)
  (let* ((w        (width object))
	 (h        (height object))
	 (res (gen-matrix-frame w h fill-value)))
    (declare (fixnum w h))
    (ploop-matrix (object x y)
      (let* ((point  (vec2 (desired x) (desired y)))
	     (vpivot (vec2 (desired (elt pivot 0)) (desired (elt pivot 1))))
	     (rev    (vec2- (vec2+ (vec2-negate (vec2- point vpivot)) vpivot)
			    (vec2 1.0 1.0)))
	     (rev-x  (floor (elt rev 0)))
	     (rev-y  (floor (elt rev 1))))
	(with-check-matrix-borders (res rev-x rev-y)
	  (setf (matrix-elt res rev-y rev-x) (matrix-elt object y x)))))
    res))

(defmethod rotate-matrix-90-degree-ccw ((object matrix) fill-value pivot)
  (let* ((w        (floor (width object)))
	 (h        (floor (height object)))
	 (res (gen-matrix-frame w h fill-value)))
    (declare (fixnum w h))
    (ploop-matrix (res x y)
       (let* ((point  (vec2 (desired x) (desired y)))
	      (vpivot (vec2 (desired (elt pivot 0)) (desired (elt pivot 1))))
	      (rev    (vec2- point vpivot))
	      (rev-x  (floor (+ (elt vpivot 0) -1 (- (elt rev 1)))))
	      (rev-y  (floor (+ (elt vpivot 1)    (elt rev 0)))))
	 (with-check-matrix-borders (res rev-x rev-y)
	   (setf (matrix-elt res y x) (matrix-elt object rev-y rev-x)))))
      res))

(defmethod rotate-matrix-90-degree-cw ((object matrix) fill-value pivot)
  (let* ((w        (floor (width object)))
	 (h        (floor (height object)))
	 (res (gen-matrix-frame w h fill-value)))
    (declare (fixnum w h))
    (ploop-matrix (res x y)
       (let* ((point  (vec2 (desired x) (desired y)))
	      (vpivot (vec2 (desired (elt pivot 0)) (desired (elt pivot 1))))
	      (rev    (vec2- point vpivot))
	      (rev-x  (floor (+ (elt vpivot 0)    (elt rev 1))))
	      (rev-y  (floor (+ (elt vpivot 1) -1 (- (elt rev 0))))))
	 (with-check-matrix-borders (res rev-x rev-y)
	   (setf (matrix-elt res y x) (matrix-elt object rev-y rev-x)))))
      res))

(defmethod rotate-matrix ((object matrix) angle
			  &key (fill-value 0)
			    (pivot (list (/ (width object)  2)
					 (/ (height object) 2)))
			    (repeat nil)
			    (rounding-fn #'round))
  (cond
    ;; using  the next tree functions because  the  usual  approach below  for
    ;; rotating did not worked for me
    ((epsilon= (desired angle) 90.0)
     (rotate-matrix-90-degree-ccw object fill-value pivot))
    ((epsilon= (desired angle) -90.0)
     (rotate-matrix-90-degree-cw object fill-value pivot))
    ((epsilon= (dabs (desired angle)) 180.0)
     (rotate-matrix-180-degree object fill-value pivot))
    (repeat
	(rotate-matrix-w-repeat object angle :fill-value fill-value :pivot pivot
				:rounding-fn rounding-fn))
    (t
     (let ((res (gen-matrix-frame (width object) (height object) fill-value))
	   (act-angle (deg->rad angle)))
       (ploop-matrix (res x y)
	 (let* ((new-pixel
		 (2d-utils:2d-vector-translate
		  (2d-utils:2d-vector-rotate
		   (2d-utils:2d-vector-translate (list x y) (- (elt pivot 0)) (- (elt pivot 1)))
		   act-angle)
		  (elt pivot 0)
		  (elt pivot 1)))
		(floor-x (floor (first new-pixel)))
		(floor-y (floor (second new-pixel)))
		(ceiling-x (ceiling (first new-pixel)))
		(ceiling-y (ceiling (second new-pixel))))
	   (with-check-matrix-borders (object floor-x floor-y)
	     (with-check-matrix-borders (object ceiling-x ceiling-y)
	       (let* ((px (bilinear-interpolation object
						  (elt new-pixel 0)
						  (elt new-pixel 1)
						  :interpolate-fn #'interpolate)))
		 (setf (matrix-elt res y x)
		       (if rounding-fn
			   (num-utils:round-all px :rounding-function rounding-fn)
			   px)))))))
	 res))))

(defmethod rotate-matrix-w-repeat ((object matrix) angle
				   &key (fill-value 0)
				     (pivot (list (/ (width object) 2)
						  (/ (height object) 2)))
				     (rounding-fn #'round))
  (let ((res (gen-matrix-frame (width object) (height object) fill-value))
	(act-angle (deg->rad angle)))
    (ploop-matrix (res x y)
       (let* ((new-pixel
	       (2d-utils:2d-vector-translate
		(2d-utils:2d-vector-rotate
		 (2d-utils:2d-vector-translate (list x y)
					       (- (first pivot))
					       (- (second pivot))) act-angle)
		(first pivot)
		(second pivot))))
	 (let* ((px  (bilinear-interpolation object
					     (elt new-pixel 0)
					     (elt new-pixel 1)
					     :interpolate-fn #'interpolate
					     :behaivour-on-border-fn #'repeat-periodic-coord)))
	   (setf (matrix-elt res y x)
		 (if rounding-fn
		     (num-utils:round-all px :rounding-function rounding-fn)
		     px)))))
    res))

(defmethod scale-matrix ((object matrix) scale-x scale-y)
  (let* ((old-width   (width object))
	 (old-height  (width object))
	 (dnew-width  (* scale-x old-width))
	 (dnew-height (* scale-y old-height))
	 (new-width   (floor dnew-width))
	 (new-height  (floor dnew-height))
	 (res         (gen-matrix-frame new-width new-height #(0 0 0 0))))
    (ploop-matrix (res x y)
       (let* ((y-frac (d/ (d+ (desired y) (d* 0.5 (d- 1.0 scale-y))) scale-y))
	      (sy     (floor y-frac))
	      (wy     (d- y-frac (desired sy)))
	      (x-frac (d/ (d+ (desired x) (d* 0.5 (d- 1.0 scale-x))) scale-x))
	      (sx     (floor x-frac))
	      (wx     (d- x-frac (desired sx)))
	      (floor-x (alexandria:clamp sx      0 (1- old-width)))
	      (ceil-x  (alexandria:clamp (1+ sx) 0 (1- old-width)))
	      (floor-y (alexandria:clamp sy      0 (1- old-height)))
	      (ceil-y  (alexandria:clamp (1+ sy) 0 (1- old-height)))
	      (a (matrix-elt object floor-y floor-x))
	      (b (matrix-elt object floor-y ceil-x))
	      (c (matrix-elt object ceil-y ceil-x))
	      (d (matrix-elt object ceil-y floor-x))
	      (inter-x1 (interpolate wx a b))
	      (inter-x2 (interpolate wx d c))
	      (inter-y  (interpolate wy inter-x1 inter-x2)))
	 (setf (matrix-elt res y x) (num-utils:round-all inter-y))))
    res))

(defmethod scale-matrix-nearest ((object matrix) scale-x scale-y)
  (let* ((new-width  (d* scale-x (desired (width object))))
	 (new-height (d* scale-y (desired (height object))))
	 (res        (gen-matrix-frame (floor new-width) (floor new-height) nil)))
    (ploop-matrix (res x y)
       (let* ((new-pixel (vec2 (d/ (desired x) scale-x)
			       (d/ (desired y) scale-y)))
	      (fx        (elt new-pixel 0))
	      (fy        (elt new-pixel 1))
	      (floor-x   (floor fx))
	      (floor-y   (floor fy))
	      (a (matrix-elt object floor-y floor-x)))
	 (setf (matrix-elt res y x) a)))
    res))

(defmethod data-as-list ((object matrix))
  (coerce (data object) 'list))

(defmethod element-type ((object matrix))
  (if (> (length (data object)) 0)
      (class-of (matrix-elt object 0 0))
      nil))

(defmethod row->sequence ((object matrix) row &optional (output-spec 'vector))
  (let ((the-row (misc:make-array-frame (width object))))
    (loop for i from 0 below (width object) do
	 (setf (elt the-row i) (matrix-elt object row i)))
    (ecase output-spec
      (vector the-row)
      (list (coerce the-row 'list)))))

(defmethod h-mirror-matrix ((object matrix))
  (let ((row-pivot (floor (/ (height object) 2))))
    (loop for y from 0 below row-pivot do
	 (loop for x from 0 below (width object) do
	      (let ((row-destination (- (1- (height object)) y)))
		(swap-elements object y x row-destination x :destructive t)))))
  object)

(defmethod v-mirror-matrix ((object matrix))
  (let ((col-pivot (floor (/ (width object) 2))))
    (loop for x from 0 below col-pivot do
	 (loop for y from 0 below (height object) do
	      (let ((col-destination (- (1- (width object)) x)))
		(swap-elements object y x y col-destination :destructive t)))))
  object)

(defmethod matrix-mult ((a matrix) (b matrix))
  (let ((res (gen-matrix-frame (width b) (height a))))
    (loop for x from 0 below (width res) do
	(loop for y from 0 below (height res) do
	     (setf (matrix-elt res y x)
		   (reduce #'+
			   (mapcar #'*
				   (loop for i from 0 below (width a) collect
					(matrix-elt a y i))
				   (loop for i from 0 below (height b) collect
					(matrix-elt b i x)))))))
    res))

(defmethod sample@ ((object matrix) x y
		    &key (clamp nil) (interpolation t) (interpolate-fn #'interpolate)
		      (behaivour-on-border-fn #'repeat-periodic-coord))
  (declare (desired-type x y))
  (let ((actual-x (d* (desired (width object))
		      (if clamp
			  (alexandria:clamp x 0.0 1.0)
			  x)))
	(actual-y (d* (desired (height object))
		      (if clamp
			  (alexandria:clamp y 0.0 1.0)
			  y))))
    (if interpolation
	(bilinear-interpolation object actual-x actual-y
				       :interpolate-fn interpolate-fn
				       :behaivour-on-border-fn behaivour-on-border-fn)
	(matrix-elt object (floor actual-y) (floor actual-x)))))

(defmethod pixel@ ((object matrix) x y)
  (declare (fixnum x y))
  (matrix-elt object y x))

(defmethod (setf pixel@) (color (object matrix) x y)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
   (declare (matrix object))
   (declare (fixnum x y))
   (setf (matrix-elt object y x) color))

(defmethod (setf sample@) (color (object matrix) x y)
  (declare (desired-type x y))
  (let ((x-abs (floor (* (alexandria:clamp x 0.0 1.0) (1- (width object)))))
	(y-abs (floor (* (alexandria:clamp y 0.0 1.0) (1- (height object))))))
    (setf (pixel@ object x-abs y-abs) color)))

(defmethod matrix-hline ((object matrix) x y width color)
  (do ((actx x (1+ actx)))
      ((not (< actx (+ x width))))
    (with-check-matrix-borders (object actx y)
      (setf (matrix-elt object y actx) color))))

(defmethod matrix-vline ((object matrix) x y height color)
  (do ((acty y (1+ acty)))
      ((not (<= acty (+ y height))))
    (with-check-matrix-borders (object x acty)
      (setf (matrix-elt object acty x) color))))

(defmethod matrix-rect ((object matrix) x y width height color)
  (do ((acty y (1+ acty)))
      ((not (< acty (+ y height))))
    (matrix-hline object x acty width color)))

(defmethod matrix-line ((object matrix) start end color)
  (let ((points (2d-utils:segment start end)))
    (loop for p in points do
         (let ((x (elt p 0))
               (y (elt p 1)))
           (with-check-matrix-borders (object x y)
             (setf (matrix-elt object y x) color))))
    object))

(defmethod pixel-inside-p ((object matrix) x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (element@-inside-p object x y))

(defmethod element@-inside-p ((object matrix) x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum x y))
  (and (>= x 0)
       (< x (the fixnum (width object)))
       (>= y 0)
       (< y (the fixnum (height object)))))

(defun good-aabb-start ()
  (list (1+ +maximum-map-size+) (1+ +maximum-map-size+) -1.0 -1.0))

(defmacro gen-neighbour-form (matrix queue x y)
  (alexandria:with-gensyms (neighbour)
    `(let ((,neighbour (gen-neighbour-position ,x ,y :add-center nil)))
       (loop for i in ,neighbour do
	    (when (pixel-inside-p ,matrix (first i) (second i))
	      (push i ,queue))))))

(defmethod flood-fill ((object matrix) x y &key
					     (tolerance 0)
					     (max-iteration 1d10)
					     (randomize-growth nil)
					     (position-acceptable-p-fn #'pixel-inside-p))
  "When evaluated return a list represents a contiguous portion of the matrix"
  (labels ((tolerance-p (px1 px2 tol)
	     (cond
	       ((or (arrayp px1) (listp px1))
		(let* ((diff (map 'vector #'- px1 px2))
		       (magn (sqrt
			     (reduce #'(lambda (a b) (+ a (expt b 2))) diff
				     :initial-value 0))))
		  (<= magn tol)))
	       ((numberp px1)
		(<= (abs (- px1 px2)) tol))))
	   (pop-position (queue randomize)
	     (and queue
		  (if randomize
		      (nth (lcg-next-upto (length queue)) queue)
		      (alexandria:lastcar queue)))))
    (let ((queue '()))
      (when (pixel-inside-p object x y)
	(let ((pixel-value (matrix-elt object y x)))
	  (push (list x y) queue)
	  (do* ((pixel (pop-position queue randomize-growth)
		       (pop-position queue randomize-growth))
		(iteration-ct 0)
		(affected-pixels '())
		(aabb (good-aabb-start)))
	       ((not (and (/= 0 (length queue))
			  (< iteration-ct max-iteration)))
		(values affected-pixels aabb))
	    (when pixel
	      (setf queue (remove pixel queue :test #'equalp))
	      (when (and (not (find pixel affected-pixels :test #'equalp))
			 (tolerance-p (matrix-elt object (second pixel) (first pixel))
				      pixel-value tolerance))
		(push pixel affected-pixels)
		(when (funcall position-acceptable-p-fn object (elt pixel 0) (elt pixel 1))
		  (setf aabb (2d-utils:expand-aabb2 aabb (mapcar #'desired pixel)))
		  (incf iteration-ct)
		  (gen-neighbour-form object queue (first pixel) (second pixel)))))))))))

(defun flood-fill-tolerance-p-fn (tol)
  #'(lambda (px1 px2)
      (cond
	((or (arrayp px1) (listp px1))
	 (let* ((diff (map 'vector #'- px1 px2))
		(magn (sqrt
		       (reduce #'(lambda (a b) (+ a (expt b 2))) diff
			       :initial-value 0))))
	   (<= magn tol)))
	((numberp px1)
	 (<= (abs (- px1 px2)) tol)))))

(defmethod flood-fill* ((object matrix) x y &key
					      (tolerance-fn (flood-fill-tolerance-p-fn 0))
					      (max-iteration 1000000000)
					      (randomize-growth nil)
					      (position-acceptable-p-fn #'pixel-inside-p))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum max-iteration))
  (declare (function tolerance-fn position-acceptable-p-fn))
  (labels ((pop-position (queue randomize)
	     (declare (list queue))
	     (and queue
		 (if randomize
		     (misc:random-elt queue)
		     (elt queue (1- (length queue)))))))
    (let ((queue '()))
      (when (pixel-inside-p object x y)
	(let ((pixel-value (matrix-elt object y x)))
	  (push (list x y) queue)
	  (do* ((pixel (pop-position queue randomize-growth)
		       (pop-position queue randomize-growth))
		(iteration-ct 0)
		(visited-pixels  '())
		(affected-pixels '())
		(aabb (good-aabb-start)))
	       ((not (and (/= 0 (length queue))
			  (< iteration-ct max-iteration)))
		(values affected-pixels aabb))
	    (when pixel
	      (setf queue (remove pixel queue :from-end t :test #'equalp))
	      (when (and (funcall tolerance-fn
				  (matrix-elt object (second pixel) (first pixel))
				  pixel-value)
			 (not (find pixel visited-pixels :test #'equalp)))
		(push pixel visited-pixels)
		(when (funcall position-acceptable-p-fn object (elt pixel 0) (elt pixel 1))
		  (push pixel affected-pixels)
		  (setf aabb (2d-utils:expand-aabb2 aabb (list (d (the fixnum (elt pixel 0)))
							       (d (the fixnum (elt pixel 1)))))))
		(incf iteration-ct)
		(gen-neighbour-form object queue (first pixel) (second pixel))))))))))

(defun gauss (x y sigma xc yc)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (desired-type x y sigma xc yc))
  (let ((sigma-square (dexpt sigma 2.0)))
    (d* (d/ 1.0 (d* 2.0 +pi+ sigma-square))
	(dexp (d- (d/ (d+ (dexpt (d- x xc) 2.0) (dexpt (d- y yc) 2.0))
		      (d* 2.0 sigma-square)))))))

(defun gaussian-kernel-matrix (&optional (radius 5))
  (when (evenp radius)
    (incf radius))
  (let ((sigma (/ radius 2))
	(matrix (gen-matrix-frame radius radius 0)))
    (loop-matrix (matrix x y)
       (setf (matrix-elt matrix y x)
	     (gauss (desired x) (desired y) (desired sigma) (desired (floor (/ radius 2))) 0.0)))
    (let ((sum 0))
      (map-matrix matrix #'(lambda (el) (incf sum el)))
      (map-matrix matrix #'(lambda (el) (/ el sum))))))

(defun sobel-kernel-matrices ()
  (let ((gx (gen-matrix-frame 3 3 0.0))
	(gy (gen-matrix-frame 3 3 0.0)))
    ;; gx
    (setf (matrix-elt gx 0 0) -1.0)
    (setf (matrix-elt gx 0 2)  1.0)
    (setf (matrix-elt gx 1 0) -2.0)
    (setf (matrix-elt gx 1 2)  2.0)
    (setf (matrix-elt gx 2 0) -1.0)
    (setf (matrix-elt gx 2 2)  1.0)
    ;; gy
    (setf (matrix-elt gy 0 0)  1.0)
    (setf (matrix-elt gy 0 1)  2.0)
    (setf (matrix-elt gy 0 2)  1.0)
    (setf (matrix-elt gy 2 0) -1.0)
    (setf (matrix-elt gy 2 1) -2.0)
    (setf (matrix-elt gy 2 2) -1.0)
  (values gx gy)))

(defun gradient-kernel-matrix ()
  (let ((res (gen-matrix-frame 3 3 0.0)))
    (setf (matrix-elt res 0 0)  0.0)
    (setf (matrix-elt res 0 1) -1.0)
    (setf (matrix-elt res 0 2)  0.0)
    (setf (matrix-elt res 1 0) -1.0)
    (setf (matrix-elt res 1 1)  0.0)
    (setf (matrix-elt res 1 2)  1.0)
    (setf (matrix-elt res 2 0)  0.0)
    (setf (matrix-elt res 2 1)  1.0)
    (setf (matrix-elt res 2 2)  0.0)
  res))

(defun gaussian-kernel-vector (&optional (radius 5))
  (when (evenp radius)
    (incf radius))
  (let ((sigma (d/ (desired radius) 2.0))
	(matrix (gen-matrix-frame radius 1 0)))
    (loop-matrix (matrix x y)
       (setf (matrix-elt matrix y x)
	     (gauss (desired x) (desired y) sigma (desired (floor (/ radius 2))) 0.0)))
    (let ((sum 0))
      (map-matrix matrix #'(lambda (el) (incf sum el)))
      (map-matrix matrix #'(lambda (el) (d/ el sum))))))

  (alexandria:define-constant +sharpen-kernel+
      (let ((res (gen-matrix-frame 3 3 -1.0)))
	(setf (matrix-elt res 1 1) 9.0)
	res)
    :test #'(lambda (a b) (declare (ignore a b)) t))

(defun kernel-+ (el delta)
  (with-matrix-op-typecase (el)
    (:ivec4
     ((lambda (a b)
	(declare (optimize (speed 1) (safety 0) (debug 0)))
	(declare (ivec4-type a))
	(+ a b)) el delta)
     :ubvec4
     ((lambda (a b)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (ubvec4-type a))
	(alexandria:clamp (f+ (the fixnum a) (the fixnum b)) 0 255)) el delta)
     :vec
     ((lambda (a b)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (single-float a delta))
	(d+ a b)) el delta)
     :vec4
     ((lambda (a b)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (vec4-type a delta))
	(d+ a b)) el delta)
     :sequence
     ((lambda (a b) (+ a b)) el delta)
     :single-float
     ((d+ el (desired delta)))
     :number
     ((+ el delta)))))

(defun kernel-* (el mult)
  (with-matrix-op-typecase (el)
    (:ivec4
     ((lambda (a)
	(declare (optimize (speed 1) (safety 0) (debug 0)))
	(declare (ivec4-type a))
	(declare (desired-type mult))
	(round (d* (desired a) mult))) el)
     :ubvec4
     ((lambda (a)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (ubvec4-type a))
	(declare (desired-type mult))
	(alexandria:clamp (the fixnum (round (d* (desired a) mult))) 0 255)) el)
     :vec
     ((lambda (a)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (single-float a mult))
	(d* a mult)) el)
     :vec4
     ((lambda (a)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(declare (vec4-type a mult))
	(d* a mult)) el)
     :sequence
     ((lambda (a) (* a mult)) el)
     :single-float
     ((d* el mult))
     :number
     ((* el mult)))))

(defmethod apply-kernel ((object matrix) kernel &key (round-fn #'identity))
  (declare (matrix object kernel))
  (declare (function round-fn))
  (let* ((res (gen-matrix-frame (width object) (height object)))
	 (kernel-w (width kernel))
	 (kernel-h (height kernel)))
    (nmap-matrix-xy object
		   #'(lambda (x y el)
		       (let ((sum (etypecase el
				    (ubvec4 (make-fresh-ubvec4))
				    (ivec4 (make-fresh-ivec4))
				    (vec4 (make-fresh-vec4))
				    (vector (misc:make-array-frame (length el) 0))
				    (list (misc:make-fresh-list (length el) 0))
				    (number 0))))
			 (loop
			    for r from (- y (floor (/ kernel-h 2))) below (+ kernel-h y)
			    for r1 from 0 below kernel-h do
			      (loop
				 for c from (- x (floor (/ kernel-w 2))) below (+ kernel-w x)
				 for c1 from 0 below kernel-w do
				   (with-check-borders
				       (c r 0 0 (1- (width object)) (1- (height object)))
				     (setf sum (kernel-+ sum
							 (kernel-* (matrix-elt object r c)
								   (matrix-elt kernel r1 c1))))
				     (setf sum (kernel-+ sum
							 (kernel-* el
								   (matrix-elt kernel r1 c1)))))))
			 (setf (matrix-elt res y x)
			       (num-utils:round-all sum :rounding-function round-fn)))))
    res))

(defmethod papply-kernel ((object matrix) kernel &key (round-fn #'identity))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (matrix object kernel))
  (declare (function round-fn))
  (let* ((res (gen-matrix-frame (width object) (height object)))
	 (kernel-w (width kernel))
	 (kernel-h (height kernel)))
    (declare (fixnum kernel-w kernel-h))
    (ploop-matrix (object x y)
      (let* ((el (matrix-elt object y x))
	     (sum (etypecase el
		    (ubvec4 (make-fresh-ubvec4))
		    (ivec4 (make-fresh-ivec4))
		    (vec4 (make-fresh-vec4))
		    (vector (misc:make-array-frame (length el) 0))
		    (list (misc:make-fresh-list (length el) 0))
		    (number 0))))
	(loop
	   for r fixnum from (f- (the fixnum y)
				 (the fixnum (truncate (/ kernel-h 2))))
	   below (f+ kernel-h y)
	   for r1 fixnum from 0 below kernel-h do
	     (loop
		for c fixnum from (f- (the fixnum x) (truncate (/ kernel-w 2)))
		below (f+ kernel-w x)
		for c1 fixnum from 0 below kernel-w do
		  (with-check-borders
		      (c r 0 0
			 (f- (the fixnum (width object)) 1)
			 (f- (the fixnum (height object)) 1))
		    (setf sum (kernel-+ sum (kernel-* (matrix-elt object r c)
						      (matrix-elt kernel r1 c1))))
		    (setf sum (kernel-+ sum (kernel-* el
						      (matrix-elt kernel r1 c1)))))))
	(setf (matrix-elt res y x)
	      (num-utils:round-all sum :rounding-function round-fn))))
    res))

(defmethod gaussian-blur ((object matrix) round-fn radius)
  (declare (function round-fn))
  (let ((kernel (gaussian-kernel-matrix radius)))
    (apply-kernel object kernel :round-fn round-fn)))

(defmethod gaussian-blur-separated ((object matrix) round-fn radius)
  (declare (function round-fn))
  (let* ((kernel-x (gaussian-kernel-vector radius))
	 (kernel-y (gaussian-kernel-vector radius)))
    (setf (width kernel-y) (height kernel-x)
	  (height kernel-y) (width kernel-x))
    (apply-kernel
     (apply-kernel object kernel-x :round-fn round-fn)
     kernel-y :round-fn round-fn)))

(defmethod pgaussian-blur-separated ((object matrix) round-fn radius)
  (declare (function round-fn))
  (let* ((kernel-x (gaussian-kernel-vector radius))
	 (kernel-y (gaussian-kernel-vector radius)))
    (setf (width kernel-y) (height kernel-x)
	  (height kernel-y) (width kernel-x))
    (papply-kernel
     (papply-kernel object kernel-x :round-fn round-fn)
     kernel-y :round-fn round-fn)))

(defmethod psobel-edge-detection ((object matrix))
  (multiple-value-bind (kernel-x kernel-y)
      (sobel-kernel-matrices)
    (let ((gx  (papply-kernel object kernel-x :round-fn #'identity))
	  (gy  (papply-kernel object kernel-y :round-fn #'identity))
	  (res (gen-matrix-frame (width object) (height object) (sb-cga:vec 1.0 2.0 3.0))))
      (ploop-matrix (res x y)
	 (let ((grad-x (matrix-elt gx y x))
	       (grad-y (matrix-elt gy y x)))
	   (setf (matrix-elt res y x)
		 (sb-cga:vec grad-x grad-y (dsqrt (d+ (dexpt grad-x 2.0)
						      (dexpt grad-y 2.0)))))))
      res)))

(defmethod pgradient-image ((object matrix) &key (round-fn #'identity))
  (let ((kernel (gradient-kernel-matrix)))
    (papply-kernel object kernel :round-fn round-fn)))

(defmethod gradient-image ((object matrix) &key (round-fn #'identity))
  (let ((kernel (gradient-kernel-matrix)))
    (apply-kernel object kernel :round-fn round-fn)))

(defmethod pblit-matrix ((src matrix) (dest matrix) x-dst y-dst
			 &key (blend-fn
			       #'(lambda (src dest x-src y-src x-dst y-dst)
				(declare (ignore dest x-dst y-dst))
				(matrix-elt src y-src x-src))))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (matrix src dest))
  (declare (fixnum x-dst y-dst))
  (declare (function blend-fn))
  (ploop-submatrix (src x y 0 0 (the fixnum (width src)) (the fixnum (height src)))
    (let ((x-dest (f+ x x-dst))
	  (y-dest (f+ y y-dst)))
      (with-check-matrix-borders (dest x-dest y-dest)
	(with-check-matrix-borders (src x y)
	  (setf (matrix-elt dest y-dest x-dest)
		(funcall blend-fn src dest x y x-dest y-dest)))))))

(defmethod blit-matrix ((src matrix) (dest matrix) x-dst y-dst
			&key (blend-fn
			      #'(lambda (src dest x-src y-src x-dst y-dst)
				  (declare (ignore dest x-dst y-dst))
				  (matrix-elt src y-src x-src))))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (matrix src dest))
  (declare (fixnum x-dst y-dst))
  (declare (function blend-fn))
  (loop-submatrix (src x y 0 0 (the fixnum (width src)) (the fixnum (height src)))
    (let ((x-dest (f+ x x-dst))
	  (y-dest (f+ y y-dst)))
      (with-check-matrix-borders (dest x-dest y-dest)
	(with-check-matrix-borders (src x y)
	  (setf (matrix-elt dest y-dest x-dest)
		(funcall blend-fn src dest x y x-dest y-dest)))))))


(defmethod pmatrix-blit ((source matrix) (destination matrix) x y
			&key (transparent-value +zero-height+)
			  (function-blend #'(lambda (src dest) (declare (ignore dest)) src)))
  (pblit-matrix source destination x y
  		:blend-fn #'(lambda (src dest x-src y-src x-dst y-dst)
  				    (let ((val-source (matrix-elt src  y-src x-src))
  					  (val-dst    (matrix-elt dest y-dst x-dst)))
  				      (if (equalp transparent-value val-source)
  					  val-dst
  					  (funcall function-blend val-source val-dst))))))

(defmethod matrix= ((a matrix) (b matrix) &key (test #'equalp))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (function test))
  (funcall test (data a) (data b)))

(defmethod submatrix= ((object matrix) (sample matrix) x y &key (test #'equalp))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((submatrix (submatrix object x y (width sample) (height sample))))
    (matrix= submatrix sample :test test)))
