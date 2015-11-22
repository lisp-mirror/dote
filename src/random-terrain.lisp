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

(in-package :random-terrain)

(alexandria:define-constant +branching-angle+                22.5 :test #'=)

(alexandria:define-constant +maximum-branching-chance+      100   :test #'=)

(alexandria:define-constant +invalicable-element-cost-dbg+  255.0 :test #'=)

(alexandria:define-constant +road-terrain-max-cost-bonus+     3.0 :test #'=)

(alexandria:define-constant +road-terrain-max-cost-bonus-dbg+ 80.0 :test #'=)

(alexandria:define-constant +muddy-terrain-cost-penalty+      4.0 :test #'=)

(alexandria:define-constant +muddy-terrain-cost-penalty-dbg+  90.0 :test #'=)

(alexandria:define-constant +costs-scale-mountain+            2.0 :test #'=)

(defun gen-empty-terrain (&optional (w +default-size+) (h +default-size+))
  (matrix:make-matrix (round w) (round h) +zero-height+))

(defclass random-terrain ()
  ((matrix
    :initform nil
    :initarg :matrix
    :accessor matrix)
   (cost-matrix
    :initform nil
    :initarg :cost-matrix
    :accessor cost-matrix)
   (texture-weights
    :initform nil
    :initarg :texture-weights
    :accessor texture-weights)
   (mountains-aabb
    :initform '()
    :initarg :mountains-aabb
    :accessor mountains-aabb)
   (lakes-aabb
    :initform '()
    :initarg :lakes-aabb
    :accessor lakes-aabb)
   (labyrinths-aabb
    :initform '()
    :initarg :labyrinths-aabb
    :accessor labyrinths-aabb)
   (labyrinths
    :initform '()
    :initarg :labyrinths
    :accessor labyrinths)
   (roads
    :initform '()
    :initarg :roads
    :accessor roads)
   (trees
    :initform '()
    :initarg :trees
    :accessor trees)))

(defmethod marshal:class-persistant-slots ((object random-terrain))
  (append '(matrix
	    cost-matrix
	    texture-weights
	    mountains-aabb
	    lakes-aabb
	    labyrinths-aabb
	    labyrinths
	    roads
	    trees)
	  (call-next-method)))

(defmethod to-sexp ((object random-terrain))
  (marshal:marshal object))

(defmethod serialize ((object random-terrain))
    (format nil "~s" (marshal:marshal object)))

(defmethod deserialize ((object random-terrain) file)
  (declare (ignore object))
  (marshal:unmarshal (read-from-string (filesystem-utils:slurp-file file))))

(defgeneric area-size (object))

(defgeneric all-aabb (object))

(defgeneric inside-any-aabb (object x y))

(defgeneric overlap-any-aabb (object aabb))

(defgeneric seed-mountain (object x y w h &optional max-height sigma-w sigma-h increment
				  cache-size))

(defgeneric make-valley (object x y mass mass-increment
				&key velocity can-get-stuck
				minimum-height circular-depression
				max-points-count
				erosion-can-branch
				branching-chance
				friction-coefficient))

(defgeneric erode-mountain (object &key minimum-blocking-height circular-depression
				   erosion-redraw erosion-can-branch))

(defgeneric generate-mountain (object x y w h height sigma-w sigma-h &key fit-to-aabb))

(defgeneric grow-mountain (object x y w h height sigma-w sigma-h &key fast-reject))

(defgeneric grow-with-flood-fill (object x y z area randomize-growth))

(defgeneric grow-lake (object x y area))

(defgeneric grow-labyrinth-space (object x y w h))

(defgeneric grow-road (object graph x y x1 y1))

(defgeneric draw-road-path (object path))

(defgeneric get-road-path (object graph x y x1 y1))

(defgeneric make-mountains (object &key rate
				   mountain-z-height-function
				   mountain-w-function
				   mountain-h-function
				   mountain-sigma-w-function
				   mountain-sigma-h-function
				   max-iter))

(defgeneric make-lakes (object &key rate maximum-iteration size-function))

(defgeneric make-labyrinth-space (object &key rate maximum-iteration size-function))

(defgeneric make-road (object &key howmany))

(defgeneric make-road-serial (object &key howmany))

(defgeneric make-labyrinths (object &key sigma-w-fn sigma-h-fn door-fn win-fn furn-fn))

(defgeneric make-trees (object rate sparseness &optional maximum-iteration))

(defgeneric make-map (object
		      &key mountain-rate
			mountain-z-height-function
			mountain-w-function
			mountain-h-function
			mountain-sigma-w-function
			mountain-sigma-h-function
			lake-rate
			lake-size-function
			labyrinth-rate
			labyrinth-size-function
			labyrinth-sigma-w-function
			labyrinth-sigma-h-function
			labyrinth-door-function
			labyrinth-win-function
			labyrinth-furn-function
			soil-threshold
			road-count
			trees-rate
			trees-sparseness
			maximum-iteration))

(defgeneric aabb (object))

(defgeneric dump (object file &key clamp-values))

(defgeneric build-texture-weights (object soil-threshold))

(defgeneric build-cost-matrix (object debugp))

(defgeneric get-cost-insecure (object x y))

(defgeneric get-cost (object x y))

(defmethod area-size ((object random-terrain))
  (* (width (matrix object)) (height (matrix object))))

(defmethod all-aabb ((object random-terrain))
  (append (mountains-aabb object) (lakes-aabb object) (labyrinths-aabb object)))

(defmethod inside-any-aabb ((object random-terrain) x y)
  (find-if #'(lambda (aabb) (2d-utils:inside-aabb2-p aabb x y)) (all-aabb object)))

(defmethod overlap-any-aabb ((object random-terrain) aabb)
  (find-if #'(lambda (aabb1)
	       (2d-utils:aabb2-intersect-p aabb1 aabb))
	   (all-aabb object)))

(defmacro gen-cumulative-size-elements (name)
  `(progn
     (defgeneric ,(alexandria:format-symbol t "~:@(cumulative-size-~as~)" name) (object))
     (defmethod  ,(alexandria:format-symbol t "~:@(cumulative-size-~as~)" name) ((object random-terrain))
       (reduce #'(lambda (a b) (let ((rect (2d-utils:aabb2->rect2 b)))
				 (+ a (* (elt rect 2) (elt rect 3)))))

	       (,(alexandria:format-symbol t "~:@(~as-aabb~)" name) object)
	       :initial-value 0))))

(gen-cumulative-size-elements mountain)

(gen-cumulative-size-elements lake)

(gen-cumulative-size-elements labyrinth)

(defun generate-particle (height
			  &key (depress nil) (min-height +min-height+)
			  (a (- (/ 1 height)))
			  (c height))
  (let* ((size (round (abs (sqrt (- ( / c a))))))
	 (matrix (matrix:gen-matrix-frame (* 2 size) (* 2 size) min-height))
	 (center (list (/ (1- (matrix:width matrix)) 2)
		       (/ (1- (matrix:height matrix)) 2))))
    (matrix:loop-matrix (matrix x y)
      (let* ((radius (2d-utils:2d-vector-magn
		      (2d-utils:2d-vector-diff (list x y)
					       center)))
	     (noise (noise:gen-fbm (num:desired (* (/ x (* 2.0 size)) 20.5))
				    (num:desired (* (/ y (* 2.0 size)) 20.5))
				    6 1.0 1.0 0.5 2.0
				    :normalize t :range-0->1 nil))
	     (z (+ (* 3.0 noise)
		   (* a (expt radius 2))
		   c))
	     (increment-z (if depress (- z) z)))
	(when (> z min-height)
	  (setf (matrix:matrix-elt matrix y x) increment-z))))
    matrix))

(defparameter *lookup-particles* (make-hash-table :test #'equalp))

(defparameter *lookup-particles-depress* (make-hash-table :test #'equalp))

(defmacro populate-lookup-particle (start end hashtable depress random-seed)
  `(num:with-lcg-seed (,random-seed)
     ,@(loop for i from start to end collect
	    `(setf (gethash ,i ,hashtable)
		   (generate-particle ,i :depress ,depress)))))

(populate-lookup-particle 1 60 *lookup-particles* nil 1)

(populate-lookup-particle 1 60 *lookup-particles-depress* t 1)

(defun radius-of-particle (a c)
  (round (abs (sqrt (- ( / c a))))))

(defun size-of-particle-from-height (height)
  (* 2 height))

(defun deposit-particle (matrix x-center y-center height
			 &key (depress nil) (min-height +min-height+)
			 (a (- (/ 1 height)))
			 (c height))
  (let* ((actual-hashtable (if depress *lookup-particles-depress* *lookup-particles*))
	 (size (radius-of-particle a c))
	 (new-particle (or (gethash height actual-hashtable)
			   (progn
			     (setf (gethash height actual-hashtable)
				   (generate-particle height :depress depress))
			     (gethash height actual-hashtable))))
	 (aabb         (trasl-aabb2  (vec4:vec4 0.0 0.0 (desired size) (desired size))
				     (- x-center size) (- y-center size)))
	 (min-acceptable-height (max (- (matrix:matrix-elt matrix y-center x-center)
					(matrix:matrix-elt matrix
							   (/ (matrix:width new-particle) 2)
							   (/ (matrix:height new-particle) 2)))
				     min-height)))
    (matrix:pmatrix-blit new-particle
			 matrix (- x-center size) (- y-center size)
			 :transparent-value +min-height+
			 :function-blend
			 (if depress
			     #'(lambda (src dest)
				 (if (> (+ src dest) min-acceptable-height)
				     (+ src dest)
				     dest))
			     #'(lambda (src dest)
				 (alexandria:clamp (+ src dest)
						   min-height +maximum-mountain-height+))))
    aabb))

(defun sigma-function (size)
  (+ (exp (/ (sqrt size) 4.1)) 20.0))

(alexandria:define-constant +size-box+ 2 :test #'=)

(alexandria:define-constant +cache-size+ 1000 :test #'=)

(alexandria:define-constant +min-cache-size+ 10 :test #'=)

(defun mountain-gaussian-probability (sigma-w sigma-h &optional (howmany +cache-size+))
  (num:bivariate-sampling sigma-w sigma-h howmany))

(defun %gaussian-probability (sigma mean)
  (+ (first (num:random-gaussian-distribution sigma))
     mean))

(let ((cache '()))
  (defmethod seed-mountain ((object random-terrain) x y w h
			    &optional (max-height +maximum-mountain-height+)
			      (sigma-w (sigma-function w))
			      (sigma-h (sigma-function h))
			      (increment 5)
			      (cache-size (round (* 10 (+ max-height sigma-w)))))
    (labels ((get-from-cache ()
	       (prog1
		   (elt cache 0)
		 (setf cache (rest cache)))))
      (with-accessors ((matrix matrix)) object
	(let* ((coords
		(progn
		  (when (not cache)
		    (setf cache
			  (mapcar #'(lambda (a)
				      (2d-utils:2d-vector-translate a
								    (+ x (/ w 2))
								    (+ y (/ h 2))))
				  (mountain-gaussian-probability sigma-w sigma-h
								 cache-size))
			  cache-size (max +min-cache-size+ (round (* cache-size 3/4)))))
		  (get-from-cache)))
	       (act-x (round (elt coords 0)))
	       (act-y (round (elt coords 1))))
	  (if (and (plusp act-x)
		   (plusp act-y)
		   (>= act-x x)
		   (>= act-y y)
		   (<= act-x (+ x w))
		   (<= act-y (+ y h)))
	      (progn
		(deposit-particle matrix act-x act-y increment)
		(if (or (null (matrix:matrix-elt matrix act-y act-x))
			(< (matrix:matrix-elt matrix act-y act-x) max-height))
		    (seed-mountain object x y w h max-height sigma-w sigma-h increment
				   cache-size)
		    (setf cache nil)))
	      (progn
		(seed-mountain object x y w h max-height sigma-w sigma-h increment
			       cache-size))))))))

(defmethod dump ((object random-terrain) (file string) &key (clamp-values nil))
  (if clamp-values
      (%dump-w-clamp object file clamp-values)
      (%dump-w/o-clamp object file)))

(defun %dump-w/o-clamp (map file)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream (pixmap:matrix->pgm
		    (matrix:map-matrix (matrix map) #'round)
		    "" +maximum-mountain-height+))))

(defun %dump-w-clamp (map file clamp-values)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream (pixmap:matrix->pgm
		    (matrix:map-matrix (matrix map)
				       #'(lambda (a) (alexandria:clamp (round a)
								       (elt clamp-values 0)
								       (elt clamp-values 1))))
		    "" +maximum-mountain-height+))))

(defclass trajectory-erosion ()
  ((x
    :initarg :x
    :accessor x)
   (y
    :initarg :y
    :accessor y)
   (depress
    :initarg :depress
    :accessor depress)
   (depress-chance
    :initarg :depress-chance
    :accessor depress-chance)))

(defmethod print-object ((object trajectory-erosion) stream)
  (format stream "~a ~a ~a ~%" (x object) (y object) (depress object)))

(defun erosion (matrix x y aabb &optional
				  (velocity (list .5 .0001))
				  (mass 5)
				  (mass-increment #'(lambda (x y)
						      (declare (ignore x))
						      y))
				  (friction-coefficient 0)
				  (max-iteration 100)
				  (dt 1e-1)
				  (displacement-tolerance 0)
				  (depress-probability 1)
				  (positions '())
				  (saved-max-iter max-iteration))
  (let ((pos-x (floor x))
	(pos-y (floor y)))
    (matrix:with-check-matrix-borders-then-else (matrix pos-x pos-y)
      (if (and (> max-iteration 0)
	       (inside-aabb2-p aabb x y))
	  (let* ((neighbour (remove-if #'(lambda (coord) (or
							  (or (null (elt coord 0))
							      (null (elt coord 1)))
							  (null coord)
							  (< (elt coord 0) 0)
							  (> (elt coord 0) (matrix:width matrix))
							  (< (elt coord 1) 0)
							  (> (elt coord 1) (matrix:height matrix))))
				       (matrix:gen-4-neighbour-counterclockwise pos-x pos-y)))
		 (gradients (mapcar #'(lambda (n) (- (or (matrix:matrix-elt matrix
									    (elt n 1)
									    (elt n 0))
							 0)

						     (or
						      (matrix:matrix-elt matrix pos-y pos-x) 0)))
				    neighbour))
		 (accelerations (mapcar #'(lambda (grad)
					    (* (+ (- (* +gravity+ (atan grad)))
						  (* friction-coefficient mass +gravity+))
					       dt))
					gradients))
		 (velo-x (elt velocity 0))
		 (velo-y (elt velocity 1)))
	    (when (= 5 (length accelerations))
	      (setf velo-x (+ velo-x (elt accelerations 1)))
	      (setf velo-y (+ velo-y (fifth accelerations))))
	    (let* ((new-x (+ x (* velo-x dt)))
		   (new-y (+ y (* velo-y dt)))
		   (height-depress mass))
	      (erosion matrix new-x new-y aabb (list velo-x velo-y)
		       (+ mass (funcall mass-increment new-x new-y))
		       mass-increment
		       friction-coefficient
		       (1- max-iteration)
		       dt displacement-tolerance depress-probability
		       (append positions (list
					  (make-instance 'trajectory-erosion
							 :x new-x
							 :y new-y
							 :depress height-depress
							 :depress-chance depress-probability)))
		       saved-max-iter)))
	  (values max-iteration positions velocity mass))
      (values max-iteration positions velocity mass))))

(defmethod erode-mountain ((object random-terrain)
			   &key (minimum-blocking-height +zero-height+)
			   (circular-depression t)
			   (erosion-redraw t)
			   (erosion-can-branch nil))
   (with-accessors ((matrix matrix)) object
     (let* ((aabb (aabb object))
	    (rect (2d-utils:aabb2->rect2 aabb))
	    (center (2d-utils:center-aabb2 aabb))
	    (max-dimension (max (elt rect 2) (elt rect 3)))
	    (max-dimension/2 (/ max-dimension 2))
	    (starting-erosion-aabb (2d-utils:random-sub-aabb aabb 1 1
							     #'(lambda (s) (* 0.15 s))))
	    (starting-erosion-rect (2d-utils:aabb2->rect2 starting-erosion-aabb))
	    (erosion-starts (if (< (/ max-dimension 10) 8)
				8
				(/ max-dimension 10)))
	    (erosion-count (if erosion-redraw
			       (alexandria:clamp erosion-starts 1 2)
			       1))
	    (start-mass (if (> max-dimension 50) 2 1))
	    (big-erosion-start (cond
	     			 ((< max-dimension 50)
	     			  0.1)
	     			 ((> 150 max-dimension 50)
	     			  (+ 0.45 (num:lcg-next-upto 0.1)))
	     			 (t
	     			  2)))
	    (big-erosion-a (if (> max-dimension 30) 0.0007 0.001))
	    (mass-increment #'(lambda (x y)
				(let ((dist (2d-utils:2d-vector-magn
					     (2d-utils:2d-vector-diff center (list x y)))))
				  (cond
				    ((< dist (* 0.1 max-dimension))
				     0)
				    ((> dist (* big-erosion-start max-dimension/2))
				     (* big-erosion-a dist ))
				    (t
				     (* 0.0001 dist)))))))
       (setf starting-erosion-aabb (2d-utils:trasl-aabb2
				    starting-erosion-aabb
				    (- (elt center 0) (/ (elt starting-erosion-rect 2) 2))
				    (- (elt center 1) (/ (elt starting-erosion-rect 3) 2))))
       (loop
	  for i from 0 below erosion-starts
	  for alpha from 0 below +2pi+ by (/ +2pi+ erosion-starts) do
	    (let* ((actual-alpha (+ alpha (num:lcg-next-upto (/ pi 4))))
		   (pos (list (* (/ (elt starting-erosion-rect 2) 2)
				 (cos actual-alpha))
			      (* (/ (elt starting-erosion-rect 2) 2)
				 (sin actual-alpha))))
		   (initial-velocity (2d-utils:2d-vector-scale
				       (2d-utils:2d-vector-normalize pos) 3))

		   (start (2d-utils:2d-vector-sum center pos))
		   (start-x (elt start 0))
		   (start-y (elt start 1)))
	      (loop for i from 0 below erosion-count do
		   (make-valley object start-x start-y start-mass mass-increment
				:velocity initial-velocity
				:minimum-height minimum-blocking-height
				:circular-depression circular-depression
				:erosion-can-branch erosion-can-branch)))))));23

(defmethod make-valley ((object random-terrain) x y mass mass-increment
			&key (velocity (list 0 0)) (can-get-stuck nil)
			(minimum-height +zero-height+)
			(circular-depression t)
			(max-points-count 10000)
			(erosion-can-branch nil)
			(branching-chance +maximum-branching-chance+)
			(friction-coefficient 0))
  (with-accessors ((matrix matrix)) object
    (multiple-value-bind (max-iteration trajectory velocity mass)
	(erosion
	 (matrix object)
	 x y
	 (aabb object)
	 velocity mass mass-increment
	 friction-coefficient)
      (when (or can-get-stuck
		(> max-iteration 0)) ; not stuck in a local minimum
	(when (> (length trajectory) max-points-count)
	  (setf trajectory (subseq trajectory 0 max-points-count)))
	(mapl #'(lambda (position)
		  (with-accessors ((x x) (y y) (depress depress)
				   (depress-chance depress-chance)) (elt position 0)
		    (matrix:with-check-matrix-borders ((matrix object) (round x) (round y))
		      (when (and (= 0 (num:lcg-next-upto depress-chance))
				 (> (matrix:matrix-elt matrix (round y) (round x))
				    minimum-height))
			(if circular-depression
			    (deposit-particle (matrix object)
					      (round x) (round y)
					      depress :depress t :min-height +zero-height+)
			    (setf (matrix:matrix-elt matrix (round y) (round x)) +zero-height+))
			(when (and erosion-can-branch
				   (> max-points-count 1)
				   (= 0 (num:lcg-next-upto branching-chance))
				   (not (null (cdr position))))
			  (let* ((vec1 (list x y))
				 (vec2 (list (x (elt position 1))
					     (y (elt position 1))))
				 (angle (if (= (num:lcg-next-upto 2) 0)
					    (- +branching-angle+)
					    +branching-angle+))
				 (diff (2d-utils:2d-vector-rotate
					(2d-utils:2d-vector-diff vec2 vec1)
					(deg->rad angle)))
				 (branch-start (2d-utils:2d-vector-sum
						(2d-utils:2d-vector-scale
						 (2d-utils:2d-vector-normalize diff)
						 (* 2 depress))
						(list x y))))
			    (make-valley object
					 (elt branch-start 0)
					 (elt branch-start 1)
					 2
					 #'(lambda (x y)
					     (declare (ignore x y))
					     0)
					 :erosion-can-branch t
					 :max-points-count (floor (/ max-points-count 4))
					 :circular-depression t
					 :minimum-height minimum-height
					 :branching-chance (* branching-chance 2)
					 :can-get-stuck nil
					 :friction-coefficient 0.01
					 :velocity (2d-utils:2d-vector-scale diff 4))))))))
	      trajectory)
	(values velocity mass)))))

(defmethod aabb ((object random-terrain))
   (let ((res (vec4 (1+ +maximum-map-size+)
		    (1+ +maximum-map-size+)
		    -1.0 -1.0)))
     (matrix:nmap-matrix-xy (matrix object)
			   #'(lambda (x y el)
			       (when (> el +zero-height+)
				 (setf res
				       (2d-utils:expand-aabb2 res
							     (list (num:desired x)
								   (num:desired y)))))))
     res))

(defmacro with-aabb ((aabb) object &body body)
  `(let ((,aabb (aabb ,object)))
     ,@body))

(defmacro with-rect ((rect) object &body body)
  (alexandria:with-gensyms (aabb)
    `(with-aabb (,aabb) ,object
       (let ((,rect (2d-utils:aabb2->rect2 ,aabb)))
	 ,@body))))

(alexandria:define-constant +z-erosion-threshold+ 50 :test #'=)

(alexandria:define-constant +force-erosion-chance+ 100 :test #'=)

(defmethod generate-mountain ((object random-terrain) x y w h height
			      sigma-w sigma-h &key (fit-to-aabb t))
  (with-accessors ((matrix matrix)) object
    (seed-mountain object x y w h height sigma-w sigma-h)
    (when (or (= (num:lcg-next-upto +force-erosion-chance+) 0)
	      (> height +z-erosion-threshold+))
      (erode-mountain object :erosion-redraw t))
    (when (> height +z-erosion-threshold+)
      (erode-mountain object
       		      :circular-depression t
       		      :erosion-redraw nil
       		      :erosion-can-branch t))
    ;; TODO this need to be fixed
    (setf matrix (matrix:map-matrix matrix #'num:desired))
    ;; rotate a random-angle
    (with-aabb (aabb) object
      (let* ((center (2d-utils:center-aabb2 aabb)))
	(setf matrix
	      (matrix:rotate-matrix matrix
				    (num:lcg-next-upto 361)
				    :rounding-fn nil
				    :fill-value +zero-height+ :pivot center))))
    (when fit-to-aabb
      (with-rect (rect) object
	(let ((mountain-fit-matrix
	       (matrix:submatrix matrix (floor (elt rect 0)) (floor (elt rect 1))
				 (floor (elt rect 2)) (floor (elt rect 3)))))
	  (setf matrix mountain-fit-matrix))))
    matrix))

(define-condition overlap-error (error)
  ((mountain-aabb
    :initarg :aabb1
    :reader aabb1)
   (aabb
    :initarg :aabb2
    :reader aabb2))
  (:report (lambda (condition stream)
	     (format stream "~a overlap with ~a" (aabb1 condition)
		     (aabb2 condition))))
  (:documentation "Error when elements overlaps"))

(define-condition mountain-invalid-position-error (error)
  ((coord
    :initarg :coord
    :reader coord))
   (:report (lambda (condition stream)
	     (format stream "mountain position ~{~a ~} invalid" (coord condition))))
   (:documentation "Error when mountain's position is invalid"))

(define-condition out-of-map-error (error)
  ((aabb
    :initarg :aabb
    :reader aabb))
   (:report (lambda (condition stream)
	      (format stream "element with aabb ~a out of map" (aabb condition))))
   (:documentation "Error when an element go beyond the map limits"))

(defmacro check-overlap (map x y w h)
  (alexandria:with-gensyms (overlapped)
    `(let ((,overlapped (overlap-any-aabb ,map (list ,x ,y (+ ,x ,w) (+ ,y ,h)))))
       (when ,overlapped
	 (error 'overlap-error
		:aabb1 (list ,x ,y (+ ,x ,w)  (+ ,y ,h))
		:aabb2 ,overlapped)))))

(defmethod grow-mountain ((object random-terrain) x y w h height sigma-w sigma-h
			  &key (fast-reject t))
  (with-accessors ((matrix matrix)) object
    (let* ((mountain (make-instance 'random-terrain
				    :matrix (gen-empty-terrain (* 2 (max w h)) ;width object))
							       (* 2 (max w h)))))) ;height object))))))
      (restart-case
	  (progn
	    (when fast-reject
	      (when (inside-any-aabb object x y)
		(error 'mountain-invalid-position-error :coord (list x y)))
	      (when (or
		     (< x 0)
		     (< y 0)
		     (> (+ x w) (width matrix))
		     (> (+ y h) (height matrix)))
		(error 'out-of-map-error :aabb
		       (list x y (+ x w) (+ y h))))
	      (check-overlap object x y w h))
	    (generate-mountain mountain
			       (round (- (/ (width (matrix mountain)) 2) (/ w 2)))
			       (round (- (/ (height (matrix mountain)) 2) (/ h 2)))
			       w h height sigma-w sigma-h)
	    (let* ((aabb (aabb mountain))
		   (aabb-mountain-world-space (list (+ x (elt aabb 0)) (+ y (elt aabb 1))
						    (+ x (elt aabb 2)) (+ y (elt aabb 3)))))
	      (when (not (2d-utils:valid-aabb2-p aabb))
		(error 'conditions:invalid-aabb-error :aabb aabb))
	      (when (or
		     (< (elt aabb-mountain-world-space 0) 0)
		     (< (elt aabb-mountain-world-space 1) 0)
		     (> (elt aabb-mountain-world-space 2) (width matrix))
		     (> (elt aabb-mountain-world-space 3) (height matrix)))
		(error 'out-of-map-error :aabb aabb-mountain-world-space))
	      (let ((overlapped (overlap-any-aabb object aabb-mountain-world-space)))
		(if overlapped
		    (error 'overlap-error
			   :aabb1 aabb-mountain-world-space
			   :aabb2 overlapped)
		    (progn
		      (matrix:pmatrix-blit (matrix mountain) matrix x y
						       :transparent-value +zero-height+)
		      (push aabb-mountain-world-space (mountains-aabb object)))))
	      aabb-mountain-world-space))
	(ignore-error ()
	  (let* ((aabb (aabb mountain))
		 (aabb-mountain-world-space (list (+ x (elt aabb 0)) (+ y (elt aabb 1))
						  (+ x (elt aabb 2)) (+ y (elt aabb 3)))))

	    (matrix:pmatrix-blit (matrix mountain) matrix x y
					     :transparent-value +zero-height+)
	    aabb-mountain-world-space))
	(use-value (v) v)))))

(define-condition invalid-position-error (error)
  ((coord
    :initarg :coord
    :reader coord))
   (:report (lambda (condition stream)
	     (format stream "position ~{~a ~} invalid" (coord condition))))
   (:documentation "Error when position is invalid"))

(defun merge-aabb (aabb other-aabbs)
  (let* ((to-merge (find-if #'(lambda (old-aabb)
				(let ((num:*default-epsilon* 0.5))
				  (2d-utils:approx-aabb2-intersect-p old-aabb aabb t)))
			    other-aabbs)))
    (if to-merge
	(merge-aabb (2d-utils:union-aabb2 to-merge aabb)
		    (remove-if #'(lambda (old-aabb) (2d-utils:aabb2~ old-aabb to-merge))
			       other-aabbs
			       :count 1))
	(append (list aabb) other-aabbs))))

(defmethod grow-lake ((object random-terrain) x y area)
  (let ((aabb (grow-with-flood-fill object x y 2 area t)))
    (when aabb
      (setf (lakes-aabb object) (merge-aabb aabb (lakes-aabb object))))))

(defmethod grow-labyrinth-space ((object random-terrain) x y w h)
  (with-accessors ((matrix matrix)) object
    (restart-case
	(progn
	  (check-overlap object (1- x) (1- y) (1+ w) (1+ h))
	  (when (inside-any-aabb object (1- x) (1- y))
	    (error 'invalid-position-error :coord (list x y)))
	  (when (or
		 (< x 1)
		 (< y 1)
		 (> (+ x w) (1- (width matrix)))
		 (> (+ y h) (1- (height matrix))))
	    (error 'out-of-map-error :aabb
		   (list x y (+ x w) (+ y h))))
	  (let ((aabb-labyrinth (2d-utils:rect2->aabb2
				 (mapcar #'num:desired (list x y w h)))))
	    (push aabb-labyrinth (labyrinths-aabb object))))
      (use-value (v) v))))

(defmethod grow-road ((object random-terrain) (graph graph:tile-based-graph) x y x1 y1)
  (get-road-path object graph x y x1 y1))

(defmethod draw-road-path ((object random-terrain) path)
  (loop for i in path do
       (setf (matrix:matrix-elt (matrix object) (elt i 1) (elt i 0)) +road-height+)))

(defmethod get-road-path ((object random-terrain) (graph graph:tile-based-graph) x y x1 y1)
  (labels ((heuristic-fn ()
	     #'(lambda (object a b start-node)
		 (declare (ignore object))
		 ;(declare (list a b start-node))
		 (declare (optimize (speed 0) (safety 3) (debug 3)))
		 (let* ((cost  (+ (abs (- (elt b 0) (elt a 0)))
				  (abs (- (elt b 1) (elt a 1)))))
			(dx1   (- (elt b 0) (elt a 0)))
			(dy1   (- (elt b 1) (elt a 1)))
			(dx2   (- (elt start-node 0) (elt a 0)))
			(dy2   (- (elt start-node 1) (elt a 1)))
			(cross (abs (- (* dx1 dy2) (* dx2 dy1)))))
		   (+ cost (* cross 0.05))))))
      (let* ((start (list x y))
	     (end (list x1 y1))
	     (path (mapcar #'(lambda (id)
			       (graph:node-id->node graph id))
			   (graph:graph->path
			    (graph:astar-search graph
						(graph:node->node-id graph start)
						(graph:node->node-id graph end)
						:heuristic-cost-function (heuristic-fn))
			    (graph:node->node-id graph end)))))
	path)))

(defun lake-particle-inside-limits-p (matrix x y)
  (let ((limit 2))
    (and (> x limit)
	 (< x (f- (width matrix) limit))
	 (> y limit)
	 (< y (f- (height matrix) limit)))))

(defmethod grow-with-flood-fill ((object random-terrain) x y z area randomize-growth)
  (with-accessors ((matrix matrix)) object
    (restart-case
	(if (inside-any-aabb object x y)
	    (error 'invalid-position-error :coord (list x y))
	    (matrix:with-check-matrix-borders (matrix x y)
	      (let ((aabb   (matrix:good-aabb-start))
		    (pixels (matrix:flood-fill matrix x y :max-iteration area
				     :tolerance 0
				     :randomize-growth randomize-growth
				     :position-acceptable-p-fn #'lake-particle-inside-limits-p)))
		(if (= (length pixels) area)
		    (progn
		      (loop for pix in pixels do
			   (if (zerop z)
			       (setf (matrix:matrix-elt matrix (elt pix 1) (elt pix 0))
				     +lake-height+)
			       (let ((aabb-part (deposit-particle matrix (elt pix 0) (elt pix 1)
								  z
								  :depress t
								  :min-height +min-height+)))
				 (setf aabb (union-aabb2 aabb aabb-part)))))
		      (map 'vec4 #'(lambda (a) (if (d< a 0.0) 0.0 a)) aabb))
		    nil))))
      (use-value (v) v))))

(defun %default-mountain-z-height-function (sigma mean
					    &optional
					      (minimum-z +minimum-mountain-height+)
					      (maximum-z +maximum-mountain-height+))
  #'(lambda (map x y iteration)
      (declare (ignore map x y iteration))
      (alexandria:clamp
       (abs (%gaussian-probability sigma mean))
       minimum-z maximum-z)))

(defun default-mountain-z-height-function ()
  (%default-mountain-z-height-function +maximum-mountain-height+
				       0
				       +minimum-mountain-height+
				       +maximum-mountain-height+))

(defun default-mountain-size-function ()
  #'(lambda (map x y z iteration)
      (declare (ignore map x y iteration))
      (let* ((s1 (if (< z 160)
		     (* 0.5 z)
		     (* 0.003 (expt z 2)))))
	(%gaussian-probability (* 0.1 s1) s1))))

(defun default-mountain-sigma-function ()
  #'(lambda (map x y z w h iteration)
      (declare (ignore map x y w h iteration))
      (let ((mean (/ (* 30 z) (+ 200 z))))
	(%gaussian-probability (* 0.1 mean) mean))))

(defun %default-lake-size-function (sigma fraction)
  #'(lambda (map x y iteration)
      (declare (ignore x y iteration))
      (let ((area (* fraction (area-size map))))
	(round (abs (%gaussian-probability (* sigma area) area))))))

(defun default-lake-size-function ()
  (%default-lake-size-function 0.5 0.05))

(defun %default-labyrinth-size-function (sigma fraction)
  #'(lambda (map x y iteration)
      (declare (ignore x y iteration))
      (let* ((area (* fraction (area-size map)))
	     (raw-size (1+ (round (sqrt (abs (%gaussian-probability (* sigma area) area)))))))
	(1- (+ raw-size (mod raw-size 2))))))

(defun default-labyrinth-size-function ()
  (%default-labyrinth-size-function 0.5 0.05))

(defmacro if-not-null-room (room &body body)
  `(cond
     (,room
	,(first body))
     (t
      ,(second body))))

(defun default-labyrinth-sigma-w-function ()
  #'(lambda (x a) (declare (ignore a)) (+ 10 x)))

(defun default-labyrinth-sigma-h-function ()
  #'(lambda (x a) (declare (ignore a)) (+ 10 x)))

(defun default-labyrinth-door-function ()
  #'(lambda (x a) (declare (ignore x a)) 5))

(defun default-labyrinth-win-function ()
  #'(lambda (x a) (declare (ignore x a)) 3))

(defun default-labyrinth-furniture-function ()
  #'(lambda (x a)
      (if-not-null-room a
	(let ((area (d (* (random-labyrinth:w a) (random-labyrinth:h a)))))
	  (truncate (num:dlerp (num:smoothstep-interpolate 0.0 40.0 area) 0.0 40.0)))
	(if (< x 1) 3 4))))

(defmethod make-map ((object random-terrain)
		     &key
		       (mountain-rate              0.2)
		       (mountain-z-height-function (default-mountain-z-height-function))
		       (mountain-w-function        (default-mountain-size-function))
		       (mountain-h-function        (default-mountain-size-function))
		       (mountain-sigma-w-function  (default-mountain-sigma-function))
		       (mountain-sigma-h-function  (default-mountain-sigma-function))
		       (lake-rate                  0.05)
		       (lake-size-function         (default-lake-size-function))
		       (labyrinth-rate             0.1)
		       (labyrinth-size-function    (default-labyrinth-size-function))
		       (labyrinth-sigma-w-function (default-labyrinth-sigma-w-function))
		       (labyrinth-sigma-h-function (default-labyrinth-sigma-h-function))
		       (labyrinth-door-function    (default-labyrinth-door-function))
		       (labyrinth-win-function     (default-labyrinth-win-function))
		       (labyrinth-furn-function    (default-labyrinth-furniture-function))
		       (soil-threshold             0.8)
		       (road-count                 (round (* 30 labyrinth-rate)))
		       (trees-rate                 0.05)
		       (trees-sparseness           0.7)
		       (maximum-iteration          1000))
  (make-mountains object
		    :rate mountain-rate
		    :mountain-z-height-function mountain-z-height-function
		    :mountain-w-function mountain-w-function
		    :mountain-h-function mountain-h-function
		    :mountain-sigma-w-function mountain-sigma-w-function
		    :mountain-sigma-h-function mountain-sigma-h-function
		    :max-iter maximum-iteration)
  (make-lakes object
	      :rate lake-rate
	      :size-function lake-size-function
	      :maximum-iteration maximum-iteration)
  (make-labyrinth-space object
			:rate labyrinth-rate
			:size-function labyrinth-size-function
			:maximum-iteration maximum-iteration)
  (if (< road-count 9)
	(make-road object :howmany road-count)
	(make-road-serial object :howmany road-count))
  (make-trees object trees-rate trees-sparseness)
  (make-labyrinths object
		   :win-fn labyrinth-win-function
		   :door-fn labyrinth-door-function
		   :furn-fn labyrinth-furn-function
		   :sigma-h-fn labyrinth-sigma-h-function
		   :sigma-w-fn labyrinth-sigma-w-function)
  (build-texture-weights object soil-threshold)
  (build-cost-matrix object nil)
  (when +debug-mode+
    (pixmap:save-pixmap (texture-weights object)
			(fs:file-in-package "layers.tga"))
    (dump object (fs:file-in-package "height.pgm"))
    (with-open-file (stream
		     (fs:file-in-package "costs.pgm")
		     :direction :output
		     :if-exists :supersede :if-does-not-exist :create)
      (format stream "~a" (pixmap:matrix->pgm (map-matrix (cost-matrix object) #'round) ""
					      (truncate +invalicable-element-cost+)))))
  object)

(alexandria:define-constant +min-size-lab-start-generation+ 5 :test #'=)

(defun %make-lab (size fallback-size
		  sigma-w-fn sigma-h-fn
		  door-fn win-fn furn-fn &optional (generation 0))
  (if (and (< generation 10)
	   (> size +min-size-lab-start-generation+))
      (let ((lab (random-labyrinth:generate size
					    :scale-fact 2
					    :func-sigma-w sigma-w-fn
					    :func-sigma-h sigma-h-fn
					    :func-door door-fn
					    :func-win win-fn
					    :func-furniture furn-fn)))
	(random-labyrinth:clean-and-redraw-mat lab)
	(if (< (random-labyrinth:occupied-rate lab) 0.8)
	    (%make-lab size fallback-size sigma-w-fn sigma-h-fn
		       door-fn win-fn furn-fn (1+ generation))
	    lab))
      (random-labyrinth:gen-simple-room fallback-size
					:furniture-fn #'(lambda (a b)
							  (declare (ignore a b))
							  30))))

(defmethod make-labyrinths ((object random-terrain)
			    &key
			      (sigma-w-fn #'(lambda (x a) (declare (ignore a)) (+ 10 x)))
			      (sigma-h-fn #'(lambda (x a) (declare (ignore a)) (* 2 x)))
			      (door-fn    #'(lambda (x a) (declare (ignore a)) (if (< x 1) 3 4)))
			      (win-fn     #'(lambda (x a) (declare (ignore a)) (if (< x 1) 3 4)))
			      (furn-fn    #'(lambda (x a) (declare (ignore a)) (if (< x 1) 3 4))))
  (setf (labyrinths object)
	(loop for rectangles in (map 'list #'2d-utils:aabb2->rect2 (labyrinths-aabb object))
	   collect
	     (let* ((raw-size (* +terrain-chunk-tile-size+
				 (if (= (mod (elt rectangles 2) 2) 0) ; unfortunately
					                              ; map
				                                      ;; with
								      ;; size
								      ;; nxn
								      ;; will
								      ;; produce
								      ;; a
								      ;; labyrinth
								      ;; of
								      ;; (n+1)X(n+1)
								      ;; max
				     (1- (elt rectangles 2))
				     (elt rectangles 2))))
		    (size/2 (truncate (/ raw-size 2)))
		    (size (- size/2 (mod size/2 2))))
	       (cond
		 ((< raw-size 3)
		  nil)
		 ((<= size +min-size-lab-start-generation+)
		  (random-labyrinth:gen-simple-room (truncate raw-size)
						    :furniture-fn #'(lambda (a b)
								      (declare (ignore a b))
								      30)))
		 (t
		  (%make-lab size (truncate raw-size)
			     sigma-w-fn sigma-h-fn
			     door-fn win-fn furn-fn))))))

  ;; mark invalid aabb-labyrinth
  (assert (= (length (labyrinths object)) (length (labyrinths-aabb object))))
  (loop for i from 0 below (length (labyrinths-aabb object)) do
       (when (null (elt (labyrinths object) i))
	 (setf (elt (labyrinths-aabb object) i) nil)))
  (setf (labyrinths object) (remove-if #'null (labyrinths object)))
  (setf	(labyrinths-aabb object) (remove-if #'null (labyrinths-aabb object)))
  (loop for i from 0 below (length (labyrinths-aabb object)) do
       (setf (elt (labyrinths-aabb object) i)
	     (vec4 (aabb2-min-x (elt (labyrinths-aabb object) i))
		   (aabb2-min-y (elt (labyrinths-aabb object) i))
		   (d+ (aabb2-min-x (elt (labyrinths-aabb object) i))
		       (d/
			(d (width (random-labyrinth:shared-matrix (elt (labyrinths object) i))))
			+terrain-chunk-tile-size+))
		   (d+ (aabb2-min-y (elt (labyrinths-aabb object) i))
		       (d/
			(d (height (random-labyrinth:shared-matrix (elt (labyrinths object) i))))
			+terrain-chunk-tile-size+)))))
  (when +debug-mode+
    (loop
       for i in (labyrinths object)
       for j from 0 do
	 (random-labyrinth:clear-mat i)
	 (random-labyrinth:room->mat i :draw-door t :draw-door-to-nowhere t
				     :draw-border t :draw-seed nil :draw-id nil)
	 (random-labyrinth:dump i (fs:file-in-package (format nil "lab~a.ppm" j))
				:draw-door-to-nowhere t :draw-id nil)))
  (loop for i in (labyrinths object) do (random-labyrinth:clean-and-redraw-mat i)))

(defmethod make-mountains ((object random-terrain)
			   &key (rate 0.2)
			     (mountain-z-height-function (default-mountain-z-height-function))
			     (mountain-w-function (default-mountain-size-function))
			     (mountain-h-function (default-mountain-size-function))
			     (mountain-sigma-w-function (default-mountain-sigma-function))
			     (mountain-sigma-h-function (default-mountain-sigma-function))
			     (max-iter 10000))
  (let* ((w-map (width (matrix object)))
	 (h-map (height (matrix object)))
	 (area-map (* w-map h-map)))
    (do* ((iteration 0 (1+ iteration))
	  (x (num:lcg-next-upto w-map) (num:lcg-next-upto w-map))
	  (y (num:lcg-next-upto h-map) (num:lcg-next-upto h-map))
	  (z (funcall mountain-z-height-function object x y iteration)
	     (funcall mountain-z-height-function object x y iteration))
	  (w (funcall mountain-w-function object x y z iteration)
	     (funcall mountain-w-function object x y z iteration))
	  (h (funcall mountain-h-function object x y z iteration)
	     (funcall mountain-h-function object x y z iteration))
	  (sigma-w (funcall mountain-sigma-w-function object x y z w h iteration)
		   (funcall mountain-sigma-w-function object x y z w h iteration))
	  (sigma-h (funcall mountain-sigma-h-function object x y z w h iteration)
		   (funcall mountain-sigma-h-function object x y z w h iteration)))
	  ((not (and (< iteration max-iter)
		  (< (/ (cumulative-size-mountains object) area-map) rate))))
      (when (and
	     (< (+ x w) w-map)
	     (< (+ y h) h-map))
	(handler-bind ((overlap-error
			#'(lambda (c)
			    (declare (ignore c))
			    (use-value nil)))
		       (conditions:invalid-aabb-error
			#'(lambda (c)
			    (declare (ignore c))
			    (use-value nil)))
		       (mountain-invalid-position-error
			#'(lambda (c)
			    (declare (ignore c))
			    (use-value nil)))
		       (out-of-map-error
			#'(lambda (c)
			    (declare (ignore c))
			    (use-value nil))))
	  (grow-mountain object x y w h z sigma-w sigma-h))))))

(defmethod make-lakes ((object random-terrain) &key
		       (rate 0.2)
		       (maximum-iteration 100000)
		       (size-function (default-lake-size-function)))
  (let* ((w-map (width  (matrix object)))
	 (h-map (height (matrix object))))
    (do* ((iteration 0 (1+ iteration))
	  (x (num:lcg-next-upto w-map) (num:lcg-next-upto w-map))
	  (y (num:lcg-next-upto h-map) (num:lcg-next-upto h-map)))
	 ((not (and (< iteration maximum-iteration)
		    (< (/ (cumulative-size-lakes object) (area-size object)) rate))))
      (let ((area (funcall size-function object x y iteration)))
	(handler-bind ((invalid-position-error
			#'(lambda (c)
			    (declare (ignore c))
			    (use-value nil)))
		       (overlap-error
			#'(lambda (c)
			    (declare (ignore c))
			    (use-value nil))))
	  (grow-lake object x y area))))))

(defmethod make-labyrinth-space ((object random-terrain)
				 &key
				   (rate 0.01)
				   (maximum-iteration 100000000)
				   (size-function (default-lake-size-function)))
  (let* ((w-map (width (matrix object)))
	 (h-map (height (matrix object))))
    (do* ((iteration 0 (1+ iteration))
	  (x (num:lcg-next-upto w-map) (num:lcg-next-upto w-map))
	  (y (num:lcg-next-upto h-map) (num:lcg-next-upto h-map)))
	 ((not (and (< iteration maximum-iteration)
		    (< (/ (cumulative-size-labyrinths object) (area-size object)) rate))))
      (let ((size (funcall size-function object x y iteration)))
	(handler-bind ((invalid-position-error
			#'(lambda (c)
			    (declare (ignore c))
			    (use-value nil)))
		       (overlap-error
			#'(lambda (c)
			    (declare (ignore c))
			    (use-value nil)))
		       (out-of-map-error
			#'(lambda (c)
			    (declare (ignore c))
			    (use-value nil))))
	  (grow-labyrinth-space object x y size size))))))

(defun %blit-labyrinth (terrain color)
  (let ((matrix-terrain (matrix terrain)))
    (loop for lab in  (labyrinths-aabb terrain) do
	 (let ((rect (2d-utils:aabb2->rect2 lab)))
	   (matrix:matrix-rect matrix-terrain
			       (truncate (elt rect 0))
			       (truncate (elt rect 1))
			       (truncate (+ 0 (elt rect 2)))
			       (truncate (+ 0 (elt rect 3)))
			       color)))))

(defmethod make-road ((object random-terrain) &key (howmany 3))
  (when (> (length (labyrinths-aabb object)) 1)
    (%blit-labyrinth object +invalicable-element-cost+)
    (do ((count 0)
	 (paths '()))
	((not (< count howmany)))
      (let* ((rect-start (2d-utils:aabb2->rect2
			  (nth (num:lcg-next-upto (length (labyrinths-aabb object)))
			       (labyrinths-aabb object))))
	     (rect-end (2d-utils:aabb2->rect2
			(nth (num:lcg-next-upto (length (labyrinths-aabb object)))
			     (labyrinths-aabb object))))
	     (start-x (+ (1- (elt rect-start 0)) (num:lcg-next-upto (1+ (elt rect-start 2)))))
	     (start-y (if (or (num:epsilon= start-x (1- (elt rect-start 0)))
			      (num:epsilon= start-x (+ (elt rect-start 0)
						       (elt rect-start 2))))
			  ;; if we are on the left or right extremes
			  (+ (elt rect-start 1) (num:lcg-next-upto (elt rect-start 3)))
			  (+ (1- (elt rect-start 1)) (* (num:lcg-next-in-range 0 2)
							(1+ (elt rect-start 3))))))
	     (end-x (+ (1- (elt rect-end 0)) (num:lcg-next-upto (1+ (elt rect-end 2)))))
	     (end-y (if (or (num:epsilon= end-x (1- (elt rect-end 0)))
			    (num:epsilon= end-x (+ 1 (elt rect-end 0) (elt rect-end 2))))
			(+ 0 (elt rect-end 1) (num:lcg-next-upto (elt rect-end 3)))
			(+ (1- (elt rect-end 1)) (* (num:lcg-next-in-range 0 2)
						      (1+ (elt rect-end 3)))))))
	(when (not (equalp rect-start rect-end))
	  (push (list (round start-x)
		      (round start-y)
		      (round end-x)
		      (round end-y))
		paths)
	  (incf count)
	  (when (= (mod (length paths) 3) 2)
	    (let* ((lparallel:*kernel* (lparallel:make-kernel (os-utils:cpu-number)))
		   (channel (lparallel:make-channel)))
	      (dolist (path paths)
		   (lparallel:submit-task
		    channel
		    (let ((cost-plus-heuristic (rb-tree:make-root-rb-node nil 'black)))
		      #'(lambda ()
			  (let* ((graph:*cumulative-cost-plus-heuristic* cost-plus-heuristic)
				 (matrix-costs (matrix:map-matrix
						(matrix object)
						#'(lambda (el)
						    (if (< el +zero-height+)
							+invalicable-element-cost+
							(if (> el +zero-height+)
							    (* el +costs-scale-mountain+)
							    el)))))
				 (graph-costs (make-instance 'graph:tile-based-graph
							     :matrix matrix-costs)))
			    (get-road-path object graph-costs
					   (elt path 0)
					   (elt path 1)
					   (elt path 2)
					   (elt path 3)))))))
	      (loop for path in paths do
		   (let ((road (lparallel:receive-result channel)))
		     (push road (roads object))))
	      (setf paths '())
	      (lparallel:end-kernel :wait t))))))
    (%blit-labyrinth object +zero-height+)))

(defmethod make-road-serial ((object random-terrain) &key (howmany 3))
  (when (> (length (labyrinths-aabb object)) 1)
    (%blit-labyrinth object +invalicable-element-cost+)
      (do ((count 0))
	  ((not (< count howmany)))
	(let* ((matrix-costs (matrix:map-matrix (matrix object)
						#'(lambda (el)
						    (let ((num:*default-epsilon* 0.5))
						      (if (or (num:epsilon= el +lake-height+)
							      (num:epsilon= el +labirinth-height+))
							  +invalicable-element-cost+
							  el)))))
	       (graph-costs (make-instance 'graph:tile-based-graph
					   :matrix matrix-costs))
	       (rect-start (2d-utils:aabb2->rect2
			    (nth (num:lcg-next-upto (length (labyrinths-aabb object)))
				 (labyrinths-aabb object))))
	       (rect-end (2d-utils:aabb2->rect2
			  (nth (num:lcg-next-upto (length (labyrinths-aabb object)))
			       (labyrinths-aabb object))))
	       (start-x (+ (1- (elt rect-start 0)) (num:lcg-next-upto (1+ (elt rect-start 2)))))
	       (start-y (if (or (num:epsilon= start-x (1- (elt rect-start 0)))
				(num:epsilon= start-x (+ (elt rect-start 0)
							 (elt rect-start 2))))
			    ;; if we are on the left or right extremes
			    (+ (elt rect-start 1) (num:lcg-next-upto (elt rect-start 3)))
			    (+ (1- (elt rect-start 1)) (* (num:lcg-next-in-range 0 2)
							  (1+ (elt rect-start 3))))))
	       (end-x (+ (1- (elt rect-end 0)) (num:lcg-next-upto (1+ (elt rect-end 2)))))
	       (end-y (if (or (num:epsilon= end-x (1- (elt rect-end 0)))
			      (num:epsilon= end-x (+ 1 (elt rect-end 0) (elt rect-end 2))))
			  (+ 0 (elt rect-end 1) (num:lcg-next-upto (elt rect-end 3)))
			  (+ (1- (elt rect-end 1)) (* (num:lcg-next-in-range 0 2)
						      (1+ (elt rect-end 3)))))))
	  (when (not (equalp rect-start rect-end))
	    (incf count)
	    (push
	     (get-road-path object graph-costs (round start-x) (round start-y)
			    (round end-x)
			    (round end-y))
	     (roads object)))))
      (%blit-labyrinth object +zero-height+)))

(misc:definline bias-noise (n)
  (num:d/ (num:d+ n 1.0) 2.0))

(defun find-in-roads (coord roads)
  (loop for road in roads do
       (when (find coord road :test #'equalp)
	   (return-from find-in-roads t))))

(defun %tree-holes-freq (map)
  (truncate (d+ (d* 0.0339674 (d (width (matrix map)))) 1.17391)))

(defmethod make-trees ((object random-terrain) rate sparseness &optional
								 (maximum-iteration 100))
  (with-accessors ((matrix matrix) (trees trees) (roads roads)) object
    (let* ((matrix-area  (desired (area-size object)))
	   (tree-rate    (d/ (desired (length trees)) matrix-area))
	   (block-width  (truncate (/ (width  matrix) 4)))
	   (block-height (truncate (/ (height matrix) 4))))
      (if (and (d< tree-rate (desired rate))
	       (> maximum-iteration 0))
	  (progn
	    (loop for x-rect from 0 below (width matrix) by block-width do
		 (loop for y-rect from 0 below (height matrix) by block-height do
		      (let ((tree-rate-inner (d/ (desired (length trees)) matrix-area)))
			(when (d< tree-rate-inner (desired rate))
			  (loop for x from x-rect below (+ x-rect block-width) do
			       (loop for y from y-rect below (+ y-rect block-height) do
				    (matrix:with-check-borders
					(x y 1 1 (f- (width  matrix) 3) (f- (height matrix) 3))
				      (let* ((x-norm (desired (/ x (width matrix))))
					     (y-norm (desired (/ y (height matrix))))
					   (1/max-iteration (d/ 1.0 (desired maximum-iteration)))
					   (noise (bias-noise (noise:perlin-3d (d/ x-norm 0.1)
									       (d/ y-norm 0.1)
									       1/max-iteration)))
					     (neighb (matrix:gen-neighbour-position x y
										    :add-center t))
					   (dx+1   (d- (matrix:matrix-elt* matrix (elt neighb 1))
						       (matrix:matrix-elt* matrix (elt neighb 0))))
					   (dy+1   (d- (matrix:matrix-elt* matrix (elt neighb 3))
						       (matrix:matrix-elt* matrix (elt neighb 0))))
					   (dx-1   (d- (matrix:matrix-elt* matrix (elt neighb 0))
						       (matrix:matrix-elt* matrix (elt neighb 2))))
					   (dy-1   (d- (matrix:matrix-elt* matrix (elt neighb 0))
						       (matrix:matrix-elt* matrix (elt neighb 4)))))
					(when (and (d> noise (desired sparseness))
						   (epsilon= dx+1 0.0)
					       (epsilon= dy+1 0.0)
					       (epsilon= dx-1 0.0)
					       (epsilon= dy-1 0.0)
					       (notany #'(lambda (a)
							   (2d-utils:inside-iaabb2-p a x y))
						       (labyrinths-aabb object))
					       (not (find (ivec2 x y) trees :test #'equalp))
					       (not (find-in-roads (list x y) roads)))
					(push (ivec2 x y) trees)))

				  nil)))))))
	    (make-trees object rate sparseness (1- maximum-iteration)))
	(setf (trees object) (loop ;; just remove some  tree here and
				   ;; there to get some holes
	 			for tree in (trees object)
				for i    from 1 when (= (mod i (%tree-holes-freq object)) 0)
				collect
	 			tree))))))

(defmethod build-texture-weights ((object random-terrain) soil-threshold)
  (let ((res (pixmap:make-pixmap (width (matrix object)) (height (matrix object))
				 4 (ubvec4 0 0 0 255))))
    (dolist (road (roads object))
      (dolist (i road)
	(setf (elt (matrix:matrix-elt res (elt i 1) (elt i 0)) pixmap:+red-channel+) #xff)))
    (loop for lab in (labyrinths-aabb object) do
	 (matrix:loop-submatrix (res x y (truncate (2d-utils:aabb2-min-x lab))
				     (truncate (2d-utils:aabb2-min-y lab))
				     (truncate (2d-utils:aabb2-max-x lab))
				     (truncate (2d-utils:aabb2-max-y lab)))
	   (setf (elt (matrix:matrix-elt res y x) pixmap:+green-channel+) #xff)))
    (let ((soil (pixmap:with-random-perlin-gradient-offset
		    (pixmap:with-draw-normalizated-coord-square
			(x y (matrix:height res) pixmap 4
			   :bindings (noise:*perlin-gradient-random-offset*))
		      (let ((noise (num:d/ (num:d+ 1.0 (noise:perlin-2d (num:d/ x 0.1)
									(num:d/ y 0.1)))
				       2.0)))
			(if (< noise soil-threshold)
			    (vec4:vec4 0.0 0.0 0.0 0.0)
			    (vec4:vec4 0.0 0.0 noise 0.0)))))))
      (matrix:ploop-matrix (res x y)
	(setf (elt (matrix:matrix-elt res y x) pixmap:+blue-channel+)
	      (truncate (elt (matrix:sample@ soil
					     (num:d/ (num:desired y)
						     (num:desired (matrix:height res)))
					     (num:d/ (num:desired x)
						     (num:desired (matrix:width  res))))
			     pixmap:+blue-channel+))))
      (loop for i in (trees object) do
	   (setf (elt (matrix:matrix-elt res (elt i 1) (elt i 0)) pixmap:+alpha-channel+)
		 0)))
    (setf (texture-weights object) res)
    res))

(defun %apply-cost-modifier (src dest channel modifier bonusp)
  (loop-matrix (dest x y)
     (let* ((px       (matrix-elt src y x))
	    (raw-cost (elt px channel)))
       (setf (matrix-elt dest y x)
	     (if bonusp
		 (max 1.0
		      (d- (matrix-elt dest y x)
			  (d* (d/ (desired raw-cost) 255.0) modifier)))
		 (d+ (matrix-elt dest y x)
		     (d* (d/ (desired raw-cost) 255.0) modifier)))))))

(defmethod build-cost-matrix ((object random-terrain) debugp)
  (with-accessors ((matrix matrix) (texture-weights texture-weights)
		   (cost-matrix cost-matrix)
		   (trees trees)) object
    (when (and texture-weights matrix)
      (let* ((res            (gen-matrix-frame (width matrix) (height matrix)
					       +open-terrain-cost+))
	     (heights        matrix)
	     (road-channel   pixmap:+red-channel+)
	     (mud-channel    pixmap:+blue-channel+))
	;;raw-terrain cost
	(loop-matrix (res x y)
	   (setf (matrix-elt res y x)
		 (let ((px (matrix-elt heights y x)))
		   (cond
		      ((epsilon= px +zero-height+) ;; flat terrain
		       +open-terrain-cost+)
		     ((d< px +zero-height+) ;; water
		      (if debugp
			  +invalicable-element-cost-dbg+
			  +invalicable-element-cost+))
		     (t                               ;; otherwise
		      (d+ +open-terrain-cost+
			  (d- (d* +costs-scale-mountain+ px) +zero-height+)))))))
	;; roads
	(if debugp
	    (%apply-cost-modifier texture-weights res road-channel
				  +road-terrain-max-cost-bonus-dbg+ nil)
	    (%apply-cost-modifier texture-weights res road-channel
				  +road-terrain-max-cost-bonus+ t))
	;; mud
	(if debugp
	    (%apply-cost-modifier texture-weights res
				  mud-channel +muddy-terrain-cost-penalty-dbg+ nil)
	    (%apply-cost-modifier texture-weights res
				  mud-channel +muddy-terrain-cost-penalty+ nil))
	;; scale
	(setf res (scale-matrix res +terrain-chunk-size-scale+ +terrain-chunk-size-scale+))
	;; labyrinths
	(loop for i from 0 below (length (labyrinths-aabb object)) do
	     (let* ((aabb   (elt (labyrinths-aabb object) i))
		    (startx (truncate (* (iaabb2-min-x aabb) +terrain-chunk-size-scale+)))
		    (starty (truncate (* (iaabb2-min-y aabb) +terrain-chunk-size-scale+)))
		    (matrix (random-labyrinth:shared-matrix (elt (labyrinths object) i))))
	       (loop-matrix (matrix x y)
		  (let ((pos-x (f+ startx x))
			(pos-y (f+ starty y)))
		    (cond
		      ;; ((random-labyrinth:doorp (matrix-elt matrix y x))
		      ;;  ;; cancel, if necessary, muddy terrain or others modifier
		      ;;  (setf (matrix-elt res pos-y pos-x) +open-terrain-cost+))
		      ((random-labyrinth:invalicablep (matrix-elt matrix y x))
		       (setf (matrix-elt res pos-y pos-x) (if debugp
							      +invalicable-element-cost-dbg+
							      +invalicable-element-cost+))))))))
	;;trees
	(loop for tree-pos in trees do
	     (let ((x (truncate (* (elt tree-pos 0) +terrain-chunk-size-scale+)))
	 	   (y (truncate (* (elt tree-pos 1) +terrain-chunk-size-scale+))))
	       (setf (matrix-elt res y x) (if debugp
					      +invalicable-element-cost-dbg+
					      +invalicable-element-cost+))))
	(setf cost-matrix res)))))

(defmethod get-cost ((object random-terrain) x y)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (if (cost-matrix object)
      (get-cost-insecure object x y)
      nil))

(defmethod get-cost-insecure ((object random-terrain) x y)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (matrix-elt (cost-matrix object) y x))

(defun test-make-map-10 (w h)
    (dotimes (count 10)
      (num:with-lcg-seed (count)
	(let ((map (make-instance 'random-terrain
				  :matrix (gen-empty-terrain w h))))
	  (make-map map)
	  (dump map (format nil
			    (fs:file-in-package "height-~a.pgm")
			    count))))))

(defun test-make-map (w h &optional (seed 2))
  (let* ((*workers-number* (if (> (os-utils:cpu-number) 1)
			       (os-utils:cpu-number)
			       1))
	 (lparallel:*kernel* (lparallel:make-kernel *workers-number*)))
    (num:with-lcg-seed (seed)
      (let ((map (make-instance 'random-terrain
				:matrix (gen-empty-terrain w h))))
	(make-map map)
	(dump map (fs:file-in-package "height.pgm"))
	(pixmap:save-pixmap (texture-weights map)
			    (fs:file-in-package "layers.tga"))
	;;dump
	(setf (data (cost-matrix map))
	      (map 'vector #'(lambda (a) (alexandria:clamp (round a) 0 255))
		   (data (cost-matrix map))))
	(with-open-file (stream
			 (fs:file-in-package "costs.pgm")
			 :direction :output
			 :if-exists :supersede :if-does-not-exist :create)
	  (format stream "~a" (pixmap:matrix->pgm (map-matrix (cost-matrix map) #'round) ""
						  (truncate +invalicable-element-cost+))))))
    t))
