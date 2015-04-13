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

(in-package :noise)

(defun coords->hash (&rest coords)
  (let ((octects (reduce #'(lambda (a b) (concatenate 'vector a b)) coords)))
    (fnv-hash-32 octects)))

(defun coords->hash* (coords)
  (let ((octects (reduce #'(lambda (a b) (concatenate 'vector a b)) coords)))
    (fnv-hash-32 octects)))

(defun get-square (x y granularity)
  (vector (* granularity (truncate (/ x granularity)))
	  (* granularity (truncate (/ y granularity)))))

(defun get-cube (x y z granularity)
  (vector (* granularity (truncate (/ x granularity)))
	  (* granularity (truncate (/ y granularity)))
	  (* granularity (truncate (/ z granularity)))))

(defun get-hyper-cube (x y z w granularity)
  (vector (* granularity (truncate (/ x granularity)))
	  (* granularity (truncate (/ y granularity)))
	  (* granularity (truncate (/ z granularity)))
	  (* granularity (truncate (/ w granularity)))))

(defun get-neighborhood-2d (center granularity)
   (let ((aabb (2d-utils:irect2->iaabb2 (concatenate 'list center
 						(list granularity granularity)))))
     
     ;;               G-----H-----I-----+
     ;;               |     |     |     |
     ;;               |     |     |     |
     ;;               E-----A-----B-----+
     ;;               |     |     |     |
     ;;               |     |     |     |
     ;;               F-----D-----C-----+
     ;;               |     |     |     |
     ;;               |     |     |     |
     ;;               +-----+-----+-----+
     
     (vector (vector (elt aabb 0) (elt aabb 1)) ; A
	     (vector (elt aabb 0) (elt aabb 3)) ; B
	     (vector (elt aabb 2) (elt aabb 3)) ; C 
	     (vector (elt aabb 2) (elt aabb 1)) ; D
	     (vector (- (elt aabb 0) granularity) (elt aabb 1)) ; E
	     (vector (- (elt aabb 0) granularity) (elt aabb 3)) ; F
	     (vector (- (elt aabb 0) granularity) (- (elt aabb 1) granularity)) ; G
	     (vector (elt aabb 0) (- (elt aabb 1) granularity)) ; H 
	     (vector (+ (elt aabb 0) granularity) (- (elt aabb 1) granularity)))))

(defun get-neighborhood-3d (center granularity)
  (let ((slice (get-neighborhood-2d (vector (elt center 0) (elt center 1)) granularity))
	(res nil))
    (loop for z from 0 below 3 do
	 (setf res (concatenate 'vector res
				(map 'vector #'(lambda (sq)
						 (concatenate 'vector sq
							      (vector (+ (elt center 2)
									 (* z granularity)))))
					slice))))
    res))

(defun get-neighborhood-4d (center granularity)
  (let ((slice (get-neighborhood-3d (vector (elt center 0) (elt center 1)
					    (elt center 2)) granularity))
	(res nil))
    (loop for w from 0 below 3 do
	 (setf res (concatenate 'vector res
				(map 'vector #'(lambda (sq)
						 (concatenate 'vector sq
							      (vector (+ (elt center 2)
									 (* w granularity)))))
					slice))))
    res))

(defmacro define-get-feature-points (dimensions)
  (alexandria:with-gensyms (how-many res)
    `(defun ,(alexandria:format-symbol t "~:@(get-feature-points-~ad~)" dimensions)
	 (cube granularity &optional (mean 4) (sigma 1) (max 9))
       (let* ((,how-many (ceiling (alexandria:clamp
				   (+ (first (num:random-gaussian-distribution sigma))
				      mean) 1 max)))
	     (,res (misc:make-array-frame ,how-many)))
	 (loop for i from 0 below ,how-many do
	      (setf (elt ,res i)
		    (vector
		     ,@(loop for i from 0 below dimensions collect
			    `(lcg-next-in-range (desired (elt cube ,i))
						(d+ (desired (elt cube ,i))
						    (desired granularity)))))))
	 ,res))))

(define-get-feature-points 2)

(define-get-feature-points 3)

(define-get-feature-points 4)

(defun euclidean-distance ()
  #'(lambda (px ft)
      (let ((delta (map 'vector
			#'(lambda (a b) (dexpt (d- (desired a) (desired b)) 2.0)) px ft)))
	(dsqrt (reduce #'(lambda(a b) (d+ a b)) delta)))))

(defmacro define-all-cubes (dimensions)
  (alexandria:with-gensyms (cube hash-key res)
    `(defun ,(alexandria:format-symbol t "~:@(all-cubes-~ad~)" (length dimensions))
	 ,(append dimensions '(granularity mean sigma max))
       (let ((,res (make-hash-table :test 'equalp)))
	 ,(let ((loops (misc:nest-expressions (loop for ctv in dimensions collect
						   `(loop for ,ctv from (- granularity)
						       to (+ ,ctv granularity) by granularity do))
					      `(let* ((,cube (vector ,@dimensions))
						      (,hash-key (coords->hash ,cube)))
						 (with-lcg-seed (,hash-key)
						   (setf (gethash ,cube ,res)
							 (,(alexandria:format-symbol t
										     "~:@(get-feature-points-~ad~)"
										     (length dimensions))
							   ,cube granularity mean sigma max)))))))
	       loops)
	   ,res))))

(define-all-cubes (x y))

(define-all-cubes (x y z))

(define-all-cubes (x y z w))

(defparameter *max-dist-to-store* 50)

(defun worley-2d-ref (x y granularity 
 		  &key
 		    (dist-fn (euclidean-distance))
 		    (sort-fn #'(lambda (a) (sort a #'<)))
 		    (mean 4) (sigma 1) (max 9) (all-feature-points nil))
   (let* ((cubes (get-neighborhood-2d (get-square x y granularity) granularity))
 	 (res (misc:make-array-frame 0)))
     (loop for cube across cubes do
 	 (with-lcg-seed ((coords->hash cube))
 	   (let ((feature-points (or (and all-feature-points 
 					  (gethash cube all-feature-points))
 				     (get-feature-points-2d cube granularity 
 							    mean sigma max))))
 	     (loop
 		for i across feature-points
 		for ct from 0 below (length feature-points) do
		  (vector-push-extend (funcall dist-fn (vec2 (desired x) (desired y)) i)
 					res)))))
     (funcall sort-fn res)))

(defmacro with-clear-cache-worley-2d (&body body)
  `(unwind-protect 
	,@body
     (clear-cache-worley-2d)))

(let ((cache (make-hash-table :test 'equalp))
      (parallel-utils:*parallel-setf-queue* 
       (lparallel.queue:make-queue :initial-contents '(t) 
				   :fixed-capacity 1)))

  (defun clear-cache-worley-2d () (setf cache (clrhash cache)))

  (defun worley-2d (x y granularity
		    &key
		      (dist-fn (euclidean-distance))
		      (sort-fn #'(lambda (a) (sort a #'<)))
		      (mean 4) (sigma 1) (max 9))
    (let* ((cubes (get-neighborhood-2d (get-square x y granularity) granularity))
	   (res (misc:make-array-frame (* (length cubes) max) nil t t))
	   (actual-elements-in-res 0))
      (loop 
	 for cube across cubes
	 for offset from 0 do
	   (let ((hashed-cube (coords->hash cube)))
	     (with-lcg-seed (hashed-cube)
	       (let ((feature-points (or (gethash hashed-cube cache)
					 (let ((new-points
						(get-feature-points-2d cube granularity 
								       mean sigma max)))
					   (parallel-utils:parallel-setf 
					    (gethash hashed-cube cache) new-points)
					   new-points))))
		 (loop
		    for i across feature-points
		    for ct from offset by 1 when (< ct (+ offset *max-dist-to-store*)) do
		      (setf (elt res ct)
			    (funcall dist-fn (vec2 (desired x) (desired y)) i)))
		 (incf offset (min (1- *max-dist-to-store*) (1- (length feature-points))))
		 (setf actual-elements-in-res offset)))))
      (setf res (subseq res 0 (1+ actual-elements-in-res)))
      (funcall sort-fn res))))

(defmacro with-clear-cache-worley-2d-seamless (&body body)
  `(with-clear-cache-worley-4d ,@body))

(defun worley-2d-seamless (x y w h granularity 
			   &key 
			     (dist-fn (euclidean-distance))
			     (sort-fn #'(lambda (a) (sort a #'<)))
			     (mean 4) (sigma 1) (max 9)
			     (x1 4.0) (x2 41.0) (y1 4.0) (y2 41.0))
  (let* ((sc (d/ (desired x) (desired w)))
	 (tc (d/ (desired y) (desired h)))
	 (dx (d- x2 x1))
	 (dy (d- y2 y1))
	 (nx (d+ x1 (d/ (d* (dcos (d* sc +2pi+)) dx) +2pi+)))
	 (ny (d+ y1 (d/ (d* (dcos (d* tc +2pi+)) dy) +2pi+)))
	 (nz (d+ x1 (d/ (d* (dsin (d* sc +2pi+)) dx) +2pi+)))
	 (nw (d+ y1 (d/ (d* (dsin (d* tc +2pi+)) dy) +2pi+))))
    (worley-4d nx ny nz nw granularity :dist-fn dist-fn :mean mean :sigma sigma :max max
	       :sort-fn sort-fn)))

(defmacro with-clear-cache-worley-3d (&body body)
  `(unwind-protect 
	,@body
     (clear-cache-worley-3d)))

(let ((cache (make-hash-table :test 'equalp))
      (parallel-utils:*parallel-setf-queue* 
       (lparallel.queue:make-queue :initial-contents '(t) 
				   :fixed-capacity 1)))

  (defun clear-cache-worley-3d () (setf cache (clrhash cache)))

  (defun worley-3d (x y z granularity 
		  &key 
		    (dist-fn (euclidean-distance))
		    (sort-fn #'(lambda (a) (sort a #'<)))
		    (mean 4) (sigma 4) (max 9))
  (let* ((cubes (get-neighborhood-3d (get-cube x y z granularity) granularity))
	 (res (misc:make-array-frame 0)))
    (loop for cube across cubes do
	 (let ((hashed-cube (coords->hash cube)))
	   (with-lcg-seed (hashed-cube)
	     (let ((feature-points (or (gethash hashed-cube cache)
					 (let ((new-points
						(get-feature-points-3d cube granularity 
								       mean sigma max)))
					   (parallel-utils:parallel-setf 
					    (gethash hashed-cube cache) new-points)
					   new-points))))
	       (loop
		  for i across feature-points
		  for ct from 0 below (length feature-points) do
		    (when (or (not *max-dist-to-store*) (< ct *max-dist-to-store*))
		      (vector-push-extend (funcall dist-fn 
						   (vec (desired x) 
							(desired y) 
							(desired z))
						   i)
					  res)))))))
    (funcall sort-fn res))))

(defmacro with-clear-cache-worley-4d (&body body)
  `(unwind-protect 
	,@body
     (clear-cache-worley-4d)))

(let ((cache (make-hash-table :test 'equalp))
      (parallel-utils:*parallel-setf-queue* 
       (lparallel.queue:make-queue :initial-contents '(t) 
				   :fixed-capacity 1)))

  (defun clear-cache-worley-4d () (setf cache (clrhash cache)))

  (defun worley-4d (x y z w granularity 
		  &key 
		    (dist-fn (euclidean-distance))
		    (sort-fn #'(lambda (a) (sort a #'<)))
		    (mean 4) (sigma 1) (max 9))
  (let* ((cubes (get-neighborhood-4d (get-hyper-cube x y z w granularity) granularity))
	 (res (misc:make-array-frame 0)))
    (loop 
       for cube across cubes
       for offset from 0 do
	 (let ((hashed-cube (coords->hash cube)))
	   (with-lcg-seed (hashed-cube)
	     (let ((feature-points (or (gethash hashed-cube cache)
				       (let ((new-points
					      (get-feature-points-4d cube granularity 
								     mean sigma max)))
					 (parallel-utils:parallel-setf 
					  (gethash hashed-cube cache) new-points)
					 new-points))))
	       (loop
		  for i across feature-points
		  for ct from offset by 1 when (< ct (+ offset *max-dist-to-store*)) do
		    (vector-push-extend (funcall dist-fn (vec4 (desired x) 
							       (desired y) 
							       (desired z)
							       (desired w)) 
						 i)
     					res))))))
    (funcall sort-fn res))))

(defparameter *perlin-gradient-random-offset* 0)

;; perlin noise permutation
(alexandria:define-constant +pnp+
  #(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142
    8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203
    117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74
    165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220
    105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132
    187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3
    64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227
    47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221
    153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185
    112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51
    145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121
    50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78
    66 215 61 156 180
    151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142
    8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203
    117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74
    165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220
    105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132
    187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3
    64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227
    47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221
    153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185
    112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51
    145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121
    50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78
    66 215 61 156 180) :test #'equalp)

(defun fade (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (desired-type v))
  (d* v v v (d+ (d* v (d- (d* v 6.0) 15.0)) 10.0)))

(defun grad (hash x y z)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (desired-type x y z))
  (declare ((unsigned-byte 9) hash))
  (let* ((h (logand hash 15))
	 (u (if (< h 8) x y))
	 (v (if (< h 4)
		y
		(if (or (= h 12)
			(= h 14))
		    x
		    z))))
    (+ (if (= (logand h 1) 0)
	   u
	   (- u))
       (if (= (logand h 2) 0)
	   v
	   (- v)))))

(declaim (ftype (function (desired-type desired-type desired-type)
			  desired-type) perlin-3d))

(defun perlin-3d (x y z)
  (declare (optimize (speed 1) (safety 0) (debug 0)))
  (declare (desired-type x y z))
  (let* ((offset *perlin-gradient-random-offset*)
	 (length-pnp (length +pnp+))
	 (x1 (logand (floor x) 255))
	 (y1 (logand (floor y) 255))
	 (z1 (logand (floor z) 255))
	 (fx (- x (floor x)))
	 (fy (- y (floor y)))
	 (fz (- z (floor z)))
	 (u  (fade fx))
	 (v  (fade fy))
	 (w  (fade fz))
	 (a  (+ (the fixnum (elt +pnp+ x1)) y1))
	 (aa (+ (the fixnum (elt +pnp+ a))  z1))
	 (ab (+ (the fixnum (elt +pnp+ (+ 1 a))) z1))
	 (b  (+ (the fixnum (elt +pnp+ (+ x1 1))) y1))
	 (ba (+ (the fixnum (elt +pnp+ b)) z1))
	 (bb (+ (the fixnum (elt +pnp+ (+ 1 b))) z1)))
    (declare (fixnum offset x1 y1 z1 a aa ab b ba bb length-pnp))
    (declare (desired-type u v w fx fy fz))
    (dlerp w
	   (dlerp v
		  (dlerp u
			 (grad (elt +pnp+ (mod (the fixnum (+ aa offset)) length-pnp))
				    fx fy fz)
			 (grad (elt +pnp+ (mod (the fixnum (+ ba offset)) length-pnp))
				    (d- fx 1.0) fy fz))
		  (dlerp u
			 (grad (elt +pnp+ (mod (the fixnum (+ ab offset)) length-pnp))
				    fx (d- fy 1.0) fz)
			 (grad (elt +pnp+ (mod (the fixnum (+ bb offset)) length-pnp))
				    (d- fx 1.0) (d- fy 1.0) fz)))
	   (dlerp v
		  (dlerp u
			 (grad (elt +pnp+ (mod (+ 1 aa offset) length-pnp))
				    fx fy (d- fz 1.0))
			 (grad (elt +pnp+ (mod (+ 1 ba offset) length-pnp))
				    (d- fx 1.0) fy (d- fz 1.0)))
		  (dlerp u
			 (grad (elt +pnp+ (mod (+ ab 1 offset) length-pnp))
				    fx (d- fy 1.0) (d- fz 1.0))
			 (grad (elt +pnp+ (mod (+ bb 1 offset) length-pnp))
				    (d- fx 1.0) (d- fy 1.0) (d- fz 1.0)))))))
(defun grad-2d (hash x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (desired-type x y))
  (declare ((unsigned-byte 9) hash))
  (+
   (if (= (logand hash 1) 0)
       x
       (- x))
   (if (= (logand hash 2) 0)
       y
       (- y))))

(declaim (ftype (function (desired-type desired-type) desired-type) perlin-2d))

(defun perlin-2d (x y)
  (declare (optimize (speed 1) (safety 0) (debug 0)))
  (declare (desired-type x y))
  (let* ((offset *perlin-gradient-random-offset*)
	 (length-pnp (length +pnp+))
	 (x1 (logand (floor x) 255))
	 (y1 (logand (floor y) 255))
	 (fx (- x (floor x)))
	 (fy (- y (floor y)))
	 (u  (fade fx))
	 (v  (fade fy))
	 (a  (+ (the fixnum (elt +pnp+ x1)) y1))
	 (b  (+ (the fixnum (elt +pnp+ (+ x1 1))) y1))
	 (aa (the fixnum (elt +pnp+ a)))
	 (ab (the fixnum (elt +pnp+ (+ 1 a))))
	 (ba (the fixnum (elt +pnp+ b)))
	 (bb (the fixnum (elt +pnp+ (+ 1 b)))))
    (declare (fixnum offset x1 y1 a aa ab b ba bb length-pnp))
    (declare (desired-type u v fx fy))
    (dlerp v
	   (dlerp u
		  (grad-2d (elt +pnp+ (mod (the fixnum (+ aa offset)) length-pnp))
			fx fy)
		  (grad-2d (elt +pnp+ (mod (the fixnum (+ ba offset)) length-pnp))
			(d- fx 1.0) fy))
	   (dlerp u
		  (grad-2d (elt +pnp+ (mod (the fixnum (+ ab offset)) length-pnp))
			fx (d- fy 1.0))
		  (grad-2d (elt +pnp+ (mod (the fixnum (+ bb offset)) length-pnp))
			(d- fx 1.0) (d- fy 1.0))))))

(declaim (inline %calc-hash))

(defun %calc-hash (coord)
  (the fixnum
    (mod (the fixnum (+ (the fixnum *perlin-gradient-random-offset*)
			(the fixnum (coords->hash coord)))) 2048)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defcached random-gradient-2d ((coord) :test equal)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (labels ((get-components (hash)
	       (declare (optimize (speed 3) (safety 0) (debug 0)))
	       (declare (fixnum hash))
	       (let* ((xc (lcg-next-in-range -2.0 2.0))
		      (yc (lcg-next-in-range -2.0 2.0))
		      (vec (vector xc yc)))
		 (declare (single-float xc yc))
		 (when (d<= (2d-utils:2d-vector-magn vec) 1.0)
		   (parallel-utils:parallel-setf (gethash hash cache)
						 (2d-utils:2d-vector-normalize vec))
		   (return-from random-gradient-2d (gethash hash cache)))
		 (get-components hash))))
      (let* ((hash (%calc-hash coord)))
	(or (gethash hash cache)
	    (with-lcg-seed (hash)
	      (get-components hash))))))

  (defcached random-gradient-3d ((coord) :test equal)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (labels ((get-components (hash)
	       (declare (optimize (speed 3) (safety 0) (debug 0)))
	       (declare (fixnum hash))
	       (let ((grad (sb-cga:vec (lcg-next-in-range -1.0 1.0)
				       (lcg-next-in-range -1.0 1.0)
				       (lcg-next-in-range -1.0 1.0))))
		 (when (d<= (sb-cga:vec-length grad) 1.0)
		   (parallel-utils:parallel-setf (gethash hash cache)
					  (sb-cga:normalize grad))
		   (return-from random-gradient-3d (gethash hash cache)))
		 (get-components hash))))
      (let ((hash (%calc-hash coord)))
	(or (gethash hash cache)
	    (with-lcg-seed (hash)
	      (get-components hash))))))

  (defcached random-gradient-4d ((coord) :test equal)
    (declare (optimize (speed 0) (safety 3) (debug 3)))
    (labels ((get-components (hash)
	       (declare (optimize (speed 3) (safety 0) (debug 0)))
	       (declare (fixnum hash))
	       (let ((grad (vec4 (lcg-next-in-range -1.0 1.0)
					  (lcg-next-in-range -1.0 1.0)
					  (lcg-next-in-range -1.0 1.0)
					  (lcg-next-in-range -1.0 1.0))))
		 (when (d<= (vec4-length grad) 1.0)
		   (parallel-utils:parallel-setf (gethash hash cache)
					  (vec4-normalize grad))
		   (return-from random-gradient-4d (gethash hash cache)))
		 (get-components hash))))
      (let* ((hash (%calc-hash coord))
	     (comp (or (gethash hash cache)
		       (with-lcg-seed (hash)
			 (get-components hash)))))
	comp))))

(declaim (ftype (function (desired-type desired-type) vector) get-grid-2d))

(defun get-grid-2d (x y)
  (declare (desired-type x y))

  ;; 0    1
  ;; +----+
  ;; |    |
  ;; |    |
  ;; +----+
  ;; 3    2
  (let* ((fx (floor x))
	 (fy (floor y)))
    (declare ((signed-byte 32) fx fy))
    (vector (vector fx fy)
	    (vector (1+ fx) fy)
	    (vector (1+ fx) (1+ fy))
	    (vector fx (1+ fy)))))

(declaim (ftype (function (desired-type desired-type desired-type) vector)
		get-grid-3d))

(defun get-grid-3d (x y z)
  (declare (inline floor))
  ;; 0    1
  ;; +----+
  ;; |    |
  ;; |    |
  ;; +----+
  ;; 3    2
  (let* ((fx (floor x))
	 (fy (floor y))
	 (fz (floor z)))
    (vector (vector fx fy fz)
	    (vector (1+ fx) fy fz)
	    (vector (1+ fx) (1+ fy) fz)
	    (vector fx (1+ fy) fz)
	    (vector fx fy (1+ fz))
	    (vector (1+ fx) fy (1+ fz))
	    (vector (1+ fx) (1+ fy) (1+ fz))
	    (vector fx (1+ fy) (1+ fz)))))

(declaim (ftype (function (desired-type
			   desired-type
			   desired-type desired-type) vector)
		get-grid-4d))

(defun get-grid-4d (x y z w)
  (declare (inline floor))
  ;; 0    1
  ;; +----+
  ;; |    |
  ;; |    |
  ;; +----+
  ;; 3    2
  (let* ((fx (floor x))
	 (fy (floor y))
	 (fz (floor z))
	 (fw (floor w)))
    (vector (vector fx fy fz fw)
	    (vector (1+ fx) fy fz fw)
	    (vector (1+ fx) (1+ fy) fz fw)
	    (vector fx (1+ fy) fz fw)
	    (vector fx fy (1+ fz) fw)
	    (vector (1+ fx) fy (1+ fz) fw)
	    (vector (1+ fx) (1+ fy) (1+ fz) fw)
	    (vector fx (1+ fy) (1+ fz) fw)
	    (vector fx fy fz (1+ fw))
	    (vector (1+ fx) fy fz (1+ fw))
	    (vector (1+ fx) (1+ fy) fz (1+ fw))
	    (vector fx (1+ fy) fz (1+ fw))
	    (vector fx fy (1+ fz) (1+ fw))
	    (vector (1+ fx) fy (1+ fz) (1+ fw))
	    (vector (1+ fx) (1+ fy) (1+ fz) (1+ fw))
	    (vector fx (1+ fy) (1+ fz) (1+ fw)))))

(defun perlin-4d (x y z w)
  (let* ((grid (get-grid-4d x y z w))
	 (gradients (loop for i across grid collect (random-gradient-4d i)))
	 (diffs (loop for i across grid collect
		     (vec4- (vec4 x y z w)
				     (vec4 (desired (svref i 0))
						    (desired (svref i 1))
						    (desired (svref i 2))
						    (desired (svref i 3))))))
	 (dot-products (map 'vector #'(lambda (a b)
					(vec4-dot-product b a))
			    gradients diffs))
    	 (wx (fade (d- x (desired (floor x)))))
	 (wy (fade (d- y (desired (floor y)))))
	 (wz (fade (d- z (desired (floor z)))))
	 (ww (fade (d- w (desired (floor w)))))
	 (a  (dlerp wx (svref dot-products 0) (svref dot-products 1)))
	 (b  (dlerp wx (svref dot-products 3) (svref dot-products 2)))
	 (ab (dlerp wy a b))
	 (c  (dlerp wx (svref dot-products 4) (svref dot-products 5)))
	 (d  (dlerp wx (svref dot-products 7) (svref dot-products 6)))
	 (cd (dlerp wy c d))
	 (z (dlerp wz ab cd))
	 (a1  (dlerp wx (svref dot-products 8) (svref dot-products 9)))
	 (b1  (dlerp wx (svref dot-products 11) (svref dot-products 10)))
	 (ab1 (dlerp wy a1 b1))
	 (c1  (dlerp wx (svref dot-products 12) (svref dot-products 13)))
	 (d1  (dlerp wx (svref dot-products 15) (svref dot-products 14)))
	 (cd1 (dlerp wy c1 d1))
	 (z1 (dlerp wz ab1 cd1))
	 (res (dlerp ww z z1)))
    res))

(defun perlin-2d-seamless (x y &optional (x1 4.0) (x2 14.0) (y1 4.0) (y2 14.0))
  (let* ((dx (d- x2 x1))
	 (dy (d- y2 y1))
	 (nx (d+ x1 (d/ (d* (dcos (d* x +2pi+)) dx) +2pi+)))
	 (ny (d+ y1 (d/ (d* (dcos (d* y +2pi+)) dy) +2pi+)))
	 (nz (d+ x1 (d/ (d* (dsin (d* x +2pi+)) dx) +2pi+)))
	 (nw (d+ y1 (d/ (d* (dsin (d* y +2pi+)) dy) +2pi+))))
    (perlin-4d nx ny nz nw)))

(defun gen-fbm (x y layers frequency amplitude ampl-step freq-step
		&key (normalize t) (range-0->1 nil))
  (let ((fbm (loop 
		for l from 0 below layers
		for freq = frequency then (d* freq freq-step)
		for ampl = amplitude then (d* ampl ampl-step) sum
		  (let ((noise (perlin-2d (d* x freq) (d* y freq))))
		    (d* (if range-0->1
			    (range-0to1 noise)
			    noise)
			ampl)))))
    (if normalize
	(d/ fbm (loop 
		   for l from 0 below layers
		   for ampl = amplitude then (d* ampl ampl-step) sum ampl))
	fbm)))

(defun gen-fbm-seamless (x y layers frequency amplitude ampl-step freq-step
			 &key (normalize t) (range-0->1 nil)
			   (x1 4.0) (x2 14.0) (y1 4.0) (y2 14.0))
  (let ((fbm (loop 
		for l from 0 below layers
		for freq = frequency then (d* freq freq-step)
		for ampl = amplitude then (d* ampl ampl-step) sum
		  (let ((noise (perlin-2d-seamless (d* x freq) (d* y freq) x1 x2 y1 y2)))
		    (d* (if range-0->1
			    (range-0to1 noise)
			    noise)
			ampl)))))
    (if normalize
	(d/ fbm (loop 
		   for l from 0 below layers
		   for ampl = amplitude then (d* ampl ampl-step) sum ampl))
	fbm)))

(defun gen-abs-fbm-seamless (x y layers frequency amplitude ampl-step freq-step
			     &key (normalize t))
  (let ((fbm (loop 
		for l from 0 below layers
		for freq = frequency then (d* freq freq-step)
		for ampl = amplitude then (d* ampl ampl-step) sum
		  (d* (dabs (perlin-2d-seamless (d* x freq) (d* y freq))) ampl))))
    (if normalize
	(d/ fbm (loop 
		   for l from 0 below layers
		   for ampl = amplitude then (d* ampl ampl-step) sum ampl))
	fbm)))

(defun gen-abs-fbm (x y layers frequency amplitude ampl-step freq-step &key (normalize t))
  (let ((fbm (loop 
		for l fixnum from 0 below layers
		for freq = frequency then (d* freq freq-step)
		for ampl = amplitude then (d* ampl ampl-step) sum
		  (d* (dabs 
		       (perlin-2d (d* x freq) (d* y freq)))
		      ampl))))
    (if normalize
	(d/ fbm (loop 
		   for l fixnum from 0 below layers
		   for ampl = amplitude then (d* ampl ampl-step) sum ampl))
	fbm)))

(defun gen-abs-fbm3 (x y z layers frequency amplitude ampl-step freq-step
		     &key (normalize t))
  (let ((fbm (loop 
		for l from 0 below layers
		for freq = frequency then (d* freq freq-step)
		for ampl = amplitude then (d* ampl ampl-step) sum
		  (d* (dabs (perlin-3d (d* x freq) (d* y freq) (d* z freq))) ampl))))
    (if normalize
	(d/ fbm (loop
		   for l from 0 below layers
		   for ampl = amplitude then (d* ampl ampl-step) sum ampl))
	fbm)))

(defmacro with-random-perlin-gradient-offset (&body body)
  `(let ((*perlin-gradient-random-offset* (lcg-next)))
     ,@body))
