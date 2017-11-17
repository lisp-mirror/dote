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

(alexandria:define-constant +default-size-pixmap-library+ 512 :test #'=)

(defmacro with-draw-normalizated-coord-square ((x y size pixmap depth &key (bindings nil))
                                               &body body)
  `(let* ((,pixmap (make-pixmap-frame ,size ,size ,depth)))
     (ploop-matrix (,pixmap ,x ,y :bindings ,bindings)
       (draw-normalized-coord ,pixmap
                              (d/ (desired ,x) (desired ,size))
                              (d/ (desired ,y) (desired ,size))
                              (lambda (,(intern "X") ,(intern "Y")) ,@body)))
     ,pixmap))

(defmacro with-draw-normalizated-coord ((x y w h pixmap depth &key (bindings nil))
                                        &body body)
  `(let* ((,pixmap (make-pixmap-frame ,w ,h ,depth)))
     (ploop-matrix (,pixmap ,x ,y :bindings ,bindings)
       (draw-normalized-coord ,pixmap
                              (d/ (desired ,x) (desired ,w))
                              (d/ (desired ,y) (desired ,h))
                              (lambda (,(intern "X") ,(intern "Y")) ,@body)))
     ,pixmap))

(defmacro with-draw-float-normalizated-coord-square ((x y size pixmap depth
                                                        &key (bindings nil))
                                             &body body)
  `(let* ((,pixmap (make-pixmap-frame ,size ,size ,depth)))
     (ploop-matrix (,pixmap ,x ,y :bindings ,bindings)
       (draw-float-normalized-coord ,pixmap
                                    (d/ (desired ,x) (desired ,size))
                                    (d/ (desired ,y) (desired ,size))
                                    (lambda (,(intern "X") ,(intern "Y")) ,@body)))
     ,pixmap))

(defmacro with-draw-float-normalizated-coord ((x y w h pixmap depth &key (bindings nil))
                                             &body body)
  `(let* ((,pixmap (make-pixmap-frame ,w ,h ,depth)))
     (ploop-matrix (,pixmap ,x ,y :bindings ,bindings)
       (draw-float-normalized-coord ,pixmap
                                    (d/ (desired ,x) (desired ,w))
                                    (d/ (desired ,y) (desired ,h))
                                    (lambda (,(intern "X") ,(intern "Y")) ,@body)))
     ,pixmap))

(defmacro with-standard-generated-pixmap-square ((pixmap size) &body body)
  `(with-draw-normalizated-coord-square (x y ,size ,pixmap 4 :bindings (*perlin-gradient-random-offset* *max-dist-to-store* *lcg-seed*))
     ,@body))

(defmacro with-standard-float-generated-pixmap-square ((pixmap size) &body body)
  `(with-draw-float-normalizated-coord-square (x y ,size ,pixmap 4 :bindings (*perlin-gradient-random-offset* *max-dist-to-store* *lcg-seed*))
     ,@body))

(defmacro with-standard-generated-pixmap ((pixmap w h) &body body)
  `(with-draw-normalizated-coord (x y ,w ,h ,pixmap 4
                                          :bindings
                                          (*perlin-gradient-random-offset* *max-dist-to-store* *lcg-seed*))
     ,@body))

(defmacro with-standard-float-generated-pixmap ((pixmap w h) &body body)
  `(with-draw-float-normalizated-coord (x y ,w ,h ,pixmap 4
                                                :bindings
                                                (*perlin-gradient-random-offset* *max-dist-to-store* *lcg-seed*))
     ,@body))

(defgeneric gen-normal-map (object &key roughness))

(defmethod gen-normal-map ((object pixmap) &key (roughness 1.0))
  (let ((edges (map-matrix
                (psobel-edge-detection
                 (map-matrix (to-grayscale object)
                             #'(lambda (a) (color-utils:byte->float (elt a 0)))))
                #'(lambda (a)
                    (sb-cga-utils:safe-normalize
                     (vec (d+ (d/ (elt a 0) 2.0) 0.5)
                          (d+ (d/ (elt a 1) 2.0) 0.5)
                          (d+ (d/ (d- 1.0 (d* roughness (elt a 2))) 2.0) 0.5)))))))
    (matrix->pixmap (map-matrix edges
                                #'(lambda (a)
                                    (ubvec4 (color-utils:float->byte (elt a 0))
                                                      (color-utils:float->byte (elt a 1))
                                                      (color-utils:float->byte (elt a 2))
                                                      255))))))

(defmethod gen-normal-map ((object string) &key (roughness 1.0))
  (let ((tga-file (make-instance 'tga :path object)))
    (gen-normal-map tga-file :roughness roughness)))

(defun fuzzy-frame (size width fuzziness blurring-radius &key (inner-color #x000000ee)
                                                           (outer-color #xffffff00))
  (with-random-perlin-gradient-offset
    (let* ((pixmap (make-instance 'pixmap
                                  :depth 4
                                  :height size
                                  :width size
                                  :data (matrix:data
                                         (pgaussian-blur-separated
                                          (with-draw-normalizated-coord-square
                                              (x y size pixmap 4
                                                 :bindings (*perlin-gradient-random-offset*))
                                            (let* ((turbulence (d* fuzziness
                                                                   (gen-fbm x y 6 1.0
                                                                            1.0 0.65 2.0
                                                                            :normalize nil)))
                                                   (actx (d+ (desired x) turbulence))
                                                   (acty (d+ (desired y) turbulence)))
                                              (if (or (> actx (d- 1.0 width)) ; the frame
                                                      (> acty (d- 1.0 width))
                                                      (< acty width)
                                                      (< actx width))
                                                  (int->vec4 inner-color)
                                                  (int->vec4 outer-color))))
                                          #'floor
                                          blurring-radius)))))
      (values pixmap "fuzzy-frame"))))

(defun test-fuzzy-frame ()
  (let ((pixmap (fuzzy-frame 512 0.039 0.25 20)))
    (with-open-file (stream (fs:file-in-package "frame.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun fuzzy-circular-frame (size inner-radius outer-radius color-1 color-2 fuzziness)
  (with-random-perlin-gradient-offset
    (let ((pixmap (with-draw-normalizated-coord-square (x y size pixmap 4
                                                  :bindings (*perlin-gradient-random-offset*))
                    (let* ((turbulence (d* fuzziness
                                           (gen-fbm x y 6 1.0 1.0 0.65 2.0 :normalize nil)))
                           (p          (vector (d- x 0.5 turbulence) (d- y 0.5 turbulence)))
                           (magn-p     (2d-utils:2d-vector-magn p))
                           (max        (d* (desired +default-size-pixmap-library+)
                                           (desired +default-size-pixmap-library+)))
                           (weight     (smoothstep-interpolate inner-radius outer-radius
                                                               (alexandria:clamp magn-p 0.0 max))))
                      (mix-color color-1 color-2 weight)))))
      (values pixmap "fuzzy-circular-frame"))))

(defun test-fuzzy-circular-frame ()
  (let ((pixmap (fuzzy-circular-frame 512 0.15 0.21 #xffffffaa
                                      #x00000000 0.05)))
    (with-open-file (stream (fs:file-in-package "circular-frame.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun wood-log-texture (&optional
                           (height +default-size-pixmap-library+)
                           (color-1 §c583926ff)
                           (color-2 §c6F4832ff)
                           (scale-factor-x 0.125)
                           (scale-factor-y 2.5))
  (with-random-perlin-gradient-offset
    (let* ((size height)
           (pixmap (make-pixmap-frame size size 4))
           (rotation-angle (num:lcg-next-upto (d/ 90.0 8.0)))
           (translate-x (desired (num:lcg-next-upto 2.0)))
           (translate-y (desired (num:lcg-next-upto 2.0))))
      (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
        (draw-normalized-coord pixmap
                               (d/ (desired x) (desired size))
                               (d/ (desired y) (desired size))
                               (lambda (x y)
                                 (let* ((p (vector (d- translate-x (desired x))
                                                   (d- translate-y (desired y))))
                                        (magn-p (2d-utils:2d-vector-magn p))
                                        (displ (d+
                                                (dsin
                                                 (d+
                                                  (d* 2.0
                                                      (perlin-2d
                                                       (d* (desired x) 10.0)
                                                       (d* (desired y) 10.0)))
                                                  (d* magn-p 100.0)))))
                                        (weight (alexandria:clamp
                                                 (d+ (dsin (d* 20.0 magn-p))
                                                     displ
                                                     (perlin-2d
                                                      (d* (desired x) 20.0)
                                                      (d* (desired y) 20.0)))
                                                 -1.0 1.0)))
                                   (map 'vec4 #'(lambda (a b)
                                                    (dlerp (dabs weight) a b))
                                        color-1
                                        color-2)))))
      (ncopy-matrix-into-pixmap pixmap (rotate-matrix
                                        (scale-matrix pixmap scale-factor-x scale-factor-y)
                                        rotation-angle :fill-value (ubvec4 0 0 0)
                                        :repeat t)
                                4)
      (values pixmap "wood-log-texture"))))

(defun wood-wall (&optional (height +default-size-pixmap-library+)
                    (color-1 §c583926ff)
                    (color-2 §c6F4832ff))
  (let* ((slice (wood-log-texture height color-1 color-2 1.0 1.0))
         (pixmap (make-pixmap-frame height height 4))
         (draw-fn (lambda (x y)
                    (let ((y-offset (/ (desired (floor (d* 4.0 x))) 4)))
                      (sample@ slice (d* x 4.0) (d+ y y-offset) :interpolation t :clamp nil)))))
    (ploop-matrix (pixmap x y)
       (draw-normalized-coord-custom-conversion pixmap
                                                (d/ (desired x) (desired height))
                                                (d/ (desired y) (desired height))
                                                draw-fn
                                                #'identity))
    pixmap))

(defun test-wood-wall ()
  (save-pixmap (wood-wall) (fs:file-in-package "wood-wall.tga"))
  t)

(defun rock-strain (x y color-1 color-2)
  (let* ((p (vector x y))
         (fbm (gen-abs-fbm x y 4 1.0 1.0 0.5 2.0 :normalize t))
         (pr (2d-utils:2d-vector-rotate p (d* fbm 2.1)))
         (afbm (gen-abs-fbm x y 8 1.0 1.0 0.5 1.8715 :normalize nil)))
    (mix-color color-1 color-2
               (dabs (d* 0.5 (dsin (d+ (d* 10.0 afbm) (d* 8.0 (elt pr 0)))))))))

(defun dry-stone-wall (&optional (mean 6) (sigma 4) (max 9))
  (let ((noise:*max-dist-to-store* max))
    (with-random-perlin-gradient-offset
      (with-clear-cache-worley-2d
        (let* ((size 256)
               (frame (fuzzy-frame size 0.039 0.015 20))
               (pixmap (pixmap:make-pixmap-frame size size 4))
               (granularity 150)
               (translate-x (num:lcg-next-in-range 0.0 (d* 2.0 (desired size))))
               (translate-y (num:lcg-next-in-range 0.0 (d* 2.0 (desired size)))))
          (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*
                                               *max-dist-to-store*))
            (let* ((float-x (d/ (desired x) (desired (width pixmap))))
                   (float-y (d/ (desired y) (desired (height pixmap))))
                   (turbulence (d* 20.0 (gen-abs-fbm float-x float-y 6 1.0 1.0 0.65 2.0
                                                     :normalize nil)))
                   (actx (d+ translate-x (desired x) turbulence))
                   (acty (d+ translate-y (desired y) turbulence))
                   (min (worley-2d actx acty granularity :mean mean :sigma sigma :max max)))
              (setf (pixel@ pixmap x y) (d- (elt min 1) (elt min 0)))))
          (let ((max (find-max (data pixmap))))
            (setf (data pixmap) (map 'vector #'(lambda (a)
                                                 (vec4 (d/ a max)
                                                       (d/ a max)
                                                       (d/ a max)
                                                       1.0))
                                     (data pixmap))))
          (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
            (setf (pixel@ pixmap x y)
                  (map 'ubvec4 #'float->byte
                       (let ((strain (rock-strain (d/ (desired x) (desired size))
                                                  (d/ (desired y) (desired size))
                                                  #xffffffff #x000000ff))
                             (bryophyte (rock-strain (d/ (desired x) (desired size))
                                                     (d/ (desired y) (desired size))
                                                     #x212c11ff #x000000ff)))
                         (mix-color #x000000ff
                                    (mix-color
                                     bryophyte
                                     (mix-color (pixel@ pixmap x y)
                                                strain
                                                (alexandria:clamp
                                                 (2d-utils:2d-vector-magn
                                                  (pixel@ pixmap x y))
                                                 0.0 1.0))
                                     (alexandria:clamp
                                      (2d-utils:2d-vector-magn
                                       (pixel@ pixmap x y))
                                      0.4 1.0))
                                    (d- 1.0 (byte->float (elt (pixel@ frame x y) 3))))))))
          (values pixmap "dry-stone-wall"))))))

(defun dry-stone-floor (&optional
                             (mean 8) (sigma 1) (max 9)
                             (gradient
                              (make-gradient
                               (make-gradient-color 1.0  §c454f5fff)
                               (make-gradient-color 0.66 §c9a9a9aff)
                               (make-gradient-color 0.33 §c9a785dff)
                               (make-gradient-color 0.0  §c803a30ff))))
  (let ((noise:*max-dist-to-store* max))
  (with-random-perlin-gradient-offset
    (let* ((size 256)
           (granularity 80)
           (worley (2d-worley-lut-indexed size granularity 0 0
                                          mean sigma max))
           (pixmap (make-pixmap-frame size size)))
      (ploop-matrix (worley x y :bindings (*perlin-gradient-random-offset*
                                           *max-dist-to-store*))
        (setf (pixel@ worley x y)
              (vec4->byte-vector
               (mix-color
                §c00000000
                (pick-color gradient (desired (/ (elt (pixel@ worley x y) 1) +lcg-max+)))
                (smoothstep-interpolate 0.0 10.0 (elt (pixel@ worley x y) 2))))))
      (setf worley (pgaussian-blur-separated worley #'identity 10))
      (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
        (draw-normalized-coord pixmap
                               (d/ (desired x) (desired size))
                               (d/ (desired y) (desired size))
                               #'(lambda (x y)
                                   (let ((noise (gen-fbm x y 6 10.0 1.0 0.65 2.0
                                                         :normalize t :range-0->1 t)))
                                     (multiply-color
                                      (byte-vector->vec4 (sample@ worley x y :clamp t))
                                      (vec4 noise noise noise 1.0))))))
      (values pixmap "dry-stone-floor")))))

(defun grass-stones-floor (size)
  (let ((grass (grass size))
        (pav (dry-stone-floor)))
    (ploop-matrix (grass x y :bindings (*perlin-gradient-random-offset*))
      (draw-normalized-coord
       grass
       (d/ (desired x) (desired size))
       (d/ (desired y) (desired size))
       #'(lambda (x y)
           (let ((alpha-pav (byte->float (elt (sample@ pav x y) 3))))
             (let ((col (mix-color
                         (map 'vec4 #'(lambda(a) (d* (byte->float a) 1.0)) (sample@ grass x y))
                         (map 'vec4 #'(lambda(a) (d* (byte->float a) 1.0)) (sample@ pav x y))
                         (alexandria:clamp (d* 2.0 alpha-pav) 0.0 1.0))))
               (setf (elt col 3) 1.0)
               col)))))
    (values grass "grass-stones-floor")))

(defun grass-dirty (&optional (mean 6) (sigma 4) (max 9))
  (with-clear-cache-worley-2d
    (with-random-perlin-gradient-offset
      (let* ((size +default-size-pixmap-library+)
             (pixmap (pixmap:make-pixmap-frame size size 4))
             (granularity 150))
        (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
          (let* ((float-x (d/ (desired x) (desired (width pixmap))))
                 (float-y (d/ (desired y) (desired (height pixmap))))
                 (turbulence (d* 20.0 (gen-abs-fbm float-x float-y 6 1.0 1.0 0.65 2.0
                                                   :normalize nil)))
                 (translate-x (num:lcg-next-in-range 0.0 (d* 2.0 (desired size))))
                 (translate-y (num:lcg-next-in-range 0.0 (d* 2.0 (desired size))))
                 (actx (d+ translate-x (desired x) turbulence))
                 (acty (d+ translate-y (desired y) turbulence))
                 (min (worley-2d actx acty granularity :mean mean :sigma sigma :max max)))
            (setf (pixel@ pixmap x y) (d- (elt min 1) (elt min 0)))))
        (let ((max (find-max (data pixmap))))
          (setf (data pixmap) (map 'vector #'(lambda (a)
                                               (vec4 (d/ a max)
                                                     (d/ a max)
                                                     (d/ a max)
                                                     1.0))
                                   (data pixmap))))
        (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
          (setf (pixel@ pixmap x y)
                (map 'vector #'float->byte
                     (let ((strain (rock-strain (d/ (desired x) (desired size))
                                                (d/ (desired y) (desired size))
                                                #xffffffff #x000000ff))
                           (bryophyte (rock-strain (d/ (desired x) (desired size))
                                                   (d/ (desired y) (desired size))
                                                   #x212c11ff #x000000ff)))
                       (mix-color
                        bryophyte
                        (mix-color (pixel@ pixmap x y)
                                   strain
                                   (alexandria:clamp
                                    (2d-utils:2d-vector-magn
                                     (pixel@ pixmap x y))
                                    0.0 1.0))
                        (alexandria:clamp
                         (2d-utils:2d-vector-magn
                          (pixel@ pixmap x y))
                         0.4 1.0))))))
        (values pixmap "grass-dirty")))))

(defun 2d-worley-lut (size granularity displacement-x displacement-y
                      mean sigma max fun
                      &key
                        (dist-fn (euclidean-distance))
                        (sort-fn #'(lambda (a) (declare (vector a))(sort a #'<)))
                        (normalize t))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (fixnum size displacement-x displacement-y))
  (declare (function fun))
  (with-clear-cache-worley-2d
    (let* ((pixmap (pixmap:make-pixmap-frame size size 4)))
      (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*
                                           *max-dist-to-store*))
        (let* ((min (worley-2d (f+ displacement-x (the fixnum x))
                               (f+ displacement-y (the fixnum y)) granularity
                               :mean mean :sigma sigma :max max
                               :dist-fn dist-fn :sort-fn sort-fn)))
          (setf (pixel@ pixmap x y) (funcall fun min))))
      (when normalize
        (let ((max (find-max (data pixmap))))
          (setf (data pixmap) (map '(simple-array desired-type (*))
                                   #'(lambda (a) (d/ a max))
                                   (the (simple-array t (*)) (data pixmap))))))
      pixmap)))

(defun 2d-worley-lut-indexed (size granularity displacement-x displacement-y
                              mean sigma max
                              &key (fun #'(lambda (a)
                                            (list (elt (elt a 0) 0) ;; index
                                                  (elt (elt a 0) 1) ;; index-hash
                                                  (d- (elt (elt a 1) 2)
                                                      (elt (elt a 0) 2))))))
  (2d-worley-lut size granularity displacement-x displacement-y mean sigma max fun
                 :dist-fn #'(lambda (pos index)
                              (vector
                               index
                               (fnv-hash-32 (round-all index))
                               (funcall (euclidean-distance) pos index)))
                 :sort-fn #'(lambda (a) (sort a #'< :key #'(lambda (a) (elt a 2))))
                 :normalize nil))

(defun test-worley-indexed (size)
  (let* ((lut (2d-worley-lut-indexed size 100 0 0 8 2 32))
         (gradient (make-gradient
                    (make-gradient-color 1.0  §cff0000ff)
                    (make-gradient-color 0.26 §cffff00ff)
                    (make-gradient-color 0.25  §caaaa00ff)
                    (make-gradient-color 0.20  §cdddd00ff)
                    (make-gradient-color 0.10  §cffff00ff)
                    (make-gradient-color 0.0  §cffffffff)))
         (pixmap (make-pixmap-frame size size)))
    (ploop-matrix (pixmap x y)
      (setf (pixel@ pixmap x y)
            (vec4->byte-vector
             (mix-color
              §c000000ff
              (pick-color gradient (desired (/ (elt (pixel@ lut x y) 1) +lcg-max+)))
              (smoothstep-interpolate 0.0 10.0 (elt (pixel@ lut x y) 2))))))
    (with-open-file (stream (fs:file-in-package "worley.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-worley ()
  (let* ((lut (2d-worley-lut 128 100 0 0 8 2 32
                             #'(lambda (a) (d- (second (elt a 1)) (second (elt a 0))))
                             :dist-fn #'(lambda (pos index)
                                          (list (fnv-hash-32 (round-all index))
                                                (funcall (euclidean-distance) pos index)))
                             :sort-fn #'(lambda (a) (sort a #'< :key #'second))))
         (pixmap (with-draw-normalizated-coord-square (x y 128 pixmap 4)
                   (vector (sample@ lut x y)
                           (sample@ lut x y)
                           (sample@ lut x y)
                           1.0))))
    (with-open-file (stream (fs:file-in-package "worley.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-worley-seamless ()
  (let* ((lut (2d-worley-lut-seamless 128 10 0 0 8 2 32
                                      #'(lambda (a) (d- (elt a 1) (elt a 0)))))
         (pixmap (with-draw-normalizated-coord-square (x y 128 pixmap 4)
                   (vector (sample@ lut x y)
                           (sample@ lut x y)
                           (sample@ lut x y)
                           1.0))))
    (with-open-file (stream (fs:file-in-package "worley.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun 2d-worley-lut-seamless (size granularity displacement-x displacement-y mean sigma max fun)
  (with-clear-cache-worley-2d-seamless
    (let* ((pixmap (pixmap:make-pixmap-frame size size 4)))
      (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*
                                           *max-dist-to-store*))
        (let* ((min (worley-2d-seamless (+ displacement-x x) (+ displacement-y y)
                                        size size granularity
                                        :x1 4.0 :x2 41.0 :y1 4.0 :y2 41.0
                                        :mean mean :sigma sigma :max max)))
          (setf (pixel@ pixmap x y) (funcall fun min))))
      (let ((max (find-max (data pixmap))))
        (setf (data pixmap) (map 'vector #'(lambda (a) (d/ a max)) (data pixmap))))
      pixmap)))

(defun sun-spikes (size &key (inner-color §cffffffff) (outer-color §cffff0000)
                          (spikes '(20.0 135.0))
                          (core-size 0.1)
                          (spot-length 1.9)
                          (spot-radial-cutoff 10.0))
  (let ((spikes-vectors (map 'vector #'(lambda (a)
                                         (let ((radian (deg->rad a)))
                                           (vector (dcos radian) (dsin radian))))
                             spikes)))
    (values
     (with-draw-normalizated-coord-square (x y size pixmap 4)
       (reduce #'(lambda (a b) (map 'vec4 #'max a b))
               (loop for spike across spikes-vectors collect
                    (let* ((*default-epsilon* 0.1)
                           (p (2d-vector-translate (vector x y) -0.5))
                           (magn-p (2d-utils:2d-vector-magn p))
                           (normalized-p (if (epsilon= 0.0 magn-p)
                                             (vector 0.0 0.0)
                                             (2d-vector-normalize p)))
                           (dot-product (2d-vector-dot-product normalized-p spike)))
                      (if (and (d> magn-p core-size)
                               (epsilon= 0.0 dot-product))
                          (vec4 (elt inner-color 0)
                                (elt inner-color 1)
                                (elt inner-color 2)
                                (smoothstep-interpolate 0.0 1.0
                                                        (d- 1.0
                                                            (max (d* spot-length magn-p)
                                                                 (dabs (d* spot-radial-cutoff dot-product))))))
                          outer-color)))
               :initial-value §c00000000))
     "sun-spikes")))

(defun test-spikes ()
  (let ((pixmap (sun-spikes 256)))
    (with-open-file (stream (fs:file-in-package "spikes.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun sun-core (size granularity-background &key (granularity-strain
                                                   (floor (/ granularity-background 5)))
                                               (color-seam #xffff00ff) (color-lake #xffe09bff)
                                               (color-circular-border-inner #xffffffaa)
                                               (color-circular-border-outer #x00000000))
  (with-random-perlin-gradient-offset
    (let* ((displacement-x (lcg-next-upto size))
           (displacement-y (lcg-next-upto size))
           (worley (2d-worley-lut (truncate (/ size 8)) granularity-background displacement-x displacement-y
                                  8 4 20
                                  #'(lambda (a) (d- (elt a 1) (elt a 0)))))
           (worley-2 (2d-worley-lut (truncate (/ size 8)) granularity-strain 0 0 6 4 10
                                    #'(lambda (a) (d- (elt a 1) (elt a 0)))))
           (frame (fuzzy-circular-frame (/ +default-size-pixmap-library+ 4)
                                        0.15 0.21 color-circular-border-inner
                                        color-circular-border-outer 0.02))
           (spikes (sun-spikes 128 :spikes (loop repeat (lcg-next-in-range 2 6) collect
                                                (lcg-next-upto 360.0))))
           (pixmap (pixmap:make-pixmap-frame size size 4)))
      (setf (data spikes) (data (pgaussian-blur-separated spikes #'floor 5)))
      (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
        (draw-normalized-coord pixmap
                               (d/ (desired x) (desired size))
                               (d/ (desired y) (desired size))
                               #'(lambda (x y)
                                   (let* ((noise-bg (d* 1.8 (gen-abs-fbm x y 6 1.0 2.0
                                                                         0.65 2.0
                                                                         :normalize nil)))

                                          (noise-strain (d* 3.0
                                                            (gen-abs-fbm (d/ x 2.0)
                                                                         (d/ y 2.0)
                                                                         6 1.0 2.0 0.5 2.0
                                                                         :normalize nil)))
                                          (background (sample@ worley
                                                               (d+ noise-bg x)
                                                               (d+ noise-bg y)
                                                               :clamp nil))
                                          (strain (sample@ worley-2
                                                           (d+ noise-strain x)
                                                           (d+ noise-strain y)
                                                           :clamp nil))
                                          (spike (map 'vec4 #'byte->float
                                                      (sample@ spikes x y))))
                                     (let ((color (mix-color
                                                   (mix-color color-seam color-lake  background)
                                                   #xffffffff strain)))
                                       (setf (elt color 3)
                                             (byte->float (elt (sample@ frame x y) 3)))
                                       (mix-color spike color
                                                  (max (elt color 3)
                                                       (elt spike 3))))))))

      (values pixmap "sun-core"))))

(defun sun-crown (size &key (inner-color #xffffffff) (outer-color #xffff0000))
  (with-random-perlin-gradient-offset
    (let* ((displ (lcg-next-in-range -0.01 0.01))
           (pixmap (with-draw-normalizated-coord-square (x y size pixmap 4
                                                   :bindings (*perlin-gradient-random-offset*))
                     (let* ((afbm (d* 0.2 (gen-abs-fbm x y 6 2.0 1.0 0.65 2.0 :normalize nil)))
                            (p (2d-utils:2d-vector-translate
                                (vector x y)
                                (d+ displ -0.5)
                                (d+ displ -0.5)))
                            (magn-p (d+ afbm (2d-utils:2d-vector-magn p)))
                            (max (d* (desired size) (desired size)))
                            (weight (smoothstep-interpolate 0.0 0.6
                                                            (alexandria:clamp magn-p 0.0 max))))
                       (mix-color inner-color outer-color weight)))))
      (values pixmap "sun-crown"))))

(defun test-scrown ()
  (let ((pixmap (sun-crown 256 :inner-color #xffffffff :outer-color #xffff4400)))
    (with-open-file (stream (fs:file-in-package "scrown.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun smoke-tray (size &key (inner-color §cffffffff) (outer-color §cffffff00)
                          (direction -90.0)
                          (evanescence 1.0)
                          (noise-fn #'(lambda (x y)
                                        (* 0.6 (gen-abs-fbm x y 8 1.0 1.0 0.5 2.0 :normalize nil))))
                          (amplitude .03)
                          (density .99))
  (let* ((*perlin-gradient-random-offset* (lcg-next))
         (direction-vector (let ((radian (deg->rad direction)))
                             (vector (dcos radian) (dsin radian))))
         (frame (fuzzy-frame 128 0.2 0.2 10 :inner-color #x00000000 :outer-color #xffffffff))
         (pixmap (with-draw-normalizated-coord-square
                     (x y size pixmap 4 :bindings (*perlin-gradient-random-offset*))
                   (let* ((*default-epsilon* amplitude)
                          (afbm (funcall noise-fn x y))
                          (p (vector (d+ (d+ x afbm) -0.5) (d+ (d+ y afbm) -1.0)))
                          (magn-p (2d-utils:2d-vector-magn p))
                          (normalized-p (if (let ((*default-epsilon* 1e-7))
                                              (epsilon= 0.0 magn-p))
                                            direction-vector
                                            (2d-vector-normalize p)))
                          (dot-product (2d-vector-dot-product normalized-p direction-vector)))
                     (multiply-color
                      (byte-vector->vec4 (sample@ frame x y))
                      (if (epsilon= 1.0 dot-product)
                          (vec4 (elt inner-color 0)
                                (elt inner-color 1)
                                (elt inner-color 2)
                                (smoothstep-interpolate 0.0 1.0
                                                        (d*
                                                         (d- 1.0 (alexandria:clamp
                                                                  (d* evanescence magn-p)
                                                                  0.0 1.0))
                                                         (d* density (expt dot-product (d/ 1.0 amplitude))))))
                          outer-color))))))
    (setf (data pixmap) (data (pgaussian-blur-separated pixmap #'floor 5)))
    (values pixmap "smoke-tray")))

(defun find-smoke-origin (pixmap)
  (let ((max -1)
        (origin nil))
    (nmap-matrix-xy pixmap #'(lambda (c r el) (when (>= (elt el 3) max)
                                               (setf max (elt el 3)
                                                     origin (vector c r)))))
    origin))

(defun sun (&key
              (size 256) (color-seam #xffff00ff) (color-lake #xffe09bff)
              (inner-color #xffffffff) (outer-color #xffff0000)
              (color-circular-border-inner #xffffffaa)
              (color-circular-border-outer #x00000000)
              (mix-core-crown-fn #'(lambda (c cr) (declare (ignore c cr)) 0.5)))
  (let* ((core (sun-core size 100
                         :color-seam color-seam :color-lake color-lake
                         :color-circular-border-inner color-circular-border-inner
                         :color-circular-border-outer color-circular-border-outer))
         (crown (sun-crown size :inner-color inner-color :outer-color outer-color))
         (pixmap (with-draw-normalizated-coord-square (x y 512 pixmap 4)
                   (let ((weight (funcall mix-core-crown-fn
                                          (map 'vec4 #'byte->float
                                               (sample@ core x y))
                                          (map 'vec4 #'byte->float
                                               (sample@ crown x y)))))
                     (mix-color
                      (map 'vec4 #'byte->float (sample@ core x y))
                      (map 'vec4 #'byte->float (sample@ crown x y))
                      weight)))))

    (values pixmap "sun")))

(defun sun-daylight (&optional (size 256))
  (values
   (sun :size size
        :color-seam #xffffffff :color-lake #xffffffff
        :inner-color #xffffffff :outer-color #xffffdd00
        :color-circular-border-inner #xffffffff
        :color-circular-border-outer #x00000000)
   "sun-daylight"))

(defun sun-sunset (&optional (size 256))
  (values
   (sun :size size
        :color-seam #xffff00dd :color-lake #xffef0011
        :inner-color #xffff00ff :outer-color #xe7c96d00
        :color-circular-border-inner #xffffffff
        :color-circular-border-outer #x00000000)
                                        ;:mix-core-crown-fn #'(lambda (c cr) (declare (ignore cr)) (elt c 3)))
   "sun-sunset"))

(defun stone-floor-fancy (&optional (size 256))
  (with-random-perlin-gradient-offset
    (let ((noise:*max-dist-to-store* 3))
      (let* ((worley (2d-worley-lut-seamless 128 (lcg-next-in-range 11 31)
                                             (lcg-next-in-range 0 128)
                                             (lcg-next-in-range 0 128)
                                             20
                                             (num:lcg-next-in-range 8 10)
                                             (num:lcg-next-in-range 20 40)
                                             #'(lambda (a) (d- (elt a 1) (elt a 0)))))
             (pixmap (make-pixmap-frame size size 4)))
        (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
          (draw-normalized-coord pixmap
                                 (d/ (desired x) (desired size))
                                 (d/ (desired y) (desired size))
                                 #'(lambda (x y)
                                     (let* ((sample (sample@ worley x y :clamp t :interpolation nil))
                                            (weight (smoothstep-interpolate 0.1 0.2 sample))
                                            (noise (gen-fbm-seamless (d* 2.0 (desired x))
                                                               (d* 2.0 (desired y))
                                                              3 0.5 1.0 0.5 2.0
                                                              :normalize t :range-0->1 t
                                                              :x1 0.0 :x2 15.0 :y1 0.0
                                                              :y2 15.0))
                                            (color (mix-color
                                                    (mix-color #xffffffee
                                                               #xaaaaaaff weight)
                                                    (vec4 noise noise noise 1.0)
                                                    (smoothstep-interpolate 0.0 1.0 sample))))
                                       color))))
        (values pixmap "stone-floor-fancy")))))

(defun ruined-stone-floor-road ()
  (with-random-perlin-gradient-offset
    (let* ((size 256)
           (worley (2d-worley-lut 128 20 (lcg-next-upto (* size 4)) (lcg-next-upto (* size 4))
                                  6 4 30 #'(lambda (a) (d- (elt a 1) (elt a 0)))))
           (frame (fuzzy-frame 256 0.1 1.0 7 :inner-color #xaaaaaa11 :outer-color #xaaaaaaff))
           (gaps (with-noise-rgba-texture (pixmap pixel noise :width 256 :height 256
                                                  :r 255 :g 255 :b 255)
                   (setf noise (perlin-2d (d/ (desired x) 24.0) (d/ (desired y) 24.0)))))
           (pixmap (make-pixmap-frame size size 4)))
      (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
        (draw-normalized-coord pixmap
                               (d/ (desired x) (desired size))
                               (d/ (desired y) (desired size))
                               #'(lambda (x y)

                                   (let* ((sample (sample@ worley x y :clamp t :interpolation t))
                                          (weight (smoothstep-interpolate 0.1 0.2 sample))
                                          (noise (alexandria:clamp (dabs
                                                                    (gen-fbm x y 6
                                                                             10.0 1.0 0.65
                                                                             2.0
                                                                             :normalize nil))
                                                                   0.0 1.0))
                                          (color (mix-color #xaaaaaa21 #x000000af weight))
                                          (color-noise (mix-color color #xaaaaaaff
                                                                  noise))
                                          (frame (map 'vec4 #'byte->float
                                                      (sample@ frame x y)))
                                          (gaps (map 'vec4 #'byte->float
                                                     (sample@ gaps x y))))
                                     ;;(setf color-noise (mix-color color-noise frame (elt frame 3)))
                                     (setf (elt color-noise 3) (min (elt frame 3)
                                                                    (elt color-noise 3)))
                                     (setf (elt color-noise 3) (min (elt gaps 3)
                                                                    (elt color-noise 3)))
                                     color-noise))))
      (values pixmap "ruined-stone-floor-road"))))

(defun stone-floor-road-brick-lut (size xs ys
                                        &key
                                          (roughness .1)
                                          (gradient
                                           (make-instance 'gradient
                                                          :colors
                                                          (list
                                                           (make-gradient-color 1.0 §c00000000)
                                                           (make-gradient-color 0.95 §c000000ff)
                                                           (make-gradient-color 0.7 §cb2b1acff)
                                                           (make-gradient-color 0.0 §cb2b1acff)))))
  (with-random-perlin-gradient-offset
    (let* ((pixmap (make-pixmap-frame size size 4))
           (push nil)
           (raw-count 0))
      (loop for y from 0 below (height pixmap) do
           (loop for x from 0 below (width pixmap) do
                (draw-normalized-coord pixmap
                                       (d/ (desired x) (desired size))
                                       (d/ (desired y) (desired size))
                                       (lambda (x y)
                                         (let* ((offset-align (/ ys (* 2 size)))
                                                (offset (+ offset-align
                                                           (if push
                                                               (/ 1 (* 2 (/ size xs)))
                                                               0)))
                                                (rel-x (mod (truncate (* (+ x offset) size)) xs))
                                                (rel-y (mod (truncate (* (+ y offset-align) size)) ys))
                                                (slope-x (/ rel-x (truncate (/ xs 2))))
                                                (slope-y (/ rel-y (truncate (/ ys 2))))
                                                (col-x (if (< rel-x (truncate (/ xs 2)))
                                                           slope-x
                                                           (abs (1- (fract slope-x)))))
                                                (col-y (if (< rel-y (truncate (/ ys 2)))
                                                           slope-y
                                                           (abs (1- (fract slope-y)))))
                                                (col (+ (* roughness
                                                           (gen-abs-fbm (d* x 1.1)
                                                                        (d* y 1.1)
                                                                        8 15.5 5.0
                                                                        0.5 2.0))
                                                        (max col-x col-y))))
                                           (incf raw-count)
                                           (let ((noise (gen-fbm
                                                         (d* x 100.0)
                                                         (d* y 150.0)
                                                         8 .5 1.0
                                                         0.5 2.0 :normalize t :range-0->1 t)))
                                             (multiply-color
                                              (vec4 noise noise noise 1.0)
                                              (pick-color gradient
                                                          (alexandria:clamp
                                                           (desired col)
                                                           0.0 1.0))))))))
           (when (= 0 (mod y ys))
             (setf push (not push))))
      (values pixmap "stone-floor-road-brick-lut"))))

(defun stone-floor-road-base-lut (size)
  (with-random-perlin-gradient-offset
    (let* ((frame (fuzzy-frame size 0.09 0.05 20 :outer-color #x000000ff
                               :inner-color #x00000000))
           (gradient
            (make-instance 'gradient
                           :colors
                           (list
                            (make-gradient-color 1.0 §cffffffff)
                            (make-gradient-color 0.7 §c515151ff)
                            (make-gradient-color 0.0 §c000000ff)))))
      (values
       (with-draw-normalizated-coord-square (x y size pixmap 4
                                       :bindings (*perlin-gradient-random-offset*))
         (let ((color (pick-color gradient (gen-abs-fbm x y 6 5.5 1.0 0.5 2.0))))
           (setf (elt color 3) (byte->float (elt (sample@ frame x y) 3)))
           color))
       "stone-floor-road-base-lut"))))

(defun brick-wall (size &optional (xs 64) (ys 32))
  (let ((bricks (stone-floor-road-brick-lut size xs ys
                                                 :gradient
                                                 (make-instance 'gradient
                                                                :colors
                                                                (list
                                                                 (make-gradient-color 1.0 §cffefdcff)
                                                                 (make-gradient-color 0.96 §c0000000ff)
                                                                 (make-gradient-color 0.80 §c961a1aff)
                                                                 (make-gradient-color 0.0 §c961a1aff))))))
    (values bricks "bricks-wall")))

(defun test-brick-wall (&optional (size 256))
  (let ((pixmap (brick-wall size 64 32)))
    (with-open-file (stream (fs:file-in-package "bricks-wall.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun stone-floor-road (size xs ys)
  (let ((background (grass size))
        (bricks (stone-floor-road-brick-lut size xs ys)))
    (values
     (with-draw-normalizated-coord-square (x y size pixmap 4)
           (mix-color (map 'vec4 #'byte->float (sample@ background x y))
                      (map 'vec4 #'byte->float (sample@ bricks x y))
                      (byte->float (elt (sample@ bricks x y) 3))))
     "stone-floor-road")))

(defun octagonal-floor (xs ys)
  (let ((size 128))
    (values
     (with-draw-normalizated-coord-square (x y size pixmap 4)
       (let* ((slope-x (/ (mod (truncate (* x size)) xs) (truncate (/ xs 2))))
              (slope-y (/ (mod (truncate (* y size)) ys) (truncate (/ ys 2))))
              (col-x (alexandria:clamp
                      (if (< (mod (truncate (* x size)) xs) (truncate (/ xs 2)))
                          slope-x
                          (abs (1- (fract slope-x))))
                      0.0 0.8))
              (col-y (alexandria:clamp
                      (if (< (mod (truncate (* y size)) ys) (truncate (/ ys 2)))
                          slope-y
                          (abs (1- (fract slope-y))))
                      0.0 0.8))
              (col (+ col-x col-y)))
         (mix-color #xffffffff #x000000ff (smoothstep-interpolate 0.8 1.0
                                                                  (coerce col 'single-float)))))
     "octagonal-floor")))

(defun test-stone-floor-road ()
  (let ((pixmap (stone-floor-road 128 32 32)))
    (with-open-file (stream (fs:file-in-package "stone-floor-road.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun glass-tile (size &key (gradient (make-gradient
                                        (make-gradient-color 1.0  §cff000000)
                                        (make-gradient-color 0.26 §cffff00ff)
                                        (make-gradient-color 0.25  §caaaa00ff)
                                        (make-gradient-color 0.20  §cdddd00ff)
                                        (make-gradient-color 0.10  §cffff00ff)
                                        (make-gradient-color 0.0  §cffffffff))))
  (values
   (let* ((pixmap (make-pixmap-frame size size 4))
          (fun-t
           #'(lambda (x y)
               (+
                (* 10000 (atan (* .09 (- y (* 0.2 size)))))
                (* 1.5 (* 4.5 (expt (- x (* 0.5 size)) 2)))
                (- (* 6000 (atan (* .2 (- y (* 0.95 size)))))))))
          (max -1.e30)
          (min 1e30))
     (loop-matrix (pixmap x y)
        (let ((val (funcall fun-t (desired x) (desired y))))
          (when (> (desired val) max)
            (setf max val))
          (when (< (desired val) min)
            (setf min val))))
     (ploop-matrix (pixmap x y)
        (setf (pixel@ pixmap x y)
              (map 'ubvec4 #'float->byte
                   (let* ((bg
                           (mix-color
                            (vec4 0.0 0.0 0.0 1.0)
                            (pick-color gradient
                                        (smoothstep-interpolate 0.0 0.5
                                                                (/ (+ (funcall fun-t x y) (- min))
                                                                   (+ max (- min)))))
                            (if (d< (elt (pick-color gradient
                                                     (smoothstep-interpolate 0.0 0.5
                                                                             (/ (+ (funcall fun-t x y) (- min))
                                                                                (+ max (- min)))))
                                         3) 0.8)
                                (- 1.0 (smoothstep-interpolate 0.0 1.0
                                                               (elt (pick-color gradient
                                                                                (smoothstep-interpolate
                                                                                 0.0 0.5
                                                                                 (/ (+ (funcall fun-t x y)
                                                                                       (- min))
                                                                                    (+ max (- min)))))
                                                                    3)))
                                (elt (pick-color gradient
                                                 (1- (smoothstep-interpolate 0.0 1.5
                                                                             (/ (+ (funcall fun-t x y)
                                                                                   (- min))
                                                                                (+ max (- min))))))
                                     3))))
                          (c (mix-color bg (vec4 0.0 0.0 0.0 1.0) (d- 1.0 (elt bg 3)))))
                     (vec4 (elt c 0) (elt c 1) (elt c 2) 1.0)))))
     pixmap)
   "glass-tile"))

(defun test-glass-tile (&optional (size 256))
  (let ((pixmap (glass-tile size)))
    (with-open-file (stream (fs:file-in-package "glass-tile.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defmacro with-standard-fbm (&body body)
  `(labels ((noise (x y range-01)
              (gen-fbm x y 6 1.0 1.0 0.5 2.0
                       :normalize t :range-0->1 range-01)))
     ,@body))

(defmacro with-standard-afbm (&body body)
  `(labels ((noise (x y range-01)
              (gen-afbm x y 6 1.0 1.0 0.5 2.0
                       :range-0->1 range-01)))
     ,@body))

(defun starfish (size)
  (values
   (let* ((pixmap (make-pixmap-frame size size 4))
          (fun-t
           #'(lambda (x y)
               (let ((x (d- x 0.5))
                     (y (d- y 0.5)))
                 (let* ((magn (desired (2d-vector-magn (vector x y))))
                        (alpha (datan (desired y) (desired x)))
                        (density (d+
                                  (d* 0.05 (dexpt (d+ 1.0 (dcos
                                                           (d* 20.0 alpha)))
                                                  1.9))
                                  (d* 0.3 (dsin (d* 5.0 alpha)))
                                  (bidimensional-gaussian x y 0.04 0.04)))
                        (color (if (d< magn density)
                                   (mix-color §c3b0d06ff
                                               (mix-color §cb32512ff §c3b0d06ff
                                                           (smoothstep-interpolate
                                                            0.0 10.0
                                                            (d+
                                                             (d* 10.0 (dcos (* 1.1 (dsin (* 5.0 alpha)))))
                                                             (d* 2.0 (dsin (d* .3 density))))))
                                               (smoothstep-interpolate 0.0 0.05
                                                                       (dabs (d- magn density))))
                                   (mix-color §c3b0d06ff §c00000000
                                               (smoothstep-interpolate 0.0 0.01
                                                                       (dabs (d- magn density)))))))
                   (vec4 (elt color 0) (elt color 1) (elt color 2) 1.0))))))
     (ploop-matrix (pixmap x y)
       (let* ((nx (d/ (desired x) (desired size)))
              (ny (d/ (desired y) (desired size))))
         (setf (pixel@ pixmap x y)
               (map 'ubvec4 #'float->byte
                    (funcall fun-t nx ny)))))
     pixmap)))

(defun test-starfish (size)
  (let ((pixmap (starfish size)))
    (with-open-file (stream (fs:file-in-package "starfish.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))


(defun hourglass-cheap (size)
  (with-random-perlin-gradient-offset
    (let ((pixmap
           (with-draw-normalizated-coord-square (x y size pixmap 4 :bindings
                                                   (*perlin-gradient-random-offset*))
             (let* ((x (d- (desired x) 0.5))
                    (y (d- (desired y) 0.5))
                    (alpha (datan y x))
                    (density (d* 1.3 (dcos (d* 2.0 alpha))
                                     (bidimensional-gaussian x y 0.04 0.04)))
                    (color (mix-color §c6ec0ff00 §cffffffff
                                      (smoothstep-interpolate 0.0 0.1 density))))
               color))))
      pixmap)))

(defun sun-cheap (size)
    (let* ((frame (fuzzy-circular-frame (f/ size 4) 0.4 0.5  §cffffffff §cffffff00 0.02))
           (pixmap (with-draw-normalizated-coord-square (x y size pixmap 4 :bindings
                                                           (*perlin-gradient-random-offset*))
                     (let* ((act-x (d- (desired x) 0.5))
                            (act-y (d- (desired y) 0.5))
                            (alpha (datan act-y act-x ))
                            (v    (vec2 act-x  act-y))
                            (magn (vec2-length v))
                            (density (d+
                                      (d* 2.0
                                          (dexpt
                                           (dcos
                                            (d* (d* 2.0 2.0) alpha))
                                           2.0))
                                      (d* (dcos (d* 0.1 alpha))
                                          (bidimensional-gaussian act-x  act-y 0.04 0.04))))
                            (color (mix-color
                                    §cffffffff
                                    (mix-color §cffffffff   §c6ec0ff00
                                               (smoothstep-interpolate 0.0 1.0
                                                                       (d+ magn density)))
                                    (smoothstep-interpolate 0.0 0.1
                                                            (d* magn magn)))))
                       (multiply-color color (byte-vector->vec4 (sample@ frame x y)))))))
      pixmap))

(defun test-sun-cheap (size)
  (let ((pixmap (sun-cheap size)))
    (with-open-file (stream (fs:file-in-package "sun-cheap.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun blood-splat (size)
  (with-random-perlin-gradient-offset
    (labels ((noise (x y range-01)
               (gen-fbm x y 6 1.0 1.0 0.5 2.0
                        :normalize t :range-0->1 range-01)))
      (values
       (let* ((pixmap (make-pixmap-frame size size 4))
              (branch (lcg-next-in-range 5.0 10.0))
              (fun-t
               #'(lambda (x y)
                   (let ((x (d- x 0.5))
                         (y (d- y 0.5)))
                     (let* ((magn (2d-vector-magn (vector x y)))
                            (alpha (atan y x)))
                       (smoothstep-interpolate 0.2 0.0
                                               (d- magn
                                                   (d*
                                                    (noise (d* 10.0 x ) (d* 10.0 y) t)
                                                    (d+
                                                     (d* 0.4
                                                         (dexpt
                                                          (dcos
                                                           (d* (d* 2.0 branch) alpha))
                                                          2.0))
                                                     (d* 0.3 (dcos (d* branch alpha)))
                                                     (bidimensional-gaussian x y 0.04 0.04))))))))))
         (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
           (let* ((nx (d/ (desired x) (desired size)))
                  (ny (d/ (desired y) (desired size)))
                  (noise (noise (d* 12.0 nx) (d* 12.0 ny) t)))
             (setf (pixel@ pixmap x y)
                   (map 'ubvec4 #'(lambda (a) (float->byte (d* (funcall fun-t nx ny) a)))
                        (mix-color  §c000000ff §cff0000bb
                                    noise)))))
         pixmap)
       "blood-splat"))))

(defun test-blood-splat (&optional (size +default-size-pixmap-library+))
  (let ((pixmap (blood-splat size)))
    (with-open-file (stream (fs:file-in-package "blood-splat.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun blood-particle (size &key (gradient
                                  (make-gradient
                                   (make-gradient-color 0.0 #(0.58 0.0 0.0 .5))
                                   (make-gradient-color 0.2 #(0.50 0.0 0.0 .5)))))
  (with-random-perlin-gradient-offset
    (labels ((noise (x y range-01)
               (gen-fbm x y 6 1.0 1.0 0.5 2.0
                        :normalize t :range-0->1 range-01)))
      (let* ((pixmap (with-standard-generated-pixmap-square (pixmap size)
                       (let* ((dist (d+ (noise x y t)
                                        (vec2-length (vec2- (vec2 x y) (vec2 0.5 0.5)))))
                              (color (pick-color gradient
                                                 (dlerp (smoothstep-interpolate 0.0 1.0 dist)
                                                        0.0 1.0))))
                         color))))
        (values pixmap "blood-particle")))))

(defun test-blood-particle (&optional (size +default-size-pixmap-library+))
  (let ((pixmap (blood-particle size)))
    (with-open-file (stream (fs:file-in-package "blood-particle.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun fire-particle (size &key (color §cbd1312ff))
  (with-random-perlin-gradient-offset
    (labels ((noise (x y range-01)
               (gen-fbm x y 6 1.0 1.0 0.5 2.0
                        :normalize t :range-0->1 range-01)))
      (let* ((pixmap (with-standard-generated-pixmap-square (pixmap size)
                       (let* ((x-noise (d+ x (d* 0.5 (noise (d* x 8.1) (d* y 8.1) t))))
                              (y-noise (d+ y (d* 0.5 (noise (d* x 8.1) (d* y 8.1) t))))
                              (dist    (vec2-length (vec2- (vec2 x-noise y-noise)
                                                           (vec2 0.5 0.5))))
                              (dist-frame (vec2-length (vec2- (vec2 x y)
                                                              (vec2 0.5 0.5))))
                              (color-bg   (d- 1.0 (dlerp (smoothstep-interpolate 0.0 1.0 dist)
                                                      0.0 1.0)))
                              (color-frame (d- 1.0 (dlerp (smoothstep-interpolate 0.0
                                                                                  0.5
                                                                                  dist-frame)
                                                          0.0 1.0))))
                         (multiply-color (multiply-color color
                                                         (vec4 color-bg color-bg color-bg 1.0))
                                         (vec4 color-frame color-frame color-frame 1.0))))))
        (values pixmap "fire-particle")))))

(defun test-fire-particle (&optional (size +default-size-pixmap-library+))
  (let ((pixmap (fire-particle size)))
    (with-open-file (stream (fs:file-in-package "fire-particle.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun smoke-particle (size &key (color §c050505ff))
  (with-random-perlin-gradient-offset
    (labels ((noise (x y range-01)
               (gen-fbm x y 6 1.0 1.0 0.5 2.0
                        :normalize t :range-0->1 range-01)))
      (let* ((pixmap (with-standard-generated-pixmap-square (pixmap size)
                       (let* ((x-noise (d+ x (d* 0.5 (noise (d* x 8.1) (d* y 8.1) t))))
                              (y-noise (d+ y (d* 0.5 (noise (d* x 8.1) (d* y 8.1) t))))
                              (dist    (vec2-length (vec2- (vec2 x-noise y-noise)
                                                           (vec2 0.5 0.5))))
                              (dist-frame (vec2-length (vec2- (vec2 x y)
                                                              (vec2 0.5 0.5))))
                              (alpha-bg   (d- 1.0 (dlerp (smoothstep-interpolate 0.0 1.0 dist)
                                                      0.0 1.0)))
                              (alpha-frame (d- 1.0 (dlerp (smoothstep-interpolate 0.0
                                                                                  0.5
                                                                                  dist-frame)
                                                          0.0 1.0))))
                         (multiply-color (multiply-color color
                                                         (vec4 1.0 1.0 1.0 alpha-bg))
                                         (vec4 1.0 1.0 1.0 alpha-frame))))))
        (values pixmap "fire-particle")))))

(defun test-smoke-particle (&optional (size +default-size-pixmap-library+))
  (let ((pixmap (smoke-particle size)))
    (with-open-file (stream (fs:file-in-package "smoke-particle.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun aerial-explosion-particle (size &key (color §cffffffff))
  (smoke-particle size :color color))

(defun test-aerial-explosion-particle (&optional (size +default-size-pixmap-library+))
  (let ((pixmap (aerial-explosion-particle size)))
    (with-open-file (stream (fs:file-in-package "aerial-explosion.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun moon-background (&optional (size +default-size-pixmap-library+))
  (with-random-perlin-gradient-offset
    (let* ((worley (2d-worley-lut 256 100 (lcg-next-upto size) (lcg-next-upto size)
                                  4 2 5 #'(lambda (a) (d- (elt a 1) (elt a 0)))))
           (pixmap (make-pixmap-frame size size 4)))
      (ploop-matrix (pixmap x y :bindings (*perlin-gradient-random-offset*))
        (draw-normalized-coord pixmap
                               (d/ (desired x) (desired size))
                               (d/ (desired y) (desired size))
                               #'(lambda (x y)
                                   (let* ((noise (smoothstep-interpolate
                                                  0.0 1.0
                                                  (gen-fbm x y 6 1.0 2.0 0.5 2.0
                                                           :normalize nil)))
                                          (noise-lake (d* 0.17
                                                          (gen-fbm x y 6 2.0 1.0 0.5 2.0
                                                                   :normalize nil)))
                                          (sample (sample@ worley
                                                           (d+ noise-lake x)
                                                           (d+ noise-lake y) :clamp nil))
                                          (weight (smoothstep-interpolate 0.0 1.0 sample)))
                                     (mix-color
                                      (mix-color #xaaaaaaff #x222222ff weight)
                                      #x222222ff noise)))))
      (values pixmap "moon-backround"))))

(defun moon (&optional (size +default-size-pixmap-library+))
  (let* ((background (moon-background size))
         (circular-frame (fuzzy-circular-frame (/ size 4)
                                               0.4 0.45 #x00000000 #x000000ff 0.0))
         (pixmap (make-pixmap-frame size size 4)))
    (ploop-matrix (pixmap x y)
      (draw-normalized-coord pixmap
                             (d/ (desired x) (desired size))
                             (d/ (desired y) (desired size))
                             #'(lambda (x y)
                                 (let ((bg (map 'vec4 #'byte->float
                                                (sample@ background x y)))
                                       (frame (d- 1.0 (byte->float
                                                       (elt (sample@ circular-frame x y) 3)))))
                                   (setf (elt bg 3) frame)
                                   bg))))
    (values pixmap "moon")))

(defun half-moon (&optional (size +default-size-pixmap-library+) (shadow-entity 0.2))
  (let* ((moon (moon-background size))
         (circular-frame (fuzzy-circular-frame (/ size 4)
                                               0.4 0.45 #x00000000 #x000000ff 0.0))
         (pixmap (with-draw-normalizated-coord-square (x y 256 pixmap 4)
                   (let ((weight (smoothstep-interpolate 0.0 1.0 (d- x shadow-entity)))
                         (frame (d- 1.0 (byte->float
                                         (elt (sample@ circular-frame x y) 3))))
                         (old-color (map 'vec4 #'byte->float
                                         (sample@ moon x y))))
                     (setf (elt old-color 3) (min weight frame))
                     old-color))))
    pixmap))

(defun crescent-moon (&optional (size (/ +default-size-pixmap-library+ 4)))
  (let* ((frame (fuzzy-circular-frame (/ size 4)
                                      0.4 0.5 #x000000ff #x00000022 0.0))
         (moon (moon))
         (pixmap (make-pixmap-frame size size 4)))
    (ploop-matrix (pixmap x y)
      (draw-normalized-coord pixmap
                             (d/ (desired x) (desired size))
                             (d/ (desired y) (desired size))
                             #'(lambda (x y)
                                 (let ((bg (map 'vec4 #'byte->float
                                                (sample@ moon x y)))
                                       (frame (map 'vec4 #'byte->float
                                                   (sample@ frame (d+ x 0.2) y :clamp t))))
                                   (when (d> (elt bg 3) 0.9)
                                     (setf (elt bg 3) (d- 1.0 (elt frame 3))))

                                   (when (d< x 0.4)
                                      (setf (elt bg 3) 0.0))
                                    bg))))
    (values pixmap "crescent-moon")))

(defun grass (size &key
                     (gradient (make-gradient
                                (make-gradient-color 0.0 §c073502ff)
                                (make-gradient-color 0.1  §c328341ff)
                                (make-gradient-color 0.4 §c32aa41ff)
                                (make-gradient-color 1.0 §cafffffff)))
                     (gradient-2 (make-gradient
                                  (make-gradient-color 0.0 §c073502ff)
                                  (make-gradient-color 1.0 §c328341ff))))
  (with-random-perlin-gradient-offset
    (let* ((pixmap (tileize
                    (with-draw-normalizated-coord-square (x y size pixmap 4
                                                    :bindings (*perlin-gradient-random-offset*))
                      (let ((noise1 (range-0to1
                                     (perlin-2d (d* (desired x) 100.0)
                                                (d* (desired y) 100.0))))
                            (noise2 (range-0to1
                                     (perlin-2d (d* (desired x) 6.0)
                                                (d* (desired y) 6.0))))
                            (noise3 (gen-fbm (d* (desired x) 6.0)
                                             (d* (desired y) 6.0)
                                             6 1.0 1.0 0.5 3.0 :normalize t :range-0->1 t)))
                        (let ((color (mix-color
                                      (mix-color (pick-color gradient noise1)
                                                 (pick-color gradient-2 noise2)
                                                 0.5)
                                      (pick-color gradient-2 noise3)
                                      0.8)))
                          (setf (elt color 3)
                                (smoothstep-interpolate 0.5 1.0
                                                        (elt (pick-color gradient noise2) 3)))
                          color))))))
      (values pixmap "grass"))))

(defun snow (size)
  (make-pixmap size size 4 (ubvec4:ubvec4 255 255 255 255)))

(defun dry-soil (size)
  (with-random-perlin-gradient-offset
    (let* ((gradient (make-gradient
                      (make-gradient-color 0.0  §c000000ff)
                      (make-gradient-color 0.01 §c7d6d4cff)))
           (lut (2d-worley-lut (floor (/ size 2)) (floor (/ size 7)) 0 0 1 3 32
                               #'(lambda (a) (d- (elt a 1) (elt a 0)))))
           (lut-noise-bg (let* ((pixmap (pixmap:make-pixmap-frame size size 4)))
                           (ploop-matrix (pixmap x y
                                                 :bindings (*perlin-gradient-random-offset*))
                             (setf (pixel@ pixmap x y)
                                   (let ((val (d* 0.1
                                                  (- 1.0
                                                     (gen-fbm (d/ (desired x) (/ size 4.0))
                                                              (d/ (desired y) (/ size 4.0))
                                                              4 4.0 2.0 0.5 2.0 :normalize t)))))
                                     (vec4 val val val 1.0))))
                           pixmap))
           (lut-noise (let* ((pixmap (pixmap:make-pixmap-frame size size 4)))
                        (ploop-matrix (pixmap x y
                                              :bindings (*perlin-gradient-random-offset*))
                          (setf (pixel@ pixmap x y)
                                (* 0.5 (gen-abs-fbm (d/ (desired x) (desired size))
                                                    (d/ (desired y) (desired size))
                                                    4 2.0 1.0 0.5 2.0 :normalize t))))
                        pixmap))
           (pixmap (with-draw-normalizated-coord-square (x y size pixmap 4
                                                   :bindings (*perlin-gradient-random-offset*))
                     (let ((worley-x (d+ x (d* 0.3 (sample@ lut-noise x y))))
                           (worley-y (d+ y (d* 0.3 (sample@ lut-noise x y)))))
                       (subtract-color (pick-color
                                        gradient
                                        (smoothstep-interpolate 0.0 0.05
                                                                (sample@ lut worley-x worley-y)))
                                       (vec4 (elt (sample@ lut-noise-bg x y) 0)
                                             (elt (sample@ lut-noise-bg x y) 1)
                                             (elt (sample@ lut-noise-bg x y) 2)
                                             0.0))))))

      (values pixmap "dry-soil"))))

(defun voronoized-starfish (size)
  (let* ((src (starfish size))
         (vor (voronoize src)))
    (values vor "voronoized-starfish")))

(defun voronoized-graal (size)
  (let* ((src (glass-tile size))
         (vor (voronoize src)))
    (values vor "voronoized-graal")))

(defun soil (size &key (gradient (make-instance 'gradient
                                                :colors
                                                (list
                                                 (make-gradient-color 0.0 §c000000ff)
                                                 (make-gradient-color 0.8 §c741a0eff)
                                                 (make-gradient-color 1.0 §c874006ff)))))
  (flet ((base (x y) (gen-abs-fbm x y 6 8.0 1.0 0.625 2.1836)))
    (let* ((bumps (2d-worley-lut 256 50 0 0 5 1 8 #'(lambda (a) (elt a 0))))
           (grains (2d-worley-lut 128 5 0 0 1 1 2 #'(lambda (a) (d- (elt a 1) (elt a 0)))))
           (pixmap (with-standard-generated-pixmap-square (pixmap size)
                     (let ((color (alexandria:clamp
                                   (d+
                                    (d* 0.3
                                        (d+
                                         (d* 1.5 (base x y))
                                         (d- 1.0 (d* 0.9 (sample@ bumps x y))))))
                                   0.0 1.0)))
                       (mix-color
                        (pick-color gradient color)
                        (pick-color gradient 0.9)
                        (smoothstep-interpolate 0.5 1.0
                                                (sample@ grains x y)))))))

      (values pixmap "soil"))))

(defun blurred-ring (size &key
                              (radius 0.15)
                              (thickness 0.4)
                              (gradient (make-instance 'gradient
                                                          :colors
                                                          (list
                                                           (make-gradient-color 0.0 §cffffff00)
                                                           (make-gradient-color 0.8 §cffffffff)))))
  (let* ((pixmap (with-standard-generated-pixmap-square (pixmap size)
                   (let* ((dist   (vec2-length (vec2- (vec2 x y) (vec2 0.5 0.5))))
                          (value1 (d- 1.0 (dlerp (smoothstep-interpolate (d+ 0.2 radius)
                                                                         (d+ 0.3 radius)
                                                                         dist)
                                                   0.0 1.0)))
                          (value2 (dlerp (smoothstep-interpolate 0.1 thickness dist)
                                         0.0 1.0))
                          (color  (pick-color gradient (d* value2 value1))))
                     color))))
    (values pixmap "blurred-ring")))

(defun test-blurred-ring (&optional (size +default-size-pixmap-library+))
  (let ((pixmap (blurred-ring size)))
    (with-open-file (stream (fs:file-in-package "blurred-ring.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun blurred-circle (size &key
                              (start-blur  .1)
                              (end-blur   0.45)
                              (gradient (make-instance 'gradient
                                                          :colors
                                                          (list
                                                           (make-gradient-color 0.0 §cffffffff)
                                                           (make-gradient-color 0.8 §cffffff00)))))
  (let* ((pixmap (with-standard-generated-pixmap-square (pixmap size)
                   (let* ((dist   (vec2-length (vec2- (vec2 x y) (vec2 0.5 0.5))))
                          (value  (dlerp (smoothstep-interpolate start-blur
                                                                 end-blur
                                                                 dist)
                                                   0.0 1.0))
                          (color  (pick-color gradient value)))
                     color))))
    (values pixmap "blurred-circle")))

(defun test-blurred-circle (&optional (size +default-size-pixmap-library+))
  (let ((pixmap (blurred-circle size)))
    (with-open-file (stream (fs:file-in-package "blurred-circle.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun blurred-cross (size &key
                             (scaling-x 0.11)
                             (scaling-y 0.11)
                             (gradient (make-instance 'gradient
                                                      :colors
                                                      (list
                                                       (make-gradient-color 0.0 §cffffff00)
                                                       (make-gradient-color 0.8 §cffffffff)))))
  (let* ((fn     #'(lambda (x y)
                     (d- 1.0 (d* (datan x)
                                 (datan y)
                                 (datan (d* x +pi+))
                                 (datan (d* y +pi+))))))
         (frame (fuzzy-circular-frame size
                                      0.35 0.5 §cffffffff §cffffff00 0.0))
         (pixmap (with-standard-generated-pixmap-square (pixmap size)
                   (let* ((value-fn (dlerp (smoothstep-interpolate 0.0 1.0
                                                                   (funcall fn
                                                                            (d/ (d- x 0.5)
                                                                                scaling-x)
                                                                            (d/ (d- y 0.5)
                                                                                scaling-y)))
                                           0.0 1.0))
                          (color  (multiply-color (byte-vector->vec4 (sample@ frame x y))
                                                  (pick-color gradient value-fn))))
                     color))))
    (values pixmap "blurred-cross")))

(defun test-blurred-cross (&optional (size +default-size-pixmap-library+))
  (let ((pixmap (blurred-cross size)))
    (with-open-file (stream (fs:file-in-package "blurred-cross.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun blurred-circled-cross (size)
  (let ((circle (blurred-ring size))
        (cross  (blurred-cross  size)))
    (blit circle cross 0 0 0 0 :function-blend (blit-blend-lerp-fn))
    cross))

(defun test-blurred-circled-cross (&optional (size +default-size-pixmap-library+))
  (let ((pixmap (blurred-circled-cross size)))
    (save-pixmap pixmap (fs:file-in-package "blurred-circled-cross.tga")))
  t)

(defun rock-1 (size &key (gradient-base (make-instance 'gradient
                                                :colors
                                                (list
                                                 (make-gradient-color 0.0 §c343944ff)
                                                 (make-gradient-color 0.3 §c5c6266ff)
                                                 (make-gradient-color 1.0 §cced1d2ff)))))
  (let* ((*max-dist-to-store* 2)
         (worley (2d-worley-lut 128 50 0 0 1 1 2 #'(lambda (a) (d- (elt a 1) (elt a 0)))))
         (pixmap (with-standard-generated-pixmap-square (pixmap size)
                   (let* ((base (alexandria:clamp
                                 (d+
                                  (d* 0.5 (sample@ worley x y))
                                  (d* 0.3 (gen-fbm (* x 2.0) (* y 2.0) 8 8.0 2.1 0.5 2.1
                                                   :normalize t))
                                  (gen-abs-fbm (* x .9) (* y .9) 6 2.0 0.1 0.2 2.1
                                               :normalize t))
                                 0.0 1.0))
                          (strain (rock-strain x y §c8d7e63ff §cffffffff)))
                     (multiply-color
                      strain
                      (pick-color gradient-base base))))))
    (values pixmap "rock-1")))

(defun rock-2 (size &key (gradient
                          (make-gradient
                           (make-gradient-color 1.0 §c8b6236ff)
                           (make-gradient-color 0.3 §cc09664ff)
                           (make-gradient-color 0.0 §cf3d599ff))))
  (with-standard-fbm
    (with-random-perlin-gradient-offset
      (let* ((pixmap (with-standard-generated-pixmap-square (pixmap size)
                       (let* ((layers (dsin (d* 100.0 (dsin (d* (noise x y t) 5.0 y)) y)))
                              (color (pick-color gradient layers)))

                          color))))
        (values pixmap "rock-2")))))

(defun wood-2 (size &optional
                      (gradient
                       (make-gradient
                        (make-gradient-color 1.0 §c8b6236ff)
                        (make-gradient-color 0.3 §cc09664ff)
                        (make-gradient-color 0.0 §cf3d599ff)))
                      (min-closeness-rings 50.0)
                      (max-closeness-rings 100.0)
                      (max-nodes-shift     2.0))
  (with-standard-fbm
    (with-random-perlin-gradient-offset
      (let* ((rings-closeness (lcg-next-in-range min-closeness-rings
                                                 max-closeness-rings))
             (nodes-shift     (lcg-next-upto max-nodes-shift))
             (pixmap (with-standard-generated-pixmap-square (pixmap size)
                       (let* ((y-dist (d+ (noise x y t) y))
                              (layers (smoothstep-interpolate 0.0 3.0
                                                              (dsin
                                                               (d* rings-closeness
                                                                   (dsin (d+ nodes-shift
                                                                             (d* 5.0 y)))
                                                                   y-dist))))
                              (color (pick-color gradient layers)))

                         color))))
        (setf (data pixmap) (data (matrix:rotate-matrix pixmap 90.0)))
        (values pixmap "wood-2")))))

(defun wood-3 (size &optional
                      (gradient
                       (make-gradient
                        (make-gradient-color 1.0 §c8b6236ff)
                        (make-gradient-color 0.3 §cc09664ff)
                        (make-gradient-color 0.0 §cf3d599ff)))
                      (min-closeness-rings 50.0)
                      (max-closeness-rings 100.0)
                      (max-nodes-shift     2.0))
  (with-standard-fbm
    (with-random-perlin-gradient-offset
      (let* ((rings-closeness (lcg-next-in-range min-closeness-rings
                                                 max-closeness-rings))
             (nodes-shift (lcg-next-upto max-nodes-shift))
             (pixmap
              (with-standard-generated-pixmap-square (pixmap size)
                (let* ((y-dist (d+ (noise x y t) y))
                       (layers      (smoothstep-interpolate 0.0 3.0
                                                       (dsin
                                                        (d* rings-closeness
                                                            (dsin (d+ nodes-shift (d* 5.0 y)))
                                                            y-dist))))
                       (bryophyte   (rock-strain x y #x212c11ff #x5f5f5fff))
                       (color (pick-color gradient layers)))
                  (multiply-color bryophyte color)))))
        (setf (data pixmap) (data (matrix:rotate-matrix pixmap 90.0)))
        (values pixmap "wood-3")))))

(defun wood-4 (size &key (gradient
                          (make-gradient
                           (make-gradient-color 1.0 §c583926ff)
                           (make-gradient-color 0.0 §c6F4832ff))))
  (labels ((noise (x y)
             (gen-abs-fbm x y 8 2.0 2.0 0.65 2.1 :normalize t)))
    (with-random-perlin-gradient-offset
      (let* ((pixmap
              (with-standard-generated-pixmap-square (pixmap size)
                (let* ((dist (d- 1.0 (d* 0.9 (noise (d* (d+ x (noise x y)) 9.0) (d* y 0.9)))))
                       (layers (d* dist (dsin (d* 50.0 (dsin (d* 10.0 x)) x))))
                       (color (pick-color gradient layers)))
                  color))))
        (values pixmap "wood-4")))))

(defun rock-layers (size &key (gradient
                                (make-gradient
                                 (make-gradient-color 0.8 §c583b1bff)
                                 (make-gradient-color 0.6 §c8d683eff)
                                 (make-gradient-color 0.4 §cc3b6a6ff)
                                 (make-gradient-color 0.0 §cdceeeeff)))
                            (gradient-erosion
                             (make-gradient
                              (make-gradient-color 1.0 §cffffffff)
                              (make-gradient-color 0.0 §c2a2a2aff))))
  (with-random-perlin-gradient-offset
    (labels ((noise (x y)
               (gen-fbm x y 8 2.0 13.0 0.5 2.0 :normalize t :range-0->1 t))
             (noise2 (x y)
               (gen-abs-fbm x y 8 1.0 1.0 0.5 2.0 :normalize t)))
      (let* ((base (with-standard-float-generated-pixmap-square (pixmap size)
                     (let* ((act-y (d+ y (d* 0.1 (noise2 (d* x 1.9) (d* y 1.9)))))
                            (act-y2 (d+ y (d* 0.2 (noise2 (d* x .9) (d* y .9)))))
                            (layers (dsin (d* (dsin (d* 9.0 act-y2)) ;; change freq
                                              (d/ (desired size) 15.0) act-y))) ;; master sin
                            (layers-color (pick-color gradient layers)))
                       layers-color)))
             (erosion (with-standard-float-generated-pixmap-square (pixmap size)
                       (pick-color gradient-erosion
                                   (d*
                                    (noise (d* x 9.9) (d* y .9))
                                    (noise (d* x .9) (d* y 9.9))))))
             (texture (with-standard-float-generated-pixmap-square (pixmap size)
                       (pick-color gradient (noise (d* x 9.9) (d* y 9.9)))))
             (pixmap (with-standard-generated-pixmap-square (pixmap size)
                       (multiply-color (sample@ erosion x y)
                                       (multiply-color (sample@ texture x y)
                                                       (sample@ base x y))))))
        (values pixmap "rock-layers")))))

(defun brown-velvet (size &key (gradient-base (make-instance 'gradient
                                                :colors
                                                (list
                                                 (make-gradient-color 0.0 §c343944ff)

                                                 (make-gradient-color 0.3 §c5c6266ff)
                                                 (make-gradient-color 1.0 §cced1d2ff)))))
  (let* ((worley (2d-worley-lut 128 50 0 0 1 1 2 #'(lambda (a) (d- (elt a 1) (elt a 0)))))
         (pixmap (with-standard-generated-pixmap-square (pixmap size)
                   (let* ((base (alexandria:clamp
                                 (d+
                                  (d* 0.5 (sample@ worley x y))
                                  (d* 0.2 (gen-fbm (* x 2.0) (* y 2.0) 8 8.0 2.1 0.5 2.1
                                                   :normalize t))
                                  (gen-abs-fbm (* x .9) (* y .9) 6 2.0 0.1 0.2 2.1
                                               :normalize t))
                                 0.0 1.0))
                          (strain (rock-strain x y §c8d7e63ff §c000000ff)))
                     (multiply-color
                      strain
                      (pick-color gradient-base base))))))
    (values pixmap "brown-velvel")))

(defun sand (size &key (gradient (make-gradient
                                  (make-gradient-color 1.0 §c8b6236ff)
                                  (make-gradient-color 0.3 §cc09664ff)
                                  (make-gradient-color 0.0 §cf3d599ff))))
  (with-standard-fbm
    (with-random-perlin-gradient-offset
      (let* ((pixmap (tileize
                      (with-standard-generated-pixmap-square (pixmap size)
                        (let* ((dunes
                                (smoothstep-interpolate 0.0 1.0
                                                        (d+ (d* 1.2 (noise (d* 5.0 x) (d* 5.0 y) t))
                                                            (d* 0.2 (dsin (d+
                                                                           (d* 50.0 (d+ y (d* 0.3 (noise x y nil))))
                                                                           (d* 2.0 (dsin (d* 10.0 x)))))))))

                               (color (pick-color gradient dunes)))

                          color)))))
        (values pixmap "sand")))))

(defun water-daytime (size &optional
                             (gradient
                              (make-gradient
                               (make-gradient-color 0.000 (vec4 0.007 0.480 1.0 1.0))
                               (make-gradient-color 0.132 (vec4 0.139 0.681 1.0 1.0))
                               (make-gradient-color 0.560 (vec4 0.284 0.887 0.999 1.0))
                               (make-gradient-color 0.920 (vec4 0.142 0.943 0.811 1.0))
                               (make-gradient-color 1.000 (vec4 0.006 0.998 0.639 1.0)))))
  (with-standard-fbm
    (with-random-perlin-gradient-offset
      (let* ((pixmap (with-standard-generated-pixmap-square (pixmap size)
                       (let* ((color (pick-color gradient (noise (d* (lcg-next-upto 3.0) x) y t))))
                         color))))
        (values pixmap "water")))))

(defun test-water ()
  (multiple-value-bind (pix name)
      (water-daytime 256)
      (save-pixmap pix (fs:file-in-package (text-utils:strcat name ".tga"))))
  t)

(defun test-sand ()
  (save-pixmap (sand 128) (fs:file-in-package "sand.tga"))
  t)

(defun test-rock-1 ()
  (save-pixmap (rock-1 512) (fs:file-in-package "rock-1.tga"))
  t)

(defun test-rock-2 ()
  (save-pixmap (rock-2 512) (fs:file-in-package "rock-2.tga"))
  t)

(defun test-wood-2 ()
  (save-pixmap (wood-2 512) (fs:file-in-package "wood-2.tga"))
  t)

(defun test-wood-3 ()
  (save-pixmap (wood-3 512) (fs:file-in-package "wood-3.tga"))
  t)

(defun test-wood-4 ()
  (save-pixmap (wood-4 512) (fs:file-in-package "wood-4.tga"))
  t)

(defun test-rock-layers ()
  (save-pixmap (rock-layers 512) (fs:file-in-package "rock-layers.tga"))
  t)

(defun test-soil ()
  (save-pixmap (soil 512) (fs:file-in-package "soil.tga"))
  t)

(defun test-voronoize-starfish ()
  (let* ((vor (voronoized-starfish 256)))
    (save-pixmap vor (fs:file-in-package "voronoized-starfish.tga"))
    t))

(defun test-voronoize-graal ()
  (let* ((vor (voronoized-graal 256)))
    (save-pixmap vor (fs:file-in-package "voronoized-graal.tga"))
    t))

(defun test-grass-dirty ()
  (let ((texture (grass-dirty)))
    (save-pixmap texture (fs:file-in-package "grass-dirty.tga"))
    t))

(defun test-dry-stone-floor ()
  (let ((texture (dry-stone-floor)))
    (save-pixmap texture (fs:file-in-package "stone-floor.tga"))
    texture))

(defun test-grass ()
  (let ((texture (grass 256)))
    (save-pixmap texture (fs:file-in-package "grass.tga"))
    t))

(defun test-grass-stones ()
  (let ((texture (grass-stones-floor 512)))
    (save-pixmap texture (fs:file-in-package "grass-stone-floor.tga"))
    t))

(defun test-sample2 ()
  (let ((check (make-instance 'tga)))
    (load check (fs:file-in-package "check.tga"))
    (loop-matrix (check x y)
       (draw-normalized-coord
        check
        (d/ (desired x) (desired (width check)))
        (d/ (desired y) (desired (height check)))
        #'(lambda (x y)
            (byte-vector->vec4 (sample@ check x y :interpolation t :clamp t)))))
    (save-pixmap check (fs:file-in-package "check-2.tga"))
    t))

(defun test-dry-soil ()
  (let ((texture (dry-soil 256)))
    (save-pixmap texture (fs:file-in-package "dry-soil.tga"))
    t))

(defun test-half-moon ()
  (let ((pixmap (half-moon)))
    (with-open-file (stream (fs:file-in-package "half-moon.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-moon ()
  (let ((pixmap (moon)))
    (with-open-file (stream (fs:file-in-package "full-moon.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-floor-broken ()
  (let ((pixmap (ruined-stone-floor-road)))
    (with-open-file (stream (fs:file-in-package "broken-floor.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-floor-fancy ()
  (let ((pixmap (stone-floor-fancy)))
    (with-open-file (stream (fs:file-in-package "fancy-floor.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-dry-stone-wall ()
  (let* ((pixmap (dry-stone-wall)))
    (save-pixmap pixmap (fs:file-in-package "dry-stone-wall.tga"))
    t))

(defun test-perlin-2d ()
  (let ((pixmap (with-draw-normalizated-coord-square (x y 256 pixmap 4)
                  (let ((pixel (range-0to1
                                (perlin-2d (d/ (d* (desired x) 256.0) 8.0)
                                           (d/ (d* (desired y) 256.0) 8.0)))))
                    (vec4 pixel pixel pixel 1.0)))))
    (with-open-file (stream (fs:file-in-package "perlin-2d.ppm") :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (format stream "~a" (pixmap:matrix->ppm* pixmap 255)))))

(defun test-perlin-2d-ref ()
  (with-random-perlin-gradient-offset
    (let ((pixmap (with-draw-normalizated-coord-square (x y 256 pixmap 4
                                                  :bindings (*perlin-gradient-random-offset*))
                    (let ((pixel (range-0to1
                                  (perlin-2d (d/ (d* (desired x) 256.0) 8.0)
                                             (d/ (d* (desired y) 256.0) 8.0)))))
                      (vec4 pixel pixel pixel 1.0)))))
      (with-open-file (stream (fs:file-in-package "perlin-2d-ref.ppm") :direction :output
                              :if-exists :supersede :if-does-not-exist :create)
        (format stream "~a" (pixmap:matrix->ppm* pixmap 255))))))


(defun test-perlin-3d ()
  (with-random-perlin-gradient-offset
    (let ((pixmap (with-draw-normalizated-coord-square (x y 256 pixmap 4
                                                  :bindings (*perlin-gradient-random-offset*))
                    (let ((pixel (pixmap::range-0to1
                                  (perlin-3d
                                   (d/ (d* (desired x) 256.0) 8.0)
                                   (d/ (d* (desired y) 256.0) 8.0) 0.0))))
                      (vec4 pixel pixel pixel 1.0)))))
      (with-open-file (stream (fs:file-in-package "perlin-3d-ref.ppm") :direction :output
                              :if-exists :supersede :if-does-not-exist :create)
        (format stream "~a" (pixmap:matrix->ppm* pixmap 255))))))

(defun test-fbm (&optional (size 1024))
  (let* ((noise-offset (lcg-next))
         (pixmap (with-draw-normalizated-coord-square (x y size pixmap 4)
                   (let* ((*perlin-gradient-random-offset* noise-offset)
                          (noise (gen-fbm (d* 10.0 x) (d*  10.0 y)
                                          6 1.0 1.0 0.5 2.0
                                          :normalize t :range-0->1 t))
                          (background (smoothstep-interpolate 0.0 0.5 x))
                          (color (sb-cga:vec noise noise noise)))
                     (concatenate 'vec4
                                  (sb-cga:vec* color background)
                                  (vector 1.0))))))
    (with-open-file (stream (fs:file-in-package "fbm.ppm") :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (format stream "~a" (pixmap:matrix->ppm* pixmap 255)))))

(defun test-crescent-moon ()
  (let ((pixmap (crescent-moon)))
    (with-open-file (stream (fs:file-in-package "crescent-moon.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))))

(defun test-wood ()
  (let ((pixmap (wood-log-texture)))
    (with-open-file (stream (fs:file-in-package "wood.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-smoke ()
  (let ((pixmap (smoke-tray 512)))
    (misc:dbg "origin@ ~a" (find-smoke-origin pixmap))
    (with-open-file (stream (fs:file-in-package "smoke.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-smoke* ()
  (let ((pixmap (smoke-tray 512 :direction -90 :evanescence 1.0)))
    (misc:dbg "origin@ ~a" (find-smoke-origin pixmap))
    (with-open-file (stream (fs:file-in-package "smoke.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-sun ()
  (let ((pixmap (sun-daylight)))
    (with-open-file (stream (fs:file-in-package "sun.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun test-sunset ()
  (let ((pixmap (sun-sunset 512)))
    (with-open-file (stream (fs:file-in-package "sunset.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))

(defun matrix-fbm-octaves (&optional (size 512))
  (num:with-lcg-seed (1)
    (loop for octave from 2 to 10 do
         (let* ((pixmap (with-draw-normalizated-coord-square (x y size pixmap 4
                                                        :bindings (*perlin-gradient-random-offset*))
                          (let* ((noise (gen-fbm (d* x 2.0) (d* y 2.0) octave 1.0 1.0 0.5 2.0 :normalize t
                                                 :range-0->1 t))
                                 (color (vec4 noise noise noise 1.0)))
                            color)))
                (name (format nil (fs:file-in-package "fbm-octave-~a.tga") octave)))
           (save-pixmap pixmap name)))))

(defun test-matrix-fbm-freq (name steps &optional (size 128))
  (num:with-lcg-seed (1)
    (let ((columns steps)
          (rows steps)
          (table (make-pixmap-frame (* steps size) (* steps size))))
      (loop
         for octave from 1 below 10 by (floor (/ 9 steps))
         for col from 0 below columns do
           (loop
              for freq from 1.0 below 4.0 by (d/ 3.0 (desired steps))
              for row from 0 below rows do
                (let* ((pixmap (with-draw-normalizated-coord-square (x y size pixmap 4
                                                               :bindings (*perlin-gradient-random-offset*))
                                 (let* ((noise (gen-fbm (d* x 2.0) (d* y 2.0) octave freq 1.0 0.5 2.0 :normalize t
                                                        :range-0->1 t))
                                        (color (vec4 noise noise noise 1.0)))
                                   color))))
                  (blit pixmap table 0 0 (* size col) (* size row)))))
      (save-pixmap table name)))
  t)

(defun test-matrix-fbm-ampl (name steps &optional (size 128))
  (num:with-lcg-seed (1)
    (let ((columns steps)
          (rows steps)
          (table (make-pixmap-frame (* steps size) (* steps size))))
      (loop
         for octave from 1 below 10 by (floor (/ 9 steps))
         for col from 0 below columns do
           (loop
              for ampl from 1.0 below 4.0 by (d/ 3.0 (desired steps))
              for row from 0 below rows do
                (let* ((pixmap (with-draw-normalizated-coord-square (x y size pixmap 4
                                                               :bindings (*perlin-gradient-random-offset*))
                                 (let* ((noise (gen-fbm (d* x 2.0) (d* y 2.0) octave 2.0 ampl
                                                        0.5 2.0 :normalize t
                                                        :range-0->1 t))
                                        (color (vec4 noise noise noise 1.0)))
                                   color))))
                  (blit pixmap table 0 0 (* size col) (* size row)))))
      (save-pixmap table name)))
  t)

(defun test-matrix-fbm-ampl-step (name steps &optional (size 128))
  (num:with-lcg-seed (1)
    (let ((columns steps)
          (rows steps)
          (table (make-pixmap-frame (* steps size) (* steps size))))
      (loop
         for octave from 2 below 10 by (floor (/ 8 steps))
         for col from 0 below columns do
           (loop
              for ampl from 1.0 downto 0.1 by (d/ 0.9 (desired steps))
              for row from 0 below rows do
                (let* ((pixmap (with-draw-normalizated-coord-square (x y size pixmap 4
                                                               :bindings (*perlin-gradient-random-offset*))
                                 (let* ((noise (gen-fbm (d* x 2.0) (d* y 2.0)

                                                        octave 1.0 0.5
                                                        ampl 2.0 :normalize t
                                                        :range-0->1 t))
                                        (color (vec4 noise noise noise 1.0)))
                                   color))))
                  (blit pixmap table 0 0 (* size col) (* size row)))))
      (save-pixmap table name)))
  t)

(defun test-matrix-fbm-freq-step (name steps &optional (size 128))
  (num:with-lcg-seed (1)
    (let ((columns steps)
          (rows steps)
          (table (make-pixmap-frame (* steps size) (* steps size))))
      (loop
         for octave from 2 below 10 by (floor (/ 8 steps))
         for col from 0 below columns do
           (loop
              for freq from 1.0 below 4.0 by (d/ 3.0 (desired steps))
              for row from 0 below rows do
                (let* ((pixmap (with-draw-normalizated-coord-square (x y size pixmap 4
                                                                      :bindings (*perlin-gradient-random-offset*))
                                 (let* ((noise (gen-fbm (d* x 2.0) (d* y 2.0)

                                                        octave 1.0 0.5
                                                        2.0 freq :normalize t
                                                        :range-0->1 t))
                                        (color (vec4 noise noise noise 1.0)))
                                   color))))
                  (blit pixmap table 0 0 (* size col) (* size row)))))
      (save-pixmap table name)))
  t)

(defun skydome-approx-pos-celestial-body ()
  (lambda (h)
    (declare (optimize (debug 0) (safety 0) (speed 3)))
    (declare (desired-type h))
    (let ((x (num:dlerp (num:d* (d- h 6.0) (desired 1/14))
                        0.25 0.75)))
      (values x
              (d+ (d* -15.0 (dexpt (d- x 0.5) 2.0))
                  0.7)))))

(defun mountain (w h &key (gradient
                           (make-gradient
                            (make-gradient-color 1.0 §c641a0eff)
                            (make-gradient-color 0.0 §c874006ff))))
  (declare (fixnum w h))
  (declare (gradient gradient))
  (labels ((noise (x y)
             (declare (desired-type x y))
             (gen-fbm x y 4 2.0 2.0 0.65 2.1 :normalize nil)))
    (with-random-perlin-gradient-offset
      (let* ((pixmap
              (with-standard-generated-pixmap (pixmap w h)
                (let* ((r 0.0)
                       (freq 5.0)
                       (r2 (d+ 0.001 (d* 0.5 (dsin (d* y 1.0)))))
                       (act-x (d+ x (d* 0.025 (noise (d* 3.1 x) y))))
                       (c (alexandria:clamp
                           (d* (d+ 0.3 (d/ (d+ 1.0 (dsin (d* y freq))) 2.0))
                               (d- (smoothstep-interpolate (d- r r2) r         (d- act-x 0.5))
                                   (smoothstep-interpolate r         (d+ r r2) (d- act-x 0.5))))
                           0.0 1.0))
                       (color (vec4* (pick-color gradient c) c)))
                  (vec->vec4 color (smoothstep-interpolate 0.0 0.01 (elt color 3)))))))
        (values pixmap "mountain")))))

(defun test-mountain ()
  (save-pixmap (mountain 512 512) (fs:file-in-package "smountain.tga"))
  t)

(defun skydome-mountains (w h)
  (declare (fixnum w h))
  (with-random-perlin-gradient-offset
    (let* ((bg (make-pixmap-frame w h 4 (ubvec4 0 0 0 0)))
           (mountains (loop repeat (truncate (/ w 200)) collect
                           (let ((h-mountain (d* (desired h)
                                                 (desired (lcg-next-in-range 0.63 0.65)))))
                             (declare (desired-type h-mountain))
                             (mountain (truncate (/ w 10)) (truncate h-mountain))))))
      (declare (pixmap bg))
      (loop
         for radius in '(5 2 1)
         for offset in '(0.7 0.8 1.0)
         for subseq-len     = (length mountains) then (truncate (* (length mountain-layer) 0.8))
         for mountain-layer = (subseq mountains 0 subseq-len) do
           (loop for mountain in mountain-layer do
                (pblit-w-repeat (matrix->pixmap
                                 (pgaussian-blur-separated mountain #'identity radius))
                                bg
                                0 0
                                (lcg-next-in-range 0 w)
                                (f- h
                                    (the fixnum (truncate (d* offset (desired (height mountain))))))
                                :discard-blit-fn #'(lambda (x-dst y-dst src dest)
                                                     (declare (ignore x-dst src dest))
                                                     (declare (optimize (debug 0)
                                                                        (safety 0)
                                                                        (speed 3)))
                                                     (declare (fixnum x-dst y-dst))
                                                     (declare (pixmap src dest))
                                                     (num:f< y-dst 0))
                                :behaivour-on-border-fn-y #'(lambda (val max)
                                                              (declare (optimize (debug 0)
                                                                                 (safety 0)
                                                                                 (speed 3)))
                                                              (declare (fixnum val max))
                                                              (if (or (f>= val max)
                                                                      (f<  val 0))
                                                                  -1
                                                                  val))
                                :function-blend (blit-blend-lerp-fn))))
        bg)))


(defun test-skydome-mountains ()
  (save-pixmap (skydome-mountains 512 384) (fs:file-in-package "smountains.tga"))
  t)

#-debug-mode (alexandria:define-constant +skydome-width+  3600  :test #'=)

#-debug-mode (alexandria:define-constant +skydome-height+ 1350 :test #'=)

#+debug-mode (alexandria:define-constant +skydome-width+  180  :test #'=)

#+debug-mode (alexandria:define-constant +skydome-height+ 67 :test #'=)

(defun hour->act-hour (h)
  (d- (desired h) 6.0))

(defun gen-bg-sky-colors-day (hour &optional (sky-gradient +skydome-gradient+))
  (let ((act-h (hour->act-hour hour)))
    (cond
      ((< hour 8)
       (pick-color sky-gradient (d/ act-h 15.0)))
      ((= hour 8)
       §cf86161ff)
      ((<= 9 hour 11)
       +standard-sky-sunny-color+)
      ((<= 12 hour 17)
       +standard-sky-sunny-color+)
      ((or (= hour 18)
           (= hour 19))
       §cf86161ff)
      (t
       (pick-color sky-gradient (d/ act-h 15.0))))))

(defun gen-bg-sky-colors (hour &optional (sky-gradient +skydome-gradient+))
  (if (<= 6 hour 20)
      (gen-bg-sky-colors-day hour sky-gradient)
      §c000000ff))

(defun skydome-day (hour &key (pos-fn (skydome-approx-pos-celestial-body))
                           (sky-gradient +skydome-gradient+)
                           (sun-size      48))
  (with-random-perlin-gradient-offset
    (multiple-value-bind (dx dy)
        (funcall pos-fn (desired hour))
      (let* ((sun (if (< 10 hour 17)
                      (sun-cheap  sun-size)
                      (sun-sunset (f* sun-size 2))))
             (sun-scale (/ (* +skydome-width+ (/ 1/12 (max dy 0.2))) (width sun))))
        (ncopy-matrix-into-pixmap sun (scale-matrix sun sun-scale sun-scale) 4)
        (setf sun (pgaussian-blur-separated sun #'identity 5))
        (let ((bg (with-standard-generated-pixmap (pixmap +skydome-width+ +skydome-height+)
                    (let* ((act-y (d- y (d- 1.0  dy)))
                           (act-x (d- x (d- 1.0  dx)))
                           (v     (vec2:vec2 act-x act-y))
                           (p     (d+ (vec2:vec2-length v)))
                           (w     (dlerp (smoothstep-interpolate 0.0 0.5 p) 0.0 1.0))
                           (act-h (hour->act-hour hour)))
                      (cond
                        ((< hour 8)
                         (mix-color
                          (pick-color sky-gradient (d/ (d+ 1.0 act-h ) 15.0))
                          (pick-color sky-gradient (d/ act-h 15.0))
                          w))
                        ((= hour 8)
                         (mix-color
                          §cffa92fff
                          §cf86161ff
                          w))
                        ((<= 9 hour 11)
                          +standard-sky-sunny-color+)
                        ((<= 12 hour 17)
                          +standard-sky-sunny-color+)
                        ((or (= hour 18)
                             (= hour 19))
                         (mix-color
                          §cffa92fff
                          §cf86161ff
                          w))
                        (t
                         (mix-color
                          (pick-color sky-gradient (d/ (d- act-h 1.0) 15.0))
                          (pick-color sky-gradient (d/ act-h 15.0))
                          w)))))))
          (let* ((x-dst-blit (floor (d- (d* (d- 1.0 dx) (desired (width bg)))
                                       (d/ (desired (width sun)) 2.0))))
                 (y-dst-blit  (floor (d- (d* (d- 1.0 dy) (desired (height bg)))
                                         (d/ (desired (height sun)) 2.0))))
                 (w-blit      (f- (width sun)
                                  (f- (f+ x-dst-blit (width sun))
                                      (width bg))))
                 (h-blit      (f- (height sun)
                                 (f- (f+ y-dst-blit (height sun))
                                     (height bg)))))
            (pblit-unsafe sun bg 0 0 x-dst-blit y-dst-blit
                          :w (if (f< w-blit (width sun)) w-blit (width sun))
                          :h (if (f< h-blit (height sun)) h-blit (height sun))
                          :function-blend (blit-blend-lerp-fn)))
          bg)))))

(defun skydome-night (hour &key
                             (pos-fn (skydome-approx-pos-celestial-body))
                             (moon-texture-size 128))
  (with-random-perlin-gradient-offset
    (multiple-value-bind (dx dy)
        (funcall pos-fn (desired (if (<= 0 hour 20)
                                     (+ hour 13)
                                     (- hour 14))))
      (let* ((shape (case (lcg-next-upto 3)
                      (0 :full)
                      (1 :half)
                      (2 :crescent)))
             (moon (case shape
                     (:full (copy-matrix (moon moon-texture-size)))
                     (:half (copy-matrix (half-moon moon-texture-size)))
                     (:crescent (copy-matrix (crescent-moon (f* moon-texture-size 2))))))
             (moon-scale (/ (* +skydome-width+ (/ 1/30 (max dy 0.2))) (width moon))))
        (setf moon (pgaussian-blur-separated moon #'identity 5))
        (ncopy-matrix-into-pixmap moon (scale-matrix moon moon-scale moon-scale) 4)
        (let ((bg (with-standard-generated-pixmap (pixmap +skydome-width+ +skydome-height+)
                    (let* ((act-y (d- y (d- 1.0  dy)))
                           (act-x (d- x (d- 1.0  dx)))
                           (v     (vec2:vec2 act-x act-y))
                           (p     (d+ (vec2:vec2-length v)))
                           (w     (case shape
                                    (:crescent
                                     (dlerp (smoothstep-interpolate 0.0 0.1 p) 0.0 1.0))
                                    (:full
                                     (dlerp (smoothstep-interpolate 0.0 0.5 p) 0.0 1.0))
                                    (:half 1.0))))
                      (mix-color §c212121ff §c000000ff w)))))
          (let* ((x-dst-blit (floor (d- (d* (d- 1.0 dx) (desired (width bg)))
                                       (d/ (desired (width moon)) 2.0))))
                 (y-dst-blit  (floor (d- (d* (d- 1.0 dy) (desired (height bg)))
                                         (d/ (desired (height moon)) 2.0))))
                 (w-blit      (f- (width moon)
                                  (f- (f+ x-dst-blit (width moon))
                                      (width bg))))
                 (h-blit      (f- (height moon)
                                 (f- (f+ y-dst-blit (height moon))
                                     (height bg)))))
            (pblit-unsafe moon bg 0 0 x-dst-blit y-dst-blit
                          :w (if (f< w-blit (width moon)) w-blit (width moon))
                          :h (if (f< h-blit (height moon)) h-blit (height moon))
                          :function-blend (blit-blend-lerp-fn)))
          (let ((width-blit (floor (/ (width bg) 5))))
            (loop for x fixnum from 0 below width-blit do
                 (loop for y fixnum from 0  below (height bg) do
                      (setf (pixel@ bg (- (width bg) 1 x) y)
                            (map 'ubvec4:ubvec4
                                 #'(lambda (a b) (floor (alexandria:lerp (d (/ x width-blit))
                                                                         b a)))
                                 (pixel@ bg (- (width bg) 1 x) y)
                                 (pixel@ bg x y))))))
          bg)))))

(defun skydome-daytime-p (hour)
  (<= 6 hour 20))

(defgeneric skydome-bottom-color (object))

(defmethod  skydome-bottom-color (hour)
  (if (skydome-daytime-p hour)
      (vec4 (elt +outside-map-day-color+ 0)
            (elt +outside-map-day-color+ 1)
            (elt +outside-map-day-color+ 2))
      (vec4 (elt +outside-map-night-color+ 0)
            (elt +outside-map-night-color+ 1)
            (elt +outside-map-night-color+ 2))))

(defun skydome (hour)
  (let ((mountain     (skydome-mountains +skydome-width+ +skydome-height+))
        (bg           (if (skydome-daytime-p hour)
                          (skydome-day hour)
                          (skydome-night hour)))
        (border-color (skydome-bottom-color  hour)))
    (pblit-unsafe mountain bg 0 0 0 0 :function-blend (blit-blend-lerp-fn))
    (values (with-draw-normalizated-coord (x y +skydome-width+ +skydome-height+ pixmap 4)
              (mix-color (byte-vector->vec4 (sample@ bg x y))
                         border-color
                         (dlerp (smoothstep-interpolate 0.95 0.98 y) 0.0 1.0)))
            (format nil "skydome-~a.tga" hour))))

(defun dump-skydomes ()
  (loop for i from 0 below 24 do (format t "~a~%" i)
       (save-pixmap (pixmap::skydome i)
                    (format nil (fs:file-in-package "skydome-~a.tga") i))))

(defun test-single-skydome* (num)
    (save-pixmap (pixmap::skydome num)
     (format nil (fs:file-in-package "skydome-~a.tga") num))
    t)

(defun test-single-skydome ()
  (test-single-skydome* 0))

(defun cloud-exp-curve (v density fuzziness)
  (let ((c (max 0.0 (desired (d- (d* 255.0 v) density)))))
    (alexandria:clamp (d/ (d- 255.0 (d* 255.0 (dexpt fuzziness c)))
                          255.0) 0.0 1.0)))

(defun clouds* (&optional (size 512) (density 0.5) (fuzziness 0.05))
  (with-random-perlin-gradient-offset
    (let* ((actual-density (d* 255.0 (d- 1.0 density)))
           (pixmap (with-standard-generated-pixmap-square (pixmap  size)
                     (let* ((noise (gen-fbm (d* x 2.0) (d* y 2.0) 10
                                            1.0 1.0 0.5 2.0 :normalize t :range-0->1 t))
                            (color (vec4 noise noise noise 1.0)))
                       (setf (elt color 3)
                             (desired (cloud-exp-curve noise actual-density fuzziness)))
                       color))))
      (values (tileize pixmap) "clouds"))))

(defun dump-exp-clouds ()
  (test-clouds-exp .6 .92 "clouds.tga")
  (test-clouds-exp .6 .92 "clouds2.tga")
  (test-clouds-exp .0 .09 "clouds3.tga"))

(defun dump-clouds ()
  (with-lcg-seed (1)
    (test-clouds* 100.0 "clouds.tga")
    (test-clouds* 700.0 "clouds2.tga")
    (test-clouds* 10.0 "clouds3.tga")))

(defun clouds (&optional (size 512) (density 22.0))
  (with-random-perlin-gradient-offset
    (let* ((pixmap (tileize (with-standard-generated-pixmap-square (pixmap  size)
                              (let* ((noise (gen-fbm (d* x 2.0) (d* y 2.0) 10
                                                     1.0 1.0 0.5 2.0 :normalize t :range-0->1 t))
                                     (color (vec4 noise noise noise 1.0)))

                                (setf (elt color 3)
                                      (d+ 1.0 (d* -1.0 (num:exp-step noise density 6.0))))
                                color)))))
      (values (with-standard-generated-pixmap-square (tmp size)
                (let ((c (byte-vector->vec4 (sample@ pixmap x y))))
                  (setf (elt c 3)
                        (dlerp (dlerp (smoothstep-interpolate 0.95 0.98 y) 0.0 1.0)
                               (elt c 3)
                               0.0))
                  c))
              "clouds"))))

(defun test-clouds-exp (ds sh name)
  (save-pixmap (clouds* 512 ds sh) (fs:file-in-package name))
  t)

(defun test-clouds* (vc name)
    (save-pixmap (clouds 512 vc) (fs:file-in-package name))
    t)

(defun test-clouds ()
  (test-clouds* 22.0 "clouds.tga")
  t)


(alexandria:define-constant +starting-heading+  (vec2 1.0 0.0)         :test #'vec2=)

(alexandria:define-constant +starting-position+ (vec2 0.0 0.0)         :test #'vec2=)

(alexandria:define-constant +starting-color+    (vec4 1.0 1.0 1.0 1.0) :test #'vec4=)

(defclass turtle ()
  ((heading
    :initform +starting-heading+
    :initarg  heading
    :accessor heading)
   (turtle-pos
    :initform +starting-position+
    :initarg  :turtle-pos
    :accessor turtle-pos)
   (turtle-color
    :initform +starting-color+
    :initarg  :turtle-color
    :accessor turtle-color)
   (line-thickness
    :initform 1
    :initarg  :line-thickness
    :accessor line-thickness)
   (antialiasp
    :initform t
    :initarg  :antialiasp
    :accessor antialiasp)
   (pen-down
    :initform t
    :initarg  :pen-down
    :reader   pen-down-p
    :writer   (setf pen-down))))

(defparameter *pixmap* nil)

(defparameter *turtle* nil)

(defun turtle-rotate-cw  (angle)
  (with-accessors ((heading heading)) *turtle*
    (setf heading (vec2-normalize (sequence->vec2 (2d-vector-rotate heading angle)))))
  *turtle*)

(defun turtle-rotate-ccw (angle)
  (turtle-rotate-cw (d- angle)))

(defun turtle-forward    (size)
  (with-accessors ((heading  heading)
                   (turtle-pos turtle-pos)
                   (turtle-color turtle-color)
                   (pen-down-p  pen-down-p)
                   (line-thickness line-thickness)
                   (antialiasp antialiasp)) *turtle*
    (let ((new-pos (map 'vec2 #'fract (vec2+ (vec2* heading size)
                                             turtle-pos))))
      (when pen-down-p
        (assert *pixmap*)
        (matrix-line-norm *pixmap*
                          turtle-pos
                          new-pos
                          (color-utils:vec4->ubvec4 turtle-color)
                          :antialiasp antialiasp
                          :width line-thickness)
        (setf turtle-pos new-pos)))
    *turtle*))

(defun turtle-set-pos (x y)
  (with-accessors ((turtle-pos turtle-pos)
                   (turtle-color turtle-color)
                   (pen-down-p  pen-down-p)) *turtle*
    (setf turtle-pos (vec2 (fract x) (fract y)))
    (when pen-down-p
      (assert *pixmap*)
      (setf (sample@ *pixmap* (elt turtle-pos 0) (elt turtle-pos 1))
            (color-utils:vec4->ubvec4 turtle-color)))))

(defun turtle-push ()
  (stack:push *turtle*))

(defun turtle-pop ()
  (setf *turtle* (stack:pop)))

(defun turtle-angle ()
  (with-accessors ((heading heading)) *turtle*
    (let ((angle (atan (elt heading 1) (elt heading 0))))
      (if (< angle 0)
          (d+ +2pi+ angle)
          angle))))

(defun set-turtle-angle (turtle angle)
  (setf (slot-value turtle 'heading) (vec2 (d (cos angle)) (d (sin angle))))
  turtle)

(defsetf turtle-angle set-turtle-angle)

(defun turtle-vector-advance (angle size)
  (setf (turtle-angle *turtle*) angle)
  (turtle-forward size)
  *turtle*)

(defmacro with-turtle ((w h &key (bg (ubvec4 0 0 0 0))) &body body)
  `(stack:with-stack (#'eq #'identity)
    (let* ((*pixmap* (make-pixmap ,w ,h 4 ,bg))
           (*turtle*  (make-instance 'turtle)))
      ,@body)))

(defun turtle-spyro (size dist-1 dist-2 angle-1 angle-2 color
                     &optional (minimum-iteration 1.0) (thickness 1))
  (declare (desired-type dist-1 dist-2 angle-1 angle-2))
  (flet ((calc-vertices (angle)
           (let ((degree (ceiling (rad->deg angle))))
             (ceiling (/ (lcm degree 360) degree)))))
    (with-turtle (size size)
      (setf (turtle-color  *turtle*) color)
      (setf (line-thickness *turtle*) (floor thickness))
      (let* ((n1                (calc-vertices angle-1))
             (n2                (calc-vertices angle-2))
             (stop              (d (lcm n1 n2))))
        (labels ((draw (ct min)
                   (declare (optimize (debug 0) (safety 0) (speed 3)))
                   (declare (desired-type ct minimum-iteration))
                   (turtle-vector-advance (d* ct angle-1) dist-1)
                   (turtle-vector-advance (d* ct angle-2) dist-2)
                   (when (or (d< ct min)
                             (<= ct stop))
                     (setf ct (d+ 1.0 ct))
                     (draw ct min))))
          (setf (pen-down *turtle*) nil)
          (turtle-set-pos 0.5 0.5)
          (setf (pen-down *turtle*) t)
          (draw 0.0 minimum-iteration)
          *pixmap*)))))

(defun turtle-poly (size dist angle color line-thickness antialiasp)
  (with-turtle (size size)
    (setf (turtle-color   *turtle*) color)
    (setf (antialiasp     *turtle*) antialiasp)
    (setf (line-thickness *turtle*) line-thickness)
    (labels ((draw ()
               (turtle-rotate-ccw angle)
               (turtle-forward    dist)
               (when (not (= (rem (round (rad->deg (turtle-angle)))
                                  360)
                             0))
                 (draw))))
      (setf (pen-down *turtle*) nil)
      (turtle-set-pos 0.5 0.5)
      (setf (heading *turtle*) (vec2-normalize (vec2 1e-3 0.0)))
      (setf (pen-down *turtle*) t)
      (draw)
      *pixmap*)))

(defun red-aura (size)
  (let* ((ext (turtle-spyro size 0.004 0.001
                            (num:deg->rad 2.24)
                            (num:deg->rad 0.5)
                            §c870000ff
                             10000.0))
         (ext2 (turtle-spyro size 0.004 0.001
                            (num:deg->rad 2.24)
                            (num:deg->rad 0.5)
                            §cff0000ff
                             10000.0)))
    (with-premultiplied-alpha (ext)
      (setf ext (pgaussian-blur-separated ext #'floor 10)))
    (with-premultiplied-alpha (ext2)
      (setf ext2 (pgaussian-blur-separated ext2 #'floor 2)))
    (pblit ext2 ext 0 0 0 0 :function-blend (blit-blend-lerp-fn))
    (clip-to-bounding-box ext)))

(defun pentacle (size)
  (let ((star (clip-to-bounding-box (turtle-poly (floor (* size 0.7)) 0.1
                                                 (deg->rad 144.0)
                                                 §cb70000ff
                                                  (max 3 (floor (/ size 300)))
                                                  t)))
        (aura (red-aura size)))
    (with-premultiplied-alpha (star)
      (ncopy-matrix-into-pixmap star (scale-matrix star
                                                   (d (/ (width  aura) (width star)))
                                                   (d (/ (height aura) (height star))))))
    (pblit star aura 0 0 0 0 :function-blend (blit-blend-lerp-fn))
    aura))

(defun test-turtle (size)
  (save-pixmap (pentacle size) (fs:file-in-package "turtle.tga"))
  t)

(defun test-scale ()
  (let ((px  (make-pixmap 7 7)))
    (setf (pixel@ px 3 3) (ubvec4 255 0 0 255))
    (ncopy-matrix-into-pixmap px (scale-matrix px 10.0 10.0))
    (save-pixmap px (fs:file-in-package "b.tga"))
    t))

(defun test-lerp ()
  (let ((a (make-pixmap 1024 1024))
        (b (make-pixmap 1024 1024 4 (ubvec4 0 255 0 255))))
    (matrix-line-norm a #(0.0 0.0) #(0.5 0.3) (ubvec4 255 0 0 255) :width 8)
    (pblit-unsafe a b 0 0 0 0 :function-blend (blit-blend-lerp-fn))
    (save-pixmap b (fs:file-in-package "b.tga"))
    t))

(defun test-all-textures ()
  (let ((tests '(test-single-skydome
                 test-wood-2 test-wood-3 test-wood-4 test-rock-2
                 test-soil test-voronoize-graal test-voronoize-starfish test-clouds
                 test-brick-wall test-smoke test-perlin-2d-ref test-perlin-3d test-grass
                 test-half-moon test-moon test-floor-broken test-dry-stone-wall
                 test-fbm test-crescent-moon test-wood  test-sun test-grass-stones
                 test-sunset test-floor-fancy test-perlin-2d test-dry-soil
                 test-glass-tile test-blood-splat test-rock-1 test-sand
                 test-rock-layers test-wood-wall
                 test-stone-floor-road)))
    (loop for i in tests do
         (num:with-lcg-seed (1)
           (misc:dbg "test ~a" i)
           (funcall (symbol-function i))))))

(defun dump-gradient (&optional (gradient
                                 (make-instance 'gradient
                                                :colors
                                                (list
                                                 (make-gradient-color 0.0 §c776d6bff)
                                                 (make-gradient-color 0.3 §c000000ff)
                                                 (make-gradient-color 0.4 §c000000ff)
                                                 (make-gradient-color 1.0 §cb0aeabff)))))
  (let* ((pixmap (make-pixmap-frame 256 128 4)))
    (loop for i from 0 below 256 do
         (let ((color (pick-color gradient (d/ (desired i) (desired 256)))))
           (loop for j from 0 below 128 do
                (setf (matrix-elt pixmap j i)
                      (map 'ivec4 #'float->byte color)))))
    (with-open-file (stream (fs:file-in-package "gradient.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    t))
