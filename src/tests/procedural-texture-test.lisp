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

(in-package :procedural-texture-test)

(defsuite procedural-texture-suite (all-suite))

(alexandria:define-constant +texture-dir+
    (concatenate 'string (test-dir) "data/procedural-texture/")
  :test #'string=)

(alexandria:define-constant +texture-dir-tmp+ (concatenate 'string (test-dir) "tmp/")
  :test #'string=)

(defun test-fuzzy-frame ()
  (let ((pixmap (pixmap::fuzzy-frame 512 0.039 0.25 20)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "frame.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
     "frame.tga"))

(defun test-fuzzy-circular-frame ()
  (let ((pixmap (pixmap::fuzzy-circular-frame 512 0.15 0.21 #xffffffaa
                                      #x00000000 0.05)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "circular-frame.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "circular-frame.tga"))

(defun test-worley ()
  (let* ((lut (pixmap::2d-worley-lut 128 10 0 0 8 2 32
                             #'(lambda (a) (d- (elt a 1) (elt a 0)))))
         (pixmap (with-draw-normalizated-coord-square (x y 128 pixmap 4)
                   (vector (matrix:sample@ lut x y)
                           (matrix:sample@ lut x y)
                           (matrix:sample@ lut x y)
                           1.0))))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "worley.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "worley.tga"))

(defun test-brick-wall (&optional (size 256))
  (let ((pixmap (pixmap::brick-wall size 64 32)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "bricks-wall.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "bricks-wall.tga"))

(defun test-stone-floor (size)
  (let ((pixmap (pixmap::stone-floor-road size 64 64)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "floor.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "floor.tga"))

(defun test-grass-stones-floor ()
    (let ((pixmap (grass-stones-floor 512))
          (name "grass-stone-floor.tga"))
      (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ name)
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
      name))

(defun test-glass-tile (&optional (size 256))
  (let ((pixmap (glass-tile size)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "glass-tile.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "glass-tile.tga"))

(defun test-starfish-tile (size)
  (let ((pixmap (starfish size)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "starfish.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "starfish.tga"))

(defun test-blood-splat (&optional (size pixmap::+default-size-pixmap-library+))
  (let ((pixmap (pixmap:blood-splat size)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "blood-splat.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "blood-splat.tga"))

(defun test-blood-particle (&optional (size pixmap::+default-size-pixmap-library+))
  (let ((pixmap (pixmap:blood-particle size)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "blood-particle.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "blood-particle.tga"))

(defun test-fire-particle (&optional (size pixmap::+default-size-pixmap-library+))
  (let ((pixmap (pixmap:fire-particle size)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "fire-particle.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "fire-particle.tga"))

(defun test-aerial-explosion-particle (&optional (size pixmap::+default-size-pixmap-library+))
  (let ((pixmap (pixmap:aerial-explosion-particle size)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "aerial-explosion-particle.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "aerial-explosion-particle.tga"))

(defun test-half-moon ()
  (let ((pixmap (half-moon)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "half-moon.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "half-moon.tga"))

(defun test-moon ()
  (let ((pixmap (moon)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "full-moon.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "full-moon.tga"))

(defun test-floor-broken ()
  (let ((pixmap (pixmap::ruined-stone-floor-road)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "broken-floor.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "broken-floor.tga"))

(defun test-floor-fancy ()
  (let ((pixmap (pixmap::stone-floor-fancy)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "fancy-floor.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
   (write-sequence (pixmap->tga-file pixmap) stream))
   "fancy-floor.tga"))

(defun test-dry-stone-wall ()
   (let* ((pixmap (dry-stone-wall)))
     (dump-pixmap pixmap +texture-dir-tmp+ "dry-stone-wall.tga")))

(defun test-perlin-2d ()
   (let ((pixmap (with-draw-normalizated-coord-square (x y 256 pixmap 4)
                   (let ((pixel (pixmap::range-0to1
                                 (noise:perlin-2d (d/ (d* (desired x) 256.0) 8.0)
                                                  (d/ (d* (desired y) 256.0) 8.0)))))
                     (vec4 pixel pixel pixel 1.0)))))
     (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "perlin-2d.ppm")
                             :direction :output
                             :if-exists :supersede :if-does-not-exist :create)
       (format stream "~a" (pixmap:matrix->ppm* pixmap 255)))
      "perlin-2d.ppm"))

(defun test-fbm (&optional (size 1024))
  (let* ((noise-offset (num:lcg-next))
         (pixmap (with-draw-normalizated-coord-square (x y size pixmap 4)
                  (let* ((noise:*perlin-gradient-random-offset* noise-offset)
                         (noise (pixmap::gen-fbm (d* 10.0 x ) (d*  10.0 y)
                                         6 1.0 1.0 0.5 2.0
                                         :normalize t :range-0->1 t))
                         (background (smoothstep-interpolate 0.0 0.5 x))
                         (color (sb-cga:vec noise noise noise)))
                    (concatenate 'vec4
                                 (sb-cga:vec* color background)
                                 (vector 1.0))))))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "fbm.ppm") :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (format stream "~a" (pixmap:matrix->ppm* pixmap 255)))
    "fbm.ppm"))

(defun test-perlin-3d-ref ()
  (pixmap::with-random-perlin-gradient-offset
    (let ((pixmap (with-draw-normalizated-coord-square (x y 256 pixmap 4
                                                  :bindings (noise:*perlin-gradient-random-offset*))
                    (let ((pixel (pixmap::range-0to1
                                  (noise:perlin-3d
                                   (d/ (d* (desired x) 256.0) 8.0)
                                   (d/ (d* (desired y) 256.0) 8.0) 0.0))))
                      (vec4 pixel pixel pixel 1.0)))))
      (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "perlin-3d-ref.ppm")
                              :direction :output
                              :if-exists :supersede :if-does-not-exist :create)
        (format stream "~a" (pixmap:matrix->ppm* pixmap 255))))
    "perlin-3d-ref.ppm"))

(defun test-perlin-2d-ref ()
  (pixmap::with-random-perlin-gradient-offset
    (let ((pixmap (with-draw-normalizated-coord-square (x y 256 pixmap 4
                                                  :bindings (noise:*perlin-gradient-random-offset*))
                    (let ((pixel (pixmap::range-0to1
                                  (noise:perlin-2d
                                   (d/ (d* (desired x) 256.0) 8.0)
                                   (d/ (d* (desired y) 256.0) 8.0)))))
                      (vec4 pixel pixel pixel 1.0)))))
      (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "perlin-2d-ref.ppm")
                              :direction :output
                              :if-exists :supersede :if-does-not-exist :create)
        (format stream "~a" (pixmap:matrix->ppm* pixmap 255))))
    "perlin-2d-ref.ppm"))


(defun test-crescent-moon ()
  (let ((pixmap (crescent-moon)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "crescent-moon.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
     "crescent-moon.tga"))

(defun test-smoke ()
  (let ((pixmap (pixmap::smoke-tray 512)))
      (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "smoke.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "smoke.tga"))

(defun test-sun ()
  (let ((pixmap (sun-daylight)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "sun.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "sun.tga"))

(defun test-sunset ()
  (let ((pixmap (sun-sunset)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "sunset.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "sunset.tga"))

(defun test-sun-cheap ()
  (let ((file-name "sun-cheap.tga"))
    (dump-pixmap (sun-cheap 128) +texture-dir-tmp+ file-name)
    file-name))

(defun test-grass ()
  (let ((pixmap (grass 256)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "grass.tga")
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
      (write-sequence (pixmap->tga-file pixmap) stream))
    "grass.tga"))

(defun test-smoke-particle (&optional (size pixmap::+default-size-pixmap-library+))
  (let ((pixmap (pixmap:smoke-particle size)))
    (dump-pixmap pixmap +texture-dir-tmp+ "smoke-particle.tga")
    "smoke-particle.tga"))

(defun test-blurred-circled-cross (&optional (size pixmap::+default-size-pixmap-library+))
  (let* ((name "blurred-circled-cross.tga")
         (pixmap (pixmap:blurred-circled-cross size)))
    (dump-pixmap pixmap +texture-dir-tmp+ name)
    name))

(defun test-dry-soil ()
  (let ((texture (dry-soil 256)))
    (dump-pixmap texture +texture-dir-tmp+ "dry-soil.tga")
    "dry-soil.tga"))

(defun test-voronoize-starfish ()
  (let* ((vor (voronoized-starfish 256)))
    (dump-pixmap vor +texture-dir-tmp+ "voronoized-starfish.tga")
    "voronoized-starfish.tga"))

(defun test-voronoize-graal ()
  (let* ((vor (voronoized-graal 256)))
    (dump-pixmap vor +texture-dir-tmp+ "voronoized-graal.tga")
    "voronoized-graal.tga"))

(defun test-soil ()
  (let ((file-name "soil.tga"))
    (dump-pixmap (soil 512) +texture-dir-tmp+ file-name)
    file-name))

(defun test-rock-1 ()
   (let ((file-name "rock-1.tga"))
     (dump-pixmap (rock-1 512) +texture-dir-tmp+ file-name)
     file-name))

(defun test-rock-layers ()
   (let ((file-name "rock-layers.tga"))
     (dump-pixmap (rock-layers 512) +texture-dir-tmp+ file-name)
     file-name))

(defun test-sand ()
  (let ((file-name "sand.tga"))
    (dump-pixmap (sand 128) +texture-dir-tmp+ file-name)
    file-name))

(defun test-clouds ()
  (let ((file-name "clouds.tga"))
    (dump-pixmap (clouds) +texture-dir-tmp+ file-name)
    file-name))

(defun test-normal-map ()
  (let ((pixmap (pixmap:gen-normal-map (text-utils:strcat +texture-dir+
                                                          "dry-stone-wall.tga"))))
    (dump-pixmap pixmap +texture-dir-tmp+ "dry-stone-wall-normal-map.tga")))

(defun test-single-skydome ()
  (let ((file-name "skydome-0.tga"))
    (dump-pixmap (pixmap::skydome 0) +texture-dir-tmp+ file-name)
    file-name))

(defun test-wood-wall ()
  (let ((file-name "wood-wall.tga"))
    (dump-pixmap (wood-wall 512)  +texture-dir-tmp+ file-name)))

(defun test-wood ()
  (let ((pixmap (wood-log-texture)))
    (with-open-file (stream (format nil "~a~a" +texture-dir-tmp+ "wood.tga") :direction :output
                            :if-exists :supersede :if-does-not-exist :create
                            :element-type +targa-stream-element-type+)
   (write-sequence (pixmap->tga-file pixmap) stream))
   "wood.tga"))

(defun test-wood-2 ()
  (let ((file-name "wood-2.tga"))
    (dump-pixmap (wood-2 512)  +texture-dir-tmp+ file-name)))

(defun test-wood-3 ()
  (let ((file-name "wood-3.tga"))
    (dump-pixmap (wood-3 512)  +texture-dir-tmp+ file-name)))

(defun test-wood-4 ()
  (let ((file-name "wood-4.tga"))
    (dump-pixmap (wood-4 512)  +texture-dir-tmp+ file-name)))

(defun test-red-aura ()
  (let ((file-name "red-aura.tga"))
    (dump-pixmap (red-aura 512) +texture-dir-tmp+ file-name)))

(defun test-pentacle ()
  (let ((file-name "pentacle.tga"))
    (dump-pixmap (pentacle 512) +texture-dir-tmp+ file-name)))

(defmacro test-all-textures ()
  `(with-kernel
     ,@(let ((tests '(test-single-skydome
                      test-normal-map
                      test-dry-stone-wall
                      test-sand
                      test-rock-1
                      test-rock-layers
                      test-soil
                      test-sunset
                      test-voronoize-graal
                      test-voronoize-starfish
                      test-brick-wall
                      test-grass-stones-floor
                      test-perlin-2d-ref
                      test-grass
                      test-half-moon
                      test-moon
                      test-floor-broken
                      test-fbm
                      test-crescent-moon
                      test-wood
                      test-wood-2
                      test-wood-3
                      test-wood-4
                      test-smoke
                      test-sun
                      test-sun-cheap
                      test-dry-soil
                      test-floor-fancy
                      test-perlin-2d
                      test-glass-tile
                      test-blood-splat
                      test-blood-particle
                      test-fire-particle
                      test-aerial-explosion-particle
                      test-smoke-particle
                      test-blurred-circled-cross
                      test-perlin-3d-ref
                      test-clouds
                      test-wood-wall
                      test-red-aura
                      test-pentacle)))
            (loop for i in tests collect
                 `(assert-true
                      (num:with-lcg-seed (1)
                        (let* ((tmp (funcall ,(symbol-function i)))
                               (file-name (concatenate 'string +texture-dir+ tmp))
                               (tmp-file-name (concatenate 'string +texture-dir-tmp+ tmp)))
                          (if (= (fs:file-hash file-name)
                                 (fs:file-hash tmp-file-name))
                              (progn
                                (uiop/filesystem:delete-file-if-exists tmp-file-name)
                                t)
                              nil)))
                    ,(format nil "~a" i))))))

(deftest test-generated-textures (procedural-texture-suite)
  (test-all-textures))
