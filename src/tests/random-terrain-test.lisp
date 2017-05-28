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

(in-package :random-terrain-test)

(defsuite random-terrain-suite (all-suite))

(alexandria:define-constant +terrain-dir+
    (concatenate 'string (test-dir) "data/terrain/")
  :test #'string=)

(defun test-make-map (w h seed)
  (num:with-lcg-seed (seed)
    (let ((map (make-instance 'random-terrain
                              :matrix (gen-empty-terrain w h))))
      (make-map map)
      map)))

(deftest generate-map-test-256-2-height (random-terrain-suite)
  (with-kernel
    (assert-true
        (let ((map (test-make-map 256 256 2))
              (height (make-instance 'pixmap:pgm)))
          (pixmap:load height (concatenate 'string  +terrain-dir+ "terrain-256-2.pgm"))
          (equalp (matrix:data (matrix:map-matrix (matrix map) #'round))
                  (matrix:data height))))))

(deftest generate-map-test-256-2-layer (random-terrain-suite)
  (with-kernel
    (assert-true
        (let ((map (test-make-map 256 256 2))
              (layers (make-instance 'pixmap:tga)))
          (pixmap:load layers (concatenate 'string  +terrain-dir+ "terrain-256-2-layers.tga"))
          (equalp (pixmap:data (texture-weights map))
                  (matrix:data layers))))))
