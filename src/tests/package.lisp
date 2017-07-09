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

(defpackage :all-test
  (:use :cl
        :clunit)
  (:export
   :test-dir
   :all-suite
   :load-test-tga
   :load-test-ppm
   :load-test-pgm
   :dump-pixmap
   :with-kernel
   :run-all-tests))

(defpackage :color-utils-test
  (:use :cl
        :clunit
        :color-utils
        :sb-cga ;; vec type
        :vec4
        :all-test)
  (:export :color-utils-suite))

(defpackage :uivec-test
  (:use :cl
        :clunit
        :uivec
        :all-test)
  (:export :uivec-suite))

(defpackage :uivec4-test
  (:use :cl
        :clunit
        :uivec4
        :all-test)
  (:export :uivec4-suite))

(defpackage :ubvec4-test
  (:use :cl
        :clunit
        :ubvec4
        :all-test)
  (:export :ubvec4-suite))

(defpackage :vec4-test
  (:use :cl
        :clunit
        :vec4
        :all-test)
  (:export :vec4-suite))

(defpackage :base64-test
  (:use :cl
        :clunit
        :base64
        :all-test)
  (:export :base64-suite))

(defpackage :misc-test
  (:use :cl
        :clunit
        :misc
        :all-test)
  (:export :base64-suite))

(defpackage :matrix-test
  (:use :cl
        :clunit
        :matrix
        :all-test)
  (:export :matrix-suite))

(defpackage :priority-queue-test
  (:use :cl
        :clunit
        :pq
        :all-test)
  (:export :test-heap-sort))

(defpackage :graph-test
  (:use :cl
        :clunit
        :graph
        :all-test)
  (:export :test-a*-multilayer))

(defpackage :resource-cache-test
  (:use :cl
        :clunit
        :resource-cache
        :all-test)
  (:export :matrix-suite))

(defpackage :quaternion-test
  (:use :cl
        :clunit
        :constants
        :quaternion
        :all-test)
  (:export :quaternion-suite))

(defpackage :vec2-test
  (:use :cl
        :clunit
        :vec2
        :all-test)
  (:export :vec2-suite))

(defpackage :numeric-test
  (:use :cl
        :clunit
        :num-utils
        :all-test)
  (:export :numeric-suite))

(defpackage :interpolation-test
  (:use :cl
        :clunit
        :num-utils
        :matrix
        :interpolation
        :all-test)
  (:import-from :sb-cga :vec :copy-vec :alloc-vec :vec+ :vec- :vec/)
  (:export :interpolation-suite))

(defpackage :quad-tree-test
  (:use :cl
        :clunit
        :vec4
        :2d-utils
        :pixmap
        :color-utils
        :quad-tree
        :all-test)
  (:shadowing-import-from :quad-tree :data)
  (:shadowing-import-from :pixmap :load)
  (:export :quadtree-suite))

(defpackage :kd-tree-test
  (:use :cl
        :clunit
        :sb-cga
        :kd-tree
        :all-test)
  (:export :kd-tree-suite))

(defpackage :rb-tree-test
  (:use :cl
        :clunit
        :bs-tree
        :rb-tree
        :all-test)
  (:shadowing-import-from :bs-tree :search :map)
  (:export :rb-tree-suite))

(defpackage :random-terrain-test
  (:use :cl
        :clunit
        :all-test
        :random-terrain)
  (:export :generate-map-test-256-2))

(defpackage :random-labyrinth-test
  (:use :cl
        :clunit
        :all-test
        :random-labyrinth)
  (:export :generate-labyrinth-test))

(defpackage :procedural-texture-test
  (:use :cl
        :clunit
        :all-test
        :num
        :vec4
        :pixmap)
  (:shadowing-import-from :pixmap :load)
  (:export :test-generated-textures))

(defpackage :pixmap-test
  (:use :cl
        :clunit
        :all-test
        :num
        :pixmap)
  (:shadowing-import-from :pixmap :load)
  (:export :test-blit))

(defpackage :sb-cga-utils-test
    (:use :cl
          :clunit
          :all-test
          :sb-cga-utils)
  (:export :test-look@))

(defpackage :terrain-chunk-test
    (:use :cl
          :clunit
          :all-test
          :terrain-chunk)
  (:export :simulated-mesh-test))

(defpackage :avatar-test
  (:use :cl
        :clunit
        :all-test
        :avatar-portrait
        :pixmap)
  (:shadowing-import-from :pixmap :load)
  (:export :test-blit))

(defpackage :mtree-test
  (:use :cl
        :clunit
        :all-test
        :mtree)
  (:export :test-find-odd))

(defpackage :kanren-test
  (:use :cl
        :clunit
        :all-test
        :alexandria
        :num
        :vec2
        :ivec2
        :cl-kanren
        :kanren-utils
        :attack-tactics))

(defpackage :action-scheduler-test
  (:use :cl
        :clunit
        :all-test
        :alexandria
        :action-scheduler))

(defpackage :attack-tactics-test
  (:use :cl
        :clunit
        :all-test
        :alexandria
        :blackboard))
