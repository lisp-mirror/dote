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

(in-package :constants)

(alexandria:define-constant +terrain-chunk-tile-size+            2.0          :test #'=)

(alexandria:define-constant +terrain-chunk-size-scale+           2.0          :test #'=)

(alexandria:define-constant +wall-w+       +terrain-chunk-tile-size+          :test #'=)

(alexandria:define-constant +wall-h-scale+                       2.5          :test #'=)

(alexandria:define-constant +wall-h+     (* +wall-h-scale+ +wall-w+)          :test #'=)

(alexandria:define-constant +gravity+                            9.81         :test #'=)

(alexandria:define-constant +maximum-map-size+                 512.0          :test #'=)

(alexandria:define-constant +zero-height+                       10.0          :test '=)

(alexandria:define-constant +terrain-clip-plane-dist-for-water+ -9.0          :test '=)

(alexandria:define-constant +terrain-noclip-plane-dist+          0.0          :test '=)

(alexandria:define-constant +min-height+                         0.0          :test '=)

(alexandria:define-constant +minimum-mountain-height+           25            :test '=)

(alexandria:define-constant +lake-height+                        0.0          :test '=)

(alexandria:define-constant +water-mesh-starting-y+              9.0          :test '=)

(alexandria:define-constant +labirinth-height+                   1.0          :test '=)

(alexandria:define-constant +road-height+                        2.0          :test '=)

(alexandria:define-constant +maximum-mountain-height+          255.0          :test '=)

(alexandria:define-constant +default-size+                     256            :test '=)

(alexandria:define-constant +map-max-size+                     512            :test '=)

(alexandria:define-constant +2pi+  (coerce (* 2 pi) 'single-float)            :test '=)

(alexandria:define-constant +pi/2+ (coerce (/ pi 2) 'single-float)            :test '=)

(alexandria:define-constant +pi+   (coerce cl:pi 'single-float)               :test '=)

(alexandria:define-constant +x-axe+ (sb-cga:vec 1.0 0.0 0.0)                  :test #'sb-cga:vec~)

(alexandria:define-constant +y-axe+ (sb-cga:vec 0.0 1.0 0.0)                  :test #'sb-cga:vec~)

(alexandria:define-constant +z-axe+ (sb-cga:vec 0.0 0.0 1.0)                  :test #'sb-cga:vec~)

(alexandria:define-constant +zero-vec+ (sb-cga:vec 0.0 0.0 0.0)               :test #'sb-cga:vec~)

(alexandria:define-constant +simple-array-fixnum-type+ '(simple-array fixnum) :test #'equal)

(alexandria:define-constant +id-camera+        0                              :test #'=)

(alexandria:define-constant +id-skydome+       1                              :test #'=)

(alexandria:define-constant +start-id-counter+ 2                              :test #'=)

(alexandria:define-constant +obj-mesh-file-extension+ "obj"                   :test #'string=)

(alexandria:define-constant +trees-resource+   "trees"                        :test #'string=)

(alexandria:define-constant +names-resource+   "names"                        :test #'string=)

(alexandria:define-constant +avatar-portrait-resource+ '("img" "avatar-portrait")
  :test #'equalp)

(alexandria:define-constant +maps-resource+        "maps"                     :test #'string=)

(alexandria:define-constant +shaders-resource+     "shaders"                  :test #'string=)

(alexandria:define-constant +models-resource+      "models"                   :test #'string=)

(alexandria:define-constant +textures-resource+    "textures"                 :test #'string=)

(alexandria:define-constant +scripts-resource+     "scripts"                  :test #'string=)

(alexandria:define-constant +furnitures-resource+  '("models" "furnitures")   :test #'equalp)

(alexandria:define-constant +fonts-resource+       "fonts"                    :test #'string=)

(alexandria:define-constant +gui-resource+         "gui"                      :test #'string=)

(alexandria:define-constant +default-gui-resource+ '("gui" "default")         :test #'equalp)

(alexandria:define-constant +default-gui-inventory-items+ '("gui" "default" "inventory")
  :test #'equalp)
