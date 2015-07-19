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

(define-constant +terrain-chunk-tile-size+            2.0                      :test #'=)

(define-constant +terrain-chunk-size-scale+           2.0                      :test #'=)

(define-constant +wall-w+       +terrain-chunk-tile-size+                      :test #'=)

(define-constant +wall-h-scale+                       2.5                      :test #'=)

(define-constant +wall-h+     (* +wall-h-scale+ +wall-w+)                      :test #'=)

(define-constant +gravity+                            9.81                     :test #'=)

(define-constant +maximum-map-size+                 512.0                      :test #'=)

(define-constant +minimium-map-size+                 32.0                      :test #'=)

(define-constant +maximum-level-difficult+           10.0                      :test #'=)

(define-constant +quad-tree-leaf-size+               64.0                      :test #'=)

(define-constant +zero-height+                       10.0                      :test '=)

(define-constant +terrain-clip-plane-dist-for-water+ -9.0                      :test '=)

(define-constant +terrain-noclip-plane-dist+          0.0                      :test '=)

(define-constant +min-height+                         0.0                      :test '=)

(define-constant +minimum-mountain-height+           25                        :test '=)

(define-constant +lake-height+                        0.0                      :test '=)

(define-constant +water-mesh-starting-y+              9.0                      :test '=)

(define-constant +labirinth-height+                   1.0                      :test '=)

(define-constant +road-height+                        2.0                      :test '=)

(define-constant +maximum-mountain-height+          255.0                      :test '=)

(define-constant +default-size+                     256                        :test '=)

(define-constant +map-max-size+                     512                        :test '=)

(define-constant +2pi+  (coerce (* 2 pi) 'single-float)                        :test '=)

(define-constant +pi/2+ (coerce (/ pi 2) 'single-float)                        :test '=)

(define-constant +pi+   (coerce cl:pi 'single-float)                           :test '=)

(define-constant +x-axe+ (sb-cga:vec 1.0 0.0 0.0)                              :test #'sb-cga:vec~)

(define-constant +y-axe+ (sb-cga:vec 0.0 1.0 0.0)                              :test #'sb-cga:vec~)

(define-constant +z-axe+ (sb-cga:vec 0.0 0.0 1.0)                              :test #'sb-cga:vec~)

(define-constant +zero-vec+ (sb-cga:vec 0.0 0.0 0.0)                           :test #'sb-cga:vec~)

(define-constant +simple-array-fixnum-type+ '(simple-array fixnum)             :test #'equal)

(define-constant +id-camera+        0                                          :test #'=)

(define-constant +id-skydome+       1                                          :test #'=)

(define-constant +start-id-counter+ 2                                          :test #'=)

(define-constant +obj-mesh-file-extension+ "obj"                               :test #'string=)

(define-constant +trees-resource+   "trees"                                    :test #'string=)

(define-constant +names-resource+   "names"                                    :test #'string=)

(define-constant +weapons-names-resource+   '("names" "weapons")               :test #'equalp)

(define-constant +shields-names-resource+   '("names" "shields")               :test #'equalp)

(define-constant +avatar-portrait-resource+ '("img" "avatar-portrait")         :test #'equalp)

(define-constant +maps-resource+        "maps"                                 :test #'string=)

(define-constant +shaders-resource+     "shaders"                              :test #'string=)

(define-constant +models-resource+      "models"                               :test #'string=)

(define-constant +textures-resource+    "textures"                             :test #'string=)

(define-constant +scripts-resource+     "scripts"                              :test #'string=)

(define-constant +furnitures-resource+  '("models" "furnitures")               :test #'equalp)

(define-constant +fonts-resource+       "fonts"                                :test #'string=)

(define-constant +gui-resource+         "gui"                                  :test #'string=)

(define-constant +default-gui-resource+ '("gui" "default")                     :test #'equalp)

(define-constant +default-gui-inventory-items+  '("gui" "default" "inventory") :test #'equalp)

(define-constant +default-character-weapons+    '("characters" "weapons")      :test #'equalp)

(define-constant +default-character-containers+ '("characters" "containers")   :test #'equalp)

(define-constant +default-character-potions+    '("characters" "potions")      :test #'equalp)

(define-constant +default-character-food+       '("characters" "food")         :test #'equalp)

(define-constant +default-character-misc+       '("characters" "misc")         :test #'equalp)
