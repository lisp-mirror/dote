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

(define-constant +terrain-chunk-tile-size+            2.0              :test #'=)

(define-constant +terrain-chunk-size-scale+           2.0              :test #'=)

(define-constant +wall-w+       +terrain-chunk-tile-size+              :test #'=)

(define-constant +wall-h-scale+                       2.5              :test #'=)

(define-constant +wall-h+      (* +wall-h-scale+ +wall-w+)             :test #'=)

(define-constant +wall-decoration-y+     (* 0.75 +wall-h+)             :test #'=)

(define-constant +gravity+                            9.81             :test #'=)

(define-constant +maximum-map-size+                 256.0              :test #'=)

(define-constant +minimium-map-size+                 32.0              :test #'=)

(define-constant +maximum-level-difficult+           10.0              :test #'=)

(define-constant +quad-tree-leaf-size+               64.0              :test #'=)

(define-constant +zero-height+                       10.0              :test '=)

(define-constant +terrain-clip-plane-dist-for-water+ -9.0              :test '=)

(define-constant +terrain-noclip-plane-dist+          0.0              :test '=)

(define-constant +min-height+                         0.0              :test '=)

(define-constant +minimum-mountain-height+           25                :test '=)

(define-constant +lake-height+                        0.0              :test '=)

(define-constant +water-mesh-starting-y+              9.0              :test '=)

(define-constant +labirinth-height+                   1.0              :test '=)

(define-constant +road-height+                        2.0              :test '=)

(define-constant +maximum-mountain-height+          255.0              :test '=)

(define-constant +invalicable-element-cost+         512.0              :test #'=)

(define-constant +minimum-player-layer-cost+          0.0              :test #'=)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +open-terrain-cost+                2.0              :test #'=))

(define-constant +rotate-entity-cost-cost+  (/ +open-terrain-cost+
					       2.0)                    :test #'=)

(define-constant +wear-object-entity-cost-cost+       1.0              :test #'=)

(define-constant +attack-melee-cost+                  3.0              :test #'=)

(define-constant +attack-long-range-bow-cost+         5.0              :test #'=)

(define-constant +attack-long-range-crossbow-cost+    7.0              :test #'=)

(define-constant +attack-long-range-bow-chance-decrement+      -1.6    :test #'=)

(define-constant +attack-long-range-crossbow-chance-decrement+ -0.8    :test #'=)

(define-constant +default-size+                     256                :test '=)

(define-constant +map-max-size+                     512                :test '=)

(define-constant +2pi+  (coerce (* 2 pi) 'single-float)                :test '=)

(define-constant +pi/2+ (coerce (/ pi 2) 'single-float)                :test '=)

(define-constant +pi+   (coerce cl:pi 'single-float)                   :test '=)

(define-constant +x-axe+    (sb-cga:vec 1.0 0.0 0.0)                   :test #'sb-cga:vec~)

(define-constant +y-axe+    (sb-cga:vec 0.0 1.0 0.0)                   :test #'sb-cga:vec~)

(define-constant +z-axe+    (sb-cga:vec 0.0 0.0 1.0)                   :test #'sb-cga:vec~)

(define-constant +zero-vec+ (sb-cga:vec 0.0 0.0 0.0)                   :test #'sb-cga:vec~)

(define-constant +entity-forward-direction+ (sb-cga:vec 0.0 0.0 1.0)   :test #'sb-cga:vec~)

(define-constant +entity-up-direction+      (sb-cga:vec 0.0 1.0 0.0)   :test #'sb-cga:vec~)

(define-constant +simple-array-fixnum-type+ '(simple-array fixnum)     :test #'equal)

(define-constant +id-camera+        0                                  :test #'=)

(define-constant +id-skydome+       1                                  :test #'=)

(define-constant +start-id-counter+ 2                                  :test #'=)

(define-constant +obj-mesh-file-extension+ "obj"                       :test #'string=)

(define-constant +trees-resource+   "trees"                            :test #'string=)

(define-constant +names-resource+   "names"                            :test #'string=)

(define-constant +weapons-names-resource+   '("names" "weapons")       :test #'equalp)

(define-constant +shields-names-resource+   '("names" "shields")       :test #'equalp)

(define-constant +armors-names-resource+    '("names" "armors")        :test #'equalp)

(define-constant +elms-names-resource+      '("names" "elms")          :test #'equalp)

(define-constant +rings-names-resource+     '("names" "rings")         :test #'equalp)

(define-constant +avatar-portrait-resource+ '("img" "avatar-portrait") :test #'equalp)

(define-constant +maps-resource+        "maps"                                 :test #'string=)

(define-constant +shaders-resource+     "shaders"                              :test #'string=)

(define-constant +models-resource+      "models"                               :test #'string=)

(define-constant +textures-resource+    "textures"                             :test #'string=)

(define-constant +scripts-resource+     "scripts"                              :test #'string=)

(define-constant +furnitures-resource+  '("models" "furnitures")               :test #'equalp)

(define-constant +human-player-models-resource+  '("models" "human-player")    :test #'equalp)

(define-constant +ai-player-models-resource+  '("models" "ai")                 :test #'equalp)

(define-constant +fonts-resource+       "fonts"                                :test #'string=)

(define-constant +gui-resource+         "gui"                                  :test #'string=)

(define-constant +default-gui-resource+         '("gui" "default")             :test #'equalp)

(define-constant +default-gui-inventory-items+  '("gui" "default" "inventory") :test #'equalp)

(define-constant +default-character-weapon-dir+ '("characters" "weapons")      :test #'equalp)

(define-constant +default-character-bow-dir+    '("bow")                       :test #'equalp)

(define-constant +default-character-crossbow+   '("crossbow")                  :test #'equalp)

(define-constant +default-character-mace+       '("mace")                      :test #'equalp)

(define-constant +default-character-spear+      '("spear")                     :test #'equalp)

(define-constant +default-character-staff+      '("staff")                     :test #'equalp)

(define-constant +default-character-sword+      '("sword")                     :test #'equalp)

(define-constant +default-character-armor-dir+     '("characters" "armor")     :test #'equalp)

(define-constant +default-character-container-dir+ '("characters" "container")    :test #'equalp)

(define-constant +default-character-elm-dir+       '("characters" "elm")          :test #'equalp)

(define-constant +default-character-fountain-dir+  '("characters" "fountain")     :test #'equalp)

(define-constant +default-character-key-dir+       '("characters" "key")          :test #'equalp)

(define-constant +default-character-elm-dir+       '("characters" "elm")          :test #'equalp)

(define-constant +default-character-potion-dir+    '("characters" "potion")       :test #'equalp)

(define-constant +default-character-ring-dir+      '("characters" "ring")         :test #'equalp)

(define-constant +default-character-shield-dir+    '("characters" "shield")       :test #'equalp)

(define-constant +default-character-shoes-dir+     '("characters" "shoes")        :test #'equalp)

(define-constant +default-character-food-dir+      '("characters" "food")         :test #'equalp)

(define-constant +default-character-misc-dir+      '("characters" "misc")         :test #'equalp)

(define-constant +default-character-inert-obj-dir+ '("characters" "inert")        :test #'equalp)

(define-constant +default-character-filename+      "character.lisp"               :test #'string=)

(define-constant +default-interaction-filename+    "interaction.lisp"             :test #'string=)

(define-constant +default-furniture-templates-dir+ "furniture-templates"          :test #'string=)

(define-constant +gui-static-text-delim+           "§"                            :test #'string=)

(define-constant +gui-static-text-nbsp+            "¬"                            :test #'string=)

(define-constant +standard-float-print-format+     "~,2@f"                        :test #'string=)

(define-constant +container-capacity+              3                              :test #'=)

(define-constant +model-preview-warrior-re+       "preview-warrior"               :test #'string=)

(define-constant +model-preview-archer-re+        "preview-archer"                :test #'string=)

(define-constant +model-preview-wizard-re+        "preview-wizard"                :test #'string=)

(define-constant +model-preview-healer-re+        "preview-healer"                :test #'string=)

(define-constant +model-preview-ranger-re+        "preview-healer"                :test #'string=)

(define-constant +model-preview-ext-re+           "\\.tga$"                       :test #'string=)

(define-constant +model-move-speed+               0.02                            :test #'=)

(define-constant +gui-zoom-entity+                30.0                            :test #'=)

(define-constant +visibility-cone-half-hangle+    0.5235988                       :test #'=)

(define-constant +visibility-cone-height+         (sb-cga:vec 0.0 0.0 40.0)
  :test #'sb-cga:vec~)

(define-constant +visibility-ray-displ-incr+      0.01                            :test #'=)

(define-constant +camera-drag-spring-k+           10.0                            :test #'=)
