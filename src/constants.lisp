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

(define-constant +maximum-map-size+                  64.0              :test #'=)

(define-constant +debug-map-size+                    16.0              :test #'=)

(define-constant +minimum-map-size+      +debug-map-size+              :test #'=)

(define-constant +tiny-map-size+       +minimum-map-size+              :test #'=)

(define-constant +small-map-size+                    32.0              :test #'=)

(define-constant +medium-map-size+                   48.0              :test #'=)

(define-constant +large-map-size+      +maximum-map-size+              :test #'=)

(define-constant +quad-tree-leaf-size+               64.0              :test #'=)

(define-constant +zero-height+                       10.0              :test '=)

(define-constant +terrain-clip-plane-dist-for-water+ -9.0              :test '=)

(define-constant +terrain-noclip-plane-dist+          0.0              :test '=)

(define-constant +min-height+                         0.0              :test '=)

(define-constant +minimum-mountain-height+           25                :test '=)

(define-constant +lake-height+                        0.0              :test '=)

(define-constant +water-mesh-starting-y+              9.0              :test '=)

(define-constant +labyrinth-height+                   1.0              :test '=)

(define-constant +road-height+                        2.0              :test '=)

(define-constant +maximum-mountain-height+          255.0              :test '=)

(define-constant +invalicable-element-cost+         512.0              :test #'=)

(define-constant +minimum-player-layer-cost+          0.0              :test #'=)

(define-constant +max-number-player-deployed+         8                :test #'=)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +open-terrain-cost+                2.0              :test #'=))

(define-constant +rotate-entity-cost-cost+  (/ +open-terrain-cost+
                                               2.0)                    :test #'=)

(define-constant +wear-object-entity-cost-cost+       1.0              :test #'=)

(define-constant +attack-melee-cost+                  3.0              :test #'=)

(define-constant +attack-long-range-bow-cost+         5.0              :test #'=)

(define-constant +attack-long-range-crossbow-cost+    8.0              :test #'=)

(define-constant +attack-long-range-bow-chance-decrement+      -1.6    :test #'=)

(define-constant +attack-long-range-crossbow-chance-decrement+ -0.8    :test #'=)

(define-constant +place-trap-cost+                  3.0                :test #'=)

(define-constant +activate-switch-cost+             4.0                :test #'=)

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

(define-constant +entity-all-direction+     (list (sb-cga:vec  1.0  0.0  0.0)
                                                  (sb-cga:vec  0.0  0.0  1.0)
                                                  (sb-cga:vec -1.0  0.0  0.0)
                                                  (sb-cga:vec  0.0  0.0 -1.0))
  :test #'equalp)

(define-constant +simple-array-fixnum-type+ '(simple-array fixnum)                :test #'equal)

(define-constant +id-camera+        0                                             :test #'=)

(define-constant +id-skydome+       1                                             :test #'=)

(define-constant +outside-map-day-color+ #(0.039215688 0.17254902 0.08235294 1.0) :test #'equalp)

(define-constant +outside-map-night-color+ #(0.0 0.0 0.0 1.0)                     :test #'equalp)

(define-constant +start-id-counter+              2                                :test #'=)

(define-constant +obj-mesh-file-extension+       "obj"                            :test #'string=)

(define-constant +game-config-resource+          "config"                         :test #'string=)

(define-constant +game-config-filename+          "configuration"                  :test #'string=)

(define-constant +game-saves-resource+           "saves"                          :test #'string=)

(define-constant +decision-tree-data-resource+   "ai"                             :test #'string=)

(define-constant +decision-tree-facts-file+      "facts"                          :test #'string=)

(define-constant +decision-tree-file+            "decision-tree"                  :test #'string=)

(define-constant +trees-resource+                "trees"                          :test #'string=)

(define-constant +names-resource+                "names"                          :test #'string=)

(define-constant +weapons-names-resource+        '("names" "weapons")             :test #'equalp)

(define-constant +shields-names-resource+        '("names" "shields")             :test #'equalp)

(define-constant +armors-names-resource+         '("names" "armors")              :test #'equalp)

(define-constant +helms-names-resource+          '("names" "helms")               :test #'equalp)

(define-constant +rings-names-resource+          '("names" "rings")               :test #'equalp)

(define-constant +avatar-portrait-resource+      '("img" "avatar-portrait")       :test #'equalp)

(define-constant +maps-resource+                 "maps"                           :test #'string=)

(define-constant +shaders-resource+              "shaders"                        :test #'string=)

(define-constant +models-resource+               "models"                         :test #'string=)

(define-constant +model-objects-resource+        '("models" "objects")            :test #'equalp)

(define-constant +model-spritesheet+             "base.tga"                       :test #'string=)

(define-constant +textures-resource+             "textures"                       :test #'string=)

(define-constant +scripts-resource+              "scripts"                        :test #'string=)

(define-constant +furnitures-resource+           '("models" "furnitures")         :test #'equalp)

(define-constant +arrows-resource+               '("models" "weapons" "arrows")   :test #'equalp)

(define-constant +human-player-sprite-resource+  '("models"
                                                   "characters"
                                                   "human-player")                :test #'equalp)

(define-constant +ai-player-sprite-resource+      '("models"
                                                    "characters"
                                                    "ai")                         :test #'equalp)

(define-constant +sprite-weapon-resource+         '("models"
                                                    "characters"
                                                    "weapons")                    :test #'equalp)

(define-constant +sprite-shields-resource+        '("models"
                                                    "characters"
                                                    "shields")                    :test #'equalp)

(define-constant +sprite-helms-resource+          '("models"
                                                    "characters"
                                                    "helmets")                    :test #'equalp)

(define-constant +sprite-armors-resource+         '("models"
                                                    "characters"
                                                    "armors")                     :test #'equalp)

(define-constant +fonts-resource+                 "fonts"                         :test #'string=)

(define-constant +gui-resource+                   "gui"                           :test #'string=)

(define-constant +spell-texture-dir+              '("textures" "spells")          :test #'equalp)

(define-constant +attack-spell-dir+               '("spells" "attack")            :test #'equalp)

(define-constant +spell-dir+                      '("spells")                     :test #'equalp)

(define-constant +default-gui-resource+           '("gui" "default")              :test #'equalp)

(define-constant +default-gui-inventory-items+    '("gui" "default" "inventory")  :test #'equalp)

(define-constant +default-gui-opening+            '("gui" "default" "opening")    :test #'equalp)

(define-constant +default-credit-items+           '("gui" "default" "credits")    :test #'equalp)

(define-constant +default-character-weapon-dir+    '("characters" "weapons")      :test #'equalp)

(define-constant +default-character-bow-dir+       '("bow")                       :test #'equalp)

(define-constant +default-character-crossbow+      '("crossbow")                  :test #'equalp)

(define-constant +default-character-mace+          '("mace")                      :test #'equalp)

(define-constant +default-character-spear+         '("spear")                     :test #'equalp)

(define-constant +default-character-staff+         '("staff")                     :test #'equalp)

(define-constant +default-character-sword+         '("sword")                     :test #'equalp)

(define-constant +default-character-armor-dir+     '("characters" "armor")        :test #'equalp)

(define-constant +default-character-container-dir+ '("characters" "container")    :test #'equalp)

(define-constant +default-character-helm-dir+       '("characters" "helm")        :test #'equalp)

(define-constant +default-character-fountain-dir+  '("characters" "fountain")     :test #'equalp)

(define-constant +default-character-key-dir+       '("characters" "key")          :test #'equalp)

(define-constant +default-character-helm-dir+      '("characters" "helm")         :test #'equalp)

(define-constant +default-character-potion-dir+    '("characters" "potion")       :test #'equalp)

(define-constant +default-character-ring-dir+      '("characters" "ring")         :test #'equalp)

(define-constant +default-character-shield-dir+    '("characters" "shield")       :test #'equalp)

(define-constant +default-character-shoes-dir+     '("characters" "shoes")        :test #'equalp)

(define-constant +default-character-food-dir+      '("characters" "food")         :test #'equalp)

(define-constant +default-character-misc-dir+      '("characters" "misc")         :test #'equalp)

(define-constant +default-character-trap-dir+      '("characters" "trap")         :test #'equalp)

(define-constant +default-character-inert-obj-dir+ '("characters" "inert")        :test #'equalp)

(define-constant +animation-texture-dir+           '("textures" "animation")      :test #'equalp)

(define-constant +turn-transition-billboard-dir+   '("textures" "turn-transition")

  :test #'equalp)

(define-constant +music-resource+                  "music"                        :test #'equalp)

(define-constant +turn-billboard-ai-texture-name+    "ai.tga"                     :test #'string=)

(define-constant +turn-billboard-human-texture-name+ "human.tga"                  :test #'string=)

(define-constant +default-character-filename+      "character.lisp"               :test #'string=)

(define-constant +default-interaction-filename+    "interaction.lisp"             :test #'string=)

(define-constant +default-furniture-templates-dir+ "furniture-templates"          :test #'string=)

(define-constant +default-planner-dir+             '("ai" "planner")              :test #'equalp)

(define-constant +attack-planner-dir+              '("ai" "planner" "attack")     :test #'equalp)

(define-constant +defend-planner-dir+              '("ai" "planner" "defend")     :test #'equalp)

(define-constant +explore-planner-dir+             '("ai" "planner" "explore")    :test #'equalp)

(define-constant +retreat-planner-dir+             '("ai" "planner" "retreat")    :test #'equalp)

(define-constant +save-game-dir-1+                 '("saved" "1")                 :test #'equalp)

(define-constant +save-game-dir-2+                 '("saved" "2")                 :test #'equalp)

(define-constant +save-game-dir-3+                 '("saved" "3")                 :test #'equalp)

(define-constant +save-game-screenshot-name+       "screenshot.tga"               :test #'equalp)

(define-constant +save-game-notes-name+            "notes"                        :test #'equalp)

(define-constant +mesh-placeholder-file+           "placeholder.lsys"             :test #'string=)

(define-constant +gui-static-text-delim+           "§"                            :test #'string=)

(define-constant +gui-static-text-nbsp+            "¬"                            :test #'string=)

(define-constant +standard-float-print-format+     "~,2@f"                        :test #'string=)

(define-constant +container-capacity+              3                              :test #'=)

(define-constant +sprite-preview-warrior+           "warrior"                     :test #'string=)

(define-constant +sprite-preview-archer+            "archer"                      :test #'string=)

(define-constant +sprite-preview-wizard+            "wizard"                      :test #'string=)

(define-constant +sprite-preview-healer+            "healer"                      :test #'string=)

(define-constant +sprite-preview-ranger+            "ranger"                      :test #'string=)

(define-constant +sprite-preview-ext-re+            "preview.tga$"                :test #'string=)

(define-constant +sprite-sheet-ext-re+              "base.tga$"                   :test #'string=)

(define-constant +tga-file-extension+               "tga"                         :test #'string=)

(define-constant +model-move-speed+                0.022                          :test #'=)

(define-constant +sprite-move-speed+               0.022                          :test #'=)

(define-constant +gui-zoom-entity+                15.0                            :test #'=)

(define-constant +visibility-cone-tolerance+       1e-2                           :test #'=)

(define-constant +visibility-cone-half-hangle+     0.7854                         :test #'=)

(define-constant +visibility-cone-height+          (sb-cga:vec 0.0 0.0 40.0)
  :test #'sb-cga:vec~)

(define-constant +visibility-ray-displ-incr+       0.1                            :test #'=)

(define-constant +camera-drag-spring-k+           10.0                            :test #'=)

(define-constant +default-arrow-name+             "bow"                           :test #'string=)

(define-constant +default-bolt-name+              "crossbow"                      :test #'string=)

(define-constant +weapon-pole-range+               2                              :test #'=)

(define-constant +weapon-melee-range+              1                              :test #'=)

(define-constant +weapon-bow-range+               15                              :test #'=)

(define-constant +weapon-crossbow-range+          25                              :test #'=)

(define-constant +explore-strategy+               :explore                        :test #'eq)

(define-constant +attack-strategy+                :attack                         :test #'eq)

(define-constant +defend-strategy+                :defend                         :test #'eq)

(define-constant +retreat-strategy+               :retreat                        :test #'eq)

(define-constant +difficult-minimum+               1                              :test #'=)

(define-constant +difficult-medium+                3                              :test #'=)

(define-constant +maximum-level-difficult+        10.0                            :test #'=)

(define-constant +exp-capital-delta+             100.0                            :test #'=)

(define-constant +exp-change-level-thrs+         1000.0                           :test #'=)

(define-constant +max-character-level+             10                             :test #'=)

(define-constant +influence-ai-sign+               -1.0                           :test #'=)

(define-constant +influence-human-sign+             1.0                           :test #'=)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-constant +ai-fact-map-under-control+           :map-under-control          :test #'eq)

  (define-constant +ai-fact-dmg-ratio+                   :dmg-ratio                  :test #'eq)

  (define-constant +ai-fact-average-dist+                :average-dist               :test #'eq)

  (define-constant +ai-fact-vulnerables-units+           :vulnerables-units          :test #'eq)

  (define-constant +ai-fact-visible-opponents-last-turn+ :visible-opponents-lst-turn :test #'eq)

  (define-constant +ai-fact-visible-opponents+           :visible-opponents          :test #'eq)

  (define-constant +ai-fact-visible-friends+             :visible-friends            :test #'eq)

  (define-constant +ai-fact-wizard-dmg+                  :wizard-dmg                 :test #'eq)

  (define-constant +ai-decision-class+                   :strategy                   :test #'eq)

  (define-constant +ai-fact-header+ (list +ai-fact-map-under-control+
                                          +ai-fact-dmg-ratio+
                                          +ai-fact-average-dist+
                                          +ai-fact-vulnerables-units+
                                          +ai-fact-visible-opponents-last-turn+
                                          +ai-fact-visible-opponents+
                                          +ai-fact-visible-friends+
                                          +ai-fact-wizard-dmg+
                                          +ai-decision-class+)
    :test #'equalp))
