(asdf:defsystem :dote
  :description "Dawn of the Era: A tactical game"
  :author "cage <cage@katamail.com>"
  :license "GPLv3"
  :version "0.0.1"
  :pathname "src"
  :serial t
  :depends-on (:swank
               :clunit
               :alexandria
               :lparallel
               :parse-number
               :ieee-floats
               :flexi-streams
               :cl-ppcre-unicode
               :osicat
               :babel
               :trivial-garbage
               :xmls
               :cl-i18n
               :marshal
               :log4cl
               :sb-cga
               :cl-opengl
               :sdl2kit
               :cl-kanren
               :s-dot)
  :components ((:file "package")
               (:file "config")
               (:file "constants")
               (:file "profiling")
               (:file "conditions")
               (:file "parallel-utils")
               (:file "typed-ops")
               (:file "misc-utils")
               (:file "base64")
               (:file "num-utils")
               (:file "die-utils")
               (:file "filesystem-utils")
               (:file "os-utils")
               (:file "text-utils")
               (:file "resources-utils")
               (:file "resource-cache")
               (:file "sdl2.kit-utils")
               (:file "interfaces")
               (:file "ivec2")
               (:file "vec2")
               (:file "uivec")
               (:file "uivec4")
               (:file "ubvec4")
               (:file "ivec4")
               (:file "vec4")
               (:file "quaternion")
               (:file "mtree-utils")
               (:file "xmls-utils")
               (:file "color-utils")
               (:file "2d-utils")
               (:file "sb-cga-utils")
               (:file "identificable")
               (:file "entity")
               (:file "bs-tree")
               (:file "rb-tree")
               (:file "kd-tree")
               (:file "quad-tree")
               (:file "priority-queue")
               (:file "queue")
               (:file "static-queue")
               (:file "stack")
               (:file "matrix")
               (:file "graph")
               (:file "kanren-utils")
               (:file "euler")
               (:file "interpolation")
               (:file "buffered-input-file")
               (:file "parser")
               (:file "noise")
               (:file "pixmap")
               (:file "with-rgb-texture")
               (:file "procedural-pixmap-library")
               (:file "fonts8x8")
               (:file "random-labyrinth")
               (:file "random-terrain")
               (:file "random-names")
               (:file "avatar-portrait")
               (:file "cl-gl-utils")
               (:file "shaders-utils")
               (:file "map-utils")
               (:file "transformable")
               (:file "mesh-material")
               (:file "texture")
               (:file "level-config")
               (:file "game-events")
               (:file "action-scheduler")
               (:file "game-state")
               (:file "basic-interaction-parameters")
               (:file "player-messages-text")
               (:file "interactive-entity")
               (:file "spell")
               (:file "ai-utils")
               (:file "character")
               (:file "random-weapon")
               (:file "random-potion")
               (:file "random-fountain")
               (:file "random-key")
               (:file "random-container")
               (:file "random-shield")
               (:file "random-armor")
               (:file "random-shoes")
               (:file "random-elm")
               (:file "random-ring")
               (:file "random-trap")
               (:file "random-inert-object")
               (:file "random-object-messages")
               (:file "camera")
               (:file "mesh")
               (:file "wall-mesh-shell")
               (:file "window-mesh-shell")
               (:file "door-mesh-shell")
               (:file "furniture-mesh-shell")
               (:file "container-mesh-shell")
               (:file "fountain-mesh-shell")
               (:file "labyrinth-mesh")
               (:file "pickable-mesh")
               (:file "able-to-see-mesh")
               (:file "building-floor-mesh")
               (:file "battle-utils")
               (:module gui
                        :components ((:file "gui-events")
                                     (:file "gui")
                                     (:file "widget")
                                     (:file "inventory-window")
                                     (:file "spell-window")
                                     (:file "player-report-window")
                                     (:file "splash-screen-progress-gauge")
                                     (:file "full-screen-masks")))
               (:file "billboard")
               (:file "world")
               (:file "terrain-chunk")
               (:file "particles")
               (:file "arrows")
               (:file "md2-mesh-normal-lut")
               (:file "md2-mesh")
               (:file "md2-mesh-ai")
               (:file "obj-mesh")
               (:file "trap-mesh-shell")
               (:file "trees")
               (:file "load-level")
               (:file "id3")
               (:file "influence-map")
               (:file "ann")
               (:file "blackboard")
               (:file "attack-tactics")
               (:file "goap")
               (:file "keyboard-config")
               (:file "main-window")
               (:module tests
                        :components ((:file "package")
                                     (:file "all-test")
                                     (:file "num-test")
                                     (:file "uivec-test")
                                     (:file "uivec4-test")
                                     (:file "ubvec4-test")
                                     (:file "vec4-test")
                                     (:file "quaternion-test")
                                     (:file "vec2-test")
                                     (:file "base64-test")
                                     (:file "misc-test")
                                     (:file "matrix-test")
                                     (:file "priority-queue-test")
                                     (:file "graph-test")
                                     (:file "resource-cache-test")
                                     (:file "interpolation-test")
                                     (:file "quad-tree-test")
                                     (:file "kd-tree-test")
                                     (:file "rb-tree-test")
                                     (:file "random-terrain-test")
                                     (:file "random-labyrinth-test")
                                     (:file "pixmap-test")
                                     (:file "color-utils-test")
                                     (:file "sb-cga-utils-test")
                                     (:file "procedural-texture-test")
                                     (:file "terrain-chunk-test")
                                     (:file "avatar-test")
                                     (:file "mtree-test")
                                     (:file "kanren-test")
                                     (:file "action-scheduler-test")
                                     (:file "attack-tactics-test")
                                     (:file "goap-test")))))

(progn
  ;; debug
  ;;(pushnew :inhibit-planner *features*)
  (pushnew :debug-mode              *features*)
  (pushnew :debug-blackboard-layers *features*)
  (pushnew :debug-ai                *features*))
