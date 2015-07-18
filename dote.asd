(asdf:defsystem :dote
  :description "Dawn of the Era: A tactical game"
  :author "cage <cage@katamail.com>"
  :license "GPLv3"
  :pathname "src"
  :serial t
  :depends-on (:swank
	       :clunit
	       :alexandria
	       :cl-lex
	       :yacc
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
	       (:file "filesystem-utils")
	       (:file "os-utils")
	       (:file "text-utils")
	       (:file "resources-utils")
	       (:file "resource-cache")
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
	       (:file "bs-tree")
	       (:file "rb-tree")
	       (:file "kd-tree")
	       (:file "quad-tree")
	       (:file "priority-queue")
	       (:file "queue")
	       (:file "stack")
	       (:file "matrix")
	       (:file "graph")
	       (:file "euler")
	       (:file "interpolation")
	       (:file "buffered-input-file")
	       (:file "parser")
	       (:file "noise")
	       (:file "pixmap")
	       (:file "with-rgb-texture")
	       (:file "procedural-pixmap-library")
	       (:file "fonts8x8")
	       (:file "identificable")
	       (:file "random-labyrinth")
	       (:file "random-terrain")
	       (:file "random-names")
	       (:file "avatar-portrait")
	       (:file "cl-gl-utils")
	       (:file "shaders-utils")
	       (:file "transformable")
	       (:file "mesh-material")
	       (:file "texture")
	       (:file "level-config")
	       (:file "game-state")
	       (:file "basic-interaction-parameters")
	       (:file "character")
	       (:file "random-weapon")
	       (:file "random-potion")
	       (:file "random-fountain")
	       (:file "random-key")
	       (:file "random-container")
	       (:file "entity")
	       (:file "camera")
	       (:file "mesh")
	       (:file "wall-mesh-shell")
	       (:file "pickable-mesh")
	       (:file "building-floor-mesh")
	       (:file "game-events")
	       (:module gui
			:components ((:file "gui-events")
				     (:file "gui")
				     (:file "widget")))
	       (:file "world")
	       (:file "terrain-chunk")
	       (:file "md2-mesh-normal-lut")
	       (:file "md2-mesh")
	       (:file "obj-mesh")
	       (:file "trees")
	       (:file "load-level")
	       (:file "id3")
	       (:file "ann")
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
				     (:file "matrix-test")
				     (:file "resource-cache-test")
				     (:file "interpolation-test")
				     (:file "quad-tree-test")
				     (:file "kd-tree-test")
				     (:file "random-terrain-test")
				     (:file "random-labyrinth-test")
				     (:file "pixmap-test")
				     (:file "color-utils-test")
				     (:file "sb-cga-utils-test")
				     (:file "procedural-texture-test")
				     (:file "terrain-chunk-test")
				     (:file "avatar-test")))))
