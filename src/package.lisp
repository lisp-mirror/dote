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

(defpackage :config
  (:use :cl)
  (:export
   :+sys-data-dir+
   :+catalog-dir+
   :+text-domain+
   :+program-name+
   :+home-data-dir+
   :+model-animations-filename+
   :+model-animations-filename+
   :+model-filename+
   :+model-texture-filename+
   :+random-first-names-filename+
   :+random-last-names-filename+
   :+game-fps+
   :+debug-mode+
   :*window-w*
   :*window-h*
   :*workers-number*
   :_
   :n_))

(defpackage :constants
  (:use :cl
	:alexandria)
  (:export
   :+terrain-chunk-tile-size+
   :+terrain-chunk-size-scale+
   :+wall-w+
   :+wall-h-scale+
   :+wall-h+
   :+wall-decoration-y+
   :+gravity+
   :+maximum-map-size+
   :+minimium-map-size+
   :+maximum-level-difficult+
   :+quad-tree-leaf-size+
   :+zero-height+
   :+min-height+
   :+lake-height+
   :+water-mesh-starting-y+
   :+terrain-noclip-plane-dist+
   :+labirinth-height+
   :+road-height+
   :+maximum-mountain-height+
   :+minimum-mountain-height+
   :+invalicable-element-cost+
   :+minimum-player-layer-cost+
   :+open-terrain-cost+
   :+rotate-entity-cost-cost+
   :+wear-object-entity-cost-cost+
   :+default-size+
   :+map-max-size+
   :+pi+
   :+2pi+
   :+pi/2+
   :+x-axe+
   :+y-axe+
   :+z-axe+
   :+entity-forward-direction+
   :+entity-up-direction+
   :+zero-vec+
   :+simple-array-fixnum-type+
   :+id-camera+
   :+id-skydome+
   :+start-id-counter+
   :+obj-mesh-file-extension+
   :+trees-resource+
   :+names-resource+
   :+weapons-names-resource+
   :+shields-names-resource+
   :+armors-names-resource+
   :+elms-names-resource+
   :+rings-names-resource+
   :+avatar-portrait-resource+
   :+maps-resource+
   :+shaders-resource+
   :+models-resource+
   :+human-player-models-resource+
   :+ai-player-models-resource+
   :+textures-resource+
   :+scripts-resource+
   :+furnitures-resource+
   :+fonts-resource+
   :+gui-resource+
   :+default-gui-inventory-items+
   :+default-gui-resource+
   :+default-character-weapon-dir+
   :+default-character-bow-dir+
   :+default-character-crossbow+
   :+default-character-mace+
   :+default-character-spear+
   :+default-character-staff+
   :+default-character-sword+
   :+default-character-armor-dir+
   :+default-character-container-dir+
   :+default-character-elm-dir+
   :+default-character-fountain-dir+
   :+default-character-key-dir+
   :+default-character-elm-dir+
   :+default-character-potion-dir+
   :+default-character-ring-dir+
   :+default-character-shield-dir+
   :+default-character-shoes-dir+
   :+default-character-food-dir+
   :+default-character-misc-dir+
   :+default-character-inert-obj-dir+
   :+default-character-filename+
   :+default-interaction-filename+
   :+default-furniture-templates-dir+
   :+gui-static-text-delim+
   :+gui-static-text-nbsp+
   :+standard-float-print-format+
   :+container-capacity+
   :+model-preview-warrior-re+
   :+model-preview-archer-re+
   :+model-preview-wizard-re+
   :+model-preview-healer-re+
   :+model-preview-ranger-re+
   :+model-preview-ext-re+
   :+model-move-speed+
   :+gui-zoom-entity+
   :+visibility-cone-half-hangle+
   :+visibility-cone-height+
   :+visibility-ray-displ-incr+
   :+camera-drag-spring-k+))

(defpackage :profiling
  (:use :cl)
  (:export :with-profiling))

(defpackage :conditions
  (:use :cl)
  (:export
   :text-error
   :text
   :not-implemented-error
   :null-reference
   :out-of-bounds
   :length-error
   :different-length-error
   :xml-no-matching-tag
   :xml-no-such-attribute
   :invalid-aabb-error))

(defpackage :parallel-utils
  (:use :cl)
  (:export
   :*parallel-setf-queue*
   :parallel-setf))

(defpackage :misc-utils
  (:use :cl
	:constants)
  (:nicknames :misc)
  (:export
   :when-debug
   :debug-log
   :dbg
   :code->char
   :char->code
   :dump-hash-table
   :safe-random
   :split-into-sublist
   :delete@
   :safe-delete@
   :remove-compact-remap-sequence
   :remove-if-null
   :remove-if-not-null
   :do-while
   :do-while*
   :not-null-p
   :permutation
   :shuffle
   :make-fresh-list
   :make-array-frame
   :make-fresh-array
   :vector-empty-p
   :random-num-filled-vector
   :random-elt
   :list->array
   :list->simple-array
   :copy-list-into-array
   :2byte->word
   :2word->int
   :byte->int
   :bytes->string
   :read-ieee-float-32
   :int16->bytes
   :int32->bytes
   :define-offset-size
   :define-parse-header-chunk
   :read-list
   :read-array
   :definline
   :defcached
   :defun-inline-function
   :format-fn-symbol
   :define-compiler-macros
   :defmethod-inline-function
   :nest-expressions
   :replace-e!
   :+nil-equiv-bag+
   :build-plist
   :build-assocs-chain
   :recursive-assoc
   :recursive-assoc-just-before
   :n-setf-path-value
   :plist-path-value
   :gen-trivial-plist-predicates
   :gen-trivial-plist-predicate
   :gen-trivial-plist-get
   :gen-trivial-plist-gets
   :coord-map->chunk
   :coord-terrain->chunk
   :coord-chunk->costs
   :coord-chunk->matrix
   :coord-layer->map-state
   :map-manhattam-distance
   :make-null-pointer))

(defpackage :base64
  (:use :cl
	:alexandria)
  (:nicknames :b64)
  (:export
   :encode
   :decode))

(defpackage :num-utils
  (:use :cl
	:constants)
  (:nicknames :num)
  (:export
   :primes-list
   :make-primes-array
   :*array-primes*
   :rad->deg
   :deg->rad
   :spherical->cartesian
   :cartesian->spherical
   :find-min-max
   :find-min
   :find-max
   :round-all
   :fract
   :fnv-hash-32
   :string-fnv-hash-32
   :fnv-hash-256
   :string-fnv-hash-256
   :*lcg-seed*
   :lcg-set-seed
   :lcg-next
   :lcg-next01
   :lcg-next-upto
   :lcg-next-in-range
   :rejection-sampling
   :bivariate-sampling
   :random-gaussian-distribution
   :gaussian-probability
   :+lcg-max+
   :with-lcg-seed
   :get-random-float-sign
   :random-select-by-frequency
   :random-select-by-frequency*
   :bag-picker
   :random-pick-from-set
   :range-0to1
   :clamp-0->max-less-one
   :*default-epsilon*
   :with-epsilon
   :epsilon=
   :smoothstep
   :sinstep
   :cosstep
   :smoothstep-interpolate
   :sinstep-interpolate
   :cosstep-interpolate
   :hermite-polynomial
   :bidimensional-gaussian
   :gaussian
   :gaussian-function
   :damped-impulse
   :zig-zag-line
   :almost-identity
   :impulse
   :exp-step
   :cubic-pulse
   :parabola
   :enzyme-kinetics
   ;; typed-ops.lisp
   :desired-type
   :desired
   :d
   :d+
   :d*
   :d-
   :d/
   :dsqrt
   :daref
   :dmin
   :dmax
   :d>
   :d<
   :d>=
   :d<=
   :d=
   :dslot-value
   :dsetf
   :dabs
   :dplusp
   :dminusp
   :dzerop
   :dcos
   :dsin
   :dtan
   :dcosh
   :dsinh
   :dtanh
   :dacos
   :dasin
   :datan
   :dexp
   :dexpt
   :dlog
   :dfloor
   :dlerp
   :parse-number->desired
   :f
   :f+
   :f-
   :f*
   :f/
   :f<
   :f<=
   :f>
   :f>=))

(defpackage :die-utils
  (:nicknames :dice)
  (:use :cl
	:alexandria
	:config
	:constants
	:conditions
	:misc
	:num)
  (:shadowing-import-from :misc   :random-elt :shuffle)
  (:export
   :gen-pass-dice
   :pass-d1.0
   :pass-d2
   :pass-d100.0))

(defpackage :filesystem-utils
  (:use :cl)
  (:nicknames :fs)
  (:export
   :+preprocess-include+
   :+file-path-regex+
   :*directory-sep-regexp*
   :*directory-sep*
   :slurp-file
   :dump-sequence-to-file
   :cat-parent-dir
   :has-extension
   :do-directory
   :search-matching-file
   :split-path-elements
   :path-last-element
   :path-first-element
   :path-to-hidden-file-p
   :parent-dir-path
   :strip-dirs-from-path
   :get-stat-mtime
   :get-stat-ctime
   :get-stat-atime
   :file-outdated-p
   :file-exists-p
   :delete-file-if-exists
   :file-hash
   :temporary-filename
   :with-anaphoric-temp-file
   :temp-file
   :file-can-write-p
   :preprocess-include-file
   :preprocess
   :package-path
   :file-in-package))

(defpackage :os-utils
  (:use :cl)
  (:export
   :cpu-number))

(defpackage :text-utils
  (:use :cl)
  (:import-from :misc :definline)
  (:export
   :+float-regexp+
   :+integer-regexp+
   :uchar-length
   :utf8-encoded-p
   :clean-unprintable-chars
   :strcat
   :join-with-srings
   :join-with-srings*
   :strip-prefix
   :strip-withespaces
   :basename
   :wrap-with
   :right-padding
   :left-padding
   :justify-monospaced-text))

(defpackage :resources-utils
  (:use :cl
	:cl-ppcre
	:config
	:constants
	:filesystem-utils
	:text-utils)
  (:nicknames :res)
  (:export
   :home-datadir
   :shared-datadir
   :get-resource-file
   :get-resource-files
   :strip-off-resource-path))

(defpackage :resource-cache
  (:use :cl
	:config
	:conditions
	:text-utils
	:filesystem-utils)
  (:export
   :*cache-system-running*
   :*cache-reference-file*
   :cache-error
   :cache-path
   :init-cache
   :ensure-cache-running
   :cache-key-element
   :make-cache-key-element
   :with-cache-system-running
   :make-cache-key
   :regular-file-strings->cache-key
   :regular-file-strings->cache-key*
   :directory-strings->cache-key
   :directory-strings->cache-key*
   :cons->cache-key
   :cons->cache-key*
   :cache-miss*
   :cache-miss
   :file-miss
   :file-miss*
   :directory-miss
   :directory-miss*
   :ensure-cache-directory
   :ensure-cache-directory*))

(defpackage :sdl2.kit-utils
  (:use :cl
	:alexandria
	:sdl2.kit
	:config
	:constants)
  (:export
   :fetch-window
   :fetch-window-id
   :with-world))

(defpackage :interfaces
  (:use :cl
	:constants
	:misc)
  (:export
   :destructible
   :main-light-pos
   :main-light-pos-eye-space
   :main-light-color
   :get-camera-pos
   :elapsed-time
   :destroy
   :renderizable
   :compiled-shaders
   :prepare-for-rendering
   :update-for-rendering
   :pick-pointer-position
   :main-state
   :calculate
   :render
   :render-debug
   :render-for-reflection
   :clone
   :clone-into
   :copy-flat
   :copy-flat-into
   :with-simple-clone
   :with-simple-copy-flat
   :initializedp
   :to-sexp
   :from-sexp
   :serialize
   :serialize-to-stream
   :deserialize
   :description-for-humans
   :inner-animation
   :start-time
   :el-time
   :animation-speed))

(defpackage :ivec2
  (:use :cl
	:constants
	:misc)
  (:export
   :ivec2-type
   :ivec2
   :+ivec2-zero+
   :make-fresh-ivec2
   :copy-ivec2
   :ivec2=
   :ivec2p
   :ivec2*
   :ivec2/
   :ivec2~
   :ivec2+
   :ivec2-
   :ivec2-length
   :ivec2-normalize))

(defpackage :vec2
  (:use :cl
	:constants
	:misc)
  (:export
   :vec2-type
   :vec2
   :+vec2-zero+
   :make-fresh-vec2
   :copy-vec2
   :vec2p
   :vec2*
   :vec2/
   :vec2~
   :vec2+
   :vec2-
   :vec2-negate
   :vec2-length
   :vec2-normalize
   :vec2-dot-product))

(defpackage :uivec
  (:use :cl
	:constants
	:num
	:misc)
  (:export
   :uivec-type
   :uivec
   :uivecp
   :copy-uivec
   :make-fresh-uivec
   :uivec=
   :+uivec-zero+
   :uivec*
   :uivec/
   :uivec=
   :uivec~
   :uivec+
   :uivec-
   :uivec-length
   :uivec-normalize
   :uivec-dot-product))

(defpackage :uivec4
  (:use :cl
	:constants
	:num
	:misc)
  (:export
   :uivec4-type
   :uivec4
   :uivec4p
   :copy-uivec4
   :uivec4=
   :+uivec4-zero+))

(defpackage :ubvec4
  (:use :cl
	:constants
	:num
	:misc)
  (:export
   :ubvec4-type
   :ubvec4
   :ubvec4p
   :copy-ubvec4
   :make-fresh-ubvec4
   :ubvec4=
   :+ubvec4-zero+
   :ubvec4*
   :ubvec4/
   :ubvec4=
   :ubvec4~
   :ubvec4+
   :ubvec4-
   :ubvec4-length
   :ubvec4-normalize
   :ubvec4-dot-product))

(defpackage :ivec4
  (:use :cl
	:constants
	:num
	:misc)
  (:export
   :ivec4-type
   :ivec4
   :ivec4p
   :copy-ivec4
   :make-fresh-ivec4
   :ivec4=
   :+ivec4-zero+
   :ivec4*
   :ivec4/
   :ivec4=
   :ivec4~
   :ivec4+
   :ivec4-
   :ivec4-length
   :ivec4-normalize
   :ivec4-dot-product))

(defpackage :vec4
  (:use :cl
	:sb-cga
	:misc)
  (:export
   :vec4-type
   :vec4
   :vec->vec4
   :vec4->vec
   :vec4p
   :copy-vec4
   :transform-vec4
   :+vec4-zero+
   :make-fresh-vec4
   :vec4*
   :vec4/
   :vec4=
   :vec4~
   :vec4+
   :vec4-average
   :vec4-average*
   :vec4-
   :vec4-length
   :vec4-normalize
   :vec4-dot-product
   :vec4-negate))

(defpackage :quaternion
  (:use :cl
	:sb-cga
	:constants
	:num-utils
	:vec4)
  (:export
   :quat
   :quat~
   :+quat-identity+
   :qv
   :qw
   :make-quat
   :copy-quat
   :make-quat-from-vw
   :vec->quat
   :axis-rad->quat
   :euler->quat
   :quat-conjugate
   :quat-norm
   :quat+
   :quat*
   :quat-scale
   :quat-inverse
   :quat-rotate-vec
   :quat->matrix
   :matrix->quat
   :quat-rotate-to-vec
   :+slerp-delta+
   :quat-slerp
   :spherical->quat))

(defpackage :mtree-utils
  (:use
   :interfaces
   :cl)
  (:nicknames :mtree)
  (:export
   :random-choose-leaf
   :traverse-apply-tree
   :traverse-napply-tree
   :traverse-find-if-tree
   :traverse-find-all-if-tree
   :traverse-apply-tree-cdr
   :traverse-nadd-child
   :traverse-ndelete-child
   :nappend-child
   :navigate
   :m-tree
   :*use-pprint-tree*
   :pprint-tree
   :children
   :parent
   :data
   :add-child
   :add-children
   :add-children*
   :make-node
   :find-node
   :leafp
   :rootp
   :top-down-visit
   :bottom-up-visit
   :remove-all-children
   :remove-child
   :remove-child-if
   :do-children
   :do-children-from-end
   :find-child
   :find-child-if))

(defpackage :xmls-utils
  (:use :cl
	:conditions)
  (:export
   :with-tagmatch
   :with-tagmatch-if-else
   :with-attribute))

(defpackage :color-utils
  (:use :cl
	:constants
	:ivec4
	:ubvec4
	:vec4
	:num
	:misc)
  (:export
   :float->byte
   :byte->float
   :int->bytes
   :int->vec4
   :byte-vector->vec4
   :vec4->byte-vector
   :vec4->ubvec4
   :random-mixed-color
   :hsv->rgb
   :hsv->rgb*
   :rgb->hsv*
   :mix-color
   :subtract-color
   :multiply-color
   :maximum-color
   :sum-color
   :gradient-color
   :make-gradient-color
   :intensity
   :color
   :gradient
   :pick-color
   :make-gradient
   :+grayscale-gradient+
   :+rainbow-gradient+
   :+standard-sky-sunny-color+
   :+skydome-gradient+))

(defpackage :2d-utils
  (:use :cl
	:constants
	:ivec2
	:ivec4
	:vec2
	:vec4)
  (:export
   :uivec2
   :90deg->rad
   :iaabb2-min-x
   :iaabb2-max-x
   :iaabb2-min-y
   :iaabb2-max-y
   :iaabb2~
   :valid-iaabb2
   :expand-iaabb2
   :union-iaabb2
   :iaabb2->irect2
   :irect2->iaabb2
   :inside-iaabb2-p
   :iaabb2-intersect-p
   :iaabb2-inglobe-p
   :iaabb2-null-p
   :trasl-iaabb2
   :trasl-irect2
   :rotate-iaabb2*
   :rotate-iaabb2
   :center-iaabb2
   :iaabb2-safe-random
   :random-sub-iaabb2
   :random-sub-irect2
   :aabb2-min-x
   :aabb2-max-x
   :aabb2-min-y
   :aabb2-max-y
   :aabb2~
   :valid-aabb2-p
   :expand-aabb2
   :nexpand-aabb2
   :union-aabb2
   :aabb2->rect2
   :rect2->aabb2
   :inside-aabb2-p
   :aabb2-intersect-p
   :aabb2-inglobe-p
   :approx-aabb2-intersect-p
   :aabb2-null-p
   :trasl-aabb2
   :trasl-rect2
   :rotate-aabb2*
   :rotate-aabb
   :scale-aabb2
   :center-aabb2
   :aabb-safe-random
   :random-sub-aabb
   :random-sub-rect
   :line-eqn
   :recursive-bezier
   :2d-vector-map
   :2d-vector-list-map
   :2d-vector-list-scale
   :2d-vector-list-translate
   :2d-vector-list-rotate
   :2d-vector-sum
   :2d-vector-diff
   :d2d-vector-diff
   :2d-vector-dot-product
   :d2d-vector-dot-product
   :2d-vector-cross-product
   :2d-vector-scale
   :2d-vector-translate
   :2d-vector-rotate
   :2d-vector-magn
   :2d-vector-normalize
   :2d-vector-angle
   :xy->pair
   :pair->interleaved-xy
   :xy->interleaved-xy
   :interleaved-xy->pair))

(defpackage :sb-cga-utils
  (:use :cl
	:sb-cga
	:vec4
	:constants
	:interfaces
	:num-utils
	:misc)
  (:nicknames :3d-utils)
  (:export
   :extract-traslation-vec
   :extract-traslation-mat
   :vec-negate
   :safe-normalize
   :aabb
   :aabb-p1
   :aabb-p2
   :expand
   :insidep
   :flatten-to-aabb2-xz
   :reset
   :aabb-center
   :aabb-top-center
   :aabb-height
   :aabb-width
   :min-x
   :min-y
   :min-z
   :max-x
   :max-y
   :max-z
   :bounding-sphere
   :sphere-center
   :sphere-radius
   :aabb->bounding-sphere
   :triangle-normal
   :triangle-centroid
   :cone
   :cone-apex
   :half-angle
   :cone-height
   :point-in-cone-p
   :ray
   :ray-direction
   :displacement
   :ray-ends
   :tangent-TBN
   :tangent-in-normal-space
   :ccw-poly-fannify
   :plane-equation
   :plane-equation-as-vec4
   :same-plane-p
   :same-plane-p*
   :vector-plane-intersection
   :plane-point-same-side-p
   :extract-frustum-plane
   :3-planes-intersection
   :vec-average
   :vec-average*
   :clone-matrix
   :transform-vec4
   :+projective-scale-bias+
   :ortho
   :ortho*
   :perspective
   :perspective-fov
   :infinite-perspective
   :frustum
   :project
   :unproject
   :look@
   :look@*))

(defpackage :identificable
  (:use :cl
	:constants
	:config
	:interfaces)
  (:export
   :*clone-id*
   :*entity-id-counter*
   :valid-id-p
   :identificable
   :id))

(defpackage :entity
  (:use :cl
	:alexandria
	:constants
	:config
	:sb-cga
	:sb-cga-utils
	:interfaces
	:identificable)
  (:shadowing-import-from :sb-cga :rotate)
  (:export
   :entity
   :modified
   :pos
   :dir
   :scaling
   :up
   :ghost
   :state
   :aabb-2d
   :find-entity-by-id
   :remove-entity-by-id
   :remove-entity-if
   :entity-dead-p))

(defpackage :bs-tree
  (:use
   :cl
   :interfaces)
  (:shadow :search :map)
  (:export
   :node
   :parent
   :data
   :left
   :right
   :make-node
   :make-leaf
   :make-root-node
   :%key
   :+data+
   :+left+
   :+right+
   :+parent+
   :render-tree
   :node->string
   :search
   :search-opt
   :with-insert-local-function
   :insert
   :leafp
   :map
   :map-node
   :walk
   :bstp
   :node->dot
   :reconstruct-parent
   :to-sexp
   :from-sexp))

(defpackage :rb-tree
  (:use
   :cl
   :interfaces
   :bs-tree)
  (:shadowing-import-from :bs-tree :search :map)
  (:export
   :rb-node
   :color
   :make-rb-node
   :make-rb-leaf
   :make-root-rb-node
   :data
   :left
   :right
   :node->string
   :search
   :search-opt
   :with-insert-local-function
   :insert
   :leafp
   :map
   :map-node
   :walk
   :bstp
   :node->dot
   :reconstruct-parent
   :to-sexp
   :from-sexp))

(defpackage :kd-tree
  (:use
   :cl
   :sb-cga
   :sb-cga-utils
   :bs-tree
   :interfaces)
  (:shadow :search :map)
  (:export
   :kd-tree-node
   :split-plane
   :maximum-split-plane
   :3d-query-range
   :p3d-query-range
   :make-kd-node
   :make-kd-leaf
   :make-root-kd-node
   :data
   :left
   :right
   :node->string
   :insert
   :leafp
   :node->dot))

(defpackage :quad-tree
  (:use
   :cl
   :num-utils
   :vec4
   :vec2
   :2d-utils
   :identificable
   :entity)
  ;;(:shadowing-import-from :num-utils epsilon=)
  (:import-from :bs-tree :leafp)
  (:export
   :quad-tree
   :parent
   :aabb
   :nw
   :ne
   :sw
   :se
   :data
   :make-leaf-quad-tree
   :subdivide
   :query-smallest-intersect-aabb
   :query-leaf-in-point
   :calculate-subaabb
   :iterate-nodes-intersect
   :iterate-nodes
   :node-quadrant
   :path-to
   :push-down
   :query-aabb2-intersect-p
   :quad-sizes->level))

(defpackage :priority-queue
  (:use :cl)
  (:nicknames :pq)
  (:export
   :priority-queue
   :key-function
   :compare-function
   :equal-function
   :push-element
   :pop-element
   :find-element
   :emptyp
   :with-min-queue))

(defpackage :queue
  (:use :cl)
  (:nicknames :qu)
  (:shadow :push :pop :find)
  (:export
   :*equal-function*
   :*key-function*
   :push
   :pop
   :find
   :emptyp
   :with-queue))

(defpackage :stack
  (:use :cl)
  (:nicknames :st)
  (:shadow :push :pop :find)
  (:export
   :*equal-function*
   :*key-function*
   :push
   :pop
   :find
   :emptyp
   :with-stack))

(defpackage :matrix
  (:use :cl
	:constants
	:interfaces
	:vec2
	:ivec2
	:vec4
	:ivec4
	:ubvec4
	:num)
  (:export
   :matrix
   :matrix-ubvec4
   :width
   :height
   :data
   :element-type
   :with-matrix-op-typecase
   :row->sequence
   :valid-index-p
   :matrix-elt
   :matrix-elt-vec4
   :matrix-elt-ubvec4
   :sample@
   :pixel@
   :matrix-elt*
   :swap-elements
   :map-matrix
   :nmap-matrix-xy
   :clone
   :loop-matrix
   :loop-submatrix
   :ploop-submatrix
   :ploop-matrix
   :gen-matrix-frame
   :make-matrix
   :make-matrix*
   :make-matrix-with-trim
   :define-matrix
   :gen-neighbour-position
   :gen-4-neighbour-counterclockwise
   :gen-neighbour-position-in-box
   :with-check-borders
   :with-check-matrix-borders
   :with-check-matrix-borders-then-else
   :matrix-incf
   :copy-matrix
   :submatrix
   :data-as-list
   :bilinear-interpolation
   :repeat-periodic-coord
   :confine-coord
   :interpolate
   :rotate-matrix
   :rotate-matrix-w-repeat
   :scale-matrix
   :scale-matrix-nearest
   :matrix-mult
   :h-mirror-matrix
   :matrix-hline
   :matrix-vline
   :matrix-rect
   :pixel-inside-p
   :element@-inside-p
   :good-aabb-start
   :flood-fill-tolerance-p-fn
   :flood-fill
   :flood-fill*
   :kernel-+
   :kernel-*
   :apply-kernel
   :papply-kernel
   :gaussian-blur
   :gaussian-blur-separated
   :pgaussian-blur-separated
   :psobel-edge-detection
   :pgradient-image
   :gradient-image
   :pmatrix-blit
   :pblit-matrix
   :blit-matrix
   :matrix=
   :submatrix=))

(defpackage :graph
  (:use :cl
	:interfaces)
  (:export
   :graph
   :tile-based-graph
   :tile-multilayers-graph
   :layers
   :make-tile-multilayer-graph
   :matrix->graph
   :matrix-graph
   :matrix
   :list-graph
   :*cumulative-cost-plus-heuristic*
   :heuristic-manhattam
   :astar-search
   :dijkstra-search
   :graph->path
   :get-first-near
   :get-first-near-as-id
   :traverse-cost
   :delete-arc
   :delete-all-arcs
   :add-arc
   :add-node
   :node->node-id
   :node-id->node))

(defpackage :kanren-utils
  (:use :cl
	:alexandria
	:kanren-trs)
  (:export
   :facts))

(defpackage :euler
  (:use :cl
	:constants
	:num-utils
	:sb-cga)
  (:export
   :define-force-single
   :define-force
   :status
   :current-pos
   :velocity
   :acceleration
   :force
   :mass
   :time
   :dt
   :integrate))

(defpackage :interpolation
  (:use :cl
	:constants
	:num-utils
	:matrix)
  (:import-from :sb-cga :vec :copy-vec :alloc-vec :vec+ :vec- :vec/)
  (:export
   :catmul-roll-interpolation
   :catmul-roll-interpolation*))

(defpackage :buffered-input-file
  (:use :cl :text-utils)
  (:export
   :make-buffer
   :buffered-input-file
   :line-mode
   :filename
   :logical-file-position
   :close-file
   :regex-scan
   :regex-scan-line-mode
   :regex-scan-line-simple
   :get-line
   :get-char
   :unget-char
   :increment-pointer
   :decrement-pointer
   :seek))

(defpackage :parser
  (:use :cl :buffered-input-file)
  (:export
   :*file*
   :*has-errors*
   :*parsing-errors*
   :*blank-space*
   :push-errors
   :parsed-file
   :comment-line
   :peek-token
   :peek-token*
   :peek-token-suppress-errors
   :parse-comment-line
   :is-comment-line-p
   :define-parser-skeleton
   :define-parser-skeleton*
   :define-is-stuff-p
   :let-noerr
   :let-noerr*
   :next-token
   :next-token-simple
   :with-error
   :with-no-errors
   :with-no-errors*
   :with-valid-stream
   :char@
   :char@1+
   :1+char@
   :peek-end-stream
   :peek-valid-stream
   :define-tokenizer
   :define-tokenizer-simple
   :predicate-sort-tokens
   :hook-to-stringpos
   :defnocfun))

(defpackage :noise
  (:use :cl
	:constants
	:vec2
	:vec4
	:sb-cga
	:num-utils
	:misc-utils)
  (:export
   :*max-dist-to-store*
   :euclidean-distance
   :all-cubes-2d
   :all-cubes-3d
   :all-cubes-4d
   :worley-2d
   :clear-cache-worley-2d
   :with-clear-cache-worley-2d
   :worley-3d
   :with-clear-cache-worley-3d
   :worley-4d
   :with-clear-cache-worley-2d
   :worley-2d-seamless
   :with-clear-cache-worley-2d-seamless
   :*perlin-gradient-random-offset*
   :perlin-2d
   :perlin-3d
   :perlin-4d
   :perlin-2d-seamless
   :gen-fbm
   :gen-fbm-seamless
   :gen-abs-fbm-seamless
   :gen-abs-fbm
   :gen-abs-fbm3
   :with-random-perlin-gradient-offset))

(defpackage :pixmap
  (:use :cl
	:constants
	:interfaces
	:vec2
	:vec4
	:ivec4
	:ubvec4
	:num-utils
	:color-utils
	:3d-utils
	:2d-utils
	:matrix
	:noise)
  (:import-from :sb-cga :vec :copy-vec :alloc-vec :vec+ :vec- :vec/)
  (:shadow :load)
  (:export
   :+red-channel+
   :+green-channel+
   :+blue-channel+
   :+alpha-channel+
   :make-pixmap-frame
   :make-pixmap-template
   :make-pixmap
   :slurp-pixmap
   :matrix->pixmap
   :pixmap
   :width
   :height
   :depth
   :data
   :bits
   :sync-data-to-bits
   :sync-bits-to-data
   :voronoize
   :nrmse
   :dhash
   :to-grayscale
   :ncopy-matrix-into-pixmap
   :save-pixmap
   :blit
   :clone
   :with-rgba-texture
   :with-noise-rgba-texture
   :with-draw-normalizated-coord
   :with-draw-float-normalizated-coord
   :with-draw-normalizated-coord-square
   :with-draw-float-normalizated-coord-square
   :pixmap->ppm
   :pixmap->tga-file
   :pixmap->matrix
   :pixmap-file
   :magic-number
   :errors
   :load
   :load-from-stream
   :load-from-vector
   :ppm
   :pgm
   :max-color
   :tga
   :+targa-stream-element-type+
   :rowbool->rowpixel
   :rowgray->rowpgm
   :rowcolor->rowppm
   :matrix->pbm
   :matrix->pgm
   :matrix->ppm
   :matrix->ppm*
   :gen-normal-map
   :tileize
   :guess-correct-replace-fn
   ;; procedural texture
   :+default-size-pixmap-library+
   :with-random-perlin-gradient-offset
   :with-draw-normalizated-coord-square
   :smoke-tray
   :brick-wall
   :wood-log-texture
   :wood-wall
   :wood-2
   :wood-3
   :wood-4
   :sand
   :snow
   :rock-1
   :rock-layers
   :dry-stone-wall
   :sun-daylight
   :sun-sunset
   :sun-cheap
   :moon
   :half-moon
   :crescent-moon
   :stone-floor
   :stone-floor-fancy
   :grass
   :stone-floor-road
   :dry-soil
   :soil
   :blood-splat
   :voronoized-starfish
   :voronoized-graal
   :glass-tile
   :starfish
   :grass-stones-floor
   :clouds
   :gen-bg-sky-colors
   :skydome))

;; procedural content

(defpackage :random-labyrinth
  (:use :cl
	:constants
	:num-utils
	:2d-utils
	:ivec4)
  (:export
   :w
   :h
   :door-n-p
   :door-s-p
   :door-e-p
   :door-w-p
   :wallp
   :furniturep
   :furniture-wall-decoration-p
   :furniture-walkable-p
   :furniture-chair-p
   :furniture-table-p
   :furniture-fountain-p
   :furniture-pillar-p
   :furniture-other-p
   :doorp
   :windowp
   :invalicablep
   :occupied-rate
   :generate
   :gen-simple-room
   :shared-matrix
   :whole-aabb
   :clear-mat
   :room->mat
   :dump
   :get-root
   :clean-and-redraw-mat))

(defpackage :random-terrain
  (:use :cl
	:config
	:constants
	:num-utils
	:vec2
	:ivec2
	:vec4
	:ubvec4
	:2d-utils
	:interfaces
	:matrix
	:pixmap)
  (:shadowing-import-from :pixmap :load)
  (:export
   :random-terrain
   :serialize
   :deserialize
   :aabb
   :make-map
   :matrix
   :texture-weights
   :cost-matrix
   :labyrinths-aabb
   :labyrinths
   :lakes-aabb
   :roads
   :trees
   :build-texture-weights
   :gen-empty-terrain
   :default-lake-size-function
   :default-labyrinth-size-function
   :default-mountain-z-height-function
   :radial-mountain-z-height-function
   :default-mountain-size-function
   :default-mountain-sigma-function
   :default-labyrinth-sigma-w-function
   :default-labyrinth-sigma-h-function
   :default-labyrinth-door-function
   :default-labyrinth-win-function
   :default-labyrinth-furniture-function
   :get-cost-insecure
   :get-cost))

(defpackage :random-names
  (:use :cl
	:constants
	:mtree-utils
	:buffered-input-file
	:parser)
  (:export
   :load-db
   :load-db*
   :generate))

(defpackage :avatar-portrait
    (:use :cl
	  :cl-ppcre
	  :config
	  :constants
	  :mtree-utils)
    (:export
     :build-avatar))

;; rendering

(defpackage :cl-gl-utils
  (:use :cl
	:cl-opengl
	:misc
	:num)
  (:nicknames :gl-utils)
  (:export
   :+transform-matrix-cointainer+
   :with-unbind-vao
   :with-no-cull-face
   :with-clip-plane
   :mock-null-pointer
   :fast-glaref
   :seq->gl-array
   :copy-gl-array
   :gl-array->list
   :lerp-gl-array
   :render-to-memory-texture
   :with-render-to-file
   :with-render-to-pixmap
   :pick-position))

(defpackage :shaders-utils
  (:use :cl
	:sdl2.kit
	:constants)
  (:export
   :+attribute-position-location+
   :+attribute-normal-location+
   :+attribute-tangent-location+
   :+attribute-texture-location+
   :+attribute-pick-pos-location+
   ;; for terrain decals (roads etc...)
   :+attribute-texture-decals-location+
   :+attribute-pick-weight-location+
   :+texture-unit-diffuse+
   :+texture-unit-normalmap+
   :+texture-unit-projector+
   :+texture-unit-clouds-1+
   :+texture-unit-clouds-2+
   :+texture-unit-clouds-3+
   :*shaders-library*
   :compile-and-check-shader
   :compile-and-link-program
   :program
   :shader-dictionary
   :find-program
   :find-uniform
   :compile-shader-dictionary
   :use-program
   :uniformi
   :uniformf
   :uniformfv
   :uniform-matrix
   :get-shader-source
   :compile-library))

(defpackage :map-utils
  (:use :cl
	:alexandria
	:constants
	:config
	:misc
	:ivec2
	:vec2)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :facingp
   :pos-entity-chunk->cost-pos))

(defpackage :transformable
  (:use :cl)
  (:import-from :interfaces :clone :clone-into)
  (:export
   :transformable
   :build-projection-matrix
   :projection-matrix
   :model-matrix
   :view-matrix))

(defpackage :mesh-material
  (:use :cl)
  (:import-from :interfaces :clone :clone-into)
  (:export
   :mesh-material
   :ka
   :kd
   :ks
   :roughness
   :shininess
   :make-mesh-material))

(defpackage :texture
  (:use :cl
	:config
	:constants
	:interfaces
	:matrix
	:pixmap
	:color-utils
	:mesh-material)
  (:shadowing-import-from :pixmap :load)
  (:export
   :clean-db
   :+skydome-bg+
   :+cloud-1+
   :+cloud-2+
   :+cloud-3+
   :+smoke-tray+
   :+brick-wall+
   :+dry-stone-wall+
   :+grass-stones-floor+
   :+stone-floor-road+
   :+soil+
   :+dry-soil+
   :+wood-wall+
   :+luxurious-floor-1+
   :+luxurious-floor-2+
   :+decal-wall-1+
   :+decal-wall-2+
   :+blood-splat+
   :+rock-1+
   :+rock-2+
   :+sand+
   :+grass+
   :+snow+
   :+texture-db-floor-level-1+
   :+texture-db-floor-level-2+
   :+texture-db-floor-level-3+
   :+texture-db-wall-level-1+
   :+texture-db-wall-level-2+
   :+texture-db-wall-level-3+
   :+texture-db-int-decal+
   :+texture-db-ext-decal+
   :+texture-db-water-terrain+
   :+texture-db-shore-terrain+
   :+texture-db-grass-terrain+
   :+texture-db-snow-terrain+
   :+texture-db-soil-terrain-level-1+
   :+texture-db-soil-terrain-level-2+
   :+texture-db-soil-terrain-level-3+
   :+texture-tag-home-terrain+
   :+texture-tag-wall-level-1+
   :+texture-tag-wall-level-2+
   :+texture-tag-wall-level-3+
   :+texture-tag-ceil-level-1+
   :+texture-tag-ceil-level-2+
   :+texture-tag-ceil-level-3+
   :+texture-tag-door-level-1+
   :+texture-tag-door-level-2+
   :+texture-tag-door-level-3+
   :+texture-tag-floor-level-1+
   :+texture-tag-floor-level-2+
   :+texture-tag-floor-level-3+
   :+texture-tag-int-decal+
   :+texture-tag-ext-decal+
   :+texture-tag-soil-decal+
   :+texture-tag-road-decal+
   :+texture-tag-building-decal+
   :+texture-tag-home-terrain+
   :+texture-tag-shore-terrain+
   :+texture-tag-grass-terrain+
   :+texture-tag-snow-terrain+
   :+texture-tag-soil-terrain-level-1+
   :+texture-tag-soil-terrain-level-2+
   :+texture-tag-soil-terrain-level-3+
   :texture
   :free-memory
   :interpolation-type
   :use-mipmap
   :tags
   :normalmap-params
   :handle
   :filename
   :s-wrap-mode
   :t-wrap-mode
   :border-color
   :get-texture
   :list-of-texture-by-tag
   :gen-name-and-inject-in-database
   :gen-name
   :clone
   :prepare-for-rendering
   :setup-texture-parameters
   :n-ka
   :n-kd
   :n-ks
   :n-roughness
   :n-shininess
   :bind-texture
   :unbind-texture))

;; game

(defpackage :level-config
  (:use :cl
	:cl-ppcre
	:constants
	:num
	:misc
	:resource-cache
	:random-terrain)
  (:export
   :+available-level-wall+
   :+available-level-floor+
   :+available-level-door+
   :+available-level-ceil+
   :*main-window*
   :*renderer*
   :*raw-seed*
   :*game-hour*
   :*map*
   :*building-level*
   :*trees*
   :*wall*
   :*ceiling*
   :*floor*
   :*furnitures*
   :*containers-furnitures*
   :*magic-furnitures*
   :*pillar-furnitures*
   :*chair-furnitures*
   :*table-furnitures*
   :*walkable-furnitures*
   :*wall-decoration-furnitures*
   :*window*
   :*door-n*
   :*door-s*
   :*door-e*
   :*door-w*
   :*chair-n*
   :*chair-s*
   :*chair-e*
   :*chair-w*
   :update-progress
   :clean-global-wars
   :gen-normalmap-if-needed))

(defpackage :game-state
  (:use :cl
	:config
	:constants
	:num
	:vec4
	:matrix
	:interfaces
	:identificable
	:entity
	:level-config
	:random-terrain)
  (:export
   :+empty-type+
   :+unknown-type+
   :+door-n-type+
   :+door-s-type+
   :+door-w-type+
   :+door-e-type+
   :+wall-type+
   :+tree-type+
   :+furniture-type+
   :+magic-furniture-type+
   :+container-type+
   :+pillar-type+
   :+chair-type+
   :+table-type+
   :+walkable-type+
   :+wall-decoration-type+
   :+npc-type+
   :+pc-type+
   :+floor-type+
   :+ceiling-type+
   :movement-path
   :tiles
   :cost
   :make-movement-path
   :invalid-entity-id-map-state
   :map-state-element
   :map-element-empty-p
   :entity-id
   :el-type
   :occlude
   :occludep
   :game-state
   :game-hour
   :game-minutes
   :game-turn
   :current-time
   :sky-bg-color
   :celestial-body-position
   :movement-costs
   :costs-from-map
   :costs-from-players
   :map-state
   :old-state-element
   :all-entities
   :labyrinth-entities
   :player-entities
   :ai-entities
   :level-difficult
   :map-cache-dir
   :window-id
   :light-color
   :fog-density
   :fetch-window
   :with-world
   :fetch-world
   :setup-game-hour
   :prepare-map-state
   :get-cost
   :get-cost-insecure
   :el-type-in-pos
   :entity-id-in-pos
   :occludep-in-pos
   :selected-pc
   :selected-path
   :build-movement-path
   :terrain-aabb-2d
   :find-entity-by-id
   :map-level
   :push-entity
   :push-labyrinth-entity
   :find-labyrinth-by-id
   :add-to-player-entities
   :add-to-ai-entities
   :fetch-from-player-entities
   :fetch-from-ai-entities
   :map-player-entities
   :map-ai-entities
   :faction-player-p
   :faction-ai-p
   :approx-terrain-height@pos
   :place-player-on-map
   :set-invalicable-cost-player-layer@
   :set-invalicable-cost-map-layer@
   :set-minimum-cost-map-layer@
   :set-minimum-cost-player-layer@
   :set-map-state-type
   :set-map-state-id
   :set-map-state-occlusion
   :setup-map-state-entity
   :move-map-state-entity
   :get-neighborhood
   :neighborhood-by-type
   :path-same-ends-p
   :turn-on-fog
   :turn-off-fog))

(defpackage :game-event
  (:use
   :cl
   :alexandria
   :misc
   :num)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :clean-all-events-vectors
   :generic-game-event
   :id-origin
   :age
   :name
   :priority
   :event-data
   :game-event-w-destination
   :make-simple-event-w-dest
   :id-destination
   :game-event-procrastinated
   :trigger-turn
   :on-game-event
   :end-turn
   :end-turn-count
   :register-for-end-turn
   :unregister-for-end-turn
   :propagate-end-turn
   :update-visibility
   :register-for-update-visibility
   :unregister-for-update-visibility
   :propagate-update-visibility
   :send-update-visibility-event
   :camera-drag-ends
   :register-for-camera-drag-ends
   :unregister-for-camera-drag-ends
   :propagate-camera-drag-ends
   :healing-effect-turn
   :register-for-healing-effect-turn
   :unregister-for-healing-effect-turn
   :propagate-healing-effect-turn
   :cancel-healing-effect-game-event
   :move-entity-along-path-event
   :register-for-move-entity-along-path-event
   :unregister-for-move-entity-along-path-event
   :propagate-move-entity-along-path-event
   :move-entity-entered-in-tile-event
   :register-for-move-entity-entered-in-tile-event
   :unregister-for-move-entity-entered-in-tile-event
   :propagate-move-entity-entered-in-tile-event
   :tile-pos
   :move-entity-along-path-end-event
   :path
   :cost
   :register-for-move-entity-along-path-end-event
   :unregister-for-move-entity-along-path-end-event
   :propagate-move-entity-along-path-end-event
   :rotate-entity-cw-event
   :register-for-rotate-entity-cw-event
   :unregister-for-rotate-entity-cw-event
   :propagate-rotate-entity-cw-event
   :rotate-entity-ccw-event
   :register-for-rotate-entity-ccw-event
   :unregister-for-rotate-entity-ccw-event
   :propagate-rotate-entity-ccw-event
   :window-accept-input-event
   :accept-input-p
   :register-for-window-accept-input-event
   :unregister-for-window-accept-input-event
   :propagate-window-accept-input-event
   :refresh-toolbar-event
   :register-for-refresh-toolbar-event
   :unregister-for-refresh-toolbar-event
   :propagate-refresh-toolbar-event
   :send-refresh-toolbar-event
   :update-highlight-path
   :register-for-update-highlight-path
   :unregister-for-update-highlight-path
   :propagate-update-highlight-path
   :open-door-event
   :register-for-open-door-event
   :unregister-for-open-door-event
   :propagate-open-door-event
   :close-door-event
   :register-for-close-door-event
   :unregister-for-close-door-event
   :propagate-close-door-event
   :cause-poisoning-event
   :register-for-cause-poisoning-event
   :unregister-for-cause-poisoning-event
   :propagate-cause-poisoning-event
   :make-cause-poisoning-event
   :cause-terror-event
   :register-for-cause-terror-event
   :unregister-for-cause-terror-event
   :propagate-cause-terror-event
   :make-cause-terror-event
   :cause-berserk-event
   :register-for-cause-berserk-event
   :unregister-for-cause-berserk-event
   :propagate-cause-berserk-event
   :make-cause-berserk-event
   :cause-faint-event
   :register-for-cause-faint-event
   :unregister-for-cause-faint-event
   :propagate-cause-faint-event
   :make-cause-faint-event
   :cure-poisoning-event
   :register-for-cure-poisoning-event
   :unregister-for-cure-poisoning-event
   :propagate-cure-poisoning-event
   :make-cure-poisoning-event
   :cure-terror-event
   :register-for-cure-terror-event
   :unregister-for-cure-terror-event
   :propagate-cure-terror-event
   :make-cure-terror-event
   :cure-faint-event
   :register-for-cure-faint-event
   :unregister-for-cure-faint-event
   :propagate-cure-faint-event
   :make-cure-faint-event
   :cure-berserk-event
   :register-for-cure-berserk-event
   :unregister-for-cure-berserk-event
   :propagate-cure-berserk-event
   :make-cure-berserk-event
   :cancel-poisoning-event
   :register-for-cancel-poisoning-event
   :unregister-for-cancel-poisoning-event
   :propagate-cancel-poisoning-event
   :make-cancel-poisoning-event
   :cancel-terror-event
   :register-for-cancel-terror-event
   :unregister-for-cancel-terror-event
   :propagate-cancel-terror-event
   :make-cancel-terror-event
   :cancel-faint-event
   :register-for-cancel-faint-event
   :unregister-for-cancel-faint-event
   :propagate-cancel-faint-event
   :make-cancel-faint-event
   :cancel-berserk-event
   :register-for-cancel-berserk-event
   :unregister-for-cancel-berserk-event
   :propagate-cancel-berserk-event
   :make-cancel-berserk-event
   :cancel-immune-poisoning-event
   :register-for-cancel-immune-poisoning-event
   :unregister-for-cancel-immune-poisoning-event
   :propagate-cancel-immune-poisoning-event
   :make-cancel-immune-poisoning-event
   :cancel-immune-terror-event
   :register-for-cancel-immune-terror-event
   :unregister-for-cancel-immune-terror-event
   :propagate-cancel-immune-terror-event
   :make-cancel-immune-terror-event
   :cancel-immune-faint-event
   :register-for-cancel-immune-faint-event
   :unregister-for-cancel-immune-faint-event
   :propagate-cancel-immune-faint-event
   :make-cancel-immune-faint-event
   :cancel-immune-berserk-event
   :register-for-cancel-immune-berserk-event
   :unregister-for-cancel-immune-berserk-event
   :propagate-cancel-immune-berserk-event
   :make-cancel-immune-berserk-event
   :heal-damage-event
   :make-heal-damage-event
   :register-for-heal-damage-event
   :unregister-for-heal-damage-event
   :propagate-heal-damage-event
   :wear-object-event
   :register-for-wear-object-event
   :unregister-for-wear-object-event
   :propagate-wear-object-event
   :unwear-object-event
   :register-for-unwear-object-event
   :unregister-for-unwear-object-event
   :propagate-unwear-object-event
   :modifier-object-event
   :register-for-modifier-object-event
   :unregister-for-modifier-object-event
   :propagate-modifier-object-event
   :make-modifier-object-event
   :immune-poisoning-event
   :register-for-immune-poisoning-event
   :unregister-for-immune-poisoning-event
   :propagate-immune-poisoning-event
   :make-immune-poisoning-event
   :immune-terror-event
   :register-for-immune-terror-event
   :unregister-for-immune-terror-event
   :propagate-immune-terror-event
   :make-immune-terror-event
   :immune-faint-event
   :register-for-immune-faint-event
   :unregister-for-immune-faint-event
   :propagate-immune-faint-event
   :make-immune-faint-event
   :immune-berserk-event
   :register-for-immune-berserk-event
   :unregister-for-immune-berserk-event
   :propagate-immune-berserk-event
   :make-immune-berserk-event))

(defpackage :basic-interaction-parameters
  (:use :cl
	:alexandria
	:constants
	:config
	:num
	:interfaces
	:identificable)
  (:nicknames :interaction)
  (:export
   :effect-unlimited-p
   :healing-effect-cure-p
   :+decay-by-use+
   :+decay-by-turns+
   :+effect-when-used+
   :+effect-when-worn+
   :+effect-when-consumed+
   :+effect-until-picked+
   :+effect-until-held+
   :+can-talk+
   :+can-ask-for-help+
   :+can-be-opened+
   :+can-open+
   :+can-attack+
   :+can-be-attacked+
   :+can-intercept-attacks+
   :+can-be-destroyed+
   :+can-be-burned+
   :+can-heal+
   :+can-be-heal+
   :+can-poison+
   :+can-be-poisoned+
   :+can-be-drunk+
   :+can-be-eaten+
   :+can-be-picked+
   :+can-be-worn-arm+
   :+can-be-worn-head+
   :+can-be-worn-neck+
   :+can-be-worn-feet+
   :+can-be-worn-body+
   :+can-be-worn-hand+
   :+can-be-held-in-hand+
   :+can-cut+
   :+can-smash+
   :+can-pierce+
   :+can-launch-bolt+
   :+can-launch-arrow+
   :+mounted-on-pole+
   :+decay+
   :+effects+
   :+strength+
   :+stamina+
   :+dexterity+
   :+agility+
   :+smartness+
   :+empaty+
   :+weight+
   :+damage-point+
   :+movement-point+
   :+magic-point+
   :+dodge-chance+
   :+melee-attack-chance+
   :+range-attack-chance+
   :+melee-attack-damage+
   :+range-attack-damage+
   :+edge-weapons-chance-bonus+
   :+edge-weapons-damage-bonus+
   :+impact-weapons-chance-bonus+
   :+impact-weapons-damage-bonus+
   :+pole-weapons-chance-bonus+
   :+pole-weapons-damage-bonus+
   :+range-weapons-chance-bonus+
   :+range-weapons-damage-bonus+
   :+unlock-chance+
   :+deactivate-trap-chance+
   :+reply-attack-chance+
   :+ambush-attack-chance+
   :+spell-chance+
   :+attack-spell-chance+
   :+healing-effects+
   :+target-self+
   :+target-other+
   :+heal-damage-points+
   :+heal-poison+
   :+heal-berserk+
   :+heal-faint+
   :+heal-terror+
   :+cause-poison+
   :+cause-berserk+
   :+cause-faint+
   :+cause-terror+
   :+immune-poison+
   :+immune-berserk+
   :+immune-faint+
   :+immune-terror+
   :+magic-effects+
   :+duration-unlimited+
   :decay-parameters
   :points
   :effect-parameters
   :modifier
   :trigger
   :duration
   :target
   :chance
   :healing-effect-parameters
   :magic-effect-parameters
   :poison-effect-parameters
   :heal-damage-points-effect-parameters
   :points-per-turn
   :define-interaction
   :define-decay
   :define-effect
   :define-effects
   :define-healing-effects
   :define-healing-effect
   :define-heal-dmg-effect
   :define-magic-effect
   :define-poison-effect
   :define-character
   :define-interaction
   :with-interaction-parameters))

(defpackage :player-messages-text
  (:use :cl
	:config)
  (:export
   :*terror-recover*
   :*berserk-recover*
   :*faint-recover*
   :*cancel-immune-berserk*
   :*cancel-immune-faint*
   :*cancel-immune-terror*
   :*cancel-immune-poisoning*
   :init-player-messages-db))

(defpackage :character
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:game-event)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :+unknown-ability-bonus+
   :+starting-exp-points+
   :+first-name+
   :+last-name+
   :+description+
   :+portrait+
   :+strength+
   :+stamina+
   :+dexterity+
   :+agility+
   :+smartness+
   :+empaty+
   :+weight+
   :+decay+
   :+damage-points+
   :+movement-points+
   :+magic-points+
   :+dodge-chance+
   :+melee-attack-chance+
   :+range-attack-chance+
   :+melee-attack-damage+
   :+range-attack-damage+
   :+edge-weapons-chance-bonus+
   :+edge-weapons-damage-bonus+
   :+impact-weapons-chance-bonus+
   :+impact-weapons-damage-bonus+
   :+pole-weapons-chance-bonus+
   :+pole-weapons-damage-bonus+
   :+unlock-chance+
   :+deactivate-trap-chance+
   :+reply-attack-chance+
   :+ambush-attack-chance+
   :+spell-chance+
   :+attack-spell-chance+
   :+status+
   :+status-poisoned+
   :+status-terror+
   :+status-berserk+
   :+status-faint+
   :+race+
   :+level+
   :+exp-points+
   :np-character
   :clean-effects
   :restart-age
   :description-type
   :player-character
   :portrait
   :first-name
   :last-name
   :model-origin-dir
   :current-path
   :gender
   :player-class
   :strength
   :stamina
   :dexterity
   :agility
   :smartness
   :empaty
   :weight
   :damage-points
   :current-damage-points
   :movement-points
   :current-movement-points
   :magic-points
   :current-magic-points
   :dodge-chance
   :melee-attack-chance
   :range-attack-chance
   :melee-attack-damage
   :range-attack-damage
   :edge-weapons-chance-bonus
   :edge-weapons-damage-bonus
   :impact-weapons-chance-bonus
   :impact-weapons-damage-bonus
   :pole-weapons-chance-bonus
   :pole-weapons-damage-bonus
   :modifiers-effects
   :unlock-chance
   :deactivate-trap-chance
   :reply-attack-chance
   :ambush-attack-chance
   :spell-chance
   :attack-spell-chance
   :actual-damage-points
   :actual-movement-points
   :actual-magic-points
   :actual-dodge-chance
   :actual-melee-attack-chance
   :actual-range-attack-chance
   :actual-melee-attack-damage
   :actual-range-attack-damage
   :actual-edge-weapons-chance-bonus
   :actual-edge-weapons-damage-bonus
   :actual-impact-weapons-chance-bonus
   :actual-impact-weapons-damage-bonus
   :actual-pole-weapons-chance-bonus
   :actual-pole-weapons-damage-bonus
   :actual-unlock-chance
   :actual-deactivate-trap-chance
   :actual-reply-attack-chance
   :actual-ambush-attack-chance
   :actual-spell-chance
   :actual-attack-spell-chance
   :status
   :immune-faint-status
   :immune-berserk-status
   :immune-poison-status
   :immune-terror-status
   :recurrent-effects
   :postponed-messages
   :race
   :level
   :exp-points
   :elm
   :shoes
   :armor
   :left-hand
   :right-hand
   :ring
   :inventory
   :inventory-slot-pages-number
   :player-gender->gender-description
   :reset-movement-points
   :reset-magic-points
   :remove-decayed-items
   :remove-from-inventory
   :remove-from-modifiers
   :add-to-inventory
   :sum-modifiers
   :player-class->class-description
   :basic-interaction-params
   :make-warrior
   :make-wizard
   :make-healer
   :make-archer
   :make-ranger
   :object-keycode
   :can-talk-p
   :can-be-opened-p
   :can-attack-p
   :can-attack-p
   :can-be-destroyed-p
   :can-be-burned-p
   :can-heal-p
   :can-be-heal-p
   :can-be-drunk-p
   :can-be-eaten-p
   :can-be-worn-arm-p
   :can-be-worn-head-p
   :can-be-worn-neck-p
   :can-be-worn-feet-p
   :can-cut-p
   :can-smash-p
   :can-launch-bolt-p
   :can-launch-arrow-p
   :mounted-on-pole-p
   :decayp
   :effectsp
   :healing-effects-p
   :magic-effect-p
   :be-consumed-p
   :potionp
   :fountainp
   :weaponp
   :armorp
   :containerp
   :keyp
   :shieldp
   :shoesp
   :elmp
   :ringp
   :can-be-worn-p
   :interaction-get-strength
   :interaction-get-stamina
   :interaction-get-dexterity
   :interaction-get-agility
   :interaction-get-smartness
   :interaction-get-empaty
   :interaction-get-decay
   :interaction-get-weight
   :interaction-get-damage-points
   :interaction-get-movement-points
   :interaction-get-magic-points
   :interaction-get-dodge-chance
   :interaction-get-melee-attack-chance
   :interaction-get-range-attack-chance
   :interaction-get-melee-attack-damage
   :interaction-get-range-attack-damage
   :interaction-get-edge-weapons-chance-bonus
   :interaction-get-edge-weapons-damage-bonus
   :interaction-get-impact-weapons-chance-bonus
   :interaction-get-impact-weapons-damage-bonus
   :interaction-get-pole-weapons-chance-bonus
   :interaction-get-pole-weapons-damage-bonus
   :interaction-get-unlock-chance
   :interaction-get-deactivate-trap-chance
   :interaction-get-reply-attack-chance
   :interaction-get-ambush-attack-chance
   :interaction-get-spell-chance
   :interaction-get-attack-spell-chance
   :interaction-get-heal-damage-points
   :interaction-get-heal-poison
   :interaction-get-heal-berserk
   :interaction-get-heal-faint
   :interaction-get-heal-terror
   :interaction-get-cause-poison
   :interaction-get-cause-berserk
   :interaction-get-cause-faint
   :interaction-get-cause-terror
   :interaction-get-immune-poison
   :interaction-get-immune-berserk
   :interaction-get-immune-faint
   :interaction-get-immune-terror
   :interaction-get-magic-effect
   :validate-interaction-file
   :%get-effects-shuffled
   :%get-normal-fx-shuffled
   :%get-healing-fx-shuffled
   :%get-magic-fx-shuffled
   :remove-generate-symbols
   :with-character-parameters
   :sum-effects-mod
   :params->player-character
   :calculate-randomized-damage-points
   :params->np-character
   :item->player-character-slot
   :item->available-player-character-slot))

(defpackage :random-armor
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-armor))

(defpackage :random-key
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-key
   :generate-key*))

(defpackage :random-container
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-container))

(defpackage :random-potion
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-potion))

(defpackage :random-shield
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-shield))

(defpackage :random-shoes
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-shoes))

(defpackage :random-weapon
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-weapon))

(defpackage :random-fountain
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-fountain))

(defpackage :random-elm
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-elm))

(defpackage :random-ring
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :generate-ring))

(defpackage :random-inert-object
  (:use :cl
	:alexandria
	:constants
	:config
	:num-utils
	:misc-utils
	:text-utils
	:mtree-utils
	:interfaces
	:identificable
	:basic-interaction-parameters
	:character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :+minimum-level+
   :+maximum-level+
   :generate-inert-object))

(defpackage :random-object-messages
  (:use
   :cl
   :alexandria
   :config
   :config
   :num-utils
   :misc-utils
   :text-utils
   :mtree-utils
   :interfaces
   :identificable
   :game-event
   :basic-interaction-parameters
   :character)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :+poison-turn-arg-key+
   :untrigged-effect-p-fn
   :to-other-target-effect-p-fn
   :heal-damage-msg
   :modifier-effect-msg
   :msg-characteristic
   :msg-object-description
   :msg-points
   :msg-origin
   :msg-modifier
   :cause-poison-msg
   :cause-terror-msg
   :cause-faint-msg
   :cause-berserk-msg
   :cure-poison-msg
   :cure-terror-msg
   :cure-faint-msg
   :cure-berserk-msg
   :immune-poison-msg
   :immune-terror-msg
   :immune-faint-msg
   :immune-berserk-msg
   :cancel-immune-poison-msg
   :cancel-immune-terror-msg
   :cancel-immune-faint-msg
   :cancel-immune-berserk-msg
   :msg-trigger
   :msg-duration
   :msg-chance
   :msg-target
   :msg-damage
   :turn
   :damage
   :effect->msg
   :params->effects-messages
   :params->effects-messages-complement
   :propagate-effects-msg))

;; engine

(defpackage :camera
  (:use :cl
	:constants
	:sb-cga
	:sb-cga-utils
	:vec4
	:vec2
	:num
	:misc
	:interfaces
	:euler
	:quaternion
	:identificable
	:entity
	:transformable)
  (:import-from :game-event :on-game-event)
  (:export
   :camera
   :frustum-aabb
   :frustum-sphere
   :frustum-cone
   :reorient-fp-camera
   :drag-camera
   :drag-camera-to
   :target
   :mode
   :minor-mode
   :pos-interpolator
   :install-drag-interpolator
   :install-path-interpolator
   :install-path-interpolator*
   :install-orbit-interpolator
   :look-at
   :look-at*
   :calculate-frustum
   :calculate-cone
   :calculate-sphere
   :containsp
   :frustum-planes
   :calculate-aabb
   :fit-to-aabb))

(defpackage :mesh
  (:use :cl
	:sb-cga
	:sb-cga-utils
	:config
	:constants
	:conditions
	:misc
	:shaders-utils
	:cl-gl-utils
	:interfaces
	:transformable
	:parser
	:num
	:vec2
	:vec4
	:quaternion
	:uivec
	:2d-utils
	:mtree-utils
	:graph
	:mesh-material
	:identificable
	:entity
	:camera
	:game-state)
  (:shadowing-import-from :graph :matrix)
  (:import-from           :game-event :on-game-event)
  (:export
   :+vbo-count+
   :+vao-count+
   :+aabb-vertex-count+
   :+tag-head-key+
   :+tag-left-weapon-key+
   :+tag-right-weapon-key+
   :triangle-mesh
   :free-memory*
   :load-mesh
   :save-mesh
   :vertex-index
   :normal-index
   :texture-index
   :get-custom-attribute
   :set-custom-attribute
   :with-camera-projection-matrix
   :with-camera-view-matrix
   :with-modelview-matrix
   :with-model-matrix
   :mesh
   :current-time
   :fog-density
   :do-children-mesh
   :do-triangles
   :triangles
   :material-params
   :vertices
   :edges
   :texture-coord
   :normals
   :tangents
   :renderer-data-vertices
   :renderer-data-count-vertices
   :renderer-data-texture
   :renderer-data-count-texture
   :renderer-data-normals
   :renderer-data-count-normals
   :renderer-data-tangents
   :renderer-data-count-tangents
   :renderer-data-normals-obj-space
   :renderer-data-aabb-obj-space
   :prepare-for-rendering-phong
   :prepare-for-rendering-normal-map
   :tags-table
   :tags-matrices
   :tag-key-parent
   :vbo
   :vao
   :vao-vertex-buffer-handle
   :find-index-by-value-from-end
   :find-index-by-value
   :render-normals
   :render-tangents
   :render-aabb
   :aabb
   :reset-aabb
   :renderp
   :use-blending-p
   :bounding-sphere
   :transform-vertices
   :get-material-from-texture
   :parent-mesh
   :children
   :texture-object
   :normal-map
   :texture-projector
   :projector
   :impostor
   :find-all-index-by-value
   :modelview-matrix
   :modelview-stack
   :vertex-v
   :vertex
   :texel-v
   :texel
   :normal-v
   :tangent-v
   :tangent
   :empty-triangles-p
   :empty-vertices-p
   :empty-normals-p
   :normal
   :push-matrix
   :pop-matrix
   :load-matrix
   :mult-matrix
   :make-data-for-opengl
   :make-data-for-opengl-aabb-obj-space
   :prepare-for-rendering
   :rendering-needed-p
   :render
   :render-phong
   :render-normalmap
   :destroy
   :merge-mesh
   :make-shared-vertices
   :fill-gaps-between-meshes
   :vbo-vertex-buffer-handle
   :vbo-normals-buffer-handle
   :vbo-tangents-buffer-handle
   :vbo-texture-buffer-handle
   :vbo-normals-object-space-buffer-handle
   :vbo-aabb-object-space-buffer-handle
   :normals-obj-space-vertex-count
   :average-normals
   :average-normals-by-shared-vertices
   :average-normals-if-near-vertices
   :gen-tangents
   :subdivide-mesh
   :smooth-mesh
   :flatten-mesh
   :manifold-maybe
   :recalculate-face-normals
   :remove-orphaned-vertices
   :find-value-by-index
   :bubbleup-modelmatrix
   :normals-count
   :use-lod-p
   :triangle
   :triangle-mesh-shell
   :tree-mesh-shell
   :tree-trunk-aabb
   :fill-mesh-data
   :fill-shell-from-mesh
   :fill-shell-from-mesh-w-renderer-data
   :load-tag-file
   :load-tags
   :tag->matrix
   :matrix->tag
   :nsetup-tag-matrix
   :find-tag-cdr
   :find-tag
   :traverse-recurrent-effects
   :process-postponed-messages
   :set-death-status
   :skydome
   :texture-clouds
   :texture-smoke
   :weather-type
   :water
   :water-mesh-p
   :cylinder
   :cube
   :parallelepiped
   :quad
   :quad-w-explicit-texture-coords
   :quads-plane
   :gen-ceiling
   :gen-skydome
   :with-pushed-matrix
   :wall-mesh-shell
   :window-mesh-shell
   :decorated-wall-mesh-shell
   :door-mesh-shell
   :instanced-mesh
   :labyrinth-mesh
   :parent-labyrinth
   :wall-instanced
   :window-instanced
   :pillar-instanced
   :door-n-instanced
   :door-s-instanced
   :door-e-instanced
   :door-w-instanced
   :table-instanced
   :chair-n-instanced
   :chair-s-instanced
   :chair-e-instanced
   :chair-w-instanced
   :openp
   :fountain-mesh-shell
   :container-mesh-shell
   :furniture-mesh-shell
   :setup-projective-texture
   :calculate-decrement-move-points-entering-tile
   :decrement-move-points-rotate
   :decrement-move-points-entering-tile
   :decrement-move-points-wear
   :can-use-movement-points-p
   :calculate-cost-position))

(defpackage :able-to-see-mesh
  (:use :cl
	:alexandria
	:sb-cga
	:sb-cga-utils
	:config
	:constants
	:conditions
	:misc
	:shaders-utils
	:cl-gl-utils
	:interfaces
	:transformable
	:num
	:vec2
	:vec4
	:quaternion
	:uivec
	:2d-utils
	:mtree-utils
	:graph
	:mesh-material
	:mesh
	:identificable
	:entity
	:camera
	:game-state)
  (:shadowing-import-from :sb-cga :matrix     :rotate)
  (:shadowing-import-from :misc   :random-elt :shuffle)
  (:export
   :able-to-see-mesh
   :visibility-cone
   :other-visible-p
   :update-visibility-cone
   :visible-players
   :other-visible-p
   :other-visible-cone-p
   :other-visible-ray-p))

(defpackage :pickable-mesh
  (:use :cl
	:alexandria
	:sb-cga
	:sb-cga-utils
	:config
	:constants
	:conditions
	:misc
	:shaders-utils
	:cl-gl-utils
	:interfaces
	:transformable
	:identificable
	:num
	:vec2
	:vec4
	:uivec
	:mtree-utils
	:mesh)
  (:shadowing-import-from :misc   :random-elt :shuffle)
  (:shadowing-import-from :sb-cga :rotate)
  (:shadowing-import-from :2d-utils :uivec2)
  (:export
   :+attribute-pick-overlay+
   :+color-tile-pick-can-move+
   :+color-tile-pick-cannot-move+
   :+pick-color-lerp-weight+
   :null-tile-element
   :out-of-bonds-tile-element
   :pickable-tile
   :triangle-1
   :triangle-2
   :index-tr-1
   :index-tr-2
   :pickable-mesh-p
   :pickable-mesh
   :origin-offset
   :lookup-tile-triangle
   :highligthed-tiles-coords
   :pick-overlay-values
   :renderer-data-count-pick-overlay
   :renderer-data-pick-overlay
   :push-pickable-attribute
   :setup-pickable-attribute
   :push-pickable-attribute
   :setup-lookup-triangle-element
   :setup-lookup-triangle-element*
   :set-tile-highlight
   :turn-off-highligthed-tiles
   :add-highligthed-tiles-coords
   :add-highligthed-tiles-coords*
   :populate-lookup-triangle-matrix
   :lookup-tile-triangle->dbg-matrix
   :vbo-pick-weights-handle
   :cost-coord->lookup-tile
   :lookup-tile-coord->cost
   :init-pick-overlay-value
   :init-pick-overlay-value*))

(defpackage :building-floor-mesh
  (:use :cl
	:sb-cga
	:sb-cga-utils
	:config
	:constants
	:conditions
	:misc
	:shaders-utils
	:cl-gl-utils
	:interfaces
	:transformable
	:identificable
	:num
	:vec2
	:vec4
	:uivec
	:2d-utils
	:mtree-utils
	:mesh
	:mesh-material
	:pickable-mesh)
  (:export
   :building-floor-mesh
   :setup-texture-coord-scaling
   :floor-tile))

(defpackage :battle-utils
  (:use :cl
	:alexandria
	:sb-cga
	:sb-cga-utils
	:config
	:constants
	:conditions
	:misc
	:num
	:die-utils
	:shaders-utils
	:cl-gl-utils
	:interfaces
	:transformable
	:identificable
	:num
	:vec2
	:vec4
	:uivec
	:2d-utils
	:mtree-utils
	:mesh
	:mesh-material
	:pickable-mesh)
  (:shadowing-import-from :misc   :random-elt :shuffle)
  (:shadowing-import-from :sb-cga :rotate)
  (:export
   :+recover-from-faint-dmg-fraction+
   :attack))

;; UI

(defpackage :gui-events
  (:use :cl
	:config
	:constants)
  (:export
   :mouse-button->code
   :mouse-pressed
   :x-event
   :y-event
   :button-event
   :mouse-dragged
   :dx-event
   :dy-event
   :key-pressed
   :char-event))

(defpackage :gui
  (:use :cl
	:config
	:constants
	:cl-i18n
	:vec2
	:vec4
	:num
	:text-utils
	:misc
	:texture
	:mesh)
  (:export
   :clean-font-db
   :+tooltip-font-handle+
   :+window-texture-name+
   :+button-texture-name+
   :+button-pressed-texture-name+
   :+button-cancel-texture-name+
   :+button-ok-texture-name+
   :+window-close-button-texture-name+
   :+window-close-button-pressed-texture-name+
   :+message-16-error-texture-name+
   :+message-16-help-texture-name+
   :+message-16-info-texture-name+
   :+message-16-ok-texture-name+
   :+message-16-warning-texture-name+
   :+frame-texture-name+
   :+window-top-bar-texture-name+
   :+text-field-texture-name+
   :+text-field-focused-texture-name+
   :+text-field-overlay-texture-name+
   :+square-button-texture-name+
   :+square-button-pressed-texture-name+
   :+inventory-slot-selected-texture-name+
   :+inventory-slot-texture-name+
   :+chest-closed-texture-name+
   :+chest-opened-texture-name+
   :+transparent-texture-name+
   :+blue-h-bar+
   :+red-h-bar+
   :+green-h-bar+
   :+check-button-texture-name+
   :+check-button-checked-green+
   :+check-button-overlay+
   :+save-overlay-texture-name+
   :+load-overlay-texture-name+
   :+up-arrow-overlay-texture-name+
   :+down-arrow-overlay-texture-name+
   :+left-overlay-texture-name+
   :+right-overlay-texture-name+
   :+wear-overlay-texture-name+
   :+use-overlay-texture-name+
   :+use-item-overlay-texture-name+
   :+plus-overlay-texture-name+
   :+minus-overlay-texture-name+
   :+up-overlay-texture-name+
   :+down-overlay-texture-name+
   :+option-overlay-texture-name+
   :+quit-overlay-texture-name+
   :+next-overlay-texture-name+
   :+previous-overlay-texture-name+
   :+rotate-char-cw-overlay-texture-name+
   :+rotate-char-ccw-overlay-texture-name+
   :+next-turn-overlay-texture-name+
   :+move-overlay-texture-name+
   :+open-overlay-texture-name+
   :+close-overlay-texture-name+
   :+zoom-overlay-texture-name+
   :+unzoom-overlay-texture-name+
   :+portrait-unknown-texture-name+
   :+preview-unknown-texture-name+
   :+silhouette-texture-name+
   :+bag-texture-name+
   :+add-to-bag-texture-name+
   :+berserk-texture-name+
   :+coma-texture-name+
   :+terror-texture-name+
   :+poison-texture-name+
   :+immune-berserk-texture-name+
   :+immune-coma-texture-name+
   :+immune-terror-texture-name+
   :+immune-poison-texture-name+
   :+conversation-overlay-texture-name+
   :+attack-short-range-overlay-texture-name+
   :+attack-long-range-overlay-texture-name+
   :+attack-long-range-imprecise-overlay-texture-name+
   :+magic-staff-overlay-texture-name+
   :+default-font+
   :+default-font-handle+
   :join-lines-for-static-text
   :join-lines-for-static-text*
   :gui-printable-p
   :get-font
   :get-char-mesh
   :load-font
   :setup-gui))

(defpackage :widget
  (:use :cl
	:config
	:constants
	:num
	:misc
	:mtree-utils
	:text-utils
	:filesystem-utils
	:shaders-utils
	:cl-gl-utils
	:interfaces
	:sb-cga-utils
	:2d-utils
	:vec2
	:vec4
	:texture
	:mesh-material
	:identificable
	:transformable
	:entity
	:camera
	:game-state
	:mesh
	:random-object-messages
	:character
	:gui-events
	:gui)
  (:export
   :+action-move+
   :widget
   :x
   :y
   :width
   :height
   :label
   :hide
   :on-mouse-pressed
   :on-mouse-released
   :on-mouse-dragged
   :on-key-pressed
   :flip-y
   :hide-parent-cb
   :hide-and-remove-parent-cb
   :naked-button
   :button
   :text-field
   :check-button
   :signalling-light
   :button-state
   :flip-state
   :common-setup-label
   :simple-label
   :static-text
   :h-bar
   :fill-level
   :window
   :labeled-check-button
   :file-chooser
   :b-ok
   :b-cancel
   :make-file-chooser
   :main-toolbar
   :selected-action
   :bound-world
   :bound-player
   :sync-with-player
   :reset-toolbar-selected-action
   :text-communication
   :text-fps
   :b-save
   :b-load
   :player-generator
   :make-player-generator
   :inventory-window
   :make-inventory-window
   :player-report
   :make-player-report-win
   :message-window
   :make-message-box
   :make-message-box*
   :progress-gauge
   :progress
   :make-splash-progress-gauge))

(defpackage :billboard
  (:use :cl
	:alexandria
	:sb-cga
	:config
	:constants
	:num
	:misc
	:mtree-utils
	:shaders-utils
	:interfaces
	:sb-cga-utils
	:vec2
	:vec4
	:texture
	:identificable
	:transformable
	:entity
	:camera
	:game-state
	:mesh
	:gui-events
	:gui
	:widget)
  (:shadowing-import-from :sb-cga :rotate)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :+tooltip-w+
   :+tooltip-h+
   :+damage-color+
   :+poison-damage-color+
   :+healing-color+
   :+blessing-color+
   :+tooltip-poison-char+
   :+tooltip-terror-char+
   :+tooltip-berserk-char+
   :+tooltip-faint-char+
   :+tooltip-immune-faint-char+
   :+tooltip-immune-poison-char+
   :+tooltip-immune-terror-char+
   :+tooltip-immune-berserk-char+
   :+tooltip-heal-char+
   :+tooltip-revive-char+
   :tooltip
   :duration
   :make-tooltip
   :apply-tooltip
   :tree-impostor-shell
   :make-impostor-pixmap
   :make-impostor-texture
   :make-impostor-mesh))

(defpackage :world
  (:use :cl
	:config
	:constants
	:mtree-utils
	:sb-cga
	:sb-cga-utils
	:cl-gl-utils
	:num
	:identificable
	:entity
	:game-state
	:transformable
	:interfaces
	:character
	:camera
	:mesh
	:pickable-mesh)
  (:import-from :widget :reset-toolbar-selected-action)
  (:export
   :doors
   :door-n
   :door-s
   :door-e
   :door-w
   :chairs
   :chair-n
   :chair-s
   :chair-e
   :chair-w
   :world
   :entities
   :main-state
   :camera
   :frame-window
   :skydome
   :state
   :trees-bag
   :walls-bag
   :doors-bag
   :ceiling-bag
   :floor-bag
   :furnitures-bag
   :containers-bag
   :magic-furnitures-bag
   :pillars-bag
   :tables-bag
   :chairs-bag
   :wall-decorations-bag
   :walkable-bag
   :windows-bag
   :gui
   :toolbar
   :render-for-reflection
   :push-entity
   :get-window-size
   :main-light-pos
   :main-light-color
   :initialize-skydome
   :cone-aabb-intersects-p
   :frustum-aabb-intersects-p
   :render-gui
   :highlight-tile-screenspace
   :highlight-path-costs-space
   :pick-player-entity
   :pick-pointer-position
   :pick-height-terrain
   :all-furniture-bags-not-empty-p
   :all-furnitures-but-pillars-not-empty-p
   :push-labyrinth-entity
   :find-labyrinth-by-id
   :push-interactive-entity
   :set-map-state-type
   :set-map-state-id
   :set-map-state-occlusion
   :setup-map-state-entity
   :move-map-state-entity
   :move-entity
   :toolbar-selected-action
   :reset-toolbar-selected-action
   :post-entity-message
   :setup-map-state-tile
   :place-player-on-map
   :set-window-accept-input
   :post-entity-message
   :point-to-entity-and-hide-cb
   :point-camera-to-entity))

(defpackage :terrain-chunk
  (:use :cl
	:alexandria
	:config
	:constants
	:sb-cga
	:sb-cga-utils
	:shaders-utils
	:cl-gl-utils
	:interfaces
	:transformable
	:identificable
	:parser
	:num-utils
	:misc-utils
	:vec2
	:ivec2
	:vec4
	:uivec
	:2d-utils
	:camera
	:entity
	:game-state
	:mesh
	:pickable-mesh)
  (:shadowing-import-from :sb-cga :matrix     :rotate)
  (:shadowing-import-from :misc   :random-elt :shuffle)
  (:export
   :+pick-color-lerp-weight+
   :terrain-chunk
   :heightmap
   :decal-weights
   :build-mesh-dbg
   :build-mesh
   :nclip-with-aabb
   :clip-with-aabb
   :approx-terrain-height
   :make-terrain-chunk))

(defpackage :particles
  (:use :cl
	:constants
	:interfaces
	:parser
	:sb-cga
	:sb-cga-utils
	:num-utils
	:mesh)
  (:export))

(defpackage :md2-mesh
  (:nicknames :md2)
  (:shadow :load)
  (:use :cl
	:sb-cga
	:sb-cga-utils
	:constants
	:config
	:conditions
	:interfaces
	:transformable
	:identificable
	:entity
	:num
	:game-event
	:misc
	:vec2
	:ivec2
	:player-messages-text
	:character
	:mesh-material
	:mesh
	:able-to-see-mesh)
  (:export
   :+tag-head-key+
   :+tag-left-weapon-key+
   :+tag-right-weapon-key+
   :find-tag
   :md2-mesh
   :load-md2-model
   :load-md2-player
   :tag-key-parent
   :set-animation))

(defpackage :obj-mesh
  (:shadow :load)
  (:use :cl
	:sb-cga
	:sb-cga-utils
	:interfaces
	:num-utils
	:buffered-input-file
	:parser
	:mesh)
  (:export
   :+texture-filename+
   :load))

(defpackage :trees
  (:use :cl
	:constants
	:interfaces
	:parser
	:sb-cga
	:sb-cga-utils
	:num-utils
	:mesh)
  (:export
   :gen-tree))

(defpackage :load-level
  (:use :cl
	:kanren-trs
	:config
	:constants
	:misc
	:num
	:2d-utils
	:matrix
	:interfaces
	:texture
	:level-config
	:random-labyrinth
	:random-terrain
	:pickable-mesh
	:terrain-chunk
	:game-state
	:world)
  (:import-from :sb-cga
		:vec
		:copy-vec
		:alloc-vec
		:vec+
		:vec-
		:vec/
		:translate
		:translate*
		:transform-point)
  (:export
   :load-level))

;; AI

(defpackage :id3
  (:use :cl
	:mtree-utils
	:interfaces))

(defpackage :ann
  (:use :cl
	:interfaces
	:parse-number
	:matrix
	:xmls-utils
	:conditions)
  (:export
   :neuron
   :weights
   :activation-threshold
   :activation-function
   :clone
   :serialize
   :ann-layer
   :neurons
   :ann
   :layers
   :outputs
   :input
   :total-sse
   :feed
   :train
   :train-batch
   :xmls->ann
   :load-ann
   :save-ann
   :quick-ann-skeleton
   :gen-blank-hopfield-ann
   :memorize
   :recall))

(defpackage :main-window
  (:use :cl
	:config
	:constants
	:sb-cga
	:num
	:misc
	:vec4
	:vec2
	:ivec2
	:sb-cga-utils
	:transformable
	:camera
	:game-state
	:world
	:sdl2.kit
	:shaders-utils
	:identificable
	:load-level)
  (:export
   :world
   :accept-input-p
   :main
   :fps))
