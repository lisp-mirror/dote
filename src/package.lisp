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
   :+default-size+
   :+map-max-size+
   :+pi+
   :+2pi+
   :+pi/2+
   :+x-axe+
   :+y-axe+
   :+z-axe+
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
   :+avatar-portrait-resource+
   :+maps-resource+
   :+shaders-resource+
   :+models-resource+
   :+textures-resource+
   :+scripts-resource+
   :+furnitures-resource+
   :+fonts-resource+
   :+gui-resource+
   :+default-gui-inventory-items+
   :+default-gui-resource+
   :+default-character-weapons+
   :+default-character-containers+
   :+default-character-potions+
   :+default-character-food+
   :+default-character-misc+))

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

(defpackage :filesystem-utils
  (:use :cl)
  (:nicknames :fs)
  (:export
   :*directory-sep-regexp*
   :*directory-sep*
   :slurp-file
   :dump-sequence-to-file
   :cat-parent-dir
   :has-extension
   :do-directory
   :split-path-elements
   :path-last-element
   :path-to-hidden-file-p
   :parent-dir-path
   :strip-dirs-from-path
   :get-stat-mtime
   :get-stat-ctime
   :get-stat-atime
   :file-outdated-p
   :file-exists-p
   :file-hash
   :temporary-filename
   :with-anaphoric-temp-file
   :temp-file
   :file-can-write-p
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
   :get-resource-file
   :get-resource-files))

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
   :description-for-humans))

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
   :ivec2p
   :ivec2*
   :ivec2/
   :ivec2~
   :ivec2+
   :ivec2-
   :ivec2-length
   :ivec2-normalyize))

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
   :vec2-normalyize))

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
  (:nicknames :tree)
  (:export
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
   :remove-child))

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
   :tangent-TBN
   :tangent-in-normal-space
   :ccw-poly-fannify
   :plane-equation
   :plane-equation-as-vec4
   :same-plane-p
   :same-plane-p*
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
   :2d-utils)
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
  (:shadow :push :pop :find)
  (:export
   :push
   :pop
   :find
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
   :gen-neighbour
   :gen-4-neighbour-counterclockwise
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
   :good-aabb-start
   :flood-fill
   :kernel-+
   :kernel-*
   :apply-kernel
   :papply-kernel
   :gaussian-blur
   :gaussian-blur-separated
   :pgaussian-blur-separated
   :psobel-edge-detection
   :pmatrix-blit
   :pblit-matrix))

(defpackage :graph
  (:use :cl
	:interfaces)
  (:export
   :graph
   :tile-based-graph
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

(defpackage :identificable
  (:use :cl
	:constants
	:config
	:interfaces)
  (:export
   :*clone-id*
   :*entity-id-counter*
   :identificable
   :id))

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
   :doorp
   :windowp
   :invalicablep
   :occupied-rate
   :generate
   :gen-simple-room
   :shared-matrix
   :whole-aabb
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
   :mock-null-pointer
   :fast-glaref
   :seq->gl-array
   :copy-gl-array
   :gl-array->list
   :lerp-gl-array
   :render-to-texture
   :with-render-to-texture
   :with-render-to-file
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
   :*window*
   :*door-n*
   :*door-s*
   :*door-e*
   :*door-w*
   :clean-global-wars
   :gen-normalmap-if-needed))

(defpackage :game-state
  (:use :cl
	:config
	:constants
	:num
	:vec4
	:matrix
	:identificable
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
   :+npc-type+
   :+pc-type+
   :+floor-type+
   :+ceiling-type+
   :map-state-element
   :entity-id
   :el-type
   :game-state
   :game-hour
   :game-minutes
   :current-time
   :sky-bg-color
   :celestial-body-position
   :movement-costs
   :map-state
   :level-difficult
   :map-cache-dir
   :light-color
   :setup-game-hour
   :prepare-map-state
   :get-cost
   :get-cost-insecure
   :el-type-in-pos
   :entity-id-in-pos
   :selected-pc
   :build-movement-path
   :terrain-aabb-2d))

;; engine

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
   :state
   :aabb-2d))

(defpackage :camera
  (:use :cl
	:constants
	:sb-cga
	:sb-cga-utils
	:interfaces
	:num
	:euler
	:quaternion
	:identificable
	:entity
	:transformable)
  (:export
   :camera
   :frustum-aabb
   :reorient-fp-camera
   :drag-camera
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
   :containsp
   :frustum-planes
   :calculate-aabb))

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
  ;;(:shadowing-import-from :num-utils epsilon=)
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
   :mesh
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
   :bounding-sphere
   :transform-vertices
   :get-material-from-texture
   :parent-mesh
   :children
   :texture-object
   :normal-map
   :texture-projector
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
   :triangle
   :triangle-mesh-shell
   :tree-mesh-shell
   :fill-mesh-data
   :fill-shell-from-mesh
   :load-tag-file
   :load-tags
   :tag->matrix
   :nsetup-tag-matrix
   :find-tag-cdr
   :find-tag
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
   :setup-projective-texture))

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

(defpackage :game-event
  (:use
   :cl
   :alexandria
   :misc
   :num)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :end-turn
   :end-turn-members
   :register-for-end-turn
   :unregister-for-end-turn
   :get-on-end-turn
   :propagate-end-turn))

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
   :+window-texture-name+
   :+button-texture-name+
   :+button-pressed-texture-name+
   :+button-cancel-texture-name+
   :+button-ok-texture-name+
   :+window-close-button-texture-name+
   :+window-close-button-pressed-texture-name+
   :+frame-texture-name+
   :+window-top-bar-texture-name+
   :+text-field-texture-name+
   :+text-field-focused-texture-name+
   :+text-field-overlay-texture-name+
   :+square-button-texture-name+
   :+square-button-pressed-texture-name+
   :+blue-h-bar+
   :+red-h-bar+
   :+green-h-bar+
   :+check-button-texture-name+
   :+check-button-checked-green+
   :+check-button-overlay+
   :+save-overlay-texture-name+
   :+load-overlay-texture-name+
   :+plus-overlay-texture-name+
   :+minus-overlay-texture-name+
   :+up-overlay-texture-name+
   :+down-overlay-texture-name+
   :+option-overlay-texture-name+
   :+quit-overlay-texture-name+
   :+next-overlay-texture-name+
   :+previous-overlay-texture-name+
   :+next-turn-overlay-texture-name+
   :+open-overlay-texture-name+
   :+close-overlay-texture-name+
   :+zoom-overlay-texture-name+
   :+unzoom-overlay-texture-name+
   :+berserk-texture-name+
   :+coma-texture-name+
   :+terror-texture-name+
   :+portrait-unknown-texture-name+
   :+poison-texture-name+
   :+conversation-overlay-texture-name+
   :+attack-short-range-overlay-texture-name+
   :+attack-long-range-overlay-texture-name+
   :+attack-long-range-imprecise-overlay-texture-name+
   :+magic-staff-overlay-texture-name+
   :+default-font+
   :+default-font-handle+
   :+static-text-delim+
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
	:gui-events
	:gui)
  (:export
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
   :naked-button
   :button
   :text-field
   :check-button
   :signalling-light
   :button-state
   :flip-state
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
   :text-communication
   :text-fps
   :b-save
   :b-load
   :player-generator
   :make-player-generator))

(defpackage :world
  (:use :cl
	:config
	:constants
	:mtree-utils
	:sb-cga
	:sb-cga-utils
	:cl-gl-utils
	:identificable
	:entity
	:game-state
	:transformable
	:interfaces
	:camera
	:mesh
	:pickable-mesh)
  (:export
   :doors
   :door-n
   :door-s
   :door-e
   :door-w
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
   :windows-bag
   :gui
   :render-for-reflection
   :push-entity
   :get-window-size
   :main-light-pos
   :main-light-color
   :initialize-skydome
   :render-gui
   :highlight-tile-screenspace))

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
   :origin-offset
   :build-mesh-dbg
   :build-mesh
   :nclip-with-aabb
   :clip-with-aabb
   :make-terrain-chunk))

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
	:num
	:misc
	:mesh-material
	:mesh)
  (:export
   :+tag-head-key+
   :+tag-left-weapon-key+
   :+tag-right-weapon-key+
   :find-tag
   :load-md2-model
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
  (:import-from :sb-cga :vec :copy-vec :alloc-vec :vec+ :vec- :vec/)
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

(defpackage :basic-interaction-parameters
  (:use :cl
	:alexandria
	:constants
	:config
	:interfaces
	:identificable)
  (:nicknames :interaction)
  (:export
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
   :decay-parameters
   :effect-parameters
   :modifier
   :trigger
   :duration
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
   :define-magic-effect
   :define-poison-effect
   :with-interaction-parameters))

(defpackage :player-character
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
	:basic-interaction-parameters)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :+unknown-ability-bonus+
   :+starting-exp-points+
   :player-character
   :portrait
   :first-name
   :last-name
   :strength
   :stamina
   :dexterity
   :agility
   :smartness
   :empaty
   :weight
   :damage-points
   :movement-points
   :magic-points
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
   :unlock-chance
   :deactivate-trap-chance
   :reply-attack-chance
   :ambush-attack-chance
   :spell-chance
   :attack-spell-chance
   :status
   :race
   :level
   :exp-points
   :make-warrior
   :make-wizard
   :make-healer
   :make-archer
   :make-ranger
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
   :decay-p
   :effects-p
   :healing-effects-p
   :magic-effect-p
   :interaction-get-strength
   :interaction-get-stamina
   :interaction-get-dexterity
   :interaction-get-agility
   :interaction-get-smartness
   :interaction-get-empaty
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
   :interaction-get-magic-effect))

(defpackage :main-window
  (:use :cl
	:config
	:constants
	:sb-cga
	:vec4
	:sb-cga-utils
	:transformable
	:camera
	:game-state
	:world
	:sdl2.kit
	:shaders-utils
	:load-level)
  (:export
   :fps))
