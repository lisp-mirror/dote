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

(in-package :mesh)

(alexandria:define-constant +vbo-count+            7             :test #'=)

(alexandria:define-constant +vao-count+            5             :test #'=)

(alexandria:define-constant +aabb-vertex-count+    72            :test #'=)

(alexandria:define-constant +aabb-points-count+    24            :test #'=)

(alexandria:define-constant +tag-head-key+         "tag_head"    :test #'string=)

(alexandria:define-constant +tag-left-weapon-key+  "tag_lweapon" :test #'string=)

(alexandria:define-constant +tag-right-weapon-key+ "tag_rweapon" :test #'string=)

(defclass triangle ()
  ((vertex-index
    :initform (uivec 0 0 0)
    :initarg :vertex-index
    :accessor vertex-index)
   (texture-index
    :initform (uivec 0 0 0)
    :initarg :texture-index
    :accessor texture-index)
   (normal-index
    :initform (uivec 0 0 0)
    :initarg :normal-index
    :accessor normal-index)
   (tangent-index
    :initform (uivec 0 0 0)
    :initarg :tangent-index
    :accessor tangent-index)
   (custom-attributes-indices
    :initform (make-hash-table :test 'eq)
    :initarg :custom-attributes-indices
    :accessor custom-attributes-indices)
   (emitted-edges
    :initform (misc:make-array-frame 3 nil 'edge t)
    :initarg :emitted-edges
    :accessor emitted-edges)))

(defparameter *print-triangle-edges* nil)

(defmethod print-object ((object triangle) stream)
  (format stream "<triangle ~a t ~a n ~a tan ~a edges ~a>"
          (vertex-index object)
          (texture-index object) (normal-index object) (tangent-index object)
          (if *print-triangle-edges*
              (emitted-edges object)
              "suppressed")))

(defmethod marshal:class-persistant-slots ((object triangle))
  '(vertex-index
    texture-index
    normal-index
    tangent-index
    custom-attributes-indices
    emitted-edges))

(defmethod clone-into :after ((from triangle) (to triangle))
  (setf (vertex-index              to) (copy-uivec (vertex-index from))
        (texture-index             to) (copy-uivec (texture-index from))
        (normal-index              to) (copy-uivec (normal-index from))
        (tangent-index             to) (copy-uivec (tangent-index from))
        (emitted-edges             to) (alexandria:copy-array (emitted-edges from))
        (custom-attributes-indices to) (alexandria:copy-hash-table
                                        (custom-attributes-indices from)))
    to)

(defmethod clone ((object triangle))
  (with-simple-clone (object 'triangle)))

(defmethod copy-flat-into :after ((from triangle) (to triangle))
  (setf (vertex-index              to) (vertex-index from)
        (texture-index             to) (texture-index from)
        (normal-index              to) (normal-index from)
        (tangent-index             to) (tangent-index from)
        (emitted-edges             to) (emitted-edges from)
        (custom-attributes-indices to) (custom-attributes-indices from))
    to)

(defmethod copy-flat ((object triangle))
  (with-simple-copy-flat (object 'triangle)))

(defgeneric tuple-vertex-texel-normal-index (object))

(defgeneric find-vertex-attibute-by-vertex-index (object index))

(defgeneric find-vertex-attribute-by-vertex-index-alist (object index))

(defgeneric find-edge-facing-vertex (face vertex-index))

(defgeneric find-faces-sharing-edge (face edge))

(defgeneric get-custom-attribute (object type))

(defgeneric set-custom-attribute (object type value))

(defmethod tuple-vertex-texel-normal-index ((object triangle))
  (if (emitted-edges object)
      (map 'vector #'(lambda (v tex n tg e) (list v tex n tg e))
           (vertex-index object)
           (texture-index object)
           (normal-index object)
           (tangent-index object)
           (emitted-edges object))
      (map 'vector #'(lambda (v tex n tg) (list v tex n tg))
           (vertex-index object)
           (texture-index object)
           (normal-index object)
           (tangent-index object))))

(defmethod find-vertex-attibute-by-vertex-index ((object triangle) index)
  (find-if #'(lambda (tuple) (= index (first tuple)))
           (tuple-vertex-texel-normal-index object)))

(alexandria:define-constant +vertex-prop+ :vertex :test #'eq)

(alexandria:define-constant +texture-prop+ :texture :test #'eq)

(alexandria:define-constant +normal-prop+ :normal :test #'eq)

(alexandria:define-constant +tangent-prop+ :tangent :test #'eq)

(alexandria:define-constant +edge-prop+ :edge :test #'eq)

(defmethod find-vertex-attribute-by-vertex-index-alist ((object triangle) index)
  (let ((attributes (find-vertex-attibute-by-vertex-index object index)))
    (if attributes
        (map 'list #'(lambda (p a) (cons p a))
             (list +vertex-prop+ +texture-prop+ +normal-prop+ +tangent-prop+ +edge-prop+)
             attributes)
        nil)))

(defmethod find-edge-facing-vertex ((face triangle) (vertex-index fixnum))
  (let ((edge (find-edge-associed-to-vertex-index face vertex-index)))
    (next-edge (next-edge edge))))

(defmacro gen-find-*-associed-by-vertex-index (key)
  `(defun ,(alexandria:format-symbol t "~:@(find-~a-associed-to-vertex-index~)" key)
       (triangle index)
     (cdr (assoc ,(alexandria:format-symbol t "~:@(+~a-prop+~)" key)
                 (find-vertex-attribute-by-vertex-index-alist triangle index)))))

(gen-find-*-associed-by-vertex-index normal)

(gen-find-*-associed-by-vertex-index tangent)

(gen-find-*-associed-by-vertex-index texture)

(gen-find-*-associed-by-vertex-index edge)

(defmethod get-custom-attribute ((object triangle) (type symbol))
  (gethash type (custom-attributes-indices object)))

(defmethod set-custom-attribute ((object triangle) (type symbol) value)
  (setf (gethash type (custom-attributes-indices object)) value))

(defun make-triangle (vertices textures normals edges
                      &key
                        (tangent (uivec 0 0 0))
                        (custom-attributes (make-hash-table :test 'eq)))
  (make-instance 'triangle
                 :vertex-index  vertices
                 :texture-index textures
                 :normal-index normals
                 :tangent-index tangent
                 :emitted-edges edges
                 :custom-attributes-indices custom-attributes))

(defclass edge ()
  ((from
    :initform nil
    :initarg :from
    :accessor from)
   (to
    :initform nil
    :initarg :to
    :accessor to)
   (opposite-edge
    :initform nil
    :initarg :opposite-edge
    :accessor opposite-edge)
   (face
    :initform nil
    :initarg :face
    :accessor face)
   (next-edge
    :initform nil
    :initarg :next-edge
    :accessor next-edge)))

(defmethod marshal:class-persistant-slots ((object edge))
  '(from
    to
    opposite-edge
    face
    next-edge))

(defmethod find-faces-sharing-edge ((face triangle) (edge edge))
  (values (face (opposite-edge edge)) (face edge)))

(defmethod print-object ((object edge) stream)
  (with-slots (from to opposite-edge face next-edge) object
    (format stream "<edge ~a -> ~a opposite ~a face ~a next edges ~a>"
            from to opposite-edge face next-edge)))

(defun edge-equal (a b)
  (and (= (from a) (from b))
       (= (to a) (to b))
       (edge-equal (opposite-edge a) (opposite-edge b))))

(defun make-edge (from to opposite-edge face next-edge)
  (make-instance 'edge
                 :from from
                 :to to
                 :opposite-edge opposite-edge
                 :face face
                 :next-edge next-edge))

(defun init-vertices-slot (&optional (length 0))
  (misc:make-array-frame length +zero-vec+ 'vec nil))

(defun init-normals-slot (&optional (length 0))
  (misc:make-array-frame length +zero-vec+ 'vec nil))

(defun init-tangents-slot (&optional (length 0))
  (misc:make-array-frame length +zero-vec+ 'vec nil))

(defun init-texture-coordinates-slot ()
  (misc:make-array-frame 0 (vec2 0.0 0.0) 'vec2 nil))

(defun init-edges-slot ()
  (misc:make-array-frame 0))

(defmacro with-camera ((camera renderer) &body body)
  `(let* ((,camera (camera ,renderer)))
     ,@body))

(defmacro with-camera-view-matrix ((matrix renderer &key (wrapped nil)) &body body)
  `(let* ((,matrix ,(if wrapped
                        `(the (simple-array simple-array (1))
                              (view-matrix (camera ,renderer)))
                        `(elt (the (simple-array simple-array (1))
                                   (view-matrix (camera ,renderer)))
                              0))))
     ,@body))

(defmacro with-camera-projection-matrix ((matrix renderer &key (wrapped nil)) &body body)
  `(let* ((,matrix ,(if wrapped
                        `(the (simple-array simple-array (1))
                              (projection-matrix (camera ,renderer)))
                        `(elt (the (simple-array simple-array (1))
                                   (projection-matrix (camera ,renderer)))
                              0))))
     ,@body))

(defmacro with-modelview-matrix ((matrix model &key (wrapped nil)) &body body)
  `(let* ((,matrix ,(if wrapped
                        `(the (simple-array simple-array (1))
                              (modelview-matrix ,model))
                        `(elt (the (simple-array simple-array (1))
                                   (modelview-matrix ,model))
                              0))))
     ,@body))

(defmacro with-model-matrix ((matrix model &key (wrapped nil)) &body body)
  `(let* ((,matrix ,(if wrapped
                        `(the (simple-array simple-array (1))
                              (model-matrix ,model))
                        `(elt (the (simple-array simple-array (1))
                                   (model-matrix ,model))
                              0))))
     ,@body))

(defclass triangle-mesh (entity transformable renderizable destructible m-tree)
  ((renderer-data-vertices
    :initform nil
    :initarg :renderer-data-vertices
    :accessor renderer-data-vertices)
   (renderer-data-count-vertices
    :initform 0
    :initarg :renderer-data-count-vertices
    :accessor renderer-data-count-vertices
    :type (unsigned-byte 64))
   (renderer-data-texture
    :initform nil
    :initarg :renderer-data-texture
    :accessor renderer-data-texture)
   (renderer-data-count-texture
    :initform 0
    :initarg :renderer-data-count-texture
    :accessor renderer-data-count-texture
    :type (unsigned-byte 64))
   (renderer-data-normals
    :initform nil
    :initarg :renderer-data-normals
    :accessor renderer-data-normals)
   (renderer-data-count-normals
    :initform 0
    :initarg :renderer-data-count-normals
    :accessor renderer-data-count-normals
    :type (unsigned-byte 64))
   (renderer-data-tangents
    :initform nil
    :initarg :renderer-data-tangents
    :accessor renderer-data-tangents)
   (renderer-data-count-tangents
    :initform 0
    :initarg :renderer-data-count-tangents
    :accessor renderer-data-count-tangents
    :type (unsigned-byte 64))
   (renderer-data-normals-obj-space
    :initform nil
    :initarg :renderer-data-normals-obj-space
    :accessor renderer-data-normals-obj-space
    :documentation "For debug only")
   (renderer-data-tangents-obj-space
    :initform nil
    :initarg :renderer-data-tangents-obj-space
    :accessor renderer-data-tangents-obj-space
    :documentation "For debug only")
   (renderer-data-aabb-obj-space
    :initform nil
    :initarg :renderer-data-aabb-obj-space
    :accessor renderer-data-aabb-obj-space
    :documentation "For debug only")
   (vertices
    :initform (init-vertices-slot)
    :initarg :vertices
    :accessor vertices)
   (vertices-count
    :initform 0
    :initarg :vertices-count
    :accessor vertices-count)
   (cumulative-vertices-count
    :initform 0
    :initarg :cumulative-vertices-count
    :accessor cumulative-vertices-count
    :documentation "Count all the vertices added so far; used for triangles compute (see vertex-v)")
   (texture-coordinates
    :initform (init-texture-coordinates-slot)
    :initarg :texture-coord
    :accessor texture-coord)
   (texture-count
    :initform 0
    :initarg :texture-count
    :accessor texture-count)
   (normals
    :initform (init-normals-slot)
    :initarg :normals
    :accessor normals)
   (normals-count
    :initform 0
    :initarg :normals-count
    :accessor normals-count)
   (tangents
    :initform (init-tangents-slot)
    :initarg :tangents
    :accessor tangents)
   (tangents-count
    :initform 0
    :initarg :tangents-count
    :accessor tangents-count)
   (texture-object
    :initform nil
    :initarg :texture-object
    :reader texture-object)
   (normal-map
    :initform nil
    :initarg :normal-map
    :reader normal-map)
   (texture-projector
    :initform nil
    :initarg :texture-projector
    :reader texture-projector)
   (projector
    :initform nil
    :initarg :projector
    :reader projector)
   (impostor
    :initform nil
    :initarg :impostor
    :accessor impostor)
   (modelview-matrix
    :initform (identity-matrix)
    :initarg :modelview-matrix
    :accessor modelview-matrix)
   (modelview-stack
    :initform '()
    :initarg :modelview-stack
    :accessor modelview-stack)
   (triangles
    :initform '()
    :initarg :triangles
    :accessor triangles)
   (material-params
    :initform (make-mesh-material 0.0 0.0 0.0 0.0 0.0)
    :initarg :material-params
    :accessor material-params)
   (edges
    :initform (init-edges-slot)
    :initarg :edges
    :accessor edges)
   (parent-mesh
    :initform nil
    :initarg :parent-mesh
    :accessor parent-mesh)
   (tags-table
    :initform '()
    :initarg  :tags-table
    :accessor tags-table
    :type cons)
   (tags-matrices
    :initform '()
    :initarg  :tags-matrices
    :accessor tags-matrices
    :type cons)
   (tag-key-parent
    :initform +tag-head-key+
    :initarg  :tag-key-parent
    :accessor tag-key-parent
    :type string)
   (aabb
    :initform (make-instance 'aabb)
    :initarg :aabb
    :writer (setf aabb))
   (bounding-sphere
    :initform (make-instance 'bounding-sphere)
    :initarg :bounding-sphere
    :accessor bounding-sphere)
   (vao
    :initform '()
    :initarg :vao
    :accessor vao)
   (vbo
    :initform '()
    :initarg :vbo
    :accessor vbo)
   (find-index-by-value-from-end
    :initform nil
    :initarg  :find-index-by-value-from-end
    :accessor find-index-by-value-from-end)
   (renderp
    :initform t
    :initarg :renderp
    :accessor renderp)
   (calculatep
    :initform t
    :initarg :calculatep
    :accessor calculatep)
   (use-blending-p
    :initform nil
    :initarg  :use-blending-p
    :accessor use-blending-p)
   (render-normals
    :initform nil
    :initarg :render-normals
    :accessor render-normals)
   (render-aabb
    :initform nil
    :initarg :render-aabb
    :accessor render-aabb)
   (render-tangents
    :initform nil
    :initarg :render-tangents
    :accessor render-tangents)))

(defmacro gen-empty-predicate (slot)
  (let ((function-name (format nil "~:@(empty-~a-p~)" (symbol-name slot))))
    `(progn
       (defgeneric ,(alexandria:format-symbol t "~a" function-name) (object))
       (defmethod ,(alexandria:format-symbol t "~a" function-name) (object)
         (= 0 (length (,slot object)))))))

(gen-empty-predicate triangles)

(gen-empty-predicate vertices)

(gen-empty-predicate normals)

(defmacro do-children-mesh ((child-var object) &body body)
  `(loop for ,child-var across (the (simple-array triangle-mesh (*)) (children ,object)) do
        ,@body))

(defmacro do-triangles ((triangle object) &body body)
  `(loop for ,triangle in (triangles ,object) do
        ,@body))

(defmacro gen-do-triangles-and-entity (name what)
  `(defmacro
       ,(alexandria:format-symbol t "~:@(do-triangles-~a-index~)" name)
       ((triangle pointer mesh) &body body)
     `(do-triangles (,triangle ,mesh)
        (loop for ,pointer across
             ,,(ecase what
                      (:vertex ``(vertex-index ,triangle))
                      (:normal ``(normal-index ,triangle))
                      (:texture ``(texture-index ,triangle)))
           do
             ,@body))))

(gen-do-triangles-and-entity vertex :vertex)

(gen-do-triangles-and-entity normal :normal)

(gen-do-triangles-and-entity texture :texture)

(defmacro gen-accessors-matrix (name)
  (let ((fn-name (list (alexandria:format-symbol t "~:@(setf~)")
                       (alexandria:format-symbol t "~:@(~a~)" name)))
        (get-name (alexandria:format-symbol t "~:@(~a~)" name)))
    `(defmethod ,fn-name (new-value (object triangle-mesh))
       (with-slots (,name) object
         (setf (elt ,name 0) new-value))
       (do-children-mesh (i object)
         (setf (,get-name i) new-value)))))

(gen-accessors-matrix projection-matrix)

(gen-accessors-matrix model-matrix)

(gen-accessors-matrix view-matrix)

(defmethod current-time ((object triangle-mesh))
  (game-state:current-time (state object)))

(defmethod fog-density ((object triangle-mesh))
  (game-state:fog-density (state object)))

(defmethod (setf pos) (new-pos (object triangle-mesh))
  (setf (slot-value object 'pos) new-pos))

(defmethod (setf compiled-shaders) (new-value (object triangle-mesh))
  (with-slots (compiled-shaders) object
    (setf compiled-shaders new-value)
    (do-children-mesh (i object)
      (setf (compiled-shaders i) new-value))))

(defmethod marshal:class-persistant-slots ((object triangle-mesh))
  (append  '(normals
             normals-count
             vertices
             vertices-count
             tangents
             tangents-count
             cumulative-vertices-count
             ;;vertices-set
             texture-coordinates
             texture-count
             texture-object
             normal-map
             texture-projector
             projector
             modelview-matrix
             modelview-stack
             triangles
             material-params
             edges
             parent-mesh
             aabb
             renderp
             use-blending-p
             render-aabb
             render-normals
             render-tangents)
           (call-next-method)))

(defmethod game-event:on-game-event ((object triangle-mesh) (event game-event:end-turn))
  ;;(misc:dbg "mesh end turn ~a(~a) ~a" (type-of object) (id object) (type-of event))
  nil)

(defmethod game-event:on-game-event ((object triangle-mesh)
                                     (event  game-event:move-entity-along-path-event))
  (if (= (id object) (game-event:id-destination event))
      t
      nil))

(defmethod game-event:on-game-event ((object triangle-mesh)
                                     (event game-event:move-entity-entered-in-tile-event))
  nil)


(defmethod on-game-event ((object triangle-mesh) (event game-event:end-attack-melee-event))
  (game-event:check-event-targeted-to-me (object event)
    (with-accessors ((attacked-by-entity attacked-by-entity)) object
      (game-event:unregister-for-end-attack-melee-event object)
      (setf (entity:reply-attack attacked-by-entity) nil)
      (setf attacked-by-entity nil))
    t))

(defmethod on-game-event ((object triangle-mesh) (event game-event:end-attack-long-range-event))
  (game-event:check-event-targeted-to-me (object event)
    (with-accessors ((attacked-by-entity attacked-by-entity)) object
      (game-event:unregister-for-end-attack-long-range-event object)
      (when attacked-by-entity
        (setf (entity:reply-attack attacked-by-entity) nil))
      (setf attacked-by-entity nil))
    t))

(defmethod on-game-event ((object triangle-mesh) (event game-event:end-attack-spell-event))
  (game-event:check-event-targeted-to-me (object event)
    (with-accessors ((attacked-by-entity attacked-by-entity)) object
      (game-event:unregister-for-end-attack-spell-event object)
      (when attacked-by-entity
        (setf (entity:reply-attack attacked-by-entity) nil))
      (setf attacked-by-entity nil))
    t))

(defgeneric aabb (object))

(defgeneric actual-aabb-for-bullets (object))

(defgeneric actual-aabb-for-visibility (object))

(defgeneric reset-aabb (object))

(defgeneric prepare-for-rendering-normal-map (object))

(defgeneric prepare-for-rendering-phong (object))

(defgeneric render-normalmap (object renderer))

(defgeneric render-phong (object renderer))

(defgeneric render-lod-1 (object renderer))

(defgeneric vertex-v (object vec &key gen-triangle gen-normal compact-vertices manifoldp))

(defgeneric vertex (object x y z &key gen-triangle gen-normal compact-vertices manifoldp))

(defgeneric texel-v (object coord))

(defgeneric texel (object s-coord t-coord))

(defgeneric normal-v (object vec))

(defgeneric normal (object x y z))

(defgeneric tangent-v (object vec))

(defgeneric tangent (object x y z))

(defgeneric triangle (object &key v1 v2 v3 t1 t2 t3 n1 n2 n3 compact-vertices manifoldp))

(defgeneric triangle-w-calculated-normal (object &key v1 v2 v3 t1 t2 t3 compact-vertices
                                                   manifoldp))

(defgeneric gen-new-triangle-edge (object triangle v1 v2))

(defgeneric push-matrix (object &key what))

(defgeneric pop-matrix (object &key what))

(defgeneric load-matrix (object matrix &key what))

(defgeneric mult-matrix (object matrix &key what))

(defgeneric bubbleup-modelmatrix (object))

(defgeneric %reset-data-renderer-count (object))

(defgeneric make-data-for-opengl (object))

(defgeneric make-data-for-opengl-normals-obj-space (object))

(defgeneric make-data-for-opengl-tangents-obj-space (object))

(defgeneric make-data-for-opengl-aabb-obj-space (object))

(defgeneric merge-mesh (object guest &key manifold))

(defgeneric find-value-by-index (object index &key what))

(defgeneric find-index-by-value (object value &key what from-end))

(defgeneric find-all-index-by-value (object value &key what))

(defgeneric find-triangle-by-vertex-index (object vertex))

(defgeneric find-all-triangles-by-vertex-index (object vertex))

(defgeneric make-shared-vertices (object epsilon))

(defgeneric %make-shared-normal-based-on-shared-vertices (object))

(defgeneric remove-orphaned-vertices (object))

(defgeneric normals-obj-space-vertex-count (object))

(defgeneric tangents-obj-space-vertex-count (object))

(defgeneric average-normals (object))

(defgeneric gen-tangents (object &key epsilon))

(defgeneric average-normals-by-shared-vertices (object))

(defgeneric average-normals-if-near-vertices (branch trunk epsilon))

(defgeneric recalculate-face-normals (object))

(defgeneric subdivide-face (object face))

(defgeneric subdivide-mesh (object &key texture-object))

(defgeneric texture-plane-mapping (object)) ;; broken

(defgeneric flatten-mesh (object))

(defgeneric smooth-mesh (object &key average-normals plane-mapping))

(defgeneric manifold-maybe (object))

(defgeneric save-mesh (object place))

(defgeneric load-mesh (object place))

(defgeneric load-tags (object file))

(defgeneric transform-vertices (object transformation))

(defgeneric get-material-from-texture (object))

(defgeneric setup-projective-texture (object))

(defgeneric remove-mesh-data (object))

(defgeneric use-lod-p (object threshold renderer))

(defgeneric calculate-decrement-move-points-entering-tile (object))

(defgeneric calculate-decrement-move-points-place-trap (object))

(defgeneric calculate-decrement-move-points-activate-switch (object switch-entity))

(defgeneric decrement-move-points (object how-much))

(defgeneric decrement-spell-points (object how-much))

(defgeneric decrement-move-points-entering-tile (object))

(defgeneric decrement-move-points-rotate (object))

(defgeneric decrement-move-points-wear (object))

(defgeneric decrement-move-points-attack-melee (object))

(defgeneric decrement-move-points-place-trap (object))

(defgeneric decrement-move-points-activate-switch (object switch-entity))

(defgeneric can-use-movement-points-p (object &key minimum))

(defgeneric can-use-spell-points-p (object &key minimum))

(defgeneric inside-room-p (object))

(defgeneric rendering-needed-p (object renderer))

(defgeneric parent-labyrinth (object))

(defgeneric traverse-recurrent-effects (object))

(defgeneric process-postponed-messages (object))

(defgeneric set-death-status (object))

(defgeneric set-attack-status (object))

(defgeneric set-attack-spell-status (object))

(defgeneric set-spell-status (object))

(defgeneric entity-facing (object))

(defgeneric trap-can-be-placed-p (object))

(defmethod remove-mesh-data ((object triangle-mesh))
  (setf (normals        object) (init-normals-slot)
        (vertices       object) (init-vertices-slot)
        (edges          object) (init-edges-slot)
        (tangents       object) (init-tangents-slot)
        (texture-coord  object) (init-texture-coordinates-slot)
        (triangles      object) nil
        (vertices-count object) 0
        (texture-count  object) 0
        (normals-count  object) 0
        (tangents-count object) 0))

(defmethod use-lod-p ((object triangle-mesh) threshold  renderer)
  (with-accessors ((aabb aabb)) object
    (with-camera (camera renderer)
      (>= (vec-length (vec- (pos camera) (aabb-center aabb)))
          (d* threshold +quad-tree-leaf-size+)))))

(defmethod aabb ((object triangle-mesh))
  (with-slots (aabb) object
    (with-accessors ((model-matrix model-matrix)
                     (bounding-sphere bounding-sphere)) object
      (declare ((simple-vector 1) model-matrix))
      (let* ((mat (elt model-matrix 0))
             (min-x (min-x aabb))
             (max-x (max-x aabb))
             (min-y (min-y aabb))
             (max-y (max-y aabb))
             (min-z (min-z aabb))
             (max-z (max-z aabb))
             (min (extract-traslation-vec mat))
             (max (copy-vec min)))
        (if (d> (mref mat 0 0) 0.0)
            (progn
              (incf (elt min 0) (d* (mref mat 0 0) min-x))
              (incf (elt max 0) (d* (mref mat 0 0) max-x)))
            (progn
              (incf (elt min 0) (d* (mref mat 0 0) max-x))
              (incf (elt max 0) (d* (mref mat 0 0) min-x))))
        (if (d> (mref mat 1 0) 0.0)
            (progn
              (incf (elt min 1) (d* (mref mat 1 0) min-x))
              (incf (elt max 1) (d* (mref mat 1 0) max-x)))
            (progn
              (incf (elt min 1) (d* (mref mat 1 0) max-x))
              (incf (elt max 1) (d* (mref mat 1 0) min-x))))
        (if (d> (mref mat 2 0) 0.0)
            (progn
              (incf (elt min 2) (d* (mref mat 2 0) min-x))
              (incf (elt max 2) (d* (mref mat 2 0) max-x)))
            (progn
              (incf (elt min 2) (d* (mref mat 2 0) max-x))
              (incf (elt max 2) (d* (mref mat 2 0) min-x))))
        (if (d> (mref mat 0 1) 0.0)
            (progn
              (incf (elt min 0) (d* (mref mat 0 1) min-y))
              (incf (elt max 0) (d* (mref mat 0 1) max-y)))
            (progn
              (incf (elt min 0) (d* (mref mat 0 1) max-y))
              (incf (elt max 0) (d* (mref mat 0 1) min-y))))
        (if (d> (mref mat 1 1) 0.0)
            (progn
              (incf (elt min 1) (d* (mref mat 1 1) min-y))
              (incf (elt max 1) (d* (mref mat 1 1) max-y)))
            (progn
              (incf (elt min 1) (d* (mref mat 1 1) max-y))
              (incf (elt max 1) (d* (mref mat 1 1) min-y))))
        (if (d> (mref mat 2 1) 0.0)
            (progn
              (incf (elt min 2) (d* (mref mat 2 1) min-y))
              (incf (elt max 2) (d* (mref mat 2 1) max-y)))
            (progn
              (incf (elt min 2) (d* (mref mat 2 1) max-y))
              (incf (elt max 2) (d* (mref mat 2 1) min-y))))
        (if (d> (mref mat 0 2) 0.0)
            (progn
              (incf (elt min 0) (d* (mref mat 0 2) min-z))
              (incf (elt max 0) (d* (mref mat 0 2) max-z)))
            (progn
              (incf (elt min 0) (d* (mref mat 0 2) max-z))
              (incf (elt max 0) (d* (mref mat 0 2) min-z))))
        (if (d> (mref mat 1 2) 0.0)
            (progn
              (incf (elt min 1) (d* (mref mat 1 2) min-z))
              (incf (elt max 1) (d* (mref mat 1 2) max-z)))
            (progn
              (incf (elt min 1) (d* (mref mat 1 2) max-z))
              (incf (elt max 1) (d* (mref mat 1 2) min-z))))
        (if (d> (mref mat 2 2) 0.0)
            (progn
              (incf (elt min 2) (d* (mref mat 2 2) min-z))
              (incf (elt max 2) (d* (mref mat 2 2) max-z)))
            (progn
              (incf (elt min 2) (d* (mref mat 2 2) max-z))
              (incf (elt max 2) (d* (mref mat 2 2) min-z))))
        (let ((res (make-instance 'aabb :aabb-p1 min :aabb-p2 max)))
          (do-children-mesh (i object)
            (let ((child (aabb i)))
              (expand res (aabb-p1 child))
              (expand res (aabb-p2 child))))
          (setf bounding-sphere (aabb->bounding-sphere res))
          res)))))

(defmethod actual-aabb-for-bullets ((object triangle-mesh))
  (aabb object))

(defmethod actual-aabb-for-visibility ((object triangle-mesh))
  (aabb object))

(defmethod aabb-2d ((object triangle-mesh))
  (flatten-to-aabb2-xz (aabb object)))

(defmethod reset-aabb ((object triangle-mesh))
  "Recreate the aabb from object space vertices"
  (with-slots (aabb) object
    (with-accessors ((vertices vertices)) object
      (setf aabb (make-instance 'aabb))
      (do-triangles-vertex-index (face vertex-index object)
        (expand aabb (elt vertices vertex-index)))))
  object)

(defmethod transform-vertices ((object triangle-mesh) transformation)
  (loop for i from 0 below (length (vertices object)) do
       (setf (elt (vertices object) i)
             (transform-point (elt (vertices object) i) transformation)))
  (reset-aabb object)
  object)

(defmethod get-material-from-texture ((object triangle-mesh))
  (assert (not (null (texture-object object))))
  (setf (material-params object)
        (clone (texture:normalmap-params (texture-object object)))))

(defmethod setup-projective-texture ((object triangle-mesh))
  ;; does nothing
  object)

(defmethod calculate-decrement-move-points-entering-tile ((object triangle-mesh))
  (with-accessors ((ghost ghost)) object
    (with-accessors ((current-path character:current-path)) ghost
      (if current-path
          (let* ((next-tile     (alexandria:first-elt current-path))
                 (cost-dec      (game-state:get-cost (state object)
                                                     (elt next-tile 0)
                                                     (elt next-tile 1))))
            cost-dec)
          (d 0)))))

(defmethod calculate-decrement-move-points-place-trap ((object triangle-mesh))
  (with-accessors ((ghost ghost)) object
    (if (not (character:pclass-ranger-p ghost))
        +place-trap-cost+
        (d/ +place-trap-cost+ 3.0))))

(defmethod calculate-decrement-move-points-activate-switch ((object triangle-mesh)
                                                            (entity-switch entity))
  (with-accessors ((ghost ghost)) object
    (cond
      ((fountain-mesh-shell-p entity-switch)
       (if (or (character:pclass-wizard-p ghost)
               (character:pclass-healer-p ghost))
           +activate-switch-cost+
           (d/  +activate-switch-cost+ 2.0)))
      (t
       (error "calculate-decrement-move-points-activate-switch, no valid type")))))

(defmethod decrement-move-points ((object triangle-mesh) how-much)
  (when (can-use-movement-points-p object :minimum how-much)
    (decf (character:current-movement-points (ghost object))
          how-much)))

(defmethod decrement-spell-points ((object triangle-mesh) how-much)
   (when (can-use-spell-points-p object :minimum how-much)
     (decf (character:current-spell-points (ghost object))
           how-much)))

(defmethod decrement-move-points-entering-tile ((object triangle-mesh))
  (let ((cost (calculate-decrement-move-points-entering-tile object)))
    (when (can-use-movement-points-p object :minimum cost)
      (decf (character:current-movement-points (ghost object))
          cost))))

(defmethod decrement-move-points-rotate ((object triangle-mesh))
  (when (can-use-movement-points-p object :minimum +rotate-entity-cost-cost+)
    (decf (character:current-movement-points (ghost object))
          +rotate-entity-cost-cost+)))

(defmethod decrement-move-points-wear ((object triangle-mesh))
  (when (can-use-movement-points-p object :minimum +wear-object-entity-cost-cost+)
    (decf (character:current-movement-points (ghost object))
          +wear-object-entity-cost-cost+)))

(defmethod decrement-move-points-attack-melee ((object triangle-mesh))
  (when (can-use-movement-points-p object :minimum +attack-melee-cost+)
    (decf (character:current-movement-points (ghost object))
          +attack-melee-cost+)))

(defmethod decrement-move-points-place-trap ((object triangle-mesh))
  (let ((cost (calculate-decrement-move-points-place-trap object)))
    (when (can-use-movement-points-p object :minimum cost)
      (decf (character:current-movement-points (ghost object))
            cost))))

(defmethod decrement-move-points-activate-switch ((object triangle-mesh) entity-switch)
  (let ((cost (calculate-decrement-move-points-activate-switch object entity-switch)))
    (when (can-use-movement-points-p object :minimum cost)
      (decf (character:current-movement-points (ghost object))
            cost))))

(defmethod can-use-movement-points-p ((object triangle-mesh) &key (minimum 0))
  (and (character:current-movement-points (ghost object))
       (>= (character:current-movement-points (ghost object)) minimum)))

(defmethod can-use-spell-points-p ((object triangle-mesh) &key (minimum 0))
  (and (character:current-spell-points (ghost object))
       (>= (character:current-spell-points (ghost object)) minimum)))

(defmethod calculate-cost-position ((object triangle-mesh))
  (with-accessors ((pos pos)) object
    (ivec2:ivec2 (map-utils:coord-chunk->costs (elt pos 0))
                 (map-utils:coord-chunk->costs (elt pos 2)))))

(defmethod inside-room-p ((object triangle-mesh))
  (with-accessors ((state state)) object
    (game-state:position-inside-room-p state (calculate-cost-position object))))

(defmethod rendering-needed-p ((object triangle-mesh) renderer)
  (declare (ignore object renderer))
  t)

(defmethod entity-facing ((object triangle-mesh))
  (with-accessors ((state state)
                   (pos pos)
                   (dir dir)) object
    (with-accessors ((map-state map-state)) state
      (let* ((position-matrix (map-utils:facing-pos pos dir)))
        (matrix:with-check-matrix-borders (map-state
                                           (elt position-matrix 0)
                                           (elt position-matrix 1))
          (let* ((entity-element (matrix:matrix-elt map-state
                                                    (elt position-matrix 1)
                                                    (elt position-matrix 0)))
                 (entity-id      (entity-id entity-element)))
            (if (/= entity-id (invalid-entity-id-map-state))
                (values (find-entity-by-id state entity-id) entity-element)
                (values nil entity-element))))))))

(defmethod trap-can-be-placed-p ((object triangle-mesh))
  (can-use-movement-points-p object :minimum (calculate-decrement-move-points-place-trap object)))

(defmethod get-first-near ((object triangle-mesh) vertex-index)
  (misc:do-while* ((first-face (find-triangle-by-vertex-index object vertex-index))
                   (face first-face)
                   (first-near (misc:make-array-frame 0 -1 'fixnum nil)))
      ((eq face first-face) first-near)
    (let ((edge (find-edge-associed-to-vertex-index face vertex-index)))
      (vector-push-extend (to edge) first-near)
      (setf face (face (opposite-edge edge))))))

(defmethod get-first-near-as-id ((object triangle-mesh) vertex-index)
  (get-first-near object vertex-index))

(defparameter *print-mesh-details* nil)

(defmethod print-object ((object triangle-mesh) stream)
  (if *print-mesh-details*
      (let ((*print-circle* t))
        (print-unreadable-object (object stream :type t :identity t)
          (loop for tr in (triangles object) do
               (format stream "~a +~a+~%--~a--~%" tr
                       (loop for i across (normal-index tr) collect
                            (find-value-by-index object i :what :normal))
                   (loop for i across (vertex-index tr) collect
                        (find-value-by-index object i))))))
      (print-unreadable-object (object stream :type t :identity nil)
        (format stream "~a" (id object)))))

(defmacro gen-recursive-writer (what argname)
  (let ((fn-name (alexandria:format-symbol t "~:@(~a~)" what)))
    `(progn
       (defgeneric (setf ,fn-name) (,argname object))
       (defmethod (setf ,fn-name) (,argname (object triangle-mesh))
         (with-slots (,what) object
           (setf ,what ,argname)
           (do-children-mesh (child object)
             (when (not (,what child))
               (setf (,what child) ,argname))))))))

(gen-recursive-writer texture-object    texture)

(gen-recursive-writer normal-map        texture)

(gen-recursive-writer texture-projector texture)

(gen-recursive-writer projector         mat)

(defmethod find-value-by-index ((object triangle-mesh) index &key (what :vertex))
  (let ((sequence (ecase what
                    (:vertex
                     (vertices object))
                    (:texture
                     (texture-coord object))
                    (:normal
                     (normals object))
                    (:tangent
                     (tangents object)))))
    (elt sequence index)))

(defmethod find-index-by-value ((object triangle-mesh) value &key (what :vertex)
                                                               (from-end (find-index-by-value-from-end object)))
  (let ((sequence (ecase what
                    (:vertex
                     (vertices object))
                    (:texture
                     (texture-coord object))
                    (:normal
                     (normals object))
                    (:tangent
                     (tangents object)))))
    (position-if #'(lambda (a) (every #'epsilon= a value)) sequence :from-end from-end)))

(defmethod find-all-index-by-value ((object triangle-mesh) value &key (what :vertex))
  (labels ((%find (sequence value &optional (count 0))
             (if (null sequence)
                 nil
                 (let ((position (position-if
                                  #'(lambda (a)
                                      (every #'epsilon= a value))
                                  sequence)))
                   (append (and position (list (+ count position)))
                           (and position
                                (%find (subseq sequence (1+ position))
                                       value
                                       (+ count position 1))))))))
    (let ((sequence (ecase what
                      (:vertex
                       (vertices object))
                      (:texture
                       (texture-coord object))
                      (:normal
                       (normals object)))))
      (%find sequence value))))

(defmethod merge-mesh ((object triangle-mesh) (guest triangle-mesh) &key (manifold t))
  (do-triangles (triangle guest)
    (let ((vertex1 (find-value-by-index guest (elt (vertex-index triangle) 0)
                                        :what :vertex))
          (vertex2 (find-value-by-index guest (elt (vertex-index triangle) 1)
                                        :what :vertex))
          (vertex3 (find-value-by-index guest (elt (vertex-index triangle) 2)
                                        :what :vertex))
          (texture1 (find-value-by-index guest (elt (texture-index triangle) 0)
                                         :what :texture))
          (texture2 (find-value-by-index guest (elt (texture-index triangle) 1)
                                         :what :texture))
          (texture3 (find-value-by-index guest (elt (texture-index triangle) 2)
                                         :what :texture))
          (normal1 (find-value-by-index guest (elt (normal-index triangle) 0)
                                        :what :normal))
          (normal2 (find-value-by-index guest (elt (normal-index triangle) 1)
                                        :what :normal))
          (normal3 (find-value-by-index guest (elt (normal-index triangle) 2)
                                        :what :normal))
          (tangent1 (find-value-by-index guest (elt (tangent-index triangle) 0)
                                         :what :tangent))
          (tangent2 (find-value-by-index guest (elt (tangent-index triangle) 1)
                                         :what :tangent))
          (tangent3 (find-value-by-index guest (elt (tangent-index triangle) 2)
                                         :what :tangent)))
      (let ((*default-epsilon* 1e-4))
        (normal-v  object normal1)
        (texel-v   object texture1)
        (tangent-v object tangent1)
        (vertex-v  object vertex1
                   :manifoldp manifold
                   :compact-vertices t
                   :gen-normal nil
                   :gen-triangle t)
        (normal-v  object normal2)
        (texel-v   object texture2)
        (tangent-v object tangent2)
        (vertex-v  object vertex2
                   :manifoldp manifold
                   :compact-vertices t
                   :gen-normal nil
                   :gen-triangle t)
        (normal-v  object normal3)
        (texel-v   object texture3)
        (tangent-v object tangent3)
        (vertex-v  object vertex3
                   :manifoldp manifold
                   :compact-vertices t
                   :gen-normal nil
                   :gen-triangle t)))))

(defmethod vertex-v ((object triangle-mesh) vec
                     &key (gen-triangle t) (gen-normal nil)
                       (compact-vertices nil)
                       (manifoldp t))
  (with-accessors ((vertices vertices)
                   (vertices-count vertices-count)
                   (cumulative-vertices-count cumulative-vertices-count)) object
    (with-slots (aabb) object
      (let ((transformed (transform-point vec (modelview-matrix object))))
        (vector-push-extend transformed vertices)
        (expand aabb transformed)
        (incf vertices-count)
        (incf cumulative-vertices-count)
        (when (and gen-triangle
                   (/= 0 cumulative-vertices-count)
                   (= (mod cumulative-vertices-count 3) 0))
          (if gen-normal
              (triangle-w-calculated-normal object
                                            :compact-vertices compact-vertices
                                            :manifoldp manifoldp)
              (triangle object :compact-vertices compact-vertices :manifoldp manifoldp)))))))

(defmethod vertex ((object triangle-mesh) x y z
                   &key (gen-triangle t) (gen-normal nil)
                     (compact-vertices nil)
                     (manifoldp nil))
  (vertex-v object (vec x y z)
            :gen-triangle gen-triangle
            :gen-normal gen-normal
            :compact-vertices compact-vertices
            :manifoldp manifoldp))

(defmethod texel ((object triangle-mesh) s-coord t-coord)
  (with-accessors ((texture-coord texture-coord)
                   (texture-count texture-count)) object
    (texel-v object (vec2 s-coord t-coord))))

(defmethod texel-v ((object triangle-mesh) coord)
  (with-accessors ((texture-coord texture-coord)
                   (texture-count texture-count)) object
    (vector-push-extend coord texture-coord)
    (incf texture-count)))

(defmethod normal-v ((object triangle-mesh) vec)
  (with-accessors ((normals normals)
                   (normals-count normals-count)) object
    (let ((transformed (transform-direction vec (modelview-matrix object))))
      (vector-push-extend transformed normals)
      (incf normals-count))))

(defmethod normal ((object triangle-mesh) x y z)
  (normal-v object (vec x y z)))


(defmethod tangent-v ((object triangle-mesh) vec)
  (with-accessors ((tangents tangents)
                   (tangents-count tangents-count)) object
    (let ((transformed (transform-direction vec (modelview-matrix object))))
      (vector-push-extend transformed tangents)
      (incf tangents-count))))

(defmethod tangent ((object triangle-mesh) x y z)
  (tangent-v object (vec x y z)))

(defmethod push-matrix ((object triangle-mesh) &key (what :modelview))
  (ecase what
    (:modelview
     (push (clone-matrix (modelview-matrix object)) (modelview-stack object)))))

(defmethod pop-matrix ((object triangle-mesh) &key (what :modelview))
  (ecase what
    (:modelview
     (setf (modelview-matrix object) (first (modelview-stack object)))
     (pop (modelview-stack object)))))

(defmethod load-matrix ((object triangle-mesh) matrix &key (what :modelview))
  (ecase what
    (:modelview
     (setf (modelview-matrix object) matrix))))

(defmethod mult-matrix ((object triangle-mesh) matrix &key (what :modelview))
  (ecase what
    (:modelview
     (setf (modelview-matrix object) (matrix* matrix (modelview-matrix object))))))

(defmethod %reset-data-renderer-count ((object triangle-mesh))
  (with-accessors ((renderer-data-count-vertices renderer-data-count-vertices)
                   (renderer-data-count-texture renderer-data-count-texture)
                   (renderer-data-count-normals renderer-data-count-normals)
                   (renderer-data-count-tangents renderer-data-count-tangents)) object
    (setf renderer-data-count-vertices 0
          renderer-data-count-texture  0
          renderer-data-count-normals  0
          renderer-data-count-tangents 0)))

(defmethod make-data-for-opengl ((object triangle-mesh))
  (with-accessors ((renderer-data-count-vertices renderer-data-count-vertices)
                   (renderer-data-count-texture renderer-data-count-texture)
                   (renderer-data-count-normals renderer-data-count-normals)
                   (renderer-data-count-tangents renderer-data-count-tangents)
                   (renderer-data-vertices renderer-data-vertices)
                   (renderer-data-texture renderer-data-texture)
                   (renderer-data-normals renderer-data-normals)
                   (renderer-data-tangents renderer-data-tangents)
                   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
                   (renderer-data-tangents-obj-space renderer-data-tangents-obj-space)
                   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
                   (vertices vertices)
                   (texture-coord texture-coord)
                   (normals normals)
                   (tangents tangents)
                   (triangles triangles)
                   (vbo vbo)
                   (vao vao)) object
    (%reset-data-renderer-count object)
    (setf renderer-data-aabb-obj-space (gl:alloc-gl-array :float +aabb-vertex-count+))
    (labels ((get-vertices (triangle results offset)
               (let* ((indices (vertex-index triangle))
                      (v1 (elt vertices (elt indices 0)))
                      (v2 (elt vertices (elt indices 1)))
                      (v3 (elt vertices (elt indices 2))))
                 (declare (vec v1 v2 v3))
                 (declare (uivec:uivec indices))
                 (declare (fixnum offset))
                 (setf (gl-utils:fast-glaref results offset)       (elt v1 0))
                 (setf (gl-utils:fast-glaref results (+ 1 offset)) (elt v1 1))
                 (setf (gl-utils:fast-glaref results (+ 2 offset)) (elt v1 2))
                 (setf (gl-utils:fast-glaref results (+ 3 offset)) (elt v2 0))
                 (setf (gl-utils:fast-glaref results (+ 4 offset)) (elt v2 1))
                 (setf (gl-utils:fast-glaref results (+ 5 offset)) (elt v2 2))
                 (setf (gl-utils:fast-glaref results (+ 6 offset)) (elt v3 0))
                 (setf (gl-utils:fast-glaref results (+ 7 offset)) (elt v3 1))
                 (setf (gl-utils:fast-glaref results (+ 8 offset)) (elt v3 2))
                 (incf renderer-data-count-vertices 9)))
             (get-text-coord (triangle results offset)
               (declare (fixnum offset))
               (let* ((indices (texture-index triangle))
                      (tex1 (elt texture-coord (elt indices 0)))
                      (tex2 (elt texture-coord (elt indices 1)))
                      (tex3 (elt texture-coord (elt indices 2))))
                 (setf (gl-utils:fast-glaref results offset)       (elt tex1 0))
                 (setf (gl-utils:fast-glaref results (+ 1 offset)) (elt tex1 1))
                 (setf (gl-utils:fast-glaref results (+ 2 offset)) (elt tex2 0))
                 (setf (gl-utils:fast-glaref results (+ 3 offset)) (elt tex2 1))
                 (setf (gl-utils:fast-glaref results (+ 4 offset)) (elt tex3 0))
                 (setf (gl-utils:fast-glaref results (+ 5 offset)) (elt tex3 1))
                 (incf renderer-data-count-texture 6)))
             (get-normals (triangle results offset)
               (declare (fixnum offset))
               (let ((norm1 (elt normals (elt (normal-index triangle) 0)))
                     (norm2 (elt normals (elt (normal-index triangle) 1)))
                     (norm3 (elt normals (elt (normal-index triangle) 2))))
                 (declare (vec norm1 norm2 norm3))
                 (setf (gl-utils:fast-glaref results offset)       (elt norm1 0))
                 (setf (gl-utils:fast-glaref results (+ 1 offset)) (elt norm1 1))
                 (setf (gl-utils:fast-glaref results (+ 2 offset)) (elt norm1 2))
                 (setf (gl-utils:fast-glaref results (+ 3 offset)) (elt norm2 0))
                 (setf (gl-utils:fast-glaref results (+ 4 offset)) (elt norm2 1))
                 (setf (gl-utils:fast-glaref results (+ 5 offset)) (elt norm2 2))
                 (setf (gl-utils:fast-glaref results (+ 6 offset)) (elt norm3 0))
                 (setf (gl-utils:fast-glaref results (+ 7 offset)) (elt norm3 1))
                 (setf (gl-utils:fast-glaref results (+ 8 offset)) (elt norm3 2))
                 (incf renderer-data-count-normals 9)))
             (get-tangent (triangle results offset)
               (declare (fixnum offset))
               (let ((tan1 (elt tangents (elt (tangent-index triangle) 0)))
                     (tan2 (elt tangents (elt (tangent-index triangle) 1)))
                     (tan3 (elt tangents (elt (tangent-index triangle) 2))))
                 (declare (vec tan1 tan2 tan3))
                 (setf (gl-utils:fast-glaref results offset)       (elt tan1 0))
                 (setf (gl-utils:fast-glaref results (+ 1 offset)) (elt tan1 1))
                 (setf (gl-utils:fast-glaref results (+ 2 offset)) (elt tan1 2))
                 (setf (gl-utils:fast-glaref results (+ 3 offset)) (elt tan2 0))
                 (setf (gl-utils:fast-glaref results (+ 4 offset)) (elt tan2 1))
                 (setf (gl-utils:fast-glaref results (+ 5 offset)) (elt tan2 2))
                 (setf (gl-utils:fast-glaref results (+ 6 offset)) (elt tan3 0))
                 (setf (gl-utils:fast-glaref results (+ 7 offset)) (elt tan3 1))
                 (setf (gl-utils:fast-glaref results (+ 8 offset)) (elt tan3 2))
                 (incf renderer-data-count-tangents 9))))
      (let ((gl-results-v   (gl:alloc-gl-array :float (* (length triangles) 9)))
            (gl-results-t   (gl:alloc-gl-array :float (* (length triangles) 6)))
            (gl-results-n   (gl:alloc-gl-array :float (* (length triangles) 9)))
            (gl-results-tan (gl:alloc-gl-array :float (* (length triangles) 9))))
        (loop
           for triangle in triangles
           for v-ct from 0 by 9
           for t-ct from 0 by 6
           for n-ct from 0 by 9
           for tan-ct from 0 by 9 do
             (get-vertices triangle gl-results-v v-ct)
             (get-text-coord triangle gl-results-t t-ct)
             (get-normals triangle gl-results-n n-ct)
             (get-tangent triangle gl-results-tan tan-ct))
        (setf renderer-data-vertices gl-results-v)
        (setf renderer-data-texture gl-results-t)
        (setf renderer-data-normals gl-results-n)
        (setf renderer-data-tangents gl-results-tan)
        (make-data-for-opengl-normals-obj-space object)
        (make-data-for-opengl-tangents-obj-space object)
        (make-data-for-opengl-aabb-obj-space object)
        ;; setup finalizer
        (let ((gl-arr-vert     (slot-value object 'renderer-data-vertices))
              (gl-arr-tex      (slot-value object 'renderer-data-texture))
              (gl-arr-norm     (slot-value object 'renderer-data-normals))
              (gl-arr-norm-obj (slot-value object 'renderer-data-normals-obj-space))
              (gl-arr-tan      (slot-value object 'renderer-data-tangents))
              (gl-arr-tan-obj  (slot-value object 'renderer-data-tangents-obj-space))
              (gl-arr-aabb     (slot-value object 'renderer-data-aabb-obj-space))
              (vbos            (slot-value object 'vbo))
              (vaos            (slot-value object 'vao))
              #+debug-mode
              (id              (slot-value object 'id)))
          (tg:finalize object
                       #'(lambda ()
                           #+debug-mode (misc:dbg "finalize destroy mesh ~a" id)
                           (free-memory* (list gl-arr-vert
                                               gl-arr-tex
                                               gl-arr-norm
                                               gl-arr-norm-obj
                                               gl-arr-tan
                                               gl-arr-tan-obj
                                               gl-arr-aabb)
                                         vbos vaos))))
        (do-children-mesh (i object)
          (make-data-for-opengl i))))))

(defmethod make-data-for-opengl-normals-obj-space ((object triangle-mesh))
  (with-accessors ((renderer-data-count-normals renderer-data-count-normals)
                   (triangles triangles)
                   (data-normals renderer-data-normals-obj-space)) object
    (let ((results-normals (misc:make-array-frame 0)))
      (do-triangles (triangle object)
        (loop
           for v across (vertex-index triangle)
           for n across (normal-index triangle) do
             (let* ((start-pos (find-value-by-index object v :what :vertex))
                    (end-pos (vec+ start-pos
                                   (find-value-by-index object n :what :normal))))
               (loop for i across start-pos do
                    (vector-push-extend i results-normals))
               (loop for i across end-pos do
                    (vector-push-extend i results-normals)))))
      (setf data-normals (gl-utils:seq->gl-array results-normals)))))

(defmethod make-data-for-opengl-tangents-obj-space ((object triangle-mesh))
  (with-accessors ((renderer-data-count-tangents renderer-data-count-tangents)
                   (triangles triangles)
                   (data-tangents renderer-data-tangents-obj-space)) object
    (let ((results-tangents (misc:make-array-frame 0)))
      (do-triangles (triangle object)
        (loop
           for v across (vertex-index triangle)
           for n across (tangent-index triangle) do
             (let* ((start-pos (find-value-by-index object v :what :vertex))
                    (end-pos (vec+ start-pos
                                   (find-value-by-index object n :what :tangent))))
               (loop for i across start-pos do
                    (vector-push-extend i results-tangents))
               (loop for i across end-pos do
                    (vector-push-extend i results-tangents)))))
      (setf data-tangents (gl-utils:seq->gl-array results-tangents)))))

(defmethod make-data-for-opengl-aabb-obj-space ((object triangle-mesh))
  (with-accessors ((aabb aabb) (frame renderer-data-aabb-obj-space)) object
    ;;      e          h
    ;;       +--------+
    ;;      /|       /|
    ;;     / |      / |
    ;; f  +--------+ g|
    ;;    |  |     |  |
    ;;    |a +-----|--+ d
    ;;    | /      | /
    ;;    |/       |/
    ;;  b +--------+ c
    ;;a->b
    (setf (gl-utils:fast-glaref frame 0)  (min-x aabb))
    (setf (gl-utils:fast-glaref frame 1)  (min-y aabb))
    (setf (gl-utils:fast-glaref frame 2)  (min-z aabb))
    (setf (gl-utils:fast-glaref frame 3)  (min-x aabb))
    (setf (gl-utils:fast-glaref frame 4)  (min-y aabb))
    (setf (gl-utils:fast-glaref frame 5)  (max-z aabb))
    ;; ;b->c
    (setf (gl-utils:fast-glaref frame 6)  (min-x aabb))
    (setf (gl-utils:fast-glaref frame 7)  (min-y aabb))
    (setf (gl-utils:fast-glaref frame 8)  (max-z aabb))
    (setf (gl-utils:fast-glaref frame 9)  (max-x aabb))
    (setf (gl-utils:fast-glaref frame 10) (min-y aabb))
    (setf (gl-utils:fast-glaref frame 11) (max-z aabb))
    ;;c->d
    (setf (gl-utils:fast-glaref frame 12) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 13) (min-y aabb))
    (setf (gl-utils:fast-glaref frame 14) (max-z aabb))
    (setf (gl-utils:fast-glaref frame 15) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 16) (min-y aabb))
    (setf (gl-utils:fast-glaref frame 17) (min-z aabb))
    ;;d->a
    (setf (gl-utils:fast-glaref frame 18) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 19) (min-y aabb))
    (setf (gl-utils:fast-glaref frame 20) (min-z aabb))
    (setf (gl-utils:fast-glaref frame 21) (min-x aabb))
    (setf (gl-utils:fast-glaref frame 22) (min-y aabb))
    (setf (gl-utils:fast-glaref frame 23) (min-z aabb))
    ;;e->f
    (setf (gl-utils:fast-glaref frame 24) (min-x aabb))
    (setf (gl-utils:fast-glaref frame 25) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 26) (min-z aabb))
    (setf (gl-utils:fast-glaref frame 27) (min-x aabb))
    (setf (gl-utils:fast-glaref frame 28) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 29) (max-z aabb))
    ;; f->g
    (setf (gl-utils:fast-glaref frame 30) (min-x aabb))
    (setf (gl-utils:fast-glaref frame 31) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 32) (max-z aabb))
    (setf (gl-utils:fast-glaref frame 33) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 34) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 35) (max-z aabb))
    ;;g->h
    (setf (gl-utils:fast-glaref frame 36) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 37) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 38) (max-z aabb))
    (setf (gl-utils:fast-glaref frame 39) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 40) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 41) (min-z aabb))
    ;;h->i
    (setf (gl-utils:fast-glaref frame 42) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 43) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 44) (min-z aabb))
    (setf (gl-utils:fast-glaref frame 45) (min-x aabb))
    (setf (gl-utils:fast-glaref frame 46) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 47) (min-z aabb))
    ;;e->a
    (setf (gl-utils:fast-glaref frame 48) (min-x aabb))
    (setf (gl-utils:fast-glaref frame 49) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 50) (min-z aabb))
    (setf (gl-utils:fast-glaref frame 51) (min-x aabb))
    (setf (gl-utils:fast-glaref frame 52) (min-y aabb))
    (setf (gl-utils:fast-glaref frame 53) (min-z aabb))
    ;;f->b
    (setf (gl-utils:fast-glaref frame 54) (min-x aabb))
    (setf (gl-utils:fast-glaref frame 55) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 56) (max-z aabb))
    (setf (gl-utils:fast-glaref frame 57) (min-x aabb))
    (setf (gl-utils:fast-glaref frame 58) (min-y aabb))
    (setf (gl-utils:fast-glaref frame 59) (max-z aabb))
    ;;g->c
    (setf (gl-utils:fast-glaref frame 60) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 61) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 62) (max-z aabb))
    (setf (gl-utils:fast-glaref frame 63) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 64) (min-y aabb))
    (setf (gl-utils:fast-glaref frame 65) (max-z aabb))
    ;;h->d
    (setf (gl-utils:fast-glaref frame 66) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 67) (max-y aabb))
    (setf (gl-utils:fast-glaref frame 68) (min-z aabb))
    (setf (gl-utils:fast-glaref frame 69) (max-x aabb))
    (setf (gl-utils:fast-glaref frame 70) (min-y aabb))
    (setf (gl-utils:fast-glaref frame 71) (min-z aabb))))

(defun free-memory (vertices texture normals normals-obj tan-obj aabb-obj vbo vao)
  (gl:free-gl-array vertices)
  (gl:free-gl-array texture)
  (gl:free-gl-array normals)
  (gl:free-gl-array normals-obj)
  (gl:free-gl-array tan-obj)
  (gl:free-gl-array aabb-obj)
  (gl:delete-vertex-arrays vao)
  (gl:delete-buffers vbo))

(defun free-memory* (arrays vbos vaos)
  (loop for i in arrays do
       (when (and i
                  (not (gl::null-pointer-p (gl::gl-array-pointer i))))
         (gl:free-gl-array i)
         (setf (gl::gl-array-pointer i) (gl::null-pointer))))
  (when vbos
    (gl:delete-buffers vbos))
  (when vaos
    (gl:delete-vertex-arrays vaos)))

(defmethod destroy ((object triangle-mesh))
  (with-accessors ((renderer-data-vertices renderer-data-vertices)
                   (renderer-data-texture renderer-data-texture)
                   (renderer-data-normals renderer-data-normals)
                   (renderer-data-tangents renderer-data-tangents)
                   (renderer-data-tangents-obj-space renderer-data-tangents-obj-space)
                   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
                   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
                   (vbo vbo) (vao vao)) object
    #+debug-mode (misc:dbg "destroy triangle mesh ~a" (id object))
    (setf vbo nil
          vao nil
          renderer-data-vertices nil
          renderer-data-texture nil
          renderer-data-normals nil
          renderer-data-normals-obj-space nil
          renderer-data-tangents nil
          renderer-data-tangents-obj-space nil
          renderer-data-aabb-obj-space     nil)
    (do-children-mesh (i object)
      (destroy i))))

(defun vbo-vertex-buffer-handle (vbo)
  (elt vbo 0))

(defun vbo-normals-buffer-handle (vbo)
  (elt vbo 1))

(defun vbo-tangents-buffer-handle (vbo)
  (elt vbo 2))

(defun vbo-texture-buffer-handle (vbo)
  (elt vbo 3))

(defun vbo-normals-object-space-buffer-handle (vbo)
  (elt vbo 4))

(defun vbo-tangents-object-space-buffer-handle (vbo)
  (elt vbo 5))

(defun vbo-aabb-object-space-buffer-handle (vbo)
  (elt vbo 6))

(defun vao-vertex-buffer-handle (vao)
  (elt vao 0))

(defun vao-normals-object-space-buffer-handle (vao)
  (elt vao 1))

(defun vao-tangents-object-space-buffer-handle (vao)
  (elt vao 2))

(defun vao-aabb-object-space-buffer-handle (vao)
  (elt vao 3))

(defmethod prepare-for-rendering ((object triangle-mesh))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (renderer-data-vertices renderer-data-vertices)
                   (renderer-data-normals renderer-data-normals)
                   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
                   (renderer-data-tangents renderer-data-tangents)
                   (renderer-data-tangents-obj-space renderer-data-tangents-obj-space)
                   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)
                   (renderer-data-texture renderer-data-texture)
                   (texture-object texture-object)
                   (normal-map normal-map)) object
    (destroy object)
    (setf vbo (gl:gen-buffers +vbo-count+)
          vao (gl:gen-vertex-arrays +vao-count+))
    (mesh:make-data-for-opengl object)
    ;; vertices
    (gl:bind-buffer :array-buffer (vbo-vertex-buffer-handle vbo))
    (gl:buffer-data :array-buffer :static-draw renderer-data-vertices)
    ;; normals
    (gl:bind-buffer :array-buffer (vbo-normals-buffer-handle vbo))
    (gl:buffer-data :array-buffer :static-draw renderer-data-normals)
    ;; tangents
    (gl:bind-buffer :array-buffer (vbo-tangents-buffer-handle vbo))
    (gl:buffer-data :array-buffer :static-draw renderer-data-tangents)
    ;; texture coordinates
    (gl:bind-buffer :array-buffer (vbo-texture-buffer-handle vbo))
    (gl:buffer-data :array-buffer :static-draw renderer-data-texture)
    ;; debug
    (gl:bind-buffer :array-buffer (vbo-normals-object-space-buffer-handle vbo))
    (gl:buffer-data :array-buffer :static-draw renderer-data-normals-obj-space)
    (gl:bind-buffer :array-buffer (vbo-tangents-object-space-buffer-handle vbo))
    (gl:buffer-data :array-buffer :static-draw renderer-data-tangents-obj-space)
    (gl:bind-buffer :array-buffer (vbo-aabb-object-space-buffer-handle vbo))
    (gl:buffer-data :array-buffer :dynamic-draw renderer-data-aabb-obj-space)
    (if normal-map
        (prepare-for-rendering-normal-map object)
        (prepare-for-rendering-phong      object))
    (do-children-mesh (i object)
      (prepare-for-rendering i))
    object))

(defmethod prepare-for-rendering-normal-map ((object triangle-mesh))
  (with-accessors ((vbo vbo) (vao vao)) object
    (with-unbind-vao
      (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
      ;; vertices
      (gl:bind-buffer :array-buffer (vbo-vertex-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-position-location+ 3 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-position-location+)
      ;; normals
      (gl:bind-buffer :array-buffer (vbo-normals-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-normal-location+ 3 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-normal-location+)
      ;; tangents
      (gl:bind-buffer :array-buffer (vbo-tangents-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-tangent-location+ 3 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-tangent-location+)
      ;; texture
      (gl:bind-buffer :array-buffer (vbo-texture-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-texture-location+ 2 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-texture-location+))))

(defmethod prepare-for-rendering-phong ((object triangle-mesh))
  (with-accessors ((vbo vbo) (vao vao)) object
    (with-unbind-vao
      (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
      ;; vertices
      (gl:bind-buffer :array-buffer (vbo-vertex-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-position-location+ 3 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-position-location+)
      ;; normals
      (gl:bind-buffer :array-buffer (vbo-normals-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-normal-location+ 3 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-normal-location+)
      ;; texture
      (gl:bind-buffer :array-buffer (vbo-texture-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-texture-location+ 2 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-texture-location+))))

(defmethod normals-obj-space-vertex-count ((object triangle-mesh))
  (* 18 (length (triangles object))))

(defmethod tangents-obj-space-vertex-count ((object triangle-mesh))
  (* 18 (length (triangles object))))

(defmethod calculate ((object triangle-mesh) dt)
  (with-accessors ((vbo vbo) (render-aabb render-aabb)
                   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)) object
    (bubbleup-modelmatrix object)
    (when render-aabb
      (make-data-for-opengl-aabb-obj-space object)
      (gl:bind-buffer :array-buffer (vbo-aabb-object-space-buffer-handle vbo))
      (gl:buffer-sub-data :array-buffer renderer-data-aabb-obj-space))
    (do-children-mesh (i object)
      (calculate i dt)))
  (call-next-method))

(defmethod bubbleup-modelmatrix ((object triangle-mesh))
  (let ((res      (matrix* (translate (pos     object))
                           (scale     (scaling object))
                           (quat->matrix (quat-rotate-to-vec +entity-forward-direction+
                                                             (dir object)
                                                             :fallback-axis +y-axe+))
                           (quat->matrix (quat-rotate-to-vec +entity-up-direction+
                                                             (up object)
                                                             :fallback-axis +z-axe+))))
        (par-mesh (parent object)))
    (when par-mesh
      (setf res (matrix* (elt (model-matrix par-mesh) 0) res)))
    (setf (model-matrix object) res)))

(defmethod render ((object triangle-mesh) renderer)
  (with-accessors ((normal-map normal-map)
                   (renderp renderp))      object
    (when (and renderp (rendering-needed-p object renderer))
      (if normal-map
          (render-normalmap object renderer)
          (render-phong object renderer))
      (do-children-mesh (c object)
        (render c renderer)))))

(defmethod render-phong ((object triangle-mesh) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)
                   (material-params material-params)
                   (current-time current-time)
                   (fog-density fog-density)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (use-program compiled-shaders :mesh-ads)
          (gl:active-texture :texture0)
          (texture:bind-texture texture-object)
          (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
          (uniformfv compiled-shaders :light-pos
                     (the vec (main-light-pos-eye-space renderer)))
          (uniformf  compiled-shaders :scale-text-coord 1.0)
          (uniformfv compiled-shaders :ia    #(1.0 1.0 1.0))
          (uniformfv compiled-shaders :id    (the vec (main-light-color renderer)))
          (uniformfv compiled-shaders :is    (the vec (main-light-color renderer)))
          (uniformf  compiled-shaders :ka    (ka material-params))
          (uniformf  compiled-shaders :kd    (kd material-params))
          (uniformf  compiled-shaders :ks    (ks material-params))
          (uniformf  compiled-shaders :shine (shininess material-params))
          (uniformf  compiled-shaders :time  current-time)
          (uniformf  compiled-shaders :fog-density fog-density)
          (uniform-matrix compiled-shaders :model-matrix 4 model-matrix nil)
          (uniform-matrix compiled-shaders :modelview-matrix 4
                                   (vector (matrix* camera-vw-matrix
                                                    (elt view-matrix 0)
                                                    (elt model-matrix 0)))
                                   nil)
          (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
          (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
          (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
          (render-debug object renderer))))))

(defmethod render-normalmap ((object triangle-mesh) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (normal-map normal-map)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)
                   (material-params material-params)
                   (current-time current-time)
                   (fog-density fog-density)) object
    (declare (texture:texture texture-object normal-map))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (use-program compiled-shaders :mesh-bump)
          (gl:active-texture :texture0)
          (texture:bind-texture texture-object)
          (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
          (gl:active-texture :texture1)
          (texture:bind-texture normal-map)
          (uniformi compiled-shaders :normal-map +texture-unit-normalmap+)
          (uniformfv compiled-shaders :light-pos
                              (the vec (main-light-pos-eye-space renderer)))
          (uniformfv compiled-shaders :ia    #(1.0 1.0 1.0))
          (uniformfv compiled-shaders :id    #(1.0 1.0 1.0))
          (uniformfv compiled-shaders :is    #(1.0 1.0 1.0))
          (uniformf  compiled-shaders :ka    (ka material-params))
          (uniformf  compiled-shaders :kd    (kd material-params))
          (uniformf  compiled-shaders :ks    (ks material-params))
          (uniformf  compiled-shaders :shine (shininess material-params))
          (uniformf  compiled-shaders :time  current-time)
          (uniformf  compiled-shaders :fog-density fog-density)
          (uniform-matrix compiled-shaders :model-matrix 4 model-matrix nil)
          (uniform-matrix compiled-shaders :modelview-matrix 4
                                   (vector (matrix* camera-vw-matrix
                                                    (elt view-matrix 0)
                                                    (elt model-matrix 0)))
                                   nil)
          (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
          (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
          (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
          (render-debug object renderer))))))

(alexandria:define-constant +debug-aabb-color+    §c0000ffff :test #'vec4~)

(alexandria:define-constant +debug-normal-color+  §cff00ffff :test #'vec4~)

(alexandria:define-constant +debug-tangent-color+ §c00ff00ff :test #'vec4~)

(defmethod render-debug ((object triangle-mesh) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)
                   (normals-obj-space-vertex-count normals-obj-space-vertex-count)
                   (tangents-obj-space-vertex-count tangents-obj-space-vertex-count)) object
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (declare (fixnum normals-obj-space-vertex-count tangents-obj-space-vertex-count))
    (with-camera-view-matrix (camera-vw-matrix renderer)
      (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
        (when (render-aabb object) ;; aabb
          (use-program compiled-shaders :mesh-debug)
          (uniformfv compiled-shaders :out-color +debug-aabb-color+)
          (uniform-matrix compiled-shaders :modelview-matrix  4
                          (vector (matrix* camera-vw-matrix
                                           (elt view-matrix 0)))
                          nil)
          (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
          (gl:bind-vertex-array (vao-aabb-object-space-buffer-handle vao))
          (gl:bind-buffer :array-buffer (vbo-aabb-object-space-buffer-handle vbo))
          (gl:vertex-attrib-pointer +attribute-position-location+ 3 :float 0 0
                                    (mock-null-pointer))
          (gl:enable-vertex-attrib-array +attribute-position-location+)
          (gl:draw-arrays :lines 0 +aabb-points-count+))
        (when (> (length triangles) 0)
          (when (render-normals object)  ;;normal obj space
            (use-program compiled-shaders :mesh-debug)
            (uniformfv compiled-shaders :out-color +debug-normal-color+)
            (uniform-matrix compiled-shaders :modelview-matrix 4
                            (vector (matrix* camera-vw-matrix
                                             (elt view-matrix 0)
                                             (elt model-matrix 0)))
                            nil)
            (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
            (gl:bind-vertex-array (vao-normals-object-space-buffer-handle vao))
            (gl:bind-buffer :array-buffer (vbo-normals-object-space-buffer-handle vbo))
            (gl:vertex-attrib-pointer +attribute-position-location+ 3
                                      :float 0 0 (mock-null-pointer))
            (gl:enable-vertex-attrib-array +attribute-position-location+)
            (gl:draw-arrays :lines 0 normals-obj-space-vertex-count))
          (when (render-tangents object)  ;;tangents obj space
            (use-program compiled-shaders :mesh-debug)
            (uniformfv compiled-shaders :out-color +debug-tangent-color+)
            (uniform-matrix compiled-shaders :modelview-matrix 4
                            (vector (matrix* camera-vw-matrix
                                             (elt view-matrix 0)
                                             (elt model-matrix 0)))
                            nil)
            (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
            (gl:bind-vertex-array (vao-tangents-object-space-buffer-handle vao))
            (gl:bind-buffer :array-buffer (vbo-tangents-object-space-buffer-handle vbo))
            (gl:vertex-attrib-pointer +attribute-position-location+ 3
                                      :float 0 0 (mock-null-pointer))
            (gl:enable-vertex-attrib-array +attribute-position-location+)
            (gl:draw-arrays :lines 0 tangents-obj-space-vertex-count)))))))

(defmacro with-new-triangle-candidate ((mesh triangle v1 v2 v3) &body body)
  (let ((v1->v2 (alexandria:format-symbol t "~:@(~a->~a~)" v1 v2))
        (v2->v1 (alexandria:format-symbol t "~:@(~a->~a~)" v2 v1))
        (v2->v3 (alexandria:format-symbol t "~:@(~a->~a~)" v2 v3))
        (v3->v2 (alexandria:format-symbol t "~:@(~a->~a~)" v3 v2))
        (v3->v1 (alexandria:format-symbol t "~:@(~a->~a~)" v3 v1))
        (v1->v3 (alexandria:format-symbol t "~:@(~a->~a~)" v1 v3)))
    `(multiple-value-bind (,v1->v2 ,v2->v1)
         (gen-new-triangle-edge ,mesh ,triangle ,v1 ,v2)
       (multiple-value-bind (,v2->v3 ,v3->v2)
           (gen-new-triangle-edge ,mesh ,triangle ,v2 ,v3)
         (multiple-value-bind (,v3->v1 ,v1->v3)
             (gen-new-triangle-edge ,mesh ,triangle ,v3 ,v1)
           (if (not (or (and (null ,v1->v2) (null ,v2->v1)) ;; ensure
                        ;; the
                        ;; edge
                        ;; does
                        ;; not
                        ;; exists
                        (and (null ,v2->v3) (null ,v3->v2))
                        (and (null ,v3->v1) (null ,v1->v3))))
               (progn
                 (cond
                   ((and ,v1->v2 ,v2->v1) ;; opposite edge exists
                    (setf (opposite-edge ,v2->v1) ,v1->v2)))
                 (cond
                   ((and ,v2->v3 ,v3->v2) ;; opposite edge exists
                    (setf (opposite-edge ,v3->v2) ,v2->v3)))
                 (cond
                   ((and ,v3->v1 ,v1->v3) ;; opposite edge exists
                    (setf (opposite-edge ,v1->v3) ,v3->v1)))
                 ,@body)))))))

(defmethod triangle ((object triangle-mesh) &key
                                              (v1 (- (vertices-count object) 3))
                                              (v2 (- (vertices-count object) 2))
                                              (v3 (- (vertices-count object) 1))
                                              (t1 (- (texture-count object) 3))
                                              (t2 (- (texture-count object) 2))
                                              (t3 (- (texture-count object) 1))
                                              (n1 (- (normals-count object) 3))
                                              (n2 (- (normals-count object) 2))
                                              (n3 (- (normals-count object) 1))
                                              (tan1 (- (tangents-count object) 3))
                                              (tan2 (- (tangents-count object) 2))
                                              (tan3 (- (tangents-count object) 1))
                                              (compact-vertices nil)
                                              (manifoldp t))
  (with-accessors ((vertices vertices) (texture-coord texture-coord)
                   (normals normals) (vertices-count vertices-count)
                   (texture-count texture-count)
                   (triangles triangles)
                   (edges edges)) object
    (let* ((v1-found (if compact-vertices
                         (find-index-by-value object (find-value-by-index object v1))
                         nil))
           (v2-found (if compact-vertices
                         (find-index-by-value object (find-value-by-index object v2))
                         nil))
           (v3-found (if compact-vertices
                         (find-index-by-value object (find-value-by-index object v3))
                         nil))
           (v1-new (or v1-found v1))
           (v2-new (or v2-found v2))
           (v3-new (or v3-found v3)))
      (let ((new-triangle (make-instance 'triangle
                                         :vertex-index  (uivec v1-new v2-new v3-new)
                                         :texture-index (uivec t1 t2 t3)
                                         :normal-index  (uivec n1 n2 n3)
                                         :tangent-index (uivec tan1 tan2 tan3))))
        ;; edges
        (cond
          ((and compact-vertices manifoldp)
           (with-new-triangle-candidate (object new-triangle v1-new v2-new v3-new)
             (vector-push-extend v1-new->v2-new edges)
             (vector-push-extend v2-new->v3-new edges)
             (vector-push-extend v3-new->v1-new edges)
             (setf (next-edge v1-new->v2-new) v2-new->v3-new
                   (next-edge v2-new->v3-new) v3-new->v1-new
                   (next-edge v3-new->v1-new) v1-new->v2-new)

             (setf (emitted-edges new-triangle)
                   (misc:list->simple-array (list v1-new->v2-new v2-new->v3-new
                                                  v3-new->v1-new)
                                            (make-instance 'edge)
                                            'edge))
             (push new-triangle (triangles object))))
          (compact-vertices
           (push new-triangle (triangles object)))
          (t
           (push new-triangle (triangles object))))))))

(defmethod triangle-w-calculated-normal ((object triangle-mesh) &key
                                                                  (v1 (- (vertices-count object) 3))
                                                                  (v2 (- (vertices-count object) 2))
                                                                  (v3 (- (vertices-count object) 1))
                                                                  (t1 (- (texture-count object) 3))
                                                                  (t2 (- (texture-count object) 2))
                                                                  (t3 (- (texture-count object) 1))
                                                                  (compact-vertices nil)
                                                                  (manifoldp t))
  (with-accessors ((vertices vertices) (texture-coord texture-coord)
                   (normals normals) (vertices-count vertices-count)
                   (texture-count texture-count)
                   (triangles triangles)
                   (edges edges)) object
    (let ((normal (triangle-normal (find-value-by-index object v1)
                                   (find-value-by-index object v2)
                                   (find-value-by-index object v3))))
      (loop repeat 3 do
           (normal-v object normal)
           (tangent object 1.0 0.0 0.0))
      (triangle object :v1 v1 :v2 v2 :v3 v3 :t1 t1 :t2 t2 :t3 t3
                :compact-vertices compact-vertices :manifoldp manifoldp))))

(defmacro with-found-edge ((edge-found oppositep) v1 v2 edges &body body)
  `(multiple-value-bind (,edge-found ,oppositep)
       (find-edge ,v1 ,v2 ,edges)
     ,@body))

(defmethod gen-new-triangle-edge ((object triangle-mesh) triangle v1 v2)
  (with-accessors ((edges edges)) object
    (with-found-edge (edge-v1->v2 oppositep) v1 v2 edges
      (cond
        ((and edge-v1->v2 oppositep) ; opposite found
         (let ((new-edge (make-edge v1 v2 edge-v1->v2 triangle nil)))
           (values new-edge edge-v1->v2)))
        ((and edge-v1->v2 (not oppositep)) ; already exists
         (values nil nil))
        (t ;not found
         (values (make-edge v1 v2 nil triangle nil) nil))))))

(defun test-edges ()
  (let ((*print-circle* t))
    (let* ((mesh (make-instance 'triangle-mesh)))
      (vertex mesh 0.0 0.0 0.0 :compact-vertices t :gen-triangle t :gen-normal t)
      (vertex mesh 1.0 0.0 0.0 :compact-vertices t :gen-triangle t :gen-normal t)
      (vertex mesh 0.0 1.0 0.0 :compact-vertices t :gen-triangle t :gen-normal t)
      (misc:dbg "primo triangolo ~a" (edges mesh))
      (vertex mesh 1.0 0.0 0.0 :compact-vertices t :gen-triangle t :gen-normal t)
      (vertex mesh 1.0 1.0 0.0 :compact-vertices t :gen-triangle t :gen-normal t)
      (vertex mesh 0.0 1.0 0.0 :compact-vertices t :gen-triangle t :gen-normal t)
      (misc:dbg "NEW edges ~{~a~%~%~}" (map 'list #'identity (edges mesh)))
      (misc:dbg "~%~%NEW triangles ~{~a~%~%~}" (map 'list #'identity (triangles mesh)))
      (misc:dbg "exampleedge ~a"  (emitted-edges (first (triangles mesh))))

      (misc:dbg "example ~a" (find-vertex-attribute-by-vertex-index-alist (first (triangles mesh))
                                                                          4)))))

(defun find-edge (v1 v2 edges)
  (let ((edge-exists (find-if #'(lambda (a) (and (= (from a) v1) (= (to a) v2))) edges)))
    (if edge-exists
        (values edge-exists nil)
        (let ((opposite-edge-exists (find-if #'(lambda (a) (and (= (from a) v2) (= (to a) v1)))
                                             edges)))
          (if opposite-edge-exists
              (values opposite-edge-exists t)
              (values nil nil))))))

(defmethod find-triangle-by-vertex-index ((object triangle-mesh) vertex)
  (find-if #'(lambda (triangle) (find vertex (vertex-index triangle)))
           (triangles object)))

(defmethod find-all-triangles-by-vertex-index ((object triangle-mesh) vertex)
  (labels ((%find (seq vertex)
             (let ((pos (position vertex seq
                                  :test #'(lambda (v tr)
                                            (find v (vertex-index tr) :test #'=)))))
               (if pos
                   (concatenate 'list
                                (list (elt seq pos))
                                (%find (misc:delete@ seq pos) vertex))
                   nil))))
    (%find (triangles object) vertex)))

(defmethod remove-orphaned-vertices ((object triangle-mesh))
  (with-accessors ((triangles triangles) (vertices vertices) (edges edges)
                   (vertices-count vertices-count)) object
    (let ((index-list '()))
      (loop for tr in (triangles object) do
           (loop for i across (vertex-index tr) do
                (pushnew i index-list :key #'identity :test #'=)))
      (let ((new-vertices (loop for i in index-list collect (elt vertices i))))
        (setf vertices (misc:copy-list-into-array new-vertices
                                                  (init-vertices-slot (length new-vertices))))
        (setf vertices-count (length vertices))
        (loop for i in triangles do
             (setf (vertex-index i)
                   (map-into
                    (uivec:uivec 0 0 0)
                    #'(lambda (i) (position i index-list
                                            :test #'=))
                    (vertex-index i))))
        (loop for i across edges do
             (setf (from i) (position (from i) index-list)
                   (to i) (position (to i) index-list)))))))

(defmethod make-shared-vertices ((object triangle-mesh) epsilon)
  (with-accessors ((triangles triangles) (vertices vertices)) object
    (let ((*default-epsilon* epsilon))
      (loop
         for tr-a in (triangles object)
         for tr-index from 1 do ;; for each probe triangle
           (loop for vertex-a in (vertex-index tr-a) do
                (loop
                   for tr-b in (triangles object)
                   for ptr-tr-b from 0 do ;; for each examinated triangle
                     (loop
                        for vertex-b across (vertex-index tr-b)
                        for ptr-index-tr-b from 0 do
                          (let ((pos-a (find-value-by-index object vertex-a))
                                (pos-b (find-value-by-index object vertex-b)))
                            (when (and (not (= vertex-a vertex-b))
                                       (every #'epsilon= pos-a pos-b))
                              (setf (elt (vertex-index (elt triangles ptr-tr-b))
                                         ptr-index-tr-b)
                                    vertex-a))))))))))

(defun fill-gaps-between-meshes (branch trunk epsilon)
  (with-epsilon (epsilon)
    (do-triangles-vertex-index (triangle-branch idx-branch branch)
      (do-triangles-vertex-index (triangle-trunk idx-trunk trunk)
        (let* ((pos-branch (find-value-by-index branch idx-branch))
               (pos-trunk (find-value-by-index trunk idx-trunk))
               (diff (vec-length (vec- pos-branch pos-trunk))))
          (when (epsilon= diff epsilon)
            (setf (elt (vertices branch) idx-branch) pos-trunk)))))))

(defmethod average-normals ((object triangle-mesh))
  (with-accessors ((triangles triangles) (vertices vertices)
                   (normals normals)) object
    (setf normals (init-normals-slot (length normals)))
    (do-triangles (triangle object)
      (let* ((vertices-idx (vertex-index triangle))
             (face-normal (triangle-normal (elt vertices (elt vertices-idx 0))
                                           (elt vertices (elt vertices-idx 1))
                                           (elt vertices (elt vertices-idx 2))
                                           :normalize nil)))
        (do-triangles-vertex-index (triangle-norm index object)
          (when (find index vertices-idx :test #'=)
            (let ((normal-index (cdr (assoc
                                      +normal-prop+
                                      (find-vertex-attribute-by-vertex-index-alist
                                       triangle-norm index)))))
              (setf (elt normals normal-index)
                    (vec+ (elt normals normal-index) face-normal)))))))
    (loop for i from 0 below (length normals) do
         (setf (elt normals i) (normalize (elt normals i))))
    (do-children-mesh (child object)
      (average-normals child))))

(defmethod average-normals-by-shared-vertices ((object triangle-mesh))
  (with-accessors ((triangles triangles) (vertices vertices)
                   (normals normals)) object

    (let ((visited-vertices (misc:make-array-frame 0)))
      (labels ((%find (chosen-triangle)
                 (loop for vertex across (vertex-index chosen-triangle)
                    for normal across (normal-index chosen-triangle) do
                      (when (not (find vertex visited-vertices :test #'=))
                        (vector-push-extend vertex visited-vertices)
                        (let* ((vertex-shared-tris (find-all-triangles-by-vertex-index object
                                                                                       vertex))
                               (normals-to-average-idx (loop for i in vertex-shared-tris collect
                                                            (elt
                                                             (position vertex (vertex-index i)
                                                                       :test #'=)
                                                             (normal-index i))))
                               (normals-to-average (do ((normals normals-to-average-idx (rest normals))
                                                        (new-normals '()))
                                                       ((not normals) new-normals)
                                                     (let ((new-normal
                                                            (find-value-by-index
                                                             object (first normals)
                                                             :what :normal)))
                                                       (pushnew new-normal new-normals
                                                                :test #'vec~))))
                               (average (normalize (reduce #'(lambda (a b) (vec+ a b))
                                                           normals-to-average
                                                           :initial-value +zero-vec+))))

                          ;;(misc:dbg "for vertex ~a shared:~%~{~a~%~} " vertex vertex-shared-tris)
                          ;; (misc:dbg "normal shared ~a average ~a ~a" normals-to-average-idx average
                          ;;        normals-to-average)
                          (loop for i in normals-to-average-idx do
                               (setf (elt (normals object) i) average))
                          (loop for i in vertex-shared-tris do (%find i)))))))
        (loop for i in triangles do
             (%find i))
        (%make-shared-normal-based-on-shared-vertices object)))))

(defmethod gen-tangents ((object triangle-mesh) &key (epsilon 1e-7))
  (with-accessors ((triangles triangles) (vertices vertices) (texture-coord texture-coord)
                   (normals normals)
                   (tangents tangents) (tangents-count tangents-count)) object
    (assert (= (length tangents) (length normals)))
    (setf tangents (init-tangents-slot (length tangents)))
    (do-triangles (triangle object)
      (let* ((vertices-idx (vertex-index triangle))
             (texels-idx   (texture-index triangle))
             (face-tangent (tangent-in-normal-space (elt vertices (elt vertices-idx 0))
                                                    (elt vertices (elt vertices-idx 1))
                                                    (elt vertices (elt vertices-idx 2))
                                                    (elt texture-coord (elt texels-idx 0))
                                                    (elt texture-coord (elt texels-idx 1))
                                                    (elt texture-coord (elt texels-idx 2))
                                                    :normalize nil)))
        (do-triangles-vertex-index (triangle-tang index object)
          ;; for any triangles and for any vertex
          (when (find index vertices-idx :test #'=)
            (let* ((tangent-index (cdr (assoc
                                        +tangent-prop+
                                        (find-vertex-attribute-by-vertex-index-alist
                                         triangle-tang index))))
                   (sum (vec+ (elt tangents tangent-index) face-tangent))
                   (new-tangent (if (vec~ sum +zero-vec+ epsilon)
                                    (elt tangents tangent-index)
                                    sum)))
              (setf (elt tangents tangent-index) new-tangent))))))
    (loop for i from 0 below (length tangents) do
         (setf (elt tangents i) (normalize (elt tangents i))))
    (do-children-mesh (child object)
      (gen-tangents child))))

(defmethod %make-shared-normal-based-on-shared-vertices ((object triangle-mesh))
  (do-triangles-vertex-index (triangle-ref vertex-ref object)
    (let ((reference-normal (elt (find-vertex-attibute-by-vertex-index triangle-ref vertex-ref) 2)))
      (loop
         for i from 0 below (length (triangles object))
         for tr in (triangles object) do
           (loop
              for v across (vertex-index tr)
              for j from 0 do
                (when (and (= v vertex-ref)
                           (/= (elt (normal-index (elt (triangles object) i)) j) reference-normal))
                  (setf (elt (normal-index (elt (triangles object) i)) j) reference-normal)))))))

(defmethod average-normals-if-near-vertices ((branch triangle-mesh) (trunk triangle-mesh)
                                             (epsilon single-float))
  (do-triangles-vertex-index (triangle-trunk index-vertex-trunk trunk)
    (let ((trunk-vertex (elt (vertices trunk) index-vertex-trunk))
          (normal-index-trunk (find-normal-associed-to-vertex-index
                               triangle-trunk index-vertex-trunk))
          (similar-indices-in-branch '())
          (similar-indices-in-trunk '()))
      (do-triangles-vertex-index (triangle-branch index-vertex-branch branch)
        (when (vec~ trunk-vertex (elt (vertices branch) index-vertex-branch)
                    epsilon) ;; se sono simili le posizioni
          (let ((normal-index-branch
                 (find-normal-associed-to-vertex-index
                  triangle-branch index-vertex-branch)))
            (push normal-index-branch similar-indices-in-branch))))
      (when similar-indices-in-branch
        (do-triangles-vertex-index (triangle-trunk2 index-vertex-trunk2 trunk)
          (when (= index-vertex-trunk2 index-vertex-trunk)
            (let ((normal-index-trunk (find-normal-associed-to-vertex-index
                                       triangle-trunk2 index-vertex-trunk2)))
              (push normal-index-trunk similar-indices-in-trunk))))
        (let* ((sum (vec+ (elt (normals trunk) normal-index-trunk)
                          (elt (normals branch) (first similar-indices-in-branch))))
               (new-normal (if (vec~ sum +zero-vec+ epsilon)
                               (elt (normals trunk) normal-index-trunk)
                               (normalize sum))))
          (loop for i in similar-indices-in-branch do
               (setf (elt (normals branch) i) new-normal))
          (loop for i in similar-indices-in-trunk do
               (setf (elt (normals trunk) i) new-normal)))))))

(defmethod recalculate-face-normals ((object triangle-mesh))
  (with-accessors ((triangles triangles) (vertices vertices) (normals normals)) object
    (do-triangles (triangle object)
      (let* ((a (elt (vertex-index triangle) 0))
             (b (elt (vertex-index triangle) 1))
             (c (elt (vertex-index triangle) 2))
             (new-face-normal (triangle-normal (find-value-by-index object a)
                                               (find-value-by-index object b)
                                               (find-value-by-index object c))))
        (loop for i across (normal-index triangle) do
             (setf (elt (normals object) i) new-face-normal))))))

(defun %new-face-point (mesh triangle)
  (let* ((all-vertex-index (vertex-index triangle))
         (all-vertex (mapcar #'(lambda (a) (find-value-by-index mesh a)) all-vertex-index)))
    (vec-average* all-vertex)))

(defmethod subdivide-face ((object triangle-mesh) (face triangle))
  ;;           V1                              P1
  ;;           .                     .---------.---------.
  ;;          / \                     \       / \       /
  ;;         /   \              face ----------> \     /
  ;;        /     \                     \   /     \   /
  ;;       /       \                     \ /   NV  \ /
  ;;  NV1 o---------o NV3             U1  .----o----. U2
  ;;     / \       / \                   / \       / \
  ;;    /   \     /   \  opposite-face ------->   /   \
  ;;   /     \   /     \               /     \   /     \
  ;;  /       \ /       \             /       \ /       \
  ;; .---------o---------.           .---------.---------.
  ;; V2        NV2       V3                    P2
  ;; NV = 3/8 (U1 + U2) + 1/8 (V1 + V2)
  (with-accessors ((vertices vertices) (texture-coord texture-coord)) object
    (let ((nvs (loop for edge across (emitted-edges face) collect
                    (let* ((u1  (from edge))
                           (u2  (to edge))
                           (p1  (to (next-edge edge)))
                           (p2  (to (next-edge (opposite-edge edge))))
                                        ;(opposite-face (face (opposite-edge edge)))
                           (tu1 (find-texture-associed-to-vertex-index face u1))
                           (tu2 (find-texture-associed-to-vertex-index face u2))
                                        ;(tp1 (find-texture-associed-to-vertex-index face p1))
                                        ;(tp2 (find-texture-associed-to-vertex-index opposite-face p2))
                           (nv  (vec+ (vec* (vec+ (elt vertices u1) (elt vertices u2)) 0.375)
                                      (vec* (vec+ (elt vertices p1) (elt vertices p2)) 0.125)))
                           (nt  (vec2+ (vec2* (elt texture-coord tu1) 0.5)
                                       (vec2* (elt texture-coord tu2) 0.5))))
                      ;; (misc:dbg "u1 ~a u2 ~a p1 ~a p2 ~a tu1 ~a tu2 ~a tp1 ~a tp2 ~a nv ~a nt ~a" u1 u2 p1 p2
                      ;;                tu1 tu2 tp1 tp2 nv nt)
                      (cons nv nt)))))
      (destructuring-bind (nv1 nv2 nv3) nvs
        (destructuring-bind (v1 v2 v3)  (loop for i across (vertex-index face) collect
                                             (elt vertices i))
          (destructuring-bind (t1 t2 t3) (loop for i across (texture-index face) collect
                                              (elt texture-coord i))
            (let ((res (misc:make-array-frame 4 nil 'cons t)))
              (setf (elt res 0) (cons (vector v1 (car nv1) (car nv3))
                                      (vector t1 (cdr nv1) (cdr nv3)))
                    (elt res 1) (cons (vector (car nv1) v2 (car nv2))
                                      (vector (cdr nv1) t2 (cdr nv2)))
                    (elt res 2) (cons (vector (car nv3) (car nv2) v3)
                                      (vector (cdr nv3) (cdr nv2) t3))
                    (elt res 3) (cons (vector (car nv1) (car nv2) (car nv3))
                                      (vector (cdr nv1) (cdr nv2) (cdr nv3))))
              res)))))))

(defmethod subdivide-mesh ((object triangle-mesh)
                           &key (texture-object (texture-object object)))
  (let ((results (make-instance 'triangle-mesh)))
    (setf (texture-object results) texture-object)
    (do-triangles (face object)
      (let ((subdivision (subdivide-face object face)))
        (loop for all across subdivision do
                                        ;(misc:dbg "all ~a" all)
             (let ((vertices (car all))
                   (texture-coordinates (cdr all)))
               (loop
                  for vertex across vertices
                  for texel across texture-coordinates do
                    (normal-v results (vec 0.0 0.0 0.0))
                    (texel-v results texel)
                    (vertex-v results vertex :manifoldp t :compact-vertices t
                              :gen-triangle t))))))
    (setf (children results)
          (misc:list->array
           (loop for i across (children object) collect
                (subdivide-mesh i :texture-object texture-object))))
    results))

(defmethod texture-plane-mapping ((object triangle-mesh))
  "Broken"
  (with-accessors ((normals normals)
                   (texture-coord texture-coord)) object
    (average-normals object)
    (do-triangles (triangle object)
      (loop
         for pointer-t across (texture-index triangle)
         for pointer-n across (normal-index triangle) do
           (let* ((n (elt normals pointer-n))
                  (nw-t (vec2 (d+ (d/ (elt n 0) 2.0) 0.5) (d+ (d/ (elt n 1) 2.0) 0.5))))
             (setf (elt texture-coord pointer-t) nw-t))))))

(defmethod smooth-mesh ((object triangle-mesh) &key (average-normals t) (plane-mapping t))
  ;;       N1       N2
  ;;       .-------.
  ;;      / \     / \
  ;;     /   \   /   \
  ;;    /     \ / V   \
  ;; N6 .------o-------. N3
  ;;    \     / \     /
  ;;     \   /   \   /
  ;;      \ /     \ /
  ;;       .-------.
  ;;       N5      N4
  ;;Vsmoothed = (1- valence * smooth-w) * V + smooth-w * (N_1 + N_2 + N_3 + ... + N_valence)
  (with-accessors ((vertices vertices)
                   (normals normals)
                   (texture-coord texture-coord)) object
    (do-triangles-vertex-index (face vertex-index object)
      (let* ((old-v (elt vertices vertex-index))
             (near (get-first-near object vertex-index))
             (valence (length near))
             (smooth-factor (loop-smooth-weight valence)))
        (setf (elt vertices vertex-index)
              (vec+
               (vec* old-v (d- 1.0 (d* (desired valence) smooth-factor)))
               (vec* (reduce #'(lambda (a b) (vec+ a (elt vertices b))) near
                             :initial-value +zero-vec+)
                     smooth-factor)))))
    (cond
      ((and average-normals plane-mapping)
       (texture-plane-mapping object))
      ((and average-normals (not plane-mapping))
       (average-normals object)))
    (do-children-mesh (child object)
      (smooth-mesh child :average-normals average-normals :plane-mapping plane-mapping))))

(defun loop-smooth-weight (valence)
  (d* (d/ 1.0 (desired valence))
      (d- (d/ 5.0 8.0)
          (dexpt
           (d+ (d/ 3.0 8.0)
               (d* (d/ 1.0 4.0)
                   (dcos (d/ +2pi+ (desired valence)))))
           2.0))))

(defmethod flatten-mesh ((object triangle-mesh))
  "Note: only the root's texture, shaders and normal map will be preserved"
  (let* ((res (make-instance 'triangle-mesh
                             :texture-object    (texture-object    object)
                             :normal-map        (normal-map        object)
                             :texture-projector (texture-projector object)
                             :projector         (projector         object)
                             :render-normals    (render-normals    object)
                             :render-aabb       (render-aabb       object)
                             :render-tangents   (render-tangents   object))))
    (top-down-visit object #'(lambda (n) (merge-mesh res n :manifold nil)))
    res))

(defmethod manifold-maybe ((object triangle-mesh))
  (and (> (length (triangles object)) 0)
       (every #'(lambda (triangle)
                  (every #'(lambda (edge) (not (null edge))) (emitted-edges triangle)))
              (triangles object))))

(defmethod clone-into :after ((from triangle-mesh) (to triangle-mesh))
  (with-slots (aabb) from
    (setf (texture-object            to) (texture-object    from)
          (normal-map                to) (normal-map        from)
          (texture-projector         to) (texture-projector from)
          (render-normals            to) (render-normals    from)
          (render-aabb               to) (render-aabb       from)
          (render-tangents           to) (render-tangents   from)
          (material-params           to) (clone (material-params from))
          (projector                 to) (and (projector from)
                                              (clone-matrix (projector from)))
          (triangles                 to) (map 'list #'clone (triangles from))
          (normals                   to) (alexandria:copy-array (normals from))
          (normals-count             to) (normals-count from)
          (vertices                  to) (alexandria:copy-array (vertices from))
          (vertices-count            to) (vertices-count from)
          (tangents                  to) (alexandria:copy-array (tangents from))
          (tangents-count            to) (tangents-count from)
          (cumulative-vertices-count to) (cumulative-vertices-count from)
          (texture-coord             to) (alexandria:copy-array (texture-coord from))
          (texture-count             to) (texture-count from)
          (edges                     to) (alexandria:copy-array (edges from))
          (aabb                      to) (clone aabb))
    (setf (children to)
          (map (type-of (children from))
               #'(lambda (a)
                   (let ((clone (make-instance 'triangle-mesh)))
                     (clone-into a clone)
                     (setf (parent clone) to)
                     clone))
               (children from)))
    to))

(defmethod clone ((object triangle-mesh))
  (with-simple-clone (object 'triangle-mesh)))

(defmethod copy-flat-into :after ((from triangle-mesh) (to triangle-mesh))
  (with-slots (aabb) from
    (setf (texture-object            to) (texture-object    from)
          (normal-map                to) (normal-map        from)
          (texture-projector         to) (texture-projector from)
          (render-normals            to) (render-normals    from)
          (render-aabb               to) (render-aabb       from)
          (render-tangents           to) (render-tangents   from)
          (material-params           to) (material-params from)
          (projector                 to) (projector from)
          (triangles                 to) (triangles from)
          (normals                   to) (normals from)
          (normals-count             to) (normals-count from)
          (vertices                  to) (vertices from)
          (vertices-count            to) (vertices-count from)
          (tangents                  to) (tangents from)
          (tangents-count            to) (tangents-count from)
          (cumulative-vertices-count to) (cumulative-vertices-count from)
          (texture-coord             to) (texture-coord from)
          (texture-count             to) (texture-count from)
          (edges                     to) (edges from)
          (aabb                      to) aabb)
    (setf (children to)
          (map (type-of (children from))
               #'(lambda (a)
                   (let ((clone (make-instance 'triangle-mesh)))
                     (copy-flat-into a clone)
                     (setf (parent clone) to)
                     clone))
               (children from)))
    to))

(defmethod copy-flat ((object triangle-mesh))
  (with-simple-copy-flat (object 'triangle-mesh)))

(defmethod save-mesh ((object triangle-mesh) place)
  (with-open-file (stream place :direction :output
                          :if-exists :supersede :if-does-not-exist :create)
    (let ((saved-texture-path    (if (texture-object object)
                                     (texture:filename (texture-object object))
                                     nil))
          (saved-normal-map-path (if (normal-map object)
                                     (texture:filename (normal-map object))
                                     nil)))
      (unwind-protect
           (progn
             (when saved-texture-path
               (setf (texture:filename (texture-object object))
                     (text-utils:strip-prefix saved-texture-path config:+sys-data-dir+)))
             (when saved-normal-map-path
               (setf (texture:filename (normal-map object))
                     (text-utils:strip-prefix saved-normal-map-path config:+sys-data-dir+)))
             (write-sequence (format nil "~s" (marshal:marshal object))
                             stream))
        (setf (texture:filename (texture-object object)) saved-texture-path)))))

(defmethod load-mesh ((object triangle-mesh) place)
  (declare (ignore object))
  (let ((res (make-instance 'triangle-mesh)))
    (with-open-file (stream place :direction :input :if-does-not-exist :error)
      (setf res (marshal:unmarshal (read stream)))
      (when (texture-object res)
        (pixmap::sync-data-to-bits (texture-object res))
        (texture:gen-name-and-inject-in-database (texture-object res))
        (texture:prepare-for-rendering (texture-object res)))
      (when (normal-map res)
        (pixmap::sync-data-to-bits (normal-map res))
        (texture:gen-name-and-inject-in-database (normal-map res))
        (texture:prepare-for-rendering (normal-map res)))
      res)))

(defmethod parent-labyrinth ((object triangle-mesh))
  (handler-case
      (parent-labyrinth (mtree:parent object))
    (error ()
      nil)))

(defmethod entity-dead-p ((object triangle-mesh))
  (with-accessors ((ghost ghost)) object
    (or (d< (character:current-damage-points ghost) 0.0)
        (epsilon= (character:current-damage-points ghost) 0.0))))

(alexandria:define-constant +magic-num-md2-tag-file '(74 68 80 50) :test #'equalp)

(alexandria:define-constant +tag-file-name-size+ 64 :test #'=)

(define-condition md2-tag-error (text-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~a" (text condition)))))

(defclass md2-tag () ())

(define-offset-size mesh tag (id 0 4) (version 4 4) (num-tags 8 4)
                    (num-frames 12 4) (offset-names 16 4) (offset-tags 20 4)
                    (offset-end 24 4) (offset-extract-end 28 4))

(define-parse-header-chunk (tag-id +tag-id-offset+ +tag-id-size+ md2-tag nil))

(define-parse-header-chunk (tag-num +tag-num-tags-offset+ +tag-num-tags-size+ md2-tag nil))

(define-parse-header-chunk (frames-num +tag-num-frames-offset+ +tag-num-frames-size+
                                            md2-tag nil))

(define-parse-header-chunk (offset-names +tag-offset-names-offset+
                                              +tag-offset-names-size+
                                              md2-tag nil))

(define-parse-header-chunk (offset-tags +tag-offset-tags-offset+
                                              +tag-offset-tags-size+
                                              md2-tag nil))

(defun load-tag-file (file)
  "returns ((\"tag\" . #(rotation1 rotation2 rotation3 origin) ...))"
  (let ((tag-results (make-instance 'md2-tag))
        (results '()))
      (with-open-file (stream file :direction :input :if-does-not-exist :error
                              :element-type '(unsigned-byte 8))
        (if (equalp +magic-num-md2-tag-file (parse-tag-id tag-results stream))
            (let* ((num-tags     (misc:byte->int (parse-tag-num tag-results stream)))
                   (num-frames   (misc:byte->int (parse-frames-num tag-results stream)))
                   (offset-names (misc:byte->int (parse-offset-names tag-results stream)))
                   (offset-tags  (misc:byte->int (parse-offset-tags tag-results stream)))
                   (names (loop
                            repeat num-tags
                            for seek-pos from offset-names by +tag-file-name-size+ collect
                              (text-utils:clean-unprintable-chars
                               (misc:bytes->string (misc:read-list stream +tag-file-name-size+
                                                                   :offset seek-pos))))))
              (file-position stream offset-tags)
              (loop for name in names do
                   (let ((orientation (loop for j from 0 below num-frames collect
                                           (vector
                                            (vec (misc:read-ieee-float-32 stream) ;; rotation1
                                                 (misc:read-ieee-float-32 stream)
                                                 (misc:read-ieee-float-32 stream))
                                            (vec (misc:read-ieee-float-32 stream) ;; rotation2
                                                 (misc:read-ieee-float-32 stream)
                                                 (misc:read-ieee-float-32 stream))
                                            (vec (misc:read-ieee-float-32 stream) ;; rotation3
                                                 (misc:read-ieee-float-32 stream)
                                                 (misc:read-ieee-float-32 stream))
                                            (vec (misc:read-ieee-float-32 stream) ;; origin
                                                 (misc:read-ieee-float-32 stream)
                                                 (misc:read-ieee-float-32 stream))))))
                     (push (cons name
                                 (list->simple-array orientation nil
                                                     '(simple-array
                                                       (simple-array single-float (3)) (4))))
                           results)))
              results)
            (error 'md2-tag-error
                   :text (format nil "Wrong magic number ~a"
                                 (parse-tag-id tag-results stream)))))))

(defun tag->matrix (orientation)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare ((simple-array (simple-array single-float (3)) (4)) orientation))
  (let ((orien (elt orientation 0))
        (r1    (elt orientation 1))
        (r2    (elt orientation 2))
        (r3    (elt orientation 3)))
    (sb-cga:matrix (elt r1 0)    (elt r2 1)     (elt r3 2)    (elt orien 0)
                   (elt r1 0)    (elt r2 1)     (elt r3 2)    (elt orien 1)
                   (elt r1 0)    (elt r2 1)     (elt r3 2)    (elt orien 2)
                   0.0           0.0            0.0              1.0)))

(defun nsetup-tag-matrix (matrix orientation)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare ((simple-array (simple-array single-float (3)) (4)) orientation))
  (declare (sb-cga:matrix matrix))
  (let ((r1    (elt orientation 0))
        (r2    (elt orientation 1))
        (r3    (elt orientation 2))
        (orien (elt orientation 3)))
    (setf (mref matrix 0 0) (elt r1 0)
          (mref matrix 1 0) (elt r1 1)
          (mref matrix 2 0) (elt r1 2)
          (mref matrix 3 0) 0.0
          ;; second column
          (mref matrix 0 1) (elt r2 0)
          (mref matrix 1 1) (elt r2 1)
          (mref matrix 2 1) (elt r2 2)
          (mref matrix 3 1) 0.0
          ;; third column
          (mref matrix 0 2) (elt r3 0)
          (mref matrix 1 2) (elt r3 1)
          (mref matrix 2 2) (elt r3 2)
          (mref matrix 3 2) 0.0
          ;; fourth colum
          (mref matrix 0 3) (elt orien 0)
          (mref matrix 1 3) (elt orien 1)
          (mref matrix 2 3) (elt orien 2)
          (mref matrix 3 3) 1.0)
    matrix))

(defun matrix->tag (matrix)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (sb-cga:matrix matrix))
  (let ((r1     (vec (mref matrix 0 0)
                     (mref matrix 1 0)
                     (mref matrix 2 0)))
        (r2     (vec (mref matrix 0 1)
                     (mref matrix 1 1)
                     (mref matrix 2 1)))
        (r3     (vec (mref matrix 0 2)
                     (mref matrix 1 2)
                     (mref matrix 2 2)))
        (orient (vec (mref matrix 0 3)
                     (mref matrix 1 3)
                     (mref matrix 2 3))))
    (values r1 r2 r3 orient)))

(defun find-tag-cdr (key tags-list)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (cdr (find-tag key tags-list)))

(defun find-tag (key tags-list)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (assoc key tags-list :test #'string=))

(defmethod load-tags ((object triangle-mesh) file)
  (with-accessors ((tags-table tags-table) (tags-matrices tags-matrices)) object
    (restart-case
        (progn
          (setf tags-table (load-tag-file file))
          (when tags-table
            (let ((matrices (loop for name in tags-table collect
                                 (cons (car name) (sb-cga:identity-matrix)))))
              (setf tags-matrices matrices))))
      (use-value (v) v))))

(defmethod deserialize ((object triangle-mesh) place)
  (load-mesh object place))

(defmacro with-pushed-matrix ((object &key (what :modelview)) &body body)
  `(unwind-protect
        (progn
          (push-matrix ,object :what ,what)
          ,@body)
     (pop-matrix ,object :what ,what)))

(defclass triangle-mesh-shell (triangle-mesh inner-animation) ())

(defmethod destroy ((object triangle-mesh-shell))
  "does nothing"
  )

(defmethod prepare-for-rendering ((object triangle-mesh-shell))
  object)

(defmethod make-data-for-opengl ((object triangle-mesh-shell))
  "does nothing to prevent gc reclaiming the data (vao vbo etc.)"
  )

(defmethod make-data-for-opengl-tangents-obj-space ((object triangle-mesh-shell))
  "does nothing to prevent gc reclaiming the data (vao vbo etc.)"
  )

(defmethod make-data-for-opengl-aabb-obj-space ((object triangle-mesh-shell))
  "does nothing to prevent gc reclaiming the data (vao vbo etc.)"
  )

(defgeneric fill-mesh-data (object source))

(defgeneric fill-mesh-data-w-renderer-data (object source))

(defmethod fill-mesh-data ((object triangle-mesh-shell) (source triangle-mesh))
  (with-accessors ((vao-host vao) (vbo-host vbo) (texture-host texture-object)
                   (normal-map-host normal-map)  (triangles-host triangles)
                   (children-host children)
                   (material-params-host material-params)
                   (compiled-shaders-host compiled-shaders)
                   (renderer-data-vertices renderer-data-vertices)
                   (renderer-data-texture  renderer-data-texture)
                   (renderer-data-normals  renderer-data-normals)
                   (renderer-data-tangents renderer-data-tangents)
                   (renderer-data-normals-obj-space renderer-data-normals-obj-space)
                   (renderer-data-tangents-obj-space  renderer-data-tangents-obj-space)
                   (renderer-data-aabb-obj-space      renderer-data-aabb-obj-space)
                   (aabb-host aabb)
                   (tags-table     tags-table)
                   (tags-matrices  tags-matrices)
                   (tag-key-parent tag-key-parent)) object
    (with-slots (aabb) source
      (setf vao-host                          (vao              source)
            vbo-host                          (vbo              source)
            texture-host                      (texture-object   source)
            normal-map-host                   (normal-map       source)
            triangles-host                    (triangles        source)
            material-params-host              (material-params  source)
            compiled-shaders-host             (compiled-shaders source)
            tags-table                        (tags-table       source)
            tags-matrices                     (tags-matrices    source)
            tag-key-parent                    (tag-key-parent   source)
            aabb-host                         aabb)
      (do-children-mesh (child source)
        (let ((new-child (make-instance 'triangle-mesh-shell)))
          (fill-mesh-data new-child child)
          (add-child object new-child)))
      object)))

(defmethod fill-mesh-data-w-renderer-data ((object triangle-mesh-shell) (source triangle-mesh))
  (with-accessors ((vao-host vao) (vbo-host vbo) (texture-host texture-object)
                   (normal-map-host normal-map)  (triangles-host triangles)
                   (children-host children)
                   (material-params-host material-params)
                   (compiled-shaders-host compiled-shaders)
                   (renderer-data-vertices renderer-data-vertices)
                   (renderer-data-texture  renderer-data-texture)
                   (renderer-data-normals  renderer-data-normals)
                   (renderer-data-tangents renderer-data-tangents)
                   (renderer-data-normals-obj-space  renderer-data-normals-obj-space)
                   (renderer-data-tangents-obj-space renderer-data-tangents-obj-space)
                   (renderer-data-aabb-obj-space     renderer-data-aabb-obj-space)
                   (aabb-host aabb))  object
    (with-slots (aabb) source
      (setf vao-host                          (vao              source)
            vbo-host                          (vbo              source)
            texture-host                      (texture-object   source)
            normal-map-host                   (normal-map       source)
            triangles-host                    (triangles        source)
            material-params-host              (material-params  source)
            compiled-shaders-host             (compiled-shaders source)
            aabb-host                         aabb))
    (setf renderer-data-vertices            (renderer-data-vertices           source)
          renderer-data-texture             (renderer-data-texture            source)
          renderer-data-normals             (renderer-data-normals            source)
          renderer-data-tangents            (renderer-data-tangents           source)
          renderer-data-normals-obj-space   (renderer-data-normals-obj-space  source)
          renderer-data-tangents-obj-space  (renderer-data-tangents-obj-space source)
          renderer-data-aabb-obj-space      (renderer-data-aabb-obj-space     source))
    (do-children-mesh (child source)
        (let ((new-child (make-instance 'triangle-mesh-shell)))
          (fill-mesh-data-w-renderer-data new-child child)
          (add-child object new-child)))
      object))

(defun fill-shell-from-mesh (mesh &optional (type 'triangle-mesh-shell))
  (fill-mesh-data (make-instance type) mesh))

(defun fill-shell-from-mesh-w-renderer-data (mesh &optional (type 'triangle-mesh-shell))
  (fill-mesh-data-w-renderer-data (make-instance type) mesh))

(defmethod on-game-event ((object triangle-mesh-shell) (event game-event:attack-melee-event))
  (game-event:check-event-targeted-to-me (object event)
    (multiple-value-bind (damage ambush)
        (battle-utils:defend-from-attack-short-range event)
      (declare (ignore ambush))
      (misc:dbg "damage shell ~a ~a" damage (character:current-damage-points (ghost object)))
      (when damage
        (apply-damage object damage))
      (game-event:register-for-end-attack-melee-event object)
      t)))

(defmethod on-game-event ((object triangle-mesh-shell) (event game-event:attack-long-range-event))
  (game-event:check-event-targeted-to-me (object event)
    (multiple-value-bind (damage ambush)
        (battle-utils:defend-from-attack-long-range event)
      (declare (ignore ambush))
      (when damage
        (apply-damage object damage))
      (game-event:register-for-end-attack-long-range-event object)
      t)))

(defmethod on-game-event ((object triangle-mesh-shell) (event game-event:attack-spell-event))
  (game-event:check-event-targeted-to-me (object event)
    (multiple-value-bind (damage ambush)
        (battle-utils:defend-from-attack-spell event)
      (declare (ignore ambush))
      (misc:dbg "damage shell ~a ~a" damage (character:current-damage-points (ghost object)))
      (apply-damage object damage :tooltip-active-p nil))))

(defmethod apply-damage ((object triangle-mesh-shell) damage
                         &key &allow-other-keys)
  (with-accessors ((ghost ghost)
                   (id id)
                   (state state)
                   (pos pos)
                   (dir dir)
                   (aabb aabb)
                   (compiled-shaders compiled-shaders)) object
    (with-accessors ((current-damage-points character:current-damage-points)) ghost
      (when (not (entity-dead-p object))
        (when (not (null damage))
          (setf current-damage-points (d- current-damage-points damage))
          (setf (fading-away-fn object) (tremor:tree-tremor-fn 20.0))
          (when (entity-dead-p object)
            (let ((cost-pos (map-utils:pos-entity-chunk->cost-pos pos)))
              (clean-map-state-entity state cost-pos)
              (game-state:with-world (world state)
                (remove-entity-by-id world id))
              (set-minimum-cost-map-layer@ state (elt cost-pos 0) (elt cost-pos 1))
              (setf (renderp object) nil)
              (when (parent-labyrinth object)
                (update-for-rendering (parent-labyrinth object))))))))))

(defclass tree-mesh-shell (triangle-mesh-shell) ())

(defun tree-mesh-shell-p (a)
  (typep a 'mesh:tree-mesh-shell))

(defmethod initialize-instance :after ((object tree-mesh-shell) &key &allow-other-keys)
  (setf (start-time object) (d (lcg-next-upto 5))))

(defmethod rendering-needed-p ((object tree-mesh-shell) renderer)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (world::frustum-aabb-intersects-p renderer object))

(defmethod calculate :after ((object tree-mesh-shell) dt)
  (declare (optimize (debug 0) (speed 1) (safety 0)))
  (with-accessors ((impostor impostor)) object
    (and impostor (calculate impostor dt))
    ;; we use 'rem' here to make  the 'el-time' periodic, otherwise
    ;; we will get non-fluid animation.
    ;; my best guess  is  that sin  function  in  the shader  give  wrong
    ;; results when argument is large (as it wa no more periodic)
    (setf (el-time object)
          (d+ (start-time object)
              (rem (d* (animation-speed object) (current-time object))
                   1000.0)))))

(defmethod render-phong ((object tree-mesh-shell) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (impostor impostor)
                   (triangles triangles)
                   (material-params material-params)
                   (el-time el-time)
                   (fog-density fog-density)
                   (animation-speed animation-speed)
                   (fading-away-fn fading-away-fn)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (declare (function fading-away-fn))
    (declare (desired-type animation-speed el-time))
    (if (use-lod-p object 2.0 renderer)
        (render impostor renderer)
        (when (> (length triangles) 0)
          (with-camera-view-matrix (camera-vw-matrix renderer)
            (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
              (with-no-cull-face
                (use-program compiled-shaders :tree)
                (gl:active-texture            :texture0)
                (texture:bind-texture texture-object)
                (uniformi  compiled-shaders :texture-object +texture-unit-diffuse+)
                (uniformfv compiled-shaders :light-pos
                           (the vec (main-light-pos-eye-space renderer)))
                (uniformfv compiled-shaders :ia    #(1.0 1.0 1.0))
                (uniformfv compiled-shaders :id    (the vec (main-light-color renderer)))
                (uniformfv compiled-shaders :is    (the vec (main-light-color renderer)))
                (uniformf  compiled-shaders :ka    (ka material-params))
                (uniformf  compiled-shaders :kd    (kd material-params))
                (uniformf  compiled-shaders :ks    (ks material-params))
                (uniformf  compiled-shaders :shine (shininess material-params))
                (uniformf  compiled-shaders :time  el-time)
                (uniformf  compiled-shaders :fog-density fog-density)
                (uniform-matrix compiled-shaders :model-matrix 4 model-matrix nil)
                (let ((tremor-matrix (funcall fading-away-fn object animation-speed)))
                  (declare (sb-cga:matrix tremor-matrix))
                  (uniform-matrix compiled-shaders :modelview-matrix 4
                                  (vector (matrix* camera-vw-matrix
                                                   (elt view-matrix 0)
                                                   (elt model-matrix 0)
                                                   tremor-matrix))
                                  nil))
                (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
                (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
                (gl:draw-arrays :triangles 0 (f* 3 (length triangles))))))
          (render-debug object renderer)))))

(defgeneric tree-trunk-aabb (object))

(defmethod tree-trunk-aabb ((object tree-mesh-shell))
  (with-accessors ((pos pos)) object
    (make-instance '3d-utils:aabb
                   :aabb-p2 (vec+ pos (vec (d/ +terrain-chunk-tile-size+ 2.0)
                                           +wall-h+
                                           (d/ +terrain-chunk-tile-size+ 2.0)))
                   :aabb-p1 (vec+ pos (vec (d- (d/ +terrain-chunk-tile-size+ 2.0))
                                           0.0
                                           (d- (d/ +terrain-chunk-tile-size+ 2.0)))))))

(defmethod actual-aabb-for-bullets ((object tree-mesh-shell))
  (tree-trunk-aabb object))

(alexandria:define-constant +clouds-levels+ 3 :test #'=)

(defclass skydome (triangle-mesh)
  ((el-time
    :initform 0.0
    :initarg :el-time
    :accessor el-time)
   (base-clouds-speed
    :initform 1e-2
    :initarg :base-clouds-speed
    :accessor base-clouds-speed)
   (texture-clouds
    :initform (misc:make-array-frame +clouds-levels+ 'texture-object t t)
    :initarg :texture-clouds
    :accessor texture-clouds)
   (weather-type
    :initform :clear-night-stormy-day ;;1 or :clear-night-stormy-day 0
    :initarg :weather-type
    :accessor weather-type)))

(defmethod initialize-instance :after ((object skydome) &key &allow-other-keys)
  (let* ((rotation-x (desired (deg->rad 5.0)))
         (rotation-y (desired (lcg-next-upto (deg->rad 360.0))))
         (center     (transform-direction (vec- +z-axe+ +zero-vec+)
                                          (matrix* (rotate-around +y-axe+ rotation-y)
                                                   (rotate-around +x-axe+ rotation-x)))))
    ;; the smoke tray
    (setf (projector object) (look@ center +zero-vec+ +y-axe+))
    (setf (id              object) +id-skydome+)))

(defmethod calculate ((object skydome) dt)
  (declare (ignore dt))
  (setf (el-time object) (d* (base-clouds-speed object) (current-time object))))

(defmethod render ((object skydome) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (el-time el-time)
                   (texture-clouds texture-clouds)
                   (state state)
                   (texture-projector texture-projector)
                   (projector projector)
                   (weather-type weather-type)
                   (triangles triangles)) object
    (declare (list triangles))
    (declare ((simple-array texture:texture (*)) texture-clouds))
    (declare ((simple-array simple-array (1)) model-matrix view-matrix))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (with-depth-disabled
            (with-depth-mask-disabled
              (use-program compiled-shaders :skydome)
              (uniformi compiled-shaders :weather-type
                        (if (eq weather-type :foggy-night-clear-day)
                            1
                            0))
              (gl:active-texture :texture0)
              (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
              (texture:bind-texture texture-object)
              (gl:active-texture :texture1)
              (texture:bind-texture (elt texture-clouds 0))
              (uniformi compiled-shaders :texture-clouds-1 +texture-unit-clouds-1+)
              (gl:active-texture :texture2)
              (texture:bind-texture (elt texture-clouds 1))
              (uniformi compiled-shaders :texture-clouds-2 +texture-unit-clouds-2+)
              (gl:active-texture :texture3)
              (texture:bind-texture (elt texture-clouds 2))
              ;; smoke
              (uniformi compiled-shaders :texture-smoke +texture-unit-projector+)
              (gl:active-texture :texture4)
              (texture:bind-texture texture-projector)
              (uniformi compiled-shaders :texture-clouds-3 +texture-unit-clouds-3+)
              (uniformf compiled-shaders :traslation-clouds-speed el-time)
              ;; comment out the line below to get rid of camera following effect
              (setf (model-matrix object) (translate (get-camera-pos renderer)))
              ;; (setf (model-matrix object) (matrix* (elt model-matrix 0)
              ;;                                           (translate (vec 0.0 -20.0 0.0))))
              (uniformi  compiled-shaders :texture-object +texture-unit-diffuse+)
              (uniformfv compiled-shaders :sky-color (the vec4 (sky-bg-color state)))
              (uniform-matrix compiled-shaders :modelview-matrix 4
                              (vector (matrix* camera-vw-matrix
                                               (elt view-matrix 0)
                                               (elt model-matrix 0)))
                              nil)
              (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
              ;; smoke projection matrix
              (uniform-matrix compiled-shaders :proj-texture-matrix 4
                              (vector (matrix* +projective-scale-bias+
                                               (elt camera-proj-matrix 0)
                                               projector))
                                        ;(elt model-matrix 0)))
                              nil)
              (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
              (gl:draw-arrays :triangles 0 (* 3 (length triangles))))))))))

(defun put-simple-plane-vertex (plane x y z inc-x inc-z &key (normal +y-axe+))
  (normal-v plane normal)
  (tangent-v plane +x-axe+)
  (texel plane inc-x inc-z)
  (mesh:vertex plane (d+ x inc-x) y (d+ z inc-z)
               :gen-normal nil :gen-triangle t
               :compact-vertices t :manifoldp nil))

(alexandria:define-constant +reflection-texture-size+ 256 :test #'=)

(defclass water (triangle-mesh)
  ((el-time
    :initform 0.0
    :initarg :el-time
    :accessor el-time)
   (framebuffer
    :initform nil
    :initarg :framebuffer
    :accessor framebuffer)
   (depthbuffer
    :initform nil
    :initarg :depthbuffer
    :accessor depthbuffer)
   (color
    :initform (vec4:vec4 0.0 0.0 1.0 1.0)
    :initarg :color
    :accessor color)
   (measures
    :initform (vec4:vec4 0.0 0.0 64.0 64.0)
    :initarg :measures
    :accessor measures)
   (triangle-step
    :initform (* 2.0 +terrain-chunk-tile-size+)
    :initarg :triangle-step
    :accessor triangle-step)
   (fluid-speed
    :initform 0.15
    :initarg :fluid-speed
    :accessor fluid-speed)))

(definline water-mesh-p (w)
  (typep w 'water))

(defmethod initialize-instance :after ((object water) &key &allow-other-keys)
  (with-accessors ((texture-object texture-object)) object
    (setf texture-object (texture:gen-name-and-inject-in-database
                          (make-instance 'texture:texture)))
    (setf (texture:s-wrap-mode texture-object) :clamp-to-edge)
    (setf (texture:t-wrap-mode texture-object) :clamp-to-edge)
    (setf (texture:interpolation-type texture-object) :linear)
    (setf (texture:border-color texture-object) §cff0000ff)
    (setf (use-blending-p object) t)
    (texture:setup-texture-parameters texture-object)))

(defmethod rendering-needed-p ((object water) renderer)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (world:frustum-aabb-intersects-p renderer object))

(defmethod calculate ((object water) dt)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (declare (ignore dt))
  (setf (el-time object) (d* (fluid-speed object) (current-time object))))

(defmethod make-data-for-opengl :after ((object water))
  (let ((framebuffers (gl:gen-framebuffers 1))
        (depthbuffers (gl:gen-renderbuffers 1)))
    (setf (framebuffer object)  (elt framebuffers 0)
          (depthbuffer object) (elt depthbuffers 0))
    ;; setup finalizer
    (let (#+debug-mode
          (id (slot-value object 'id)))
      (tg:finalize object
                   #'(lambda ()
                       #+debug-mode (misc:dbg "finalize water ~a" id)
                       (gl:delete-framebuffers framebuffers)
                       (gl:delete-renderbuffers depthbuffers))))))

(defun %water-step-calculator (size min)
  (dmax min
        (d+ (d* 0.05 size) 3.8)))

(defmethod prepare-for-rendering ((object water))
  "                    d
             a+--------+--------+ ...
     x        |\-      |\-      |
    +----->   |  \-    |  \-    |
  z |         |    \-  |    \-  |
    |         |      \-|c     \-|
    |        b+--------+--------+ ...
    v         |\-      |\-      |
              |  \-    |  \-    |
              |    \-  |    \-  |
              |      \-|      \-|
              +--------+--------+ ...
"
  (let* ((aabb   (map 'vector #'(lambda (a) (map-utils:coord-terrain->chunk a :tile-offset 0.0))
                      (vec4:vec4 (d- (elt (measures object) 0) +terrain-chunk-tile-size+)
                                 (d- (elt (measures object) 1) +terrain-chunk-tile-size+)
                                 (d+ (elt (measures object) 2) +terrain-chunk-tile-size+)
                                 (d+ (elt (measures object) 3) +terrain-chunk-tile-size+))))
         (x-end   (aabb2-max-x aabb))
         (y-end   (aabb2-max-y aabb))
         (x-start (aabb2-min-x aabb))
         (y-start (aabb2-min-y aabb))
         (width   (d- x-end x-start))
         (height  (d- y-end y-start))
         (inc-x   (%water-step-calculator width  (triangle-step object)))
         (inc-z   (%water-step-calculator height (triangle-step object))))
    (loop for x from x-start below x-end by inc-x do
         (loop for z from y-start below y-end by inc-z do
            ;; a
              (put-simple-plane-vertex object x +water-mesh-starting-y+ z
                                       0.0 0.0 :normal +y-axe+)
            ;; b
              (put-simple-plane-vertex object x +water-mesh-starting-y+ z
                                       0.0 inc-z :normal +y-axe+)
            ;; c
              (put-simple-plane-vertex object x +water-mesh-starting-y+ z
                                       inc-x inc-z :normal +y-axe+)
            ;; a
              (put-simple-plane-vertex object x +water-mesh-starting-y+ z
                                       0.0 0.0 :normal +y-axe+)
            ;; c
              (put-simple-plane-vertex object x +water-mesh-starting-y+ z
                                       inc-x inc-z :normal +y-axe+)
            ;; d
              (put-simple-plane-vertex object x +water-mesh-starting-y+ z
                                       inc-x 0.0 :normal +y-axe+))))
  (call-next-method))

(defmethod render-for-reflection ((object water) renderer)
  ;;do nothing
  t)

(defmethod render-lod-1 ((object water) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (triangles triangles)
                   (el-time el-time)
                   (material-params material-params)
                   (current-time current-time)
                   (fog-density fog-density)) object
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
          (gl:enable :blend)
          (gl:blend-func :src-alpha :one-minus-src-alpha)
          (use-program compiled-shaders :water-no-texture)
          (uniformfv compiled-shaders :id        (the vec (main-light-color renderer)))
          (uniformfv compiled-shaders :is        (the vec (main-light-color renderer)))
          (uniformf  compiled-shaders :ka        1.0)
          (uniformf  compiled-shaders :kd        1.0)
          (uniformf  compiled-shaders :ks        0.0)
          (uniformf  compiled-shaders :shine    100.0)
          (uniformfv compiled-shaders
                              :ia (vec4:vec4->vec (the vec4 (sky-bg-color (main-state renderer)))))

          (uniformf compiled-shaders :time el-time)
          (uniformfv compiled-shaders :light-pos
                     (the vec (main-light-pos-eye-space renderer)))
          (uniform-matrix compiled-shaders :modelview-matrix 4
                                   (vector (matrix* camera-vw-matrix
                                                    (elt view-matrix 0)
                                                    (elt model-matrix 0)))
                                   nil)
          (uniform-matrix compiled-shaders :proj-matrix 4 camera-proj-matrix nil)
          (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
          (gl:draw-arrays :triangles 0 (* 3 (length triangles)))
          (gl:disable :blend))))))

(defmethod render ((object water) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (when (rendering-needed-p object renderer)
    (if (use-lod-p object 3.0 renderer)
        (render-lod-1 object renderer)
        (progn
          ;; setup the texture
          (let* ((camera              (camera renderer))
                 (saved-camera-pos    (pos camera))
                 (saved-camera-target (target camera))
                 (saved-camera-up     (up camera))
                 (saved-camera-dir    (vec- saved-camera-target saved-camera-pos))
                 (saved-camera-side   (normalize (cross-product saved-camera-up saved-camera-dir)))
                 (new-camera-pos-y    (d- (d* 2.0 +water-mesh-starting-y+)
                                          (elt saved-camera-pos 1)))
                 (new-camera-target-y (d- (d* 2.0 +water-mesh-starting-y+)
                                          (elt saved-camera-target 1)))
                 (new-camera-pos      (vec (elt saved-camera-pos 0)
                                           new-camera-pos-y
                                           (elt saved-camera-pos 2)))
                 (new-camera-target   (vec (elt saved-camera-target 0)
                                           new-camera-target-y
                                           (elt saved-camera-target 2)))
                 (new-camera-dir      (vec- new-camera-target new-camera-pos))
                 (new-camera-up       (normalize (cross-product saved-camera-side new-camera-dir)))
                 (view-matrix-camera  (view-matrix camera)))
            (declare ((simple-array simple-array (1)) view-matrix-camera))
            (setf (pos camera)    new-camera-pos
                  (target camera) new-camera-target
                  (up  camera)    new-camera-up)
            (look-at* camera)
            (setf (projector object) (elt view-matrix-camera 0))
            (render-to-memory-texture (framebuffer object) (depthbuffer object)
                                      (texture:handle (texture-object object))
                                      +reflection-texture-size+
                                      +reflection-texture-size+
                                      #'(lambda () (render-for-reflection renderer renderer)))
            (setf (pos camera)    saved-camera-pos
                  (target camera) saved-camera-target
                  (up  camera)    saved-camera-up)
            (look-at* camera))
          (gl:viewport 0 0 (the fixnum *window-w*) (the fixnum *window-h*))
          (render-plane object renderer)))))

(defmethod destroy :after ((object water))
  (with-accessors ((framebuffer framebuffer) (depthbuffer depthbuffer)) object
    #+debug-mode (misc:dbg "destroy water ~a" (id object))
    (setf framebuffer nil
          depthbuffer nil)))

(defgeneric render-plane (object renderer))

(defmethod render-plane ((object water) renderer)
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (texture-object texture-object)
                   (projection-matrix projection-matrix)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (compiled-shaders compiled-shaders)
                   (el-time el-time)
                   (texture-clouds texture-clouds)
                   (state state)
                   (texture-smoke texture-smoke)
                   (projector projector)
                   (weather-type weather-type)
                   (triangles triangles)) object
    (declare (texture:texture texture-object))
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list triangles vao vbo))
    (when (> (length triangles) 0)
      (with-camera-view-matrix (camera-vw-matrix renderer)
        (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped t)
                                        ;(gl:disable :depth-test)
          (with-blending
            (gl:blend-func :src-alpha :one-minus-src-alpha)
            (use-program compiled-shaders :water)
            (gl:active-texture :texture0)
            (texture:bind-texture texture-object)
            (uniformi compiled-shaders :texture-object +texture-unit-diffuse+)
            (uniformfv compiled-shaders :light-pos
                       (the vec (main-light-pos-eye-space renderer)))
            (uniformfv compiled-shaders :ia        (the vec (main-light-color renderer)))
            (uniformfv compiled-shaders :id        (the vec (main-light-color renderer)))
            (uniformfv compiled-shaders :is        (the vec (main-light-color renderer)))
            (uniformf  compiled-shaders :ka        1.0)
            (uniformf  compiled-shaders :kd        1.0)
            (uniformf  compiled-shaders :ks        0.0)
            (uniformf  compiled-shaders :shine    100.0)
            (uniformfv compiled-shaders
                       :ia (vec4:vec4->vec (the vec4 (sky-bg-color (main-state renderer)))))
            (uniformf compiled-shaders :time el-time)
            (uniform-matrix compiled-shaders :modelview-matrix 4
                            (vector (matrix* camera-vw-matrix
                                             (elt view-matrix 0)
                                             (elt model-matrix 0)))
                            nil)
            (uniform-matrix compiled-shaders :proj-matrix  4 camera-proj-matrix nil)
            (uniform-matrix compiled-shaders :proj-texture-matrix 4
                            (vector (matrix* +projective-scale-bias+
                                             (elt camera-proj-matrix 0)
                                             projector
                                             (elt model-matrix 0)))
                            nil)
            (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
            (gl:draw-arrays :triangles 0 (* 3 (length triangles)))))))))

(defclass blocker-render-children (triangle-mesh) ())

(defmethod render ((object blocker-render-children) renderer)
  (with-accessors ((parent parent)
                   (renderp renderp)) object
    (when (and parent
               (not (renderp parent)))
      (do-children-mesh (child object)
        (render child renderer)))))

(defmethod calculate ((object blocker-render-children) dt)
  (with-accessors ((parent parent)
                   (renderp renderp)) object
    (when (and parent
               (not (renderp parent)))
      (bubbleup-modelmatrix object)
      (do-children-mesh (child object)
        (calculate child dt)))))

(defun cylinder-section (mesh radius height divisions
                         texture-sstart texture-tstart
                         texture-sstep texture-tstep
                         manifold)
  ;; (misc:dbg "sect ~a -> ~a s ~a -> ~a" texture-tstart
  ;;        texture-tstep texture-sstart
  ;;        texture-sstep)
  (let* ((*default-epsilon* 1e-3)
         (alpha (coerce (/ +2pi+ divisions) 'single-float))
         (pointer (vec 0.0 0.0 radius))
         (rotate-y (rotate-around +y-axe+ alpha))
         (height-vec (vec 0.0 height 0.0))
         (a pointer)
         (b (transform-point pointer rotate-y))
         (c (vec+ pointer height-vec))
         (d (vec+ b height-vec))
         (normal (triangle-normal a b c)))
    ;; c +-------+ d
    ;;   |\      |
    ;;   | \ T2  |
    ;;   |  \    |
    ;;   |   \   |
    ;;   |    \  |
    ;;   | T1  \ |
    ;;   |      \|
    ;;   +-------+
    ;;   a        b
    ;; first triangle
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh texture-sstart texture-tstart)
    (vertex-v mesh a :gen-triangle t  :compact-vertices t :manifoldp manifold) ; a
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh (+ texture-sstart texture-sstep) texture-tstart)
    (vertex-v mesh b :gen-triangle t  :compact-vertices t :manifoldp manifold) ;b
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh texture-sstart texture-tstep)
    (vertex-v mesh c :gen-triangle t  :compact-vertices t :manifoldp manifold) ;c
    ;; second triangle
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh (+ texture-sstart texture-sstep) texture-tstart)
    (vertex-v mesh b :gen-triangle t  :compact-vertices t :manifoldp manifold) ;b
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh (+ texture-sstart texture-sstep) texture-tstep)
    (vertex-v mesh d :gen-triangle t  :compact-vertices t :manifoldp manifold) ;d
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh texture-sstart texture-tstep)
    (vertex-v mesh c :gen-triangle t  :compact-vertices t :manifoldp manifold) ;c
    (when manifold
      ;; vertex c d origin
      (normal-v mesh +y-axe+) ;top
      (tangent-v mesh normal)
      (texel mesh texture-sstart texture-tstart)
      (vertex-v mesh c :gen-triangle t  :compact-vertices t :manifoldp manifold) ;c
      (normal-v mesh +y-axe+)
      (tangent-v mesh normal)
      (texel mesh (+ texture-sstart texture-sstep) texture-tstart)
      (vertex-v mesh d :gen-triangle t  :compact-vertices t :manifoldp manifold) ;d
      (normal-v mesh +y-axe+)
      (tangent-v mesh normal)
      (texel mesh (/ (+ texture-sstart texture-sstep) 2.0) texture-tstep)
      (vertex-v mesh height-vec :gen-triangle t  :compact-vertices t :manifoldp manifold) ;origin
      ;; vertex b a origin
      (vertex-v mesh b :gen-triangle t  :compact-vertices t :manifoldp manifold) ;b
      (normal-v mesh (vec- +zero-vec+ +y-axe+))
      (tangent-v mesh normal)
      (texel mesh (/ (+ texture-sstart texture-sstep) 2.0) texture-tstep)
      (normal-v mesh (vec- +zero-vec+ +y-axe+))
      (tangent-v mesh normal)
      (texel mesh texture-sstart texture-tstart)
      (vertex-v mesh a :gen-triangle t  :compact-vertices t :manifoldp manifold) ; a
      (normal-v mesh (vec- +zero-vec+ +y-axe+))
      (tangent-v mesh normal)
      (texel mesh (+ texture-sstart texture-sstep) texture-tstart)
      (vertex-v mesh +zero-vec+ :gen-triangle t  :compact-vertices t
                :manifoldp manifold)))) ;origin

(defun quad (mesh w h
             texture-sstart texture-tstart
             texture-sstep texture-tstep
             &optional
               (origin +zero-vec+)
               (manifold t)
               (compact-vertices t))
  (let* ((*default-epsilon* 1e-3)
         (a (copy-vec origin))
         (b (vec+ a (vec   w 0.0 0.0)))
         (c (vec+ a (vec 0.0   h 0.0)))
         (d (vec+ a (vec   w   h 0.0)))
         (normal (triangle-normal a b c)))
    ;;  - c +-------+ d
    ;;  |   |\      |
    ;;  |   | \ T2  |
    ;;h |   |  \    |
    ;;  |   |   \   |
    ;;  |   |    \  |      ^ y
    ;;  |   | T1  \ |      |
    ;;  |   |      \|      |    x
    ;;  -   +-------+      +---->
    ;;      a        b    /
    ;;      |-------|    / z
    ;;         w        v
    ;; first triangle
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh texture-sstart texture-tstart)
    (vertex-v mesh a :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ; a
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh (+ texture-sstart texture-sstep) texture-tstart)
    (vertex-v mesh b :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ;b
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh texture-sstart texture-tstep)
    (vertex-v mesh c :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ;c
    ;; second triangle
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh (+ texture-sstart texture-sstep) texture-tstart)
    (vertex-v mesh b :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ;b
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh (+ texture-sstart texture-sstep) texture-tstep)
    (vertex-v mesh d :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ;d
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel mesh texture-sstart texture-tstep)
    (vertex-v mesh c :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold))) ;c

(defun quad-w-explicit-texture-coords (mesh w h
                                       texture-coords
                                       &optional
                                         (origin +zero-vec+)
                                         (manifold t)
                                         (compact-vertices t))
  (let* ((*default-epsilon* 1e-3)
         (a (copy-vec origin))
         (b (vec+ a (vec   w 0.0 0.0)))
         (c (vec+ a (vec 0.0   h 0.0)))
         (d (vec+ a (vec   w   h 0.0)))
         (normal (triangle-normal a b c)))
    ;;  - c +-------+ d
    ;;  |   |\      |
    ;;  |   | \ T2  |
    ;;h |   |  \    |
    ;;  |   |   \   |
    ;;  |   |    \  |      ^ y
    ;;  |   | T1  \ |      |
    ;;  |   |      \|      |    x
    ;;  -   +-------+      +---->
    ;;      a        b    /
    ;;      |-------|    / z
    ;;         w        v
    ;; first triangle
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel-v mesh (elt texture-coords 0))
    (vertex-v mesh a :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ; a
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel-v mesh (elt texture-coords 1))
    (vertex-v mesh b :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ;b
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel-v mesh (elt texture-coords 2))
    (vertex-v mesh c :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ;c
    ;; second triangle
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel-v mesh (elt texture-coords 3))
    (vertex-v mesh b :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ;b
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel-v mesh (elt texture-coords 4))
    (vertex-v mesh d :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold) ;d
    (normal-v mesh normal)
    (tangent-v mesh normal)
    (texel-v mesh (elt texture-coords 5))
    (vertex-v mesh c :gen-triangle t :compact-vertices compact-vertices
              :manifoldp manifold))) ;c

(defun cylinder (radius height &key (divisions 4)
                                 (max-s-texture 1.0)
                                 (start-s-texture 0.0)
                                 (start-t-texture 0.0)
                                 (end-t-texture 1.0)
                                 (manifold nil)
                                 (wrapper-transformation (identity-matrix)))
  (when (< divisions 4)
    (setf divisions 4))
  (let* ((mesh (make-instance 'triangle-mesh))
         (alpha (d/ +2pi+ (desired divisions)))
         (texture-sstep (/ (- max-s-texture start-s-texture) divisions)))
    (with-pushed-matrix (mesh :what :modelview)
      (load-matrix mesh (identity-matrix))
      (loop
         for i from 0.0 below +2pi+ by alpha
         for sstart from start-s-texture by texture-sstep do
           (with-pushed-matrix (mesh :what :modelview)
             (mult-matrix mesh (rotate-around +y-axe+ i))
             (mult-matrix mesh wrapper-transformation)
             (cylinder-section mesh radius height divisions sstart
                               start-t-texture texture-sstep end-t-texture
                               manifold)))
      (remove-orphaned-vertices mesh)
      mesh)))

(defun cube (size &key
                    (max-s-texture 1.0)
                    (start-s-texture 0.0)
                    (start-t-texture 0.0)
                    (end-t-texture 1.0)
                    (wrapper-transformation (identity-matrix))
                    (manifold t) (average-normals nil)
                    (compact-vertices t)
                    (draw-top t)
                    (draw-bottom t))
  (let* ((mesh (make-instance 'triangle-mesh))
         (alpha  +pi/2+)
         (size/2 (d/ size 2.0))
         (origin (vec (d- size/2) 0.0 size/2))
         (texture-sstep (d/ (d- max-s-texture start-s-texture) 4.0)))
    (when (or (null draw-bottom)
              (null draw-top))
      (setf manifold nil))
    (with-pushed-matrix (mesh :what :modelview)
      (load-matrix mesh wrapper-transformation)
      (with-pushed-matrix (mesh :what :modelview)
        (loop
           repeat 4
           for sstart from start-s-texture by texture-sstep do
             (mult-matrix mesh (rotate-around +y-axe+ alpha))
             (quad mesh size size
                   sstart          start-t-texture
                   texture-sstep   end-t-texture
                   origin manifold compact-vertices)))
      ;; bottom
      (when draw-bottom
        (with-pushed-matrix (mesh :what :modelview)
          (mult-matrix mesh (translate (vec (d- size/2) (d- size/2) 0.0)))
          (mult-matrix mesh (rotate-around +x-axe+ alpha))
          (quad mesh size size
                start-s-texture start-t-texture
                texture-sstep   end-t-texture
                +zero-vec+ manifold compact-vertices)))
      ;; top
      (when draw-top
        (with-pushed-matrix (mesh :what :modelview)
          (mult-matrix mesh (translate (vec (d- size/2) (d- size/2) 0.0)))
          (mult-matrix mesh (translate (vec 0.0      0.0  size)))
          (mult-matrix mesh (rotate-around +x-axe+ (d- alpha)))
          (quad mesh size size
                start-s-texture start-t-texture
                texture-sstep   end-t-texture
                +zero-vec+ manifold compact-vertices)))
      (remove-orphaned-vertices mesh)
      (when average-normals
        (average-normals mesh))
      mesh)))

(defun parallelepiped (w h &key
                             (max-s-texture 1.0)
                             (start-s-texture 0.0)
                             (start-t-texture 0.0)
                             (end-t-texture 1.0)
                             (wrapper-transformation (identity-matrix))
                             (manifold t) (average-normals nil) (compact-vertices t)
                             (draw-top t)
                             (draw-bottom t))
  (let ((mesh (cube 1.0
                    :max-s-texture          max-s-texture
                    :start-s-texture        start-s-texture
                    :start-t-texture        start-t-texture
                    :end-t-texture          end-t-texture
                    :wrapper-transformation (identity-matrix)
                    :manifold               manifold
                    :average-normals        nil
                    :compact-vertices       compact-vertices
                    :draw-top               draw-top
                    :draw-bottom            draw-bottom)))
    (loop for vertex across (vertices mesh) do
         (setf (elt vertex 0) (d* (elt vertex 0) w)
               (elt vertex 1) (d* (elt vertex 1) h)
               (elt vertex 2) (d* (elt vertex 2) w)))
    (loop for i from 0 below (length (vertices mesh)) do
         (setf (elt (vertices mesh) i)
               (transform-point (elt (vertices mesh) i) wrapper-transformation)))
    (loop for i from 0 below (length (normals mesh)) do
         (setf (elt (normals mesh) i)
               (transform-direction (elt (normals mesh) i) wrapper-transformation)))
    (reset-aabb mesh)
    (when average-normals
      (average-normals mesh))
    mesh))

(defun put-simple-plane-vertex-w-texture (plane x y z
                                          inc-x inc-z
                                          s-coord t-coord
                                          tinc-x tinc-z
                                          &key (normal +y-axe+))
  (normal-v plane normal)
  (tangent-v plane +x-axe+)
  (texel plane (d+ s-coord tinc-x) (d+ t-coord tinc-z))
  (mesh:vertex plane (d+ x inc-x) y (d+ z inc-z)
               :gen-normal nil :gen-triangle t
               :compact-vertices t :manifoldp nil))


(defun quads-plane (mesh x-size z-size x-divisions z-divisions h
                    &key (set-find-index-by-value-from-end t))
  "The center of the mesh is on the axes origin"
  ;;             a        d
  ;;             +--------+--------+ ...
  ;;    x        |\-      |\-      |
  ;;   +----->   |  \-    |  \-    |
  ;; z |         |    \-  |    \-  |
  ;;   |         |      \-|c     \-|
  ;;   |        b+--------+--------+ ...
  ;;   v         |\-      |\-      |
  ;;             |  \-    |  \-    |
  ;;             |    \-  |    \-  |
  ;;             |      \-|      \-|
  ;;             +--------+--------+ ...
  (setf (find-index-by-value-from-end mesh) set-find-index-by-value-from-end)
  (let* ((inc-x         (d/ x-size x-divisions))
         (inc-z         (d/ z-size z-divisions))
         (text-inc-x    (d/ 1.0 x-divisions))
         (text-inc-z    (d/ 1.0 z-divisions))
         (h/2           (d/ h 2.0))
         (top-normal    +y-axe+))
    ;; top
    (loop
       for x from (d- (d/ x-size 2.0)) below (d/ x-size 2.0) by inc-x
       for xt from 0.0 below 1.0 by text-inc-x do
         (loop
            for z from (d- (d/ z-size 2.0)) below (d/ z-size 2.0) by inc-z
            for zt from 0.0 below 1.0 by text-inc-z do
            ;; a
              (put-simple-plane-vertex-w-texture mesh
                                                 x h/2 z
                                                 0.0 0.0
                                                 xt zt
                                                 0.0 0.0
                                                 :normal top-normal)
            ;; b
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 0.0 inc-z
                                                 xt zt
                                                 0.0 text-inc-z
                                                 :normal top-normal)
            ;; c
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 inc-x inc-z
                                                 xt zt
                                                 text-inc-x text-inc-z
                                                 :normal top-normal)
            ;; a
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 0.0 0.0
                                                 xt zt
                                                 0.0 0.0
                                                 :normal top-normal)
            ;; c
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 inc-x inc-z
                                                 xt zt
                                                 text-inc-x text-inc-z
                                                 :normal top-normal)
            ;; d
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 inc-x 0.0
                                                 xt zt
                                                 text-inc-x 0.0
                                                 :normal top-normal)))
    mesh))


(defun gen-ceiling (x-size z-size x-divisions z-divisions h &key (remove-orphaned-vertices nil))
  ;;             a        d
  ;;             +--------+--------+ ...
  ;;    x        |\-      |\-      |
  ;;   +----->   |  \-    |  \-    |
  ;; z |         |    \-  |    \-  |
  ;;   |         |      \-|c     \-|
  ;;   |        b+--------+--------+ ...
  ;;   v         |\-      |\-      |
  ;;             |  \-    |  \-    |
  ;;             |    \-  |    \-  |
  ;;             |      \-|      \-|
  ;;             +--------+--------+ ...
  (let* ((mesh          (make-instance 'triangle-mesh :find-index-by-value-from-end t))
         (inc-x         (d/ x-size x-divisions))
         (inc-z         (d/ z-size z-divisions))
         (text-inc-x    (d/ 1.0 x-divisions))
         (text-inc-z    (d/ 1.0 z-divisions))
         (h/2           (d/ h 2.0))
         (top-normal    +y-axe+)
         (bottom-normal (vec-negate +y-axe+)))
    ;; top
    (loop
       for x from (d- (d/ x-size 2.0)) below (d/ x-size 2.0) by inc-x
       for xt from 0.0 below 1.0 by text-inc-x do
         (loop
            for z from (d- (d/ z-size 2.0)) below (d/ z-size 2.0) by inc-z
            for zt from 0.0 below 1.0 by text-inc-z do
            ;; a
              (put-simple-plane-vertex-w-texture mesh
                                                 x h/2 z
                                                 0.0 0.0
                                                 xt zt
                                                 0.0 0.0
                                                 :normal top-normal)
            ;; b
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 0.0 inc-z
                                                 xt zt
                                                 0.0 text-inc-z
                                                 :normal top-normal)
            ;; c
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 inc-x inc-z
                                                 xt zt
                                                 text-inc-x text-inc-z
                                                 :normal top-normal)
            ;; a
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 0.0 0.0
                                                 xt zt
                                                 0.0 0.0
                                                 :normal top-normal)
            ;; c
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 inc-x inc-z
                                                 xt zt
                                                 text-inc-x text-inc-z
                                                 :normal top-normal)
            ;; d
              (put-simple-plane-vertex-w-texture mesh x h/2 z
                                                 inc-x 0.0
                                                 xt zt
                                                 text-inc-x 0.0
                                                 :normal top-normal)))
    ;; bottom
    (loop
       for x from (d- (d/ x-size 2.0)) below (d/ x-size 2.0) by inc-x
       for xt from 0.0 below 1.0 by text-inc-x do
         (loop
            for z from (d- (d/ z-size 2.0)) below (d/ z-size 2.0) by inc-z
            for zt from 0.0 below 1.0 by text-inc-z do
            ;; a
              (put-simple-plane-vertex-w-texture mesh x (d- h/2) z
                                                 0.0 0.0
                                                 xt zt
                                                 0.0 0.0
                                                 :normal bottom-normal)
            ;; c
              (put-simple-plane-vertex-w-texture mesh x (d- h/2) z
                                                 inc-x inc-z
                                                 xt zt
                                                 text-inc-x text-inc-z
                                                 :normal bottom-normal)
            ;; b
              (put-simple-plane-vertex-w-texture mesh x (d- h/2) z
                                                 0.0 inc-z
                                                 xt zt
                                                 0.0 text-inc-z
                                                 :normal bottom-normal)
            ;; a
              (put-simple-plane-vertex-w-texture mesh x (d- h/2) z
                                                 0.0 0.0
                                                 xt zt
                                                 0.0 0.0
                                                 :normal bottom-normal)
            ;; d
              (put-simple-plane-vertex-w-texture mesh x (d- h/2) z
                                                 inc-x 0.0
                                                 xt zt
                                                 text-inc-x 0.0
                                                 :normal bottom-normal)
            ;; c
              (put-simple-plane-vertex-w-texture mesh x (d- h/2) z
                                                 inc-x inc-z
                                                 xt zt
                                                 text-inc-x text-inc-z
                                                 :normal bottom-normal)))
    ;; sides
    (loop
       repeat 2
       for alpha from 0.0 by +pi+ do
         (with-pushed-matrix (mesh :what :modelview)
           (load-matrix mesh (identity-matrix))
           (mult-matrix mesh (translate (vec (d- (d/ x-size 2.0)) 0.0 0.0)))
           (mult-matrix mesh (translate (vec 0.0                  0.0 (d/ z-size 2.0))))
           (mult-matrix mesh (translate (vec 0.0                  (d- h/2) 0.0)))
           (mult-matrix mesh (rotate-around +y-axe+ alpha))
           (quad-w-explicit-texture-coords mesh
                                           x-size
                                           h
                                           (vector (vec2 0.0 0.0)  ; a
                                                   (vec2 1.0 0.0)  ; b
                                                   (vec2 0.0 1.0)  ; c
                                                   (vec2 1.0 0.0)  ; b
                                                   (vec2 1.0 1.0)  ; d
                                                   (vec2 0.0 1.0)) ; c
                                           +zero-vec+ nil t)))
    (loop
       repeat 2
       for alpha from 0.0 by +pi+ do
         (with-pushed-matrix (mesh :what :modelview)
           (load-matrix mesh (identity-matrix))
           (mult-matrix mesh (translate (vec (d- (d/ z-size 2.0)) 0.0 0.0)))
           (mult-matrix mesh (translate (vec 0.0                  0.0 (d/ x-size 2.0))))
           (mult-matrix mesh (translate (vec 0.0                  (d- h/2) 0.0)))
           (mult-matrix mesh (rotate-around +y-axe+ +pi/2+))
           (mult-matrix mesh (rotate-around +y-axe+ alpha))
           (quad-w-explicit-texture-coords mesh
                                           z-size
                                           h
                                           (vector (vec2 0.0 0.0)  ; a
                                                   (vec2 1.0 0.0)  ; b
                                                   (vec2 0.0 1.0)  ; c
                                                   (vec2 1.0 0.0)  ; b
                                                   (vec2 1.0 1.0)  ; d
                                                   (vec2 0.0 1.0)) ; c
                                           +zero-vec+ nil t)))
    (when remove-orphaned-vertices
      (remove-orphaned-vertices mesh))
    mesh))

(defun gen-skydome (radius &optional (parallel-div 16.0) (meridian-div 16.0))
  (let* ((mesh (make-instance 'skydome))
         (phi-step   (d/ +pi+ parallel-div))
         (theta-step (d/ +2pi+  meridian-div))
         (v +z-axe+))
    (loop for phi from (- +pi/2+) below (- phi-step) by phi-step do
         (loop for theta from 0.0 below +2pi+ by theta-step do
            ;;  c        d
            ;;   +-------+-------+
            ;;   |\      |      /|
            ;;   | \ T2  |     / |
            ;;   |  \    |    /  |
            ;;   |   \   |   /   |
            ;;   |    \  |  /    | ^ y phi
            ;;   | T1  \ | /     | |
            ;;   |      \|/      | |
            ;;   +-------+-------+ +--> z theta
            ;;   a        b
              (let* ((rot-theta  (rotate-around +y-axe+ theta))
                     (rot-theta+ (rotate-around +y-axe+ (d+ theta theta-step)))
                     (rot-phi    (rotate-around +x-axe+ phi))
                     (rot-phi+   (rotate-around +x-axe+ (d+ phi phi-step)))
                     (a (vec* (transform-direction
                               (transform-direction v rot-phi)
                               rot-theta)
                              radius))
                     (b (vec* (transform-direction
                               (transform-direction v rot-phi)
                               rot-theta+)
                              radius))
                     (c (transform-direction (transform-direction (vec* v radius) rot-phi+)
                                             rot-theta))
                     (d (vec* (transform-direction (transform-direction v rot-phi+)
                                                   rot-theta+)
                              radius))

                     (norm-a (normalize a))
                     (norm-b (normalize b))
                     (norm-c (normalize c))
                     (norm-d (normalize d)))
                (normal-v mesh norm-a)
                (tangent-v mesh norm-a)
                (texel mesh
                       (d+ 0.5 (d/ (datan (elt norm-a 2) (elt norm-a 0)) +2pi+))
                       (d- 0.5 (d/ (dasin (elt norm-a 1)) +pi+)))
                (vertex-v mesh a :gen-triangle t :compact-vertices t :manifoldp nil) ; a
                ;; b
                (normal-v mesh norm-b)
                (tangent-v mesh norm-b)
                (texel mesh
                       (d+ 0.5 (d/ (datan (elt norm-b 2) (elt norm-b 0)) +2pi+))
                       (d- 0.5 (d/ (dasin (elt norm-b 1)) +pi+)))
                (vertex-v mesh b :gen-triangle t :compact-vertices t :manifoldp nil)
                ;; c
                (normal-v mesh norm-c)
                (tangent-v mesh norm-c)
                (texel mesh
                       (d+ 0.5 (d/ (datan (elt norm-c 2) (elt norm-c 0)) +2pi+))
                       (d- 0.5 (d/ (dasin (elt norm-c 1)) +pi+)))
                (vertex-v mesh c :gen-triangle t :compact-vertices t :manifoldp nil) ;c
                ;; second triangle b->d->c
                (normal-v mesh norm-b)
                (tangent-v mesh norm-b)
                (texel mesh
                       (d+ 0.5 (d/ (datan (elt norm-b 2) (elt norm-b 0)) +2pi+))
                       (d- 0.5 (d/ (dasin (elt norm-b 1)) +pi+)))
                (vertex-v mesh b :gen-triangle t :compact-vertices t :manifoldp nil) ; a
                ;; d
                (normal-v mesh norm-d)
                (tangent-v mesh norm-d)
                (texel mesh
                       (d+ 0.5 (d/ (datan (elt norm-d 2) (elt norm-d 0)) +2pi+))
                       (d- 0.5 (d/ (dasin (elt norm-d 1)) +pi+)))
                (vertex-v mesh d :gen-triangle t :compact-vertices t :manifoldp nil)
                ;; c
                (normal-v mesh norm-c)
                (tangent-v mesh norm-c)
                (texel mesh
                       (d+ 0.5 (d/ (datan (elt norm-c 2) (elt norm-c 0)) +2pi+))
                       (d- 0.5 (d/ (dasin (elt norm-c 1)) +pi+)))
                (vertex-v mesh c :gen-triangle t :compact-vertices t :manifoldp nil))))
    mesh))
