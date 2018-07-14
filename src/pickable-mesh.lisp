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

(in-package :pickable-mesh)

(define-constant +attribute-pick-overlay+      :pick-overlay                :test #'eq)

(define-constant +color-tile-pick-can-move+    (vec4 0.408 0.737 0.945 1.0) :test #'vec4~)

(define-constant +color-tile-pick-cannot-move+ (vec4 1.0 0.0 0.0 1.0)       :test #'vec4~)

(define-constant +pick-color-lerp-weight+      0.2                          :test #'=)

(defclass pickable-tile ()
  ((triangle-1
    :initform nil
    :initarg :triangle-1
    :accessor triangle-1)
   (triangle-2
    :initform nil
    :initarg :triangle-2
    :accessor triangle-2)
   (index-tr-1
    :initform nil
    :initarg :index-tr-1
    :accessor index-tr-1)
   (index-tr-2
    :initform nil
    :initarg :index-tr-2
    :accessor index-tr-2)))

(defmethod marshal:class-persistant-slots ((object pickable-tile))
  '(triangle-1 triangle-2 index-tr-1 index-tr-2))

(defun init-pick-overlay-value ()
  (make-fresh-array 0 (num:desired 0.0) 'desired-type nil))

(defun init-pick-overlay-value* (size)
  (make-fresh-array size (num:desired 0.0) 'desired-type nil))

(defun init-highligthed-tiles-coords ()
  (make-fresh-array 0 nil 'uivec2 nil))

(definline pickable-mesh-p (w)
  (typep w 'pickable-mesh))

(defclass pickable-mesh (triangle-mesh)
  ((origin-offset
    :initform (vec 0.0 0.0 0.0)
    :initarg  :origin-offset
    :accessor origin-offset)
   (lookup-tile-triangle
    :initform nil
    :initarg  :lookup-tile-triangle
    :accessor lookup-tile-triangle
    :type matrix:matrix)
   (highligthed-tiles-coords
    :initform (init-highligthed-tiles-coords)
    :initarg  :highligthed-tiles-coords
    :accessor highligthed-tiles-coords
    :type array)
   (pick-overlay-values
    :initform (init-pick-overlay-value)
    :initarg  :pick-overlay-values
    :accessor pick-overlay-values)
   (renderer-data-count-pick-overlay
    :initform 0
    :initarg  :renderer-data-count-pick-overlay
    :accessor renderer-data-count-pick-overlay)
   (renderer-data-pick-overlay
    :initform nil
    :initarg  :renderer-data-pick-overlay
    :accessor renderer-data-pick-overlay)))

(defmethod marshal:class-persistant-slots ((object pickable-mesh))
  (append '(origin-offset
            lookup-tile-triangle
            highligthed-tiles-coords
            pick-overlay-values)
          (call-next-method)))

(defmethod clone-into :after ((from pickable-mesh) (to pickable-mesh))
  (setf (origin-offset        to) (copy-array (origin-offset from))
        (pick-overlay-values  to) (copy-array (pick-overlay-values from))
        (lookup-tile-triangle to) (clone      (lookup-tile-triangle from)))
  to)

(defmethod clone ((object pickable-mesh))
  (with-simple-clone (object 'pickable-mesh)))

(defmethod copy-flat-into :after ((from pickable-mesh) (to pickable-mesh))
  (setf (origin-offset        to) (origin-offset from)
        (pick-overlay-values  to) (pick-overlay-values from)
        (lookup-tile-triangle to) (lookup-tile-triangle from))
  to)

(defmethod copy-flat ((object pickable-mesh))
  (with-simple-copy-flat (object 'pickable-mesh)))

(defmethod destroy :after ((object pickable-mesh))
  (with-accessors ((renderer-data-pick-overlay renderer-data-pick-overlay)) object
    #+debug-mode
      (misc:dbg "destroy pickable-mesh ~a" (id object))
    (setf renderer-data-pick-overlay nil)))

(defmethod make-data-for-opengl :after ((object pickable-mesh))
  (with-accessors ((renderer-data-count-pick-overlay renderer-data-count-pick-overlay)
                   (renderer-data-pick-overlay renderer-data-pick-overlay)
                   (pick-overlay-values pick-overlay-values)) object
    (let ((weights (gl:alloc-gl-array :float (* 3 (length (triangles object))))))
      (loop
         for triangle in (triangles object)
         for ct from 0 by 3                 do
           (let ((pick-indices (get-custom-attribute triangle +attribute-pick-overlay+)))
             (loop
                for i across pick-indices
                for offset from 0 by 1 do
                  (let ((weight (elt pick-overlay-values i)))
                    (setf (fast-glaref weights (+ ct offset)) weight)))))
      (setf renderer-data-pick-overlay weights)
      (setf renderer-data-count-pick-overlay (* 3 (length (triangles object)))))
    ;; setup finalizer
    (let ((gl-arr-weight (slot-value object 'renderer-data-pick-overlay))
          #+debug-mode
          (id            (slot-value object 'id)))
      (tg:finalize object #'(lambda ()
                              #+debug-mode (misc:dbg "finalize destroy pickable ~a" id)
                              (free-memory* (list (gl:free-gl-array gl-arr-weight)) nil nil)
                              (setf gl-arr-weight nil))))))

(defmethod prepare-for-rendering :after ((object pickable-mesh))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (renderer-data-pick-overlay renderer-data-pick-overlay)) object
    (setf vbo (append vbo (gl:gen-buffers 1)))
    (mesh:make-data-for-opengl object)
    (with-unbind-vao
      ;; pick-weights
      (gl:bind-buffer :array-buffer (last-elt vbo))
      (gl:buffer-data :array-buffer :dynamic-draw renderer-data-pick-overlay)
      (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
      (gl:vertex-attrib-pointer +attribute-pick-weight-location+ 1 :float 0 0
                                (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-pick-weight-location+))
    object))

(defmethod update-for-rendering ((object pickable-mesh))
  (with-accessors ((vbo vbo) (vao vao)
                   (renderer-data-pick-overlay renderer-data-pick-overlay)
                   (pick-overlay-values pick-overlay-values)) object
    (with-unbind-vao
      (gl:bind-buffer :array-buffer (vbo-pick-weights-handle object))
      (gl:buffer-sub-data :array-buffer renderer-data-pick-overlay))
    object))

(defgeneric push-pickable-attribute (object value))

(defgeneric setup-pickable-attribute (object &key triangle-index pick-index))

(defgeneric setup-lookup-triangle-element (object &key
                                                    first-triangle-index
                                                    second-triangle-index))

(defgeneric setup-lookup-triangle-element* (object
                                            first-triangle second-triangle
                                            first-triangle-index second-triangle-index))

(defgeneric set-tile-highlight (object row column &key
                                                    weight
                                                    clear-highligthed-set
                                                    add-to-highligthed-set))

(defgeneric turn-off-highligthed-tiles (object))

(defgeneric add-highligthed-tiles-coords (object row column))

(defgeneric add-highligthed-tiles-coords* (object coord))

(defgeneric populate-lookup-triangle-matrix (object))

(defgeneric vbo-pick-weights-handle (object))

(defgeneric lookup-tile-triangle->dbg-matrix (object))

(defgeneric lookup-tile-coord->cost (object row column))

(defgeneric cost-coord->lookup-tile (object row column))

(defmethod push-pickable-attribute ((object pickable-mesh) value)
  (declare (desired-type value))
  (vector-push-extend value (pick-overlay-values object)))

(defmethod setup-pickable-attribute ((object pickable-mesh) &key
                                                            (triangle-index 0)
                                                            (pick-index 0))
  (let* ((first-triangle (elt (triangles object) triangle-index))
         (indices        (uivec pick-index pick-index pick-index)))
    (set-custom-attribute first-triangle +attribute-pick-overlay+ indices)))

(defmethod setup-lookup-triangle-element ((object pickable-mesh) &key
                                                                   (first-triangle-index 0)
                                                                   (second-triangle-index 1))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-accessors ((lookup-tile-triangle lookup-tile-triangle)
                   (triangles triangles)
                   (origin-offset origin-offset)) object
    (declare (list triangles))
    (declare (vec origin-offset))
    (let* ((x-origin                 (elt origin-offset 0))
           (z-origin                 (elt origin-offset 2))
           (first-triangle           (elt triangles first-triangle-index))
           (first-triangle-indices   (vertex-index first-triangle))
           (second-triangle          (elt triangles second-triangle-index))
           (first-triangle-vertices  (vector (find-value-by-index object
                                                                  (elt first-triangle-indices 0)
                                                                  :what :vertex)
                                             (find-value-by-index object
                                                                  (elt first-triangle-indices 1)
                                                                  :what :vertex)
                                             (find-value-by-index object
                                                                  (elt first-triangle-indices 2)
                                                                  :what :vertex)))
           (barycenter-first-triangle (3d-utils:triangle-centroid (elt first-triangle-vertices 0)
                                                                  (elt first-triangle-vertices 1)
                                                                  (elt first-triangle-vertices 2)))
           (row     (map-utils:coord-chunk->matrix (d- (elt barycenter-first-triangle 2)
                                                       z-origin)))
           (column  (map-utils:coord-chunk->matrix (d- (elt barycenter-first-triangle 0)
                                                       x-origin)))
           (element (make-instance 'pickable-tile
                                   :triangle-1 first-triangle
                                   :triangle-2 second-triangle
                                   :index-tr-1 first-triangle-index
                                   :index-tr-2 second-triangle-index)))
      (declare (uivec first-triangle-indices))
      (declare (vec barycenter-first-triangle))
      (setf (matrix:matrix-elt lookup-tile-triangle row column) element)
      (values row column))))

(defmethod setup-lookup-triangle-element* ((object pickable-mesh)
                                           first-triangle
                                           second-triangle
                                           first-triangle-index
                                           second-triangle-index)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (with-accessors ((lookup-tile-triangle lookup-tile-triangle)
                   (triangles triangles)
                   (origin-offset origin-offset)) object
    (declare (list triangles))
    (declare (vec origin-offset))
    (let* ((x-origin                 (elt origin-offset 0))
           (z-origin                 (elt origin-offset 2))
           (first-triangle-indices   (vertex-index first-triangle))
           (first-triangle-vertices  (vector (find-value-by-index object
                                                                  (elt first-triangle-indices 0)
                                                                  :what :vertex)
                                             (find-value-by-index object
                                                                  (elt first-triangle-indices 1)
                                                                  :what :vertex)
                                             (find-value-by-index object
                                                                  (elt first-triangle-indices 2)
                                                                  :what :vertex)))
           (barycenter-first-triangle (3d-utils:triangle-centroid (elt first-triangle-vertices 0)
                                                                  (elt first-triangle-vertices 1)
                                                                  (elt first-triangle-vertices 2)))
           (row     (map-utils:coord-chunk->matrix (d- (elt barycenter-first-triangle 2)
                                                       z-origin)))
           (column  (map-utils:coord-chunk->matrix (d- (elt barycenter-first-triangle 0)
                                                       x-origin)))
           (element (make-instance 'pickable-tile
                                   :triangle-1 first-triangle
                                   :triangle-2 second-triangle
                                   :index-tr-1 first-triangle-index
                                   :index-tr-2 second-triangle-index)))
      (declare (uivec first-triangle-indices))
      (declare (vec barycenter-first-triangle))
      (setf (matrix:matrix-elt lookup-tile-triangle row column) element)
      (values row column))))

(define-condition null-tile-element (warning)
  ((coordinates
    :initarg :coordinates
    :reader coordinates))
  (:report (lambda (condition stream)
             (format stream "Element null in row ~a, column ~a"
                     (elt (coordinates condition) 0)
                     (elt (coordinates condition) 1)))))

(define-condition out-of-bonds-tile-element (warning)
  ((coordinates
    :initarg :coordinates
    :reader coordinates)
   (mat
    :initarg :mat
    :reader   mat))
  (:report (lambda (condition stream)
             (format stream "Element in row ~a, column ~a for a marix ~aX~a"
                     (elt (coordinates condition) 0)
                     (elt (coordinates condition) 1)
                     (matrix:width  (mat condition))
                     (matrix:height (mat condition))))))

(defmethod pick-pointer-position ((object pickable-mesh) renderer x y)
  (with-accessors ((model-matrix model-matrix) (view-matrix view-matrix)
                   (lookup-tile-triangle lookup-tile-triangle)
                   (origin-offset origin-offset)) object
    (with-camera-view-matrix (camera-vw-matrix renderer)
      (with-camera-projection-matrix (camera-proj-matrix renderer :wrapped nil)
        (let* ((modelview    (matrix* camera-vw-matrix (elt view-matrix 0) (elt model-matrix 0)))
               (raw-position (pick-position x y modelview camera-proj-matrix
                                            *window-w* *window-h*))
               (x-origin (elt origin-offset 0))
               (z-origin (elt origin-offset 2))
               (cost-matrix-position (ivec2 (map-utils:coord-chunk->matrix (elt raw-position 0))
                                            (map-utils:coord-chunk->matrix (elt raw-position 2))))
               (column             (map-utils:coord-chunk->matrix (d- (elt raw-position 0)
                                                                      x-origin)))
               (row                (map-utils:coord-chunk->matrix (d- (elt raw-position 2)
                                                                      z-origin)))
               (matrix-position (ivec2 column row)))
          (handler-bind ((pickable-mesh:null-tile-element
                          #'(lambda(e)
                              (declare (ignore e))
                              (invoke-restart 'pickable-mesh::use-value nil)))
                         (pickable-mesh:out-of-bonds-tile-element
                          #'(lambda(e)
                              (declare (ignore e))
                              (invoke-restart 'pickable-mesh::use-value nil))))
            (restart-case
                (matrix:with-check-matrix-borders-then-else (lookup-tile-triangle column row)
                  (let ((tile (matrix:matrix-elt lookup-tile-triangle row column)))
                    (if tile
                        (values object cost-matrix-position matrix-position raw-position)
                        (error 'null-tile-element :coordinates (vector column row))))
                  (error 'out-of-bonds-tile-element :coordinates (vector column row)
                         :mat lookup-tile-triangle))
              (use-value (v) v))))))))

(defmethod lookup-tile-coord->cost ((object pickable-mesh) row column)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum row column))
  (with-accessors ((origin-offset origin-offset)) object
    (declare (vec origin-offset))
    (let* ((x-origin (elt origin-offset 0))
           (z-origin (elt origin-offset 2)))
      (declare (desired-type x-origin z-origin))
      (ivec2 (f+ column (the fixnum (map-utils:coord-chunk->matrix x-origin)))
             (f+ row    (the fixnum (map-utils:coord-chunk->matrix z-origin)))))))

(defmethod cost-coord->lookup-tile ((object pickable-mesh) row column)
  (with-accessors ((origin-offset origin-offset)) object
    (let* ((x-origin (elt origin-offset 0))
           (z-origin (elt origin-offset 2)))
      (ivec2 (f- column (map-utils:coord-chunk->matrix x-origin))
             (f- row    (map-utils:coord-chunk->matrix z-origin))))))

(defmethod set-tile-highlight ((object pickable-mesh) row column
                               &key
                                 (weight +pick-color-lerp-weight+)
                                 (clear-highligthed-set nil)
                                 (add-to-highligthed-set nil))
  "Row and column are integer coordinates relative to lookup-tile-triangle"
  (with-accessors ((renderer-data-pick-overlay renderer-data-pick-overlay)
                   (lookup-tile-triangle lookup-tile-triangle)
                   (pick-overlay-values pick-overlay-values)) object
    (restart-case
        (matrix:with-check-matrix-borders-then-else (lookup-tile-triangle column row)
          (let ((tile (matrix:matrix-elt lookup-tile-triangle row column)))
            (if tile
                (let* ((w1 (get-custom-attribute (triangle-1 tile)
                                                 +attribute-pick-overlay+))
                       (w2 (get-custom-attribute (triangle-2 tile)
                                                 +attribute-pick-overlay+))
                       (idx-triangle-1 (index-tr-1 tile))
                       (idx-triangle-2 (index-tr-2 tile))
                       (gl-arr-offset-1 (* idx-triangle-1 3))
                       (gl-arr-offset-2 (* idx-triangle-2 3)))
                  (when clear-highligthed-set
                    (turn-off-highligthed-tiles object))
                  (when add-to-highligthed-set
                    (add-highligthed-tiles-coords object row column))
                  (setf (fast-glaref renderer-data-pick-overlay gl-arr-offset-1) weight
                        (fast-glaref renderer-data-pick-overlay (+ gl-arr-offset-1 1)) weight
                        (fast-glaref renderer-data-pick-overlay (+ gl-arr-offset-1 2)) weight
                        (fast-glaref renderer-data-pick-overlay gl-arr-offset-2) weight
                        (fast-glaref renderer-data-pick-overlay (+ gl-arr-offset-2 1)) weight
                        (fast-glaref renderer-data-pick-overlay (+ gl-arr-offset-2 2)) weight)
                  (setf (elt pick-overlay-values (elt w1 0)) weight
                        (elt pick-overlay-values (elt w1 1)) weight
                        (elt pick-overlay-values (elt w1 2)) weight
                        (elt pick-overlay-values (elt w2 0)) weight
                        (elt pick-overlay-values (elt w2 1)) weight
                        (elt pick-overlay-values (elt w2 2)) weight)
                  (update-for-rendering object))
                (error 'null-tile-element :coordinates (vector column row))))
          (error 'out-of-bonds-tile-element :coordinates (vector column row)
                 :mat lookup-tile-triangle))
      (use-value (v) v))))

(defmethod turn-off-highligthed-tiles ((object pickable-mesh))
  (with-accessors ((highligthed-tiles-coords highligthed-tiles-coords)) object
    (loop for i across highligthed-tiles-coords do
         (set-tile-highlight object (elt i 1) (elt i 0) :weight 0.0))
    (setf highligthed-tiles-coords (init-highligthed-tiles-coords))
    object))

(defmethod add-highligthed-tiles-coords ((object pickable-mesh) row column)
  (add-highligthed-tiles-coords* object (uivec2 column row))
  object)

(defmethod add-highligthed-tiles-coords* ((object pickable-mesh) coord)
  (vector-push-extend coord (highligthed-tiles-coords object))
  object)

(defmethod populate-lookup-triangle-matrix ((object pickable-mesh))
  (loop
     for idx from 0   below (length (triangles object)) by 2
     for (tr tr2) on  (triangles object)                by #'cddr do
     ;; assign triandle indices for this tile
       (setup-lookup-triangle-element* object tr tr2 idx (1+ idx)))
  object)

(defmethod lookup-tile-triangle->dbg-matrix ((object pickable-mesh))
  (matrix:map-matrix (lookup-tile-triangle object)
                     #'(lambda (a) (if a t nil))))

(defmethod vbo-pick-weights-handle ((object pickable-mesh))
  (with-accessors ((vbo vbo)) object
    (elt vbo (- (length vbo) 2))))
