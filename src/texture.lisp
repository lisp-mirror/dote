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

(in-package :texture)

(alexandria:define-constant +skydome-bg+                "skydome-bg"         :test #'string=)

(alexandria:define-constant +cloud-1+                   "cloud-1"            :test #'string=)

(alexandria:define-constant +cloud-2+                   "cloud-2"            :test #'string=)

(alexandria:define-constant +cloud-3+                   "cloud-3"            :test #'string=)

(alexandria:define-constant +smoke-tray+                "smoke-tray"         :test #'string=)

(alexandria:define-constant +brick-wall+                "brick-wall"         :test #'string=)

(alexandria:define-constant +dry-stone-wall+            "dry-stone-wall"     :test #'string=)

(alexandria:define-constant +stone-floor-road+          "stone-floor-road"   :test #'string=)

(alexandria:define-constant +grass-stones-floor+        "grass-stones-floor" :test #'string=)

(alexandria:define-constant +soil+                      "soil"               :test #'string=)

(alexandria:define-constant +dry-soil+                  "dry-soil"           :test #'string=)

(alexandria:define-constant +wood-wall+                 "wood-wall"          :test #'string=)

(alexandria:define-constant +luxurious-floor-1+         "floor-1"            :test #'string=)

(alexandria:define-constant +luxurious-floor-2+         "floor-2"            :test #'string=)

(alexandria:define-constant +decal-wall-1+              "decal-wall-1"       :test #'string=)

(alexandria:define-constant +decal-wall-2+              "decal-wall-2"       :test #'string=)

(alexandria:define-constant +blood-splat+               "blood-splat"        :test #'string=)

(alexandria:define-constant +blood-particle+            "blood-particle"     :test #'string=)

(alexandria:define-constant +fire-particle+             "fire-particle"      :test #'string=)

(alexandria:define-constant +smoke-particle+            "smoke-particle"     :test #'string=)

(alexandria:define-constant +cross-particle+            "cross-particle"     :test #'string=)

(alexandria:define-constant +rock-1+                    "rock-1"             :test #'string=)

(alexandria:define-constant +rock-2+                    "rock-2"             :test #'string=)

(alexandria:define-constant +sand+                      "sand"               :test #'string=)

(alexandria:define-constant +grass+                     "grass"              :test #'string=)

(alexandria:define-constant +snow+                      "snow"               :test #'string=)

(alexandria:define-constant +texture-tag-wall-level-1+     "wall-level-1"     :test #'string= )

(alexandria:define-constant +texture-tag-wall-level-2+     "wall-level-2"     :test #'string= )

(alexandria:define-constant +texture-tag-wall-level-3+     "wall-level-3"     :test #'string=)

(alexandria:define-constant +texture-tag-door-level-1+     "door-level-1"     :test #'string= )

(alexandria:define-constant +texture-tag-door-level-2+     "door-level-2"     :test #'string= )

(alexandria:define-constant +texture-tag-door-level-3+     "door-level-3"     :test #'string=)

(alexandria:define-constant +texture-tag-ceil-level-1+     "ceil-level-1"     :test #'string= )

(alexandria:define-constant +texture-tag-ceil-level-2+     "ceil-level-2"     :test #'string= )

(alexandria:define-constant +texture-tag-ceil-level-3+     "ceil-level-3"     :test #'string=)

(alexandria:define-constant +texture-tag-floor-level-1+    "floor-level-1"    :test #'string=)

(alexandria:define-constant +texture-tag-floor-level-2+    "floor-level-2"    :test #'string=)

(alexandria:define-constant +texture-tag-floor-level-3+    "floor-level-3"    :test #'string=)

(alexandria:define-constant +texture-tag-int-decal+        "int-decal"        :test #'string=)

(alexandria:define-constant +texture-tag-ext-decal+        "ext-decal"        :test #'string=)

(alexandria:define-constant +texture-tag-soil-decal+       "soil-decal"       :test #'string=)

(alexandria:define-constant +texture-tag-road-decal+       "road-decal"       :test #'string=)

(alexandria:define-constant +texture-tag-building-decal+   "building-decal"   :test #'string=)

(alexandria:define-constant +texture-tag-home-terrain+     "home-terrain"     :test #'string= )

(alexandria:define-constant +texture-tag-shore-terrain+    "shore-terrain"    :test #'string= )

(alexandria:define-constant +texture-tag-grass-terrain+    "grass-terrain"    :test #'string= )

(alexandria:define-constant +texture-tag-snow-terrain+     "snow-terrain"     :test #'string= )

(alexandria:define-constant +texture-tag-smoke-particle+   "smoke-particle"   :test #'string= )

(alexandria:define-constant +texture-tag-aerial-expl-particle+ "aerial-explosion-particle"
   :test #'string= )

(alexandria:define-constant +texture-tag-decals-explosion+ "decals-explosion-particle"
  :test #'string=)

(alexandria:define-constant +texture-tag-decals-cure+      "decals-cure-particle"
  :test #'string=)

(alexandria:define-constant +texture-tag-decals-circular-wave+ "decals-circular-wave"
  :test #'string=)

(alexandria:define-constant +texture-tag-explosion-debris+     "explosion-debris"
  :test #'string=)

(alexandria:define-constant +texture-tag-soil-terrain-level-1+ "soil-level-1" :test #'string= )

(alexandria:define-constant +texture-tag-soil-terrain-level-2+ "soil-level-2" :test #'string= )

(alexandria:define-constant +texture-tag-soil-terrain-level-3+ "soil-level-3" :test #'string= )

(alexandria:define-constant +mipmap-base-level+ 0 :test #'=)

(alexandria:define-constant +mipmap-max-level+  4 :test #'=)

(alexandria:define-constant +id-handle-invalid+  -1 :test #'=)

(defparameter *texture-factory-db* '())

(defun clean-db ()
  (map 'nil #'destroy *texture-factory-db*)
  (setf *texture-factory-db* nil))

(defun free-memory (texture-handle)
  (when (/= texture-handle +id-handle-invalid+)
    (gl:delete-textures (list texture-handle))))

(defclass texture (renderizable destructible tga)
  ((handle
    :initform +id-handle-invalid+
    :initarg :handle
    :accessor handle)
   (filename
    :initform nil
    :initarg :filename
    :accessor filename)
   (interpolation-type
    :initform :nearest
    :initarg :interpolation-type
    :accessor interpolation-type)
   (s-wrap-mode
    :initform :repeat
    :initarg :s-wrap-mode
    :accessor s-wrap-mode)
   (t-wrap-mode
    :initform :repeat
    :initarg :t-wrap-mode
    :accessor t-wrap-mode)
   (border-color
    :initform §cff0000ff
    :initarg :border-color
    :accessor border-color)
   (use-mipmap
    :initform nil
    :initarg :use-mipmap
    :accessor use-mipmap)
   (tags
    :initform (misc:make-array-frame 0 "" 'string nil)
    :initarg :tags
    :accessor tags)
   (normalmap-params
    :initform (make-instance 'mesh-material)
    :initarg :normalmap-params
    :accessor normalmap-params)
   (prepared-for-rendering
    :initform nil
    :initarg :prepared-for-rendering
    :accessor prepared-for-rendering)))

(defmethod print-object ((object texture) stream)
  (format stream "texture (~aX~a) handle ~a filename ~a tags ~s normalmap ~a"
	  (width object) (height object)
	  (handle object) (filename object) (tags object)
	  (normalmap-params object)))

(defmethod marshal:class-persistant-slots ((object texture))
  ;; note that we do not serialize prepared-for-rendering
  (append '(filename
	    interpolation-type
	    s-wrap-mode
	    t-wrap-mode
	    use-mipmap
	    border-color
	    tags
	    normalmap-params)
	  (call-next-method)))

(defmethod initializedp ((object texture))
  (prepared-for-rendering object))

(defmethod destroy :after ((object texture))
  (when (and (initializedp object)
	     (/= (handle object) +id-handle-invalid+))
    (tg:cancel-finalization object)
    (free-memory (handle object))
    (setf (handle object) +id-handle-invalid+)))

(defmethod clone ((object texture))
  (let ((res (make-instance 'texture)))
    (clone-into object res)
    res))

(defmethod clone-into :after ((from texture) (to texture))
  (setf (filename  to)           (filename from)
	(interpolation-type to)  (interpolation-type      from)
	(s-wrap-mode        to)  (s-wrap-mode             from)
	(t-wrap-mode        to)  (t-wrap-mode             from)
	(border-color       to)  (vec4:copy-vec4          (border-color from))
	(use-mipmap         to)  (use-mipmap              from)
	(tags               to)  (alexandria:copy-array   (tags from))
	(normalmap-params   to)  (clone (normalmap-params from)))
  to)

(defmethod tileize :after ((object texture) &key
					      (blend-replace-fn (guess-correct-replace-fn object)))
  (declare (ignore blend-replace-fn))
  (pixmap:sync-data-to-bits object)
  (prepare-for-rendering object))

(defmethod voronoize :after ((object texture) &key
					(size (width object))
					(tile-divisions (floor (/ (width object) 15)))
					(mean 8)
					(sigma 2)
					(max 10)
					(average-size 5))
  (declare (ignore size tile-divisions mean sigma max average-size))
  (pixmap:sync-data-to-bits object)
  (prepare-for-rendering object))

(defgeneric gen-name-and-inject-in-database (object))

(defgeneric gen-name (object))

(defgeneric setup-texture-parameters (object))

(defgeneric get-texture (handle))

(defgeneric bind-texture (object))

(defgeneric unbind-texture (object))

(defgeneric setup-finalizer (object))

(defmethod gen-name-and-inject-in-database ((object pixmap))
  (let ((texture (make-instance 'texture
				:data   (alexandria:copy-array (data object))
				:depth  4
				:width  (width object)
				:height (height object))))
    (pixmap:sync-data-to-bits texture)
    (gen-name-and-inject-in-database texture)
    texture))

(defmethod gen-name-and-inject-in-database ((object texture))
  "Generate a new handle each time is called"
  (let ((handle (first (gl:gen-textures 1))))
    (if (> handle 0)
	(progn
	  (setf (handle object) handle)
	  (setup-finalizer object)
	  (setf (filename object) (text-utils:strcat (filename object)
						     (format nil "~a" handle)))
	  (push object *texture-factory-db*)
	  object)
	nil)))

(defmethod gen-name ((object texture))
  "Generate a new handle each time is called"
  (let ((handle (first (gl:gen-textures 1))))
    (if (> handle 0)
	(progn
	  (destroy object)
	  (setf (handle object) handle)
	  (setf (filename object) (text-utils:strcat (filename object)
						     (format nil "~a" handle)))
	  (setup-finalizer object)
	  object)
	nil)))

(defmethod get-texture ((handle number))
  (let ((found
	 (find-if #'(lambda (texture) (= (handle texture) handle)) *texture-factory-db*)))
    (or found
	(allocate-empty-texture))))

(defmethod get-texture ((handle string))
  (let ((found
	 (find-if #'(lambda (texture) (string= (filename texture) handle))
		  *texture-factory-db*)))
    (or found
	(allocate-texture handle))))

(defun list-of-texture-by-tag (tag)
  (remove-if #'(lambda (texture)
		 (or (= (length (tags texture)) 0)
		     (not (find tag (tags texture) :test #'string=))))
	     *texture-factory-db*))

(defun not-empty-bits-or-null-pointer (bits)
  (if (misc:vector-empty-p bits)
      (misc:make-null-pointer)
      bits))

(defmethod prepare-for-rendering ((object texture))
  (with-accessors ((handle handle) (bits bits) (w width) (h height)
		   (interpolation-type interpolation-type)
		   (s-wrap-mode s-wrap-mode) (t-wrap-mode t-wrap-mode)
		   (border-color border-color)
		   (use-mipmap use-mipmap)
		   (prepared-for-rendering prepared-for-rendering))object
    (if (initializedp object)
	(update-for-rendering object)
	(let ((actual-bits (not-empty-bits-or-null-pointer bits)))
	  (if use-mipmap
	      (progn
		(gl:bind-texture :texture-2d handle)
		(gl:tex-parameter :texture-2d :texture-base-level +mipmap-base-level+)
		(gl:tex-parameter :texture-2d :texture-max-level  +mipmap-max-level+)
		(gl:tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte actual-bits)
		(gl:generate-mipmap :texture-2d)
		(gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
		(gl:tex-parameter :texture-2d :texture-mag-filter interpolation-type))
	      (progn
		(setup-texture-parameters object)
		(gl:tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte actual-bits)))
	  (setf prepared-for-rendering t)))))

(defmethod update-for-rendering ((object texture))
  (with-accessors ((handle handle) (bits bits) (w width) (h height)
		   (interpolation-type interpolation-type)
		   (s-wrap-mode s-wrap-mode) (t-wrap-mode t-wrap-mode)
		   (border-color border-color)
		   (use-mipmap use-mipmap)) object
    (let ((actual-bits (not-empty-bits-or-null-pointer bits)))
      (if use-mipmap
	  (progn
	    (gl:bind-texture :texture-2d handle)
	    (gl:tex-parameter :texture-2d :texture-base-level +mipmap-base-level+)
	    (gl:tex-parameter :texture-2d :texture-max-level  +mipmap-max-level+)
	    (gl:tex-sub-image-2d :texture-2d 0 0 0 w h :rgba :unsigned-byte actual-bits)
	    (gl:generate-mipmap :texture-2d)
	    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
	    (gl:tex-parameter :texture-2d :texture-mag-filter interpolation-type))
	  (progn
	    (setup-texture-parameters object)
	    (gl:tex-sub-image-2d :texture-2d 0 0 0 w h :rgba :unsigned-byte actual-bits))))))

(defmethod setup-texture-parameters ((object texture))
  (with-accessors ((handle handle)
		   (interpolation-type interpolation-type)
		   (s-wrap-mode s-wrap-mode) (t-wrap-mode t-wrap-mode)
		   (border-color border-color)
		   (use-mipmap use-mipmap)) object
    (gl:bind-texture :texture-2d handle)
    (gl:tex-parameter :texture-2d :texture-min-filter interpolation-type)
    (gl:tex-parameter :texture-2d :texture-mag-filter interpolation-type)
    (gl:tex-parameter :texture-2d :texture-wrap-s s-wrap-mode)
    (gl:tex-parameter :texture-2d :texture-wrap-t t-wrap-mode)
    (gl:tex-parameter :texture-2d :texture-border-color border-color)))

(defmethod setup-finalizer ((object texture))
  (let ((handle   (slot-value object 'handle))
	(filename (slot-value object 'filename)))
    (tg:finalize object #'(lambda ()
			    (when +debug-mode+
			      (misc:dbg "finalize texture ~a ~a" handle filename))
			    (free-memory handle)))))

(defmethod bind-texture ((object texture))
  (unbind-texture object)
  (gl:bind-texture :texture-2d (handle object)))

(defmethod unbind-texture (object)
  (gl:bind-texture :texture-2d 0))

(defmacro gen-normalmap-params-reader (name)
  (let ((fn-name (alexandria:format-symbol t "~:@(n-~a~)" name)))
    `(progn
       (defgeneric ,fn-name (object))
       (defmethod  ,fn-name ((object texture))
	 (declare (optimize (debug 0) (safety 0) (speed 3)))
	 (,(alexandria:format-symbol t "~:@(~a~)" name)
	  (normalmap-params object))))))

(gen-normalmap-params-reader ka)

(gen-normalmap-params-reader kd)

(gen-normalmap-params-reader ks)

(gen-normalmap-params-reader roughness)

(gen-normalmap-params-reader shininess)

(defun allocate-empty-texture ()
  (let ((handle (first (gl:gen-textures 1))))
    (if (> handle 0)
	(let ((new-texture (make-instance 'texture :handle handle)))
	  (setup-finalizer new-texture)
	  (push new-texture *texture-factory-db*)
	  handle)
      nil)))

(defun allocate-texture (file)
  (let ((handle (allocate-empty-texture))
	(texture (first *texture-factory-db*)))
    (if handle
	(progn
	  (pixmap:load texture file)
	  (setf (filename texture) file)
	  (if (not (null (errors texture)))
	      (values nil (errors texture))
	      (progn
		(setup-finalizer texture)
		(values texture nil))))
	(values nil (list
		     (format nil "Can not create texture handle from file ~a" file))))))
