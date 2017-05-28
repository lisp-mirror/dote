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

(in-package :cl-gl-utils)

(deftype +transform-matrix-cointainer+ ()
  `(simple-array (simple-array single-float (16)) (1)))

(defmacro with-unbind-vao (&body body)
  `(unwind-protect
        (progn
          ,@body)
     (gl:bind-vertex-array 0)))

(defmacro with-no-cull-face (&body body)
  `(progn
     (gl:disable :cull-face)
     ,@body
     (gl:enable :cull-face)))

(defmacro with-clip-plane (&body body)
  `(progn
    (gl:enable :clip-distance0)
    ,@body
    (gl:disable :clip-distance0)))

(defmacro with-rasterizer-discard (&body body)
  `(progn
     (gl:enable :rasterizer-discard)
     ,@body
     (gl:disable :rasterizer-discard)))

(defmacro with-transform-feedback ((type) &body body)
  `(progn
     (%gl:begin-transform-feedback ,type)
     ,@body
     (%gl:end-transform-feedback)))

(defmacro with-blending (&body body)
  `(progn
     (gl:enable :blend)
     ,@body
     (gl:disable :blend)))

(defmacro with-depth-disabled (&body body)
  `(progn
     (gl:disable :depth-test)
     ,@body
     (gl:enable :depth-test)))

(defmacro with-depth-mask-disabled (&body body)
  `(progn
     (gl:depth-mask nil)
     ,@body
     (gl:depth-mask t)))

(definline fast-glaref (v offset)
  (cffi:mem-aref (gl::gl-array-pointer v) :float offset))

(definline set-fast-glaref (v offset new-val)
  (setf (cffi:mem-aref (gl::gl-array-pointer v) :float offset)
        new-val)
  new-val)

(definline mock-null-pointer ()
  0)

(defsetf fast-glaref set-fast-glaref)

(defgeneric seq->gl-array (seq))

(defmethod seq->gl-array ((seq vector))
  (declare (optimize (debug 0) (safety 0) (speed 1)))
  (declare ((array desired-type (*)) seq))
  (let ((results (gl:alloc-gl-array :float (length seq))))
    (loop for i fixnum from 0 below (length seq) do
         (setf (fast-glaref results i) (aref seq i)))
    results))

(defmethod seq->gl-array ((seq list))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (let ((results (gl:alloc-gl-array :float (length seq))))
    (loop for i fixnum from 0 below (length seq) do
         (setf (fast-glaref results i) (elt seq i)))
    results))

(defun copy-gl-array (a b count)
  (loop for i from 0 below count do (setf (fast-glaref b i) (fast-glaref a i))))

(defun lerp-gl-array (a b c count interpolation-factor)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (fixnum count))
  (declare (desired-type interpolation-factor))
  (dotimes (i count)
    (declare (fixnum i))
    (setf (fast-glaref c i)
      (alexandria:lerp interpolation-factor
                       (fast-glaref a i)
                       (fast-glaref b i)))))

(defun gl-array->list (seq)
  (loop for i fixnum from 0 below (gl::gl-array-size seq) collect
         (fast-glaref seq i)))

(defun prepare-framebuffer-for-rendering  (framebuffer depthbuffer texture w h)
  (gl:bind-framebuffer :framebuffer framebuffer)
  (gl:bind-texture  :texture-2d texture)
  (gl:tex-image-2d  :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte (misc:make-null-pointer))
  ;; depth
  (gl:bind-renderbuffer :renderbuffer depthbuffer)
  (gl:renderbuffer-storage :renderbuffer :depth-component w h)
  (gl:framebuffer-renderbuffer :framebuffer :depth-attachment :renderbuffer depthbuffer)
  (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture 0)
  (gl:draw-buffers (list :color-attachment0))
  ;; validate framebuffer
  (let ((framebuffer-status (gl:check-framebuffer-status :framebuffer)))
    (unless (gl::enum= framebuffer-status :framebuffer-complete)
      (error "Framebuffer not complete: ~A." framebuffer-status)))
  (gl:bind-framebuffer :framebuffer framebuffer)
  (gl:viewport 0.0 0.0 w h)
  (gl:clear :color-buffer :depth-buffer))

(defun render-to-memory-texture (framebuffer depthbuffer texture w h fn)
  (prepare-framebuffer-for-rendering framebuffer depthbuffer texture w h)
  ;; draw
  (funcall fn)
  (gl:bind-framebuffer :framebuffer 0)
  (gl:bind-texture :texture-2d 0))

(defmacro with-render-to-memory-texture ((framebuffer depthbuffer texture w h) &body body)
  `(render-to-memory-texture ,framebuffer ,depthbuffer ,texture ,w ,h
                             #'(lambda () (progn ,@body))))

(defun render-to-pixmap (framebuffer depthbuffer texture w h fn)
  (prepare-framebuffer-for-rendering framebuffer depthbuffer texture w h)
  ;; draw
  (funcall fn)
  (gl:pixel-store :pack-alignment 1)
  (let* ((dump (gl:read-pixels 0 0 w h :rgba :unsigned-byte))
         (pixmap (pixmap:make-pixmap w h)))
    (setf (pixmap:bits pixmap) dump)
    (pixmap:sync-bits-to-data pixmap)
    (matrix:ploop-matrix (pixmap x y)
      (setf (elt (matrix:pixel@ pixmap x y) 3) 255))
    (gl:bind-framebuffer :framebuffer 0)
    (gl:bind-texture :texture-2d 0)
    pixmap))

(defun render-to-file (file framebuffer depthbuffer texture w h fn)
  (let ((pixmap (render-to-pixmap framebuffer depthbuffer texture w h fn)))
    (pixmap:save-pixmap pixmap file)))

(defmacro with-render-to-file ((file w h) &body body)
  (alexandria:with-gensyms (texture framebuffers depthbuffers framebuffer depthbuffer)
    `(let* ((,framebuffers (gl:gen-framebuffers 1))
            (,depthbuffers (gl:gen-renderbuffers 1))
            (,framebuffer  (elt ,framebuffers 0))
            (,depthbuffer  (elt ,depthbuffers 0))
            (,texture      (make-instance 'texture:texture
                                          :width ,w
                                          :height ,h
                                          :interpolation-type :linear)))
       (texture:gen-name ,texture)
       (texture:prepare-for-rendering ,texture)
       (unwind-protect
            (render-to-file ,file ,framebuffer ,depthbuffer (texture:handle ,texture) ,w ,h
                            #'(lambda () (progn ,@body)))
         (gl:delete-framebuffers  ,framebuffers)
         (gl:delete-renderbuffers ,depthbuffers)
         (interfaces:destroy ,texture)))))

(defmacro with-render-to-pixmap ((w h) &body body)
  (alexandria:with-gensyms (texture framebuffers depthbuffers framebuffer depthbuffer)
    `(let* ((,framebuffers (gl:gen-framebuffers 1))
            (,depthbuffers (gl:gen-renderbuffers 1))
            (,framebuffer  (elt ,framebuffers 0))
            (,depthbuffer  (elt ,depthbuffers 0))
            (,texture      (make-instance 'texture:texture
                                          :width ,w
                                          :height ,h
                                          :interpolation-type :linear)))
       (texture:gen-name ,texture)
       (texture:prepare-for-rendering ,texture)
       (unwind-protect
            (render-to-pixmap ,framebuffer ,depthbuffer (texture:handle ,texture) ,w ,h
                            #'(lambda () (progn ,@body)))
         (gl:delete-framebuffers  ,framebuffers)
         (gl:delete-renderbuffers ,depthbuffers)
         (interfaces:destroy ,texture)))))

(defun pick-position (x y modelview-matrix projection-matrix win-w win-h)
  (let* ((dx (d x))
         (z  (elt (gl:read-pixels x (f- win-h y) 1 1 :depth-component :float) 0)))
    ;; Note gl:read-pixels return a vector
    (3d-utils:unproject dx (d (f- win-h y)) z modelview-matrix projection-matrix
                        0.0 0.0 (d win-w) (d win-h))))
