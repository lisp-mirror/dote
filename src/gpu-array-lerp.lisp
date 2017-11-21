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

(in-package :gpu-array-lerp)

(define-constant +attribute-a-location+        0                  :test #'=)

(define-constant +attribute-b-location+        1                  :test #'=)

(define-constant +vbo-count+                   3                  :test #'=)

(define-constant +shader-name+       :array-lerp                  :test #'eq)

(defclass array-mixer (renderizable)
  ((lerp-vao
    :initform '()
    :initarg  :lerp-vao
    :accessor lerp-vao)
   (lerp-vbo
    :initform '()
    :initarg  :lerp-vbo
    :accessor lerp-vbo)
   (array-size
    :initform -1
    :initarg  :array-size
    :accessor array-size)))

(defgeneric bind-computational-buffers (object array-a array-b))

(defgeneric prepare (object))

(defgeneric mix (object w array-a array-b array-dest))

(defun vbo-input-a (vbo)
  (elt vbo 0))

(defun vbo-input-b (vbo)
  (elt vbo 1))

(defun vbo-output (vbo)
  (elt vbo 2))

(defmethod bind-computational-buffers ((object array-mixer) array-a array-b)
  (with-accessors ((lerp-vao lerp-vao)
                   (lerp-vbo lerp-vbo)
                   (array-size array-size)) object
    #+debug-mode (assert (= (gl-utils:array-byte-size array-a)
                            (gl-utils:array-byte-size array-b)))
    (setf array-size (gl-utils:array-size array-a))
    (cl-gl-utils:with-unbind-vao
      (gl:bind-vertex-array (elt lerp-vao 0))
      ;; input-a
      (gl:bind-buffer :array-buffer (vbo-input-a lerp-vbo))
      (gl:buffer-data :array-buffer :static-draw array-a)
      (gl:vertex-attrib-pointer +attribute-a-location+ 1 :float 0 0 (gl-utils:mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-a-location+)
      ;; input-b
      (gl:bind-buffer :array-buffer (vbo-input-b lerp-vbo))
      (gl:buffer-data :array-buffer :static-draw array-b)
      (gl:vertex-attrib-pointer +attribute-b-location+ 1 :float 0 0 (gl-utils:mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-b-location+)
      ;; output
      (gl:bind-buffer :array-buffer (vbo-output lerp-vbo))
      (gl:buffer-data :array-buffer :dynamic-copy (gl:make-null-gl-array :float)
                      :size (gl-utils:array-byte-size array-a)))))

(defmethod prepare ((object array-mixer))
  (with-accessors ((lerp-vao lerp-vao)
                   (lerp-vbo lerp-vbo)) object
    (setf lerp-vbo (gl:gen-buffers +vbo-count+))
    (setf lerp-vao (gl:gen-vertex-arrays 1))
    (let ((vbos (slot-value object 'lerp-vbo))
          (vaos (slot-value object 'lerp-vao)))
      (tg:finalize object
                   #'(lambda ()
                       #+debug-mode (misc:dbg "finalize destroy array-lerp")
                       (mesh:free-memory* nil
                                          vbos vaos))))))

(defmethod mix ((object array-mixer) w array-a array-b array-dest)
  (with-accessors ((lerp-shaders lerp-shaders)
                   (lerp-vbo lerp-vbo)
                   (lerp-vao lerp-vao)
                   (array-size array-size)
                   (compiled-shaders compiled-shaders)) object
    #+debug-mode (assert (gl::gl-array-p array-dest))
    #+debug-mode (assert (and (gl::gl-array-p array-a)
                              (gl::gl-array-p array-b)))
    #+debug-mode (assert (= (gl-utils:array-byte-size array-a)
                            (gl-utils:array-byte-size array-b)
                            (gl-utils:array-byte-size array-dest)))
    (cl-gl-utils:with-unbind-vao
      (bind-computational-buffers object array-a array-b)
      (shaders-utils:use-program compiled-shaders +shader-name+)
      (shaders-utils:uniformf compiled-shaders :w w)
      (cl-gl-utils:with-rasterizer-discard
        (%gl:bind-buffer-base :transform-feedback-buffer 0 (vbo-output lerp-vbo))
        (cl-gl-utils:with-transform-feedback (:points)
          (gl:bind-vertex-array (elt lerp-vao 0))
          (gl:draw-arrays :points 0 array-size)
          (gl:flush)
          (cffi:with-foreign-object (res-array :float (gl-utils:array-byte-size array-a))
            (%gl:get-buffer-sub-data :transform-feedback-buffer
                                     0
                                     (gl-utils:array-byte-size array-a)
                                     res-array)
          (loop for i fixnum from 0 below array-size  by 1 do
                   (setf (cl-gl-utils:fast-glaref array-dest i)
                         (cffi:mem-aref res-array :float i)))))))))
