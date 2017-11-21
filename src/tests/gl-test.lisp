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

(in-package :gl-test)

(defsuite gl-suite (all-suite))

(defclass test-3-lerp-window (sdl2.kit:gl-window)
  ((res
    :initform '()
    :accessor res)
   (compiled-shaders
    :initform nil
    :accessor compiled-shaders
    :initarg :compiled-shaders)
   (shaders-dictionary
    :initform nil
    :accessor shaders-dictionary
    :initarg :shaders-dictionary)))

(defmethod initialize-instance :after ((object test-3-lerp-window) &key &allow-other-keys)
  (setf (compiled-shaders object) (shaders-utils:compile-library))
  (let ((a     (gl-utils:seq->gl-array '(0.0 0.5 0.2)))
        (b     (gl-utils:seq->gl-array '(1.0 1.0 0.3)))
        (o     (gl-utils:seq->gl-array '(0.0 0.0 0.0)))
        (mixer (make-instance 'gpu-lerp:array-mixer
                              :compiled-shaders (compiled-shaders object))))
    (gpu-lerp:prepare mixer)
    (gpu-lerp:mix mixer 0.5 a b o)
    (setf (res object) (gl-utils:gl-array->list o))
    (gl:free-gl-array a)
    (gl:free-gl-array b)
    (gl:free-gl-array o)))

(defmethod close-window ((w test-3-lerp-window))
  (call-next-method))

(deftest lerp-3-test (gl-suite)
  (sdl2.kit:start)
  (sdl2:gl-set-attr :context-profile-mask  1)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (let ((w (make-instance 'test-3-lerp-window :w 200 :h 200)))
    (assert-true
        (every #'num:epsilon=
               (res w)
               (list 0.5 0.75 0.25)))
    (sdl2.kit:close-window w)))

(defclass test-random-lerp-window (test-3-lerp-window)
  ((res-cpu
    :accessor res-cpu)))

(defmethod initialize-instance :after ((object test-random-lerp-window)
                                       &key &allow-other-keys)
  (setf (compiled-shaders object) (shaders-utils:compile-library))
  (flet ((init-array ()
           (loop repeat 10000 collect (num:d (num:lcg-next01)))))
    (let ((a     (gl-utils:seq->gl-array (init-array)))
          (b     (gl-utils:seq->gl-array (init-array)))
          (o     (gl-utils:seq->gl-array (init-array)))
          (o-cpu (gl-utils:seq->gl-array (init-array)))
          (mixer (make-instance 'gpu-lerp:array-mixer
                                :compiled-shaders (compiled-shaders object)))
          (w     0.5))
      (lerp-gl-array a b o-cpu (gl::gl-array-size a) w)
      (gpu-lerp:prepare mixer)
      (gpu-lerp:mix mixer w a b o)
      (setf (res     object) (gl-utils:gl-array->list o))
      (setf (res-cpu object) (gl-utils:gl-array->list o-cpu))
      (gl:free-gl-array a)
      (gl:free-gl-array b)
      (gl:free-gl-array o)
      (gl:free-gl-array o-cpu)
      object)))

(defmethod close-window ((w test-random-lerp-window))
  (call-next-method))

(deftest lerp-random-test (gl-suite)
  (sdl2.kit:start)
  (sdl2:gl-set-attr :context-profile-mask  1)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (let ((w (make-instance 'test-random-lerp-window :w 200 :h 200)))
    (assert-true
        (every #'num:epsilon= (res w) (res-cpu w)))
    (sdl2.kit:close-window w)))
