;; Dawn of the era: a tactical game.
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

(in-package :gui)

(defparameter *fonts-db* (make-hash-table :test 'eql))

(alexandria:define-constant +ascii-size+ 128 :test #'=)

(alexandria:define-constant +font-file-extension+ ".tga" :test #'string=)

(alexandria:define-constant +default-font+ "default" :test #'string=)

(alexandria:define-constant +default-font-handle+ 0 :test #'=)

(alexandria:define-constant +static-text-delim+ "§" :test #'string=)

(defparameter *cancel-lbl* (_ "Cancel"))

(defparameter *apply-lbl*  (_ "Apply"))

(defparameter *ok-lbl*     (_ "Ok"))

(defparameter *no-lbl*     (_ "No"))

(defparameter *yes-lbl*    (_ "Yes"))

(defparameter *close-lbl*  (_ "Close"))

(defparameter *open-lbl*   (_ "Open"))

(defparameter *load-lbl*   (_ "Load"))

(defparameter *save-lbl*   (_ "Save"))

(defun join-lines-for-static-text (lines)
  (join-with-srings lines +static-text-delim+))

(defun join-lines-for-static-text* (&rest lines)
  (join-with-srings lines +static-text-delim+))

(defun get-font (handle)
  (gethash handle *fonts-db*))

(defun get-char-mesh (handle char)
  (gethash char (gethash handle *fonts-db*)))

;; fonts picking
(defun char->texture (char &key (min-code-value 32) (max-code-value 126) (min-s 0.0) (max-s 1.0))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (fixnum min-code-value max-code-value))
  (let* ((code       (d  (alexandria:clamp (char-code char) min-code-value max-code-value)))
	 (delta-code (d  (f- (1+ max-code-value) min-code-value)))
	 (incs       (d/ (d- max-s min-s) delta-code))
	 (s          (d* incs (d- (d code) (d min-code-value)))))
    (declare (desired-type code s))
    ;;  - c +-------+ d
    ;;  |   |\      |     c has coordinates (s, 1.0)
    ;;  |   | \ T2  |
    ;;h |   |  \    |
    ;;  |   |   \   |
    ;;  |   |    \  |      ^ t
    ;;  |   | T1  \ |      |
    ;;  |   |      \|      |    s
    ;;  -   +-------+      +---->
    ;;      a        b
    (values (vec2 s           0.0) (vec2 (d+ s incs) 0.0)
	    (vec2 (d+ s incs) 1.0) (vec2 s           1.0))))

(definline gui-printable-p (char &key
			       (min-code-value 32)
			       (max-code-value 126))
  (<= min-code-value (char->code char) max-code-value))

(defun char->quad-texture (char &key
				  (min-code-value 32)
				  (max-code-value 126)
				  (min-s 0.0)
				  (max-s 1.0))
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  ;;  - c +-------+ d
  ;;  |   |\      |     c has coordinates (sc, tc)
  ;;  |   | \ T2  |
  ;;h |   |  \    |
  ;;  |   |   \   |
  ;;  |   |    \  |      ^ y
  ;;  |   | T1  \ |      |
  ;;  |   |      \|      |    x
  ;;  -   +-------+      +---->
  ;;      a        b
  (multiple-value-bind (a b d c)
      (char->texture char
		     :min-code-value min-code-value
		     :max-code-value max-code-value
		     :min-s min-s
		     :max-s max-s)
    (let* ((res  (make-array-frame 6 (vec2:vec2 0.0 0.0) 'vec2:vec2 t)))
      (declare ((simple-array vec2:vec2 (6)) res))
      (setf (elt res 0) a
	    (elt res 1) b
	    (elt res 2) c
	    (elt res 3) b
	    (elt res 4) d
	    (elt res 5) c)
      res)))

(defun load-font (font-file handle shaders-dictionary)
  (let ((font-texture (get-texture (res:get-resource-file (strcat font-file
								  +font-file-extension+)
							  constants:+fonts-resource+
							  :if-does-not-exists :error)))
	(map          (make-hash-table :test 'eql)))
    (setf (texture:interpolation-type font-texture) :nearest)
    (prepare-for-rendering font-texture)
    (loop for i from 0 below +ascii-size+ do
	 (let ((texture-coords (char->quad-texture (code-char i)))
	       (mesh  (make-instance 'triangle-mesh)))
	   (setf (interfaces:compiled-shaders mesh) shaders-dictionary)
	   (setf (texture-object mesh) font-texture)
	   (quad-w-explicit-texture-coords mesh
					   1.0 1.0
					   texture-coords
					   +zero-vec+ nil t)
	   (remove-orphaned-vertices mesh)
	   (prepare-for-rendering mesh)
	   (setf (gethash (code-char i) map) mesh)))
    (setf (gethash handle *fonts-db*) map)))

(defun clean-font-db ()
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (maphash #'(lambda (k2 v2)
			    (declare (ignore k2))
			    (interfaces:destroy v2))
			v))
	   *fonts-db*)
  (maphash #'(lambda (k v) (declare (ignore k)) (setf v nil)) *fonts-db*)
  (setf *fonts-db* nil)
  (setf *fonts-db* (make-hash-table :test 'eql)))

(defmacro gen-simple-bg-setup (function-name texture-handle parameter file
			       resource-name
			       &rest texture-setup)
  (let ((fn-name   (alexandria:format-symbol t "~:@(setup-bg-~a~)" function-name)))
    `(progn
       (defparameter ,parameter (res:get-resource-file ,file
						       ,resource-name
						       :if-does-not-exists :error))
       (alexandria:define-constant ,texture-handle
	   ,(string-trim '(#\+) (symbol-name texture-handle))
	 :test #'string=)
       (defun ,fn-name ()
	 (multiple-value-bind (bg error)
	     (texture:get-texture ,parameter)
	   (if (not error)
	       (progn
		 ,@texture-setup
		 (texture:prepare-for-rendering bg)
		 (setf (texture:filename bg) ,texture-handle)
		 bg)
	       (values nil error)))))))

(defmacro gen-texture-bulk (&rest file)
  `(progn
     ,@(loop for f in file collect
	    (let* ((file-name (if (consp f)
				  (symbol-name (first f))
				  (symbol-name f)))
		   (res-name  (if (consp f)
				  (second f)
				  '+default-gui-resource+))
		   (base      (basename file-name)))
	      `(gen-simple-bg-setup ,(alexandria:symbolicate base)
				    ,(alexandria:symbolicate (wrap-with (strcat base
										"-TEXTURE-NAME")
									"+"))
				    ,(alexandria:symbolicate (wrap-with (strcat "BG-"
										base)
									"*"))
				    ,(string-downcase file-name)
				    ,res-name
				    ,@(if (consp f)
					  (subseq f 2)
					  nil))))))

(gen-texture-bulk (portrait-unknown.tga
		   +default-gui-resource+
		   (setf (interpolation-type bg) :linear))
		  (berserk.tga
		   +default-gui-resource+
		   (setf (interpolation-type bg) :linear))
		  (coma.tga
		   +default-gui-resource+
		   (setf (interpolation-type bg) :linear))
		  (terror.tga
		   +default-gui-resource+
		   (setf (interpolation-type bg) :linear))
		  (poison.tga
		   +default-gui-resource+
		   (setf (interpolation-type bg) :linear))
		   window-top-bar.tga
		   window.tga
		   button.tga
		   button-pressed.tga
		   text-field.tga
		   text-field-focused.tga
		   text-field-overlay.tga
		   check-button.tga
		   square-button.tga
		   square-button-pressed.tga
		   inventory-slot.tga
		   inventory-slot-selected.tga
		   transparent.tga
		   (plus-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (minus-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (up-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (down-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (load-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (save-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (option-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (quit-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (next-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (previous-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (next-turn-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (attack-short-range-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (attack-long-range-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (attack-long-range-imprecise-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (conversation-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (magic-staff-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (open-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (close-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (zoom-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (unzoom-overlay.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (chest-opened.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000))
		   (chest-closed.tga
		    +default-gui-resource+
		    (setf (s-wrap-mode  bg) :clamp-to-border)
		    (setf (t-wrap-mode  bg) :clamp-to-border)
		    (setf (border-color bg) §c00000000)))

(gen-simple-bg-setup frame
		     +frame-texture-name+
		     *bg-frame*
		     "basic-frame.tga"
		     +default-gui-resource+)

(gen-simple-bg-setup button-cancel
		     +button-cancel-texture-name+
		     *bg-button-cancel*
		     "cancel-overlay.tga"
		     +default-gui-resource+
		     (setf (s-wrap-mode  bg) :clamp-to-border)
		     (setf (t-wrap-mode  bg) :clamp-to-border)
		     (setf (border-color bg) §c00000000))

(gen-simple-bg-setup button-ok
		     +button-ok-texture-name+
		     *bg-button-ok*
		     "ok-overlay.tga"
		     +default-gui-resource+
		     (setf (s-wrap-mode  bg) :clamp-to-border)
		     (setf (t-wrap-mode  bg) :clamp-to-border)
		     (setf (border-color bg) §c00000000))

(gen-simple-bg-setup window-close-button
		     +window-close-button-texture-name+
		     *bg-window-close-button*
		     "window-button.tga"
		     +default-gui-resource+)

(gen-simple-bg-setup window-close-button-pressed
		     +window-close-button-pressed-texture-name+
		     *bg-window-close-button-pressed*
		     "window-button-pressed.tga"
		     +default-gui-resource+)

(gen-simple-bg-setup blue-h-bar
		     +blue-h-bar+
		     *bg-blue-h-bar*
		     "blue-bar.tga"
		     +default-gui-resource+)

(gen-simple-bg-setup red-h-bar
		     +red-h-bar+
		     *bg-red-h-bar*
		     "red-bar.tga"
		     +default-gui-resource+)

(gen-simple-bg-setup green-h-bar
		     +green-h-bar+
		     *bg-green-h-bar*
		     "green-bar.tga"
		     +default-gui-resource+)

(gen-simple-bg-setup check-button-checked-green
		     +check-button-checked-green+
		     *bg-check-button-checked-green*
		     "check-button-checked-green.tga"
		     +default-gui-resource+)

(gen-simple-bg-setup check-button-overlay
		     +check-button-overlay+
		     *bg-check-button-overlay*
		     "check-button-overlay.tga"
		     +default-gui-resource+)

(defun setup-gui (compiled-shaders)
  (load-font +default-font+ +default-font-handle+ compiled-shaders)
  (setup-bg-frame)
  (setup-bg-window-top-bar)
  (setup-bg-window-close-button)
  (setup-bg-window-close-button-pressed)
  (setup-bg-window)
  (setup-bg-button-cancel)
  (setup-bg-button-ok)
  (setup-bg-button)
  (setup-bg-button-pressed)
  (setup-bg-square-button)
  (setup-bg-square-button-pressed)
  (setup-bg-inventory-slot)
  (setup-bg-inventory-slot-selected)
  (setup-bg-transparent)
  (setup-bg-blue-h-bar)
  (setup-bg-red-h-bar)
  (setup-bg-green-h-bar)
  (setup-bg-check-button-overlay)
  (setup-bg-check-button-checked-green)
  (setup-bg-check-button)
  (setup-bg-text-field)
  (setup-bg-text-field-focused)
  (setup-bg-text-field-overlay)
  (setup-bg-save-overlay)
  (setup-bg-plus-overlay)
  (setup-bg-minus-overlay)
  (setup-bg-up-overlay)
  (setup-bg-down-overlay)
  (setup-bg-load-overlay)
  (setup-bg-quit-overlay)
  (setup-bg-next-overlay)
  (setup-bg-previous-overlay)
  (setup-bg-next-turn-overlay)
  (setup-bg-option-overlay)
  (setup-bg-berserk)
  (setup-bg-coma)
  (setup-bg-portrait-unknown)
  (setup-bg-terror)
  (setup-bg-poison)
  (setup-bg-conversation-overlay)
  (setup-bg-attack-short-range-overlay)
  (setup-bg-attack-long-range-overlay)
  (setup-bg-attack-long-range-imprecise-overlay)
  (setup-bg-magic-staff-overlay)
  (setup-bg-open-overlay)
  (setup-bg-close-overlay)
  (setup-bg-zoom-overlay)
  (setup-bg-unzoom-overlay)
  (setup-bg-chest-closed)
  (setup-bg-chest-opened))
