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

(in-package :widget)

(defclass spell-button (inventory-slot-button) ())

(defun make-disabled-texture (texture)
  (gen-name-and-inject-in-database (pixmap:to-grayscale (get-texture texture))))

(defun make-spell-button (x y &key (callback nil))
  (make-instance 'spell-button
		 :theme            nil
		 :x                x
		 :y                y
		 :width            (small-square-button-size *reference-sizes*)
		 :height           (small-square-button-size *reference-sizes*)
		 :texture-object   (get-texture +inventory-slot-texture-name+)
		 :texture-pressed  (get-texture +inventory-slot-selected-texture-name+)
		 :texture-overlay  (get-texture +transparent-texture-name+)
		 :contained-entity nil
		 :callback         callback))

(defun select-spell-cb (w e)
  (with-parent-widget (win) w
    (with-accessors ((owner owner)) win
      (multiple-value-bind (slot item)
	  (get-selected-item win)
	(if (and slot item)
	    (progn
	      (inventory-update-description-cb w e)
	      (setf (character:spell-loaded (ghost owner)) item))
	    (setf (character:spell-loaded (ghost owner)) nil))))))

(defclass spell-window (table-paginated-window) ())

(defun spell-window-width ()
  (d+ (d* (d +slots-per-page-side-size+) (small-square-button-size *reference-sizes*))
      (d* 2.0
	  (d* (d* 4.0 (small-square-button-size *reference-sizes*))
	      (left-frame-offset *reference-sizes*)))))

(defun spell-window-height ()
    (d+ (d* 10.0 (small-square-button-size *reference-sizes*))))

(defun make-spell-window (player)
  (let ((window (make-instance 'spell-window
			       :owner  player
			       :x      0.0
			       :y      200.0
			       :width  (spell-window-width)
			       :height (spell-window-height)
			       :label  (_ "Spells")
			       :make-slot-button-fn #'make-spell-button
			       :callback-slot-button #'select-spell-cb)))
    (setf (width (text-description window))
	  (d* 0.9 (spell-window-width)))
    (loop
       for button in (alexandria:flatten (slots-pages window))
       for spell  in (spell:db) do
	 (setf (texture-overlay  button) (spell:gui-texture spell)
	       (contained-entity button) spell))
    window))