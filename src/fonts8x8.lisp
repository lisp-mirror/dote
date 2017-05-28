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

(in-package :random-labyrinth)

(alexandria:define-constant +font-w+ 8 :test #'=)

(alexandria:define-constant +font-h+ 8 :test #'=)

(defmacro generate-fonts (width height &rest fonts)
  `(progn
     ,@(loop for i in fonts collect
            `(defparameter ,(alexandria:format-symbol t "~:@(~a~)" (first i))
               (make-instance 'matrix:matrix
                              :data ,(second i)
                              :width ,width
                              :height ,height)))))

(generate-fonts +font-w+ +font-h+
                (*zero*  #(0 1 1 1 1 1 1 0
                           1 1 0 0 0 0 1 1
                           1 1 0 0 0 1 1 1
                           1 1 0 0 1 0 1 1
                           1 1 0 1 0 0 1 1
                           1 1 1 0 0 0 1 1
                           1 1 0 0 0 0 1 1
                           0 1 1 1 1 1 1 0))
                (*one*   #(0 0 0 1 1 0 0 0
                           0 0 1 1 1 0 0 0
                           0 1 0 1 1 0 0 0
                           0 0 0 1 1 0 0 0
                           0 0 0 1 1 0 0 0
                           0 0 0 1 1 0 0 0
                           0 0 0 1 1 0 0 0
                           0 1 1 1 1 1 1 0))
                (*two*   #(0 0 1 1 1 1 0 0
                           0 0 0 0 0 1 1 0
                           0 0 0 0 0 1 1 0
                           0 0 0 0 0 1 1 0
                           0 0 1 1 1 1 0 0
                           0 1 1 0 0 0 0 0
                           0 1 1 0 0 0 0 0
                           0 0 1 1 1 1 0 0))
                (*three* #(0 0 1 1 1 1 0 0
                           0 0 0 0 0 1 1 0
                           0 0 0 0 0 1 1 0
                           0 0 0 0 0 1 1 0
                           0 0 1 1 1 1 0 0
                           0 0 0 0 0 1 1 0
                           0 0 0 0 0 1 1 0
                           0 0 1 1 1 1 0 0))
                (*four*  #(0 0 0 1 1 0 0 0
                           0 0 1 1 1 0 0 0
                           0 1 1 0 1 0 0 0
                           0 1 0 0 1 0 0 0
                           1 1 0 0 1 0 0 0
                           1 1 1 1 1 1 1 1
                           0 0 0 0 1 0 0 0
                           0 0 0 0 1 0 0 0))
                (*five*  #(0 0 1 1 1 1 0 0
                           0 1 1 0 0 0 0 0
                           0 1 1 0 0 0 0 0
                           0 1 1 0 0 0 0 0
                           0 0 1 1 1 1 0 0
                           0 0 0 0 0 1 1 0
                           0 0 0 0 0 1 1 0
                           0 0 1 1 1 1 0 0))
                (*six*   #(0 0 1 1 1 1 0 0
                           0 1 1 0 0 0 0 0
                           0 1 1 0 0 0 0 0
                           0 1 1 0 0 0 0 0
                           0 0 1 1 1 1 0 0
                           0 1 1 0 0 1 1 0
                           0 1 1 0 0 1 1 0
                           0 0 1 1 1 1 0 0))
                (*seven* #(0 1 1 1 1 1 1 0
                           0 0 0 0 0 0 1 1
                           0 0 0 0 0 1 1 0
                           0 0 0 0 1 1 0 0
                           0 0 0 1 1 0 0 0
                           0 0 1 1 0 0 0 0
                           0 1 1 0 0 0 0 0
                           0 1 0 0 0 0 0 0))
                (*eight* #(0 0 1 1 1 1 0 0
                           0 1 1 0 0 1 1 0
                           0 1 1 0 0 1 1 0
                           0 1 1 0 0 1 1 0
                           0 0 1 1 1 1 0 0
                           0 1 1 0 0 1 1 0
                           0 1 1 0 0 1 1 0
                           0 0 1 1 1 1 0 0))
                (*nine* #(0 0 1 1 1 1 0 0
                          0 1 1 0 0 1 1 0
                          0 1 1 0 0 1 1 0
                          0 1 1 0 0 1 1 0
                          0 0 1 1 1 1 0 0
                          0 0 0 0 0 1 1 0
                          0 0 0 0 0 1 1 0
                          0 0 1 1 1 1 0 0)))

(defun find-font (char)
  (cond
    ((char= char #\0) *zero*)
    ((char= char #\1) *one*)
    ((char= char #\2) *two*)
    ((char= char #\3) *three*)
    ((char= char #\4) *four*)
    ((char= char #\5) *five*)
    ((char= char #\6) *six*)
    ((char= char #\7) *seven*)
    ((char= char #\8) *eight*)
    ((char= char #\9) *nine*)))

(defun draw-font (mat x y color font)
  (loop for col from 0 below (matrix:height font) do
       (loop for row from 0 below (matrix:width font) do
            (when (= 1 (matrix:matrix-elt font row col))
              (matrix:with-check-matrix-borders (mat (+ x col) (+ y row) )
                (setf (matrix:matrix-elt mat (+ y row) (+ x col)) color))))))

(defun draw-string (mat x y color string)
  (let ((xstart x)
        (ystart y))
    (map 'list #'(lambda (ch)
                   (draw-font mat xstart ystart color (find-font ch))
                   (incf xstart (matrix:width (find-font ch))))
         string)))
