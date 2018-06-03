;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

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

(in-package :saved-game)

(define-constant +number-opponents-sigma+  #(1.0 1.0 1.1 1.1 2.0 2.2 2.3 2.5 2.7 2.7)
  :test #'equalp)

(define-constant +opponents-type+ '(:wizard
                                    :warrior
                                    :warrior
                                    :archer
                                    :warrior
                                    :archer
                                    :healer
                                    :ranger)
  :test #'equalp)

(defun no-of-opponents (difficult-level)
  (let* ((average (truncate (lerp (smoothstep-interpolate (d +difficult-minimum+)
                                                          (d +maximum-level-difficult+)
                                                          (d difficult-level))
                                  4.0
                                  +maximum-level-difficult+)))
         (number (gaussian-probability (elt +number-opponents-sigma+ (1- difficult-level))
                                       average)))
    (max 3 (ceiling number))))

(defun type-of-opponents (no-of-opponents)
  (let ((excess   (- no-of-opponents (length +opponents-type+)))
        (template (safe-subseq +opponents-type+ 0 no-of-opponents)))
    (append template
            (loop repeat excess collect (random-elt +opponents-type+)))))

(defun place-opponents (world difficult)
  (let ((all-opponents (type-of-opponents (no-of-opponents difficult))))
    (loop for i in all-opponents do
         ;; TODO change with (random-elt '(:male :female)
         (world:add-ai-opponent world :warrior :male))))
