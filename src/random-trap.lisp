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

(in-package :random-trap)

(define-constant +file-record-sep+                       "-"   :test #'string=)

(define-constant +type-name+                             "trap" :test #'string=)

(define-constant +minimum-level+                         1     :test #'=)

(define-constant +maximum-level+                         9     :test #'=)

(define-constant +minimum-modifier+                      1.0   :test #'=)

(define-constant +minimum-duration-healing-fx+           2.0   :test #'=)

(define-constant +minimum-chance-healing-fx+             0.05  :test #'=)

(define-constant +level-sigma+         #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +level-mean+          #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.8 4.8 6.0)
  :test #'equalp)

(define-constant +modifier-sigma+      #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(define-constant +modifier-mean+       #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +duration-healing-fx-sigma+  #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(define-constant +duration-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+ #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +minimum-num-healing-effects+            2.0          :test #'=)

(define-constant +maximum-num-healing-effects+            4.0          :test #'=)

(define-constant +minimum-damage-point+                   1.0          :test #'=)

(define-constant +maximum-damage-point+                  20.0          :test #'=)

(defun randomize-damage-points (character level)
  (setf (damage-points character)
        (calculate-randomized-damage-points level
                                             +minimum-level+
                                             +maximum-level+
                                             +minimum-damage-point+
                                             +maximum-damage-point+
                                             (d/ (d level) (d* 5.0 (d +maximum-level+))))))

(defun level-params (map-level)
  (values (elt +level-sigma+ map-level)
          (elt +level-mean+  map-level)))

(defun calculate-level (map-level)
  (multiple-value-bind (sigma mean)
      (level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
           +minimum-level+ +maximum-level+)))

(defun modifier-params (trap-level)
  (values (elt +modifier-sigma+ trap-level)
          (elt +modifier-mean+  trap-level)))

(defun calculate-modifier (trap-level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- trap-level))
    (d- (gaussian-probability sigma mean)
        (gaussian-probability (d/ sigma 4.0) (- trap-level)))))

(defun healing-fx-params-duration (trap-level)
  (values (elt +duration-healing-fx-sigma+ trap-level)
          (elt +duration-healing-fx-mean+  trap-level)))

(defun calculate-healing-fx-params-duration (trap-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-duration (1- trap-level))
    (truncate (max +minimum-duration-healing-fx+ (gaussian-probability sigma mean)))))

(defun healing-fx-params-chance (armor-level)
  (values (elt +chance-healing-fx-sigma+ armor-level)
          (elt +chance-healing-fx-mean+  armor-level)))

(defun calculate-healing-fx-params-chance (armor-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-chance (1- armor-level))
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun set-poison-effect (effect-path trap-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
                                      :chance (calculate-healing-fx-params-chance trap-level)
                                      :target          +target-other+
                                      :points-per-turn (calculate-modifier trap-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun set-healing-hp-effect (path trap-level interaction)
  (let ((effect-object (make-instance 'heal-damage-points-effect-parameters
                                      :trigger +effect-when-used+
                                      :points  (calculate-modifier trap-level)
                                      :chance  (calculate-healing-fx-params-chance trap-level)
                                      :target  +target-other+)))
    (n-setf-path-value interaction path effect-object)))

(defun set-healing-effect (effect-path trap-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
                                      :trigger  +effect-when-used+
                                      :duration (ceiling (healing-fx-params-duration trap-level))
                                      :chance   (calculate-healing-fx-params-chance trap-level)
                                      :target   +target-other+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun number-of-healing-effects (trap-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
                                                           10.0
                                                           (d (1- trap-level)))
                                  +minimum-num-healing-effects+
                                  +maximum-num-healing-effects+)
                       number-of-normal-effects))))
    (if (<= max 0)
        0
        (lcg-next-upto (max 0.0 max)))))

(defun set-magic-effect (trap-level interaction)
  (let* ((spell-level   trap-level)
         (spells        (spell:filter-spell-db #'(lambda (a)
                                                   (or (not (spell:attack-spell-p a))
                                                        (> (spell:level a)
                                                            spell-level)
                                                        (< (spell:level a)
                                                           (max 0
                                                                (/ spell-level 4)))))))
         (spell-id      (spell:identifier (random-elt spells)))
         (effect-object (make-instance 'magic-effect-parameters
                                       :spell-id spell-id
                                       :trigger  +effect-when-used+)))
    (n-setf-path-value interaction (list +magic-effects+) effect-object)))

(defun generate-trap (map-level)
  (clean-effects
   (generate-trap* (res:get-resource-file +default-interaction-filename+
                                          +default-character-trap-dir+
                                          :if-does-not-exists :error)
                   (res:get-resource-file +default-character-filename+
                                          +default-character-trap-dir+
                                          :if-does-not-exists :error)
                  map-level)))

(defun generate-trap* (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters-file (template interaction-file)
      (let* ((trap-level          (calculate-level map-level))
             (healing-effects-no (number-of-healing-effects trap-level 0))
             (healing-effects    (get-healing-fx-shuffled template healing-effects-no)))
        (n-setf-path-value char-template (list +level+) (d trap-level))
        (loop for i in healing-effects do
             (cond
               ((eq i +heal-damage-points+)
                (set-healing-hp-effect (list +healing-effects+ i) trap-level template))
               ((eq i +cause-poison+)
                (set-poison-effect (list +healing-effects+ i) trap-level template))
               (t
                (set-healing-effect (list +healing-effects+ i) trap-level template))))
        (when (plist-path-value template (list +magic-effects+))
          (set-magic-effect trap-level template))
        (setf template (remove-generate-symbols template))
        (fill-character-plist char-template)
        (let ((trap-character (params->player-character char-template)))
          (setf (basic-interaction-params trap-character) template)
          (setf (character:ambush-attack-chance trap-character) 1.0)
          (setf (character:attack-spell-chance trap-character)  1.0)
          (randomize-damage-points trap-character trap-level)
          trap-character)))))

(defun regexp-file-portrait (level)
  (format nil "~a~a~a" +type-name+ +file-record-sep+ (floor level)))

(defun fill-character-plist (character)
  (let* ((level          (plist-path-value character (list +level+)))
         (regex          (regexp-file-portrait level))
         (portrait-file  (find-object-portrait-filename regex)))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (n-setf-path-value character (list +last-name+) "")
    (n-setf-path-value character (list +first-name+) "")))
