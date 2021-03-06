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

(in-package :random-helm)

(define-constant +file-record-sep+ "-"  :test #'string=)

(define-constant +minimum-level+                  1          :test #'=)

(define-constant +maximumlevel+                  10          :test #'=)

(define-constant +level-sigma+    #(1 1.2 1.8 1.9 2.0 2.2 2.3 2.5 2.7 3.0)
  :test #'equalp)

(define-constant +level-mean+     #(1.2 1.5 1.8 2.1 2.4 2.9 3.1 3.8 4.8 6.0)
  :test #'equalp)

(define-constant +decay-sigma+    #(56.0 48.0 40.0 36.0 36.0 24.0 22.0 20.0 30.0 35.0)
  :test #'equalp)

(define-constant +decay-mean+     #(30.0 28.0 26.0 24.0 12.0 10.0 9.0 8.0 4.0 0.0)
  :test #'equalp)

(define-constant +modifier-sigma+ #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(define-constant +modifier-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +duration-healing-fx-sigma+  #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(define-constant +duration-healing-fx-mean+  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +chance-healing-fx-sigma+  #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(define-constant +chance-healing-fx-mean+   #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(define-constant +minimum-duration-healing-fx+           2.0        :test #'=)

(define-constant +minimum-chance-healing-fx+             0.05       :test #'=)

(define-constant +minimum-decay+                        20.0        :test #'=)

(define-constant +minimum-level+                         1          :test #'=)

(define-constant +maximum-level+                         9          :test #'=)

(define-constant +minimum-chance-effects+                2.0        :test #'=)

(define-constant +maximum-chance-effects+                4.0        :test #'=)

(define-constant +minimum-num-healing-effects+           2.0        :test #'=)

(define-constant +maximum-num-healing-effects+           4.0        :test #'=)

(define-constant +type-name+                             "helm"     :test #'string=)

(define-constant +healing-target-self-chance+            3          :test #'=)

(define-constant +minimum-damage-point+                   1.0       :test #'=)

(define-constant +maximum-damage-point+                 100.0       :test #'=)

(defun randomize-damage-points (character level)
  (setf (damage-points character)
        (calculate-randomized-damage-points level
                                            +minimum-level+
                                            +maximum-level+
                                            +minimum-damage-point+
                                            +maximum-damage-point+
                                             (d/ (d level) (d* 5.0 (d +maximum-level+))))))

(defun decay-params (helm-level)
  (values (elt +decay-sigma+ helm-level)
          (elt +decay-mean+  helm-level)))

(defun calculate-decay-points (helm-level)
  (multiple-value-bind (sigma mean)
      (decay-params (1- helm-level))
    (truncate (max +minimum-decay+ (gaussian-probability sigma mean)))))


(defun calculate-decay (object-level decay-points)
  (make-instance 'decay-parameters
                 :leaving-message (format nil
                                          (_ " (object level ~a).") object-level)
                 :points decay-points
                 :when-decay +decay-by-turns+))

(defun level-params (map-level)
  (values (elt +level-sigma+ map-level)
          (elt +level-mean+  map-level)))

(defun modifier-params (helm-level)
  (values (elt +modifier-sigma+ helm-level)
          (elt +modifier-mean+  helm-level)))

(defun calculate-modifier (helm-level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- helm-level))
    (gaussian-probability sigma mean)))

(defun calculate-level (map-level)
  (multiple-value-bind (sigma mean)
      (level-params (1- map-level))
    (clamp (truncate (gaussian-probability sigma mean))
           +minimum-level+ +maximum-level+)))

(defun number-of-effects (helm-level)
  (let ((max (round (num:dlerp (num:smoothstep-interpolate 0.0
                                                           10.0
                                                           (d (1- helm-level)))
                               +minimum-chance-effects+
                               +maximum-chance-effects+))))
    (lcg-next-upto max)))

(defun number-of-healing-effects (helm-level number-of-normal-effects)
  (let ((max (round (- (num:dlerp (num:smoothstep-interpolate 0.0
                                                           10.0
                                                           (d (1- helm-level)))
                                  +minimum-num-healing-effects+
                                  +maximum-num-healing-effects+)
                       number-of-normal-effects))))
    (if (<= max 0)
        0
        (lcg-next-upto (max 0.0 max)))))

(defun generate-helm (map-level)
  (clean-effects
   (%generate-helm (res:get-resource-file +default-interaction-filename+
                                          +default-character-helm-dir+
                                          :if-does-not-exists :error)
                   (res:get-resource-file +default-character-filename+
                                          +default-character-helm-dir+
                                          :if-does-not-exists :error)
                   map-level)))

(defun %generate-helm (interaction-file character-file map-level)
  (validate-interaction-file interaction-file)
  (with-character-parameters (char-template character-file)
    (with-interaction-parameters-file (template interaction-file)
      (let* ((helm-level         (calculate-level map-level))
             (helm-decay         (calculate-decay-points helm-level))
             (effects-no         (number-of-effects helm-level))
             (healing-effects-no (number-of-healing-effects helm-level effects-no))
             (effects            (get-normal-fx-shuffled  template effects-no))
             (healing-effects    (get-healing-fx-shuffled template healing-effects-no)))
        (n-setf-path-value char-template (list +level+) (d helm-level))
        (n-setf-path-value char-template (list +description+) +type-name+)
        (n-setf-path-value template
                           (list +decay+)
                           (calculate-decay helm-level helm-decay))
        (loop for i in effects do
             (set-effect (list +effects+ i) helm-level template))
        (loop for i in healing-effects do
             (cond
               ((eq i +heal-damage-points+)
                nil)
               ((eq i +cause-poison+)
                (set-poison-effect (list +healing-effects+ i) helm-level template))
               (t
                (set-healing-effect (list +healing-effects+ i) helm-level template))))
        (n-setf-path-value template (list +magic-effects+) nil) ;; no magic effects
        (setf template (remove-generate-symbols template))
        (fill-character-plist char-template template helm-level)
        (let ((helm-character (params->np-character char-template)))
          (setf (basic-interaction-params helm-character) template)
          (randomize-damage-points helm-character helm-level)
          helm-character)))))

(defun set-effect (effect-path helm-level interaction)
  (let ((effect-object (make-instance 'effect-parameters
                                      :modifier (calculate-modifier helm-level)
                                      :trigger  +effect-when-worn+
                                       ;; effect lasting forever  for
                                       ;; helms,  they   will  broke
                                       ;; anyway.
                                      :duration +duration-unlimited+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun healing-fx-params-duration (helm-level)
  (values (elt +duration-healing-fx-sigma+ helm-level)
          (elt +duration-healing-fx-mean+  helm-level)))

(defun calculate-healing-fx-params-duration (helm-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-duration (1- helm-level))
    (truncate (max +minimum-duration-healing-fx+ (gaussian-probability sigma mean)))))

(defun healing-fx-params-chance (helm-level)
  (values (elt +chance-healing-fx-sigma+ helm-level)
          (elt +chance-healing-fx-mean+  helm-level)))

(defun calculate-healing-fx-params-chance (helm-level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-chance (1- helm-level))
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun healing-target ()
  (if (= (lcg-next-upto +healing-target-self-chance+) 0)
      +target-other+
      +target-self+))

(defun set-healing-effect (effect-path helm-level interaction)
  (let ((effect-object (make-instance 'healing-effect-parameters
                                      :trigger  +effect-when-worn+
                                       ;; effect lasting forever  for
                                       ;; helms,  they   will  broke
                                       ;; anyway.
                                      :duration +duration-unlimited+
                                      :chance (calculate-healing-fx-params-chance helm-level)
                                      :target +target-self+)))
    (n-setf-path-value interaction effect-path effect-object)))

(defun set-poison-effect (effect-path helm-level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
                                      :points-per-turn (calculate-modifier helm-level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun filename-effects-string (interaction)
  (cond
    ((plist-path-value interaction (list +healing-effects+ +immune-poison+))
     (strcat "immune" +file-record-sep+ "poison"))
    ((plist-path-value interaction (list +healing-effects+ +immune-berserk+))
     (strcat "immune" +file-record-sep+ "berserk"))
    ((plist-path-value interaction (list +healing-effects+ +immune-faint+))
     (strcat "immune" +file-record-sep+ "faint"))
    (t
     "normal")))

(defun regexp-file-portrait (interaction helm-name-type helm-level)
  (strcat helm-name-type
          +file-record-sep+
          (filename-effects-string interaction)
          +file-record-sep+
          (format nil "~2,'0d" helm-level)))

(defun build-file-names-db (helm-name-type helm-level)
  (strcat helm-name-type
          +file-record-sep+
          (format nil "~2,'0d" helm-level)
          ".lisp"))

(defun fill-character-plist (character interaction helm-level)
  (let* ((regex          (regexp-file-portrait interaction +type-name+ helm-level))
         (names-filename (build-file-names-db +type-name+ helm-level))
         (portrait-file  (find-object-portrait-filename regex)))
    (n-setf-path-value character (list +portrait+) (uiop:native-namestring portrait-file))
    (random-names:load-db* +helms-names-resource+ names-filename)
    (n-setf-path-value character (list +last-name+)  "")
    (n-setf-path-value character (list +first-name+) (random-names:generate))))
