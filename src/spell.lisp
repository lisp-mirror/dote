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

(in-package :spell)

(alexandria:define-constant +maximum-level+             9
  :test #'=)

(alexandria:define-constant +chance-healing-fx-sigma+   #(.08 .1 .12 .18 .22 .23 .24 .25 .26 .28)
  :test #'equalp)

(alexandria:define-constant +chance-healing-fx-mean+    #(0.0 0.0 0.0 0.0 0.0 0.0 0.1 0.2 0.3 0.4)
  :test #'equalp)

(alexandria:define-constant +minimum-chance-healing-fx+ 0.05
  :test #'=)

(alexandria:define-constant +modifier-sigma+            #(1.0 2.0 3.0 4.0 5.0 6.0 6.5 7.0 7.5 8.0)
  :test #'equalp)

(alexandria:define-constant +modifier-mean+             #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(defparameter *spells-db* '())

(defun clean-db ()
  (setf *spells-db* nil))

(defun get-spell (key)
  (find-if #'(lambda (a) (eq (identifier a) key))
	   *spells-db*))

(defun remove-spell (key)
  (setf *spells-db*
	(delete-if #'(lambda (a) (eq (identifier a) key))
		   *spells-db*)))

(defun add-spell (spell)
  (remove-spell (identifier spell))
  (push spell *spells-db*))

(defun load-db ()
  (let ((all-attack-spells (resources-utils:get-resource-files +attack-spell-dir+))
	(all-other-spells  (resources-utils:get-resource-files +spell-dir+)))
    (dolist (spell-file (nconc all-attack-spells all-other-spells))
      (load spell-file))))

(defun db ()
  *spells-db*)

(defclass spell (identificable interactive-entity)
  ((identifier
    :initform nil
    :initarg :identifier
    :accessor identifier)
   (target
    :initform +target-self+
    :initarg  :target
    :accessor target)
   (gui-texture
    :initform nil
    :initarg  :gui-texture
    :accessor gui-texture)
   (cost
    :initform 0.0
    :initarg  :cost
    :accessor cost)
   (visual-effect-self
    :initform nil
    :initarg  :visual-effect-self
    :accessor visual-effect-self)
   (range
    :initform 0.0
    :initarg :range
    :accessor range)
   (effective-range
    :initform 0.0
    :initarg :effective-range
    :accessor effective-range)
   (visual-effect-target
    :initform nil
    :initarg  :visual-effect-target
    :accessor visual-effect-target)))

(defmethod description-for-humans :around ((object spell))
  (text-utils:strcat
   (format nil
	   (_ "~a~arange: ~a effective range: ~a cost: ~a~a")
	   (cl-ppcre:regex-replace-all "-" (string (identifier object)) " ")
	   +gui-static-text-delim+
	   (range           object)
	   (effective-range object)
	   (cost            object)
	   +gui-static-text-delim+)
   (call-next-method)))

(defclass attack-spell (spell)
  ((element
    :initform :fire
    :initarg :element
    :accessor element
    :type (or :fire :ice :electricity))
   (damage-inflicted
    :initform 0.0
    :initarg :damage-inflicted
    :accessor damage-inflicted)
   (arrow
    :initform nil
    :initarg :arrow
    :accessor arrow)))

(defun attack-spell-p (a)
  (typep a 'attack-spell))

(defmethod description-for-humans :around ((object attack-spell))
  (text-utils:strcat
   (call-next-method)
   (format nil
	   (_ "element: ~a~adamage: ~a~a")
	   (element          object)
	   +gui-static-text-delim+
	   (damage-inflicted object)
	   +gui-static-text-delim+)))

(defun make-gui-texture (f)
  (let ((texture (texture:get-texture (res:get-resource-file f
							     +spell-texture-dir+))))
    (setf (texture:border-color       texture) §c00000000)
    (setf (texture:s-wrap-mode        texture) :clamp-to-border)
    (setf (texture:t-wrap-mode        texture) :clamp-to-border)
    (setf (texture:interpolation-type texture) :linear)
    (prepare-for-rendering texture)
    texture))

(defun healing-fx-params-chance (level)
  (values (elt +chance-healing-fx-sigma+ level)
	  (elt +chance-healing-fx-mean+  level)))

(defun calculate-healing-fx-params-chance (level)
  (multiple-value-bind (sigma mean)
      (healing-fx-params-chance (1- level))
    (max +minimum-chance-healing-fx+ (dabs (gaussian-probability sigma mean)))))

(defun modifier-params (level)
  (values (elt +modifier-sigma+ level)
	  (elt +modifier-mean+  level)))

(defun calculate-modifier (level)
  (multiple-value-bind (sigma mean)
      (modifier-params (1- level))
    (abs (gaussian-probability sigma mean))))

(defun set-healing-effect (effect-path level interaction)
  (let* ((effect-object (make-instance 'healing-effect-parameters
				       :trigger  +effect-when-used+
				       :duration (ceiling (max 1
							       (- +maximum-level+ level)))
				       :chance (calculate-healing-fx-params-chance
						level)
				       :target +target-other+)))
    (n-setf-path-value interaction effect-path effect-object)))


(defun set-healing-dmg-effect (path level interaction)
  (let ((effect-object (make-instance 'heal-damage-points-effect-parameters
				      :trigger +effect-when-used+
				      :points  (calculate-modifier level)
				      :chance  (calculate-healing-fx-params-chance
						level)
				      :target  +target-self+)))
    (n-setf-path-value interaction path effect-object)))


(defun set-poison-effect (effect-path level interaction)
  (let ((effect-object (make-instance 'poison-effect-parameters
				      :target          +target-other+
				      :chance (calculate-healing-fx-params-chance level)

				      :points-per-turn (random-weapon:calculate-modifier
							level))))
    (n-setf-path-value interaction effect-path effect-object)))

(defun generate-spell-common (interaction level)
  (let* ((healing-effects (%get-healing-fx-shuffled interaction 1)))
    (loop for i in healing-effects do
	 (cond
	   ((eq i +heal-damage-points+)
	    (set-healing-dmg-effect (list +healing-effects+ i)
				    level interaction))
	   ((eq i +cause-poison+)
	    (set-poison-effect (list +healing-effects+ i) level interaction))
	   (t
	    (set-healing-effect (list +healing-effects+ i) level interaction)))))
  interaction)

(defun %get-param (params a &optional (default nil))
  (getf params a default))

(defmacro define-attack-spell ((id) &body params)
  (alexandria:with-gensyms (spell)
    `(with-interaction-parameters
	   (let ((,spell (make-instance 'attack-spell
					:element                  ,(or (%get-param params :element)
								       :fire)

					:identifier               ,id
					:target                   +target-other+
					:gui-texture              ,(make-gui-texture (%get-param
										      params
										      :gui-texture))
					:cost                     ,(%get-param params :cost)
					:visual-effect-self       ,(and (%get-param
									 params
									 :visual-effect-self)
									`(function (%get-param
										    params
										    :visual-effect-self)))
					:range                    ,(%get-param params :range 0.0)
					:effective-range          ,(%get-param params
									       :effective-range
									       +terrain-chunk-tile-size+)
					:visual-effect-target     ,(and (%get-param
									 params
									 :visual-effect-target)
									`(function ,(%get-param
										     params
										     :visual-effect-target)))
					:damage-inflicted         ,(%get-param
								    params
								    :damage-inflicted 0.0)
					:arrow                    ,(and (%get-param params :arrow)
									`(function ,(%get-param
										     params
										     :arrow))))))

	     ,(%get-param params :effects)
	     (setf (basic-interaction-params ,spell)
		   (generate-spell-common *interaction-parameters*
					  ,(%get-param params :level)))
	     (add-spell ,spell)))))

(defmacro define-spell ((id) &body params)
  (alexandria:with-gensyms (spell)
    `(with-interaction-parameters
       (let ((,spell (make-instance 'spell
				    :identifier               ,id
				    :target                   +target-other+
				    :gui-texture           ,(make-gui-texture (%get-param
									       params
									       :gui-texture))
				    :cost                     ,(%get-param params :cost)
				    :visual-effect-self ,(and (%get-param params
									  :visual-effect-self)
							      `(function (%get-param
									  params
									  :visual-effect-self)))
				    :range                    ,(%get-param params :range 0.0)
				    :effective-range          ,(%get-param
								params
								:effective-range
								+terrain-chunk-tile-size+)
				    :visual-effect-target ,(and (%get-param
								 params
								 :visual-effect-target)
								`(function ,(%get-param
									     params
									     :visual-effect-target))))))
	 ,(%get-param params :effects)
	 (setf (basic-interaction-params ,spell)
	       (generate-spell-common *interaction-parameters*
				      ,(%get-param params :level)))
	 (add-spell ,spell)))))
