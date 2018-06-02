;; Dawn of the era: a tactical game.
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

(in-package :train-ai-window)


(defun train-ai-button-w ()
  (d* 3.0 (square-button-size *reference-sizes*)))

(defun train-ai-button-h ()
  (checkbutton-h *reference-sizes*))

(defun train-ai-window-w ()
  (let ((frame (train-ai-button-w)))
    (adjust-window-w frame)))

(defun train-ai-window-h ()
  (let ((frame (d+ (d* 7.0 (train-ai-button-h))
                   (spacing *reference-sizes*))))
    (adjust-window-h frame)))

(defun make-callback (strategy-decision)
  #'(lambda (button e)
      (declare (ignore e))
      (with-grandparent-widget (win) button ; button is child of labeled-check-button
        (setf (strategy-chosen win) strategy-decision)
        t)))

(defun help-msg ()
  (_ "Choose the strategy you are going to use in this turn, then close."))

(defclass train-ai-window (window)
  ((strategy-chosen
    :initform nil
    :initarg  :strategy-chosen
    :accessor   strategy-chosen)
   (lb-help
    :initform (make-instance 'static-text
                             :label     (help-msg)
                             :font-size (h1-font-size *reference-sizes*)
                             :width     (train-ai-button-w)
                             :height    (d* 3.0 (train-ai-button-h))
                             :x         0.0
                             :y         0.0)
    :initarg  :lb-help
    :accessor lb-help)
   (checkb-explore
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (train-ai-button-h)
                             :width              (train-ai-button-w)
                             :x                  0.0
                             :y                  (add-epsilon-rel (d* 3.0 (train-ai-button-h))
                                                                  (spacing-rel *reference-sizes*))
                             :label              (_ "Explore")
                             :callback           (make-callback +explore-strategy+)
                             :initial-state      nil
                             :color              :green)
    :initarg  :checkb-explore
    :accessor checkb-explore)
   (checkb-attack
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (train-ai-button-h)
                             :width              (train-ai-button-w)
                             :x                  0.0
                             :y                  (add-epsilon-rel (d* 4.0 (train-ai-button-h))
                                                                  (spacing-rel *reference-sizes*))
                             :label              (_ "Attack")
                             :callback           (make-callback +attack-strategy+)
                             :initial-state      nil
                             :color              :green)
    :initarg  :checkb-attack
    :accessor checkb-attack)
   (checkb-defend
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (train-ai-button-h)
                             :width              (train-ai-button-w)
                             :x                  0.0
                             :y                  (add-epsilon-rel (d* 5.0 (train-ai-button-h))
                                                                  (spacing-rel *reference-sizes*))
                             :label              (_ "Defend")
                             :callback           (make-callback +defend-strategy+)
                             :initial-state      nil
                             :color              :green)
    :initarg  :checkb-defend
    :accessor checkb-defend)
   (checkb-retreat
    :initform (make-instance 'labeled-check-button
                             :button-toggle-type t
                             :height             (train-ai-button-h)
                             :width              (train-ai-button-w)
                             :x                  0.0
                             :y                  (add-epsilon-rel (d* 6.0 (train-ai-button-h))
                                                                  (spacing-rel *reference-sizes*))
                             :label              (_ "Retreat")
                             :callback           (make-callback +retreat-strategy+)
                             :initial-state      nil
                             :color              :green)
    :initarg  :checkb-retreat
    :accessor checkb-retreat)))

(defmethod initialize-instance :after ((object train-ai-window) &key &allow-other-keys)
  (with-accessors ((lb-help        lb-help)
                   (checkb-explore checkb-explore)
                   (checkb-attack  checkb-attack)
                   (checkb-defend  checkb-defend)
                   (checkb-retreat checkb-retreat)) object
    (add-child object lb-help)
    (add-child object checkb-explore)
    (add-child object checkb-attack)
    (add-child object checkb-defend)
    (add-child object checkb-retreat)
    (let ((group-class (make-check-group* checkb-explore
                                          checkb-attack
                                          checkb-defend
                                          checkb-retreat)))
      (setf (group checkb-explore) group-class)
      (setf (group checkb-attack)  group-class)
      (setf (group checkb-defend)  group-class)
      (setf (group checkb-retreat) group-class))))

(defun close-button-callback (b e)
  "Close the window and set the strategy in facts db"
  (with-grandparent-widget (w) b
    (when (strategy-chosen w)
      (when-let ((db (strategic-ai:read-facts-file)))
        (setf (lastcar db)
              (append (lastcar db) (list (strategy-chosen w))))
        (strategic-ai:dump-ai-facts db))
      (hide-and-remove-grandparent-cb b e))))

(defun make-train-ai-window (compiled-shaders)
  (let ((w (make-instance 'train-ai-window
                          :x      0.0
                          :y      (d (- *window-h* (train-ai-window-h)))
                          :width  (train-ai-window-w)
                          :height (train-ai-window-h)
                          :label  (_ "Train AI"))))
    (setf (compiled-shaders w) compiled-shaders)
    (setf (callback (close-button w)) #'close-button-callback)
    w))
