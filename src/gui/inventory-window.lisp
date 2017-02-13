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

(defclass inventory-slot-button (check-button)
  ((contained-entity
    :initform nil
    :initarg  :contained-entity
    :accessor contained-entity)))

(defun y-just-under-slot-page ()
  (d+ (d* (d +slots-per-page-side-size+)
	  (small-square-button-size *reference-sizes*))
      (small-square-button-size *reference-sizes*)
      (spacing                      *reference-sizes*)))

(defun make-inventory-slot-button (x y &key (callback nil))
  (make-instance 'inventory-slot-button
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

(defun %unlock-cb (player chest)
  #'(lambda (w e)
      (declare (ignore w e))
      (game-event:send-unlock-event player chest)))

(defun %force-unlock-cb (player chest)
  #'(lambda (w e)
      (declare (ignore w e))
      (let ((ghost-chest  (ghost chest))
	    (ghost-player (ghost player))
	    (state        (state player)))
	(if (die-utils:pass-d1.0 (d/ (actual-unlock-chance ghost-player)
				     (level                ghost-chest)))
	    (game-event:send-unlock-event player chest :force t)
	    (game-state:with-world (world state)
	      (world:post-entity-message world
					 player
					 (_ "Unlocking failed")
					 t))))))

(defun %unlock-chest (world player chest)
  (world:post-entity-message world
			     player
			     (_ "This chest is locked")
			     t
			     (cons (_ "force lock")
				   (%force-unlock-cb player chest))
			     (cons (_ "use key")
				   (%unlock-cb player chest))))

(defun show/hide-chest-slots-cb (w e)
  (declare (ignore e))
  (let* ((window (parent w))
	 (chest-slots (chest-slots window))
	 (chest       (chest window))
	 (chest-ghost (and chest
			   (ghost (chest window))))
	 (player      (owner       window))
	 (state       (state       player)))
    (when chest
      (if (lockedp chest-ghost)
	  (game-state:with-world (world state)
	    (%unlock-chest world player chest))
	  (if (button-state w)
	      (loop for slot in chest-slots do
		   (show slot))
	      (loop for slot in chest-slots do
		   (hide slot)))))))

(defun remove-inventory-page (window)
  (let ((current-slots-page (current-slots-page window)))
    (setf (children window)
	  (remove-if #'(lambda (c)
			 (find-if #'(lambda (s) (= (id s) (id c))) current-slots-page))
		     (children window)))))

(defun add-containded-item (slot item)
  (if item
      (progn
	(setf (texture-overlay  slot)
	      (or (and item
		       (character:portrait item))
		  (get-texture +transparent-texture-name+)))
	(setf (contained-entity slot) item))
      (remove-containded-item slot)))

(defun remove-containded-item (slot)
  (setf (texture-overlay  slot) (get-texture +transparent-texture-name+)
	(contained-entity slot) nil))

(defun empty-slot-p (slot)
  (null (contained-entity slot)))

(defun inventory-update-description-cb (widget e)
  (declare (ignore e))
  (with-parent-widget (win) widget
    (when (contained-entity widget)
      (setf (label (text-description win))
	    (description-for-humans (contained-entity widget))))
    t))

(defun pick-item-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (with-accessors ((chest-slots chest-slots) (owner owner)) win
      (multiple-value-bind (chest-slot item)
	  (get-selected-chest-item win)
	(when (and chest-slot item)
	  (let ((available-slot (find-if #'(lambda (s) (empty-slot-p s))
					 (alexandria:flatten (slots-pages win)))))
	    (when (and available-slot
		       chest-slot)
	      (character:restart-age item)
	      (add-containded-item     available-slot item)
	      (remove-containded-item  chest-slot)
	      (remove-child (ghost (chest win)) item :test #'= :key #'id)
	      (character:add-to-inventory (ghost owner) item))))))))

(defun find-available-slot (inventory-win)
  (find-if #'(lambda (s) (empty-slot-p s))
	   (alexandria:flatten (slots-pages inventory-win))))

(defun remove-worn-item (slot-from slot-to character character-slot)
  "Return the worn item"
  (when (and (empty-slot-p slot-to)
	     (not (empty-slot-p slot-from)))
    (let ((saved (contained-entity slot-from)))
      (setf (slot-value character character-slot) nil)
      (add-containded-item slot-from nil)
      (add-containded-item slot-to   saved)
      saved)))

(defun remove-worn-item-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (with-accessors ((owner owner)) win
      (with-accessors ((ghost ghost)) owner
	(multiple-value-bind (selected-worn-slot worn-item)
	    (get-selected-worn-item win)
	  (when (and selected-worn-slot
		     worn-item)
	    (let ((available-slot (find-available-slot win)))
	      (when available-slot
		(let ((c-slot (item->player-character-slot ghost worn-item)))
		  (when c-slot
		    (remove-worn-item selected-worn-slot
				      available-slot
				      (ghost owner)
				      c-slot)
		    (character:add-to-inventory (ghost owner) worn-item)
		    ;; events
		    (let ((all-effects (remove-if
					(random-object-messages:untrigged-effect-p-fn
					 basic-interaction-parameters:+effect-when-worn+)
					(random-object-messages:params->effects-messages-complement worn-item))))
		      (game-event:propagate-unwear-object-event
		       (game-event:make-simple-event-w-dest 'game-event:unwear-object-event
							    (id worn-item)
							    (id owner)))
		      (random-object-messages:propagate-effects-msg worn-item owner all-effects))))))))))))

(defun remove-item-from-inventory (owner slot item)
  (remove-containded-item slot)
  (setf (character:inventory (ghost owner))
	(remove-if #'(lambda (a) (= (id item) a))
		   (character:inventory (ghost owner))
		   :key #'id)))

(defun dismiss-item-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (with-accessors ((chest-slots chest-slots) (owner owner) (chest chest)) win
      (multiple-value-bind (slot item)
	  (get-selected-item win)
	(when (and slot
		   item
		   owner)
	  (with-accessors ((state state)) owner
	    (if chest
		(if (lockedp (ghost chest))
		    (game-state:with-world (world state)
		      (%unlock-chest world owner chest))
		    (let ((available-chest-slot (loop
						   named inner
						   for slot in chest-slots do
						     (when (empty-slot-p slot)
						       (return-from inner slot)))))
		      (when (and slot
				 available-chest-slot)
			(add-containded-item available-chest-slot item)
			(add-child (ghost chest) item)
			(remove-item-from-inventory owner slot item))))
		(game-state:with-world (world state)
		  (world:post-entity-message world
					     owner
					     (_ "There is not any chest here")
					     nil)))))))))

(defun open-characteristics-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (with-accessors ((owner owner)) win
      (let ((new-window (make-player-report-win (ghost owner))))
	(setf (compiled-shaders new-window) (compiled-shaders win))
	(setf (callback (close-button new-window)) #'hide-and-remove-grandparent-cb)
	(add-child win new-window)))))

(defun open-spell-list-cb (w e)
  (declare (ignore e))
  (let ((root (find-root-widget w)))
    (with-parent-widget (win) w
      (with-accessors ((owner owner)) win
	(when owner
	  (with-accessors ((ghost ghost)) owner
	    (let* ((spell-list (make-spell-window owner)))
	      (setf (compiled-shaders spell-list) (compiled-shaders win))
	      (add-child root spell-list))))))
    t))

(defun next-slot-page-cb (w e)
  (declare (ignore e))
  (let* ((window       (parent w))
	 (slots        (slots-pages window))
	 (pages-count  (length (slots-pages window)))
	 (page-no      (current-slots-page-number window))
	 (next-page-no (mod (1+ page-no) pages-count)))
    (remove-inventory-page window)
    (setf (current-slots-page window) (elt slots next-page-no))
    (map nil #'(lambda (s) (setf (compiled-shaders s) (compiled-shaders window)))
	 (current-slots-page window))
    (map nil #'(lambda (s) (add-child window s)) (current-slots-page window))
    (update-page-counts window next-page-no)))

(defun prev-slot-page-cb (w e)
  (declare (ignore e))
  (let* ((window       (parent w))
	 (slots        (slots-pages window))
	 (page-no      (current-slots-page-number window))
	 (next-page-no (if (< (1- page-no) 0)
			   (1- (length (slots-pages window)))
			   (1- page-no))))
    (remove-inventory-page window)
    (setf (current-slots-page window) (elt slots next-page-no))
    (map nil #'(lambda (s) (setf (compiled-shaders s) (compiled-shaders window)))
	 (current-slots-page window))
    (map nil #'(lambda (s) (add-child window s)) (current-slots-page window))
    (update-page-counts window next-page-no)))

(defun item->window-accessor (window item)
  (with-accessors ((elm-slot        elm-slot)
		   (shoes-slot      shoes-slot)
		   (armor-slot      armor-slot)
		   (left-hand-slot  left-hand-slot)
		   (right-hand-slot right-hand-slot)
		   (ring-slot       ring-slot))      window
    (and item
	 (cond
	   ((interactive-entity:ringp item)
	    ring-slot)
	   ((interactive-entity:armorp item)
	    armor-slot)
	   ((interactive-entity:elmp item)
	    elm-slot)
	   ((interactive-entity:shoesp item)
	    shoes-slot)
	   ((or (interactive-entity:weaponp item)
		(interactive-entity:shieldp item))
	    (if (empty-slot-p left-hand-slot)
		left-hand-slot
		right-hand-slot))))))

(defun swap-worn-item (slot-from slot-to character character-slot)
  (let ((saved (contained-entity slot-from)))
    (setf (slot-value character character-slot) (contained-entity slot-from))
    (add-containded-item slot-from (contained-entity slot-to))
    (add-containded-item slot-to   saved)))

(defun worn-item (slot-from slot-to character character-slot)
  (when (and (empty-slot-p slot-to)
	     (not (empty-slot-p slot-from)))
    (let ((saved (contained-entity slot-from)))
      (setf (slot-value character character-slot) (contained-entity slot-from))
      (remove-from-inventory character saved)
      (add-containded-item slot-from (contained-entity slot-to))
      (add-containded-item slot-to   saved))))

(defun wear-item-cb (widget e)
  (declare (ignore e))
  (with-parent-widget (win) widget
    (with-accessors ((owner owner)) win
      (with-accessors ((ghost ghost)) owner
	(multiple-value-bind (slot item)
	    (get-selected-item win)
	  (when item
	    (let ((c-slot       (item->available-player-character-slot ghost item))
		  (win-accessor (item->window-accessor       win   item)))
	      (when (and c-slot
			 win-accessor
			 (can-use-movement-points-p owner))
		(worn-item slot win-accessor (ghost owner) c-slot)
		(let* ((messages (random-object-messages:params->effects-messages item))
		       (event    (make-instance 'game-event:wear-object-event
						:id-origin      (id item)
						:id-destination (id owner)
						:event-data     messages)))
		  (game-event:propagate-wear-object-event event))))))))))

(defun sort-items (pages)
  (let ((all (mapcar #'contained-entity (alexandria:flatten pages))))
    (remove-if-null
     (nconc
      (remove-if #'(lambda (a) (not (interactive-entity:weaponp a))) all)
      (remove-if #'(lambda (a) (not (interactive-entity:shieldp a))) all)
      (remove-if #'(lambda (a) (not (interactive-entity:elmp a))) all)
      (remove-if #'(lambda (a) (not (interactive-entity:armorp a))) all)
      (remove-if #'(lambda (a) (not (interactive-entity:shoesp a))) all)
      (remove-if #'(lambda (a) (not (interactive-entity:potionp a))) all)
      (remove-if #'(lambda (a) (not (interactive-entity:ringp a))) all)
      (remove-if #'(lambda (a) (not (interactive-entity:keyp a))) all)
      (remove-if #'(lambda (a) (not (interactive-entity:trapp a))) all)))))

(defun sort-items-enter-cb (w e)
  (declare (ignore e))
  (with-parent-widget (win) w
    (let ((sorted-items (sort-items (slots-pages win))))
      (map nil
	   #'(lambda (a) (remove-containded-item a))
	   (alexandria:flatten (slots-pages win)))
      (map nil #'(lambda (s i) (add-containded-item s i))
	   (alexandria:flatten (slots-pages win))
	   sorted-items))))

(defun use-item-cb (widget e)
  (declare (ignore e))
  (with-parent-widget (win) widget
    (with-accessors ((owner owner)) win
      (multiple-value-bind (slot item)
	  (get-selected-item win)
	(when item
	  (cond
	    ((be-consumed-p item)
	     (let ((all-effects (random-object-messages:params->effects-messages
				 item)))
	       (remove-item-from-inventory owner slot item)
	       (random-object-messages:propagate-effects-msg item owner all-effects)))
	    ((trapp item)
	     (when (mesh:trap-can-be-placed-p owner)
	       (md2-mesh:place-trap owner item)
	       (remove-item-from-inventory owner slot item)))))))))

(defclass table-paginated-window (window)
  ((owner
    :initform nil
    :initarg  :owner
    :accessor owner
    :type md2:md2-mesh)
   (slots-pages
    :initform '()
    :initarg  :slots-pages
    :accessor slots-pages)
   (b-next-page
    :initform (make-instance 'naked-button
			     :x               (d* 3.0 (small-square-button-size *reference-sizes*))
			     :y               0.0
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +right-overlay-texture-name+)
			     :callback        #'next-slot-page-cb)
    :initarg  :b-next-page
    :accessor b-next-page)
   (b-prev-page
    :initform (make-instance 'naked-button
			     :x               0.0
			     :y               0.0
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +left-overlay-texture-name+)
			     :callback         #'prev-slot-page-cb)

    :accessor b-prev-page)
   (lb-page-count
    :initform (make-instance 'simple-label-prefixed
			     :width  (d* 1.9 (small-square-button-size *reference-sizes*))
			     :height (input-text-h *reference-sizes*)
			     :x      (small-square-button-size *reference-sizes*)
			     :y      (d- (d/ (small-square-button-size *reference-sizes*) 2.0)
					 (d/ (input-text-h *reference-sizes*)))
			     :prefix (_ "Page ")
			     :label "")
    :initarg  :lb-page-count
    :accessor lb-page-count)
   (current-slots-page
    :initform nil
    :initarg  :current-slots-page
    :accessor current-slots-page)
   (current-slots-page-number
    :initform  0
    :initarg  :current-slots-page-number
    :accessor current-slots-page-number)
   (text-description
    :initform (make-instance 'widget:static-text
			     :height (d* 3.0
					 (small-square-button-size *reference-sizes*))
			     :width  (d* 6.0
					 (small-square-button-size *reference-sizes*))
			     :x      0.0
			     :y      (d+ (y-just-under-slot-page)
					 (small-square-button-size *reference-sizes*))
			     :font-size (d* 0.1 *square-button-size*)
			     :label ""
			     :justified t)
    :initarg  :text-description
    :accessor text-description)))

(defgeneric get-selected-item (object))

(defgeneric update-page-counts (object pos))

(defmethod get-selected-item ((object table-paginated-window))
  (with-accessors ((current-slots-page current-slots-page)) object
    (let ((found (find-if #'(lambda (a) (and (button-state     a)
					     (contained-entity a)))
			  current-slots-page)))
      (when found
	(values found (contained-entity found))))))

(defmethod update-page-counts ((object table-paginated-window) pos)
  (with-accessors ((lb-page-count lb-page-count)
		   (slots-pages slots-pages)
		   (current-slots-page-number current-slots-page-number)) object
    (setf current-slots-page-number pos)
    (setf (label lb-page-count)
	  (format nil (_ "~a of ~a") (1+ current-slots-page-number) (length slots-pages)))))

(defmethod initialize-instance :after ((object table-paginated-window)
				       &key
					 (pages-num 3)
					 (make-slot-button-fn #'make-inventory-slot-button)
					 (callback-slot-button #'inventory-update-description-cb)
					 &allow-other-keys)
  (with-accessors ((owner owner)
		   (slots-pages slots-pages)
		   (current-slots-page current-slots-page)
		   (current-slots-page-number current-slots-page-number)
		   (b-next-page b-next-page)
		   (b-prev-page b-prev-page)
		   (lb-page-count lb-page-count)
		   (text-description text-description)) object
    (let ((page-count (if owner
			  (character:inventory-slot-pages-number (ghost owner))
			  pages-num))
	  (starting-y (small-square-button-size *reference-sizes*)))
      (setf slots-pages
	    (loop repeat page-count collect
		 (let ((page '())
		       (button-size (small-square-button-size *reference-sizes*)))
		   (loop for i from 0 below +slots-per-page-side-size+ do
			(loop for j from 0 below  +slots-per-page-side-size+ do
			     (let* ((button (funcall
					     make-slot-button-fn
					     (* i button-size)
					     (d+ starting-y
						 (* j button-size))
					     :callback callback-slot-button)))
			       (push button page))))
		   (reverse page))))
      (setf current-slots-page (elt slots-pages 0))
      (let ((group-slots (make-check-group (alexandria:flatten slots-pages))))
	(loop for i in (alexandria:flatten slots-pages) do
	     (setf (group i) group-slots)))
      (loop for slot in current-slots-page do
	   (add-child object slot))
      (add-child object b-prev-page)
      (add-child object b-next-page)
      (add-child object lb-page-count)
      (add-child object text-description)
      (update-page-counts object current-slots-page-number))))

(defun chest-texture (chest)
  (cond
    ((null chest)
     (get-texture +transparent-texture-name+))
    ((lockedp (ghost chest))
     (get-texture +chest-closed-locked-texture-name+))
    (t
     (get-texture +chest-closed-texture-name+))))

(defclass inventory-window (table-paginated-window)
  ((chest
    :initform nil
    :initarg  :chest
    :accessor chest
    :type     container-mesh-shell)
   (chest-slots
    :initform (loop for x from 1.0 to (d +container-capacity+) by 1.0 collect
		   (make-inventory-slot-button (d* x (small-square-button-size *reference-sizes*))
					       (y-just-under-slot-page)
					       :callback #'inventory-update-description-cb))
    :initarg  :chest-slots
    :accessor chest-slots)
   (b-chest
    :initform (make-instance 'toggle-button
			     :width  (small-square-button-size *reference-sizes*)
			     :height (small-square-button-size *reference-sizes*)
			     :x      0.0
			     :y      (y-just-under-slot-page)
			     :texture-object  (get-texture +chest-closed-texture-name+)
			     :texture-pressed (get-texture +chest-opened-texture-name+)
			     :texture-overlay (get-texture +transparent-texture-name+)
			     :callback        #'show/hide-chest-slots-cb
			     :button-status   nil)
    :initarg  :b-chest
    :accessor b-chest)
   (b-sort
    :initform (make-instance 'button
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y      0.0
			     :callback #'sort-items-enter-cb
			     :label (_ "sort"))
    :initarg  b-sort
    :accessor b-sort)
   (b-use
    :initform (make-instance 'naked-button
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y               (small-square-button-size *reference-sizes*)
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +use-item-overlay-texture-name+)
			     :callback        #'use-item-cb)
    :initarg :b-use
    :accessor b-use)
   (b-wear
    :initform (make-instance 'naked-button
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y               (d* 2.0
						  (small-square-button-size *reference-sizes*))
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +wear-overlay-texture-name+)
			     :callback        #'wear-item-cb)
    :initarg :b-wear
    :accessor b-wear)
   (b-pick
    :initform (make-instance 'naked-button
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y               (d* 3.0
						  (small-square-button-size *reference-sizes*))
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +up-arrow-overlay-texture-name+)
			     :callback        #'pick-item-cb)
    :initarg :b-pick
    :accessor b-pick)
   (b-dismiss
    :initform (make-instance 'naked-button
			     :x               (d* (d +slots-per-page-side-size+)
						  (small-square-button-size *reference-sizes*))
			     :y               (d* 4.0
						  (small-square-button-size *reference-sizes*))
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +down-arrow-overlay-texture-name+)
			     :callback        #'dismiss-item-cb)
    :initarg :b-dismiss
    :accessor b-dismiss)
   (b-characteristics
    :initform (make-instance 'naked-button
			     :x               (d* (d (1+ +slots-per-page-side-size+))
						  (small-square-button-size *reference-sizes*))
			     :y               (small-square-button-size *reference-sizes*)
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +use-overlay-texture-name+)
			     :callback        #'open-characteristics-cb)
    :initarg :b-characteristics
    :accessor b-characteristics)
   (b-spell-book
    :initform (make-instance 'naked-button
			     :x               (d* (d (1+ +slots-per-page-side-size+))
						  (small-square-button-size *reference-sizes*))
			     :y               (d* 2.0
						  (small-square-button-size *reference-sizes*))
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +spell-book-overlay-texture-name+)
			     :callback        #'open-spell-list-cb)
    :initarg :b-spell-book
    :accessor b-spell-book)
   (img-silhouette
    :initform (make-instance 'signalling-light
			     :x             (d- (d* 0.66 (inventory-window-width))
						(d/ (inventory-silhouette-w) 2.0))
			     :y             (small-square-button-size *reference-sizes*)
			     :width         (inventory-silhouette-w)
			     :height        (inventory-silhouette-h)
			     :texture-name  +silhouette-texture-name+
			     :button-status t)
    :initarg  :img-silhouette
    :accessor img-silhouette)
   (elm-slot
    :initform (make-inventory-slot-button (d- (d* 0.66 (inventory-window-width))
					      (d/ (small-square-button-size *reference-sizes*)
						  2.0))
					  (d/ (small-square-button-size *reference-sizes*)
					      2.0)
					  :callback #'inventory-update-description-cb)
    :initarg  :elm-slot
    :accessor elm-slot)
   (shoes-slot
    :initform (make-inventory-slot-button (d- (d* 0.66 (inventory-window-width))
					      (d/ (small-square-button-size *reference-sizes*)
						  2.0))
					  (d- (inventory-window-height)
					      (small-square-button-size *reference-sizes*))
					  :callback #'inventory-update-description-cb)
    :initarg  :shoes-slot
    :accessor shoes-slot)
   (armor-slot
    :initform (make-inventory-slot-button (d- (d* 0.66 (inventory-window-width))
					      (d/ (small-square-button-size *reference-sizes*)
						  2.0))
					  (d- (d/ (inventory-window-height) 2.0)
					      (d* 1.5
						  (small-square-button-size *reference-sizes*)))
					  :callback #'inventory-update-description-cb)
    :initarg  :armor-slot
    :accessor armor-slot)
    (right-hand-slot
    :initform (make-inventory-slot-button (d- (d* 0.66 (inventory-window-width))
					      (d* 2.0
						  (small-square-button-size *reference-sizes*)))
					  (d/ (inventory-window-height) 2.0)
					  :callback #'inventory-update-description-cb)
    :initarg  :right-hand-slot
    :accessor right-hand-slot)
   (left-hand-slot
    :initform (make-inventory-slot-button (d+ (d* 0.66 (inventory-window-width))
					      (small-square-button-size *reference-sizes*))
					  (d/ (inventory-window-height) 2.0)
					  :callback #'inventory-update-description-cb)
    :initarg  :left-hand-slot
    :accessor left-hand-slot)
   (ring-slot
    :initform (make-inventory-slot-button (d+ (d* 0.66 (inventory-window-width))
					      (small-square-button-size *reference-sizes*))
					  (d- (d/ (inventory-window-height) 2.0)
					      (small-square-button-size *reference-sizes*)
					      (spacing *reference-sizes*))
					  :callback #'inventory-update-description-cb)
    :initarg  :ring-slot
    :accessor ring-slot)
   (b-remove-worn-item
    :initform (make-instance 'naked-button
			     :x               (d- (d* 0.66 (inventory-window-width))
					      (d* 2.0
						  (small-square-button-size *reference-sizes*)))
			     :y               (d- (d/ (inventory-window-height) 2.0)
						  (small-square-button-size *reference-sizes*)
						  (spacing *reference-sizes*))
			     :width           (small-square-button-size *reference-sizes*)
			     :height          (small-square-button-size *reference-sizes*)
			     :texture-object  (get-texture +button-texture-name+)
			     :texture-pressed (get-texture +button-pressed-texture-name+)
			     :texture-overlay (get-texture +add-to-bag-texture-name+)
			     :callback        #'remove-worn-item-cb)
    :initarg  :b-remove-worn-item
    :accessor b-remove-worn-item)))

(defmethod initialize-instance :after ((object inventory-window) &key &allow-other-keys)
  (with-accessors ((slots-pages slots-pages) (current-slots-page current-slots-page)
		   (owner owner) (b-use b-use)
		   (chest chest)
		   (b-sort b-sort)
		   (b-wear b-wear)
		   (b-pick b-pick)
		   (b-dismiss b-dismiss)
		   (b-characteristics b-characteristics)
		   (b-spell-book b-spell-book)
		   (b-chest b-chest) (b-next-page b-next-page)
		   (current-slots-page-number current-slots-page-number)
		   (b-prev-page b-prev-page) (lb-page-count lb-page-count)
		   (chest-slots chest-slots)
		   (img-silhouette img-silhouette)
		   (elm-slot elm-slot) (shoes-slot shoes-slot)
		   (armor-slot armor-slot) (left-hand-slot left-hand-slot)
		   (right-hand-slot right-hand-slot) (ring-slot ring-slot)
		   (b-remove-worn-item b-remove-worn-item))             object

    (setf (texture-object  b-chest) (chest-texture chest))
    (setf (current-texture b-chest) (chest-texture chest))
    (setf (texture-pressed b-chest) (chest-texture chest))
    (let ((group-chest (make-check-group chest-slots)))
      (map nil
	   #'(lambda (a) (setf (group a) group-chest))
	   chest-slots))
      (loop for slot in chest-slots do
	   (add-child object slot)
	   (hide slot))
      (add-child object b-sort)
      (add-child object b-use)
      (add-child object b-wear)
      (add-child object b-pick)
      (add-child object b-dismiss)
      (add-child object b-characteristics)
      (add-child object b-spell-book)
      (add-child object b-chest)
      (add-child object img-silhouette)
      (let ((group-worn (make-check-group* elm-slot
					   shoes-slot
					   armor-slot
					   left-hand-slot
					   right-hand-slot
					   ring-slot)))
	(setf (group elm-slot) group-worn)
	(setf (group shoes-slot) group-worn)
	(setf (group armor-slot) group-worn)
	(setf (group left-hand-slot) group-worn)
	(setf (group right-hand-slot) group-worn)
	(setf (group ring-slot) group-worn))
      (add-child object elm-slot)
      (add-child object shoes-slot)
      (add-child object armor-slot)
      (add-child object left-hand-slot)
      (add-child object right-hand-slot)
      (add-child object ring-slot)
      (add-child object b-remove-worn-item)
      (add-inventory-objects object)))

(defgeneric add-inventory-objects (object))

(defgeneric get-selected-chest-item (object))

(defgeneric get-selected-worn-item  (object))

(defmacro gen-get-selected-item ((name) &rest slots)
  (alexandria:with-gensyms (selected)
    `(defmethod  ,(alexandria:format-symbol t "~@:(get-selected-~a-item~)" name)
	 ((object inventory-window))
       (let ((,selected (or
			 ,@(loop for slot in slots collect
				`(and (,slot object)
				      (button-state (,slot object))
				      (contained-entity (,slot object))
				      (,slot object))))))
	 (and ,selected
	      (values ,selected (contained-entity ,selected)))))))

(defmethod get-selected-chest-item ((object inventory-window))
  (let ((selected (loop
		     named inner
		     for slot in (chest-slots object) do
		       (when (and slot
				  (button-state slot))
			 (return-from inner slot)))))
    (and selected
         (values selected (contained-entity selected)))))

(gen-get-selected-item (worn)
		       elm-slot
		       armor-slot
		       shoes-slot
		       left-hand-slot
		       right-hand-slot
		       ring-slot)

(defmethod add-inventory-objects ((object inventory-window))
  (with-accessors ((slots-pages slots-pages)
		   (owner           owner)
		   (chest           chest)
		   (chest-slots     chest-slots)
		   (elm-slot        elm-slot)
		   (shoes-slot      shoes-slot)
		   (armor-slot      armor-slot)
		   (right-hand-slot right-hand-slot)
		   (left-hand-slot  left-hand-slot)
		   (ring-slot       ring-slot)) object
    (when owner
      (assert (<= (length (character:inventory (ghost owner)))
		  (length (alexandria:flatten slots-pages))))
      (with-accessors ((ghost ghost)) owner
	(with-accessors ((elm        elm)
			 (shoes      shoes)
			 (armor      armor)
			 (left-hand  left-hand)
			 (right-hand right-hand)
			 (ring       ring)) ghost
	  (loop
	     for slot in (alexandria:flatten slots-pages)
	     for obj  in (character:inventory ghost) do
	       (setf (contained-entity slot) obj
		     (texture-overlay  slot) (character:portrait obj)))
	  (when elm
	    (setf (contained-entity elm-slot) elm
		  (texture-overlay  elm-slot) (character:portrait elm)))
	  (when shoes
	    (setf (contained-entity shoes-slot) shoes
		  (texture-overlay  shoes-slot) (character:portrait shoes)))
	  (when armor
	    (setf (contained-entity armor-slot) armor
		  (texture-overlay  armor-slot) (character:portrait armor)))
	  (when left-hand
	    (setf (contained-entity left-hand-slot) left-hand
		  (texture-overlay  left-hand-slot) (character:portrait left-hand)))
	  (when right-hand
	    (setf (contained-entity right-hand-slot) right-hand
		  (texture-overlay  right-hand-slot) (character:portrait right-hand)))
	  (when ring
	    (setf (contained-entity ring-slot) ring
		  (texture-overlay  ring-slot) (character:portrait ring))))))
    ;; display stuff inside the chest, if any
    (when chest
      (let ((chest-ghost (ghost chest)))
	(assert (or (null (children chest-ghost))
		    (<= (length (children chest-ghost)) 3)))
	(loop for i from 0 below (length (children chest-ghost)) do
	     (setf (contained-entity (elt chest-slots i)) (elt (children chest-ghost) i))
	     (setf (texture-overlay  (elt chest-slots i))
		   (character:portrait (elt (children chest-ghost) i))))))))

(defun inventory-window-width ()
  (d+ (d* 13.0 (small-square-button-size *reference-sizes*))
      (d* 2.0
	  (left-frame-offset *reference-sizes*)
	  (d* 5.0 (small-square-button-size *reference-sizes*)))))

(defun inventory-silhouette-w ()
  (d* 0.25 (inventory-window-width)))

(defun inventory-silhouette-h ()
  (d- (inventory-window-height)
      (d* 3.0 (small-square-button-size *reference-sizes*))))

(defun inventory-window-height ()
    (d+ (d* 12.0 (small-square-button-size *reference-sizes*))))

(defun make-inventory-window (player &optional (chest nil))
  (make-instance 'inventory-window
		 :owner  player
		 :chest  chest
		 :x      0.0
		 :y      (d (- *window-h* (inventory-window-height)))
		 :width  (inventory-window-width)
		 :height (inventory-window-height)
		 :label  (_ "Inventory")))
