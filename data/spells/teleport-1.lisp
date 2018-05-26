(in-package :spell)

(define-spell (:teleport-1)
  :level                 1
  :element               :fire
  :tags                  (:teleport)
  :description           (_ "Teleport in a random location with maximum radius of ~6 tiles.")
  :gui-texture           "misc/teleport-1.tga"
  :cost                  10.0
  :visual-effect-self    none
  :range                 15  ;; in tile units
  :effective-range        0  ;; in tile units
  :visual-effect-target  particles:make-teleport-level-1
  :sound-effect-target     sound:+generic-spell+ ;; see: sound.lisp
  :effects
  (lambda (attacker defender)
    (declare (ignore attacker))
    (with-accessors ((state state)) defender
      (game-state:with-world (world state)
	(flet ((gen-displacement ()
		   (let* ((offset  (mapcar #'(lambda (a)
						 (d* +terrain-chunk-tile-size+
						     (d (round a))))
					   (first (num:bivariate-sampling 2.0 2.0 1))))
			  (x-chunk (d+  (d+ (elt (pos defender) 0) (elt offset 0))))
			  (z-chunk (d+  (d+ (elt (pos defender) 2) (elt offset 1))))
			  (y-chunk (d+ 1.5 ; hardcoded :(  to be removed soon
				       (game-state:approx-terrain-height@pos state
									     x-chunk
									     z-chunk))))
		     (matrix:with-check-matrix-borders ((game-state:map-state state)
							(map-utils:coord-chunk->matrix x-chunk)
							(map-utils:coord-chunk->matrix z-chunk))
		       (vec x-chunk y-chunk z-chunk))))
		 (count-max-reached-p (ct)
		   (>= ct 1000)))
	    (let ((displacement
		   (do ((displacement (gen-displacement) (gen-displacement))
			(ct           0                  (1+ ct)))
		       ((and (not (count-max-reached-p ct))
			     displacement
			     (game-state:map-element-empty-p
			      (game-state:element-mapstate@ state
							    (map-utils:coord-chunk->matrix
							     (elt displacement 0))
							    (map-utils:coord-chunk->matrix
							     (elt displacement 2)))))
			(and (not (count-max-reached-p ct))
			     displacement))))
		  (old-tile (map-utils:pos->game-state-pos defender)))
	      (when displacement
		(setf (pos defender) displacement)
		;; update state matrix and quadtree
		(world:move-entity world defender old-tile))))))))
