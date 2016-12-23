(in-package :spell)

(define-attack-spell (:firebolt-3)
  :level                 10
  :element               :fire
  :gui-texture           "attack/firearrow-lvl3.tga"
  :cost                  20.0
  :visual-effect-self    nil
  :range                 20 ;; in tile units
  :effective-range       2  ;; in tile units
  :effective-aabb-size   #'(lambda (spell)
			     (declare (ignore spell))
			     (d* (d/ +terrain-chunk-tile-size+ 8.0)
				 3.0))
  :visual-effect-target  particles:make-aerial-explosion-level-2
  :damage-inflicted      30.0
  :arrow                 particles:make-fire-dart-level-2
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects)))
