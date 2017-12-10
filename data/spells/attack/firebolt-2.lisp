(in-package :spell)

(define-attack-spell (:firebolt-2)
  :level                 5
  :element               :fire
  :tags                  (:damage)
  :gui-texture           "attack/firearrow-lvl2.tga"
  :cost                  5.0
  :visual-effect-self    none
  :range                 25 ;; in tile units
  :effective-range       0  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-1
  :effective-aabb-size  #'(lambda (spell)
			    (declare (ignore spell))
			    (d/ +terrain-chunk-tile-size+ 7.5))
  :damage-inflicted      18.0
  :arrow                 particles:make-fire-dart-level-1
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects)))
