(in-package :spell)

(define-attack-spell (:firebolt-1)
  :level                 1
  :element               :fire
  :gui-texture           "attack/firearrow.tga"
  :cost                  2.0
  :visual-effect-self    none
  :range                 20 ;; in tile units
  :effective-range       0  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-0
  :effective-aabb-size  #'(lambda (spell)
			    (declare (ignore spell))
			    (d/ +terrain-chunk-tile-size+ 8.0))
  :damage-inflicted      10.0
  :arrow                 particles:make-fire-dart-level-0
  :tremor                (lambda (world) (world:apply-tremor-0 world))
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects)))
