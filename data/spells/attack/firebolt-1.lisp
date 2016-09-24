(in-package :spell)

(define-attack-spell (:firebolt-1)
  :level                 1
  :element               :fire
  :gui-texture           "attack/firearrow.tga"
  :cost                   2.0
  :visual-effect-self    nil
  :range                 20 ;; in tile units
  :effective-range       1  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-0
  :damage-inflicted      10.0
  :arrow                 particles:make-fire-dart-level-0
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       cause-poison generate)))
