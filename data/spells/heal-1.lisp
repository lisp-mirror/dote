(in-package :spell)

(define-attack-spell (:heal-1)
  :level                 1
  :element               :fire
  :gui-texture           "heal-1.tga"
  :cost                   2.0
  :visual-effect-self    nil
  :range                 20.0
  :effective-range        1.0
  :visual-effect-target  particles:make-heal-level-0
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-damage-points generate)))
