(in-package :spell)

(define-spell (:heal-2)
  :level                 4
  :element               :fire
  :gui-texture           "heal/heal-2.tga"
  :cost                   5.0
  :visual-effect-self    nil
  :range                 20.0
  :effective-range        1.0
  :visual-effect-target  particles:make-heal-level-1
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-damage-points generate)))
