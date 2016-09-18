(in-package :spell)

(define-spell (:heal-3)
  :level                 10
  :element               :fire
  :gui-texture           "heal/heal-3.tga"
  :cost                   10.0
  :visual-effect-self    nil
  :range                 20.0
  :effective-range        5.0
  :visual-effect-target  particles:make-heal-level-2
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-damage-points generate)))
