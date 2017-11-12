(in-package :spell)

(define-spell (:heal-1)
  :level                 3
  :element               :fire
  :tags                  (:heal :heal-reward)
  :gui-texture           "heal/heal-1.tga"
  :cost                  5.0
  :visual-effect-self    nil
  :range                 5   ;; in tile units
  :effective-range       0   ;; in tile units
  :visual-effect-target  particles:make-heal-level-0
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-damage-points generate)))
