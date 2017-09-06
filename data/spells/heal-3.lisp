(in-package :spell)

(define-spell (:heal-3)
  :level                 10
  :tags                  (:heal)
  :element               :fire
  :gui-texture           "heal/heal-3.tga"
  :cost                  30.0
  :visual-effect-self    nil
  :range                 20  ;; in tile units
  :effective-range        5  ;; in tile units
  :visual-effect-target  particles:make-heal-level-2
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-damage-points generate)))
