(in-package :spell)

(define-spell (:heal-2)
  :level                 6
  :tags                  (:heal)
  :element               :fire
  :gui-texture           "heal/heal-2.tga"
  :cost                  10.0
  :visual-effect-self    nil
  :range                 10  ;; in tile units
  :effective-range        2  ;; in tile units
  :visual-effect-target  particles:make-heal-level-1
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-damage-points generate)))
