(in-package :spell)

(define-spell (:heal-1)
  :level                 3
  :element               :fire
  :tags                  (:heal :heal-reward)
  :gui-texture           "heal/heal-1.tga"
  :cost                  5.0
  :visual-effect-self    none
  :range                 8   ;; in tile units
  :effective-range       0   ;; in tile units
  :visual-effect-target  particles:make-heal-level-0
  :sound-effect-target     sound:+generic-spell+ ;; see: sound.lisp
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-damage-points generate)))
