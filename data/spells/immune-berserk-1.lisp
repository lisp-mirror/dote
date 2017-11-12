(in-package :spell)

(define-spell (:immune-berserk-1)
  :level                 1
  :element               :fire
  :gui-texture           "cure/immune-berserk-1.tga"
  :cost                  2.0
  :visual-effect-self    nil
  :range                 5   ;; in tile units
  :effective-range       0   ;; in tile units
  :visual-effect-target  particles:make-heal-level-0
  :effects (define-interaction
	     effects         (define-effects)
	     healing-effects (define-healing-effects
				 immune-berserk generate)))
