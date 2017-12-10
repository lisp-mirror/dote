(in-package :spell)

(define-spell (:poison-1)
  :level                 1
  :element               :poison
  :gui-texture           "poison/cause-poison-1.tga"
  :cost                  2.0
  :visual-effect-self    none
  :range                 8  ;; in tile units
  :effective-range       0  ;; in tile units
  :visual-effect-target  particles:make-poison-level-0
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       cause-poison generate)))
