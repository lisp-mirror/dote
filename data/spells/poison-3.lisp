(in-package :spell)

(define-spell (:poison-3)
  :level                 10
  :element               :poison
  :gui-texture           "poison/cause-poison-3.tga"
  :cost                  10.0
  :visual-effect-self    nil
  :range                 20  ;; in tile units
  :effective-range       5  ;; in tile units
  :visual-effect-target  particles:make-poison-level-2
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       cause-poison generate)))
