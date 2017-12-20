(in-package :spell)

(define-spell (:poison-2)
  :level                 5
  :tags                  (:damage)
  :element               :poison
  :gui-texture           "poison/cause-poison-2.tga"
  :cost                  5.0
  :visual-effect-self    none
  :range                 20  ;; in tile units
  :effective-range        0  ;; in tile units
  :visual-effect-target  particles:make-poison-level-1
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       cause-poison generate)))
