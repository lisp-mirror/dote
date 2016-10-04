(in-package :spell)

(define-spell (:cure-poison-2)
  :level                 5
  :element               :fire
  :gui-texture           "cure/cure-poison-2.tga"
  :cost                  5.0
  :visual-effect-self    nil
  :range                 7   ;; in tile units
  :effective-range       2  ;; in tile units
  :visual-effect-target  particles:make-cure-level-1
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-poison generate)))
