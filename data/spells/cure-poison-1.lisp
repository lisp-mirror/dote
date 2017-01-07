(in-package :spell)

(define-spell (:cure-poison-1)
  :level                 1
  :element               :fire
  :gui-texture           "cure/cure-poison-1.tga"
  :cost                  2.0
  :visual-effect-self    nil
  :range                 5   ;; in tile units
  :effective-range       1  ;; in tile units
  :visual-effect-target  particles:make-cure-level-0
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-poison generate)))