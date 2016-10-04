(in-package :spell)

(define-spell (:cure-berserk-2)
  :level                 5
  :element               :fire
  :gui-texture           "cure/cure-berserk-2.tga"
  :cost                  5.0
  :visual-effect-self    nil
  :range                 5   ;; in tile units
  :effective-range       5   ;; in tile units
  :visual-effect-target  particles:make-cure-level-1
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-berserk generate)))
