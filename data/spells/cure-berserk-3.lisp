(in-package :spell)

(define-spell (:cure-berserk-3)
  :level                 10
  :element               :fire
  :gui-texture           "cure/cure-berserk-3.tga"
  :cost                  10.0
  :visual-effect-self    nil
  :range                 5   ;; in tile units
  :effective-range       10  ;; in tile units
  :visual-effect-target  particles:make-cure-level-2
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-berserk generate)))
