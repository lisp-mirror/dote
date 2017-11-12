(in-package :spell)

(define-spell (:cure-faint-1)
  :level                 1
  :element               :fire
  :gui-texture           "cure/cure-coma.tga"
  :cost                  50.0
  :visual-effect-self    nil
  :range                 5  ;; in tile units
  :effective-range       0  ;; in tile units
  :visual-effect-target  particles:make-cure-level-0
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-faint (define-healing-effect
							      (duration 1
									trigger  when-used
									chance   0.9
									target   other)))))
