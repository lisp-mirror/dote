(in-package :spell)

(define-spell (:cure-faint-1)
  :level                 1
  :element               :fire
  :gui-texture           "cure/cure-coma.tga"
  :cost                  50.0
  :visual-effect-self    none
  :range                 5  ;; in tile units
  :effective-range       0  ;; in tile units
  :visual-effect-target  particles:make-cure-level-0
  :sound-effect-target     sound:+generic-spell+ ;; see: sound.lisp
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-faint (define-healing-effect
							      (duration 1
									trigger  when-used
									chance   0.9
									target   other)))))
