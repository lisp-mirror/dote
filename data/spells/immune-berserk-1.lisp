(in-package :spell)

(define-spell (:immune-berserk-1)
  :level                 1
  :element               :fire
  :gui-texture           "cure/immune-berserk-1.tga"
  :cost                  2.0
  :visual-effect-self    none
  :range                 8   ;; in tile units
  :effective-range       0   ;; in tile units
  :visual-effect-target  particles:make-heal-level-0
  :sound-effect-target     sound:+generic-spell+ ;; see: sound.lisp
  :effects (define-interaction
	     effects         (define-effects)
	     healing-effects (define-healing-effects
				 immune-berserk generate)))
