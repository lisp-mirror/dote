(in-package :spell)

(define-spell (:cure-terror-2)
  :level                 5
  :element               :fire
  :gui-texture           "cure/cure-terror-2.tga"
  :cost                  5.0
  :visual-effect-self    none
  :range                 11 ;; in tile units
  :effective-range       2  ;; in tile units
  :visual-effect-target  particles:make-cure-level-1
  :sound-effect-target     sound:+generic-spell+ ;; see: sound.lisp
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-terror generate)))
