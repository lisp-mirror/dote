(in-package :spell)

(define-spell (:cure-terror-3)
  :level                 10
  :element               :fire
  :gui-texture           "cure/cure-terror-3.tga"
  :cost                  10.0
  :visual-effect-self    none
  :range                 11 ;; in tile units
  :effective-range       3  ;; in tile units
  :visual-effect-target  particles:make-cure-level-2
  :sound-effect-target     sound:+generic-spell+ ;; see: sound.lisp
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       heal-terror generate)))
