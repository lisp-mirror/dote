(in-package :spell)

(define-spell (:angel-heart-2)
  :level                 10
  :tags                  (:heal)
  :element               :fire
  :gui-texture           "cure/cure-2.tga"
  :cost                  90.0
  :visual-effect-self    none
  :range                 11  ;; in tile units
  :effective-range       0   ;; in tile units
  :visual-effect-target  particles:make-heal-level-2
  :sound-effect-target   sound:+heal-2+ ;; see: sound.lisp
  :effects (define-interaction
	     effects         (define-effects)
	     healing-effects (define-healing-effects
				 heal-berserk generate
			       heal-terror    generate
			       heal-poison    generate
			       heal-damage-points  (define-heal-dmg-effect (duration unlimited
									      points   1000.0
									      trigger  when-used
									      chance   0.8
									      target other)))))
