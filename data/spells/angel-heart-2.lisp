(in-package :spell)

(define-spell (:angel-heart-2)
  :level                 10
  :element               :fire
  :gui-texture           "cure/cure-2.tga"
  :cost                  50.0
  :visual-effect-self    nil
  :range                 7   ;; in tile units
  :effective-range       1   ;; in tile units
  :visual-effect-target  particles:make-heal-level-2
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
