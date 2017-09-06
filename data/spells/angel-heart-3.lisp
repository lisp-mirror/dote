(in-package :spell)

(define-spell (:angel-heart-3)
  :level                 10
  :tags                  (:heal)
  :element               :fire
  :gui-texture           "cure/cure-3.tga"
  :cost                  120.0
  :visual-effect-self    nil
  :range                 10   ;; in tile units
  :effective-range       7   ;; in tile units
  :visual-effect-target  particles:make-heal-level-2
  :effects (define-interaction
	     effects         (define-effects)
	     healing-effects (define-healing-effects
				 heal-berserk generate
			       heal-terror    generate
			       heal-poison    generate
			       heal-faint     (define-healing-effect
						  (duration 1
							    trigger  when-used
							    chance   0.9
							    target   other))
			       heal-damage-points  (define-heal-dmg-effect (duration unlimited
									      points   1000.0
									      trigger  when-used
									      chance   0.8
									      target other)))))
