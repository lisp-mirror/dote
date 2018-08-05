(in-package :spell)

(define-spell (:angel-heart-1)
  :level                 1
  :tags                  (:heal)
  :element               :fire
  :gui-texture           "cure/cure-1.tga"
  :cost                  50.0
  :visual-effect-self    none
  :range                 8   ;; in tile units
  :effective-range       0   ;; in tile units
  :visual-effect-target  particles:make-heal-level-2
  :sound-effect-target   sound:+heal-1+ ;; see: sound.lisp
  :effects (define-interaction
	     effects         (define-effects)
	     healing-effects (define-healing-effects
				 heal-damage-points  (define-heal-dmg-effect
                                                         (duration unlimited
                                                          points   1000.0
                                                          trigger  when-used
                                                          chance   0.8
                                                          target   other)))))
