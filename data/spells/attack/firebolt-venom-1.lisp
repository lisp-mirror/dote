(in-package :spell)

(define-attack-spell (:firebolt-venom-1)
  :level                 1
  :element               :fire
  :tags                  (:damage)
  :gui-texture           "attack/firearrow-venom-1.tga"
  :cost                  7.0
  :visual-effect-self    none
  :range                 20 ;; in tile units
  :effective-range       0  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-0
  :damage-inflicted      10.0
  :arrow                 particles:make-fire-dart-level-0
  :tremor                (lambda (world) (world:apply-tremor-0 world))
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects
					       cause-poison generate)))
