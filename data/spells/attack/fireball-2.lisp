(in-package :spell)

(define-attack-spell (:fireball-2)
  :level                 5
  :tags                  (:damage)
  :element               :fire
  :gui-texture           "attack/fireball-2.tga"
  :cost                  10.0
  :visual-effect-self    none
  :range                 25 ;; in tile units
  :effective-range       3  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-2
  :damage-inflicted      15.0
  :arrow                 particles:make-fireball-level-1
  :tremor                (lambda (world) (world:apply-tremor-1 world))
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects)))
