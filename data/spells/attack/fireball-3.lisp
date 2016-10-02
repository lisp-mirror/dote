(in-package :spell)

(define-attack-spell (:fireball-3)
  :level                 10
  :element               :fire
  :gui-texture           "attack/fireburst.tga"
  :cost                  20.0
  :visual-effect-self    nil
  :range                 20 ;; in tile units
  :effective-range       5  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-3
  :damage-inflicted      20.0
  :arrow                 particles:make-fireball-level-2
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects)))
