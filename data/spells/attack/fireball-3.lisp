(in-package :spell)

(define-attack-spell (:fireball-3)
  :level                 10
  :element               :fire
  :gui-texture           "attack/fireburst.tga"
  :cost                  40.0
  :visual-effect-self    none
  :range                 30 ;; in tile units
  :effective-range       7  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-3
  :sound-effect-target   sound:+boom-3+ ;; can be a function too
  :damage-inflicted      30.0
  :arrow                 particles:make-fireball-level-2
  :tremor                (lambda (world) (world:apply-tremor-2 world))
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects)))
