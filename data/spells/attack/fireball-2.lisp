(in-package :spell)

(define-attack-spell (:fireball-2)
  :level                 5
  :element               :fire
  :gui-texture           "attack/fireball-2.tga"
  :cost                  10.0
  :visual-effect-self    none
  :sound-effect-self     sound:+fireball-2+ ;; see: sound.lisp
  :range                 25 ;; in tile units
  :effective-range       3  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-2
  :sound-effect-target   sound:+boom-2+ ;; see: sound.lisp
  :damage-inflicted      15.0
  :arrow                 particles:make-fireball-level-1
  :tremor                (lambda (world) (world:apply-tremor-1 world))
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects)))
