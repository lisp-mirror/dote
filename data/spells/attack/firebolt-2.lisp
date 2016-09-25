(in-package :spell)

(define-attack-spell (:firebolt-2)
  :level                 5
  :element               :fire
  :gui-texture           "attack/firearrow-lvl2.tga"
  :cost                  5.0
  :visual-effect-self    nil
  :range                 20 ;; in tile units
  :effective-range       1  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-1
  :damage-inflicted      15.0
  :arrow                 particles:make-fire-dart-level-0
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects)))
