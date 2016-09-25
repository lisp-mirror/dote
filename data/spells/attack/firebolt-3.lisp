(in-package :spell)

(define-attack-spell (:firebolt-3)
  :level                 10
  :element               :fire
  :gui-texture           "attack/firearrow-lvl3.tga"
  :cost                  10.0
  :visual-effect-self    nil
  :range                 20 ;; in tile units
  :effective-range       1  ;; in tile units
  :visual-effect-target  particles:make-aerial-explosion-level-2
  :damage-inflicted      20.0
  :arrow                 particles:make-fire-dart-level-1
  :effects               (define-interaction
			   effects         (define-effects)
			   healing-effects (define-healing-effects)))
