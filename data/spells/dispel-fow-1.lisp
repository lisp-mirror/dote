(in-package :spell)

(define-spell (:dispel-fow-1)
  :level                 1
  :element               :fire
  :tags                  ()
  :description           (_ "Dispel 4 tiles of FOW.")
  :gui-texture           "misc/dispel-fow-1.tga"
  :cost                  7.0
  :visual-effect-self    none
  :range                 15  ;; in tile units
  :effective-range        0  ;; in tile units
  :visual-effect-target  particles:make-dispel-fow-1-fx
  :sound-effect-target   sound:+generic-spell+ ;; see: sound.lisp
  :effects
  (lambda (attacker defender)
    (declare (ignore attacker))
    (entity:popup-from-fow defender :size 4)))
