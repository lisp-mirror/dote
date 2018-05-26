(in-package :spell)

(define-spell (:repair-weapon-1)
  :level                 5
  :element               :fire
  :description           "Repair your objects."
  :gui-texture           "misc/repair-weapon-1.tga"
  :cost                  50.0
  :visual-effect-self    none
  :range                 1  ;; in tile units
  :effective-range       0  ;; in tile units
  :visual-effect-target  particles:make-repair-1-fx
  :sound-effect-target     sound:+generic-spell+ ;; see: sound.lisp
  :effects
  (lambda (attacker defender)
    (declare (ignore attacker))
    (with-accessors ((ghost ghost)) defender
      (with-accessors ((elm        elm)
                       (shoes      shoes)
                       (armor      armor)
                       (left-hand  left-hand)
                       (right-hand right-hand)
                       (ring       ring)) ghost
        (flet ((repair (o)
                 (when o
                   (character:restart-age o))))
          (repair elm)
          (repair shoes)
          (repair armor)
          (repair left-hand)
          (repair right-hand)
          (repair ring))))))
