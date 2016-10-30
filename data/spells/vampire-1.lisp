(in-package :spell)

(define-spell (:vampire-1)
  :level                 1
  :element               :fire
  :description           "Steals 3 dmg (maximum)."
  :gui-texture           "misc/vampire-1.tga"
  :cost                  2.0
  :visual-effect-self    nil
  :range                 20  ;; in tile units
  :effective-range        1  ;; in tile units
  :visual-effect-target  particles:make-poison-level-0
  :effects
  (lambda (attacker defender)
    (let ((msg-melee  (make-instance 'game-event:attack-melee-event
				     :id-origin       (id attacker)
				     :id-destination  (id defender)
				     :attacker-entity attacker))
	  (msg-heal (make-instance 'random-object-messages:heal-damage-msg
				   :msg-points  (num:lcg-next-upto 3.0)
				   :msg-trigger +effect-when-used+
				   :msg-origin  (id defender)
				   :msg-target  +target-self+
				   :msg-chance  0.8)))
      (game-event:propagate-attack-melee-event msg-melee)
      (game-event:propagate-heal-damage-event
       (game-event:make-heal-damage-event (id attacker) (id attacker) msg-heal)))))
