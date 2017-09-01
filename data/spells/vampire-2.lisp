(in-package :spell)

(define-spell (:vampire-2)
  :level                 5
  :element               :fire
  :tags                  (:attack)
  :description          (_ "Steals maximum 10 dmg (to inflict damage a melee weapon must be worn).")
  :gui-texture           "misc/vampire-2.tga"
  :cost                  10.0
  :visual-effect-self    nil
  :range                 20  ;; in tile units
  :effective-range        1  ;; in tile units
  :visual-effect-target  particles:make-poison-level-1
  :effects
  (lambda (attacker defender)
    (let ((msg-melee  (make-instance 'game-event:attack-melee-event
				     :id-origin       (id attacker)
				     :id-destination  (id defender)
				     :attacker-entity attacker))
	  (msg-heal (make-instance 'random-object-messages:heal-damage-msg
				   :msg-points  (num:lcg-next-upto 10.0)
				   :msg-trigger +effect-when-used+
				   :msg-origin  (id defender)
				   :msg-target  +target-self+
				   :msg-chance  0.8)))
      (game-event:propagate-attack-melee-event msg-melee)
      (game-event:propagate-heal-damage-event
       (game-event:make-heal-damage-event (id attacker) (id attacker) msg-heal)))))
