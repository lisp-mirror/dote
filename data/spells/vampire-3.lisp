(in-package :spell)

(define-spell (:vampire-3)
  :level                 10
  :element               :fire
  :tags                  (:attack)
  :description          (_ "Steals maximum 30 dmg and poison enemy (to inflict damage a melee weapon must be worn).")
  :gui-texture           "misc/vampire-3.tga"
  :cost                  30.0
  :visual-effect-self    none
  :range                 30  ;; in tile units
  :effective-range        0  ;; in tile units
  :visual-effect-target  particles:make-vampire-level-2
  :effects
  (lambda (attacker defender)
    (let ((msg-melee  (make-instance 'game-event:attack-melee-event
				     :id-origin       (id attacker)
				     :id-destination  (id defender)
				     :attacker-entity attacker))
	  (msg-heal   (make-instance 'random-object-messages:heal-damage-msg
                                     :msg-points  (num:lcg-next-upto 30.0)
                                     :msg-trigger +effect-when-used+
                                     :msg-origin  (id defender)
                                     :msg-target  +target-self+
                                     :msg-chance  0.8))
          (msg-poison (make-instance 'random-object-messages:cause-poison-msg
                                     :msg-damage  (num:lcg-next-upto 10.0)
                                     :msg-trigger +effect-when-used+
                                     :msg-target  +target-self+
                                     :msg-origin  (id attacker)
                                     :msg-chance  0.9)))
      (game-event:propagate-attack-melee-event msg-melee)
      (game-event:propagate-cause-poisoning-event
       (game-event:make-cause-poisoning-event (id attacker) (id defender) msg-poison))
      (game-event:propagate-heal-damage-event
       (game-event:make-heal-damage-event (id attacker) (id attacker) msg-heal)))))
