(in-package :spell)

(let ((max-damage 10.0))
  (define-spell (:vampire-2)
    :level                 5
    :element               :fire
    :tags                  (:damage)
    :description           (_ "Steals maximum 10 dmg (to inflict damage a melee weapon must be worn).")
    :gui-texture           "misc/vampire-2.tga"
    :cost                  10.0
    :damage-inflicted      max-damage
    :visual-effect-self    none
    :range                 25 ;; in tile units
    :effective-range        0 ;; in tile units
    :visual-effect-target  (lambda (a b) (declare (ignore a b)) nil)
    :effects
    (lambda (attacker defender)
      (with-accessors ((state state)) defender
        (let ((msg-melee  (make-instance 'game-event:attack-melee-event
                                         :id-origin       (id attacker)
                                         :id-destination  (id defender)
                                         :attacker-entity attacker))
              (msg-heal (make-instance 'random-object-messages:heal-damage-msg
                                       :msg-points  (num:lcg-next-upto max-damage)
                                       :msg-trigger +effect-when-used+
                                       :msg-origin  (id defender)
                                       :msg-target  +target-self+
                                       :msg-chance  0.8)))
          (let ((texture (res:get-resource-file "vampire-bite.tga"
                                                +animation-texture-dir+))
                (size          (num:d* 5.0 constants:+terrain-chunk-tile-size+)))
            (when (not (faction-ai-p state (id defender)))
              (billboard:enqueue-animated-billboard (pos defender)
                                                    texture
                                                    state
                                                    (compiled-shaders defender)
                                                    :w                         size
                                                    :h                         size
                                                    :texture-horizontal-offset 0.25
                                                    :duration/2                2.0
                                                    :loop-p                    t
                                                    :gravity                   0.1
                                                    :frequency-animation       20)))
          (game-event:propagate-attack-melee-event msg-melee)
          (game-event:propagate-heal-damage-event
           (game-event:make-heal-damage-event (id attacker) (id attacker) msg-heal)))))))
