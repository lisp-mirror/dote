(in-package :random-elm)

(define-interaction
    can-intercept-attacks true
    can-be-worn-hand      false
    can-be-picked         true
    can-be-held-in-hand   false
    can-be-worn-head      true
    decay                 generate
    effects
    (define-effects
      strength                   none
      stamina                    none
      dexterity                  none
      agility                    none
      smartness                  none
      empaty                     none
      weight                     none
      damage-points              none
      movement-points            none
      magic-points               none
      dodge-chance               none
      melee-attack-chance        none
      range-attack-chance        none
      melee-attack-damage        none
      range-attack-damage        none
      edge-weapons-chance-bonus    none
      edge-weapons-damage-bonus    none
      impact-weapons-chance-bonus  none
      impact-weapons-damage-bonus  none
      pole-weapons-chance-bonus    none
      pole-weapons-damage-bonus    none
      range-weapons-chance-bonus   none
      range-weapons-damage-bonus   none
      unlock-chance                none
      deactivate-trap-chance       none
      reply-attack-chance          none
      ambush-attack-chance         none
      spell-chance                 none
      attack-spell-chance          none)
    healing-effects
    (define-healing-effects
      immune-berserk               generate
      immune-faint                 generate
      immune-terror                generate)
    magic-effect                   none)
