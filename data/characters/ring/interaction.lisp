(define-interaction
    can-intercept-attacks false
    can-be-worn-hand      true
    can-be-picked         true
    can-be-held-in-hand   false
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
      edge-weapons-chance-bonus    generate
      edge-weapons-damage-bonus    generate
      impact-weapons-chance-bonus  generate
      impact-weapons-damage-bonus  generate
      pole-weapons-chance-bonus    generate
      pole-weapons-damage-bonus    generate
      range-weapons-chance-bonus   generate
      range-weapons-damage-bonus   generate
      unlock-chance                generate
      deactivate-trap-chance       generate
      reply-attack-chance          generate
      ambush-attack-chance         generate
      spell-chance                 none
      attack-spell-chance          none)
    healing-effects
    (define-healing-effects
      immune-berserk               generate
      immune-faint                 generate
      immune-terror                generate)
    magic-effect                   generate)
