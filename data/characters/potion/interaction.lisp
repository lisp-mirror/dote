(in-package :random-potion)

(define-interaction
    can-be-drunk                   true
    can-be-picked                  true
    can-be-held-in-hand            false
    decay                          generate
    effects
    (define-effects
      strength                     none
      stamina                      none
      dexterity                    none
      agility                      none
      smartness                    none
      empaty                       none
      weight                       none
      damage-points                none
      movement-points              none
      magic-points                 none
      dodge-chance                 none
      melee-attack-chance          none
      range-attack-chance          none
      melee-attack-damage          none
      range-attack-damage          none
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
      heal-damage-points         generate
      heal-poison                generate
      heal-berserk               generate
      heal-faint                 generate
      heal-terror                generate
      cause-poison               none
      cause-berserk              none
      cause-faint                none
      cause-terror               none
      immune-poison              generate
      immune-berserk             generate
      immune-faint               generate
      immune-terror              generate)
    magic-effect                 generate)
