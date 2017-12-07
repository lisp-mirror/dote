(in-package :random-weapon)

(define-interaction
    can-be-picked       true
    can-talk            false
    can-ask-for-help    false
    can-be-opened       false
    can-open            false
    can-attack          false
    can-be-attacked     false
    can-be-destroyed    false
    can-be-burned       false
    can-heal            false
    can-be-heal         false
    can-poison          false
    can-be-poisoned     false
    can-be-drunk        false
    can-be-eaten        false
    can-be-worn-arm     false
    can-be-worn-head    false
    can-be-worn-neck    false
    can-be-worn-feet    false
    can-be-held-in-hand true
    can-cut             true
    can-smash           false
    can-pierce          false
    can-launch-bolt     false
    can-launch-arrow    false
    mounted-on-pole     false
    decay               generate
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
      spell-points                 none
      dodge-chance                 none
      melee-attack-chance          none
      range-attack-chance          none
      melee-attack-damage          none
      range-attack-damage          none
      edge-weapons-chance-bonus    generate
      edge-weapons-damage-bonus    generate
      impact-weapons-chance-bonus  none
      impact-weapons-damage-bonus  none
      pole-weapons-chance-bonus    none
      pole-weapons-damage-bonus    none
      range-weapons-chance-bonus   none
      range-weapons-damage-bonus   none
      unlock-chance                generate
      deactivate-trap-chance       generate
      reply-attack-chance          generate
      ambush-attack-chance         generate
      spell-chance                 generate
      attack-spell-chance          generate)
    healing-effects
    (define-healing-effects
	heal-damage-points         none
      heal-poison                  none
      heal-berserk                 none
      heal-faint                   none
      heal-terror                  none
      cause-poison                 generate
      cause-berserk                generate
      cause-faint                  generate
      cause-terror                 generate
      immune-poison                generate
      immune-berserk               generate
      immune-faint                 generate
      immune-terror                generate)
    magic-effect                   generate)
