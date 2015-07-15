(define-interaction
    can-talk            none
    can-ask-for-help    none
    can-be-opened       none
    can-open            none
    can-attack          none
    can-be-attacked     none
    can-be-destroyed    none
    can-be-burned       none
    can-heal            none
    can-be-heal         none
    can-poison          none
    can-be-poisoned     none
    can-be-drunk        none
    can-be-eaten        none
    can-be-weared-arm   none
    can-be-weared-head  none
    can-be-weared-neck  none
    can-be-weared-feet  none
    can-cut             none
    can-smash           none
    can-pierce          none
    can-launch-bolt     true
    can-launch-arrow    none
    mounted-on-pole     none
    decay               generate
    effects
    (define-effects
      strength                    none
      stamina                     none
      dexterity                   none
      agility                     none
      smartness                   none
      empaty                      none
      weight                      none
      damage-points               none
      movement-points             none
      magic-points                none
      dodge-chance                none
      melee-attack-chance         none
      range-attack-chance         none
      melee-attack-damage         none
      range-attack-damage         none
      edge-weapons-chance-bonus   none
      edge-weapons-damage-bonus   none
      impact-weapons-chance-bonus none
      impact-weapons-damage-bonus none
      pole-weapons-chance-bonus   none
      pole-weapons-damage-bonus   none
      range-weapons-chance-bonus  generate
      range-weapons-damage-bonus  generate
      unlock-chance               none
      deactivate-trap-chance      none
      reply-attack-chance         generate
      ambush-attack-chance        generate
      spell-chance                generate
      attack-spell-chance         generate)
    healing-effects
    (define-healing-effects
	heal-damage-points         none
      heal-poison                generate
      heal-berserk                 generate
      heal-faint                   generate
      heal-terror                  generate
      cause-poison                 generate
      cause-berserk                generate
      cause-faint                  generate
      cause-terror                 generate
      immune-poison                generate
      immune-berserk               generate
      immune-faint                 generate
      immune-terror                generate)
    magic-effect                   generate)
