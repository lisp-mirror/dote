(define-interaction
    can-intercept-attacks true
    can-be-worn-body      true
    can-be-picked         true
    can-be-held-in-hand   false
    decay                 generate
    effects
    (define-effects
      dodge-chance               generate
      reply-attack-chance        generate
      ambush-attack-chance       generate
      spell-chance               none
      attack-spell-chance        none)
    healing-effects
    (define-healing-effects
      immune-berserk               generate
      immune-faint                 generate
      immune-terror                generate)
    magic-effect                   none)
