(in-package :random-shoes)

(define-interaction
    can-be-worn-feet       true
    can-be-picked          true
    decay                  generate
    effects
    (define-effects
      movement-points      generate
      dodge-chance         generate
      reply-attack-chance  generate
      ambush-attack-chance generate)
    healing-effects        (define-healing-effects)
    magic-effect           none)
